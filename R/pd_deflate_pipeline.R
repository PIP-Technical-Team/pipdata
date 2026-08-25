#' Batch-deflate every survey in the PIP master inventory
#'
#' Builds a fresh dependency plan, loads exact planned data and metadata
#' versions through [pd_deflation_exact()], saves verified receipts to the
#' `"pip_deflated"` alias, and publishes inventory and manifest checkpoints.
#'
#' This function is an independent pipeline stage: it is **not** called by
#' [pd_process_data()]. Run it after cleaning (and after the `"pip_deflated"`
#' alias is registered, e.g. via [pipdata_dlw_process()] or an explicit
#' `stamp::st_init()`).
#'
#' First version policy (R7): everything not yet deflated is deflated. There is
#' no incremental aux-hash gating yet -- pass `force = TRUE` to re-deflate
#' surveys whose `deflated` column is already `TRUE`.
#'
#' @param inv A master inventory `data.table` (as returned by
#'   [pipload::load_pip_master_inventory()]). Default `NULL`, in which case the
#'   master inventory is loaded internally.
#' @param force Logical. If `TRUE`, re-deflate every row regardless of the
#'   `deflated` column. Default `FALSE`.
#' @param verbose Logical. Controls verbosity of downstream
#'   [pipload::load_pip_master_inventory()] and [pipload::pip_write()] calls.
#'   Default: `getOption("pipdata.verbose", default = TRUE)`.
#' @param bootstrap Logical. Explicitly permit unknown-provenance work.
#' @param bootstrap_entities Optional restrictive bootstrap identifiers.
#' @param dependency_plan Optional advisory plan, revalidated before writes.
#'
#' @return The updated master inventory `data.table` (rows deflated in this run
#'   have `deflated = TRUE`, `content_hash_deflated` and
#'   `aux_*_hash_at_deflation` populated). Side effects: when at least one
#'   survey is processed, writes deflated artifacts to the `"pip_deflated"`
#'   alias, writes the updated master to `"pip_master"`, and logs a
#'   `deflate_summary_inf` entry.
#'
#' @details
#' **Logging**: writes a `deflate_summary_inf` info entry to `"pipdata_log"`
#' with pinned keys `n_total`, `n_success`, `n_failed`, `surveys_success`, and
#' `surveys_failed` whenever at least one survey was processed. Per-survey
#' failures are logged as `error` entries (`deflation_na`,
#' `deflate_save_error`, or the underlying condition class) with the survey
#' id. A missing `content_hash_deflated` for a deflated survey is logged
#' (`deflate_provenance_missing`), never silent.
#'
#' **Column provenance**:
#' - `deflated` -- logical; `TRUE` for surveys successfully deflated.
#' - `content_hash_deflated` -- `content_hash` of the `"pip_deflated"`
#'   artifact (queried from stamp after the run).
#' - `aux_cpi_hash_at_deflation`, `aux_ppp_hash_at_deflation`,
#'   `aux_pop_hash_at_deflation` -- aux `content_hash` resolved once per run by
#'   [get_aux_hashes()], snapshot on the deflated rows. These describe the aux
#'   catalog state when the pipeline ran; the exact aux vintage consumed is
#'   embedded in the pinned `pip_meta` artifact
#'   (pinned by `version_id_metadata`), not this snapshot.
#'
#' @family pd_deflate_pipeline pipeline
#' @seealso [pd_process_data()] for the cleaning stage, [pd_deflation()] for
#'   single-survey deflation, [log_report()] for the report that renders the
#'   `deflate_summary_inf` entry.
#' @export
#'
#' @examples
#' \dontrun{
#' release <- "20260401"
#' pipfun::setup_working_release(release, "TEST", verbose = FALSE)
#' stamp::st_init(
#'   root = fs::path(getOption("pipfun.main_dir"), "pip_repository", "pip_deflated"),
#'   alias = "pip_deflated"
#' )
#' new_pip_inv <- pd_deflate_pipeline(force = TRUE, verbose = TRUE)
#' }
pd_deflate_pipeline <- function(
  inv     = NULL,
  force   = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE),
  bootstrap = FALSE,
  bootstrap_entities = NULL,
  dependency_plan = NULL
) {
  if (!isTRUE(bootstrap) && !is.null(bootstrap_entities)) {
    cli::cli_abort("bootstrap_entities requires bootstrap = TRUE.",
                   class = "pipdata_bootstrap_selector_error")
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load master inventory and normalise schema  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(inv)) {
    inv <- pipload::load_pip_master_inventory(verbose = verbose)
  }
  data.table::setDT(inv)

  context <- pd_dependency_context()
  execution <- pd_prepare_execution(
    inv = data.table::data.table(), master = inv, context = context,
    advisory_plan = dependency_plan, bootstrap = bootstrap,
    bootstrap_entities = bootstrap_entities, force = force,
    verbose = verbose
  )
  on.exit(pd_lease_release(execution$lease), add = TRUE)
  actions <- execution$plan$actions[
    stage == "deflate" & action != "none"
  ]
  if (!nrow(actions)) return(inv)
  actions <- inv[actions, on = "pip_id", nomatch = 0L]
  required <- c("version_id_data", "version_id_metadata",
                "content_hash_data", "content_hash_metadata")
  if (!all(required %in% names(actions)) ||
      anyNA(actions[, required, with = FALSE])) {
    rlang::abort("Fresh deflation actions lack exact input receipts.",
                 class = "pipdata_deflation_action_invalid")
  }
  writer <- function(alias, id) {
    function(candidate, lease) {
      pd_save_receipt(candidate, id, alias, verbose, lease)
    }
  }
  action_rows <- split(actions, seq_len(nrow(actions)))
  attempted_ids <- character()
  successful_ids <- character()
  pd_run_checkpoint_batches(action_rows, worker = function(action) {
    attempted_ids <<- c(attempted_ids, action$pip_id)
    attr(action, "lease") <- execution$lease
    attr(action, "execution") <- execution
    result <- pd_execute_deflate(action, verbose)
    if (!is.null(result) && isTRUE(result$success)) {
      successful_ids <<- c(successful_ids, action$pip_id)
    } else {
      inv <<- pd_persist_failed_invalidation(
        execution, inv, action,
        writer("pip_inv", "pip_release_inventory"),
        writer("pip_master", "pip_master_inventory")
      )
    }
    result
  }, checkpoint = function(results) {
    result_dt <- data.table::rbindlist(results, fill = TRUE)
    finalized <- pd_finalize_checkpoint(
      execution, inv, "deflate", result_dt,
      writer("pip_inv", "pip_release_inventory"),
      writer("pip_master", "pip_master_inventory")
    )
    inv <<- finalized$candidate
    execution <<- finalized$execution
  })
  return(inv)
}

pd_persist_failed_invalidation <- function(execution, master, action,
                                           release_writer, master_writer) {
  candidate <- pd_invalidate_failed_action(master, action)
  pd_assert_execution_fence(execution)
  release_receipt <- release_writer(candidate, execution$lease)
  if (!isTRUE(release_receipt$success)) {
    rlang::abort("Failed invalidation release write was not verified.",
                 class = "pipdata_failed_invalidation_release_error")
  }
  candidate[, latest_release_version_id := release_receipt$version_id]
  pd_assert_execution_fence(execution)
  master_receipt <- master_writer(candidate, execution$lease)
  if (!isTRUE(master_receipt$success)) {
    rlang::abort("Failed invalidation master write was not verified.",
                 class = "pipdata_failed_invalidation_master_error")
  }
  pd_assert_execution_fence(execution)
  candidate
}

#' Deflate one survey (worker for [pd_deflate_pipeline()])
#'
#' Wraps [pd_deflation_exact()] in a `tryCatch`, treats a non-`data.table`
#' return as a failure (including `NA`), saves successful deflation to the
#' `"pip_deflated"` alias via [save_pip_data()], and logs errors to
#' `"pipdata_log"`. No `pd_env_set`/`pd_env_rm` -- `pip_id` is captured by
#' closure in the handlers.
#'
#' @param inv_row A one-row `data.table` from the master inventory (must have
#'   a `pip_id` column).
#' @param verbose Logical scalar passed to [save_pip_data()].
#'
#' @return `list(pip_id, success = TRUE)` on success, or `NULL` when deflation
#'   or saving failed (the failure is logged to `"pipdata_log"`).
#' @noRd
pd_execute_deflate <- function(inv_row, verbose) {
  pip_id <- inv_row$pip_id

  tryCatch(
    expr = {
      required <- c("version_id_data", "version_id_metadata",
                    "content_hash_data", "content_hash_metadata")
      if (!all(required %in% names(inv_row)) ||
          anyNA(unlist(inv_row[, required, with = FALSE]))) {
        rlang::abort("Deflation action lacks exact input receipts.",
                     class = "pipdata_deflation_action_invalid")
      }
      dt <- pd_deflation_exact(
        pip_id = pip_id,
        data_version_id = inv_row$version_id_data,
        metadata_version_id = inv_row$version_id_metadata,
        data_hash = inv_row$content_hash_data,
        metadata_hash = inv_row$content_hash_metadata,
        verbose = FALSE
      )

      # Deflation may return NA rather than raising on invalid survey data.
      if (!data.table::is.data.table(dt)) {
        pipfun::log_add(
          event = "error",
          message = "Deflation returned a non-data.table result (deflation failed).",
          name = "pipdata_log",
          logmeta = list(error = "deflation_na", survey = pip_id,
                         status = "The survey was not deflated")
        )
        return(NULL)
      }

      lease <- attr(inv_row, "lease")
      execution <- attr(inv_row, "execution")
      if (!is.null(execution)) pd_assert_execution_fence(execution)
      receipt <- pd_save_receipt(dt, pip_id, "pip_deflated", verbose, lease)
      saved <- isTRUE(receipt$success)
      dt_size <- as.numeric(utils::object.size(dt))
      rm(dt)
      if (dt_size > getOption("pipdata.gc_threshold_bytes", default = 100e6)) {
        gc(verbose = FALSE)
      }

      if (!saved) {
        pipfun::log_add(
          event = "error",
          message = "Deflated survey could not be saved to pip_deflated.",
          name = "pipdata_log",
          logmeta = list(error = "deflate_save_error", survey = pip_id,
                         status = "The survey was not saved")
        )
        return(NULL)
      }

      c(list(stage = "deflate", pip_id = pip_id,
             data_version_id = inv_row$version_id_data,
             metadata_version_id = inv_row$version_id_metadata,
             input_hash = inv_row$input_hash,
             code_hash = inv_row$code_hash), receipt)
    },
    piperr = function(cnd) {
      # cli/pipload conditions carry the specific class first (e.g.
      # c("load_deflation_aux", "piperr", ...)); extract it so the type
      # summary triages the real failure, not the shared "piperr" marker.
      err_class <- setdiff(
        class(cnd), c("piperr", "rlang_error", "error", "condition")
      )
      if (length(err_class) == 0L) {
        err_class <- "unknown_error"
      } else {
        err_class <- err_class[1L]
      }
      pipfun::log_add(
        event = "error", message = cnd$message, name = "pipdata_log",
        logmeta = list(error = err_class, survey = pip_id,
                       status = "The survey was not deflated")
      )
      NULL
    },
    error = function(cnd) {
      original_cnd <- cnd
      while (!is.null(original_cnd$parent)) original_cnd <- original_cnd$parent
      if (inherits(original_cnd, "piperr")) {
        err_class <- setdiff(
          class(original_cnd), c("piperr", "rlang_error", "error", "condition")
        )
        if (length(err_class) == 0L) {
          err_class <- "unknown_error"
        } else {
          err_class <- err_class[1L]
        }
        pipfun::log_add(
          event = "error", message = original_cnd$message, name = "pipdata_log",
          logmeta = list(error = err_class, survey = pip_id,
                         status = "The survey was not deflated")
        )
      } else {
        pipfun::log_add(
          event = "error", message = cnd$message, name = "pipdata_log",
          logmeta = list(error = "unknown_error", survey = pip_id,
                         status = "The survey was not deflated")
        )
      }
      NULL
    }
  )
}

deflate_one <- function(inv_row, verbose) {
  pip_id <- inv_row$pip_id
  tryCatch({
    dt <- pd_deflation(pip_id, verbose = FALSE)
    if (!data.table::is.data.table(dt)) return(NULL)
    data <- stats::setNames(list(dt), pip_id)
    saved <- save_pip_data(data, "pip_deflated", verbose)
    if (is.null(saved) || !isTRUE(saved[[1L]]$success)) return(NULL)
    list(pip_id = pip_id, success = TRUE)
  }, error = function(e) NULL)
}
