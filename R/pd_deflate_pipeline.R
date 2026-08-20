#' Batch-deflate every survey in the PIP master inventory
#'
#' Iterates over the master inventory of cleaned surveys, deflates each via
#' [pd_deflation()] (simple Mode B: `pip_id` only, no `version` hint), correctly
#' detects and skips failures -- including `NA` returns from [pd_deflation()]
#' -- saves each successful deflated survey to the dedicated `"pip_deflated"`
#' stamp alias, updates the master inventory with deflation columns, and logs a
#' structured `deflate_summary_inf` summary entry to `"pipdata_log"`.
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
#'   catalog state when the pipeline ran; the exact aux vintage consumed by
#'   [pd_deflation()] is the one embedded in the survey's `pip_meta` artifact
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
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load master inventory and normalise schema  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(inv)) {
    inv <- pipload::load_pip_master_inventory(verbose = verbose)
  }
  data.table::setDT(inv)

  # Normalise missing deflation columns (older inventories built before the
  # schema bump) so the candidate filter, column updates, and the persisted
  # master all share the same five-column schema. `deflated` is logical NA;
  # the hash columns are NA_character_.
  if (!"deflated" %in% names(inv)) {
    inv[, deflated := NA]
  }
  if (!"content_hash_deflated" %in% names(inv)) {
    inv[, content_hash_deflated := NA_character_]
  }
  for (col in c("aux_cpi_hash_at_deflation", "aux_ppp_hash_at_deflation",
                "aux_pop_hash_at_deflation")) {
    if (!col %in% names(inv)) {
      inv[, (col) := NA_character_]
    }
  }

  # Empty inventory: nothing to do.
  if (nrow(inv) == 0L) {
    if (verbose) {
      cli::cli_alert_info("Master inventory is empty -- nothing to deflate.")
    }
    return(inv)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Select candidate surveys  ---------
  # First version deflates everything not yet deflated (R7); force bypasses
  # the deflated flag entirely.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  candidates <- if (isTRUE(force)) {
    inv
  } else {
    inv[is.na(deflated) | deflated == FALSE]
  }

  if (nrow(candidates) == 0L) {
    if (verbose) {
      cli::cli_alert_info("No surveys pending deflation -- nothing to do.")
    }
    return(inv)
  }

  # Resolve current auxiliary content hashes once for the whole run (R10).
  aux_hashes <- get_aux_hashes(c("cpi", "ppp", "pop"), verbose = verbose)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Deflate one survey at a time  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inv_ls <- split(candidates, seq_len(nrow(candidates)))
  names(inv_ls) <- candidates$pip_id

  results <- lapply(inv_ls, deflate_one, verbose = verbose)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Summarise the run  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  successful_results <- Filter(Negate(is.null), results)
  successful_ids  <- vapply(successful_results, \(x) x$pip_id, character(1))
  fail_ids        <- setdiff(candidates$pip_id, successful_ids)
  n_total         <- length(results)
  n_success       <- length(successful_ids)
  n_failed        <- n_total - n_success

  # Use log_add() with an explicit `args` list: the pipfun typed log wrappers
  # (log_info/log_error) capture all caller formals by reference, and `inv`
  # is a whole-inventory formal here -- retaining it in .piplogenv would
  # defeat per-survey garbage collection over full-inventory runs.
  pipfun::log_add(
    event = "info",
    message = "Deflation pipeline complete.",
    name = "pipdata_log",
    args = list(n_total = n_total, n_success = n_success, n_failed = n_failed),
    logmeta = list(
      info = "deflate_summary_inf",
      n_total = n_total,
      n_success = n_success,
      n_failed = n_failed,
      surveys_success = successful_ids,
      surveys_failed = fail_ids
    )
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update master inventory with deflation provenance  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (n_success > 0L) {
    # Resolve the content_hash of each saved pip_deflated artifact once.
    # st_catalog_query() returns one row per artifact (latest version), but a
    # pip_id can span several rows (e.g. older runs, force re-deflations);
    # mirror build_pip_inventory() and keep the latest row per pip_id so the
    # mapping is deterministic and one-to-one.
    cat_deflated <- tryCatch(
      stamp::st_catalog_query(alias = "pip_deflated"),
      error = function(e) NULL
    )
    content_hash_map <- stats::setNames(character(0), character(0))
    if (!is.null(cat_deflated) && nrow(cat_deflated) > 0L) {
      cat_deflated[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]
      data.table::setorder(cat_deflated, pip_id, -created_at, path)
      cat_deflated <- cat_deflated[, .SD[1L], by = "pip_id"]
      content_hash_map <- stats::setNames(cat_deflated$content_hash, cat_deflated$pip_id)
    }

    # A successful survey whose artifact hash cannot be resolved must not be
    # silently persisted with NA provenance -- surface it in the log.
    hash_missing <- successful_ids[is.na(content_hash_map[successful_ids])]
    if (length(hash_missing) > 0L) {
      pipfun::log_add(
        event = "error",
        message = "Deflated survey content hash could not be resolved from the pip_deflated catalog.",
        name = "pipdata_log",
        logmeta = list(
          error = "deflate_provenance_missing",
          surveys = hash_missing,
          status = "The survey was deflated but its provenance hash is missing"
        )
      )
    }

    hash_dt <- data.table::data.table(
      pip_id = successful_ids,
      content_hash_deflated = unname(content_hash_map[successful_ids])
    )

    inv[hash_dt, on = "pip_id", content_hash_deflated := i.content_hash_deflated]
    inv[pip_id %in% successful_ids, deflated := TRUE]

    # Snapshot the run-time aux catalog hashes. Note these describe the
    # catalog state when the pipeline ran, not necessarily the exact
    # pip_meta-embedded vintage consumed by pd_deflation() (which is pinned
    # by version_id_metadata). Guard against partial aux maps so a NULL
    # value never deletes the column via :=.
    for (m in c("cpi", "ppp", "pop")) {
      col <- paste0("aux_", m, "_hash_at_deflation")
      val <- aux_hashes[[m]]
      if (is.null(val)) {
        val <- NA_character_
      }
      data.table::set(
        inv,
        i = which(inv$pip_id %in% successful_ids),
        j = col,
        value = val
      )
    }
  }

  if (length(fail_ids) > 0L) {
    # Failures must not keep stale deflation provenance. For a force
    # re-deflation this clears a previous success so the survey is retried on
    # the next run; for a normal run the failed rows were NA/FALSE already.
    fail_idx <- which(inv$pip_id %in% fail_ids)
    if (length(fail_idx) > 0L) {
      data.table::set(inv, i = fail_idx, j = "deflated", value = NA)
      data.table::set(inv, i = fail_idx, j = "content_hash_deflated", value = NA_character_)
      for (m in c("cpi", "ppp", "pop")) {
        data.table::set(
          inv, i = fail_idx, j = paste0("aux_", m, "_hash_at_deflation"),
          value = NA_character_
        )
      }
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save updated master inventory  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pipload::pip_write(
    x = inv,
    id = "pip_master_inventory",
    alias = "pip_master",
    pk = c("survey_id", "pip_id"),
    verbose = verbose
  )

  if (verbose) {
    cli::cli_alert_success(
      "Deflation pipeline complete: {n_success}/{n_total} surveys deflated."
    )
  }

  return(inv)
}

#' Deflate one survey (worker for [pd_deflate_pipeline()])
#'
#' Wraps [pd_deflation()] (Mode B) in a `tryCatch`, treats a non-`data.table`
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
deflate_one <- function(inv_row, verbose) {
  pip_id <- inv_row$pip_id

  tryCatch(
    expr = {
      dt <- pd_deflation(pip_id = pip_id, verbose = FALSE)

      # pd_deflation() returns NA (not an error) on failure
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

      # name the list so save_pip_data() iterates over it correctly
      dt_ls <- list(dt)
      names(dt_ls) <- pip_id
      sv <- save_pip_data(dt_ls, alias = "pip_deflated", verbose = verbose)
      saved <- !is.null(sv) && length(sv) > 0L && isTRUE(sv[[1L]]$success)
      dt_size <- as.numeric(utils::object.size(dt))
      rm(dt, dt_ls)
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

      list(pip_id = pip_id, success = TRUE)
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
