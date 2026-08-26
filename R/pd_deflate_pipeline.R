#' Batch-deflate every survey in the PIP master inventory
#'
#' Builds a fresh dependency plan, loads exact planned data and metadata
#' versions through the internal strict exact-deflation path, saves verified receipts to the
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
  pd_deflate_pipeline_core(
    inv, force, verbose, bootstrap, bootstrap_entities, dependency_plan,
    force_surveys = character(), entity_error_policy = "continue",
    fatal_error_policy = "abort"
  )$master
}

#' Execute deflation through the typed stage boundary
#' @noRd
pd_run_deflate_stage <- function(
  inv = NULL, force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE), bootstrap = FALSE,
  bootstrap_entities = NULL, dependency_plan = NULL,
  force_surveys = character(), entity_error_policy = "continue",
  fatal_error_policy = "capture_at_run_boundary"
) {
  pd_deflate_pipeline_core(
    inv, force, verbose, bootstrap, bootstrap_entities, dependency_plan,
    force_surveys, entity_error_policy, fatal_error_policy
  )$result
}

pd_deflate_pipeline_core <- function(
  inv, force, verbose, bootstrap, bootstrap_entities, dependency_plan,
  force_surveys, entity_error_policy, fatal_error_policy
) {
  started_at <- Sys.time()
  run_id <- pd_random_id()
  if (is.null(inv)) inv <- pipload::load_pip_master_inventory(verbose = verbose)
  data.table::setDT(inv)
  dependency_context <- pd_dependency_context()
  execution <- pd_prepare_execution(
    inv = data.table::data.table(), master = inv, context = dependency_context,
    advisory_plan = dependency_plan, bootstrap = bootstrap,
    bootstrap_entities = bootstrap_entities, force = force,
    force_surveys = force_surveys, verbose = verbose
  )
  primary <- NULL
  on.exit({
    tryCatch(pd_lease_release(execution$lease), error = function(cnd) {
      if (is.null(primary)) stop(cnd)
    })
  }, add = TRUE)
  options <- pd_pipeline_options(
    verbose, force, force_surveys, bootstrap, bootstrap_entities %||% character(),
    getOption("pipdata.manifest_checkpoint_n", 25L),
    getOption("pipdata.manifest_checkpoint_seconds", 60),
    entity_error_policy, fatal_error_policy
  )
  actions <- execution$plan$actions[stage == "deflate" & action != "none"]
  selected <- sort(unique(execution$plan$actions[stage == "deflate", pip_id]))
  runtime <- new.env(parent = emptyenv())
  runtime$execution <- execution
  context <- new_pipeline_context(
    execution, run_id, options,
    list(survey_id = sort(unique(actions$survey_id)), pip_id = selected,
         force_requested = sort(unique(force_surveys))),
    pd_pipeline_storage(dependency_context), runtime = runtime
  )
  units <- pd_empty_stage_units()
  artifacts <- pd_empty_artifact_references()
  warnings <- list()
  errors <- list()
  checkpoint_generations <- numeric()
  receipts <- list()
  writer <- function(alias, id) {
    function(candidate, lease) pd_save_receipt(candidate, id, alias, verbose, lease)
  }
  if (nrow(actions)) {
    if (anyDuplicated(actions$pip_id) || anyDuplicated(inv$pip_id)) {
      rlang::abort("Deflation actions and inventory must match one-to-one.",
                   class = "pipdata_deflation_action_invalid")
    }
    inventory_rows <- inv[match(actions$pip_id, inv$pip_id)]
    if (nrow(inventory_rows) != nrow(actions) || anyNA(inventory_rows$pip_id)) {
      rlang::abort("Fresh deflation actions are absent from the inventory.",
                   class = "pipdata_deflation_action_invalid")
    }
    comparisons <- list(
      data_version_id = "version_id_data", data_hash = "content_hash_data",
      metadata_version_id = "version_id_metadata",
      metadata_hash = "content_hash_metadata"
    )
    if (!all(names(comparisons) %in% names(actions)) ||
        !all(unlist(comparisons) %in% names(inventory_rows)) ||
        any(vapply(names(comparisons), function(plan_field) {
          inventory_field <- comparisons[[plan_field]]
          anyNA(actions[[plan_field]]) ||
            !identical(actions[[plan_field]], inventory_rows[[inventory_field]])
        }, logical(1L)))) {
      rlang::abort("Fresh deflation actions lack exact input receipts.",
                   class = "pipdata_deflation_action_invalid")
    }
    inventory_fields <- setdiff(names(inventory_rows), names(actions))
    actions <- cbind(actions, inventory_rows[, inventory_fields, with = FALSE])
    actions[, `:=`(
      version_id_data = data_version_id,
      content_hash_data = data_hash,
      version_id_metadata = metadata_version_id,
      content_hash_metadata = metadata_hash
    )]
    action_rows <- split(actions, seq_len(nrow(actions)))
    pending_ids <- character()
    pending_started <- list()
    active_id <- NULL
    active_started <- NULL
    run <- function() pd_run_checkpoint_batches(
      action_rows,
      worker = function(action) {
        stamp <- Sys.time()
        active_id <<- action$pip_id
        active_started <<- stamp
        attr(action, "lease") <- execution$lease
        attr(action, "execution") <- execution
        outcome <- pd_execute_deflate(action, verbose)
        if (isTRUE(outcome$success)) {
          pending_ids <<- c(pending_ids, action$pip_id)
          pending_started[[action$pip_id]] <<- stamp
          receipts[[action$pip_id]] <<- outcome
        } else {
          units <<- rbind(units, pd_deflate_unit_row(
            action, "failed", outcome$condition$code, stamp, Sys.time()
          ))
          errors[[length(errors) + 1L]] <<- outcome$condition
          pd_log_stage_condition(run_id, outcome$condition)
          inv <<- pd_persist_failed_invalidation(
            execution, inv, action,
            writer("pip_inv", "pip_release_inventory"),
            writer("pip_master", "pip_master_inventory")
          )
          if (identical(entity_error_policy, "abort")) stop(outcome$condition)
        }
        active_id <<- NULL
        active_started <<- NULL
        outcome
      },
      checkpoint = function(results) {
        finalized <- pd_finalize_checkpoint(
          execution, inv, "deflate", data.table::rbindlist(results, fill = TRUE),
          writer("pip_inv", "pip_release_inventory"),
          writer("pip_master", "pip_master_inventory")
        )
        inv <<- finalized$candidate
        execution <<- finalized$execution
        runtime$execution <- execution
        checkpoint_generations <<- c(
          checkpoint_generations, execution$manifest_identity$generation
        )
        for (id in pending_ids) {
          action <- actions[pip_id == id][1L]
          units <<- rbind(units, pd_deflate_unit_row(
            action, "success", action$reason[[1L]] %||% character(),
            pending_started[[id]], Sys.time(), receipts[[id]]$content_hash
          ))
        }
        pending_ids <<- character()
        pending_started <<- list()
      }, checkpoint_n = options$checkpoint_size,
      checkpoint_seconds = options$checkpoint_seconds
    )
    caught <- tryCatch({ run(); NULL }, error = function(cnd) cnd)
    if (!is.null(caught)) {
      primary <- caught
      if (identical(fatal_error_policy, "abort")) stop(caught)
      completed <- Sys.time()
      for (id in pending_ids) {
        action <- actions[pip_id == id][1L]
        units <- rbind(units, pd_deflate_unit_row(
          action, "failed", "checkpoint_uncommitted",
          pending_started[[id]], completed
        ))
      }
      if (!is.null(active_id) && !active_id %in% units$entity_id) {
        action <- actions[pip_id == active_id][1L]
        units <- rbind(units, pd_deflate_unit_row(
          action, "failed", "fatal_uncommitted", active_started, completed
        ))
      }
      remaining_ids <- setdiff(actions$pip_id, units$entity_id)
      for (id in remaining_ids) {
        action <- actions[pip_id == id][1L]
        units <- rbind(units, pd_deflate_unit_row(
          action, "skipped", "upstream_failed",
          as.POSIXct(NA, tz = "UTC"), as.POSIXct(NA, tz = "UTC")
        ))
      }
      errors[[length(errors) + 1L]] <- new_stage_condition_record(
        caught, "error", stage = "deflate", operation = "stage",
        recoverable = FALSE
      )
    }
  }
  represented <- units$entity_id
  cached_ids <- setdiff(selected, c(actions$pip_id, represented))
  for (id in cached_ids) {
    units <- rbind(units, pd_deflate_unit_row(
      data.table::data.table(stage = "deflate", entity_id = id, pip_id = id,
                             survey_id = NA_character_, action = "none",
                             input_hash = NA_character_),
      "cached", "current", as.POSIXct(NA, tz = "UTC"),
      as.POSIXct(NA, tz = "UTC")
    ))
  }
  if (!length(selected)) context$selection$pip_id <- character()
  latest <- execution$manifest_identity
  if (pd_manifest_identity_valid(latest, FALSE)) {
    for (id in names(receipts)[names(receipts) %in% units[status == "success", entity_id]]) {
      finalized <- list(execution = execution)
      artifacts <- rbind(artifacts, new_artifact_reference(
        receipts[[id]], finalized, "deflate", id
      ))
    }
  }
  provenance <- list(
    release = context$release, identity = context$identity,
    scope_id = context$dependency$scope_id,
    context_hash = context$dependency$context_hash,
    plan_hash = context$dependency$plan_hash,
    manifest_before = context$dependency$manifest_before,
    manifest_after = latest,
    checkpoint_generations = sort(unique(checkpoint_generations)),
    stage_reason_codes = if (!length(selected)) "no_selection" else
      sort(unique(unlist(units$reason_codes)))
  )
  terminal <- !is.null(primary)
  result <- new_pipdata_stage_result(
    context, "deflate", terminal, units, artifacts, warnings, errors,
    list(name = "pipdata_log", run_id = run_id,
         summary_discriminator = if (sum(units$status %in% c("success", "failed")))
           "deflate_summary_inf" else NA_character_, log_checkpoint = NULL),
    provenance, started_at, Sys.time()
  )
  if (result$counts[["attempted"]] > 0L) pd_log_deflate_summary(result)
  list(result = result, master = inv, context = context)
}

pd_deflate_unit_row <- function(action, status, reasons, started_at, completed_at,
                                output_hash = NA_character_) {
  data.table::data.table(
    stage = "deflate", entity_id = action$pip_id,
    survey_id = action$survey_id %||% NA_character_, pip_id = action$pip_id,
    status = status, action = action$action %||% NA_character_,
    reason_codes = list(sort(unique(as.character(reasons)))),
    input_hash = action$input_hash %||% NA_character_, output_hash = output_hash,
    started_at = started_at, completed_at = completed_at
  )
}

pd_log_stage_condition <- function(run_id, record) {
  pipfun::log_add(
    event = "error", message = record$message, name = "pipdata_log",
    args = list(run_id = run_id, stage = record$stage,
                entity_id = record$entity_id, condition_id = record$condition_id),
    logmeta = list(error = record$code, survey = record$pip_id,
                   condition_id = record$condition_id)
  )
}

pd_log_deflate_summary <- function(result) {
  successful <- result$units[status == "success", pip_id]
  failed <- result$units[status == "failed", pip_id]
  pipfun::log_add(
    event = "info", message = "Deflation stage completed.", name = "pipdata_log",
    args = list(run_id = result$run_id, stage = "deflate",
                entity_id = NA_character_, condition_id = NA_character_),
    logmeta = list(
      info = "deflate_summary_inf", run_id = result$run_id,
      status = result$status, n_total = result$counts[["attempted"]],
      n_success = result$counts[["succeeded"]],
      n_failed = result$counts[["failed"]], surveys_success = successful,
      surveys_failed = failed, cached = result$counts[["cached"]],
      skipped = result$counts[["skipped"]]
    )
  )
}

pd_persist_failed_invalidation <- function(execution, master, action,
                                           release_writer, master_writer) {
  invalidate <- pd_invalidate_failed_action
  candidate <- if ("emit_log" %in% names(formals(invalidate))) {
    invalidate(master, action, emit_log = FALSE)
  } else {
    invalidate(master, action)
  }
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
#' Wraps the internal strict exact-deflation path in a `tryCatch`, treats a non-`data.table`
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
      dt <- pd_deflation_exact_strict(
        pip_id = pip_id,
        data_version_id = inv_row$version_id_data,
        metadata_version_id = inv_row$version_id_metadata,
        data_hash = inv_row$content_hash_data,
        metadata_hash = inv_row$content_hash_metadata,
        verbose = FALSE
      )

      # Deflation may return NA rather than raising on invalid survey data.
      if (!data.table::is.data.table(dt)) {
        condition <- new_stage_condition_record(
          severity = "error", code = "deflation_na",
          message = "Deflation returned a non-data.table result.",
          stage = "deflate", pip_id = pip_id, entity_id = pip_id,
          operation = "transform", recoverable = TRUE
        )
        return(list(success = FALSE, condition = condition))
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
        condition <- new_stage_condition_record(
          severity = "error", code = "deflate_save_error",
          message = "Deflated survey could not be saved to pip_deflated.",
          stage = "deflate", pip_id = pip_id, entity_id = pip_id,
          operation = "save", recoverable = TRUE,
          details = list(receipt_error = receipt$error %||% NA_character_)
        )
        return(list(success = FALSE, condition = condition))
      }

      c(list(stage = "deflate", pip_id = pip_id,
             data_version_id = inv_row$version_id_data,
             metadata_version_id = inv_row$version_id_metadata,
             input_hash = inv_row$input_hash,
             code_hash = inv_row$code_hash), receipt)
    },
    piperr = function(cnd) {
      if (pd_deflate_fatal_condition(cnd)) stop(cnd)
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
      list(success = FALSE, condition = new_stage_condition_record(
        cnd, "error", code = err_class, stage = "deflate", pip_id = pip_id,
        entity_id = pip_id, operation = "transform", recoverable = TRUE
      ))
    },
    error = function(cnd) {
      if (pd_deflate_fatal_condition(cnd)) stop(cnd)
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
        normalized <- original_cnd
      } else {
        err_class <- "unknown_error"
        normalized <- cnd
      }
      list(success = FALSE, condition = new_stage_condition_record(
        normalized, "error", code = err_class, stage = "deflate",
        pip_id = pip_id, entity_id = pip_id, operation = "transform",
        recoverable = TRUE
      ))
    }
  )
}

pd_deflate_fatal_condition <- function(cnd) {
  recoverable <- c(
    "validate_deflation_input", "load_deflation_aux", "pd_deflation_exact",
    "pd_deflation", "add_ppp", "add_cpi", "adjust_population"
  )
  !any(class(cnd) %in% recoverable)
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
