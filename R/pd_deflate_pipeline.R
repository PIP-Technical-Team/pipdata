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
  actions <- execution$plan$actions[stage == "deflate"]
  runtime <- new.env(parent = emptyenv())
  runtime$execution <- execution
  context <- pd_stage_context(
    execution, run_id, options, actions, force_surveys
  )
  context$runtime <- runtime
  validate_pipeline_context(context)
  prepared <- tryCatch(
    pd_run_deflate_stage_prepared(
      execution, actions, run_id, context, inv, options, verbose
    ),
    error = function(cnd) {
      primary <<- cnd
      rlang::cnd_signal(cnd)
    }
  )
  primary <- prepared$error
  execution <- prepared$execution
  inv <- prepared$master
  outcome <- prepared$outcome
  runtime$execution <- execution
  result <- pd_stage_outcome_result(
    outcome,
    context,
    execution,
    terminal = isTRUE(prepared$terminal),
    log_ref = list(
      name = "pipdata_log",
      run_id = run_id,
      summary_discriminator = if (sum(
        outcome$units$status %in% c("success", "failed")
      )) "deflate_summary_inf" else NA_character_,
      log_checkpoint = NULL
    )
  )
  if (result$counts[["attempted"]] > 0L) pd_log_deflate_summary(result)
  list(result = result, master = inv, context = context)
}

pd_run_deflate_stage_prepared <- function(
  execution, actions, run_id, context, master, options, verbose = FALSE,
  checkpoint_callback = NULL
) {
  actions <- pd_prepared_stage_actions(actions, "deflate", run_id, context)
  outcome <- pd_new_stage_outcome("deflate", execution$manifest_identity)
  blocked <- if ("scheduling_state" %in% names(actions)) {
    actions$scheduling_state == "blocked"
  } else {
    rep(FALSE, nrow(actions))
  }
  blocked[is.na(blocked)] <- FALSE
  for (i in which(actions$action == "none" & !blocked)) {
    outcome$units <- rbind(
      outcome$units,
      pd_stage_unit_row(actions[i], "deflate", "cached", "current")
    )
  }
  for (i in which(blocked)) {
    outcome$units <- rbind(
      outcome$units,
      pd_stage_unit_row(
        actions[i], "deflate", "skipped", "upstream_failed"
      )
    )
  }
  runnable <- actions[action != "none" & !blocked]
  receipts <- list()
  writer <- function(alias, id) {
    function(candidate, lease) pd_save_receipt(candidate, id, alias, verbose, lease)
  }
  primary <- NULL
  if (nrow(runnable)) {
    master <- data.table::as.data.table(data.table::copy(master))
    if (anyDuplicated(runnable$pip_id) || anyDuplicated(master$pip_id)) {
      rlang::abort("Deflation actions and inventory must match one-to-one.",
                   class = "pipdata_deflation_action_invalid")
    }
    inventory_rows <- master[match(runnable$pip_id, master$pip_id)]
    if (nrow(inventory_rows) != nrow(runnable) ||
        anyNA(inventory_rows$pip_id)) {
      rlang::abort("Fresh deflation actions are absent from the inventory.",
                   class = "pipdata_deflation_action_invalid")
    }
    comparisons <- list(
      data_version_id = "version_id_data", data_hash = "content_hash_data",
      metadata_version_id = "version_id_metadata",
      metadata_hash = "content_hash_metadata"
    )
    if (!all(names(comparisons) %in% names(runnable)) ||
        !all(unlist(comparisons) %in% names(inventory_rows)) ||
        any(vapply(names(comparisons), function(plan_field) {
          inventory_field <- comparisons[[plan_field]]
          anyNA(runnable[[plan_field]]) ||
            !identical(
              runnable[[plan_field]], inventory_rows[[inventory_field]]
            )
        }, logical(1L)))) {
      rlang::abort("Fresh deflation actions lack exact input receipts.",
                   class = "pipdata_deflation_action_invalid")
    }
    inventory_fields <- setdiff(names(inventory_rows), names(runnable))
    runnable <- cbind(
      runnable, inventory_rows[, inventory_fields, with = FALSE]
    )
    runnable[, `:=`(
      version_id_data = data_version_id,
      content_hash_data = data_hash,
      version_id_metadata = metadata_version_id,
      content_hash_metadata = metadata_hash
    )]
    action_rows <- split(runnable, seq_len(nrow(runnable)))
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
        worker_result <- pd_execute_deflate(action, verbose)
        if (isTRUE(worker_result$success)) {
          pending_ids <<- c(pending_ids, action$pip_id)
          pending_started[[action$pip_id]] <<- stamp
          receipts[[action$pip_id]] <<- worker_result
        } else {
          if (is.null(worker_result$condition)) {
            rlang::abort(
              "Deflation worker returned an unclassified failure.",
              class = "pipdata_deflation_worker_result_invalid"
            )
          }
          outcome_record <- worker_result$condition
          outcome_units <- pd_deflate_unit_row(
            action, "failed", "entity_failed", stamp, Sys.time()
          )
          outcome$units <<- rbind(outcome$units, outcome_units)
          outcome$errors[[length(outcome$errors) + 1L]] <<- outcome_record
          pd_log_stage_condition(run_id, outcome_record)
          persisted <- pd_persist_failed_invalidation(
            execution, master, action,
            writer("pip_inv", "pip_release_inventory"),
            writer("pip_master", "pip_master_inventory")
          )
          master <<- persisted$candidate
          execution <<- persisted$execution
          if (identical(options$entity_error_policy, "abort")) {
            rlang::cnd_signal(outcome_record)
          }
        }
        active_id <<- NULL
        active_started <<- NULL
        worker_result
      },
      checkpoint = function(results) {
        finalized <- pd_finalize_checkpoint(
          execution, master, "deflate",
          data.table::rbindlist(results, fill = TRUE),
          writer("pip_inv", "pip_release_inventory"),
          writer("pip_master", "pip_master_inventory")
        )
        master <<- finalized$candidate
        execution <<- finalized$execution
        outcome$checkpoint_generations <<- c(
          outcome$checkpoint_generations,
          execution$manifest_identity$generation
        )
        outcome$manifest_after <<- execution$manifest_identity
        for (id in pending_ids) {
          action <- runnable[pip_id == id][1L]
          outcome$units <<- rbind(outcome$units, pd_deflate_unit_row(
            action, "success", pd_action_reason_codes(execution, action),
            pending_started[[id]], Sys.time(), receipts[[id]]$content_hash
          ))
          outcome$receipts[[id]] <<-
            data.table::as.data.table(receipts[[id]])
        }
        pending_ids <<- character()
        pending_started <<- list()
        if (is.function(checkpoint_callback)) {
          execution <<- checkpoint_callback(execution, master)
        }
      }, checkpoint_n = options$checkpoint_size,
      checkpoint_seconds = options$checkpoint_seconds
    )
    caught <- tryCatch({ run(); NULL }, error = function(cnd) cnd)
    if (!is.null(caught)) {
      if (pd_is_pipeline_cancellation(caught)) {
        rlang::cnd_signal(caught)
      }
      primary <- caught
      if (identical(options$fatal_error_policy, "abort")) {
        rlang::cnd_signal(caught)
      }
      completed <- Sys.time()
      for (id in pending_ids) {
        action <- runnable[pip_id == id][1L]
        outcome$units <- rbind(outcome$units, pd_deflate_unit_row(
          action, "failed", "checkpoint_uncommitted",
          pending_started[[id]], completed
        ))
      }
      if (!is.null(active_id) && !active_id %in% outcome$units$entity_id) {
        action <- runnable[pip_id == active_id][1L]
        outcome$units <- rbind(outcome$units, pd_deflate_unit_row(
          action, "failed", "fatal_uncommitted", active_started, completed
        ))
      }
      remaining_ids <- setdiff(runnable$pip_id, outcome$units$entity_id)
      for (id in remaining_ids) {
        action <- runnable[pip_id == id][1L]
        outcome$units <- rbind(outcome$units, pd_deflate_unit_row(
          action, "skipped", "upstream_failed",
          as.POSIXct(NA, tz = "UTC"), as.POSIXct(NA, tz = "UTC")
        ))
      }
      outcome$errors[[length(outcome$errors) + 1L]] <- new_stage_condition_record(
        caught, "error", stage = "deflate", operation = "stage",
        recoverable = FALSE
      )
    }
  }
  data.table::setorder(outcome$units, stage, entity_id)
  outcome$completed_at <- pd_utc_time(Sys.time())
  list(
    execution = execution,
    master = master,
    context = context,
    outcome = outcome,
    terminal = !is.null(primary),
    error = primary
  )
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
  survey <- if (identical(record$stage, "clean")) {
    record$survey_id
  } else {
    record$pip_id
  }
  tryCatch(
    pipfun::log_add(
      event = "error", message = record$message, name = "pipdata_log",
      args = list(run_id = run_id, stage = record$stage,
                  entity_id = record$entity_id,
                  condition_id = record$condition_id),
      logmeta = list(
        error = record$code, survey = survey,
        condition_id = record$condition_id,
        condition_msg = record$message
      )
    ),
    error = function(cnd) invisible(NULL)
  )
  invisible(record)
}

pd_log_deflate_summary <- function(result) {
  successful <- result$units[status == "success", pip_id]
  failed <- result$units[status == "failed", pip_id]
  tryCatch(
    pipfun::log_add(
      event = "info", message = "Deflation stage completed.",
      name = "pipdata_log",
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
    ),
    error = function(cnd) invisible(NULL)
  )
  invisible(result)
}

pd_persist_failed_invalidation <- function(execution, master, action,
                                           release_writer, master_writer) {
  invalidate <- pd_invalidate_failed_action
  candidate <- if ("emit_log" %in% names(formals(invalidate))) {
    invalidate(master, action, emit_log = FALSE)
  } else {
    invalidate(master, action)
  }
  advanced_receipts <- data.table::data.table()
  assert_fence <- function() {
    fence <- pd_assert_execution_fence
    if ("advanced_receipts" %in% names(formals(fence))) {
      fence(execution, advanced_receipts)
    } else {
      fence(execution)
    }
  }
  assert_fence()
  release_receipt <- release_writer(
    pd_release_inventory_candidate(candidate), execution$lease
  )
  if (!isTRUE(release_receipt$success)) {
    rlang::abort("Failed invalidation release write was not verified.",
                 class = "pipdata_failed_invalidation_release_error")
  }
  if (all(c("alias", "path", "content_hash") %in% names(release_receipt))) {
    pd_revalidate_receipt(release_receipt)
  }
  advanced_receipts <- data.table::rbindlist(list(
    advanced_receipts,
    data.table::as.data.table(release_receipt)
  ), fill = TRUE)
  candidate[, latest_release_version_id := release_receipt$version_id]
  assert_fence()
  master_receipt <- master_writer(candidate, execution$lease)
  if (!isTRUE(master_receipt$success)) {
    rlang::abort("Failed invalidation master write was not verified.",
                 class = "pipdata_failed_invalidation_master_error")
  }
  if (all(c("alias", "path", "content_hash") %in% names(master_receipt))) {
    pd_revalidate_receipt(master_receipt)
  }
  advanced_receipts <- data.table::rbindlist(list(
    advanced_receipts,
    data.table::as.data.table(master_receipt)
  ), fill = TRUE)
  assert_fence()
  execution <- pd_advance_execution_state(
    execution, candidate, advanced_receipts
  )
  list(
    candidate = candidate,
    execution = execution,
    release_receipt = release_receipt,
    master_receipt = master_receipt,
    advanced_receipts = advanced_receipts
  )
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
