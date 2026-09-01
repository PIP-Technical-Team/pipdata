pd_normalize_aux_measures <- function(aux_measures) {
  allowed <- c("pfw", "cpi", "ppp", "pop", "gdp", "pce")
  if (!is.character(aux_measures) || anyNA(aux_measures) ||
      any(!nzchar(trimws(aux_measures)))) {
    rlang::abort(
      "aux_measures must contain nonempty measure names.",
      class = "pipdata_dependency_input_invalid"
    )
  }
  measures <- unique(tolower(trimws(aux_measures)))
  if (any(!measures %in% allowed)) {
    rlang::abort(
      "aux_measures contains an unsupported measure.",
      class = "pipdata_dependency_input_invalid"
    )
  }
  measures
}

pd_stage_context <- function(execution, run_id, options, actions,
                             force_surveys = character()) {
  new_pipeline_context(
    execution,
    run_id,
    options,
    list(
      survey_id = sort(unique(actions$survey_id[!is.na(actions$survey_id)])),
      pip_id = sort(unique(actions$pip_id[!is.na(actions$pip_id)])),
      force_requested = sort(unique(force_surveys %||% character()))
    ),
    pd_pipeline_storage(execution$context)
  )
}

pd_prepared_stage_actions <- function(actions, stage, run_id, context) {
  actions <- data.table::as.data.table(data.table::copy(actions))
  invalid <- !is.character(run_id) || length(run_id) != 1L ||
    is.na(run_id) || !nzchar(run_id) || !identical(context$run_id, run_id)
  if (nrow(actions)) {
    invalid <- invalid || any(actions$stage != stage) ||
      anyDuplicated(actions[, .(stage, entity_id)]) ||
      any(!actions$action %in% .PD_ACTIONS)
  }
  if (invalid) {
    rlang::abort(
      "Prepared stage actions do not match their accepted context.",
      class = "pipdata_dependency_action_not_runnable"
    )
  }
  actions
}

pd_action_reason_codes <- function(execution, action) {
  reasons <- execution$plan$reasons %||% pd_empty_reasons()
  selected_stage <- action$stage[[1L]]
  selected_entity <- action$entity_id[[1L]]
  sort(unique(reasons[
    stage == selected_stage & entity_id == selected_entity, reason
  ]))
}

pd_condition_root <- function(cnd) {
  while (!is.null(cnd$parent)) cnd <- cnd$parent
  cnd
}

pd_condition_allowlisted <- function(cnd, allowed) {
  any(class(pd_condition_root(cnd)) %in% allowed)
}

pd_is_pipeline_cancellation <- function(cnd) {
  inherits(cnd, c(
    "interrupt", "pipdata_dlw_cancellation", "pipdata_dlw_cancelled",
    "pipdata_user_cancelled", "pipdata_pipeline_cancellation"
  ))
}

pd_terminalize_stage_outcome <- function(
  outcome, actions, cnd, pending_ids = character(), active_id = NULL,
  pending_started = list(), active_started = NULL
) {
  completed_at <- Sys.time()
  for (selected_entity in pending_ids) {
    if (!selected_entity %in% outcome$units$entity_id) {
      action <- actions[entity_id == selected_entity][1L]
      outcome$units <- rbind(
        outcome$units,
        pd_stage_unit_row(
          action, outcome$stage, "failed", "checkpoint_uncommitted",
          pending_started[[selected_entity]] %||% completed_at, completed_at
        )
      )
    }
  }
  if (!is.null(active_id) && !active_id %in% outcome$units$entity_id) {
    action <- actions[entity_id == active_id][1L]
    outcome$units <- rbind(
      outcome$units,
      pd_stage_unit_row(
        action, outcome$stage, "failed", "fatal_uncommitted",
        active_started %||% completed_at, completed_at
      )
    )
  }
  remaining <- actions[
    action != "none" & !entity_id %in% outcome$units$entity_id
  ]
  for (i in seq_len(nrow(remaining))) {
    outcome$units <- rbind(
      outcome$units,
      pd_stage_unit_row(
        remaining[i], outcome$stage, "skipped", "upstream_failed"
      )
    )
  }
  outcome$errors[[length(outcome$errors) + 1L]] <-
    new_stage_condition_record(
      cnd,
      "error",
      stage = outcome$stage,
      entity_id = active_id %||% NA_character_,
      operation = outcome$stage,
      recoverable = FALSE
    )
  outcome$completed_at <- pd_utc_time(completed_at)
  data.table::setorder(outcome$units, stage, entity_id)
  return(outcome)
}

.PD_CLEAN_RECOVERABLE_CLASSES <- c(
  "yr_wrng", "report_lvl", "info_pfw", "no_unq_pfw", "no_wlf_tp",
  "dom_var", "alt_welf_issue", "gd_type_miss"
)

.PD_METADATA_RECOVERABLE_CLASSES <- c(
  "report_lvl", "no_wlf_tp", "dom_var", "pipdata_metadata_entity_error"
)

pd_run_clean_stage_prepared <- function(
  execution, actions, run_id, context, master, inv, options,
  recode_spec = NULL, verbose = FALSE, checkpoint_callback = NULL
) {
  actions <- pd_prepared_stage_actions(actions, "clean", run_id, context)
  inv <- data.table::as.data.table(data.table::copy(inv))
  outcome <- pd_new_stage_outcome("clean", execution$manifest_identity)
  for (i in which(actions$action == "none")) {
    outcome$units <- rbind(
      outcome$units,
      pd_stage_unit_row(actions[i], "clean", "cached", "current")
    )
  }
  runnable <- actions[action != "none"]
  pending_ids <- character()
  pending_started <- list()
  active_id <- NULL
  active_started <- NULL
  primary <- NULL
  for (i in seq_len(nrow(runnable))) {
    action <- runnable[i]
    active_id <- action$entity_id[[1L]]
    active_started <- Sys.time()
    caught <- tryCatch({
      inv_row <- inv[survey_id == action$survey_id[[1L]]]
      if (nrow(inv_row) != 1L) {
        rlang::abort(
          "Accepted clean action lacks one completed-validation row.",
          class = "pipdata_dependency_facts_invalid"
        )
      }
      result <- pd_execute_clean(
        action, inv_row, execution, recode_spec, verbose
      )
      worker_failed <- !isTRUE(result$success)
      if (worker_failed) {
        if (is.null(result$condition)) {
          rlang::abort(
            "Clean worker returned an unclassified failure.",
            class = "pipdata_clean_worker_result_invalid"
          )
        }
        outcome$units <- rbind(
          outcome$units,
          pd_stage_unit_row(
            action, "clean", "failed", "entity_failed",
            active_started, Sys.time()
          )
        )
        outcome$errors[[length(outcome$errors) + 1L]] <- result$condition
        pd_log_stage_condition(run_id, result$condition)
        persisted <- pd_persist_failed_invalidation(
          execution,
          master,
          action,
          pd_inventory_writer("pip_inv", "pip_release_inventory", verbose),
          pd_inventory_writer("pip_master", "pip_master_inventory", verbose)
        )
        master <- persisted$candidate
        execution <- persisted$execution
        if (identical(options$entity_error_policy, "abort")) {
          rlang::cnd_signal(result$condition)
        }
        active_id <- NULL
        active_started <- NULL
      } else {
        receipt_set <- pd_clean_receipt_set(
          result$receipts, action$expected_pip_ids[[1L]]
        )
        pending_ids <- active_id
        pending_started[[active_id]] <- active_started
        finalized <- pd_finalize_checkpoint(
          execution,
          master,
          "clean",
          result$receipts,
          pd_inventory_writer("pip_inv", "pip_release_inventory", verbose),
          pd_inventory_writer("pip_master", "pip_master_inventory", verbose),
          survey_id = action$survey_id[[1L]],
          expected_pip_ids = action$expected_pip_ids[[1L]]
        )
        master <- finalized$candidate
        execution <- finalized$execution
        outcome$checkpoint_generations <- c(
          outcome$checkpoint_generations,
          execution$manifest_identity$generation
        )
        outcome$manifest_after <- execution$manifest_identity
        outcome$receipts[[active_id]] <- receipt_set$receipts
        outcome$units <- rbind(
          outcome$units,
          pd_stage_unit_row(
            action, "clean", "success",
            pd_action_reason_codes(execution, action),
            active_started, Sys.time(), receipt_set$output_hash
          )
        )
        pending_ids <- character()
        pending_started <- list()
        active_id <- NULL
        active_started <- NULL
        if (is.function(checkpoint_callback)) {
          execution <- checkpoint_callback(execution, master)
        }
        rm(result)
        gc(verbose = FALSE)
      }
      NULL
    }, error = function(cnd) cnd)
    if (inherits(caught, "condition")) {
      if (pd_is_pipeline_cancellation(caught)) {
        rlang::cnd_signal(caught)
      }
      primary <- caught
      if (identical(options$fatal_error_policy, "abort")) {
        rlang::cnd_signal(caught)
      }
      outcome <- pd_terminalize_stage_outcome(
        outcome, actions, caught, pending_ids, active_id,
        pending_started, active_started
      )
      break
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

pd_metadata_reconstruct_reasons <- function() {
  c(
    "new_entity", "unknown_provenance", "output_missing", "output_drift",
    "metadata_code_changed", "upstream_output_changed", "clean_code_changed",
    "recode_spec_changed", "dlw_changed", "pfw_changed",
    "legacy_input_changed", "forced"
  )
}

pd_assert_metadata_prerequisite <- function(action, master, manifest) {
  master <- data.table::as.data.table(master)
  selected_pip_id <- action$pip_id[[1L]]
  row <- master[pip_id == selected_pip_id]
  required <- c("data_version_id", "data_hash")
  valid_master <- nrow(row) == 1L &&
    all(c("version_id_data", "content_hash_data") %in% names(row))
  if (!valid_master || !all(required %in% names(action)) ||
      anyNA(action[, ..required]) ||
      !identical(action$data_version_id[[1L]], row$version_id_data[[1L]]) ||
      !identical(action$data_hash[[1L]], row$content_hash_data[[1L]])) {
    rlang::abort(
      "Metadata action differs from its exact clean prerequisite.",
      class = "pipdata_metadata_action_invalid"
    )
  }
  receipt <- pd_committed_output_receipt(
    manifest, "clean", row$survey_id[[1L]], selected_pip_id
  )
  if (is.null(receipt) ||
      !identical(receipt$version_id, action$data_version_id[[1L]]) ||
      !identical(receipt$content_hash, action$data_hash[[1L]])) {
    rlang::abort(
      "Metadata action lacks an exact committed clean receipt.",
      class = "pipdata_metadata_action_invalid"
    )
  }
  invisible(action)
}

pd_run_metadata_stage_prepared <- function(
  execution, actions, run_id, context, master, options, verbose = FALSE,
  checkpoint_callback = NULL
) {
  actions <- pd_prepared_stage_actions(actions, "metadata", run_id, context)
  outcome <- pd_new_stage_outcome("metadata", execution$manifest_identity)
  runnable <- list()
  for (i in seq_len(nrow(actions))) {
    action <- actions[i]
    blocked <- "scheduling_state" %in% names(action) &&
      identical(action$scheduling_state[[1L]], "blocked")
    if (blocked) {
      outcome$units <- rbind(
        outcome$units,
        pd_stage_unit_row(
          action, "metadata", "skipped", "upstream_failed"
        )
      )
    } else if (identical(action$action[[1L]], "none")) {
      outcome$units <- rbind(
        outcome$units,
        pd_stage_unit_row(action, "metadata", "cached", "current")
      )
    }
  }
  pending <- list()
  pending_started <- list()
  active_id <- NULL
  active_started <- NULL
  primary <- NULL
  caught <- tryCatch({
    candidates <- actions[
      action != "none" & !entity_id %in% outcome$units$entity_id
    ]
    for (i in seq_len(nrow(candidates))) {
      action <- candidates[i]
      active_id <- action$entity_id[[1L]]
      active_started <- Sys.time()
      pd_assert_metadata_prerequisite(action, master, execution$manifest)
      reasons <- pd_action_reason_codes(execution, action)
      metadata_fields <- intersect(
        c("metadata_version_id", "metadata_hash"), names(action)
      )
      reconstruct <- any(reasons %in% pd_metadata_reconstruct_reasons()) ||
        length(metadata_fields) != 2L || anyNA(action[, ..metadata_fields])
      action[, reconstruct_base_metadata := reconstruct]
      runnable[[length(runnable) + 1L]] <- action
      active_id <- NULL
      active_started <- NULL
    }
    if (length(runnable)) {
      pd_run_checkpoint_batches(
      runnable,
      worker = function(action) {
        active_id <<- action$entity_id[[1L]]
        active_started <<- Sys.time()
        result <- tryCatch(
          pd_execute_metadata(
            action, execution$snapshot, execution, NULL, verbose
          ),
          error = function(cnd) {
            if (!pd_condition_allowlisted(
              cnd, .PD_METADATA_RECOVERABLE_CLASSES
            )) {
              rlang::cnd_signal(cnd)
            }
            list(
              success = FALSE,
              condition = new_stage_condition_record(
                pd_condition_root(cnd), "error", stage = "metadata",
                entity_id = action$entity_id[[1L]],
                survey_id = action$survey_id[[1L]],
                pip_id = action$pip_id[[1L]], operation = "metadata",
                recoverable = TRUE
              )
            )
          }
        )
        if (!isTRUE(result$success)) {
          if (is.null(result$condition)) {
            rlang::abort(
              "Metadata receipt was not verified.",
              class = "pipdata_metadata_receipt_invalid"
            )
          }
          outcome$units <<- rbind(
            outcome$units,
            pd_stage_unit_row(
              action, "metadata", "failed", "entity_failed",
              active_started, Sys.time()
            )
          )
          outcome$errors[[length(outcome$errors) + 1L]] <<- result$condition
          pd_log_stage_condition(run_id, result$condition)
          persisted <- pd_persist_failed_invalidation(
            execution,
            master,
            action,
            pd_inventory_writer("pip_inv", "pip_release_inventory", verbose),
            pd_inventory_writer("pip_master", "pip_master_inventory", verbose)
          )
          master <<- persisted$candidate
          execution <<- persisted$execution
          if (identical(options$entity_error_policy, "abort")) {
            rlang::cnd_signal(result$condition)
          }
        } else {
          pending[[action$entity_id[[1L]]]] <<- result
          pending_started[[action$entity_id[[1L]]]] <<- active_started
        }
        active_id <<- NULL
        active_started <<- NULL
        result
      },
      checkpoint = function(results) {
        finalized <- pd_finalize_checkpoint(
          execution,
          master,
          "metadata",
          data.table::rbindlist(results, fill = TRUE),
          pd_inventory_writer("pip_inv", "pip_release_inventory", verbose),
          pd_inventory_writer("pip_master", "pip_master_inventory", verbose)
        )
        master <<- finalized$candidate
        execution <<- finalized$execution
        outcome$checkpoint_generations <<- c(
          outcome$checkpoint_generations,
          execution$manifest_identity$generation
        )
        outcome$manifest_after <<- execution$manifest_identity
        for (selected_entity in names(pending)) {
          action <- actions[entity_id == selected_entity]
          receipt <- data.table::as.data.table(pending[[selected_entity]])
          outcome$receipts[[selected_entity]] <<- receipt
          outcome$units <<- rbind(
            outcome$units,
            pd_stage_unit_row(
              action, "metadata", "success",
              pd_action_reason_codes(execution, action),
              pending_started[[selected_entity]], Sys.time(),
              receipt$content_hash[[1L]]
            )
          )
        }
        pending <<- list()
        pending_started <<- list()
        if (is.function(checkpoint_callback)) {
          execution <<- checkpoint_callback(execution, master)
        }
      },
      checkpoint_n = options$checkpoint_size,
        checkpoint_seconds = options$checkpoint_seconds
      )
    }
    NULL
  }, error = function(cnd) cnd)
  if (inherits(caught, "condition")) {
    if (pd_is_pipeline_cancellation(caught)) {
      rlang::cnd_signal(caught)
    }
    primary <- caught
    if (identical(options$fatal_error_policy, "abort")) {
      rlang::cnd_signal(caught)
    }
    outcome <- pd_terminalize_stage_outcome(
      outcome,
      actions,
      caught,
      pending_ids = names(pending),
      active_id = active_id,
      pending_started = pending_started,
      active_started = active_started
    )
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
