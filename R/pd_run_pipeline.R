.PD_PIPELINE_MEASURES <- c("pfw", "cpi", "ppp", "pop", "gdp", "pce")

pd_validate_pipeline_arguments <- function(
  force, verbose, force_surveys, bootstrap, bootstrap_entities,
  checkpoint_size, checkpoint_seconds
) {
  logical_scalar <- function(value, name) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      rlang::abort(
        paste(name, "must be one nonmissing logical value."),
        class = "pipdata_pipeline_argument_error"
      )
    }
  }
  selector <- function(value, name) {
    if (is.null(value)) {
      return(character())
    }
    if (!is.character(value) || anyNA(value) ||
        any(!nzchar(trimws(value)))) {
      rlang::abort(
        paste(name, "must contain nonempty character identifiers."),
        class = "pipdata_pipeline_argument_error"
      )
    }
    return(unique(value))
  }
  logical_scalar(force, "force")
  logical_scalar(verbose, "verbose")
  logical_scalar(bootstrap, "bootstrap")
  if (isTRUE(force) && !is.null(force_surveys)) {
    cli::cli_abort(.force_exclusive_msg, class = "piperr")
  }
  if (!isTRUE(bootstrap) && !is.null(bootstrap_entities)) {
    cli::cli_abort(
      "bootstrap_entities requires bootstrap = TRUE.",
      class = "pipdata_bootstrap_selector_error"
    )
  }
  valid_checkpoint_size <- is.numeric(checkpoint_size) &&
    length(checkpoint_size) == 1L && !is.na(checkpoint_size) &&
    is.finite(checkpoint_size) && checkpoint_size > 0 &&
    checkpoint_size == floor(checkpoint_size) &&
    checkpoint_size <= .Machine$integer.max
  if (!valid_checkpoint_size) {
    rlang::abort(
      "checkpoint_size must be one positive whole number.",
      class = "pipdata_pipeline_argument_error"
    )
  }
  valid_checkpoint_seconds <- is.numeric(checkpoint_seconds) &&
    length(checkpoint_seconds) == 1L && !is.na(checkpoint_seconds) &&
    checkpoint_seconds > 0
  if (!valid_checkpoint_seconds) {
    rlang::abort(
      "checkpoint_seconds must be Inf or one positive number.",
      class = "pipdata_pipeline_argument_error"
    )
  }
  return(list(
    force = force,
    verbose = verbose,
    force_surveys = selector(force_surveys, "force_surveys"),
    bootstrap = bootstrap,
    bootstrap_entities = selector(
      bootstrap_entities, "bootstrap_entities"
    ),
    checkpoint_size = as.integer(checkpoint_size),
    checkpoint_seconds = as.numeric(checkpoint_seconds)
  ))
}

pd_resolve_pipeline_selectors <- function(selectors, inv, master, name) {
  if (!length(selectors)) {
    return(character())
  }
  resolved <- resolve_force_surveys(
    selectors,
    inv_svy_full = data.table::as.data.table(data.table::copy(inv)),
    dt_master = data.table::as.data.table(data.table::copy(master)),
    verbose = FALSE
  )
  if (length(resolved$unknown)) {
    rlang::abort(
      paste(
        "Unknown", name, "selector(s):",
        paste(sort(unique(resolved$unknown)), collapse = ", ")
      ),
      class = paste0("pipdata_", name, "_selector_error"),
      unknown_identifiers = sort(unique(resolved$unknown))
    )
  }
  return(sort(unique(resolved$survey_ids)))
}

pd_accept_stage_wave <- function(execution, stage,
                                 blocked_entities = character()) {
  accepted <- execution
  accepted$plan <- execution$plan
  selected_stage <- stage
  accepted$plan$actions <- data.table::copy(
    execution$plan$actions[stage == selected_stage]
  )
  keys <- paste(
    accepted$plan$actions$stage,
    accepted$plan$actions$entity_id
  )
  accepted$plan$reasons <- data.table::copy(
    execution$plan$reasons[
      paste(stage, entity_id) %in% keys
    ]
  )
  pd_validate_plan(accepted$plan)
  if (length(blocked_entities)) {
    accepted$plan$actions[
      entity_id %in% blocked_entities,
      scheduling_state := "blocked"
    ]
  }
  return(accepted)
}

pd_boundary_stage_outcome <- function(execution, stage, cnd) {
  actions <- execution$plan$actions
  outcome <- pd_new_stage_outcome(stage, execution$manifest_identity)
  blocked <- if ("scheduling_state" %in% names(actions)) {
    actions$scheduling_state == "blocked"
  } else {
    rep(FALSE, nrow(actions))
  }
  blocked[is.na(blocked)] <- FALSE
  for (i in which(blocked)) {
    outcome$units <- rbind(
      outcome$units,
      pd_stage_unit_row(
        actions[i], stage, "skipped", "upstream_failed"
      )
    )
  }
  for (i in which(actions$action == "none" & !blocked)) {
    outcome$units <- rbind(
      outcome$units,
      pd_stage_unit_row(actions[i], stage, "cached", "current")
    )
  }
  remaining <- actions[!entity_id %in% outcome$units$entity_id]
  active_id <- if (nrow(remaining)) remaining$entity_id[[1L]] else NULL
  outcome <- pd_terminalize_stage_outcome(
    outcome,
    actions,
    cnd,
    active_id = active_id,
    active_started = Sys.time()
  )
  outcome$errors <- list()
  return(outcome)
}

pd_pipeline_stage_log_ref <- function(stage, run_id, outcome) {
  attempted <- sum(outcome$units$status %in% c("success", "failed"))
  discriminator <- switch(
    stage,
    clean = if (attempted) "process_summary_inf" else NA_character_,
    metadata = NA_character_,
    deflate = if (attempted) "deflate_summary_inf" else NA_character_
  )
  return(list(
    name = "pipdata_log",
    run_id = run_id,
    summary_discriminator = discriminator,
    log_checkpoint = NULL
  ))
}

pd_pipeline_run_condition <- function(cnd, stage) {
  new_stage_condition_record(
    cnd,
    "error",
    stage = stage,
    operation = "run_boundary",
    recoverable = FALSE
  )
}

pd_pipeline_summary_logmeta <- function(result) {
  stage_status <- function(stage) {
    stage_result <- result$stage_results[[stage]]
    if (is.null(stage_result)) NA_character_ else stage_result$status
  }
  generation <- function(identity) {
    if (is.null(identity)) NA_integer_ else as.integer(identity$generation)
  }
  return(list(
    info = "pipeline_run_summary_inf",
    run_id = result$run_id,
    status = result$status,
    terminal = result$terminal,
    n_selected = result$counts$selected,
    n_attempted = result$counts$attempted,
    n_success = result$counts$succeeded,
    n_failed = result$counts$failed,
    n_cached = result$counts$cached,
    n_blocked = result$counts$blocked,
    clean_status = stage_status("clean"),
    metadata_status = stage_status("metadata"),
    deflate_status = stage_status("deflate"),
    manifest_before_generation = generation(result$manifest_before),
    manifest_after_generation = generation(result$manifest_after),
    started_at = result$started_at,
    completed_at = result$completed_at
  ))
}

pd_log_pipeline_summary <- function(result) {
  tryCatch(
    pipfun::log_add(
      event = "info",
      message = "Pipeline run completed.",
      name = "pipdata_log",
      args = list(
        run_id = result$run_id,
        status = result$status,
        terminal = result$terminal
      ),
      logmeta = pd_pipeline_summary_logmeta(result)
    ),
    error = function(cnd) invisible(NULL)
  )
  invisible(result)
}

pd_log_clean_summary <- function(result) {
  tryCatch(
    pipfun::log_add(
      event = "info",
      message = "Processing complete.",
      name = "pipdata_log",
      args = list(run_id = result$run_id, stage = "clean"),
      logmeta = list(
        info = "process_summary_inf",
        run_id = result$run_id,
        n_total = result$counts[["attempted"]],
        n_success = result$counts[["succeeded"]],
        n_failed = result$counts[["failed"]],
        surveys_success = result$units[status == "success", survey_id]
      )
    ),
    error = function(cnd) invisible(NULL)
  )
  invisible(result)
}

pd_final_retained_manifest <- function(execution) {
  retained <- pd_manifest_read(execution$context, allow_absent = TRUE)
  identity <- if (inherits(retained, "pipdata_manifest_absent")) {
    NULL
  } else {
    attr(retained, "manifest_identity")
  }
  if (!identical(identity, execution$manifest_identity)) {
    rlang::abort(
      "The final retained manifest differs from the accepted execution.",
      class = "pipdata_manifest_parent_changed"
    )
  }
  if (!inherits(retained, "pipdata_manifest_absent")) {
    execution$manifest <- retained
  }
  return(execution)
}

#' Run the staged PIP data pipeline incrementally
#'
#' Runs the durable `clean`, `metadata`, and `deflate` stages in topological
#' waves under one dependency-manifest writer lease. Current nodes are returned
#' as cached units. Downstream waves are accepted only after committed upstream
#' receipts have been reloaded into the authoritative dependency facts.
#'
#' @param inv The complete completed-validation inventory. If `NULL`, the
#'   current durable validation inventory is loaded with
#'   [pipload::load_gmd_valid_inv()].
#' @param force Logical scalar. Rebuild all selected nodes and temporarily use
#'   Stamp timestamp versioning. Mutually exclusive with `force_surveys`.
#' @param verbose Logical scalar passed to pipeline storage operations.
#' @param force_surveys Optional character vector of exact `survey_id` or
#'   `pip_id` selectors. Selected chains are added to ordinary invalidation.
#' @param bootstrap Logical scalar. Explicitly permit unknown C2 provenance.
#' @param bootstrap_entities Optional character vector of bootstrap survey or
#'   PIP selectors. A PIP selector includes its owning clean survey and complete
#'   atomic output chain. Requires `bootstrap = TRUE`.
#' @param checkpoint_size Positive whole-number metadata and deflate checkpoint
#'   batch size.
#' @param checkpoint_seconds `Inf` or a positive numeric checkpoint interval in
#'   seconds.
#'
#' @return A visible `pipdata_pipeline_result`. This differs intentionally from
#'   the legacy stage wrappers, which continue to return master inventories.
#'
#' @details
#' The C2 dependency manifest and exact Stamp receipts are the only currentness
#' authority. The function does not persist a run cursor. A restart creates a
#' new run and replans from the latest valid manifest. Recoverable entity errors
#' block only their descendants. Unknown storage, lease, fence, receipt, and
#' checkpoint failures stop later writes and are captured only after a complete
#' stage context exists. Interrupts and explicit cancellation conditions always
#' propagate.
#'
#' The only durable nodes are `clean:<survey_id>`, `metadata:<pip_id>`, and
#' `deflate:<pip_id>`. Load, PFW merge, recode, auxiliary attachment, and save
#' helpers are code-fingerprint components, not separately cached nodes. Each
#' selected node is reported as current or stale/forced and then as cached,
#' runnable, successful, failed, skipped, or blocked. Cached clean nodes do not
#' load household artifacts.
#'
#' `force = TRUE` rebuilds the complete selected graph. `force_surveys` adds
#' the selected survey or PIP chain to ordinary invalidation without suppressing
#' unrelated stale work. An absent manifest or unknown pre-C2 provenance
#' requires `bootstrap = TRUE`; use `bootstrap_entities` for a restrictive
#' canary before a complete baseline rebuild.
#'
#' Auxiliary invalidation is keyed. For example, a CPI change for Colombia 2018
#' refreshes only matching Colombia 2018 metadata and deflate nodes; another
#' Colombia year and unrelated country/year nodes stay cached. Worker completion
#' is not success until exact receipts, inventories, and a manifest checkpoint
#' are finalized. Recoverable entity failures block only their descendants. A
#' later call resumes by authoritative replan from Stamp and the last valid
#' manifest, without a persisted run cursor or an exactly-once guarantee.
#'
#' The top-level API always uses the canonical auxiliary measures `pfw`, `cpi`,
#' `ppp`, `pop`, `gdp`, and `pce`. Production activation remains blocked until
#' signed target Windows/SMB fencing and immutable unique-rename evidence are
#' complete.
#'
#' @family pd_process_data pipeline
#' @export
#' @examples
#' \dontrun{
#' pipfun::setup_working_release("20260831", "TEST")
#' result <- pd_run_pipeline(verbose = FALSE)
#' print(result)
#' }
pd_run_pipeline <- function(
  inv = NULL,
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE),
  force_surveys = NULL,
  bootstrap = FALSE,
  bootstrap_entities = NULL,
  checkpoint_size = 25L,
  checkpoint_seconds = Inf
) {
  arguments <- pd_validate_pipeline_arguments(
    force,
    verbose,
    force_surveys,
    bootstrap,
    bootstrap_entities,
    checkpoint_size,
    checkpoint_seconds
  )
  started_at <- Sys.time()
  if (is.null(inv)) {
    inv <- pipload::load_gmd_valid_inv(verbose = arguments$verbose)
  }
  inv <- .filter_completed_dlw_validation_inventory(inv)
  context <- pd_dependency_context()
  preliminary_master <- pipload::load_pip_master_inventory(
    verbose = arguments$verbose
  )
  pd_assert_no_removed_surveys(inv, preliminary_master)
  pd_resolve_pipeline_selectors(
    arguments$force_surveys,
    inv,
    preliminary_master,
    "force"
  )
  pd_resolve_pipeline_selectors(
    arguments$bootstrap_entities,
    inv,
    preliminary_master,
    "bootstrap"
  )

  run_id <- pd_random_id()
  lease <- pd_lease_acquire(context, run_id = run_id)
  release_primary <- NULL
  on.exit({
    tryCatch(
      pd_lease_release(lease),
      error = function(cnd) {
        if (is.null(release_primary)) {
          rlang::cnd_signal(cnd)
        }
      }
    )
  }, add = TRUE)

  master <- tryCatch(
    pipload::load_pip_master_inventory(verbose = arguments$verbose),
    error = function(cnd) {
      release_primary <<- cnd
      rlang::cnd_signal(cnd)
    }
  )
  pd_assert_no_removed_surveys(inv, master)
  resolved_force <- pd_resolve_pipeline_selectors(
    arguments$force_surveys, inv, master, "force"
  )
  resolved_bootstrap <- pd_resolve_pipeline_selectors(
    arguments$bootstrap_entities, inv, master, "bootstrap"
  )
  if (!length(arguments$bootstrap_entities)) {
    resolved_bootstrap <- NULL
  }
  execution <- pd_prepare_execution_locked(
    inv = inv,
    master = master,
    context = context,
    lease = lease,
    bootstrap = arguments$bootstrap,
    bootstrap_entities = resolved_bootstrap,
    force = arguments$force,
    force_surveys = resolved_force,
    verbose = arguments$verbose,
    measures = .PD_PIPELINE_MEASURES,
    metadata_measures = c("cpi", "ppp", "pop"),
    strict_bootstrap_selectors = TRUE
  )
  manifest_before <- execution$manifest_identity
  initial_plan_hash <- pd_plan_hash(execution)
  options <- pd_pipeline_options(
    verbose = arguments$verbose,
    force = arguments$force,
    force_surveys = arguments$force_surveys,
    bootstrap = arguments$bootstrap,
    bootstrap_entities = arguments$bootstrap_entities,
    checkpoint_size = arguments$checkpoint_size,
    checkpoint_seconds = arguments$checkpoint_seconds,
    entity_error_policy = "continue",
    fatal_error_policy = "capture_at_run_boundary"
  )

  execution <- pd_accept_stage_wave(execution, "clean")
  clean_context <- pd_stage_context(
    execution, run_id, options, execution$plan$actions,
    arguments$force_surveys
  )
  contexts <- list(clean = clean_context, metadata = NULL, deflate = NULL)
  outcomes <- list(clean = NULL, metadata = NULL, deflate = NULL)
  run_errors <- list()
  terminal <- FALSE
  terminal_stage <- NA_character_
  stage_terminal <- stats::setNames(rep(FALSE, length(.PDP_STAGES)),
                                    .PDP_STAGES)
  active_stage <- "clean"
  active_execution <- execution

  versioning_switched <- FALSE
  old_versioning <- NULL
  if (arguments$force) {
    on.exit({
      if (versioning_switched) {
        stamp::st_opts(versioning = old_versioning)
      }
    }, add = TRUE)
  }

  caught <- tryCatch({
    if (arguments$force) {
      old_versioning <- stamp::st_opts("versioning", .get = TRUE)
      stamp::st_opts(versioning = "timestamp")
      versioning_switched <- TRUE
    }
    recode_spec <- if (any(execution$plan$actions$action != "none")) {
      sync_recode_spec(alias = "pip_inv", verbose = arguments$verbose)
    } else {
      NULL
    }
    clean <- pd_run_clean_stage_prepared(
      execution,
      execution$plan$actions,
      run_id,
      clean_context,
      master,
      inv,
      options,
      recode_spec,
      arguments$verbose
    )
    execution <- clean$execution
    active_execution <- execution
    master <- clean$master
    outcomes$clean <- clean$outcome
    if (isTRUE(clean$terminal)) {
      terminal <- TRUE
      terminal_stage <- "clean"
      stage_terminal[["clean"]] <- TRUE
      release_primary <- clean$error
    }

    if (!terminal) {
      active_stage <- "metadata"
      execution <- pd_refresh_execution_facts(
        execution,
        master,
        force = arguments$force,
        force_surveys = resolved_force,
        bootstrap = arguments$bootstrap,
        bootstrap_entities = resolved_bootstrap,
        verbose = arguments$verbose,
        strict_bootstrap_selectors = TRUE
      )
      failed_surveys <- outcomes$clean$units[
        status == "failed", unique(survey_id)
      ]
      execution <- pd_accept_stage_wave(
        execution,
        "metadata",
        execution$plan$actions[
          stage == "metadata" & survey_id %in% failed_surveys,
          entity_id
        ]
      )
      active_execution <- execution
      contexts$metadata <- pd_stage_context(
        execution, run_id, options, execution$plan$actions,
        arguments$force_surveys
      )
      metadata <- pd_run_metadata_stage_prepared(
        execution,
        execution$plan$actions,
        run_id,
        contexts$metadata,
        master,
        options,
        arguments$verbose
      )
      execution <- metadata$execution
      active_execution <- execution
      master <- metadata$master
      outcomes$metadata <- metadata$outcome
      if (isTRUE(metadata$terminal)) {
        terminal <- TRUE
        terminal_stage <- "metadata"
        stage_terminal[["metadata"]] <- TRUE
        release_primary <- metadata$error
      }
    }

    if (!terminal) {
      active_stage <- "deflate"
      execution <- pd_refresh_execution_facts(
        execution,
        master,
        force = arguments$force,
        force_surveys = resolved_force,
        bootstrap = arguments$bootstrap,
        bootstrap_entities = resolved_bootstrap,
        verbose = arguments$verbose,
        strict_bootstrap_selectors = TRUE
      )
      failed_metadata <- outcomes$metadata$units[
        status %in% c("failed", "skipped"), unique(entity_id)
      ]
      execution <- pd_accept_stage_wave(
        execution,
        "deflate",
        execution$plan$actions[
          stage == "deflate" & entity_id %in% failed_metadata,
          entity_id
        ]
      )
      active_execution <- execution
      contexts$deflate <- pd_stage_context(
        execution, run_id, options, execution$plan$actions,
        arguments$force_surveys
      )
      deflate <- pd_run_deflate_stage_prepared(
        execution,
        execution$plan$actions,
        run_id,
        contexts$deflate,
        master,
        options,
        arguments$verbose
      )
      execution <- deflate$execution
      active_execution <- execution
      master <- deflate$master
      outcomes$deflate <- deflate$outcome
      if (isTRUE(deflate$terminal)) {
        terminal <- TRUE
        terminal_stage <- "deflate"
        stage_terminal[["deflate"]] <- TRUE
        release_primary <- deflate$error
      }
    }

    if (!terminal) {
      pd_assert_execution_fence(execution)
    }
    NULL
  }, error = function(cnd) cnd)

  if (inherits(caught, "condition")) {
    if (pd_is_pipeline_cancellation(caught)) {
      release_primary <- caught
      rlang::cnd_signal(caught)
    }
    release_primary <- caught
    terminal <- TRUE
    terminal_stage <- active_stage
    if (!is.null(contexts[[active_stage]]) &&
        is.null(outcomes[[active_stage]])) {
      outcomes[[active_stage]] <- pd_boundary_stage_outcome(
        active_execution, active_stage, caught
      )
      stage_terminal[[active_stage]] <- TRUE
    }
    run_errors[[length(run_errors) + 1L]] <-
      pd_pipeline_run_condition(caught, active_stage)
  }

  retained <- tryCatch(
    pd_final_retained_manifest(active_execution),
    error = function(cnd) cnd
  )
  if (inherits(retained, "condition")) {
    release_primary <- retained
    rlang::cnd_signal(retained)
  } else {
    execution <- retained
    active_execution <- retained
  }

  stage_results <- stats::setNames(vector("list", length(.PDP_STAGES)),
                                   .PDP_STAGES)
  for (stage in .PDP_STAGES) {
    if (!is.null(contexts[[stage]]) && !is.null(outcomes[[stage]])) {
      stage_results[[stage]] <- pd_stage_outcome_result(
        outcomes[[stage]],
        contexts[[stage]],
        active_execution,
        terminal = stage_terminal[[stage]],
        log_ref = pd_pipeline_stage_log_ref(
          stage, run_id, outcomes[[stage]]
        )
      )
    }
  }
  plan_hashes <- c(
    initial = initial_plan_hash,
    vapply(.PDP_STAGES, function(stage) {
      if (is.null(contexts[[stage]]) || is.null(stage_results[[stage]])) {
        return(NA_character_)
      }
      contexts[[stage]]$dependency$plan_hash
    }, character(1L))
  )
  result <- new_pipdata_pipeline_result(
    run_id = run_id,
    stage_results = stage_results,
    warnings = list(),
    errors = run_errors,
    plan_hashes = plan_hashes,
    manifest_before = manifest_before,
    manifest_after = active_execution$manifest_identity,
    log_ref = list(
      name = "pipdata_log",
      run_id = run_id,
      summary_discriminator = "pipeline_run_summary_inf",
      log_checkpoint = NULL
    ),
    started_at = started_at,
    completed_at = Sys.time(),
    terminal = terminal
  )
  if (!is.null(result$stage_results$clean) &&
      result$stage_results$clean$counts[["attempted"]] > 0L) {
    pd_log_clean_summary(result$stage_results$clean)
  }
  if (!is.null(result$stage_results$deflate) &&
      result$stage_results$deflate$counts[["attempted"]] > 0L) {
    pd_log_deflate_summary(result$stage_results$deflate)
  }
  pd_log_pipeline_summary(result)
  return(result)
}
