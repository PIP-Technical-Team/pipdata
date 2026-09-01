test_that("pd_run_pipeline has the frozen public signature", {
  expect_identical(
    names(formals(pd_run_pipeline)),
    c(
      "inv", "force", "verbose", "force_surveys", "bootstrap",
      "bootstrap_entities", "checkpoint_size", "checkpoint_seconds"
    )
  )
  expect_identical(formals(pd_run_pipeline)$inv, NULL)
  expect_identical(formals(pd_run_pipeline)$force, FALSE)
  expect_identical(formals(pd_run_pipeline)$force_surveys, NULL)
  expect_identical(formals(pd_run_pipeline)$bootstrap, FALSE)
  expect_identical(formals(pd_run_pipeline)$bootstrap_entities, NULL)
  expect_identical(formals(pd_run_pipeline)$checkpoint_size, 25L)
  expect_identical(formals(pd_run_pipeline)$checkpoint_seconds, Inf)
})

test_that("pipeline pure guards precede lease and storage access", {
  lease_calls <- 0L
  inventory_reads <- 0L
  versioning_calls <- 0L

  testthat::local_mocked_bindings(
    pd_lease_acquire = function(...) {
      lease_calls <<- lease_calls + 1L
      rlang::abort("unexpected lease")
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    load_gmd_valid_inv = function(...) {
      inventory_reads <<- inventory_reads + 1L
      rlang::abort("unexpected inventory read")
    },
    load_pip_master_inventory = function(...) {
      inventory_reads <<- inventory_reads + 1L
      rlang::abort("unexpected inventory read")
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_opts = function(...) {
      versioning_calls <<- versioning_calls + 1L
      rlang::abort("unexpected versioning mutation")
    },
    .package = "stamp"
  )

  expect_error(
    pd_run_pipeline(force = TRUE, force_surveys = "S1"),
    class = "piperr"
  )
  expect_error(
    pd_run_pipeline(bootstrap_entities = "S1"),
    class = "pipdata_bootstrap_selector_error"
  )
  expect_error(
    pd_run_pipeline(checkpoint_size = 0L),
    class = "pipdata_pipeline_argument_error"
  )
  expect_error(
    pd_run_pipeline(checkpoint_seconds = 0),
    class = "pipdata_pipeline_argument_error"
  )
  expect_identical(lease_calls, 0L)
  expect_identical(inventory_reads, 0L)
  expect_identical(versioning_calls, 0L)
})

test_that("pipeline summary logmeta has exact compact fields and types", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  result <- new_pipdata_pipeline_result(
    run_id = "run-1",
    stage_results = list(clean = NULL, metadata = NULL, deflate = NULL),
    warnings = list(),
    errors = list(),
    plan_hashes = c(
      initial = "initial", clean = NA_character_,
      metadata = NA_character_, deflate = NA_character_
    ),
    manifest_before = NULL,
    manifest_after = NULL,
    log_ref = list(
      name = "pipdata_log", run_id = "run-1",
      summary_discriminator = "pipeline_run_summary_inf",
      log_checkpoint = NULL
    ),
    started_at = now,
    completed_at = now
  )

  meta <- pd_pipeline_summary_logmeta(result)

  expect_named(meta, c(
    "info", "run_id", "status", "terminal", "n_selected",
    "n_attempted", "n_success", "n_failed", "n_cached", "n_blocked",
    "clean_status", "metadata_status", "deflate_status",
    "manifest_before_generation", "manifest_after_generation",
    "started_at", "completed_at"
  ))
  expect_identical(meta$info, "pipeline_run_summary_inf")
  expect_true(all(vapply(meta[c(
    "n_selected", "n_attempted", "n_success", "n_failed", "n_cached",
    "n_blocked", "manifest_before_generation", "manifest_after_generation"
  )], is.integer, logical(1L))))
  expect_identical(meta$clean_status, NA_character_)
  expect_identical(meta$metadata_status, NA_character_)
  expect_identical(meta$deflate_status, NA_character_)
  expect_s3_class(meta$started_at, "POSIXct")
  expect_s3_class(meta$completed_at, "POSIXct")
})

pipeline_test_inventory <- function(
  survey_id = "COL_2020_GEIH_V01_M_V01_A_GMD_ALL"
) {
  data.table::data.table(
    survey_id = survey_id,
    pipeline_version = 1L,
    latest_version_id = "dlw-v1",
    content_hash = "dlw-h1",
    file_path = "s1.qs2",
    status = "valid",
    data_available = "Yes",
    date_validated = as.POSIXct("2026-08-31 10:00:00", tz = "UTC"),
    Checksum = "checksum",
    country_code = "COL",
    surveyid_year = 2020L,
    survey_acronym = "GEIH",
    vermast = "v01",
    veralt = "v01",
    collection = "GMD",
    module = "ALL",
    tool = "TB"
  )
}

pipeline_test_context <- function() {
  aliases <- c("pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv")
  context <- list(
    schema_version = 1L,
    release = "20260831",
    identity = "TEST",
    roots = as.list(stats::setNames(file.path("root", aliases), aliases)),
    namespace = "test"
  )
  context$scope_id <- pd_context_hash(context)
  context
}

pipeline_test_plan <- function(context) {
  survey_id <- pipeline_test_inventory()$survey_id[[1L]]
  actions <- data.table::data.table(
    stage = c("clean", "metadata", "deflate"),
    entity_id = c(survey_id, "P1", "P1"),
    survey_id = survey_id,
    pip_id = c(NA_character_, "P1", "P1"),
    action = "none"
  )
  structure(
    list(
      context = context,
      actions = actions,
      reasons = pd_empty_reasons(),
      snapshot = list()
    ),
    class = "pip_dependency_plan"
  )
}

pipeline_test_execution <- function(context, plan, lease) {
  survey_id <- pipeline_test_inventory()$survey_id[[1L]]
  list(
    context = context,
    snapshot = list(
      inventory = pipeline_test_inventory(),
      master = data.table::data.table(survey_id = survey_id, pip_id = "P1"),
      measures = .PD_PIPELINE_MEASURES,
      aux = list(catalog = data.table::data.table(), objects = list()),
      catalogs = list(),
      fingerprints = list(
        summary = data.table::data.table(
          stage = c("clean", "metadata", "deflate"),
          hash = c("clean-code", "metadata-code", "deflate-code")
        )
      ),
      captured_at = "2026-08-31 10:00:00 UTC"
    ),
    plan = plan,
    manifest = pd_empty_manifest(context),
    manifest_identity = NULL,
    lease = lease
  )
}

pipeline_cached_core <- function(execution, actions, context) {
  outcome <- pd_new_stage_outcome(
    unique(actions$stage), execution$manifest_identity
  )
  for (i in seq_len(nrow(actions))) {
    outcome$units <- rbind(
      outcome$units,
      pd_stage_unit_row(
        actions[i], unique(actions$stage), "cached", "current"
      )
    )
  }
  outcome$completed_at <- pd_utc_time(Sys.time())
  list(
    execution = execution,
    master = execution$snapshot$master,
    context = context,
    outcome = outcome,
    terminal = FALSE,
    error = NULL
  )
}

test_that("pipeline accepts ordered waves under one lease and fences cached return", {
  context <- pipeline_test_context()
  full_plan <- pipeline_test_plan(context)
  lease <- list(token = "lease", run_id = NA_character_)
  survey_id <- pipeline_test_inventory()$survey_id[[1L]]
  master <- data.table::data.table(survey_id = survey_id, pip_id = "P1")
  trace <- character()
  master_reads <- 0L
  summary_calls <- 0L
  accepted_run_ids <- character()
  terminal_mode <- FALSE
  versioning_requests <- character()

  testthat::local_mocked_bindings(
    pd_dependency_context = function(...) {
      trace <<- c(trace, "context")
      context
    },
    pd_lease_acquire = function(context, run_id, ...) {
      trace <<- c(trace, "lease")
      lease$run_id <<- run_id
      lease
    },
    pd_lease_release = function(lease) {
      trace <<- c(trace, "release")
      invisible(NULL)
    },
    pd_prepare_execution_locked = function(..., lease) {
      trace <<- c(trace, "authoritative-plan")
      pipeline_test_execution(context, full_plan, lease)
    },
    pd_refresh_execution_facts = function(execution, ...) {
      trace <<- c(trace, "refresh")
      execution$plan <- full_plan
      execution
    },
    pd_assert_execution_fence = function(execution) {
      trace <<- c(trace, "final-fence")
      invisible(execution)
    },
    pd_final_retained_manifest = function(execution) {
      trace <<- c(trace, "final-manifest")
      execution
    },
    pd_run_clean_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) {
      trace <<- c(trace, "clean")
      accepted_run_ids <<- c(accepted_run_ids, run_id)
      result <- pipeline_cached_core(execution, actions, context)
      if (terminal_mode) {
        condition <- simpleError("injected terminal failure")
        result$outcome$errors[[1L]] <- new_stage_condition_record(
          condition, "error", stage = "clean", operation = "clean",
          recoverable = FALSE
        )
        result$terminal <- TRUE
        result$error <- condition
      }
      result
    },
    pd_run_metadata_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) {
      trace <<- c(trace, "metadata")
      accepted_run_ids <<- c(accepted_run_ids, run_id)
      pipeline_cached_core(execution, actions, context)
    },
    pd_run_deflate_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) {
      trace <<- c(trace, "deflate")
      accepted_run_ids <<- c(accepted_run_ids, run_id)
      pipeline_cached_core(execution, actions, context)
    },
    pd_log_pipeline_summary = function(result) {
      trace <<- c(trace, "summary")
      summary_calls <<- summary_calls + 1L
      invisible(result)
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    st_opts = function(x = NULL, .get = FALSE, versioning = NULL, ...) {
      if (isTRUE(.get)) {
        return("content")
      }
      versioning_requests <<- c(versioning_requests, versioning)
      invisible(NULL)
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      master_reads <<- master_reads + 1L
      trace <<- c(trace, paste0("master-", master_reads))
      data.table::copy(master)
    },
    .package = "pipload"
  )

  result <- pd_run_pipeline(inv = pipeline_test_inventory(), verbose = FALSE)

  expect_s3_class(result, "pipdata_pipeline_result")
  expect_identical(result$status, "cached")
  expect_identical(result$counts$selected, 3L)
  expect_identical(result$counts$cached, 3L)
  expect_identical(master_reads, 2L)
  expect_identical(summary_calls, 1L)
  expect_length(unique(accepted_run_ids), 1L)
  expect_identical(unique(accepted_run_ids), result$run_id)
  expect_identical(lease$run_id, result$run_id)
  expect_identical(
    trace,
    c(
      "context", "master-1", "lease", "master-2",
      "authoritative-plan", "clean", "refresh", "metadata", "refresh",
      "deflate", "final-fence", "final-manifest", "summary", "release"
    )
  )

  trace <- character()
  master_reads <- 0L
  summary_calls <- 0L
  accepted_run_ids <- character()
  normal_force <- pd_run_pipeline(
    inv = pipeline_test_inventory(), force = TRUE, verbose = FALSE
  )
  terminal_mode <- TRUE
  terminal_force <- pd_run_pipeline(
    inv = pipeline_test_inventory(), force = TRUE, verbose = FALSE
  )

  expect_identical(normal_force$status, "cached")
  expect_true(terminal_force$terminal)
  expect_identical(
    versioning_requests,
    c("timestamp", "content", "timestamp", "content")
  )
})

test_that("post-lease survey removal releases lease before pipeline writes", {
  context <- pipeline_test_context()
  inventory <- pipeline_test_inventory()
  survey_id <- inventory$survey_id[[1L]]
  removed_id <- "BOL_2021_EH_V01_M_V01_A_GMD_ALL"
  masters <- list(
    data.table::data.table(survey_id = survey_id, pip_id = "P1"),
    data.table::data.table(
      survey_id = c(survey_id, removed_id), pip_id = c("P1", "P2")
    )
  )
  master_reads <- 0L
  lease_releases <- 0L
  plan_calls <- 0L
  worker_calls <- 0L

  testthat::local_mocked_bindings(
    pd_dependency_context = function(...) context,
    pd_lease_acquire = function(...) list(token = "lease"),
    pd_lease_release = function(...) {
      lease_releases <<- lease_releases + 1L
      invisible(NULL)
    },
    pd_prepare_execution_locked = function(...) {
      plan_calls <<- plan_calls + 1L
      rlang::abort("must not plan")
    },
    pd_run_clean_stage_prepared = function(...) {
      worker_calls <<- worker_calls + 1L
      rlang::abort("must not work")
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      master_reads <<- master_reads + 1L
      data.table::copy(masters[[master_reads]])
    },
    .package = "pipload"
  )

  expect_error(
    pd_run_pipeline(inv = inventory, verbose = FALSE),
    class = "pipdata_upstream_survey_removed"
  )
  expect_identical(master_reads, 2L)
  expect_identical(lease_releases, 1L)
  expect_identical(plan_calls, 0L)
  expect_identical(worker_calls, 0L)
})

test_that("authoritative force selectors discard the pre-lease reverse map", {
  first_id <- pipeline_test_inventory()$survey_id[[1L]]
  second_id <- "COL_2021_GEIH_V01_M_V01_A_GMD_ALL"
  second <- pipeline_test_inventory(second_id)
  second[, surveyid_year := 2021L]
  inventory <- data.table::rbindlist(list(
    pipeline_test_inventory(), second
  ))
  masters <- list(
    data.table::data.table(survey_id = first_id, pip_id = "PSEL"),
    data.table::data.table(survey_id = second_id, pip_id = "PSEL")
  )
  master_reads <- 0L
  resolved_force <- NULL
  releases <- 0L

  testthat::local_mocked_bindings(
    pd_dependency_context = function(...) pipeline_test_context(),
    pd_lease_acquire = function(...) list(token = "lease"),
    pd_lease_release = function(...) {
      releases <<- releases + 1L
      invisible(NULL)
    },
    pd_prepare_execution_locked = function(..., force_surveys) {
      resolved_force <<- force_surveys
      rlang::abort("stop after authoritative setup", class = "setup_stop")
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      master_reads <<- master_reads + 1L
      data.table::copy(masters[[master_reads]])
    },
    .package = "pipload"
  )

  expect_error(
    pd_run_pipeline(
      inv = inventory, force_surveys = "PSEL", verbose = FALSE
    ),
    class = "setup_stop"
  )
  expect_identical(master_reads, 2L)
  expect_identical(resolved_force, second_id)
  expect_identical(releases, 1L)
})

test_that("V20 bootstrap selectors re-resolve post-lease with atomic closure", {
  first_id <- pipeline_test_inventory()$survey_id[[1L]]
  second_id <- "COL_2021_GEIH_V01_M_V01_A_GMD_ALL"
  second <- pipeline_test_inventory(second_id)
  second[, surveyid_year := 2021L]
  inventory <- data.table::rbindlist(list(
    pipeline_test_inventory(), second
  ))
  preliminary <- data.table::data.table(
    survey_id = first_id, pip_id = "PSEL"
  )
  authoritative <- data.table::data.table(
    survey_id = second_id, pip_id = c("PSEL", "PALT")
  )
  masters <- list(
    preliminary, authoritative, preliminary, authoritative
  )
  master_reads <- 0L
  observed <- list()
  releases <- 0L
  dependency_context <- pipeline_test_context()
  real_locked <- pd_prepare_execution_locked

  testthat::local_mocked_bindings(
    pd_dependency_context = function(...) dependency_context,
    pd_lease_acquire = function(...) list(token = "lease"),
    pd_lease_release = function(...) {
      releases <<- releases + 1L
      invisible(NULL)
    },
    pd_prepare_dependency_facts = function(
      inv, master, context, ...
    ) {
      current <- data.table::rbindlist(list(
        inv[, .(
          stage = "clean", entity_id = survey_id, survey_id,
          pip_id = NA_character_
        )],
        master[, .(
          stage = "metadata", entity_id = pip_id, survey_id, pip_id
        )],
        master[, .(
          stage = "deflate", entity_id = pip_id, survey_id, pip_id
        )]
      ))
      list(
        context = context,
        manifest = structure(list(), class = "pipdata_manifest_absent"),
        snapshot = list(
          inventory = data.table::copy(inv),
          master = data.table::copy(master),
          fingerprints = list(),
          current = current,
          facts = data.table::data.table()
        )
      )
    },
    pd_prepare_execution_locked = function(...) {
      execution <- real_locked(...)
      observed[[length(observed) + 1L]] <<-
        data.table::copy(execution$plan$actions)
      rlang::abort("stop after bootstrap closure", class = "setup_stop")
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      master_reads <<- master_reads + 1L
      data.table::copy(masters[[master_reads]])
    },
    .package = "pipload"
  )

  for (selector in c("PSEL", second_id)) {
    expect_error(
      pd_run_pipeline(
        inv = inventory,
        bootstrap = TRUE,
        bootstrap_entities = selector,
        verbose = FALSE
      ),
      class = "setup_stop"
    )
  }

  expect_identical(master_reads, 4L)
  expect_identical(releases, 2L)
  expect_length(observed, 2L)
  for (actions in observed) {
    expect_true(all(actions$survey_id == second_id))
    expect_setequal(actions$entity_id, c(second_id, "PSEL", "PALT"))
    expect_setequal(actions$stage, c("clean", "metadata", "deflate"))
  }
})

test_that("resolved bootstrap surveys remain stable through all refreshes", {
  inventory <- pipeline_test_inventory()
  survey_id <- inventory$survey_id[[1L]]
  master <- data.table::data.table(survey_id = survey_id, pip_id = "PSEL")
  context <- pipeline_test_context()
  plan <- pipeline_test_plan(context)
  plan$actions[, survey_id := survey_id]
  observed <- list(initial = NULL, refresh = list(), provenance = NULL)
  execution <- pipeline_test_execution(context, plan, list(token = "lease"))

  testthat::local_mocked_bindings(
    pd_dependency_context = function(...) context,
    pd_lease_acquire = function(...) list(token = "lease"),
    pd_lease_release = function(...) invisible(NULL),
    pd_prepare_execution_locked = function(..., bootstrap_entities) {
      observed$initial <<- bootstrap_entities
      execution
    },
    pd_refresh_execution_facts = function(
      execution, ..., bootstrap_entities
    ) {
      observed$refresh[[length(observed$refresh) + 1L]] <<- bootstrap_entities
      execution$plan <- plan
      execution
    },
    pd_run_clean_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) {
      observed$provenance <<- context$options$bootstrap_entities
      pipeline_cached_core(execution, actions, context)
    },
    pd_run_metadata_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) pipeline_cached_core(execution, actions, context),
    pd_run_deflate_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) pipeline_cached_core(execution, actions, context),
    pd_assert_execution_fence = function(...) invisible(NULL),
    pd_final_retained_manifest = function(execution) execution,
    pd_log_pipeline_summary = function(result) invisible(result),
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) data.table::copy(master),
    .package = "pipload"
  )

  result <- pd_run_pipeline(
    inv = inventory, bootstrap = TRUE, bootstrap_entities = "PSEL",
    verbose = FALSE
  )

  expect_s3_class(result, "pipdata_pipeline_result")
  expect_identical(observed$initial, survey_id)
  expect_true(length(observed$refresh) >= 2L)
  expect_true(all(vapply(
    observed$refresh, identical, logical(1L), survey_id
  )))
  expect_identical(observed$provenance, "PSEL")
})

test_that("top-level selector resolution rejects unknown identifiers", {
  inventory <- pipeline_test_inventory()
  master <- data.table::data.table(
    survey_id = inventory$survey_id, pip_id = "P1"
  )

  expect_error(
    pd_resolve_pipeline_selectors("missing", inventory, master, "force"),
    class = "pipdata_force_selector_error"
  )
  expect_error(
    pd_resolve_pipeline_selectors(
      "missing", inventory, master, "bootstrap"
    ),
    class = "pipdata_bootstrap_selector_error"
  )
})

test_that("clean checkpoint failure accounts pending and later accepted units", {
  actions <- data.table::data.table(
    stage = "clean",
    entity_id = c("S1", "S2", "S3"),
    survey_id = c("S1", "S2", "S3"),
    pip_id = NA_character_,
    action = "rebuild",
    input_hash = c("i1", "i2", "i3"),
    code_hash = "clean-code",
    expected_pip_ids = list("P1", "P2", "P3")
  )
  reasons <- actions[, .(
    stage, entity_id, reason = "dlw_changed", input = "dlw",
    old = "old", new = "new"
  )]
  execution <- list(
    plan = structure(
      list(
        context = list(scope_id = "scope"), actions = actions,
        reasons = reasons, snapshot = list()
      ),
      class = "pip_dependency_plan"
    ),
    manifest_identity = NULL
  )
  inv <- data.table::data.table(survey_id = c("S1", "S2", "S3"))
  master <- data.table::data.table(
    survey_id = c("S1", "S2", "S3"), pip_id = c("P1", "P2", "P3")
  )
  options <- pd_pipeline_options(
    checkpoint_seconds = Inf,
    fatal_error_policy = "capture_at_run_boundary"
  )
  context <- list(run_id = "run")
  worker_calls <- 0L
  checkpoint_calls <- 0L
  refresh_calls <- 0L

  testthat::local_mocked_bindings(
    pd_execute_clean = function(action, ...) {
      worker_calls <<- worker_calls + 1L
      receipts <- data.table::data.table(
        success = TRUE, pip_id = action$expected_pip_ids[[1L]],
        alias = "pip", artifact = action$expected_pip_ids[[1L]],
        path = paste0(action$expected_pip_ids[[1L]], ".qs2"),
        version_id = paste0("v", worker_calls),
        content_hash = paste0("h", worker_calls),
        input_hash = action$input_hash, code_hash = action$code_hash
      )
      list(
        success = TRUE,
        receipts = receipts,
        expected_pip_ids = action$expected_pip_ids[[1L]],
        metadata = list()
      )
    },
    pd_clean_receipt_set = function(receipts, expected) {
      list(receipts = receipts, output_hash = paste0("set-", expected))
    },
    pd_finalize_checkpoint = function(execution, ...) {
      checkpoint_calls <<- checkpoint_calls + 1L
      if (checkpoint_calls == 2L) {
        rlang::abort(
          "injected checkpoint failure",
          class = "pipdata_manifest_write_error"
        )
      }
      execution$manifest_identity <- list(
        filename = "m1", uuid = "u1", checksum = "c1", generation = 1
      )
      list(candidate = master, execution = execution)
    },
    pd_log_stage_condition = function(...) invisible(NULL),
    .package = "pipdata"
  )

  result <- pd_run_clean_stage_prepared(
    execution, actions, "run", context, master, inv, options,
    checkpoint_callback = function(execution, master) {
      refresh_calls <<- refresh_calls + 1L
      execution
    }
  )

  expect_true(result$terminal)
  expect_identical(worker_calls, 2L)
  expect_identical(checkpoint_calls, 2L)
  expect_identical(refresh_calls, 1L)
  expect_identical(
    result$outcome$units$status,
    c("success", "failed", "skipped")
  )
  expect_identical(
    result$outcome$units$reason_codes,
    list("dlw_changed", "checkpoint_uncommitted", "upstream_failed")
  )
})

test_that("recoverable clean failure is durable and remains aggregateable", {
  action <- data.table::data.table(
    stage = "clean", entity_id = "S1", survey_id = "S1",
    pip_id = NA_character_, action = "rebuild", input_hash = "input",
    code_hash = "clean-code", expected_pip_ids = list("P1")
  )
  reasons <- data.table::data.table(
    stage = "clean", entity_id = "S1", reason = "dlw_changed",
    input = "dlw", old = "old", new = "new"
  )
  condition <- new_stage_condition_record(
    severity = "error", code = "yr_wrng", message = "bad year",
    stage = "clean", entity_id = "S1", survey_id = "S1",
    operation = "clean", recoverable = TRUE
  )
  execution <- list(
    plan = list(actions = action, reasons = reasons), manifest_identity = NULL,
    lease = list()
  )
  master <- data.table::data.table(
    survey_id = "S1", pip_id = "P1", version_id_data = "d1",
    content_hash_data = "dh1"
  )
  persisted <- 0L
  testthat::local_mocked_bindings(
    pd_execute_clean = function(...) list(success = FALSE, condition = condition),
    pd_persist_failed_invalidation = function(execution, master, action, ...) {
      persisted <<- persisted + 1L
      master[, version_id_data := NA_character_]
      master
    },
    pd_log_stage_condition = function(...) invisible(NULL),
    .package = "pipdata"
  )

  out <- pd_run_clean_stage_prepared(
    execution, action, "run", list(run_id = "run"), master,
    data.table::data.table(survey_id = "S1"),
    pd_pipeline_options(checkpoint_seconds = Inf), verbose = FALSE
  )

  expect_false(out$terminal)
  expect_identical(persisted, 1L)
  expect_true(is.na(out$master$version_id_data))
  expect_identical(out$outcome$units$reason_codes, list("entity_failed"))
  expect_identical(out$outcome$errors[[1L]]$code, "yr_wrng")
  expect_no_error(validate_stage_units(out$outcome$units))
})

test_that("final retained-manifest integrity failures propagate without results", {
  context <- pipeline_test_context()
  plan <- pipeline_test_plan(context)
  execution <- pipeline_test_execution(context, plan, list(token = "lease"))
  master <- execution$snapshot$master
  bind_calls <- 0L

  testthat::local_mocked_bindings(
    pd_dependency_context = function(...) context,
    pd_lease_acquire = function(...) list(token = "lease"),
    pd_lease_release = function(...) invisible(NULL),
    pd_prepare_execution_locked = function(...) execution,
    pd_refresh_execution_facts = function(execution, ...) {
      execution$plan <- plan
      execution
    },
    pd_run_clean_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) pipeline_cached_core(execution, actions, context),
    pd_run_metadata_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) pipeline_cached_core(execution, actions, context),
    pd_run_deflate_stage_prepared = function(
      execution, actions, run_id, context, ...
    ) pipeline_cached_core(execution, actions, context),
    pd_assert_execution_fence = function(...) invisible(NULL),
    pd_final_retained_manifest = function(...) {
      rlang::abort(
        "retained manifest changed", class = "pipdata_manifest_parent_changed"
      )
    },
    pd_stage_outcome_result = function(...) {
      bind_calls <<- bind_calls + 1L
      rlang::abort("stale stage evidence was bound")
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) data.table::copy(master),
    .package = "pipload"
  )

  expect_error(
    pd_run_pipeline(inv = pipeline_test_inventory(), verbose = FALSE),
    class = "pipdata_manifest_parent_changed"
  )
  expect_identical(bind_calls, 0L)
})

pipeline_fault_aliases <- c(
  artifact = "phase3_fault_artifact",
  release = "phase3_fault_release",
  master = "phase3_fault_master"
)

pipeline_fault_manifest_projection <- function(manifest, aliases = NULL) {
  records <- data.table::copy(manifest$records)
  receipts <- data.table::rbindlist(lapply(seq_len(nrow(records)), function(i) {
    stage <- records$stage[[i]]
    entity_id <- records$entity_id[[i]]
    receipt <- pd_committed_output_receipt(
      manifest, stage, entity_id, entity_id
    )
    if (is.null(receipt)) {
      return(data.table::data.table())
    }
    alias <- receipt$alias
    if (!is.null(aliases) && alias %in% aliases) {
      alias <- names(aliases)[match(alias, aliases)]
    }
    data.table::data.table(
      stage = stage,
      entity_id = entity_id,
      alias = alias,
      artifact = receipt$artifact,
      path = fs::path_file(receipt$path),
      version_id = receipt$version_id,
      content_hash = receipt$content_hash
    )
  }), fill = TRUE)
  records[, output_receipts := NULL]
  records[, output_version_id := NULL]
  if ("version_id" %in% names(receipts)) {
    receipts[, version_id := NULL]
  }
  list(
    records = pd_canonical_snapshot_table(records),
    receipts = pd_canonical_snapshot_table(receipts),
    inputs = pd_canonical_snapshot_table(manifest$inputs),
    fingerprints = pd_canonical_snapshot_table(manifest$fingerprints),
    tombstones = pd_canonical_snapshot_table(manifest$tombstones)
  )
}

pipeline_fault_table_projection <- function(x) {
  projected <- data.table::copy(data.table::as.data.table(x))
  version_columns <- grep("version_id", names(projected), value = TRUE)
  if (length(version_columns)) {
    projected[, (version_columns) := NULL]
  }
  pd_canonical_snapshot_table(projected)
}

pipeline_fault_current_receipt <- function(fixture) {
  receipt <- pd_catalog_receipt(
    stamp::st_catalog_query(alias = fixture$aliases[["artifact"]]),
    fixture$pip_id
  )
  c(
    list(
      alias = fixture$aliases[["artifact"]],
      artifact = fixture$pip_id
    ),
    receipt,
    list(success = TRUE)
  )
}

pipeline_fault_master <- function(fixture) {
  pipload::pip_read(
    fixture$master_id,
    alias = fixture$aliases[["master"]],
    verbose = FALSE
  )
}

pipeline_fault_replan <- function(fixture) {
  manifest <- pd_manifest_read(fixture$context, fixture$root)
  receipt <- pipeline_fault_current_receipt(fixture)
  current <- data.table::data.table(
    stage = "metadata",
    entity_id = fixture$pip_id,
    survey_id = fixture$survey_id,
    pip_id = fixture$pip_id,
    input_hash = fixture$input_hash,
    legacy_input_hash = fixture$input_hash,
    legacy_input_version = fixture$input_hash,
    code_hash = fixture$code_hash,
    output_version_id = receipt$version_id,
    output_hash = receipt$content_hash,
    input_rows = list(data.table::data.table())
  )
  snapshot <- list(
    current = current,
    facts = data.table::data.table(),
    fingerprints = list(
      components = data.table::data.table(
        stage = character(), component = character(), hash = character()
      )
    )
  )
  snapshot$facts <- pd_snapshot_facts(snapshot, manifest)
  pd_dependency_plan(
    inv = data.table::data.table(survey_id = character()),
    master = pipeline_fault_master(fixture),
    manifest = manifest,
    context = fixture$context,
    fingerprints = list(),
    snapshot = snapshot
  )
}

pipeline_fault_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  suffix <- gsub("[^A-Za-z0-9]", "", fs::path_file(root))
  aliases <- stats::setNames(
    paste0(unname(pipeline_fault_aliases), "_", suffix),
    names(pipeline_fault_aliases)
  )
  storage_roots <- file.path(root, names(pipeline_fault_aliases))
  for (i in seq_along(aliases)) {
    stamp::st_init(
      root = storage_roots[[i]], alias = aliases[[i]]
    )
  }
  context <- list(scope_id = "phase3-fault-scope")
  survey_id <- "COL_2020_GEIH_V01_M_V01_A_GMD_ALL"
  pip_id <- "COL_2020_GEIH_INC"
  release_id <- "phase3_fault_release_inventory"
  master_id <- "phase3_fault_master_inventory"
  old_artifact <- list(value = "old")
  new_artifact <- list(value = "new")
  baseline_lease <- pd_lease_acquire(
    context, root, run_id = "phase3-baseline"
  )
  artifact_receipt <- pd_save_receipt(
    old_artifact,
    pip_id,
    aliases[["artifact"]],
    lease = baseline_lease
  )
  master <- data.table::data.table(
    survey_id = survey_id,
    pip_id = pip_id,
    version_id_data = "data-v1",
    content_hash_data = "data-h1",
    version_id_metadata = artifact_receipt$version_id,
    content_hash_metadata = artifact_receipt$content_hash,
    deflated = FALSE
  )
  release_receipt <- pd_save_receipt(
    master,
    release_id,
    aliases[["release"]],
    lease = baseline_lease
  )
  master[, latest_release_version_id := release_receipt$version_id]
  pd_save_receipt(
    master,
    master_id,
    aliases[["master"]],
    lease = baseline_lease
  )
  manifest <- pd_empty_manifest(context)
  manifest$header[, created_at := "2026-09-01 00:00:00 UTC"]
  manifest$records <- data.table::data.table(
    stage = "metadata",
    entity_id = pip_id,
    output_version_id = artifact_receipt$version_id,
    output_hash = artifact_receipt$content_hash,
    input_hash = "input-old",
    code_hash = "metadata-code",
    output_receipts = list(list(list(
      alias = artifact_receipt$alias,
      artifact = artifact_receipt$artifact,
      path = artifact_receipt$path,
      version_id = artifact_receipt$version_id,
      content_hash = artifact_receipt$content_hash
    )))
  )
  manifest$inputs <- data.table::data.table(
    stage = "metadata", entity_id = pip_id, name = "canonical",
    version_id = "input-old", content_hash = "input-old"
  )
  baseline <- pd_manifest_publish(
    manifest, context, baseline_lease, root, parent = NULL
  )
  pd_lease_release(baseline_lease)
  list(
    root = root,
    aliases = aliases,
    context = context,
    survey_id = survey_id,
    pip_id = pip_id,
    release_id = release_id,
    master_id = master_id,
    new_artifact = new_artifact,
    input_hash = "input-new",
    code_hash = "metadata-code",
    baseline_identity = attr(baseline, "manifest_identity"),
    baseline_manifest = pipeline_fault_manifest_projection(baseline, aliases),
    baseline_release = pipeline_fault_table_projection(
      pipload::pip_read(
        release_id,
        alias = aliases[["release"]],
        verbose = FALSE
      )
    ),
    baseline_master = pipeline_fault_table_projection(master)
  )
}

pipeline_fault_result <- function(fixture, receipt) {
  data.table::data.table(
    stage = "metadata",
    pip_id = fixture$pip_id,
    alias = receipt$alias,
    artifact = receipt$artifact,
    path = receipt$path,
    version_id = receipt$version_id,
    content_hash = receipt$content_hash,
    success = TRUE,
    input_hash = fixture$input_hash,
    code_hash = fixture$code_hash
  )
}

pipeline_fault_abort <- function(point) {
  rlang::abort(
    paste("Injected Phase 3 fault:", point),
    class = "pipeline_phase3_fault",
    fault_point = point
  )
}

pipeline_fault_attempt <- function(fixture, point = "none") {
  lease <- pd_lease_acquire(
    fixture$context,
    fixture$root,
    run_id = paste0("phase3-", point)
  )
  on.exit({
    if (fs::dir_exists(lease$path)) {
      try(pd_lease_release(lease), silent = TRUE)
    }
  }, add = TRUE)
  if (identical(point, "before_worker")) {
    pipeline_fault_abort(point)
  }
  if (identical(point, "after_artifact_write")) {
    pipload::pip_write(
      fixture$new_artifact,
      fixture$pip_id,
      alias = fixture$aliases[["artifact"]],
      verbose = FALSE
    )
    pipeline_fault_abort(point)
  }
  receipt <- pd_save_receipt(
    fixture$new_artifact,
    fixture$pip_id,
    fixture$aliases[["artifact"]],
    lease = lease
  )
  if (identical(point, "after_verified_receipt")) {
    pipeline_fault_abort(point)
  }
  results <- pipeline_fault_result(fixture, receipt)
  execution <- list(
    context = fixture$context,
    lease = lease,
    manifest = pd_manifest_read(fixture$context, fixture$root),
    manifest_identity = NULL,
    snapshot = NULL
  )
  execution$manifest_identity <- attr(
    execution$manifest, "manifest_identity"
  )
  master <- pipeline_fault_master(fixture)
  release_writer <- function(candidate, active_lease) {
    written <- pd_save_receipt(
      candidate,
      fixture$release_id,
      fixture$aliases[["release"]],
      lease = active_lease
    )
    if (identical(point, "after_release_inventory")) {
      pipeline_fault_abort(point)
    }
    if (identical(point, "lease_loss")) {
      fs::dir_delete(active_lease$path)
    }
    written
  }
  master_writer <- function(candidate, active_lease) {
    written <- pd_save_receipt(
      candidate,
      fixture$master_id,
      fixture$aliases[["master"]],
      lease = active_lease
    )
    if (identical(point, "after_master_inventory")) {
      pipeline_fault_abort(point)
    }
    written
  }
  finalize <- function() {
    pd_finalize_checkpoint(
      execution,
      master,
      "metadata",
      results,
      release_writer,
      master_writer,
      manifest_root = fixture$root
    )
  }
  if (identical(point, "before_manifest_publication")) {
    return(testthat::with_mocked_bindings(
      finalize(),
      pd_manifest_publish = function(...) pipeline_fault_abort(point),
      .package = "pipdata"
    ))
  }
  if (identical(point, "after_manifest_publication")) {
    real_publish <- pd_manifest_publish
    return(testthat::with_mocked_bindings(
      finalize(),
      pd_manifest_publish = function(...) {
        real_publish(...)
        pipeline_fault_abort(point)
      },
      .package = "pipdata"
    ))
  }
  finalize()
}

pipeline_fault_state <- function(fixture) {
  manifest <- pd_manifest_read(fixture$context, fixture$root)
  artifact_receipt <- pipeline_fault_current_receipt(fixture)
  release <- pipload::pip_read(
    fixture$release_id,
    alias = fixture$aliases[["release"]],
    verbose = FALSE
  )
  master <- pipeline_fault_master(fixture)
  record <- manifest$records[
    stage == "metadata" & entity_id == fixture$pip_id
  ]
  release_versions <- stamp::st_versions(
    paste0(fixture$release_id, ".qs2"),
    alias = fixture$aliases[["release"]]
  )$version_id
  consistent <- nrow(record) == 1L &&
    identical(record$output_version_id[[1L]], artifact_receipt$version_id) &&
    identical(record$output_hash[[1L]], artifact_receipt$content_hash) &&
    identical(master$version_id_metadata[[1L]], artifact_receipt$version_id) &&
    identical(master$content_hash_metadata[[1L]], artifact_receipt$content_hash) &&
    identical(release$version_id_metadata[[1L]], artifact_receipt$version_id) &&
    identical(release$content_hash_metadata[[1L]], artifact_receipt$content_hash) &&
    master$latest_release_version_id[[1L]] %in% release_versions
  list(
    generation = attr(manifest, "manifest_identity")$generation,
    manifest = pipeline_fault_manifest_projection(manifest, fixture$aliases),
    artifact = pipload::pip_read(
      fixture$pip_id,
      alias = fixture$aliases[["artifact"]],
      verbose = FALSE
    ),
    release = pipeline_fault_table_projection(release),
    master = pipeline_fault_table_projection(master),
    consistent = consistent
  )
}

pipeline_fault_recover <- function(fixture) {
  restart_plan <- pipeline_fault_replan(fixture)
  metadata_action <- restart_plan$actions[
    stage == "metadata" & entity_id == fixture$pip_id, action
  ]
  if (!identical(metadata_action, "none")) {
    pipeline_fault_attempt(fixture)
  }
  list(
    restart_plan = restart_plan,
    final_plan = pipeline_fault_replan(fixture),
    state = pipeline_fault_state(fixture)
  )
}

c4_pipeline_normalize_path <- function(path, root) {
  normalized <- tolower(gsub("\\\\", "/", path))
  normalized_root <- tolower(gsub("\\\\", "/", root))
  sub(normalized_root, "<ROOT>", normalized, fixed = TRUE)
}

c4_pipeline_exact_state <- function(fixture) {
  manifest <- pd_manifest_read(fixture$context, fixture$root)
  records <- data.table::copy(manifest$records)
  normalize_receipts <- function(receipts) {
    while (is.list(receipts) && length(receipts) == 1L &&
           is.list(receipts[[1L]]) && is.null(names(receipts[[1L]]))) {
      receipts <- receipts[[1L]]
    }
    if (is.list(receipts) && !is.null(names(receipts))) {
      receipts <- list(receipts)
    }
    lapply(receipts, function(receipt) {
      receipt$path <- c4_pipeline_normalize_path(receipt$path, fixture$root)
      receipt
    })
  }
  receipts <- data.table::rbindlist(lapply(seq_len(nrow(records)), function(i) {
    output_receipts <- normalize_receipts(records$output_receipts[[i]])
    data.table::rbindlist(lapply(output_receipts, function(receipt) {
      data.table::as.data.table(c(
        list(stage = records$stage[[i]], entity_id = records$entity_id[[i]]),
        receipt
      ))
    }), fill = TRUE)
  }), fill = TRUE)
  records[, output_receipts := lapply(output_receipts, normalize_receipts)]
  release <- pipload::pip_read(
    "pip_release_inventory", alias = fixture$aliases[["pip_inv"]],
    verbose = FALSE
  )
  master <- pipload::pip_read(
    "pip_master_inventory", alias = fixture$aliases[["pip_master"]],
    verbose = FALSE
  )
  inventory_versions <- function(alias, artifact) {
    versions <- stamp::st_versions(
      paste0(artifact, ".qs2"), alias = fixture$aliases[[alias]]
    )
    versions[, .(version_id, content_hash)]
  }
  list(
    generation = attr(manifest, "manifest_identity")$generation,
    records = pd_canonical_snapshot_table(records),
    receipts = pd_canonical_snapshot_table(receipts),
    inputs = pd_canonical_snapshot_table(manifest$inputs),
    fingerprints = pd_canonical_snapshot_table(manifest$fingerprints),
    tombstones = pd_canonical_snapshot_table(manifest$tombstones),
    release = pd_canonical_snapshot_table(release),
    master = pd_canonical_snapshot_table(master),
    release_versions = pd_canonical_snapshot_table(inventory_versions(
      "pip_inv", "pip_release_inventory"
    )),
    master_versions = pd_canonical_snapshot_table(inventory_versions(
      "pip_master", "pip_master_inventory"
    ))
  )
}

test_that("V10 crash restart matrix preserves authority and converges", {
  uninterrupted <- c4_pipeline_fixture()
  c4_pipeline_change_code(
    uninterrupted, "metadata", "pd_execute_metadata", "metadata-fault-v2"
  )
  expected_run <- c4_pipeline_run(
    uninterrupted, checkpoint_size = 1L, checkpoint_seconds = Inf
  )
  expect_false(expected_run$result$terminal)
  expect_true(all(c4_pipeline_units(expected_run$result)$status %in%
    c("success", "cached")))

  points <- c(
    "before_worker",
    "after_artifact_write",
    "after_verified_receipt",
    "after_release_inventory",
    "after_master_inventory",
    "before_manifest_publication",
    "after_manifest_publication",
    "lease_loss"
  )
  for (point in points) {
    fixture <- c4_pipeline_fixture()
    c4_pipeline_change_code(
      fixture, "metadata", "pd_execute_metadata", "metadata-fault-v2"
    )
    versioning_before <- stamp::st_opts("versioning", .get = TRUE)
    fault <- c4_pipeline_run(
      fixture, checkpoint_size = 1L, checkpoint_seconds = Inf,
      fault_point = point
    )
    fault_units <- c4_pipeline_units(fault$result)
    expect_true(fault$counters$faulted, info = point)
    expect_true(fault$result$terminal, info = point)
    expect_identical(
      fault$result$counts$selected,
      nrow(fault_units),
      info = point
    )
    expect_true(all(fault_units$status %in% c(
      "success", "failed", "cached", "skipped"
    )), info = point)
    expect_true(is.null(fault$result$stage_results$deflate), info = point)
    expect_false(fs::dir_exists(fault$counters$lease_path), info = point)
    expect_identical(
      stamp::st_opts("versioning", .get = TRUE), versioning_before,
      info = point
    )

    restart <- c4_pipeline_run(
      fixture, checkpoint_size = 1L, checkpoint_seconds = Inf
    )
    restart_state <- c4_pipeline_exact_state(fixture)
    converged <- c4_pipeline_run(
      fixture, checkpoint_size = 1L, checkpoint_seconds = Inf
    )
    expect_false(restart$result$terminal, info = point)
    expect_false(identical(restart$result$run_id, fault$result$run_id), info = point)
    expect_true(all(c4_pipeline_units(converged$result)$status == "cached"),
                info = point)
    expect_true(all(converged$counters$writes == 0L), info = point)
    expect_identical(
      c4_pipeline_exact_state(fixture), restart_state, info = point
    )
  }

  forced <- c4_pipeline_fixture()
  versioning_before <- stamp::st_opts("versioning", .get = TRUE)
  forced_fault <- c4_pipeline_run(
    forced, force = TRUE, checkpoint_size = 1L,
    fault_point = "before_worker"
  )
  expect_true(forced_fault$result$terminal)
  expect_identical(stamp::st_opts("versioning", .get = TRUE), versioning_before)
})

test_that("V12 Colombia 2018 CPI invalidation is exact through the executor", {
  fixture <- c4_pipeline_fixture()
  c4_pipeline_change_aux(fixture, "cpi", function(cpi) {
    cpi[
      country_code == "COL" & year == 2018L & survey_acronym == "GEIH",
      cpi_value := cpi_value + 1
    ]
    cpi
  })

  run <- c4_pipeline_run(fixture)
  units <- c4_pipeline_units(run$result)
  target <- sort(fixture$master[
    country_code == "COL" & surveyid_year == 2018L, pip_id
  ])
  nontarget <- sort(setdiff(fixture$master$pip_id, target))

  expect_identical(
    units[stage == "clean", .(entity_id, status, reason_codes)],
    data.table::data.table(
      entity_id = sort(fixture$inv$survey_id),
      status = "cached",
      reason_codes = rep(list("current"), nrow(fixture$inv))
    )
  )
  expect_identical(sort(run$counters$workers$clean), character())
  expect_identical(sort(run$counters$workers$metadata), target)
  expect_identical(sort(run$counters$workers$deflate), target)
  expect_true(all(units[
    stage == "metadata" & entity_id %in% target,
    vapply(reason_codes, identical, logical(1L), "aux_cpi_changed")
  ]))
  expect_true(all(units[
    stage == "deflate" & entity_id %in% target,
    vapply(
      reason_codes,
      identical,
      logical(1L),
      c(
        "aux_cpi_changed", "output_missing", "upstream_output_changed"
      )
    )
  ]))
  expect_true(all(units[
    entity_id %in% nontarget & stage %in% c("metadata", "deflate"),
    status == "cached"
  ]))
  expect_identical(run$counters$household_reads, 0L)
  expect_identical(
    run$counters$writes[c("pip", "pip_meta", "pip_deflated")],
    c(pip = 0L, pip_meta = 2L, pip_deflated = 2L)
  )
  expect_identical(
    run$counters$writes[c("pip_inv", "pip_master")],
    c(pip_inv = 2L, pip_master = 2L)
  )

  manifest <- pd_manifest_read(fixture$context, fixture$root)
  for (stage in c("metadata", "deflate")) {
    selected_stage <- stage
    alias <- if (stage == "metadata") "pip_meta" else "pip_deflated"
    for (pip_id in target) {
      receipt <- run$counters$receipts[[paste(stage, pip_id, sep = ":")]]
      record <- manifest$records[
        get("stage") == selected_stage & entity_id == pip_id
      ]
      expect_identical(nrow(record), 1L)
      expect_identical(record$output_version_id[[1L]], receipt$version_id)
      expect_identical(record$output_hash[[1L]], receipt$content_hash)
      expect_identical(
        pd_committed_output_receipt(manifest, stage, pip_id, pip_id)$alias,
        alias
      )
    }
  }

  rerun <- c4_pipeline_run(fixture)
  rerun_units <- c4_pipeline_units(rerun$result)
  expect_true(all(rerun_units$status == "cached"))
  expect_true(all(rerun$counters$writes == 0L))
  expect_identical(rerun$counters$household_reads, 0L)
})

test_that("V13 invalidation matrix has exact effects and immediate convergence", {
  col_2018 <- c("COL_2018_GEIH_CON_ALL", "COL_2018_GEIH_INC_ALL")
  col_2019 <- "COL_2019_GEIH_INC_ALL"
  per_2018 <- "PER_2018_ENAHO_INC_ALL"
  all_pip <- sort(c(col_2018, col_2019, per_2018))
  scenarios <- list(
    dlw = list(
      mutate = function(f) {
        f$inv[surveyid_year == 2019L, `:=`(
          latest_version_id = "dlw-2019-v2", content_hash = "dlw-2019-h2"
        )]
      },
      clean = "COL_2019_GEIH_V01_M_V01_A_GMD_ALL",
      metadata = col_2019,
      deflate = col_2019,
      reasons = list(
        clean = "dlw_changed",
        metadata = "upstream_output_changed",
        deflate = c("output_missing", "upstream_output_changed")
      )
    ),
    clean_code = list(
      mutate = function(f) c4_pipeline_change_code(
        f, "clean", "pd_execute_clean", "clean-code-v2"
      ),
      clean = sort(c4_pipeline_entities()$survey_id),
      metadata = all_pip,
      deflate = all_pip,
      reasons = list(
        clean = "clean_code_changed",
        metadata = "upstream_output_changed",
        deflate = c("output_missing", "upstream_output_changed")
      )
    ),
    recode = list(
      mutate = function(f) c4_pipeline_change_code(
        f, "clean", "recode_spec.yml", "clean-code-v2"
      ),
      clean = sort(c4_pipeline_entities()$survey_id),
      metadata = all_pip,
      deflate = all_pip,
      reasons = list(
        clean = "recode_spec_changed",
        metadata = "upstream_output_changed",
        deflate = c("output_missing", "upstream_output_changed")
      )
    ),
    pfw = list(
      mutate = function(f) c4_pipeline_change_aux(f, "pfw", function(pfw) {
        pfw[country_code == "COL" & surveyid_year == 2019L,
            gdp_domain := 2L]
        pfw
      }),
      clean = "COL_2019_GEIH_V01_M_V01_A_GMD_ALL",
      metadata = col_2019,
      deflate = col_2019,
      reasons = list(
        clean = "pfw_changed",
        metadata = c("aux_gdp_changed", "upstream_output_changed"),
        deflate = c("output_missing", "upstream_output_changed")
      )
    ),
    ppp = list(
      mutate = function(f) c4_pipeline_change_aux(f, "ppp", function(ppp) {
        ppp[country_code == "COL", ppp := ppp + 1]
        ppp
      }),
      clean = character(),
      metadata = sort(c(col_2018, col_2019)),
      deflate = sort(c(col_2018, col_2019)),
      reasons = list(
        metadata = "aux_ppp_changed",
        deflate = c(
          "aux_ppp_changed", "output_missing", "upstream_output_changed"
        )
      )
    ),
    population = list(
      mutate = function(f) c4_pipeline_change_aux(f, "pop", function(pop) {
        pop[country_code == "COL" & year == 2019L, pop := pop + 1]
        pop
      }),
      clean = character(), metadata = col_2019, deflate = col_2019,
      reasons = list(
        metadata = "aux_pop_changed",
        deflate = c(
          "aux_pop_changed", "output_missing", "upstream_output_changed"
        )
      )
    ),
    gdp = list(
      mutate = function(f) c4_pipeline_change_aux(f, "gdp", function(gdp) {
        gdp[country_code == "COL" & year == 2019L, gdp := gdp + 1]
        gdp
      }),
      clean = character(), metadata = col_2019, deflate = col_2019,
      reasons = list(
        metadata = "aux_gdp_changed",
        deflate = c("output_missing", "upstream_output_changed")
      )
    ),
    pce = list(
      mutate = function(f) c4_pipeline_change_aux(f, "pce", function(pce) {
        pce[country_code == "PER" & year == 2018L, pce := pce + 1]
        pce
      }),
      clean = character(), metadata = per_2018, deflate = per_2018,
      reasons = list(
        metadata = "aux_pce_changed",
        deflate = c("output_missing", "upstream_output_changed")
      )
    ),
    metadata_code = list(
      mutate = function(f) c4_pipeline_change_code(
        f, "metadata", "pd_execute_metadata", "metadata-code-v2"
      ),
      clean = character(), metadata = all_pip, deflate = all_pip,
      reasons = list(
        metadata = "metadata_code_changed",
        deflate = c("output_missing", "upstream_output_changed")
      )
    ),
    deflate_code = list(
      mutate = function(f) c4_pipeline_change_code(
        f, "deflate", "pd_execute_deflate", "deflate-code-v2"
      ),
      clean = character(), metadata = character(), deflate = all_pip,
      reasons = list(deflate = "deflate_code_changed")
    ),
    metadata_missing = list(
      mutate = function(f) f$hidden$pip_meta <- col_2019,
      clean = character(), metadata = col_2019, deflate = col_2019,
      reasons = list(
        metadata = "output_missing",
        deflate = c("output_missing", "upstream_output_changed")
      )
    ),
    deflate_drift = list(
      mutate = function(f) {
        receipt <- c4_pipeline_external_output(
          f, "pip_deflated", per_2018,
          data.table::data.table(pip_id = per_2018, source = "external-drift")
        )
        f$master[pip_id == per_2018, `:=`(
          version_id_deflated = receipt$version_id,
          content_hash_deflated = receipt$content_hash,
          deflated = TRUE
        )]
      },
      clean = character(), metadata = character(), deflate = per_2018,
      reasons = list(deflate = "output_drift")
    )
  )

  for (name in names(scenarios)) {
    scenario <- scenarios[[name]]
    fixture <- c4_pipeline_fixture()
    scenario$mutate(fixture)
    run <- c4_pipeline_run(fixture)
    units <- c4_pipeline_units(run$result)

    expect_identical(
      sort(run$counters$workers$clean), sort(scenario$clean), info = name
    )
    expect_identical(
      sort(run$counters$workers$metadata), sort(scenario$metadata), info = name
    )
    expect_identical(
      sort(run$counters$workers$deflate), sort(scenario$deflate), info = name
    )
    for (stage in names(scenario$reasons)) {
      selected_stage <- stage
      ids <- scenario[[stage]]
      expected <- sort(scenario$reasons[[stage]])
      expect_true(all(units[
        get("stage") == selected_stage & entity_id %in% ids,
        vapply(reason_codes, function(x) identical(sort(x), expected), logical(1L))
      ]), info = paste(name, stage))
    }
    expect_identical(
      run$counters$household_reads, length(scenario$clean), info = name
    )

    rerun <- c4_pipeline_run(fixture)
    expect_true(all(c4_pipeline_units(rerun$result)$status == "cached"), info = name)
    expect_true(all(rerun$counters$writes == 0L), info = name)
    expect_identical(rerun$counters$household_reads, 0L, info = name)
  }
})

test_that("V13 force selectors are exact and additive to independent staleness", {
  selectors <- c(
    "COL_2018_GEIH_V01_M_V01_A_GMD_ALL",
    "COL_2018_GEIH_INC_ALL"
  )
  selected_pip <- c("COL_2018_GEIH_CON_ALL", "COL_2018_GEIH_INC_ALL")
  for (selector in selectors) {
    fixture <- c4_pipeline_fixture()
    run <- c4_pipeline_run(fixture, force_surveys = selector)
    units <- c4_pipeline_units(run$result)
    expect_identical(
      sort(run$counters$workers$clean),
      "COL_2018_GEIH_V01_M_V01_A_GMD_ALL",
      info = selector
    )
    expect_identical(sort(run$counters$workers$metadata), sort(selected_pip))
    expect_identical(sort(run$counters$workers$deflate), sort(selected_pip))
    expect_true(all(units[
      status == "success", vapply(
        reason_codes, function(x) "forced" %in% x, logical(1L)
      )
    ]))
  }

  fixture <- c4_pipeline_fixture()
  fixture$inv[country_code == "PER", `:=`(
    latest_version_id = "dlw-per-v2", content_hash = "dlw-per-h2"
  )]
  mixed <- c4_pipeline_run(
    fixture, force_surveys = "COL_2018_GEIH_V01_M_V01_A_GMD_ALL"
  )
  units <- c4_pipeline_units(mixed$result)
  expect_identical(
    sort(mixed$counters$workers$clean),
    sort(c(
      "COL_2018_GEIH_V01_M_V01_A_GMD_ALL",
      "PER_2018_ENAHO_V01_M_V01_A_GMD_ALL"
    ))
  )
  expect_true(all(units[
    survey_id == "COL_2018_GEIH_V01_M_V01_A_GMD_ALL" & status == "success",
    vapply(reason_codes, function(x) "forced" %in% x, logical(1L))
  ]))
  expect_true(all(units[
    survey_id == "PER_2018_ENAHO_V01_M_V01_A_GMD_ALL" & status == "success",
    !vapply(reason_codes, function(x) "forced" %in% x, logical(1L))
  ]))
})

test_that("V13 clean receipt sets converge for one multiple and permuted outputs", {
  fixture <- c4_pipeline_fixture()
  fixture$catalog_permuted <- TRUE
  run <- c4_pipeline_run(fixture)
  units <- c4_pipeline_units(run$result)
  expect_true(all(units$status == "cached"))
  expect_true(all(run$counters$writes == 0L))
  expect_identical(run$counters$household_reads, 0L)

  clean_records <- pd_manifest_read(fixture$context, fixture$root)$records[
    stage == "clean"
  ]
  receipt_counts <- vapply(
    clean_records$output_receipts, length, integer(1L)
  )
  expect_setequal(receipt_counts, c(1L, 1L, 2L))
})

test_that("V13 removed PIP output publishes one clean tombstone", {
  fixture <- c4_pipeline_fixture()
  removed <- "COL_2018_GEIH_CON_ALL"
  c4_pipeline_change_aux(fixture, "pfw", function(pfw) {
    pfw[!(
      country_code == "COL" & surveyid_year == 2018L &
        welfare_type == "consumption"
    )]
  })
  run <- c4_pipeline_run(fixture)
  manifest <- pd_manifest_read(fixture$context, fixture$root)

  expect_identical(
    run$counters$workers$clean,
    "COL_2018_GEIH_V01_M_V01_A_GMD_ALL"
  )
  expect_false(removed %in% fixture$master$pip_id)
  expect_identical(
    manifest$tombstones[pip_id == removed, .(pip_id, reason)],
    data.table::data.table(pip_id = removed, reason = "output_removed")
  )
  expect_false(removed %in% run$counters$workers$metadata)
  expect_false(removed %in% run$counters$workers$deflate)
})

test_that("V13 no-match and outside-selection auxiliary changes do no work", {
  changes <- list(
    no_match = function(cpi) {
      cpi[
        country_code == "COL" & year == 2018L & survey_acronym == "OTHER",
        cpi_value := cpi_value + 1
      ]
      cpi
    },
    outside_selection = function(cpi) {
      data.table::rbindlist(list(cpi, data.table::data.table(
        country_code = "ECU", year = 2020L, survey_acronym = "ENEMDU",
        cpi_year = 2017L, reporting_level = "national", cpi_value = 7
      )), fill = TRUE)
    }
  )
  for (name in names(changes)) {
    fixture <- c4_pipeline_fixture()
    before <- pd_manifest_read(fixture$context, fixture$root)
    before_identity <- attr(before, "manifest_identity")
    before_tables <- lapply(
      before[c("records", "inputs", "fingerprints", "tombstones")],
      pd_canonical_snapshot_table
    )
    c4_pipeline_change_aux(fixture, "cpi", changes[[name]])
    run <- c4_pipeline_run(fixture)
    after <- pd_manifest_read(fixture$context, fixture$root)
    after_tables <- lapply(
      after[c("records", "inputs", "fingerprints", "tombstones")],
      pd_canonical_snapshot_table
    )
    expect_true(all(c4_pipeline_units(run$result)$status == "cached"), info = name)
    expect_true(all(run$counters$writes == 0L), info = name)
    expect_identical(run$counters$household_reads, 0L, info = name)
    expect_identical(attr(after, "manifest_identity"), before_identity, info = name)
    expect_identical(after_tables$records, before_tables$records, info = name)
    expect_identical(after_tables$inputs, before_tables$inputs, info = name)
    expect_identical(
      after_tables$fingerprints, before_tables$fingerprints, info = name
    )
    expect_identical(
      after_tables$tombstones, before_tables$tombstones, info = name
    )
  }
})
