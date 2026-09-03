# Tests for pd_process_data.R: force_surveys mutual-exclusivity guard and
# stamp versioning preservation.
#
# Covers:
#   - force = TRUE + force_surveys aborts with class "piperr" (C3)
#   - force_surveys never calls stamp::st_opts() (R3 / C1)
#   - force = TRUE alone still switches to timestamp versioning (R10 regression)

# ---------------------------------------------------------------------------
# force = TRUE is mutually exclusive with force_surveys
# ---------------------------------------------------------------------------

make_process_validation_inventory <- function() {
  data.table::data.table(
    survey_id = c(
      "BOL_2020_EH_V01_M_V01_A_GMD_ALL",
      "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL"
    ),
    pipeline_version = c(1L, 2L),
    latest_version_id = c("v1", "v2"),
    content_hash = c("hash-1", "hash-2"),
    file_path = c("bol.qs2", "zwe.qs2"),
    status = c("valid", "invalid"),
    data_available = "Yes",
    date_validated = as.POSIXct(
      c("2026-08-26 11:00:00", "2026-08-26 12:00:00"), tz = "UTC"
    ),
    Checksum = c("checksum-1", "checksum-2"),
    country_code = c("BOL", "ZWE"),
    surveyid_year = c(2020L, 2021L),
    survey_acronym = c("EH", "PICES"),
    vermast = c("v01", "v02"),
    veralt = "v01",
    collection = "GMD",
    module = "ALL",
    tool = "TB"
  )
}

test_that("pd_process_data aborts with piperr when force and force_surveys are both set", {
  # The guard must fire BEFORE the stamp-versioning side effect, so st_opts
  # must never be touched even with force = TRUE.
  st_opts_called <- 0L
  testthat::local_mocked_bindings(
    st_opts = function(x, .get = FALSE, versioning = NULL, ...) {
      st_opts_called <<- st_opts_called + 1L
      if (isTRUE(.get)) "content" else invisible(NULL)
    },
    .package = "stamp"
  )

  expect_error(
    pd_process_data(
      inv = data.table::data.table(survey_id = character(0)),
      force = TRUE,
      force_surveys = "COL_2020_GEIH",
      verbose = FALSE
    ),
    class = "piperr"
  )
  expect_equal(
    st_opts_called,
    0L,
    info = "the mutual-exclusivity guard must fire before the stamp versioning switch"
  )
})

# ---------------------------------------------------------------------------
# P1.1 regression: force_surveys appended after verbose preserves positional
# `verbose` compatibility (4th argument must still bind to verbose).
# ---------------------------------------------------------------------------

test_that("pd_process_data positional verbose call still binds to verbose", {
  # P1.1 regression: force_surveys must be appended AFTER verbose so existing
  # positional callers (inv, aux_measures, force, verbose) keep binding the 4th
  # slot to verbose. Assert the parameter order in the definition.
  fml <- names(formals(pd_process_data))
  expect_true("verbose" %in% fml)
  expect_true("force_surveys" %in% fml)
  expect_true(
    match("force_surveys", fml) > match("verbose", fml),
    info = "force_surveys must be appended after verbose to preserve positional compatibility"
  )
})

# ---------------------------------------------------------------------------
# force_surveys never calls stamp::st_opts()
# ---------------------------------------------------------------------------

test_that("pd_process_data never calls st_opts when only force_surveys is set", {
  st_opts_called <- 0L

  testthat::local_mocked_bindings(
    st_opts = function(x, .get = FALSE, versioning = NULL, ...) {
      st_opts_called <<- st_opts_called + 1L
      if (isTRUE(.get)) "content" else invisible(NULL)
    },
    .package = "stamp"
  )
  # Abort on inventory load so the test never touches the real pipeline.
  testthat::local_mocked_bindings(
    load_gmd_valid_inv = function(...) stop("stub inventory load"),
    .package = "pipload"
  )

  # force = FALSE + force_surveys: the mutual-exclusivity guard passes and the
  # run proceeds to inventory load, which we stub to stop. st_opts must never
  # have been called along the force_surveys path.
  expect_error(
    pd_process_data(
      inv = NULL,
      force = FALSE,
      force_surveys = "COL_2020_GEIH",
      verbose = FALSE
    ),
    "stub inventory load"
  )

  expect_equal(st_opts_called, 0L)
})

# ---------------------------------------------------------------------------
# force = TRUE alone still switches stamp versioning to "timestamp" (R10)
# ---------------------------------------------------------------------------

test_that("pd_process_data force waits for authoritative preflight", {
  versioning_requests <- character(0)

  testthat::local_mocked_bindings(
    st_opts = function(x, .get = FALSE, versioning = NULL, ...) {
      if (!isTRUE(.get)) {
        versioning_requests <<- c(versioning_requests, versioning)
      }
      if (isTRUE(.get)) "content" else invisible(NULL)
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    load_gmd_valid_inv = function(...) stop("stub inventory load"),
    .package = "pipload"
  )

  # Inventory loading and bootstrap validation are read-only preflight work.
  expect_error(
    pd_process_data(
      inv = NULL,
      force = TRUE,
      force_surveys = NULL,
      verbose = FALSE
    ),
    "stub inventory load"
  )

  expect_false(
    "timestamp" %in% versioning_requests,
    info = "force must not mutate versioning before authoritative preflight"
  )
})

test_that("pd_process_data returns authoritative no-op master unchanged", {
  inv <- make_process_validation_inventory()[1L]
  master <- data.table::data.table(survey_id = "s", pip_id = "p")
  prepared <- FALSE
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function() list(scope_id = "scope"),
    pd_prepare_execution = function(...) {
      prepared <<- TRUE
      list(plan = list(actions = pd_empty_actions()), lease = list())
    },
    pd_lease_release = function(...) invisible(NULL),
    .package = "pipdata"
  )
  out <- pd_process_data(inv, verbose = FALSE)
  expect_true(prepared)
  expect_identical(out, master)
})

test_that("pd_process_data filters retry rows before dependency preparation", {
  inv <- make_process_validation_inventory()
  retry <- inv[1L]
  retry[, `:=`(
    survey_id = "PER_2022_ENAHO_V01_M_V01_A_GMD_ALL",
    latest_version_id = "",
    content_hash = "",
    file_path = "",
    status = "",
    data_available = "No"
  )]
  inv <- data.table::rbindlist(list(inv, retry))
  observed <- NULL
  master <- data.table::data.table(survey_id = character(), pip_id = character())

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function() list(scope_id = "scope"),
    pd_prepare_execution = function(inv, ...) {
      observed <<- data.table::copy(inv)
      list(plan = list(actions = pd_empty_actions()), lease = list())
    },
    pd_lease_release = function(...) invisible(NULL),
    .package = "pipdata"
  )

  pd_process_data(inv = inv, verbose = FALSE)
  expect_false(retry$survey_id %in% observed$survey_id)
  expect_true(any(observed$status == "invalid"))
})

test_that("pd_process_data filters loaded durable retry rows", {
  inv <- make_process_validation_inventory()
  retry <- inv[1L]
  retry[, `:=`(
    survey_id = "PER_2022_ENAHO_V01_M_V01_A_GMD_ALL",
    latest_version_id = "",
    content_hash = "",
    file_path = "",
    status = "",
    data_available = "No"
  )]
  durable <- data.table::rbindlist(list(inv, retry))
  observed <- NULL
  master <- data.table::data.table(survey_id = character(), pip_id = character())

  testthat::local_mocked_bindings(
    load_gmd_valid_inv = function(...) durable,
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function() list(scope_id = "scope"),
    pd_prepare_execution = function(inv, ...) {
      observed <<- data.table::copy(inv)
      list(plan = list(actions = pd_empty_actions()), lease = list())
    },
    pd_lease_release = function(...) invisible(NULL),
    .package = "pipdata"
  )

  pd_process_data(inv = NULL, verbose = FALSE)
  expect_false(retry$survey_id %in% observed$survey_id)
})

test_that("new clean outputs refresh accepted metadata facts before checkpoint", {
  inv <- make_process_validation_inventory()[1L]
  survey_id <- inv$survey_id[[1L]]
  pip_id <- "BOL_2020_EH_INC_ALL"
  context <- list(scope_id = "scope")
  initial_master <- data.table::data.table(
    survey_id = character(), pip_id = character()
  )
  clean_action <- data.table::data.table(
    stage = "clean", entity_id = survey_id, survey_id = survey_id,
    pip_id = NA_character_, action = "create",
    input_hash = "clean-input", code_hash = "clean-code",
    expected_pip_ids = list(pip_id)
  )
  execution <- list(
    context = context, lease = list(), manifest = pd_empty_manifest(context),
    manifest_identity = NULL,
    snapshot = list(
      inventory = inv, master = initial_master,
      measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
      aux = list(), catalogs = list(), fingerprints = list()
    ),
    plan = list(actions = clean_action, reasons = pd_empty_reasons())
  )
  refreshed <- FALSE
  metadata_checkpointed <- FALSE
  clean_receipts <- data.table::data.table(
    survey_id = survey_id, pip_id = pip_id, alias = "pip",
    artifact = pip_id, path = "clean.qs2", version_id = "data-v1",
    content_hash = "data-h1", success = TRUE,
    input_hash = "clean-input", code_hash = "clean-code"
  )
  refreshed_master <- data.table::data.table(
    survey_id = survey_id, pip_id = pip_id,
    version_id_data = "data-v1", content_hash_data = "data-h1"
  )
  accepted_inputs <- pd_build_input_rows(
    "metadata", pip_id,
    data.table::data.table(
      name = c("clean_data", "aux_cpi"),
      version_id = c("data-v1", "cpi-v1"),
      content_hash = c("data-h1", "cpi-h1")
    )
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) initial_master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function() context,
    pd_prepare_execution = function(...) execution,
    pd_lease_release = function(...) invisible(NULL),
    pd_stage_context = function(execution, run_id, ...) list(run_id = run_id),
    pd_assert_metadata_prerequisite = function(...) invisible(NULL),
    sync_recode_spec = function(...) list(),
    pd_execute_clean = function(...) list(
      stage = "clean", survey_id = survey_id, success = TRUE,
      expected_pip_ids = pip_id, receipts = clean_receipts,
      metadata = stats::setNames(list(list()), pip_id)
    ),
    pd_build_dependency_snapshot = function(inv, master, context, ...) {
      refreshed <<- TRUE
      list(
        context = context, inventory = inv, master = master,
        measures = execution$snapshot$measures,
        aux = execution$snapshot$aux, catalogs = list(),
        fingerprints = list(),
        current = data.table::data.table(
          stage = "metadata", entity_id = pip_id, survey_id = survey_id,
          pip_id = pip_id, input_hash = "metadata-input",
          code_hash = "metadata-code", input_rows = list(accepted_inputs),
          aux_projection = list(list()), data_version_id = "data-v1",
          data_hash = "data-h1"
        )
      )
    },
    pd_snapshot_facts = function(...) data.table::data.table(
      stage = "metadata", entity_id = pip_id, survey_id = survey_id,
      pip_id = pip_id, reason = "new_entity", input = "manifest",
      old = NA_character_, new = "metadata-input"
    ),
    pd_snapshot_identity = function(...) "refreshed-snapshot",
    pd_dependency_plan = function(..., snapshot) structure(
      list(
        context = context,
        actions = data.table::copy(snapshot$current)[, action := "refresh"],
        reasons = data.table::data.table(
          stage = "metadata", entity_id = pip_id, reason = "new_entity",
          input = "manifest", old = NA_character_, new = "metadata-input"
        ),
        snapshot = snapshot
      ),
      class = "pip_dependency_plan"
    ),
    pd_assert_bootstrap = function(plan, ...) plan,
    pd_assert_execution_fence = function(...) invisible(NULL),
    pd_execute_metadata = function(action, ...) list(
      stage = "metadata", pip_id = action$pip_id[[1L]], success = TRUE,
      version_id = "meta-v1", content_hash = "meta-h1"
    ),
    pd_finalize_checkpoint = function(execution, master, stage, ...) {
      if (identical(stage, "clean")) {
        execution$manifest_identity <- list(
          filename = "manifest-v1-1.rds", uuid = "u1", checksum = "c1",
          generation = 1
        )
        return(list(candidate = refreshed_master, execution = execution))
      }
      metadata_checkpointed <<- any(
        execution$snapshot$current$stage == "metadata" &
          execution$snapshot$current$entity_id == pip_id
      )
      if (!metadata_checkpointed) {
        rlang::abort("metadata facts were not refreshed")
      }
      list(candidate = master, execution = execution)
    },
    .package = "pipdata"
  )

  expect_no_error(pd_process_data(inv = inv, verbose = FALSE))
  expect_true(refreshed)
  expect_true(metadata_checkpointed)
})

test_that("prepared clean core accounts for cached work without household loads", {
  action <- data.table::data.table(
    stage = "clean", entity_id = "S1", survey_id = "S1",
    pip_id = NA_character_, action = "none", expected_pip_ids = list("P1")
  )
  execution <- list(plan = list(actions = action), lease = list())
  context <- list(run_id = "run")
  master <- data.table::data.table(survey_id = "S1", pip_id = "P1")
  household_loads <- 0L
  testthat::local_mocked_bindings(
    inv_dlw_load = function(...) {
      household_loads <<- household_loads + 1L
      stop("cached clean work must not load household data")
    },
    pd_execute_clean = function(...) stop("cached clean work reached worker"),
    pd_finalize_checkpoint = function(...) stop("cached clean work checkpointed"),
    .package = "pipdata"
  )

  out <- pd_run_clean_stage_prepared(
    execution = execution, actions = action, run_id = "run",
    context = context, master = master,
    inv = make_process_validation_inventory()[1L],
    options = pd_pipeline_options(), recode_spec = list(), verbose = FALSE
  )

  expect_identical(out$outcome$units$status, "cached")
  expect_identical(out$outcome$units$reason_codes[[1L]], "current")
  expect_identical(household_loads, 0L)
  expect_identical(out$master, master)
})

test_that("prepared clean core lets unknown worker conditions escape", {
  inv <- make_process_validation_inventory()[1L]
  inv[, survey_id := "S1"]
  action <- data.table::data.table(
    stage = "clean", entity_id = "S1", survey_id = "S1",
    pip_id = NA_character_, action = "rebuild", input_hash = "input",
    code_hash = "code", expected_pip_ids = list("P1")
  )
  execution <- list(plan = list(actions = action), lease = list())
  testthat::local_mocked_bindings(
    pd_execute_clean = function(...) {
      rlang::abort("unknown storage failure", class = "unknown_storage_failure")
    },
    .package = "pipdata"
  )

  expect_error(
    pd_run_clean_stage_prepared(
      execution, action, "run", list(run_id = "run"),
      data.table::data.table(survey_id = "S1", pip_id = "P1"),
      inv, pd_pipeline_options(),
      recode_spec = list(), verbose = FALSE
    ),
    class = "unknown_storage_failure"
  )
})

test_that("prepared clean core commits one complete multi-output survey", {
  inv <- make_process_validation_inventory()[1L]
  inv[, survey_id := "S1"]
  action <- data.table::data.table(
    stage = "clean", entity_id = "S1", survey_id = "S1",
    pip_id = NA_character_, action = "rebuild", input_hash = "input",
    code_hash = "code", expected_pip_ids = list(c("P1", "P2"))
  )
  receipts <- data.table::data.table(
    stage = "clean", survey_id = "S1", pip_id = c("P1", "P2"),
    alias = "pip", artifact = c("P1", "P2"),
    path = c("p1.qs2", "p2.qs2"), version_id = c("v1", "v2"),
    content_hash = c("h1", "h2"), success = TRUE,
    input_hash = "input", code_hash = "code"
  )
  execution <- list(plan = list(actions = action), lease = list())
  checkpoint_calls <- 0L
  testthat::local_mocked_bindings(
    pd_execute_clean = function(...) list(
      stage = "clean", survey_id = "S1", success = TRUE,
      expected_pip_ids = c("P1", "P2"), receipts = receipts,
      metadata = list(P1 = list(), P2 = list())
    ),
    pd_finalize_checkpoint = function(execution, master, ...) {
      checkpoint_calls <<- checkpoint_calls + 1L
      execution$manifest_identity <- list(
        filename = "manifest-v1-2.rds", uuid = "u2", checksum = "c2",
        generation = 2
      )
      list(candidate = master, execution = execution)
    },
    .package = "pipdata"
  )

  out <- pd_run_clean_stage_prepared(
    execution, action, "run", list(run_id = "run"),
    data.table::data.table(survey_id = character(), pip_id = character()),
    inv,
    pd_pipeline_options(checkpoint_size = 25L),
    recode_spec = list(), verbose = FALSE
  )

  expect_identical(checkpoint_calls, 1L)
  expect_identical(out$outcome$units$status, "success")
  expect_identical(nrow(out$outcome$receipts[["S1"]]), 2L)
})

test_that("standalone process adapter normalizes aux subset without reordering", {
  inv <- make_process_validation_inventory()[1L]
  master <- data.table::data.table(survey_id = "S1", pip_id = "P1")
  observed <- NULL
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function() list(scope_id = "scope"),
    pd_prepare_execution = function(..., measures) {
      observed <<- measures
      list(plan = list(actions = pd_empty_actions()), lease = list())
    },
    pd_lease_release = function(...) invisible(NULL),
    .package = "pipdata"
  )

  pd_process_data(inv, aux_measures = c("PCE", "cpi", "PCE"), verbose = FALSE)

  expect_identical(observed, c("pce", "cpi"))
})

test_that("standalone process adapter actively validates its metadata subset", {
  inv <- make_process_validation_inventory()[1L]
  survey_id <- inv$survey_id[[1L]]
  action <- data.table::data.table(
    stage = "metadata", entity_id = "P1", survey_id, pip_id = "P1",
    action = "refresh", input_hash = "metadata-input", code_hash = "meta-code",
    data_version_id = "data-v1", data_hash = "data-h1",
    metadata_version_id = "meta-v1", metadata_hash = "meta-h1"
  )
  action[, aux_projection := list(list(
    pce = stats::setNames(6, "2020_national"),
    cpi = stats::setNames(2, "2017_national")
  ))]
  clean_action <- data.table::data.table(
    stage = "clean", entity_id = survey_id, survey_id,
    pip_id = NA_character_, action = "none", expected_pip_ids = list("P1")
  )
  actions <- data.table::rbindlist(list(clean_action, action), fill = TRUE)
  clean_receipt <- list(
    alias = "pip", artifact = "P1", path = "p1.qs2",
    version_id = "data-v1", content_hash = "data-h1"
  )
  master <- data.table::data.table(
    survey_id, pip_id = "P1", version_id_data = "data-v1",
    content_hash_data = "data-h1", version_id_metadata = "meta-v1",
    content_hash_metadata = "meta-h1"
  )
  observed_measures <- NULL
  saved <- NULL
  dependency_context <- list(
    schema_version = 1L,
    release = "20260831",
    identity = "TEST",
    roots = as.list(stats::setNames(
      paste0("root/", c(
        "pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv"
      )),
      c("pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv")
    )),
    namespace = "legacy-subset-test"
  )
  dependency_context$scope_id <- pd_context_hash(dependency_context)
  execution <- list(
    context = dependency_context,
    plan = list(
      actions = actions,
      reasons = data.table::data.table(
        stage = "metadata", entity_id = "P1", reason = "aux_pce_changed",
        input = "aux_pce", old = "old", new = "new"
      )
    ),
    snapshot = list(
      measures = c("pce", "cpi"), metadata_measures = c("pce", "cpi"),
      fingerprints = list(summary = data.table::data.table(
        stage = c("clean", "metadata", "deflate"),
        hash = c("clean-code", "meta-code", "deflate-code")
      )),
      captured_at = Sys.time()
    ),
    manifest = list(records = data.table::data.table(
      stage = "clean", entity_id = survey_id,
      output_receipts = list(list(clean_receipt))
    )),
    manifest_identity = NULL,
    lease = list()
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) data.table::copy(master),
    load_pip_data = function(pip_id, version, alias, verbose) list(
      pce = stats::setNames(5, "2020_national"),
      cpi = stats::setNames(1, "2017_national")
    ),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_hash_obj = function(x) "meta-h1",
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function() dependency_context,
    pd_prepare_execution = function(..., measures, metadata_measures) {
      observed_measures <<- list(
        measures = measures, metadata_measures = metadata_measures
      )
      execution
    },
    pd_lease_release = function(...) invisible(NULL),
    pd_assert_execution_fence = function(...) invisible(NULL),
    pd_save_receipt = function(x, ...) {
      saved <<- x
      list(
        success = TRUE, alias = "pip_meta", artifact = "P1",
        path = "p1.qs2", version_id = "meta-v2", content_hash = "meta-h2"
      )
    },
    pd_finalize_checkpoint = function(execution, master, ...) {
      list(candidate = master, execution = execution)
    },
    .package = "pipdata"
  )

  expect_no_error(pd_process_data(
    inv, aux_measures = c("PCE", "cpi", "PCE"), verbose = FALSE
  ))
  expect_identical(observed_measures, list(
    measures = c("pce", "cpi"), metadata_measures = c("pce", "cpi")
  ))
  expect_setequal(names(saved), c("pce", "cpi"))
  expect_identical(saved$pce, stats::setNames(6, "2020_national"))
})
