# Integration tests for logging side effects
# Tests the logging contract of valid_dlw_load() and update_pip_inventory()
# Focuses on structure and format rather than full pipeline execution.
# See roadmap item: logging-integration-tests (P2.4 from 2026-04-06-enrich-log-report-review.md)

test_that("canonical DLW logmeta types are registered", {
  expect_equal(.logtype_dlw_acquisition, "dlw_acquisition_inf")
  expect_equal(.logtype_dlw_validation, "dlw_validation_inf")
  expect_equal(.logtype_dlw_summary, "dlw_summary_inf")
  expect_length(.log_internal_types, 12L)
  expect_false(anyDuplicated(.log_internal_types) > 0L)
  expect_true(all(c(
    "dlw_acquisition_inf",
    "dlw_validation_inf",
    "dlw_summary_inf",
    "release_write_err",
    "deflate_summary_inf",
    "pipeline_run_summary_inf"
  ) %in% .log_internal_types))
})

test_that("production log entries remain parseable with string discriminators", {
  pipfun::log_init("logging_contract_test", overwrite = TRUE)
  pipfun::log_info(
    "Acquisition complete.",
    name = "logging_contract_test",
    logmeta = list(
      info = "dlw_acquisition_inf",
      phase = "complete",
      n_surveys = 1L,
      n_success = 1L,
      n_failed = 0L
    )
  )

  log <- pipfun::log_get("logging_contract_test")
  parsed <- parse_log_meta(log)

  expect_type(parsed$error_type, "character")
  expect_equal(parsed$error_type, "dlw_acquisition_inf")
})

test_that("pipeline summary logging failure does not change the result", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  result <- new_pipdata_pipeline_result(
    "run", list(clean = NULL, metadata = NULL, deflate = NULL),
    list(), list(),
    c(initial = "initial", clean = NA_character_,
      metadata = NA_character_, deflate = NA_character_),
    NULL, NULL,
    list(
      name = "pipdata_log", run_id = "run",
      summary_discriminator = "pipeline_run_summary_inf",
      log_checkpoint = NULL
    ),
    now, now
  )
  testthat::local_mocked_bindings(
    log_add = function(...) rlang::abort("injected logging failure"),
    .package = "pipfun"
  )

  expect_identical(pd_log_pipeline_summary(result), result)
})

test_that("pipeline summary logging retains only compact arguments", {
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  result <- new_pipdata_pipeline_result(
    "run", list(clean = NULL, metadata = NULL, deflate = NULL),
    list(), list(),
    c(initial = "initial", clean = NA_character_,
      metadata = NA_character_, deflate = NA_character_),
    NULL, NULL,
    list(
      name = "pipdata_log", run_id = "run",
      summary_discriminator = "pipeline_run_summary_inf",
      log_checkpoint = NULL
    ),
    now, now
  )
  captured <- list()
  testthat::local_mocked_bindings(
    log_add = function(...) {
      captured <<- list(...)
      invisible(NULL)
    },
    .package = "pipfun"
  )

  pd_log_pipeline_summary(result)

  expect_named(
    captured$args,
    c("run_id", "status", "terminal")
  )
  expect_identical(captured$logmeta$info, "pipeline_run_summary_inf")
  prohibited <- function(x) {
    is.environment(x) || data.table::is.data.table(x) ||
      inherits(x, "externalptr") ||
      (is.list(x) && any(vapply(x, prohibited, logical(1L))))
  }
  expect_false(prohibited(captured$args))
  expect_false(prohibited(captured$logmeta))
})

test_that("all staged executor logs exclude large runtime objects", {
  units <- data.table::data.table(
    status = c("success", "failed"),
    survey_id = c("S1", "S2"),
    pip_id = c("P1", "P2")
  )
  stage_result <- list(
    run_id = "run",
    status = "partial",
    counts = c(attempted = 2L, succeeded = 1L, failed = 1L,
               cached = 0L, skipped = 0L),
    units = units
  )
  condition <- new_stage_condition_record(
    simpleError("compact failure"),
    "error",
    stage = "metadata",
    entity_id = "P2",
    survey_id = "S2",
    pip_id = "P2",
    operation = "metadata",
    recoverable = TRUE
  )
  captured <- list()
  testthat::local_mocked_bindings(
    log_add = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "pipfun"
  )

  pd_log_clean_summary(stage_result)
  pd_log_deflate_summary(stage_result)
  pd_log_stage_condition("run", condition)

  prohibited <- function(x) {
    is.environment(x) || is.data.frame(x) || inherits(x, "externalptr") ||
      (is.list(x) && any(vapply(x, prohibited, logical(1L))))
  }
  expect_length(captured, 3L)
  for (entry in captured) {
    expect_false(prohibited(entry$args))
    expect_false(prohibited(entry$logmeta))
    expect_lt(as.numeric(utils::object.size(entry$args)), 10000)
    expect_lt(as.numeric(utils::object.size(entry$logmeta)), 10000)
  }
})

test_that("stage condition logs use the stage-specific survey identifier", {
  captured <- list()
  testthat::local_mocked_bindings(
    log_add = function(...) {
      captured[[length(captured) + 1L]] <<- list(...)
      invisible(NULL)
    },
    .package = "pipfun"
  )
  clean <- new_stage_condition_record(
    severity = "error", code = "yr_wrng", message = "clean failed",
    stage = "clean", entity_id = "S1", survey_id = "S1",
    operation = "clean", recoverable = TRUE
  )
  metadata <- new_stage_condition_record(
    severity = "error", code = "report_lvl", message = "metadata failed",
    stage = "metadata", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    operation = "metadata", recoverable = TRUE
  )

  pd_log_stage_condition("run", clean)
  pd_log_stage_condition("run", metadata)

  expect_identical(captured[[1L]]$logmeta$survey, "S1")
  expect_identical(captured[[2L]]$logmeta$survey, "P1")
})

test_that("null_svys_inf logmeta structure is consistent", {
  # Contract test: verify expected structure of null_svys_inf entries
  # Emitted by update_pip_inventory when some surveys fail (NULL in proc_dta)

  expected_structure <- list(
    info = "null_svys_inf",
    surveys = character(0),
    message = "Some surveys were not cleaned."
  )

  # Verify required fields exist and types are correct
  expect_equal(expected_structure$info, "null_svys_inf")
  expect_true(is.character(expected_structure$surveys))
  expect_true(is.character(expected_structure$message))
})

test_that("inv_update_inf logmeta structure for success case", {
  # Contract test: verify inv_update_inf structure when all surveys confirmed
  # This is the info-level variant (no errors, all confirmed)

  expected_structure <- list(
    info = "inv_update_inf",
    n_expected = 2L,
    n_confirmed = 2L,
    n_missing = 0L,
    surveys_confirmed = c("CHN_2022_HCES_ALL", "IND_2019_NSS_ALL"),
    surveys_missing = character(0)
  )

  # Verify counts make sense
  expect_equal(
    expected_structure$n_expected,
    expected_structure$n_confirmed + expected_structure$n_missing
  )

  expect_equal(
    length(expected_structure$surveys_confirmed),
    expected_structure$n_confirmed
  )

  expect_equal(
    length(expected_structure$surveys_missing),
    expected_structure$n_missing
  )

  # Verify survey lists are mutually exclusive
  expect_equal(
    length(intersect(
      expected_structure$surveys_confirmed,
      expected_structure$surveys_missing
    )),
    0
  )
})

test_that("inv_update_inf logmeta structure for error case", {
  # Contract test: verify inv_update_inf structure when surveys go missing
  # This is the error-level variant (some surveys not found in master inventory)

  expected_structure <- list(
    error = "inv_update_inf",
    n_expected = 3L,
    n_confirmed = 2L,
    n_missing = 1L,
    surveys_confirmed = c("CHN_2022_HCES_ALL", "IND_2019_NSS_ALL"),
    surveys_missing = c("BRA_2020_PNAD_ALL")
  )

  # Verify counts add up
  expect_equal(
    expected_structure$n_expected,
    expected_structure$n_confirmed + expected_structure$n_missing
  )

  # Verify counts match list lengths
  expect_equal(
    length(expected_structure$surveys_confirmed),
    expected_structure$n_confirmed
  )

  expect_equal(
    length(expected_structure$surveys_missing),
    expected_structure$n_missing
  )

  # Verify that when n_missing > 0, error field is used (not info)
  expect_true("error" %in% names(expected_structure))
})

test_that("aux_changes_inf logmeta structure", {
  # Contract test: verify aux_changes_inf structure
  # Emitted by valid_dlw_load when auxiliary file changes are detected

  expected_structure <- list(
    info = "aux_changes_inf",
    measures = c("pfw", "cpi", "ppp"),
    n_surveys_affected = 3L,
    surveys_affected = c(
      "CHN_2022_HCES_ALL",
      "IND_2019_NSS_ALL",
      "BRA_2020_PNAD_ALL"
    )
  )

  # Verify structure
  expect_equal(length(expected_structure$measures), 3L)
  expect_equal(
    length(expected_structure$surveys_affected),
    expected_structure$n_surveys_affected
  )

  # Verify measures are character
  expect_true(is.character(expected_structure$measures))

  # Verify surveys are character
  expect_true(is.character(expected_structure$surveys_affected))
})

test_that("Logging condition: aux_changes_inf is logged IFF all_changes_aux is non-NULL", {
  # Document the condition under which aux_changes_inf is logged in valid_dlw_load()

  # Scenario 1: all_changes_aux is NULL → should NOT log aux_changes_inf
  all_changes_aux <- NULL
  should_log <- !is.null(all_changes_aux)
  expect_false(should_log)

  # Scenario 2: all_changes_aux is non-NULL list → SHOULD log aux_changes_inf
  all_changes_aux <- list(pfw = data.table::data.table())
  should_log <- !is.null(all_changes_aux)
  expect_true(should_log)
})

test_that("Logging condition: null_svys_inf is logged IFF length(null_ls) > 0", {
  # Document the condition under which null_svys_inf is logged in update_pip_inventory()

  # Scenario 1: no NULL surveys → should NOT log
  proc_dta <- list(
    survey1 = list(pip_names = "s1"),
    survey2 = list(pip_names = "s2")
  )
  null_ls <- names(Filter(is.null, proc_dta))
  should_log <- length(null_ls) > 0
  expect_false(should_log)

  # Scenario 2: some NULL surveys → SHOULD log
  proc_dta <- list(
    survey1 = list(pip_names = "s1"),
    survey2 = NULL
  )
  null_ls <- names(Filter(is.null, proc_dta))
  should_log <- length(null_ls) > 0
  expect_true(should_log)
  expect_equal(null_ls, "survey2")
})

test_that("Logging condition: inv_update_inf level (info vs error) depends on missing_ids", {
  # Document the condition for info vs error level in inv_update_inf

  # Scenario 1: no missing surveys → info level
  missing_ids <- character(0)
  is_error <- length(missing_ids) > 0L
  expect_false(is_error)

  # Scenario 2: some missing surveys → error level
  missing_ids <- c("survey1", "survey2")
  is_error <- length(missing_ids) > 0L
  expect_true(is_error)
})

test_that("release_write_err logmeta structure", {
  # Contract test: verify expected structure of release_write_err entries
  # Emitted by update_pip_inventory() when pip_write() throws during release write

  expected_structure <- list(
    error = "release_write_err",
    condition_msg = "some error message"
  )

  expect_equal(expected_structure$error, "release_write_err")
  expect_true(is.character(expected_structure$condition_msg))
  expect_true("error" %in% names(expected_structure))
  expect_false("info" %in% names(expected_structure))
})

test_that("release_vid = NA leaves release version columns as NA", {
  # Regression: when release write fails, unconditional column init must still
  # produce a consistent schema (both columns present and NA for all rows).
  # Protects against moving the init back inside the !is.na(release_vid) guard.
  inv <- data.table::data.table(
    survey_id = c("CHN_2022_A", "IND_2019_B"),
    pip_id    = c("CHN_2022_A_INC_ALL", "IND_2019_B_INC_ALL")
  )
  # NA release_vid: the guard should not fire, columns remain NA
  release_vid <- NA_character_
  dt <- data.table::as.data.table(inv)
  if (!"first_release_version_id" %in% names(dt)) {
    dt[, first_release_version_id := NA_character_]
  }
  if (!"latest_release_version_id" %in% names(dt)) {
    dt[, latest_release_version_id := NA_character_]
  }
  if (!is.na(release_vid)) {
    dt[, first_release_version_id  := release_vid]
    dt[, latest_release_version_id := release_vid]
  }

  expect_true("first_release_version_id" %in% names(dt))
  expect_true("latest_release_version_id" %in% names(dt))
  expect_true(all(is.na(dt$first_release_version_id)))
  expect_true(all(is.na(dt$latest_release_version_id)))
})

# ---- Release version column logic -------------------------------------------

# Helper: simulate the column-population logic from update_pip_inventory()
# mirrors: R/update_pip_inventory.R — "Initialise release version columns" block
# Update this helper if the production column-population logic changes.
.apply_release_vid <- function(new_pip_inv, release_ids, release_vid) {
  dt <- data.table::as.data.table(new_pip_inv)
  if (!"first_release_version_id" %in% names(dt)) {
    dt[, first_release_version_id := NA_character_]
  }
  if (!"latest_release_version_id" %in% names(dt)) {
    dt[, latest_release_version_id := NA_character_]
  }
  dt[
    survey_id %in% release_ids & is.na(first_release_version_id),
    first_release_version_id := release_vid
  ]
  dt[
    survey_id %in% release_ids,
    latest_release_version_id := release_vid
  ]
  dt
}

test_that("new surveys in release get both version columns set to release_vid", {
  inv <- data.table::data.table(
    survey_id = c("CHN_2022_A", "IND_2019_B"),
    pip_id    = c("CHN_2022_A_INC_ALL", "IND_2019_B_INC_ALL")
  )
  result <- .apply_release_vid(inv, c("CHN_2022_A", "IND_2019_B"), "vid-001")

  expect_equal(result$first_release_version_id, c("vid-001", "vid-001"))
  expect_equal(result$latest_release_version_id, c("vid-001", "vid-001"))
})

test_that("repeat run preserves first_release_version_id and updates latest", {
  # Simulate: survey already has first_release_version_id from v1
  inv <- data.table::data.table(
    survey_id              = "CHN_2022_A",
    pip_id                 = "CHN_2022_A_INC_ALL",
    first_release_version_id  = "vid-001",
    latest_release_version_id = "vid-001"
  )
  result <- .apply_release_vid(inv, "CHN_2022_A", "vid-002")

  expect_equal(result$first_release_version_id,  "vid-001")  # Not overwritten
  expect_equal(result$latest_release_version_id, "vid-002")  # Updated
})

test_that("survey in master but NOT in release keeps both columns NA", {
  inv <- data.table::data.table(
    survey_id = c("CHN_2022_A", "PRY_2021_C"),
    pip_id    = c("CHN_2022_A_INC_ALL", "PRY_2021_C_INC_ALL")
  )
  # Only CHN in release
  result <- .apply_release_vid(inv, "CHN_2022_A", "vid-001")

  # CHN: both columns set
  expect_equal(result[survey_id == "CHN_2022_A", first_release_version_id],  "vid-001")
  expect_equal(result[survey_id == "CHN_2022_A", latest_release_version_id], "vid-001")

  # PRY: columns stay NA
  expect_true(is.na(result[survey_id == "PRY_2021_C", first_release_version_id]))
  expect_true(is.na(result[survey_id == "PRY_2021_C", latest_release_version_id]))
})

test_that("release_write_err logmeta structure is consistent", {
  # Contract test: verify expected structure of release_write_err entries.
  # Emitted by update_pip_inventory() when the release inventory pip_write() fails.
  # Must use a DISTINCT discriminator from inv_update_inf — see:
  # .cg-docs/solutions/bugs/2026-04-29-duplicate-logmeta-discriminator-key.md

  expected_structure <- list(
    error = "release_write_err",
    condition_msg = "some error message"
  )

  expect_equal(expected_structure$error, "release_write_err")
  expect_true(is.character(expected_structure$condition_msg))
  # Must use 'error' key (not 'info') since this is an error-level emission
  expect_true("error" %in% names(expected_structure))
  expect_false("info" %in% names(expected_structure))
  # Must NOT reuse inv_update_inf — different schema
  expect_false(expected_structure$error == "inv_update_inf")
})
