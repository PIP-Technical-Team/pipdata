# Helper: build a minimal piplog with controlled logmeta entries
make_piplog <- function(...) {
  entries <- list(...)
  dt <- data.table::rbindlist(entries, fill = TRUE)
  data.table::setattr(dt, "class", c("piplog", class(dt)))
  dt
}

make_entry <- function(event, message, logmeta) {
  data.table::data.table(
    time = Sys.time(),
    package = "",
    fun = "test_fun",
    event = event,
    message = message,
    args = list(list()),
    output = list(NULL),
    trace = list(NULL),
    logmeta = list(logmeta)
  )
}

# ── parse_log_meta ────────────────────────────────────────────────────────────

test_that("parse_log_meta extracts error_type from error entry", {
  log <- make_piplog(
    make_entry(
      "error",
      "bad things",
      list(error = "gd_type_miss", survey = "BOL_1990_EPF")
    )
  )
  dt <- parse_log_meta(log)
  expect_equal(dt$error_type, "gd_type_miss")
  expect_equal(dt$survey, "BOL_1990_EPF")
  expect_equal(dt$country, "BOL")
})

test_that("parse_log_meta extracts error_type from info entry", {
  log <- make_piplog(
    make_entry(
      "info",
      "done",
      list(info = "null_svys_inf", surveys = c("A", "B"))
    )
  )
  dt <- parse_log_meta(log)
  expect_equal(dt$error_type, "null_svys_inf")
  expect_true(is.na(dt$survey)) # $survey is NULL -> NA
  expect_true(is.na(dt$country))
})

test_that("parse_log_meta normalizes legacy condition discriminators", {
  log <- make_piplog(
    make_entry(
      "error",
      "legacy failure",
      list(error = simpleError("legacy boom"))
    )
  )

  dt <- parse_log_meta(log)

  expect_type(dt$error_type, "character")
  expect_equal(dt$error_type, "legacy_simpleError")
})

test_that("DLW logmeta discriminators are registered and report-suppressed", {
  expect_equal(.logtype_dlw_acquisition, "dlw_acquisition_inf")
  expect_equal(.logtype_dlw_validation, "dlw_validation_inf")
  expect_equal(.logtype_dlw_summary, "dlw_summary_inf")
  expect_length(.log_internal_types, 12L)
  expect_false(anyDuplicated(.log_internal_types) > 0L)
  expect_true(all(c(
    .logtype_dlw_acquisition,
    .logtype_dlw_validation,
    .logtype_dlw_summary
  ) %in% .log_internal_types))
  expect_true(all(c(
    "release_write_err", "deflate_summary_inf", "pipeline_run_summary_inf"
  ) %in% .log_internal_types))
})

test_that("pipeline run report selects the latest completed run", {
  first <- make_entry(
    "info", "first run",
    list(
      info = "pipeline_run_summary_inf", run_id = "run-1",
      status = "success", terminal = FALSE,
      n_selected = 3L, n_attempted = 3L, n_success = 3L, n_failed = 0L,
      n_cached = 0L, n_blocked = 0L, clean_status = "success",
      metadata_status = "success", deflate_status = "success",
      manifest_before_generation = 1L, manifest_after_generation = 4L,
      started_at = as.POSIXct("2026-08-31 10:00:00", tz = "UTC"),
      completed_at = as.POSIXct("2026-08-31 10:01:00", tz = "UTC")
    )
  )
  latest <- make_entry(
    "info", "latest run",
    list(
      info = "pipeline_run_summary_inf", run_id = "run-2",
      status = "cached", terminal = FALSE,
      n_selected = 3L, n_attempted = 0L, n_success = 0L, n_failed = 0L,
      n_cached = 3L, n_blocked = 0L, clean_status = "cached",
      metadata_status = "cached", deflate_status = "cached",
      manifest_before_generation = 4L, manifest_after_generation = 4L,
      started_at = as.POSIXct("2026-08-31 11:00:00", tz = "UTC"),
      completed_at = as.POSIXct("2026-08-31 11:00:01", tz = "UTC")
    )
  )

  output <- build_pipeline_run_summary(
    parse_log_meta(make_piplog(latest, first))
  )

  expect_true(any(grepl("Pipeline Run Summary", output)))
  expect_true(any(grepl("run-2", output)))
  expect_true(any(grepl("cached", output)))
  expect_false(any(grepl("run-1", output)))
})

test_that("stage summaries and header follow the latest pipeline run ID", {
  pipeline_summary <- function(run_id, completed_at) {
    make_entry("info", paste(run_id, "pipeline"), list(
      info = "pipeline_run_summary_inf", run_id = run_id,
      status = "success", terminal = FALSE,
      n_selected = 3L, n_attempted = 3L, n_success = 3L, n_failed = 0L,
      n_cached = 0L, n_blocked = 0L, clean_status = "success",
      metadata_status = "success", deflate_status = "success",
      manifest_before_generation = 1L, manifest_after_generation = 4L,
      started_at = completed_at - 1, completed_at = completed_at
    ))
  }
  process_summary <- function(run_id, total) {
    make_entry("info", paste(run_id, "clean"), list(
      info = "process_summary_inf", run_id = run_id, n_total = total,
      n_success = total, n_failed = 0L, surveys_success = character()
    ))
  }
  deflate_summary <- function(run_id, total) {
    make_entry("info", paste(run_id, "deflate"), list(
      info = "deflate_summary_inf", run_id = run_id, status = "success",
      n_total = total, n_success = total, n_failed = 0L,
      surveys_success = character(), surveys_failed = character(),
      cached = 0L, skipped = 0L
    ))
  }
  older_time <- as.POSIXct("2026-08-31 10:00:00", tz = "UTC")
  latest_time <- as.POSIXct("2026-08-31 11:00:00", tz = "UTC")
  dt <- parse_log_meta(make_piplog(
    process_summary("run-old", 99L),
    deflate_summary("run-new", 7L),
    pipeline_summary("run-new", latest_time),
    process_summary("run-new", 7L),
    deflate_summary("run-old", 99L),
    pipeline_summary("run-old", older_time)
  ))

  header <- build_header(dt, "Test Report")
  processing <- build_processing_summary(dt)
  deflation <- build_deflation_summary(dt)

  expect_true(any(grepl("7 total", header, fixed = TRUE)))
  expect_true(any(grepl("| 7 |", processing, fixed = TRUE)))
  expect_false(any(grepl("| 99 |", processing, fixed = TRUE)))
  expect_true(any(grepl("| 7 |", deflation, fixed = TRUE)))
  expect_false(any(grepl("| 99 |", deflation, fixed = TRUE)))
})

test_that("build_dlw_acquisition_summary uses denominator and failure rows", {
  log <- make_piplog(
    make_entry(
      "info",
      "DLW acquisition started.",
      list(
        info = "dlw_acquisition_inf",
        phase = "start",
        n_surveys = 3L
      )
    ),
    make_entry(
      "error",
      "download failed",
      list(
        error = "dlw_acquisition_inf",
        phase = "download",
        survey = "BOL_2020_EH",
        country = "BOL",
        year = 2020L,
        module = "ALL",
        condition_msg = "timeout"
      )
    ),
    make_entry(
      "info",
      "DLW acquisition complete.",
      list(
        info = "dlw_acquisition_inf",
        phase = "complete",
        n_surveys = 3L,
        n_success = 2L,
        n_failed = 1L
      )
    )
  )

  out <- build_dlw_acquisition_summary(parse_log_meta(log))

  expect_true(any(grepl("DLW Acquisition Summary", out)))
  expect_true(any(grepl("3 attempted, 2 succeeded, 1 failed", out)))
  expect_true(any(grepl("BOL_2020_EH", out)))
  expect_true(any(grepl("timeout", out)))
})

test_that("build_dlw_acquisition_summary omits malformed and absent input", {
  empty <- make_piplog(
    make_entry("error", "other", list(error = "other_error"))
  )
  malformed <- make_piplog(
    make_entry(
      "info",
      "missing denominator",
      list(info = "dlw_acquisition_inf", phase = "start")
    )
  )

  expect_length(build_dlw_acquisition_summary(parse_log_meta(empty)), 0L)
  expect_length(build_dlw_acquisition_summary(parse_log_meta(malformed)), 0L)
})

test_that("build_dlw_acquisition_summary reports no-work runs", {
  log <- make_piplog(
    make_entry(
      "info",
      "No new GMD data was found.",
      list(info = "dlw_acquisition_inf", phase = "no_new_data")
    )
  )

  out <- build_dlw_acquisition_summary(parse_log_meta(log))

  expect_true(any(grepl("0 attempted, 0 succeeded, 0 failed", out)))
})

test_that("build_dlw_acquisition_summary renders fatal workflow failures", {
  log <- make_piplog(
    make_entry(
      "error",
      "catalog failed",
      list(
        error = "dlw_acquisition_inf",
        phase = "catalog_load",
        condition_msg = "catalog unavailable"
      )
    )
  )

  out <- build_dlw_acquisition_summary(parse_log_meta(log))

  expect_true(any(grepl("Acquisition workflow failures", out)))
  expect_true(any(grepl("catalog unavailable", out)))
})

test_that("build_dlw_acquisition_summary aggregates repeated run starts", {
  log <- make_piplog(
    make_entry(
      "info",
      "run one",
      list(info = "dlw_acquisition_inf", phase = "start", n_surveys = 2L)
    ),
    make_entry(
      "info",
      "run two",
      list(info = "dlw_acquisition_inf", phase = "start", n_surveys = 3L)
    )
  )

  out <- build_dlw_acquisition_summary(parse_log_meta(log))

  expect_true(any(grepl("3 attempted, 3 succeeded, 0 failed", out)))
})

test_that("acquisition report uses only the latest attempt segment", {
  log <- make_piplog(
    make_entry(
      "info", "old boundary",
      list(info = "dlw_acquisition_inf", phase = "attempt_start")
    ),
    make_entry(
      "error", "obsolete acquisition",
      list(
        error = "dlw_acquisition_inf", phase = "download",
        survey = "OLD_2020_SURVEY", country = "OLD", year = 2020L,
        module = "ALL", condition_msg = "obsolete"
      )
    ),
    make_entry(
      "info", "old completion",
      list(
        info = "dlw_acquisition_inf", phase = "complete",
        outcome = "failed", n_total = 1L, n_success = 0L, n_failed = 1L,
        surveys_success = character(), surveys_failed = "OLD_2020_SURVEY"
      )
    ),
    make_entry(
      "info", "current boundary",
      list(info = "dlw_acquisition_inf", phase = "attempt_start")
    ),
    make_entry(
      "info", "current completion",
      list(
        info = "dlw_acquisition_inf", phase = "complete",
        outcome = "success", n_total = 1L, n_success = 1L, n_failed = 0L,
        surveys_success = "NEW_2021_SURVEY", surveys_failed = character()
      )
    )
  )

  out <- build_dlw_acquisition_summary(parse_log_meta(log))

  expect_true(any(grepl("1 attempted, 1 succeeded, 0 failed", out)))
  expect_false(any(grepl("OLD_2020_SURVEY|obsolete", out)))
})

test_that("malformed current acquisition completion falls back in-segment", {
  log <- make_piplog(
    make_entry(
      "info", "old boundary",
      list(info = "dlw_acquisition_inf", phase = "attempt_start")
    ),
    make_entry(
      "info", "old completion",
      list(
        info = "dlw_acquisition_inf", phase = "complete",
        outcome = "success", n_total = 9L, n_success = 9L, n_failed = 0L,
        surveys_success = paste0("OLD", seq_len(9L)),
        surveys_failed = character()
      )
    ),
    make_entry(
      "info", "current boundary",
      list(info = "dlw_acquisition_inf", phase = "attempt_start")
    ),
    make_entry(
      "info", "current start",
      list(info = "dlw_acquisition_inf", phase = "start", n_surveys = 2L)
    ),
    make_entry(
      "error", "current failure",
      list(
        error = "dlw_acquisition_inf", phase = "download",
        survey = "CUR_2022_SURVEY", country = "CUR", year = 2022L,
        module = "ALL", condition_msg = "current timeout"
      )
    ),
    make_entry(
      "info", "malformed current completion",
      list(
        info = "dlw_acquisition_inf", phase = "complete",
        outcome = "success", n_total = 2, n_success = 2L, n_failed = 0L,
        surveys_success = c("A", "B"), surveys_failed = character(),
        extra = TRUE
      )
    )
  )

  out <- build_dlw_acquisition_summary(parse_log_meta(log))

  expect_true(any(grepl("2 attempted, 1 succeeded, 1 failed", out)))
  expect_true(any(grepl("CUR_2022_SURVEY", out)))
  expect_false(any(grepl("9 attempted", out)))
})

test_that("acquisition report selects the latest valid in-segment completion", {
  log <- make_piplog(
    make_entry(
      "info", "current boundary",
      list(info = "dlw_acquisition_inf", phase = "attempt_start")
    ),
    make_entry(
      "info", "valid completion",
      list(
        info = "dlw_acquisition_inf", phase = "complete",
        outcome = "success", n_total = 1L, n_success = 1L, n_failed = 0L,
        surveys_success = "CUR_2022_SURVEY", surveys_failed = character()
      )
    ),
    make_entry(
      "info", "malformed duplicate completion",
      list(
        info = "dlw_acquisition_inf", phase = "complete",
        outcome = "success", n_total = 8, n_success = 8L, n_failed = 0L,
        surveys_success = paste0("BAD", seq_len(8L)),
        surveys_failed = character(), extra = TRUE
      )
    )
  )

  out <- build_dlw_acquisition_summary(parse_log_meta(log))

  expect_true(any(grepl("1 attempted, 1 succeeded, 0 failed", out)))
  expect_false(any(grepl("8 attempted", out)))
})

test_that("build_dlw_validation_summary groups workflow phases", {
  log <- make_piplog(
    make_entry(
      "info",
      "DLW validation started.",
      list(info = "dlw_validation_inf", phase = "start", n_surveys = 2L)
    ),
    make_entry(
      "error",
      "load failed",
      list(
        error = "dlw_validation_inf",
        phase = "load",
        survey = "BOL_2020_EH",
        condition_msg = "bad qs2"
      )
    ),
    make_entry(
      "info",
      "inventory saved",
      list(
        info = "dlw_validation_inf",
        phase = "inventory_save",
        artifact = "gmd_valid_inv"
      )
    ),
    make_entry(
      "error",
      "report failed",
      list(
        error = "dlw_validation_inf",
        phase = "report_load_fail",
        condition_msg = "missing report"
      )
    )
  )

  out <- build_dlw_validation_summary(parse_log_meta(log))

  expect_true(any(grepl("DLW Validation Summary", out)))
  expect_true(any(grepl("2 attempted", out)))
  expect_true(any(grepl("load", out)))
  expect_true(any(grepl("inventory_save", out)))
  expect_true(any(grepl("report_load_fail", out)))
  expect_true(any(grepl("BOL_2020_EH", out)))
  expect_true(any(grepl("missing report", out)))
})

test_that("validation completion separates invalid from execution failure", {
  log <- make_piplog(
    make_entry(
      "info", "validation boundary",
      list(info = "dlw_validation_inf", phase = "attempt_start")
    ),
    make_entry(
      "error", "classified invalid",
      list(
        error = "dlw_validation_inf", phase = "validation",
        survey = "BOL_2020_INVALID", validation_messages = "bad welfare"
      )
    ),
    make_entry(
      "error", "engine failed",
      list(
        error = "dlw_validation_inf", phase = "validation_engine",
        survey = "IND_2021_FAILED", condition_msg = "engine boom"
      )
    ),
    make_entry(
      "info", "validation completion",
      list(
        info = "dlw_validation_inf", phase = "complete",
        outcome = "partial", n_total = 3L, n_valid = 1L,
        n_invalid = 1L, n_failed = 1L,
        surveys_valid = "CHN_2019_VALID",
        surveys_invalid = "BOL_2020_INVALID",
        surveys_failed = "IND_2021_FAILED"
      )
    )
  )

  out <- build_dlw_validation_summary(parse_log_meta(log))

  expect_true(any(grepl("3 attempted, 1 valid, 1 invalid", out)))
  expect_true(any(grepl("1 execution failed", out)))
  expect_true(any(grepl("Invalid classifications", out)))
  expect_true(any(grepl("Execution failures", out)))
  expect_true(any(grepl("BOL_2020_INVALID", out)))
  expect_true(any(grepl("IND_2021_FAILED", out)))
})

test_that("validation report never falls back before the current boundary", {
  log <- make_piplog(
    make_entry(
      "info", "old boundary",
      list(info = "dlw_validation_inf", phase = "attempt_start")
    ),
    make_entry(
      "info", "old completion",
      list(
        info = "dlw_validation_inf", phase = "complete",
        outcome = "success", n_total = 8L, n_valid = 8L,
        n_invalid = 0L, n_failed = 0L,
        surveys_valid = paste0("OLD", seq_len(8L)),
        surveys_invalid = character(), surveys_failed = character()
      )
    ),
    make_entry(
      "info", "current boundary",
      list(info = "dlw_validation_inf", phase = "attempt_start")
    ),
    make_entry(
      "info", "current start",
      list(info = "dlw_validation_inf", phase = "start", n_surveys = 2L)
    ),
    make_entry(
      "error", "current invalid",
      list(
        error = "dlw_validation_inf", phase = "validation",
        survey = "CUR_2022_INVALID"
      )
    ),
    make_entry(
      "info", "malformed completion",
      list(
        info = "dlw_validation_inf", phase = "complete",
        outcome = "success", n_total = 2L, n_valid = 2L,
        n_invalid = 0L, n_failed = 0L,
        surveys_valid = c("A", "A"), surveys_invalid = character(),
        surveys_failed = character()
      )
    )
  )

  out <- build_dlw_validation_summary(parse_log_meta(log))

  expect_true(any(grepl("2 attempted, 1 valid, 1 invalid", out)))
  expect_false(any(grepl("8 attempted|OLD", out)))
})

test_that("build_stage_warning distinguishes stage combinations", {
  dlw_only <- make_piplog(
    make_entry(
      "info",
      "DLW complete",
      list(
        info = "dlw_summary_inf",
        get_dlw_data = TRUE,
        validate_dlw_data = TRUE
      )
    )
  )
  pipeline_only <- make_piplog(
    make_entry(
      "info",
      "Pipeline complete",
      list(info = "process_summary_inf", n_total = 1L, n_success = 1L, n_failed = 0L)
    )
  )
  no_op <- make_piplog(
    make_entry(
      "info",
      "DLW complete",
      list(
        info = "dlw_summary_inf",
        get_dlw_data = FALSE,
        validate_dlw_data = FALSE
      )
    )
  )
  neither <- make_piplog(
    make_entry("error", "unknown", list(error = "unknown_error"))
  )

  expect_true(any(grepl("Only DLW", build_stage_warning(parse_log_meta(dlw_only)))))
  expect_true(any(grepl("DLW acquisition was not", build_stage_warning(parse_log_meta(pipeline_only)))))
  expect_true(any(grepl("DLW no-op", build_stage_warning(parse_log_meta(no_op)))))
  expect_true(any(grepl("incomplete", build_stage_warning(parse_log_meta(neither)), ignore.case = TRUE)))
})

test_that("build_stage_warning handles mixed typed and untyped entries", {
  log <- make_piplog(
    make_entry("info", "untyped", list(stage = "unknown")),
    make_entry(
      "info",
      "pipeline",
      list(info = "process_summary_inf", n_total = 1L, n_success = 1L, n_failed = 0L)
    )
  )

  expect_no_error(build_stage_warning(parse_log_meta(log)))
})

test_that("log_report orders DLW sections before pipeline sections", {
  log <- make_piplog(
    make_entry(
      "info",
      "DLW acquisition started.",
      list(info = "dlw_acquisition_inf", phase = "start", n_surveys = 1L)
    ),
    make_entry(
      "info",
      "DLW validation started.",
      list(info = "dlw_validation_inf", phase = "start", n_surveys = 1L)
    ),
    make_entry(
      "info",
      "DLW complete",
      list(info = "dlw_summary_inf", get_dlw_data = TRUE, validate_dlw_data = TRUE)
    ),
    make_entry(
      "info",
      "Pipeline complete",
      list(info = "process_summary_inf", n_total = 1L, n_success = 1L, n_failed = 0L)
    )
  )

  report <- log_report(log)
  positions <- vapply(
    c(
      "## DLW Acquisition Summary",
      "## DLW Validation Summary",
      "## Processing Summary"
    ),
    function(section) match(section, report),
    integer(1)
  )

  expect_true(all(diff(positions) > 0L))
})

# ── build_type_summary ───────────────────────────────────────────────────────

test_that("build_type_summary excludes internal logmeta types", {
  log <- make_piplog(
    make_entry(
      "error",
      "There is no gd_type variable",
      list(error = "gd_type_miss", survey = "BOL_1990_EPF")
    ),
    make_entry(
      "info",
      "Processing complete.",
      list(
        info = "process_summary_inf",
        n_total = 1L,
        n_success = 0L,
        n_failed = 1L
      )
    ),
    make_entry(
      "info",
      "Aux changes.",
      list(info = "aux_changes_inf", measures = "cpi", n_surveys_affected = 1L)
    )
  )
  dt <- parse_log_meta(log)
  out <- build_type_summary(dt)
  expect_true(any(grepl("gd_type_miss", out))) # real error shown
  expect_false(any(grepl("process_summary", out))) # internal type excluded
  expect_false(any(grepl("aux_changes", out))) # internal type excluded
})

test_that("build_type_summary returns only-header when all types are internal", {
  log <- make_piplog(
    make_entry(
      "info",
      "done",
      list(info = "null_svys_inf", surveys = character(0))
    )
  )
  dt <- parse_log_meta(log)
  out <- build_type_summary(dt)
  # Header should still be present but no data rows
  expect_true(any(grepl("Summary by Type", out)))
  # No table data rows (lines starting with |`)
  expect_false(any(grepl("^\\| `", out)))
})

# ── build_processing_summary ─────────────────────────────────────────────────

test_that("build_processing_summary returns correct table rows", {
  log <- make_piplog(
    make_entry(
      "info",
      "Processing complete.",
      list(
        info = "process_summary_inf",
        n_total = 10L,
        n_success = 8L,
        n_failed = 2L,
        surveys_success = c("A_2000", "B_2001")
      )
    )
  )
  out <- build_processing_summary(parse_log_meta(log))
  expect_true(any(grepl("10", out)))
  expect_true(any(grepl("8", out)))
  expect_true(any(grepl("2", out)))
  expect_true(any(grepl("Processing Summary", out)))
})

test_that("build_processing_summary returns empty when entry absent", {
  log <- make_piplog(
    make_entry(
      "error",
      "oops",
      list(error = "unknown_error", survey = "X_2000")
    )
  )
  expect_length(build_processing_summary(parse_log_meta(log)), 0L)
})

# ── build_deflation_summary ─────────────────────────────────────────────────

test_that("build_deflation_summary renders counts and failed surveys", {
  log <- make_piplog(
    make_entry(
      "info",
      "Deflation pipeline complete.",
      list(
        info = "deflate_summary_inf",
        n_total = 3L,
        n_success = 2L,
        n_failed = 1L,
        surveys_success = c("A_2000", "B_2001"),
        surveys_failed = "C_2002"
      )
    )
  )
  out <- build_deflation_summary(parse_log_meta(log))
  expect_true(any(grepl("Deflation Summary", out)))
  expect_true(any(grepl("3", out)))
  expect_true(any(grepl("2", out)))
  expect_true(any(grepl("C_2002", out)))
})

test_that("build_deflation_summary returns empty when entry absent", {
  log <- make_piplog(
    make_entry(
      "error",
      "oops",
      list(error = "unknown_error", survey = "X_2000")
    )
  )
  expect_length(build_deflation_summary(parse_log_meta(log)), 0L)
})

test_that("build_type_summary excludes deflate_summary_inf", {
  log <- make_piplog(
    make_entry(
      "error",
      "There is no gd_type variable",
      list(error = "gd_type_miss", survey = "BOL_1990_EPF")
    ),
    make_entry(
      "info",
      "Deflation pipeline complete.",
      list(info = "deflate_summary_inf", n_total = 3L, n_success = 2L, n_failed = 1L)
    )
  )
  out <- build_type_summary(parse_log_meta(log))
  expect_true(any(grepl("gd_type_miss", out)))   # real error shown
  expect_false(any(grepl("deflate_summary", out))) # internal type excluded
})

# ── build_aux_changes ─────────────────────────────────────────────────────────

test_that("build_aux_changes lists changed measures, survey count, and survey IDs", {
  log <- make_piplog(
    make_entry(
      "info",
      "Aux changes.",
      list(
        info = "aux_changes_inf",
        measures = c("cpi", "ppp"),
        n_surveys_affected = 2L,
        surveys_affected = c("IND_2011_NSS", "BOL_1990_EPF")
      )
    )
  )
  out <- build_aux_changes(parse_log_meta(log))
  expect_true(any(grepl("cpi", out)))
  expect_true(any(grepl("ppp", out)))
  expect_true(any(grepl("2", out)))
  expect_true(any(grepl("IND_2011_NSS", out)))
  expect_true(any(grepl("BOL_1990_EPF", out)))
  expect_true(any(grepl("Auxiliary File Changes", out)))
})

test_that("build_aux_changes returns empty when entry absent", {
  log <- make_piplog(
    make_entry(
      "error",
      "oops",
      list(error = "unknown_error", survey = "X_2000")
    )
  )
  expect_length(build_aux_changes(parse_log_meta(log)), 0L)
})

test_that("build_aux_changes aggregates multiple aux_changes_inf entries", {
  log <- make_piplog(
    make_entry(
      "info",
      "Aux changes 1.",
      list(
        info = "aux_changes_inf",
        measures = "cpi",
        n_surveys_affected = 5L
      )
    ),
    make_entry(
      "info",
      "Aux changes 2.",
      list(
        info = "aux_changes_inf",
        measures = c("cpi", "ppp"),
        n_surveys_affected = 3L
      )
    )
  )
  out <- build_aux_changes(parse_log_meta(log))
  # Total n_affected should be 5 + 3 = 8
  expect_true(any(grepl("8", out)))
  # Unique measures: cpi, ppp (2 total)
  expect_true(any(grepl("2", out)))
})

# ── build_inventory_additions ─────────────────────────────────────────────────

test_that("build_inventory_additions shows confirmed and missing counts", {
  log <- make_piplog(
    make_entry(
      "info",
      "Inventory verification complete.",
      list(
        info = "inv_update_inf",
        n_expected = 5L,
        n_confirmed = 4L,
        n_missing = 1L,
        surveys_confirmed = c("A", "B", "C", "D"),
        surveys_missing = "E_2000"
      )
    )
  )
  out <- build_inventory_additions(parse_log_meta(log))
  expect_true(any(grepl("5", out)))
  expect_true(any(grepl("4", out)))
  expect_true(any(grepl("1", out)))
  expect_true(any(grepl("E_2000", out)))
  expect_true(any(grepl("Inventory Verification", out)))
})

test_that("build_inventory_additions detects error-level inv_update_inf (missing surveys)", {
  log <- make_piplog(
    make_entry(
      "error",
      "Some successfully cleaned surveys are missing from the master inventory.",
      list(
        error = "inv_update_inf",
        n_expected = 3L,
        n_confirmed = 2L,
        n_missing = 1L,
        surveys_confirmed = c("A_2000", "B_2001"),
        surveys_missing = "C_2002"
      )
    )
  )
  out <- build_inventory_additions(parse_log_meta(log))
  expect_true(any(grepl("Inventory Verification", out)))
  expect_true(any(grepl("C_2002", out)))
  expect_true(any(grepl("1", out)))
})

test_that("build_inventory_additions omits missing-surveys list when none missing", {
  log <- make_piplog(
    make_entry(
      "info",
      "All good.",
      list(
        info = "inv_update_inf",
        n_expected = 3L,
        n_confirmed = 3L,
        n_missing = 0L,
        surveys_confirmed = c("A", "B", "C"),
        surveys_missing = character(0)
      )
    )
  )
  out <- build_inventory_additions(parse_log_meta(log))
  expect_false(any(grepl(
    "missing from inventory:\\*\\*",
    out,
    ignore.case = TRUE
  )))
})

test_that("build_inventory_additions returns empty when entry absent", {
  log <- make_piplog(
    make_entry(
      "error",
      "oops",
      list(error = "unknown_error", survey = "X_2000")
    )
  )
  expect_length(build_inventory_additions(parse_log_meta(log)), 0L)
})

# ── build_null_surveys ───────────────────────────────────────────────────────

test_that("build_null_surveys lists surveys not cleaned", {
  log <- make_piplog(
    make_entry(
      "info",
      "Some surveys not cleaned.",
      list(
        info = "null_svys_inf",
        surveys = c("BOL_1990_EPF", "IND_2011_NSS")
      )
    )
  )
  out <- build_null_surveys(parse_log_meta(log))
  expect_true(any(grepl("Surveys Not Cleaned", out)))
  expect_true(any(grepl("BOL_1990_EPF", out)))
  expect_true(any(grepl("IND_2011_NSS", out)))
  expect_true(any(grepl("2", out))) # count in heading
})

test_that("build_null_surveys returns empty when entry absent", {
  log <- make_piplog(
    make_entry("error", "oops", list(error = "x", survey = "X_2000"))
  )
  expect_length(build_null_surveys(parse_log_meta(log)), 0L)
})

test_that("build_null_surveys returns empty when surveys is empty", {
  log <- make_piplog(
    make_entry(
      "info",
      "No failed surveys.",
      list(info = "null_svys_inf", surveys = character(0))
    )
  )
  expect_length(build_null_surveys(parse_log_meta(log)), 0L)
})

# ── build_country_table ───────────────────────────────────────────────────────

test_that("build_country_table pivots to wide markdown table", {
  log <- make_piplog(
    make_entry(
      "error",
      "No gd_type.",
      list(error = "gd_type_miss", survey = "BOL_1990_EPF")
    ),
    make_entry(
      "error",
      "No gd_type.",
      list(error = "gd_type_miss", survey = "IND_2011_NSS")
    ),
    make_entry(
      "error",
      "Bad welfare.",
      list(error = "welfare_miss", survey = "BOL_2000_EH")
    )
  )
  out <- build_country_table(parse_log_meta(log))
  expect_true(any(grepl("Breakdown by Country", out)))
  expect_true(any(grepl("BOL", out)))
  expect_true(any(grepl("IND", out)))
  expect_true(any(grepl("gd_type_miss", out)))
})

test_that("build_country_table reports no entries when all surveys are NA", {
  log <- make_piplog(
    make_entry(
      "info",
      "done",
      list(info = "null_svys_inf", surveys = character(0))
    )
  )
  out <- build_country_table(parse_log_meta(log))
  expect_true(any(grepl("No country-level entries found", out)))
})

test_that("build_country_table uses em-dash for missing country/type combos", {
  log <- make_piplog(
    make_entry(
      "error",
      "No gd_type.",
      list(error = "gd_type_miss", survey = "BOL_1990_EPF")
    ),
    make_entry(
      "error",
      "Bad welfare.",
      list(error = "welfare_miss", survey = "IND_2011_NSS")
    )
  )
  out <- build_country_table(parse_log_meta(log))
  # BOL has no welfare_miss row: cell should be em-dash
  bol_row <- out[grepl("^\\| BOL", out)]
  expect_true(any(grepl("\u2014", bol_row)))
})

# ── build_skipped_surveys ─────────────────────────────────────────────────────

test_that("build_skipped_surveys lists data and metadata skips", {
  log <- make_piplog(
    make_entry(
      "info",
      "Skipped data.",
      list(
        info = "skipped_svys_data",
        surveys = c("BOL_1990_EPF", "IND_2011_NSS"),
        reasons = c("missing_welfare", "bad_weights")
      )
    ),
    make_entry(
      "info",
      "Skipped metadata.",
      list(
        info = "skipped_svys_metadata",
        surveys = "CHN_2005_CHN",
        reasons = "no_pfw_match"
      )
    )
  )
  out <- build_skipped_surveys(parse_log_meta(log))
  expect_true(any(grepl("Skipped Surveys", out)))
  expect_true(any(grepl("BOL_1990_EPF", out)))
  expect_true(any(grepl("missing_welfare", out)))
  expect_true(any(grepl("CHN_2005_CHN", out)))
  expect_true(any(grepl("no_pfw_match", out)))
})

test_that("build_skipped_surveys returns empty when no skipped entries", {
  log <- make_piplog(
    make_entry("error", "oops", list(error = "x", survey = "X_2000"))
  )
  expect_length(build_skipped_surveys(parse_log_meta(log)), 0L)
})

test_that("build_skipped_surveys uses 'unknown' when reasons vector is shorter than surveys", {
  log <- make_piplog(
    make_entry(
      "info",
      "Skipped data.",
      list(
        info = "skipped_svys_data",
        surveys = c("BOL_1990_EPF", "IND_2011_NSS", "CHN_2005_CHN"),
        reasons = "missing_welfare" # only one reason for three surveys
      )
    )
  )
  out <- build_skipped_surveys(parse_log_meta(log))
  # Second and third surveys should fall back to "unknown"
  expect_true(sum(grepl("unknown", out)) >= 2L)
  # First survey should still get its actual reason
  expect_true(any(grepl("missing_welfare", out)))
})

# ── build_header ─────────────────────────────────────────────────────────────

test_that("build_header includes survey counts when process_summary_inf present", {
  log <- make_piplog(
    make_entry(
      "info",
      "done",
      list(
        info = "process_summary_inf",
        n_total = 20L,
        n_success = 18L,
        n_failed = 2L
      )
    )
  )
  dt <- parse_log_meta(log)
  out <- build_header(dt, "Test Report")
  expect_true(any(grepl("20", out)))
  expect_true(any(grepl("18", out)))
  expect_true(any(grepl("2", out)))
})

test_that("build_header works without process_summary_inf", {
  log <- make_piplog(
    make_entry(
      "error",
      "oops",
      list(error = "unknown_error", survey = "X_2000")
    )
  )
  dt <- parse_log_meta(log)
  out <- build_header(dt, "Test Report")
  expect_true(any(grepl("Test Report", out)))
  expect_false(any(grepl("cleaned", out)))
})

# ── log_report integration ────────────────────────────────────────────────────

test_that("log_report includes all sections when all logmeta entries present", {
  log <- make_piplog(
    make_entry(
      "error",
      "There is no gd_type variable",
      list(
        error = "gd_type_miss",
        survey = "BOL_1990_EPF_v01_M_v01_A_GMD_GROUP"
      )
    ),
    make_entry(
      "info",
      "Processing complete.",
      list(
        info = "process_summary_inf",
        n_total = 1L,
        n_success = 0L,
        n_failed = 1L,
        surveys_success = character(0)
      )
    ),
    make_entry(
      "info",
      "Aux changes.",
      list(
        info = "aux_changes_inf",
        measures = c("cpi", "ppp"),
        n_surveys_affected = 1L
      )
    ),
    make_entry(
      "info",
      "Inventory check.",
      list(
        info = "inv_update_inf",
        n_expected = 0L,
        n_confirmed = 0L,
        n_missing = 0L,
        surveys_confirmed = character(0),
        surveys_missing = character(0)
      )
    ),
    make_entry(
      "info",
      "Some surveys not cleaned.",
      list(
        info = "null_svys_inf",
        surveys = "BOL_1990_EPF_v01_M_v01_A_GMD_GROUP"
      )
    )
  )

  out <- log_report(log)

  expect_true(any(grepl("Processing Summary", out)))
  expect_true(any(grepl("Auxiliary File Changes", out)))
  expect_true(any(grepl("Inventory Verification", out)))
  expect_true(any(grepl("Surveys Not Cleaned", out)))
  expect_true(any(grepl("gd_type_miss", out)))
})

test_that("log_report gracefully omits optional sections when entries absent", {
  log <- make_piplog(
    make_entry(
      "error",
      "There is no gd_type variable",
      list(
        error = "gd_type_miss",
        survey = "BOL_1990_EPF"
      )
    )
  )
  out <- log_report(log)
  expect_false(any(grepl("Processing Summary", out)))
  expect_false(any(grepl("Auxiliary File Changes", out)))
  expect_false(any(grepl("Inventory Verification", out)))
})

test_that("rendered report keeps latest DLW details only in dedicated sections", {
  log <- make_piplog(
    make_entry(
      "info", "obsolete acquisition boundary",
      list(info = "dlw_acquisition_inf", phase = "attempt_start")
    ),
    make_entry(
      "error", "OBSOLETE_ACQUISITION_DETAIL",
      list(
        error = "dlw_acquisition_inf", phase = "download",
        survey = "OLD_2020_ACQUISITION", country = "OLD", year = 2020L,
        module = "ALL", condition_msg = "obsolete acquisition"
      )
    ),
    make_entry(
      "info", "obsolete acquisition completion",
      list(
        info = "dlw_acquisition_inf", phase = "complete",
        outcome = "failed", n_total = 1L, n_success = 0L, n_failed = 1L,
        surveys_success = character(),
        surveys_failed = "OLD_2020_ACQUISITION"
      )
    ),
    make_entry(
      "info", "current acquisition boundary",
      list(info = "dlw_acquisition_inf", phase = "attempt_start")
    ),
    make_entry(
      "error", "CURRENT_ACQUISITION_DETAIL",
      list(
        error = "dlw_acquisition_inf", phase = "download",
        survey = "CUR_2022_ACQUISITION", country = "CUR", year = 2022L,
        module = "ALL", condition_msg = "current acquisition"
      )
    ),
    make_entry(
      "info", "current acquisition completion",
      list(
        info = "dlw_acquisition_inf", phase = "complete",
        outcome = "failed", n_total = 1L, n_success = 0L, n_failed = 1L,
        surveys_success = character(),
        surveys_failed = "CUR_2022_ACQUISITION"
      )
    ),
    make_entry(
      "info", "obsolete validation boundary",
      list(info = "dlw_validation_inf", phase = "attempt_start")
    ),
    make_entry(
      "error", "OBSOLETE_VALIDATION_DETAIL",
      list(
        error = "dlw_validation_inf", phase = "validation",
        survey = "OLD_2020_VALIDATION"
      )
    ),
    make_entry(
      "info", "obsolete validation completion",
      list(
        info = "dlw_validation_inf", phase = "complete",
        outcome = "success", n_total = 1L, n_valid = 0L,
        n_invalid = 1L, n_failed = 0L, surveys_valid = character(),
        surveys_invalid = "OLD_2020_VALIDATION", surveys_failed = character()
      )
    ),
    make_entry(
      "info", "current validation boundary",
      list(info = "dlw_validation_inf", phase = "attempt_start")
    ),
    make_entry(
      "error", "CURRENT_INVALID_DETAIL",
      list(
        error = "dlw_validation_inf", phase = "validation",
        survey = "CUR_2022_INVALID", validation_messages = "invalid data"
      )
    ),
    make_entry(
      "error", "CURRENT_EXECUTION_DETAIL",
      list(
        error = "dlw_validation_inf", phase = "load",
        survey = "CUR_2022_FAILED", condition_msg = "load failed"
      )
    ),
    make_entry(
      "info", "current validation completion",
      list(
        info = "dlw_validation_inf", phase = "complete",
        outcome = "failed", n_total = 2L, n_valid = 0L,
        n_invalid = 1L, n_failed = 1L, surveys_valid = character(),
        surveys_invalid = "CUR_2022_INVALID",
        surveys_failed = "CUR_2022_FAILED"
      )
    ),
    make_entry(
      "info", "DLW complete",
      list(
        info = "dlw_summary_inf", phase = "complete",
        get_dlw_data = TRUE, validate_dlw_data = TRUE,
        outcome = "failed", acquisition_outcome = "failed",
        validation_outcome = "failed", acquisition_n_total = 1L,
        acquisition_n_success = 0L, acquisition_n_failed = 1L,
        validation_n_total = 2L, validation_n_valid = 0L,
        validation_n_invalid = 1L, validation_n_failed = 1L
      )
    ),
    make_entry(
      "error", "GENERIC_DETAIL",
      list(error = "gd_type_miss", survey = "GEN_2023_SURVEY")
    )
  )

  report <- log_report(log)
  type_start <- match("## Summary by Type", report)
  country_start <- match("## Breakdown by Country", report)
  inventory_start <- match("## Inventory Verification", report)
  country_end <- if (is.na(inventory_start)) length(report) else inventory_start - 1L
  type_section <- report[type_start:(country_start - 1L)]
  country_section <- report[country_start:country_end]

  expect_false(any(grepl("OLD_2020|OBSOLETE_", report)))
  expect_identical(sum(grepl("CUR_2022_ACQUISITION", report)), 1L)
  expect_identical(sum(grepl("CUR_2022_INVALID", report)), 1L)
  expect_identical(sum(grepl("CUR_2022_FAILED", report)), 1L)
  expect_true(any(grepl("GEN_2023_SURVEY|GEN", country_section)))
  expect_false(any(grepl("CUR_2022|dlw_(acquisition|validation|summary)_inf", type_section)))
  expect_false(any(grepl("CUR_2022|dlw_(acquisition|validation|summary)_inf", country_section)))
})

test_that("log_report writes file when path is provided", {
  log <- make_piplog(
    make_entry(
      "error",
      "oops",
      list(error = "unknown_error", survey = "X_2000")
    )
  )
  tmp <- withr::local_tempfile(fileext = ".md")
  log_report(log, path = tmp, overwrite = TRUE)
  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("Pipeline Log Report", content)))
})

test_that("log_report loads pipdata_log by default", {
  log <- make_piplog(
    make_entry("info", "pipeline", list(
      info = "process_summary_inf",
      n_total = 1L,
      n_success = 1L,
      n_failed = 0L
    ))
  )
  requested_name <- NULL
  testthat::local_mocked_bindings(
    log_filter = function(name, ...) {
      requested_name <<- name
      log
    },
    .package = "pipfun"
  )

  report <- log_report()

  expect_equal(requested_name, "pipdata_log")
  expect_true(any(grepl("Processing Summary", report)))
})

test_that("log_report errors on non-piplog input", {
  expect_error(log_report(data.frame(x = 1)), class = "rlang_error")
})

test_that("log_report errors on empty log", {
  log <- pipfun::log_init("test_empty", overwrite = TRUE)
  log <- pipfun::log_get("test_empty")
  pipfun::log_reset("test_empty")
  expect_error(log_report(log), class = "rlang_error")
})

# ── Enhanced edge-case tests for P2.1 ─────────────────────────────────────────

test_that("build_skipped_surveys returns character vector with proper structure", {
  log <- make_piplog(
    make_entry(
      "info",
      "Skipped data.",
      list(
        info = "skipped_svys_data",
        surveys = "BOL_1990_EPF",
        reasons = "missing_welfare"
      )
    )
  )
  out <- build_skipped_surveys(parse_log_meta(log))
  expect_type(out, "character")
  expect_true(length(out) > 0L)
  expect_true(any(grepl("^##", out))) # markdown heading
})

test_that("build_skipped_surveys formats surveys/reasons as markdown list", {
  log <- make_piplog(
    make_entry(
      "info",
      "Skipped data.",
      list(
        info = "skipped_svys_data",
        surveys = c("BOL_1990_EPF", "IND_2011_NSS"),
        reasons = c("missing_income", "bad_weights")
      )
    )
  )
  out <- build_skipped_surveys(parse_log_meta(log))
  # Should have markdown list items (starting with -)
  expect_true(any(grepl("^-", out)))
  # Both surveys and reasons should appear
  expect_true(any(grepl("BOL_1990_EPF", out)))
  expect_true(any(grepl("IND_2011_NSS", out)))
  expect_true(any(grepl("missing_income", out)))
  expect_true(any(grepl("bad_weights", out)))
})

test_that("build_null_surveys formats count correctly in heading", {
  log <- make_piplog(
    make_entry(
      "info",
      "Surveys not cleaned.",
      list(
        info = "null_svys_inf",
        surveys = c("A", "B", "C")
      )
    )
  )
  out <- build_null_surveys(parse_log_meta(log))
  # Heading should include count: "Surveys Not Cleaned (3)"
  expect_true(any(grepl("\\(3\\)", out)))
})

test_that("build_null_surveys output is properly formatted markdown", {
  log <- make_piplog(
    make_entry(
      "info",
      "Surveys not cleaned.",
      list(
        info = "null_svys_inf",
        surveys = c("BOL_1990_EPF", "IND_2011_NSS", "CHN_2005_CHN")
      )
    )
  )
  out <- build_null_surveys(parse_log_meta(log))
  expect_type(out, "character")
  # Should have heading
  expect_true(any(grepl("^##", out)))
  # Should have list items for each survey
  expect_true(sum(grepl("^-", out)) >= 3L)
})

test_that("build_country_table returns character vector with markdown delimiters", {
  log <- make_piplog(
    make_entry(
      "error",
      "Error.",
      list(error = "gd_type_miss", survey = "BOL_1990_EPF")
    )
  )
  out <- build_country_table(parse_log_meta(log))
  expect_type(out, "character")
  expect_true(length(out) > 0L)
  # Should have markdown table pipes
  expect_true(any(grepl("\\|", out)))
})

test_that("build_country_table table has consistent column structure", {
  log <- make_piplog(
    make_entry(
      "error",
      "Error 1.",
      list(error = "type_miss", survey = "BOL_1990_EPF")
    ),
    make_entry(
      "error",
      "Error 2.",
      list(error = "welfare_miss", survey = "IND_2011_NSS")
    )
  )
  out <- build_country_table(parse_log_meta(log))
  # All table rows should have consistent pipe count
  table_rows <- grep("^\\|", out, value = TRUE)
  expect_true(length(table_rows) > 0L)
  # Get pipe counts for each row
  pipe_counts <- lengths(strsplit(table_rows, "\\|"))
  # All rows should have same pipe count (consistent columns)
  expect_true(length(unique(pipe_counts)) == 1L)
})

test_that("build_country_table formats em-dash for missing values", {
  log <- make_piplog(
    make_entry(
      "error",
      "Error.",
      list(error = "type_a", survey = "BOL_1990_EPF")
    ),
    make_entry(
      "error",
      "Error.",
      list(error = "type_b", survey = "IND_2011_NSS")
    )
  )
  out <- build_country_table(parse_log_meta(log))
  # Should use em-dash (\u2014) for missing cells
  expect_true(any(grepl("\u2014", out)))
})
