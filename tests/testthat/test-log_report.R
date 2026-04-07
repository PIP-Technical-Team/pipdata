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
  out <- build_processing_summary(log)
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
  expect_length(build_processing_summary(log), 0L)
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
  out <- build_aux_changes(log)
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
  expect_length(build_aux_changes(log), 0L)
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
  out <- build_aux_changes(log)
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
  out <- build_inventory_additions(log)
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
  out <- build_inventory_additions(log)
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
  out <- build_inventory_additions(log)
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
  expect_length(build_inventory_additions(log), 0L)
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

test_that("log_report errors on non-piplog input", {
  expect_error(log_report(data.frame(x = 1)), class = "rlang_error")
})

test_that("log_report errors on empty log", {
  log <- pipfun::log_init("test_empty", overwrite = TRUE)
  log <- pipfun::log_get("test_empty")
  pipfun::log_reset("test_empty")
  expect_error(log_report(log), class = "rlang_error")
})
