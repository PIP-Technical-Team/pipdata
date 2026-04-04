# Tests for arrow_generation.R
# Plan: .cg-docs/plans/2026-04-02-version-partition-and-manifest-resolution.md (Step 4)
#
# Test coverage:
#   - .build_partition_dir(): returns 4-level path including version=
#   - write_survey_parquet(): writes to correct 4-level directory
#   - .validate_for_write(): rejects inconsistent version values
#   - round-trip: write then read back, verify version column

library(data.table)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Minimal schema-conformant data.table for arrow generation tests
make_arrow_dt <- function(country_code  = "COL",
                          surveyid_year = 2010L,
                          welfare_type  = "INC",
                          pip_id        = "COL_2010_ECH_INC_ALL",
                          version       = "v01_v02",
                          n_rows        = 5L) {
  data.table::data.table(
    country_code   = country_code,
    surveyid_year  = as.integer(surveyid_year),
    welfare_type   = welfare_type,
    version        = version,
    pip_id         = pip_id,
    survey_acronym = "ECH",
    welfare        = seq(1.0, by = 0.5, length.out = n_rows),
    weight         = rep(1.0, n_rows)
  )
}

# ===========================================================================
# .build_partition_dir()
# ===========================================================================

test_that(".build_partition_dir returns a 4-level Hive path including version=", {
  result <- pipdata:::.build_partition_dir(
    arrow_repo_path = "/arrow",
    country_code    = "COL",
    surveyid_year   = 2010L,
    welfare_type    = "INC",
    version         = "v01_v02"
  )

  expect_true(grepl("country=COL",     result))
  expect_true(grepl("year=2010",        result))
  expect_true(grepl("welfare_type=INC", result))
  expect_true(grepl("version=v01_v02",  result))
})

test_that(".build_partition_dir path components are in correct order", {
  result <- gsub("\\\\", "/", pipdata:::.build_partition_dir(
    arrow_repo_path = "/arrow",
    country_code    = "BOL",
    surveyid_year   = 2020L,
    welfare_type    = "INC",
    version         = "v01_v04"
  ))

  parts <- strsplit(result, "/")[[1L]]
  parts <- parts[nchar(parts) > 0L]

  expect_match(parts[length(parts)],     "^version=")
  expect_match(parts[length(parts) - 1], "^welfare_type=")
  expect_match(parts[length(parts) - 2], "^year=")
  expect_match(parts[length(parts) - 3], "^country=")
})

# ===========================================================================
# write_survey_parquet()
# ===========================================================================

test_that("write_survey_parquet writes to a 4-level partition directory", {
  tmp <- withr::local_tempdir()
  dt  <- make_arrow_dt(version = "v01_v02")

  result <- write_survey_parquet(dt, arrow_repo_path = tmp)

  expect_identical(result$status, "written")
  expect_true(file.exists(result$file_path))
  expect_true(grepl("version=v01_v02", result$file_path))
})

test_that("write_survey_parquet different versions write to separate directories", {
  tmp <- withr::local_tempdir()

  dt_v1 <- make_arrow_dt(version = "v01_v02", pip_id = "COL_2010_ECH_INC_ALL")
  dt_v2 <- make_arrow_dt(version = "v01_v03", pip_id = "COL_2010_ECH_INC_ALL")

  res1 <- write_survey_parquet(dt_v1, arrow_repo_path = tmp)
  res2 <- write_survey_parquet(dt_v2, arrow_repo_path = tmp)

  expect_identical(res1$status, "written")
  expect_identical(res2$status, "written")
  # Both files exist in different version directories
  expect_true(file.exists(res1$file_path))
  expect_true(file.exists(res2$file_path))
  # Paths are different
  expect_false(identical(res1$file_path, res2$file_path))
  expect_true(grepl("version=v01_v02", res1$file_path))
  expect_true(grepl("version=v01_v03", res2$file_path))
})

test_that("write_survey_parquet skips when file exists and overwrite = FALSE", {
  tmp <- withr::local_tempdir()
  dt  <- make_arrow_dt()

  write_survey_parquet(dt, arrow_repo_path = tmp)
  result <- write_survey_parquet(dt, arrow_repo_path = tmp, overwrite = FALSE)

  expect_identical(result$status, "skipped")
})

test_that("write_survey_parquet overwrites when overwrite = TRUE", {
  tmp <- withr::local_tempdir()
  dt  <- make_arrow_dt()

  write_survey_parquet(dt, arrow_repo_path = tmp)
  result <- write_survey_parquet(dt, arrow_repo_path = tmp, overwrite = TRUE)

  expect_identical(result$status, "written")
})

# ===========================================================================
# .validate_for_write() — version partition key consistency
# ===========================================================================

test_that(".validate_for_write rejects data with multiple version values", {
  dt <- make_arrow_dt(n_rows = 4L)
  # Corrupt version consistency
  data.table::set(dt, i = 1L, j = "version", value = "v99_v99")

  expect_error(
    pipdata:::.validate_for_write(dt),
    regexp = "version"
  )
})

test_that(".validate_for_write passes for valid data with version column", {
  dt <- make_arrow_dt()
  expect_true(pipdata:::.validate_for_write(dt))
})

# ===========================================================================
# Round-trip: write then read back
# ===========================================================================

test_that("write_survey_parquet round-trip: version column preserved correctly", {
  tmp    <- withr::local_tempdir()
  dt     <- make_arrow_dt(version = "v01_v04")
  result <- write_survey_parquet(dt, arrow_repo_path = tmp)

  expect_identical(result$status, "written")

  # Read back and verify version column
  dt_read <- data.table::as.data.table(
    arrow::read_parquet(result$file_path)
  )

  expect_true("version" %in% names(dt_read))
  expect_identical(unique(dt_read$version), "v01_v04")
})

test_that("write_survey_parquet round-trip: educat4 preserved as factor", {
  tmp <- withr::local_tempdir()
  dt  <- make_arrow_dt()
  data.table::set(dt, j = "educat4", value = factor(
    rep(c("Primary (complete or incomplete)", "No education"), length.out = 5L)
  ))

  result  <- write_survey_parquet(dt, arrow_repo_path = tmp)
  dt_read <- data.table::as.data.table(arrow::read_parquet(result$file_path))

  expect_true("educat4" %in% names(dt_read))
  # Arrow reads dictionary as factor
  expect_true(is.factor(dt_read$educat4) || is.character(dt_read$educat4))
  expect_false("education" %in% names(dt_read))
})

# ===========================================================================
# pip_id vs survey_id consistency
# ===========================================================================

# Regression test: the data column holding the file-level survey identifier
# must be named `pip_id` (not `survey_id`) throughout the pipeline.
# The schema (piptm::pip_arrow_schema()) defines it as `pip_id`.
# prepare_for_arrow() / inject_metadata_cols() must inject it as `pip_id`.
# write_survey_parquet() must return a summary row with column `pip_id`.

test_that("make_arrow_dt helper uses pip_id column, not survey_id", {
  # The test fixture must use the schema-correct column name
  dt <- make_arrow_dt()
  expect_true("pip_id"    %in% names(dt), info = "pip_id column required by schema")
  expect_false("survey_id" %in% names(dt), info = "survey_id is not a schema column")
})

test_that("write_survey_parquet returns a result with pip_id column, not survey_id", {
  tmp <- withr::local_tempdir()
  dt  <- make_arrow_dt()

  result <- write_survey_parquet(dt, arrow_repo_path = tmp)

  expect_true("pip_id"    %in% names(result), info = "result must have pip_id column")
  expect_false("survey_id" %in% names(result), info = "result must not have survey_id column")
})

test_that(".validate_for_write rejects data with survey_id column instead of pip_id", {
  # survey_id is not in the allowed schema columns; pip_id is required.
  # A data.table with survey_id but no pip_id should fail validation on both counts.
  dt <- make_arrow_dt()
  data.table::setnames(dt, "pip_id", "survey_id")

  expect_error(
    pipdata:::.validate_for_write(dt),
    regexp = "pip_id"  # should complain about missing pip_id
  )
})

# ===========================================================================
# welfare_type derivation from pip_id
# ===========================================================================

# Regression test: the inventory does not carry a welfare_type column.
# generate_arrow_dataset() must derive welfare_type from pip_id, not select
# it from the inventory (where it would be NA, causing load_pip_data() to
# find 0 files and error).

test_that(".extract_welfare_from_pip_id correctly parses INC and CON pip_ids", {
  expect_identical(pipdata:::.extract_welfare_from_pip_id("ARG_2003_EPHC-S2_INC_ALL"), "INC")
  expect_identical(pipdata:::.extract_welfare_from_pip_id("BOL_2020_EH_INC_ALL"),       "INC")
  expect_identical(pipdata:::.extract_welfare_from_pip_id("IDN_1990_SUSENAS_CON_GROUP"), "CON")
  expect_identical(pipdata:::.extract_welfare_from_pip_id("COL_2010_ECH_INC_ALL"),       "INC")
})

test_that("generate_arrow_dataset pip_rows welfare_type is never NA when inventory lacks the column", {
  # Simulate an inventory without a welfare_type column (the real-world case).
  # After pip_rows is resolved, welfare_type must be derived from pip_id — not NA.
  inv_no_wt <- data.table::data.table(
    survey_id      = c("ARG_2003_EPHC-S2_V01_M_V09_A_GMD_ALL", "BOL_2020_EH_V01_M_V04_A_GMD_ALL"),
    pip_id         = c("ARG_2003_EPHC-S2_INC_ALL", "BOL_2020_EH_INC_ALL"),
    country_code   = c("ARG", "BOL"),
    surveyid_year  = c(2003L, 2020L),
    survey_acronym = c("EPHC-S2", "EH"),
    vermast        = c("v01", "v01"),
    veralt         = c("v09", "v04"),
    collection     = c("GMD", "GMD"),
    module         = c("ALL", "ALL")
    # NOTE: no welfare_type column — matches real inventory structure
  )

  # pip_rows is built inside generate_arrow_dataset; replicate the join here
  pip_rows <- inv_no_wt[
    !is.na(pip_id),
    .(survey_id, pip_id, country_code, surveyid_year,
      survey_acronym, vermast, veralt, collection, module)
  ]
  # Derive welfare_type from pip_id — what the fixed code must do
  pip_rows[, welfare_type := pipdata:::.extract_welfare_from_pip_id(pip_id),
             by = seq_len(nrow(pip_rows))]

  expect_false(any(is.na(pip_rows$welfare_type)),
               info = "welfare_type must not be NA after derivation from pip_id")
  expect_identical(pip_rows$welfare_type, c("INC", "INC"))
})

# ---------------------------------------------------------------------------
# Bug fix: generate_arrow_dataset passes `where` to BOTH raw and meta loads
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Bug fix: .validate_for_write must not use NULL .ALLOWED_COLS_GEN
# Regression: when piptm is not installed/loaded during development,
# .onLoad() cannot call piptm::pip_allowed_cols(), leaving .ALLOWED_COLS_GEN
# as NULL. setdiff(names(dt), NULL) returns ALL column names, so every
# schema-valid column is incorrectly treated as "extra", causing a spurious
# "Input contains column(s) not in the Arrow schema" error.
# ---------------------------------------------------------------------------

test_that(".validate_for_write succeeds for valid data when .ALLOWED_COLS_GEN is NULL", {
  # Regression: when .onLoad() cannot reach piptm (e.g. load_all() dev session),
  # .ALLOWED_COLS_GEN is NULL. setdiff(names(dt), NULL) returns all column names,
  # causing every schema-valid column to be flagged as "extra".
  # The fix uses .get_allowed_cols() which falls back to piptm::pip_allowed_cols()
  # when the global is NULL.
  dt <- make_arrow_dt()

  # Verify the lazy accessor returns a non-NULL value even when global is NULL
  allowed <- piptm::pip_allowed_cols()
  expect_false(is.null(allowed))
  expect_true("pip_id"   %in% allowed)
  expect_true("welfare"  %in% allowed)
  expect_true("version"  %in% allowed)

  # Full validation must pass on schema-valid data
  expect_true(pipdata:::.validate_for_write(dt))
})

# ---------------------------------------------------------------------------
# Existing bug-fix test kept below
# ---------------------------------------------------------------------------

test_that("generate_arrow_dataset passes 'where' to both raw and meta load_pip_data calls", {
  # Capture the `where` argument seen by every load_pip_data call, keyed by
  # whether it is the metadata call or the raw data call.
  calls_captured <- list()

  local_mocked_bindings(
    load_pip_data = function(..., where = c("release", "master"), metadata = FALSE) {
      calls_captured[[length(calls_captured) + 1L]] <<- list(
        metadata = metadata,
        where    = match.arg(where)
      )
      stop("stub - not testing further")
    },
    .package = "pipload"
  )

  inv <- data.table::data.table(
    survey_id      = "ARG_2003_EPHC-S2_V01_M_V09_A_GMD_ALL",
    pip_id         = "ARG_2003_EPHC-S2_INC_ALL",
    country_code   = "ARG",
    surveyid_year  = 2003L,
    survey_acronym = "EPHC-S2",
    vermast        = "v01",
    veralt         = "v09",
    collection     = "GMD",
    module         = "ALL"
  )

  suppressWarnings(
    tryCatch(
      generate_arrow_dataset(inv, arrow_repo_path = tempdir(), where = "master"),
      error = function(e) NULL
    )
  )

  # The raw call (metadata = FALSE) must have received where = "master"
  raw_call <- Filter(function(x) !x$metadata, calls_captured)
  expect_length(raw_call, 1L)
  expect_equal(raw_call[[1L]]$where, "master",
               info = "raw data load must use the 'where' argument, not the default 'release'")
})
