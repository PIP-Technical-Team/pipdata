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
                          survey_id     = "COL_2010_ECH_V01_M_V02_A_INC_ALL",
                          version       = "v01_v02",
                          n_rows        = 5L) {
  data.table::data.table(
    country_code   = country_code,
    surveyid_year  = as.integer(surveyid_year),
    welfare_type   = welfare_type,
    version        = version,
    survey_id      = survey_id,
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

  expect_true(grepl("country=COL", result))
  expect_true(grepl("year=2010",   result))
  expect_true(grepl("welfare=INC", result))
  expect_true(grepl("version=v01_v02", result))
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
  expect_match(parts[length(parts) - 1], "^welfare=")
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

  dt_v1 <- make_arrow_dt(version = "v01_v02", survey_id = "COL_2010_ECH_V01_M_V02_A_INC_ALL")
  dt_v2 <- make_arrow_dt(version = "v01_v03", survey_id = "COL_2010_ECH_V01_M_V03_A_INC_ALL")

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
