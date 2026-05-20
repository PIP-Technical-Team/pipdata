# Tests for prepare_for_arrow() — arrow_prep.R
# Plan: .cg-docs/plans/2026-05-20-drop-nonfinite-welfare-cols.md
#
# Focused on the non-finite welfare column drop logic (§4 of prepare_for_arrow):
#   - welfare column with all NA  → dropped, warning, welfare_vars updated
#   - welfare column with all Inf → dropped, warning
#   - all welfare columns valid   → no drop, no warning
#   - all welfare columns bad     → hard abort

library(data.table)

# ---------------------------------------------------------------------------
# Helper: minimal deflated-style data.table accepted by prepare_for_arrow()
# ---------------------------------------------------------------------------

#' Build a minimal mock deflated dt for prepare_for_arrow() tests
#'
#' @param welfare_lcu        numeric vector for welfare_lcu column
#' @param welfare_ppp_2017   numeric vector for welfare_ppp_2017_01_02 column
#' @param welfare_ppp_2005   numeric vector for welfare_ppp_2005_01_01 column
#'   (NULL means column is excluded from both data and welfare_vars)
#' @param n_rows             number of rows (ignored when vectors are supplied)
make_deflated_dt <- function(welfare_lcu      = c(100, 200, 300),
                             welfare_ppp_2017 = c(1.0, 2.0, 3.0),
                             welfare_ppp_2005 = NULL,
                             n_rows           = 3L) {
  wv <- c("welfare_lcu", "welfare_ppp_2017_01_02")
  dt <- data.table::data.table(
    welfare_lcu           = as.double(welfare_lcu),
    welfare_ppp_2017_01_02 = as.double(welfare_ppp_2017),
    weight                = rep(1.0, length(welfare_lcu))
  )
  if (!is.null(welfare_ppp_2005)) {
    dt[, welfare_ppp_2005_01_01 := as.double(welfare_ppp_2005)]
    wv <- c(wv, "welfare_ppp_2005_01_01")
  }
  # Required attributes read by inject_metadata_cols()
  data.table::setattr(dt, "country_code",   "KAZ")
  data.table::setattr(dt, "surveyid_year",  2006L)
  data.table::setattr(dt, "welfare_type",   "CONSUMPTION")
  data.table::setattr(dt, "vermast",        "V01")
  data.table::setattr(dt, "veralt",         "V02")
  data.table::setattr(dt, "welfare_vars",   wv)
  data.table::setattr(dt, "ppp_sort",       2017L)
  dt
}

# ===========================================================================
# Non-finite welfare column drop logic
# ===========================================================================

test_that("welfare column that is all-NA is dropped with a warning", {
  dt <- make_deflated_dt(
    welfare_ppp_2005 = c(NA_real_, NA_real_, NA_real_)
  )
  expect_warning(
    result <- pipdata::prepare_for_arrow(dt, pip_id = "KAZ_2006_HBS_CON_ALL"),
    regexp = "welfare_ppp_2005_01_01"
  )
  expect_false("welfare_ppp_2005_01_01" %in% names(result))
  expect_false("welfare_ppp_2005_01_01" %in% attr(result, "welfare_vars"))
})

test_that("warning message names the survey pip_id", {
  dt <- make_deflated_dt(welfare_ppp_2005 = rep(NA_real_, 3))
  expect_warning(
    pipdata::prepare_for_arrow(dt, pip_id = "KAZ_2006_HBS_CON_ALL"),
    regexp = "KAZ_2006_HBS_CON_ALL"
  )
})

test_that("welfare column that is all-Inf is dropped with a warning", {
  dt <- make_deflated_dt(
    welfare_ppp_2005 = c(Inf, Inf, Inf)
  )
  expect_warning(
    result <- pipdata::prepare_for_arrow(dt, pip_id = "KAZ_2006_HBS_CON_ALL"),
    regexp = "welfare_ppp_2005_01_01"
  )
  expect_false("welfare_ppp_2005_01_01" %in% names(result))
})

test_that("welfare column that is all-NaN is dropped with a warning", {
  dt <- make_deflated_dt(
    welfare_ppp_2005 = c(NaN, NaN, NaN)
  )
  expect_warning(
    result <- pipdata::prepare_for_arrow(dt, pip_id = "KAZ_2006_HBS_CON_ALL"),
    regexp = "welfare_ppp_2005_01_01"
  )
  expect_false("welfare_ppp_2005_01_01" %in% names(result))
})

test_that("surviving welfare columns are kept and welfare_vars attr is updated", {
  dt <- make_deflated_dt(
    welfare_ppp_2005 = rep(NA_real_, 3)
  )
  expect_warning(
    result <- pipdata::prepare_for_arrow(dt, pip_id = "KAZ_2006_HBS_CON_ALL")
  )
  wv_out <- attr(result, "welfare_vars")
  expect_true("welfare_lcu"            %in% wv_out)
  expect_true("welfare_ppp_2017_01_02" %in% wv_out)
  expect_equal(length(wv_out), 2L)
})

test_that("no warning and no drop when all welfare columns have finite values", {
  dt <- make_deflated_dt(
    welfare_ppp_2005 = c(0.5, 1.0, 1.5)
  )
  expect_no_warning(
    result <- pipdata::prepare_for_arrow(dt, pip_id = "KAZ_2006_HBS_CON_ALL")
  )
  expect_true("welfare_ppp_2005_01_01" %in% names(result))
  expect_true("welfare_ppp_2005_01_01" %in% attr(result, "welfare_vars"))
})

test_that("error when ALL welfare columns are non-finite", {
  dt <- make_deflated_dt(
    welfare_lcu      = rep(NA_real_, 3),
    welfare_ppp_2017 = rep(NA_real_, 3),
    welfare_ppp_2005 = rep(NA_real_, 3)
  )
  expect_error(
    suppressWarnings(
      pipdata::prepare_for_arrow(dt, pip_id = "KAZ_2006_HBS_CON_ALL")
    ),
    regexp = "All welfare columns were dropped"
  )
})

test_that("mixed column (some NA, some finite) is retained but fails validation", {
  # A welfare column with partial NAs is a genuine data-quality error —
  # it passes the drop guard (has finite values) but is correctly rejected
  # by validate_pre_write(), which requires ALL values to be finite.
  dt <- make_deflated_dt(
    welfare_ppp_2005 = c(NA_real_, 1.5, NA_real_)
  )
  expect_error(
    pipdata::prepare_for_arrow(dt, pip_id = "KAZ_2006_HBS_CON_ALL"),
    regexp = "non-finite values"
  )
})
