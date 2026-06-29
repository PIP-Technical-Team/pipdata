# Tests for valid_dlw_load.R helper functions
#
# Covers:
#   inv_to_process()    — joyn::anti_join must not leak .joyn column
#   valid_dlw_load()    — no duplicate survey_id in return value when a survey
#                         appears in both new-survey and aux-changed sets

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Minimal DLW inventory row suitable for last_ver_inv() and inv_to_process().
make_dlw_inv <- function(
  survey_ids,
  country_codes = NULL,
  surveyid_years = NULL,
  survey_acronyms = NULL,
  modules = NULL
) {
  n <- length(survey_ids)
  data.table::data.table(
    survey_id = survey_ids,
    country_code = if (is.null(country_codes)) {
      toupper(substr(survey_ids, 1L, 3L))
    } else {
      country_codes
    },
    surveyid_year = if (is.null(surveyid_years)) {
      rep(2020L, n)
    } else {
      surveyid_years
    },
    survey_acronym = if (is.null(survey_acronyms)) {
      rep("HBS", n)
    } else {
      survey_acronyms
    },
    module = if (is.null(modules)) rep("ALL", n) else modules,
    tool = rep("LSMS", n),
    status = rep("valid", n),
    vermast = rep("01", n),
    veralt = rep("01", n),
    pipeline_version = rep("01", n),
    latest_version_id = paste0("v_", seq_len(n)),
    content_hash = paste0("h_", seq_len(n)),
    Checksum = paste0("c_", seq_len(n)),
    file_path = paste0("/dlw/", survey_ids, ".dta")
  )
}

# ---------------------------------------------------------------------------
# inv_to_process: .joyn column must not be present in result
# ---------------------------------------------------------------------------

test_that("inv_to_process does not add .joyn column to result", {
  # A single survey not in the master inventory — anti_join should keep it.
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master contains a DIFFERENT survey so our survey survives the anti_join.
  master_no_col <- data.table::data.table(
    country_code = "BRA",
    surveyid_year = 2019L,
    survey_acronym = "PNADC"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_no_col,
    .package = "pipload"
  )

  result <- pipdata:::inv_to_process(inv)

  expect_false(
    ".joyn" %in% names(result),
    info = paste0(
      "joyn::anti_join in inv_to_process() must be called with ",
      "reportvar = FALSE to avoid the .joyn column leaking into inv_to_clean ",
      "and causing duplicate survey_id rows"
    )
  )
})

# ---------------------------------------------------------------------------
# valid_dlw_load: no duplicate survey_ids when survey is in both new + aux sets
# ---------------------------------------------------------------------------

test_that("valid_dlw_load returns no duplicate survey_ids when survey appears in new and aux-changed sets", {
  # COL 2020 GEIH: not in master (new survey) AND in aux-change set for CPI.
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master without this survey → inv_to_process keeps it in inv_svy.
  master_empty <- data.table::data.table(
    country_code = character(0),
    surveyid_year = integer(0),
    survey_acronym = character(0)
  )

  # Aux change for COL 2020 → filter_aux_inv will add the same survey to inv_aux.
  cpi_changes <- list(
    data.table::data.table(country_code = "COL", surveyid_year = 2020L)
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_empty,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      # Return a named list matching the structure expected by filter_aux_inv
      stats::setNames(list(cpi_changes), measure[[1]])
    },
    .package = "pipdata"
  )

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    force = FALSE,
    verbose = FALSE
  )

  expect_false(
    is.null(result),
    info = "valid_dlw_load should return a non-NULL result"
  )
  expect_equal(
    anyDuplicated(result$survey_id),
    0L,
    info = "valid_dlw_load must return unique survey_id values (no duplicates)"
  )
})
