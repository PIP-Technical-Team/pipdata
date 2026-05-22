# Tests for build_pip_inventory.R
#
# Covers all scenarios from the Phase 2 plan (Step 3):
#   Happy path, second-run merge, column collision avoidance, catalog missing-
#   meta exclusion, empty inv_to_clean, non-standard pip_id warning, duplicate
#   survey_id assertion, empty-catalog aborts, and duplicate-pip_id abort.
#
# External calls mocked via local_mocked_bindings():
#   stamp::st_catalog_query, pipload::load_pip_master_inventory,
#   pipload::load_aux_data, pipload::pip_write,
#   pipfun::log_add, pipfun::log_info, pipfun::log_error, stamp::st_latest

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Returns a catalog data.table matching the schema from st_catalog_query().
# path is constructed as "/<pip_id>.qs2" so pip_id derivation works.
make_catalog <- function(pip_ids = character(0)) {
  if (length(pip_ids) == 0L) {
    return(data.table::data.table(
      path = character(),
      version_id = character(),
      content_hash = character(),
      code_hash = character(),
      size_bytes = numeric(),
      created_at = character()
    ))
  }
  data.table::data.table(
    path = paste0("/fake/", tolower(pip_ids), ".qs2"),
    version_id = paste0("vid_", pip_ids),
    content_hash = paste0("hash_", pip_ids),
    code_hash = paste0("code_", pip_ids),
    size_bytes = rep(1000, length(pip_ids)),
    created_at = rep("2026-01-01T00:00:00", length(pip_ids))
  )
}

# Returns a minimal inv_to_clean data.table for given survey_ids.
make_inv_to_clean <- function(
  survey_ids,
  country_codes = NULL,
  surveyid_years = NULL,
  survey_acronyms = NULL
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
      2020L
    } else {
      surveyid_years
    },
    survey_acronym = if (is.null(survey_acronyms)) {
      "HBS"
    } else {
      survey_acronyms
    },
    pipeline_version = paste0("v", seq_len(n)),
    latest_version_id = paste0("dlw_vid_", seq_len(n)),
    content_hash = paste0("dlw_hash_", seq_len(n)),
    Checksum = paste0("chk_", seq_len(n)),
    file_path = paste0("/dlw/", survey_ids, ".dta")
  )
}

# Returns a pip_id_map data.table.
make_pip_id_map <- function(survey_ids, pip_ids_list) {
  # pip_ids_list: named list(survey_id = c(pip_id1, ...), ...)
  data.table::rbindlist(
    lapply(
      stats::setNames(pip_ids_list, survey_ids),
      \(pids) data.table::data.table(pip_id = pids)
    ),
    idcol = "survey_id"
  )
}

# Returns a minimal mock PFW data.table.
make_pfw <- function(
  country_codes,
  surveyid_years,
  survey_acronyms,
  inpovcal = 1L
) {
  data.table::data.table(
    country_code = country_codes,
    surveyid_year = surveyid_years,
    survey_acronym = survey_acronyms,
    inpovcal = inpovcal
  )
}

# Null-return mock for logging functions.
null_log <- function(...) invisible(NULL)

# ---------------------------------------------------------------------------
# Happy path: 3 pip_ids across 2 surveys (one dual-pip_id survey)
# ---------------------------------------------------------------------------

test_that("build_pip_inventory happy path returns correct structure", {
  pip_ids <- c(
    "BOL_2022_EH_INC_ALL",
    "BOL_2022_EH_INC_GPWG",
    "PRY_2020_EPH_INC_ALL"
  )
  survey_ids <- c("BOL_2022_EH", "PRY_2020_EPH")

  inv_to_clean <- make_inv_to_clean(
    survey_ids,
    country_codes = c("BOL", "PRY"),
    surveyid_years = c(2022L, 2020L),
    survey_acronyms = c("EH", "EPH")
  )
  pip_id_map <- make_pip_id_map(
    survey_ids,
    list(
      c("BOL_2022_EH_INC_ALL", "BOL_2022_EH_INC_GPWG"),
      c("PRY_2020_EPH_INC_ALL")
    )
  )
  mock_pfw <- make_pfw(
    country_codes = c("BOL", "PRY"),
    surveyid_years = c(2022L, 2020L),
    survey_acronyms = c("EH", "EPH"),
    inpovcal = 1L
  )

  cat_returns <- list()
  write_counter <- 0L

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) {
      if (identical(alias, "pip")) {
        make_catalog(pip_ids)
      } else if (identical(alias, "pip_meta")) {
        make_catalog(pip_ids)
      } else {
        make_catalog()
      }
    },
    .package = "stamp"
  )
  local_mocked_bindings(
    st_latest = function(id, alias = NULL) "release_vid_001",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) mock_pfw,
    pip_write = function(x, id, alias, pk = NULL) {
      list(version_id = paste0(id, "_vid"), skipped = FALSE)
    },
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  result <- build_pip_inventory(inv_to_clean, pip_id_map)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3L)
  expect_true("version_id_data" %in% names(result))
  expect_true("version_id_metadata" %in% names(result))
  expect_true("welfare_type" %in% names(result))
  expect_true("survey_id" %in% names(result))
  expect_true("path_dlw" %in% names(result))
  # DLW source columns must be renamed (not clash with catalog hash)
  expect_false("content_hash" %in% names(result))
  expect_true("content_hash_data" %in% names(result))
  expect_true("content_hash_dlw" %in% names(result))
  # welfare_type correctly extracted
  expect_equal(
    sort(unique(result$welfare_type)),
    sort(c("INC"))
  )
})

# ---------------------------------------------------------------------------
# Second run: old master surveys retained, new surveys added
# ---------------------------------------------------------------------------

test_that("build_pip_inventory retains old master surveys not reprocessed", {
  new_pip_id <- "BRA_2019_PNAD_INC_ALL"
  new_survey <- "BRA_2019_PNAD"
  old_pip_id <- "CHN_2018_HIES_INC_ALL"
  old_survey <- "CHN_2018_HIES"

  inv_to_clean <- make_inv_to_clean(
    new_survey,
    country_codes = "BRA",
    surveyid_years = 2019L,
    survey_acronyms = "PNAD"
  )
  pip_id_map <- make_pip_id_map(new_survey, list(c(new_pip_id)))

  mock_pfw <- make_pfw("BRA", 2019L, "PNAD")

  old_master <- data.table::data.table(
    survey_id = old_survey,
    pip_id = old_pip_id,
    version_id_data = "old_vid_data",
    version_id_metadata = "old_vid_meta",
    welfare_type = "INC",
    country_code = "CHN",
    surveyid_year = 2018L,
    survey_acronym = "HIES",
    first_release_version_id = NA_character_,
    latest_release_version_id = NA_character_
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(new_pip_id),
    .package = "stamp"
  )
  local_mocked_bindings(
    st_latest = function(id, alias = NULL) "release_vid_002",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) old_master,
    load_aux_data = function(measure, ...) mock_pfw,
    pip_write = function(x, id, alias, pk = NULL) {
      list(version_id = paste0(id, "_vid"), skipped = FALSE)
    },
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  result <- build_pip_inventory(inv_to_clean, pip_id_map)

  expect_equal(nrow(result), 2L)
  expect_true(old_pip_id %in% result$pip_id)
  expect_true(new_pip_id %in% result$pip_id)
})

# ---------------------------------------------------------------------------
# Column collision: DLW columns renamed before join
# ---------------------------------------------------------------------------

test_that("build_pip_inventory renames DLW columns before join to avoid collisions", {
  pip_id <- "IND_2021_NSS_INC_ALL"
  survey_id <- "IND_2021_NSS"

  inv_to_clean <- make_inv_to_clean(
    survey_id,
    country_codes = "IND",
    surveyid_years = 2021L,
    survey_acronyms = "NSS"
  )
  pip_id_map <- make_pip_id_map(survey_id, list(pip_id))

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(pip_id),
    .package = "stamp"
  )
  local_mocked_bindings(
    st_latest = function(id, alias = NULL) "vid_xyz",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) make_pfw("IND", 2021L, "NSS"),
    pip_write = function(x, id, alias, pk = NULL) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  result <- build_pip_inventory(inv_to_clean, pip_id_map)

  # DLW content_hash must be in _dlw column, not bare content_hash
  col_names <- names(result)
  expect_true("content_hash_dlw" %in% col_names)
  expect_true("path_dlw" %in% col_names)
  expect_true("Checksum_dlw" %in% col_names)
  # No bare collision columns
  expect_false("content_hash" %in% col_names)
  expect_false("file_path" %in% col_names)
})

# ---------------------------------------------------------------------------
# Edge case: pip_id present in data catalog but absent from meta catalog
# → excluded by inner join (nomatch = 0)
# ---------------------------------------------------------------------------

test_that("build_pip_inventory excludes pip_id missing from meta catalog", {
  both_pip_id <- "ARG_2020_EPH_INC_ALL"
  only_data_pid <- "ARG_2019_EPH_INC_ALL"
  survey_id <- "ARG_2020_EPH"

  inv_to_clean <- make_inv_to_clean(
    survey_id,
    country_codes = "ARG",
    surveyid_years = 2020L,
    survey_acronyms = "EPH"
  )
  pip_id_map <- make_pip_id_map(survey_id, list(c(both_pip_id, only_data_pid)))

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) {
      if (identical(alias, "pip")) {
        make_catalog(c(both_pip_id, only_data_pid))
      } else if (identical(alias, "pip_meta")) {
        make_catalog(both_pip_id)
      } else {
        make_catalog()
      }
    },
    .package = "stamp"
  )
  local_mocked_bindings(
    st_latest = function(...) "vid_arg",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) make_pfw("ARG", 2020L, "EPH"),
    pip_write = function(x, id, alias, pk = NULL) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  result <- build_pip_inventory(inv_to_clean, pip_id_map)

  # Only both_pip_id survives the inner join
  expect_equal(nrow(result), 1L)
  expect_equal(result$pip_id, both_pip_id)
  expect_false(only_data_pid %in% result$pip_id)
})

# ---------------------------------------------------------------------------
# Edge case: empty inv_to_clean → build_pip_inventory sees no surveys to run
# (run_inv will be empty after the survey_id filter); old master is returned
# ---------------------------------------------------------------------------

test_that("build_pip_inventory with empty inv_to_clean returns old master", {
  old_pip_id <- "MEX_2018_ENIGH_INC_ALL"
  old_master <- data.table::data.table(
    survey_id = "MEX_2018_ENIGH",
    pip_id = old_pip_id,
    version_id_data = "v_data",
    version_id_metadata = "v_meta",
    welfare_type = "INC",
    country_code = "MEX",
    surveyid_year = 2018L,
    survey_acronym = "ENIGH",
    first_release_version_id = NA_character_,
    latest_release_version_id = NA_character_
  )

  inv_to_clean <- make_inv_to_clean(character(0))[FALSE, ]
  pip_id_map <- data.table::data.table(
    survey_id = character(),
    pip_id = character()
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) {
      make_catalog("MEX_2018_ENIGH_INC_ALL")
    },
    .package = "stamp"
  )
  local_mocked_bindings(
    st_latest = function(...) "vid_mex",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) old_master,
    load_aux_data = function(measure, ...) {
      make_pfw("MEX", 2018L, "ENIGH")
    },
    pip_write = function(x, id, alias, pk = NULL) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  result <- build_pip_inventory(inv_to_clean, pip_id_map)

  # With empty inp, the run_inv is empty; the only rows come from old_master
  expect_true(old_pip_id %in% result$pip_id)
})

# ---------------------------------------------------------------------------
# Edge case: non-standard pip_id format in catalog → warning + excluded
# ---------------------------------------------------------------------------

test_that("build_pip_inventory warns and excludes non-standard pip_id artifacts", {
  good_pip_id <- "TZA_2014_HBS_INC_ALL"
  bad_pip_id <- "not_a_valid_pip_id"
  survey_id <- "TZA_2014_HBS"

  inv_to_clean <- make_inv_to_clean(
    survey_id,
    country_codes = "TZA",
    surveyid_years = 2014L,
    survey_acronyms = "HBS"
  )
  pip_id_map <- make_pip_id_map(survey_id, list(good_pip_id))

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) {
      make_catalog(c(good_pip_id, bad_pip_id))
    },
    .package = "stamp"
  )
  local_mocked_bindings(
    st_latest = function(...) "vid_tza",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) make_pfw("TZA", 2014L, "HBS"),
    pip_write = function(x, id, alias, pk = NULL) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  # Warning should be emitted about non-standard pip_id
  expect_warning(
    result <- build_pip_inventory(inv_to_clean, pip_id_map),
    regexp = "non-standard pip_id"
  )
  expect_false(bad_pip_id %in% result$pip_id)
})

# ---------------------------------------------------------------------------
# Edge case: duplicate survey_id in inv_to_clean → stopifnot fires
# ---------------------------------------------------------------------------

test_that("build_pip_inventory aborts when inv_to_clean has duplicate survey_id", {
  pip_id <- "KEN_2015_KIHBS_INC_ALL"
  survey_id <- "KEN_2015_KIHBS"

  # Duplicate row
  inv_to_clean <- data.table::rbindlist(list(
    make_inv_to_clean(
      survey_id,
      country_codes = "KEN",
      surveyid_years = 2015L,
      survey_acronyms = "KIHBS"
    ),
    make_inv_to_clean(
      survey_id,
      country_codes = "KEN",
      surveyid_years = 2015L,
      survey_acronyms = "KIHBS"
    )
  ))

  pip_id_map <- make_pip_id_map(survey_id, list(pip_id))

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(pip_id),
    .package = "stamp"
  )

  expect_error(
    build_pip_inventory(inv_to_clean, pip_id_map),
    regexp = "anyDuplicated"
  )
})

# ---------------------------------------------------------------------------
# Error path: both catalogs empty + pip_id_map empty → "first run" abort
# ---------------------------------------------------------------------------

test_that("build_pip_inventory aborts with first-run message when catalogs empty and no pip_ids", {
  inv_to_clean <- make_inv_to_clean(
    "CHL_2017_CASEN",
    country_codes = "CHL",
    surveyid_years = 2017L,
    survey_acronyms = "CASEN"
  )
  pip_id_map <- data.table::data.table(
    survey_id = character(),
    pip_id = character()
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(),
    .package = "stamp"
  )

  expect_error(
    build_pip_inventory(inv_to_clean, pip_id_map),
    class = "build_pip_inventory_empty_first_run"
  )
})

# ---------------------------------------------------------------------------
# Error path: both catalogs empty + pip_id_map non-empty → "st_save failed"
# ---------------------------------------------------------------------------

test_that("build_pip_inventory aborts with st_save message when catalogs empty but pip_ids exist", {
  inv_to_clean <- make_inv_to_clean(
    "URY_2019_ECH",
    country_codes = "URY",
    surveyid_years = 2019L,
    survey_acronyms = "ECH"
  )
  pip_id_map <- make_pip_id_map("URY_2019_ECH", list("URY_2019_ECH_INC_ALL"))

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(),
    .package = "stamp"
  )

  expect_error(
    build_pip_inventory(inv_to_clean, pip_id_map),
    class = "build_pip_inventory_empty_catalog"
  )
})

# ---------------------------------------------------------------------------
# Error path: duplicate pip_id after merge with old master → abort
# ---------------------------------------------------------------------------

test_that("build_pip_inventory aborts when duplicate pip_id arises after merge", {
  pip_id <- "SEN_2011_ESPS_INC_ALL"
  survey_id <- "SEN_2011_ESPS"

  inv_to_clean <- make_inv_to_clean(
    survey_id,
    country_codes = "SEN",
    surveyid_years = 2011L,
    survey_acronyms = "ESPS"
  )
  pip_id_map <- make_pip_id_map(survey_id, list(pip_id))

  # Old master already has the same pip_id but a DIFFERENT survey_id
  # (should not happen in practice, but guards the uniqueness invariant)
  old_master <- data.table::data.table(
    survey_id = "SEN_2011_ESPS_DUP", # different survey_id
    pip_id = pip_id, # same pip_id → collision after merge
    version_id_data = "old_v",
    version_id_metadata = "old_m",
    welfare_type = "INC",
    country_code = "SEN",
    surveyid_year = 2011L,
    survey_acronym = "ESPS",
    first_release_version_id = NA_character_,
    latest_release_version_id = NA_character_
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(pip_id),
    .package = "stamp"
  )
  local_mocked_bindings(
    st_latest = function(...) "vid_sen",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) old_master,
    load_aux_data = function(measure, ...) make_pfw("SEN", 2011L, "ESPS"),
    pip_write = function(x, id, alias, pk = NULL) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  expect_error(
    build_pip_inventory(inv_to_clean, pip_id_map),
    class = "build_pip_inventory_dup_pip_id"
  )
})
