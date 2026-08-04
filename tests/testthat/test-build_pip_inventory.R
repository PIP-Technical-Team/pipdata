# Tests for build_pip_inventory.R
#
# Covers all scenarios from the Phase 2 plan (Step 3, delta/update strategy):
#   Happy path, second-run upsert, column collision avoidance, catalog missing-
#   meta exclusion, empty pip_id_map early return, missing-from-catalog warning,
#   bad_pip_id_format warning, duplicate survey_id assertion, empty-catalog aborts,
#   and duplicate-pip_id abort.
#
# External calls mocked via local_mocked_bindings():
#   stamp::st_catalog_query, stamp::st_latest,
#   pipload::load_pip_master_inventory, pipload::load_aux_data,
#   pipload::pip_write, pipfun::log_add, pipfun::log_info, pipfun::log_error

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
  if (n == 0L) {
    return(data.table::data.table(
      survey_id = character(),
      country_code = character(),
      surveyid_year = integer(),
      survey_acronym = character(),
      pipeline_version = character(),
      latest_version_id = character(),
      content_hash = character(),
      Checksum = character(),
      file_path = character()
    ))
  }
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
    pipeline_version = paste0("v", seq_len(n)),
    latest_version_id = paste0("dlw_vid_", seq_len(n)),
    content_hash = paste0("dlw_hash_", seq_len(n)),
    Checksum = paste0("chk_", seq_len(n)),
    file_path = paste0("/dlw/", survey_ids, ".dta")
  )
}

# Returns a pip_id_map data.table.
make_pip_id_map <- function(survey_ids, pip_ids_list) {
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

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(pip_ids),
    st_latest = function(id, alias = NULL) "release_vid_001",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) mock_pfw,
    pip_write = function(x, id, alias, pk = NULL, ...) {
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
  expect_equal(sort(unique(result$welfare_type)), "INC")
})

# ---------------------------------------------------------------------------
# Second run: old master surveys retained via upsert (not reprocessed)
# ---------------------------------------------------------------------------

test_that("build_pip_inventory upserts: retains old surveys, updates reprocessed", {
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
    st_latest = function(id, alias = NULL) "release_vid_002",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) old_master,
    load_aux_data = function(measure, ...) make_pfw("BRA", 2019L, "PNAD"),
    pip_write = function(x, id, alias, pk = NULL, ...) {
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
  # Old survey's version_id_data must be unchanged (not replaced by catalog)
  expect_equal(
    result[result$pip_id == old_pip_id, version_id_data],
    "old_vid_data"
  )
  # New survey (BRA, in PFW) gets release version columns populated.
  # pip_write for release returns version_id = "pip_release_inventory_vid".
  expect_equal(
    result[result$pip_id == new_pip_id, first_release_version_id],
    "pip_release_inventory_vid"
  )
  expect_equal(
    result[result$pip_id == new_pip_id, latest_release_version_id],
    "pip_release_inventory_vid"
  )
  # Old survey (CHN, not in this run's PFW mock) retains NA release columns.
  expect_true(
    is.na(result[result$pip_id == old_pip_id, first_release_version_id])
  )
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
    st_latest = function(id, alias = NULL) "vid_xyz",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) make_pfw("IND", 2021L, "NSS"),
    pip_write = function(x, id, alias, pk = NULL, ...) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  result <- build_pip_inventory(inv_to_clean, pip_id_map)

  col_names <- names(result)
  expect_true("content_hash_dlw" %in% col_names)
  expect_true("path_dlw" %in% col_names)
  expect_true("Checksum_dlw" %in% col_names)
  expect_false("content_hash" %in% col_names)
  expect_false("file_path" %in% col_names)
})

# ---------------------------------------------------------------------------
# Edge case: pip_id in pip_id_map absent from meta catalog
# → warns (missing_from_catalog) + excluded by inner join
# ---------------------------------------------------------------------------

test_that("build_pip_inventory warns and excludes pip_id missing from meta catalog", {
  both_pip_id <- "ARG_2020_EPH_INC_ALL"
  only_data_pid <- "ARG_2019_EPH_INC_ALL"
  survey_id <- "ARG_2020_EPH"

  inv_to_clean <- make_inv_to_clean(
    survey_id,
    country_codes = "ARG",
    surveyid_years = 2020L,
    survey_acronyms = "EPH"
  )
  pip_id_map <- make_pip_id_map(
    survey_id,
    list(c(both_pip_id, only_data_pid))
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) {
      if (identical(alias, "pip")) {
        make_catalog(c(both_pip_id, only_data_pid))
      } else {
        make_catalog(both_pip_id)
      }
    },
    st_latest = function(...) "vid_arg",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) make_pfw("ARG", 2020L, "EPH"),
    pip_write = function(x, id, alias, pk = NULL, ...) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  expect_warning(
    result <- build_pip_inventory(inv_to_clean, pip_id_map),
    class = "build_pip_inventory_missing_from_catalog"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$pip_id, both_pip_id)
})

# ---------------------------------------------------------------------------
# Edge case: empty pip_id_map + old master → returns old master unchanged
# (no catalog query performed — early return in Step 2)
# ---------------------------------------------------------------------------

test_that("build_pip_inventory returns old master unchanged when pip_id_map is empty", {
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

  inv_to_clean <- make_inv_to_clean(character(0))
  pip_id_map <- data.table::data.table(
    survey_id = character(),
    pip_id = character()
  )

  # Only load_pip_master_inventory needs mocking — catalog is never queried.
  local_mocked_bindings(
    load_pip_master_inventory = function(...) old_master,
    .package = "pipload"
  )

  result <- build_pip_inventory(inv_to_clean, pip_id_map)

  expect_equal(nrow(result), 1L)
  expect_equal(result$pip_id, old_pip_id)
  expect_equal(result$version_id_data, "v_data")
})

# ---------------------------------------------------------------------------
# Edge case: duplicate survey_id in inv_to_clean → stopifnot fires
# ---------------------------------------------------------------------------

test_that("build_pip_inventory aborts when inv_to_clean has duplicate survey_id", {
  survey_id <- "KEN_2015_KIHBS"
  pip_id <- "KEN_2015_KIHBS_INC_ALL"

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
    load_pip_master_inventory = function(...) NULL,
    .package = "pipload"
  )

  expect_error(
    build_pip_inventory(inv_to_clean, pip_id_map),
    regexp = "anyDuplicated"
  )
})

# ---------------------------------------------------------------------------
# Error path: no pip_id_map + no prior master → "first run" abort
# (no catalog query — aborts in Step 2 before querying catalogs)
# ---------------------------------------------------------------------------

test_that("build_pip_inventory aborts with first-run message when no pip_ids and no prior master", {
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
    load_pip_master_inventory = function(...) stop("no master exists"),
    .package = "pipload"
  )

  expect_error(
    build_pip_inventory(inv_to_clean, pip_id_map),
    class = "build_pip_inventory_empty_first_run"
  )
})

# ---------------------------------------------------------------------------
# Error path: catalogs empty after filtering + pip_ids present
# → "st_save failed" abort
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
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    .package = "pipload"
  )

  expect_error(
    build_pip_inventory(inv_to_clean, pip_id_map),
    class = "build_pip_inventory_empty_catalog"
  )
})

# ---------------------------------------------------------------------------
# Error path: duplicate pip_id after upsert → abort
# ---------------------------------------------------------------------------

test_that("build_pip_inventory aborts when duplicate pip_id arises after upsert", {
  # Two different survey_ids both resolve to the same pip_id — this can
  # happen when DLW has a versioned re-upload under a different survey_id.
  # new_versions will carry both rows; duplicate pip_id triggers the abort.
  pip_id <- "SEN_2011_ESPS_INC_ALL"
  survey_ids <- c("SEN_2011_ESPS", "SEN_2011_ESPS_V2")

  inv_to_clean <- make_inv_to_clean(
    survey_ids,
    country_codes = c("SEN", "SEN"),
    surveyid_years = c(2011L, 2011L),
    survey_acronyms = c("ESPS", "ESPS")
  )
  # Both survey_ids map to the same pip_id — ensures 2 rows in new_versions
  pip_id_map <- data.table::data.table(
    survey_id = survey_ids,
    pip_id = c(pip_id, pip_id)
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(pip_id),
    st_latest = function(...) "vid_sen",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) make_pfw("SEN", 2011L, "ESPS"),
    pip_write = function(x, id, alias, pk = NULL, ...) list(version_id = "v1"),
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
# ---------------------------------------------------------------------------
# Warning path: catalog artifact has a non-standard pip_id (fails 5-segment
# pattern) — fires build_pip_inventory_bad_pip_id_format and excludes it
# ---------------------------------------------------------------------------

test_that("build_pip_inventory warns and drops artifact with non-standard pip_id format", {
  good_pip_id <- "BOL_2022_EH_INC_ALL"
  bad_pip_id  <- "BAD_FORMAT"  # only 2 segments — fails 5-segment pattern
  survey_id   <- "BOL_2022_EH"

  inv_to_clean <- make_inv_to_clean(
    survey_id,
    country_codes = "BOL",
    surveyid_years = 2022L,
    survey_acronyms = "EH"
  )
  # Both pip_ids are in target_ids (i.e. pip_id_map includes the malformed
  # one — representing a save_pip artifact persisted under a bad name).
  # The format validation fires AFTER the target_ids filter, so only
  # current-run artifacts are checked.
  pip_id_map <- make_pip_id_map(
    survey_id,
    list(c(good_pip_id, bad_pip_id))
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(c(good_pip_id, bad_pip_id)),
    st_latest = function(...) "vid_bol",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) make_pfw("BOL", 2022L, "EH"),
    pip_write = function(x, id, alias, pk = NULL, ...) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  # The format warning fires first; a secondary missing_from_catalog warning
  # follows because bad_pip_id is in target_ids but absent from the filtered
  # catalogs after being dropped. suppressWarnings() silences the secondary one.
  suppressWarnings(
    expect_warning(
      result <- build_pip_inventory(inv_to_clean, pip_id_map),
      class = "build_pip_inventory_bad_pip_id_format"
    )
  )
  # Only the valid artifact survives into the inventory
  expect_equal(nrow(result), 1L)
  expect_equal(result$pip_id, good_pip_id)
})

# ---------------------------------------------------------------------------
# Release step: pip_write returns skipped=TRUE → st_latest fallback
# ---------------------------------------------------------------------------

test_that("build_pip_inventory uses st_latest for release_vid when pip_write returns skipped", {
  pip_id   <- "TZA_2019_HBS_INC_ALL"
  survey   <- "TZA_2019_HBS"

  inv_to_clean <- make_inv_to_clean(
    survey,
    country_codes  = "TZA",
    surveyid_years = 2019L,
    survey_acronyms = "HBS"
  )
  pip_id_map <- make_pip_id_map(survey, list(pip_id))

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(pip_id),
    # st_latest is the fallback when pip_write returns skipped = TRUE
    st_latest = function(id, alias = NULL) "from_latest_vid",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) NULL,
    load_aux_data = function(measure, ...) make_pfw("TZA", 2019L, "HBS"),
    pip_write = function(x, id, alias, pk = NULL, ...) {
      if (identical(id, "pip_release_inventory")) {
        # Simulate unchanged content → pip_write skips the write
        list(version_id = NULL, skipped = TRUE)
      } else {
        # Master inventory write succeeds normally
        list(version_id = paste0(id, "_vid"), skipped = FALSE)
      }
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

  # When pip_write returns skipped=TRUE, st_latest() supplies the version id
  expect_equal(result[result$pip_id == pip_id, first_release_version_id], "from_latest_vid")
  expect_equal(result[result$pip_id == pip_id, latest_release_version_id], "from_latest_vid")
})

# ---------------------------------------------------------------------------
# Regression: reporting_level persisted in old master is stripped by Step 1
# ---------------------------------------------------------------------------

test_that("build_pip_inventory strips reporting_level legacy column from old master", {
  new_pip_id <- "NGA_2019_GHS_INC_ALL"
  new_survey  <- "NGA_2019_GHS"
  old_pip_id  <- "NGA_2018_GHS_INC_ALL"

  inv_to_clean <- make_inv_to_clean(
    new_survey,
    country_codes  = "NGA",
    surveyid_years = 2019L,
    survey_acronyms = "GHS"
  )
  pip_id_map <- make_pip_id_map(new_survey, list(new_pip_id))

  # Old master written by update_pip_inventory() carried reporting_level
  old_master <- data.table::data.table(
    survey_id              = "NGA_2018_GHS",
    pip_id                 = old_pip_id,
    version_id_data        = "old_v",
    version_id_metadata    = "old_m",
    welfare_type           = "INC",
    reporting_level        = "national",   # legacy column to be stripped
    country_code           = "NGA",
    surveyid_year          = 2018L,
    survey_acronym         = "GHS",
    first_release_version_id  = NA_character_,
    latest_release_version_id = NA_character_
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(new_pip_id),
    st_latest = function(...) "vid_nga",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) old_master,
    load_aux_data = function(measure, ...) make_pfw("NGA", 2019L, "GHS"),
    pip_write = function(x, id, alias, pk = NULL, ...) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  result <- build_pip_inventory(inv_to_clean, pip_id_map)

  expect_false("reporting_level" %in% names(result))
  expect_equal(nrow(result), 2L)
  expect_true(old_pip_id %in% result$pip_id)
})

# ---------------------------------------------------------------------------
# Regression: old master column type drift causes collapse::rowbind abort
# (size_bytes_* stored as fs::fs_bytes in old master vs plain numeric in
# new_versions from st_catalog_query)
# ---------------------------------------------------------------------------

test_that("build_pip_inventory rowbinds correctly when old master has fs_bytes size columns", {
  new_pip_id <- "ECU_2021_ENEMDU_INC_ALL"
  new_survey <- "ECU_2021_ENEMDU"
  old_pip_id <- "ECU_2018_ENEMDU_INC_ALL"

  inv_to_clean <- make_inv_to_clean(
    new_survey,
    country_codes = "ECU",
    surveyid_years = 2021L,
    survey_acronyms = "ENEMDU"
  )
  pip_id_map <- make_pip_id_map(new_survey, list(new_pip_id))

  # Old master has size_bytes_data/size_bytes_metadata as fs::fs_bytes class
  # (as old update_pip_inventory() persisted them via the fs package).
  # new_versions has them as plain numeric from st_catalog_query().
  # collapse::rowbind aborts on class attribute mismatch.
  size_val <- structure(1000, class = c("fs_bytes", "numeric"))
  old_master <- data.table::data.table(
    survey_id = "ECU_2018_ENEMDU",
    pip_id = old_pip_id,
    version_id_data = "old_v",
    size_bytes_data = size_val, # fs_bytes, not plain numeric
    version_id_metadata = "old_m",
    size_bytes_metadata = size_val, # fs_bytes, not plain numeric
    welfare_type = "INC",
    country_code = "ECU",
    surveyid_year = 2018L,
    survey_acronym = "ENEMDU",
    first_release_version_id = NA_character_,
    latest_release_version_id = NA_character_
  )

  local_mocked_bindings(
    st_catalog_query = function(alias = NULL) make_catalog(new_pip_id),
    st_latest = function(...) "vid_ecu",
    .package = "stamp"
  )
  local_mocked_bindings(
    load_pip_master_inventory = function(...) old_master,
    load_aux_data = function(measure, ...) make_pfw("ECU", 2021L, "ENEMDU"),
    pip_write = function(x, id, alias, pk = NULL, ...) list(version_id = "v1"),
    .package = "pipload"
  )
  local_mocked_bindings(
    log_add = null_log,
    log_info = null_log,
    log_error = null_log,
    .package = "pipfun"
  )

  # Should succeed (not abort with class mismatch) and contain both rows.
  result <- build_pip_inventory(inv_to_clean, pip_id_map)
  expect_equal(nrow(result), 2L)
  expect_true(old_pip_id %in% result$pip_id)
  expect_true(new_pip_id %in% result$pip_id)
})