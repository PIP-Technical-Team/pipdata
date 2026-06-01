# Tests for update_pip_inventory.R
#
# Covers:
#   format_vrs()  — pip_names deduplication, multi-pip_id survey reshaping,
#                   NULL metadata entries, skipped entries

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_ventry <- function(
  content_hash = "abc123",
  version_id = NULL,
  skipped = FALSE,
  reason = NULL
) {
  out <- list(
    version_id = if (is.null(version_id)) {
      paste0("ver_", content_hash)
    } else {
      version_id
    },
    metadata = list(content_hash = content_hash)
  )
  if (isTRUE(skipped)) {
    out$skipped <- TRUE
    out$reason <- if (is.null(reason)) "test reason" else reason
  }
  out
}

# A mock process_data entry for a survey with two pip_ids.
make_proc_entry_dual <- function(
  pip_ids = c("BOL_2022_EH_INC_ALL", "BOL_2022_EH_INC_GPWG"),
  hash_a = "hash_a",
  hash_b = "hash_b"
) {
  versions <- stats::setNames(
    list(make_ventry(hash_a), make_ventry(hash_b)),
    pip_ids
  )
  list(
    pip_names = pip_ids,
    versions_data = versions,
    versions_metadata = versions
  )
}

# ---------------------------------------------------------------------------
# format_vrs() — single pip_id survey
# ---------------------------------------------------------------------------

test_that("format_vrs returns one row for a single pip_id survey", {
  proc <- list(
    "BOL_2022_EH" = list(
      pip_names = "BOL_2022_EH_INC_ALL",
      versions_data = list(
        BOL_2022_EH_INC_ALL = make_ventry("d1")
      ),
      versions_metadata = list(
        BOL_2022_EH_INC_ALL = make_ventry("m1")
      )
    )
  )

  result_dt <- pipdata:::format_vrs(proc, "versions_data")

  expect_equal(nrow(result_dt), 1L)
  expect_equal(result_dt$pip_id, "BOL_2022_EH_INC_ALL")
  expect_equal(result_dt$survey_id, "BOL_2022_EH")
  expect_equal(result_dt$version_id, "ver_d1")
})

# ---------------------------------------------------------------------------
# format_vrs() — multi-pip_id survey (two welfare types)
# ---------------------------------------------------------------------------

test_that("format_vrs returns one row per pip_id for a multi-pip_id survey", {
  pip_ids <- c("BOL_2022_EH_INC_ALL", "BOL_2022_EH_INC_GPWG")
  entry <- make_proc_entry_dual(pip_ids)
  proc <- list("BOL_2022_EH" = entry)

  result_dt <- pipdata:::format_vrs(proc, "versions_data")

  expect_equal(nrow(result_dt), 2L)
  expect_setequal(result_dt$pip_id, pip_ids)
  expect_true(all(result_dt$survey_id == "BOL_2022_EH"))
})

# ---------------------------------------------------------------------------
# format_vrs() — one row per pip_id (no duplication by the function itself)
# The upstream guarantee (pd_split_alt_welfare produces distinct pip_ids)
# ensures pip_names are always distinct. This test documents that format_vrs()
# maps n distinct pip_names → exactly n output rows.
# ---------------------------------------------------------------------------

test_that("format_vrs produces exactly one row per distinct pip_id", {
  pip_ids <- c("BOL_2022_EH_INC_ALL", "BOL_2022_EH_INC_GPWG")
  entry <- make_proc_entry_dual(pip_ids)
  proc <- list("BOL_2022_EH" = entry)

  result_dt <- pipdata:::format_vrs(proc, "versions_data")

  # Exactly 2 rows — one per input pip_id, no duplication added by format_vrs.
  expect_equal(nrow(result_dt), length(pip_ids))
  expect_setequal(result_dt$pip_id, pip_ids)
})

# ---------------------------------------------------------------------------
# format_vrs() — NULL version list returns NULL (survey skipped entirely)
# ---------------------------------------------------------------------------

test_that("format_vrs returns NULL for a survey with NULL version list", {
  entry <- list(
    pip_names = "BOL_2022_EH_INC_ALL",
    versions_data = NULL,
    versions_metadata = NULL
  )
  proc <- list("BOL_2022_EH" = entry)

  result_dt <- pipdata:::format_vrs(proc, "versions_data")

  # rbindlist of all-NULL returns empty data.table
  expect_equal(nrow(result_dt), 0L)
})

# ---------------------------------------------------------------------------
# format_vrs() — skipped entry is preserved with skipped = TRUE
# ---------------------------------------------------------------------------

test_that("format_vrs preserves skipped flag and reason for skipped pip_ids", {
  entry <- list(
    pip_names = "BOL_2022_EH_INC_ALL",
    versions_data = list(
      BOL_2022_EH_INC_ALL = list(
        metadata = list(content_hash = NA_character_),
        skipped = TRUE,
        reason = "content unchanged"
      )
    ),
    versions_metadata = list(
      BOL_2022_EH_INC_ALL = list(
        metadata = list(content_hash = NA_character_),
        skipped = TRUE,
        reason = "content unchanged"
      )
    )
  )
  proc <- list("BOL_2022_EH" = entry)

  result_dt <- pipdata:::format_vrs(proc, "versions_data")

  expect_equal(nrow(result_dt), 1L)
  expect_true(isTRUE(result_dt$skipped[[1L]]))
  expect_equal(result_dt$reason[[1L]], "content unchanged")
})

# ---------------------------------------------------------------------------
# missing-metadata exclusion path (P2.3)
# Simulates the sentinel_col / missing_meta filter inside update_pip_inventory()
# by constructing the joined vrs table directly, without calling the full
# function (which requires mocking pipload, pipfun, etc.).
# ---------------------------------------------------------------------------

test_that("missing-metadata sentinel filters pip_ids with NA content_hash_metadata", {
  # vrs_dt has 2 pip_ids; vrs_mdt only has 1 (the second has no saved metadata).
  pip_ids <- c("BOL_2022_EH_INC_ALL", "BOL_2022_EH_INC_GPWG")

  proc_data <- list(
    "BOL_2022_EH" = make_proc_entry_dual(pip_ids)
  )
  # Versions_metadata for GPWG is absent — simulate by using proc with only ALL.
  proc_data_meta <- list(
    "BOL_2022_EH" = list(
      pip_names = pip_ids[[1L]],
      versions_data = list(
        BOL_2022_EH_INC_ALL = make_ventry("m1")
      ),
      versions_metadata = list(
        BOL_2022_EH_INC_ALL = make_ventry("m1")
      )
    )
  )

  vrs_dt  <- pipdata:::format_vrs(proc_data, "versions_data")
  vrs_mdt <- pipdata:::format_vrs(proc_data_meta, "versions_metadata")

  # Replicate the join from update_pip_inventory()
  vrs <- joyn::left_join(
    vrs_dt,
    vrs_mdt,
    by = c("survey_id", "pip_id"),
    suffix = c("_data", "_metadata"),
    relationship = "one-to-one",
    reportvar = FALSE,
    verbose = FALSE
  )

  # The sentinel logic from update_pip_inventory()
  sentinel_col <- if ("content_hash" %in% names(vrs_mdt)) {
    "content_hash_metadata"
  } else {
    NULL
  }

  expect_false(is.null(sentinel_col), label = "content_hash must exist in vrs_mdt")
  expect_true(sentinel_col %in% names(vrs), label = "sentinel column must be in joined vrs")

  # GPWG should have NA sentinel; ALL should not
  gpwg_row <- vrs[pip_id == "BOL_2022_EH_INC_GPWG"]
  all_row  <- vrs[pip_id == "BOL_2022_EH_INC_ALL"]

  expect_true(is.na(gpwg_row[[sentinel_col]]))
  expect_false(is.na(all_row[[sentinel_col]]))

  # After applying the filter, only ALL survives
  vrs_filtered <- vrs[!is.na(data.table::as.data.table(vrs)[[sentinel_col]])]
  expect_equal(nrow(vrs_filtered), 1L)
  expect_equal(vrs_filtered$pip_id, "BOL_2022_EH_INC_ALL")
})

# ---------------------------------------------------------------------------
# reporting_level derivation (P2.4)
# Replicates the pfw_rl_unq computation + join from update_pip_inventory()
# on a controlled mock, without calling the full function.
# ---------------------------------------------------------------------------

test_that("reporting_level derivation produces '1' for national and '2' for subnational", {
  # Mock inventory with one national and one subnational survey
  new_pip_inv <- data.table::data.table(
    survey_id     = c("ABX_2020_HBS", "ABX_2021_HBS"),
    pip_id        = c("ABX_2020_HBS_INC_ALL", "ABX_2021_HBS_INC_ALL"),
    country_code  = c("ABX", "ABX"),
    surveyid_year = c(2020L, 2021L),
    survey_acronym = c("HBS", "HBS")
  )

  # Mock PFW: ABX 2020 is national (all domains = 1),
  #           ABX 2021 is subnational (cpi_domain = 2)
  mock_pfw <- data.table::data.table(
    country_code   = c("ABX", "ABX"),
    surveyid_year  = c(2020L, 2021L),
    survey_acronym = c("HBS", "HBS"),
    welfare_type   = c("income", "income"),
    inpovcal       = c(1L, 1L),
    cpi_domain     = c(1L, 2L),
    ppp_domain     = c(1L, 1L),
    gdp_domain     = c(1L, 1L),
    pce_domain     = c(1L, 1L),
    pop_domain     = c(1L, 1L)
  )

  # Replicate logic from update_pip_inventory()
  pfw_rl <- mock_pfw[inpovcal == 1L]
  pfw_rl[,
    reporting_level := as.character(do.call(pmax, .SD)),
    .SDcols = pipdata:::.DOMAIN_COLS
  ]
  pfw_rl_unq <- pfw_rl[,
    .(reporting_level = reporting_level[[1L]]),
    by = .(country_code, surveyid_year, survey_acronym)
  ]
  result <- joyn::left_join(
    new_pip_inv,
    pfw_rl_unq,
    by = c("country_code", "surveyid_year", "survey_acronym"),
    relationship = "many-to-one",
    reportvar = FALSE,
    verbose = FALSE
  )

  expect_equal(result[pip_id == "ABX_2020_HBS_INC_ALL", reporting_level], "1")
  expect_equal(result[pip_id == "ABX_2021_HBS_INC_ALL", reporting_level], "2")
  # Values must be character, not integer
  expect_type(result$reporting_level, "character")
})

# ---------------------------------------------------------------------------
# reporting_level re-run collision (regression: duplicate column on second run)
# When new_pip_inv already has a reporting_level column (carried over from
# old_pip_inv loaded off disk), joining pfw_rl_unq — which also produces
# reporting_level — causes collapse::ftransform_core() to error with
# "All columns of .data have to be uniquely named".
# ---------------------------------------------------------------------------

test_that("reporting_level join succeeds when new_pip_inv already has reporting_level", {
  # Simulate new_pip_inv as it exists after a second run:
  # the old inventory row already carries reporting_level = "1".
  new_pip_inv <- data.table::data.table(
    survey_id = "ABX_2020_HBS",
    pip_id = "ABX_2020_HBS_INC_ALL",
    country_code = "ABX",
    surveyid_year = 2020L,
    survey_acronym = "HBS",
    reporting_level = "1" # ← already present from previous run
  )

  mock_pfw <- data.table::data.table(
    country_code = "ABX",
    surveyid_year = 2020L,
    survey_acronym = "HBS",
    inpovcal = 1L,
    cpi_domain = 1L,
    ppp_domain = 1L,
    gdp_domain = 1L,
    pce_domain = 1L,
    pop_domain = 1L
  )

  pfw_rl <- mock_pfw[inpovcal == 1L]
  pfw_rl[,
    reporting_level := as.character(do.call(pmax, .SD)),
    .SDcols = pipdata:::.DOMAIN_COLS
  ]
  pfw_rl_unq <- pfw_rl[,
    .(reporting_level = reporting_level[[1L]]),
    by = .(country_code, surveyid_year, survey_acronym)
  ]

  # Must not error — delegates to the same drop_rl_cols() called in production.
  pipdata:::drop_rl_cols(new_pip_inv)
  expect_no_error({
    result <- joyn::left_join(
      new_pip_inv,
      pfw_rl_unq,
      by = c("country_code", "surveyid_year", "survey_acronym"),
      relationship = "many-to-one",
      reportvar = FALSE,
      verbose = FALSE
    )
  })
  expect_equal(result$reporting_level, "1")
})

# ---------------------------------------------------------------------------
# reporting_level suffixed-column cleanup (regression: .x/.y artifacts)
# When a previous run persisted reporting_level.x and reporting_level.y to disk
# (from a joyn suffix collision), those columns survive into new_pip_inv via
# rowbind with old_pip_inv. The pattern-based drop must remove all variants.
# ---------------------------------------------------------------------------

test_that("reporting_level join succeeds when new_pip_inv has .x/.y suffixed artifacts", {
  new_pip_inv <- data.table::data.table(
    survey_id = "ABX_2020_HBS",
    pip_id = "ABX_2020_HBS_INC_ALL",
    country_code = "ABX",
    surveyid_year = 2020L,
    survey_acronym = "HBS",
    reporting_level.x = NA_character_, # \u2190 artifact from historic joyn collision
    reporting_level.y = NA_character_, # \u2190 artifact from historic joyn collision
    reporting_level = "1" # \u2190 stale exact column also present
  )

  mock_pfw <- data.table::data.table(
    country_code = "ABX",
    surveyid_year = 2020L,
    survey_acronym = "HBS",
    inpovcal = 1L,
    cpi_domain = 1L,
    ppp_domain = 1L,
    gdp_domain = 1L,
    pce_domain = 1L,
    pop_domain = 1L
  )

  pfw_rl <- mock_pfw[inpovcal == 1L]
  pfw_rl[,
    reporting_level := as.character(do.call(pmax, .SD)),
    .SDcols = pipdata:::.DOMAIN_COLS
  ]
  pfw_rl_unq <- pfw_rl[,
    .(reporting_level = reporting_level[[1L]]),
    by = .(country_code, surveyid_year, survey_acronym)
  ]

  # Delegates to the same drop_rl_cols() called in production.
  pipdata:::drop_rl_cols(new_pip_inv)

  expect_no_error({
    result <- joyn::left_join(
      new_pip_inv,
      pfw_rl_unq,
      by = c("country_code", "surveyid_year", "survey_acronym"),
      relationship = "many-to-one",
      reportvar = FALSE,
      verbose = FALSE
    )
  })

  # Exactly one reporting_level column, correct value, no suffixed artifacts
  expect_equal(
    grep("^reporting_level", names(result), value = TRUE),
    "reporting_level"
  )
  expect_equal(result$reporting_level, "1")
})

