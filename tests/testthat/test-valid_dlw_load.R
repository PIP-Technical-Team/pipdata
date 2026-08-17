# Tests for valid_dlw_load.R helper functions
#
# Covers:
#   filter_aux_inv()    — no max_year clamp; joyn::inner_join discards
#                         unmatched years naturally
#   valid_dlw_load()    — aux_no_changes_inf / aux_changes_no_surveys_inf /
#                         aux_changes_inf logmeta discriminators; cli_abort
#                         (class "piperr") when nothing to clean; no
#                         duplicate survey_id in return value when a survey
#                         appears in both new-survey and aux-changed sets;
#                         surveys_to_clean_inf summary logmeta
#   inv_to_process()    — content_hash vs content_hash_dlw comparison on
#                         survey_id; joyn::left_join must not leak .joyn
#                         column or duplicate rows on multi-pip_id surveys

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

# Minimal master-inventory fragment with survey_id + content_hash_dlw, the
# only two columns inv_to_process() needs from the master inventory.
make_master_hash <- function(survey_ids, content_hash_dlw) {
  data.table::data.table(
    survey_id = survey_ids,
    content_hash_dlw = content_hash_dlw
  )
}

# Master-inventory fragment that also carries pip_id (and optionally aux hashes)
# for force_surveys pip_id reverse-map tests.
make_master_hash_pip <- function(survey_ids, content_hash_dlw, pip_ids) {
  data.table::data.table(
    survey_id = survey_ids,
    content_hash_dlw = content_hash_dlw,
    pip_id = pip_ids
  )
}

# ---------------------------------------------------------------------------
# inv_to_process: .joyn column must not be present in result
# ---------------------------------------------------------------------------

test_that("inv_to_process does not add .joyn column to result", {
  # A single survey not in the master inventory — the left_join leaves
  # content_hash_dlw as NA, so the survey is kept.
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master contains a DIFFERENT survey so our survey has no matching
  # content_hash_dlw (NA after the left_join) and is kept.
  master_no_col <- make_master_hash("BRA_2019_PNADC", "h_9")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_no_col,
    .package = "pipload"
  )

  result <- pipdata:::inv_to_process(inv)

  expect_false(
    ".joyn" %in% names(result),
    info = paste0(
      "joyn::left_join in inv_to_process() must be called with ",
      "reportvar = FALSE to avoid the .joyn column leaking into the ",
      "result and causing duplicate survey_id rows"
    )
  )
  expect_false(
    "content_hash_dlw" %in% names(result),
    info = "content_hash_dlw must be dropped from the result after comparison"
  )
})

# ---------------------------------------------------------------------------
# valid_dlw_load: no duplicate survey_ids when survey is in both new + aux sets
# ---------------------------------------------------------------------------

test_that("valid_dlw_load returns no duplicate survey_ids when survey appears in DLW-changed and aux-changed sets", {
  # COL 2020 GEIH: DLW-content-changed (kept by inv_to_process) AND
  # aux-hash-changed (candidate + affected by CPI change).
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master has this survey with a DIFFERENT DLW content_hash (so inv_to_process
  # keeps it) AND a DIFFERENT aux hash (so it is an aux candidate).
  master_changed <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_0",  # differs from DLW's "h_1" → DLW-changed
    aux_cpi_hash = "old_cpi_hash"  # differs from current → aux candidate
  )

  # Aux change for COL 2020 → filter_aux_inv returns the same survey.
  cpi_changes <- list(
    data.table::data.table(country_code = "COL", surveyid_year = 2020L)
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_changed,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      # Return a named list matching the structure expected by filter_aux_inv
      stats::setNames(list(cpi_changes), measure[[1]])
    },
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "new_cpi_hash"),
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

  # surveys_to_clean_inf summary logmeta must be present with correct counts.
  log <- pipfun::log_get("pipdata_log")
  summary_entries <- Filter(
    function(x) !is.null(x) && identical(x$info, "surveys_to_clean_inf"),
    log$logmeta
  )
  expect_length(summary_entries, 1L)
  expect_equal(summary_entries[[1]]$n_total_unique, nrow(result))
})

# ---------------------------------------------------------------------------
# filter_aux_inv: no max_year clamp -- join discards unmatched years
# ---------------------------------------------------------------------------

test_that("filter_aux_inv handles an aux-change year beyond the max inventory year without a clamp", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Aux change year (2099) is beyond max(inv$surveyid_year) == 2020. With the
  # max_year clamp removed, joyn::inner_join must discard this naturally.
  changes_aux <- list(
    data.table::data.table(country_code = "COL", surveyid_year = 2099L)
  )

  result <- pipdata:::filter_aux_inv(inv, changes_aux)

  expect_null(
    result,
    info = paste0(
      "filter_aux_inv must rely on joyn::inner_join to discard unmatched ",
      "years -- no max_year clamp should remain"
    )
  )
})

# ---------------------------------------------------------------------------
# valid_dlw_load: aux-change logmeta discriminators (aux_no_changes_inf /
# aux_changes_no_surveys_inf)
# ---------------------------------------------------------------------------

test_that("valid_dlw_load logs aux_no_changes_inf when no aux hash changed", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master has this survey with the SAME aux hash as current → no candidate.
  master_same <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1",
    aux_cpi_hash = "hash_cpi"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_same,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

  # inv_to_process returns NULL (survey already cleaned, same DLW hash), and
  # aux_hash_candidates returns NULL (same aux hash) → nothing to clean → abort.
  suppressMessages(
    expect_error(
      valid_dlw_load(
        inv = inv,
        aux_measures = "cpi",
        aux_hashes = c(cpi = "hash_cpi"),
        force = FALSE,
        verbose = FALSE
      ),
      class = "piperr"
    )
  )

  log <- pipfun::log_get("pipdata_log")
  info_values <- vapply(
    log$logmeta,
    function(x) if (is.null(x) || is.null(x$info)) NA_character_ else x$info,
    character(1)
  )

  expect_true("aux_no_changes_inf" %in% info_values)
  expect_false("aux_changes_no_surveys_inf" %in% info_values)
  expect_false("aux_changes_inf" %in% info_values)
})

test_that("valid_dlw_load logs aux_changes_no_surveys_inf when aux changes affect no requested surveys", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master has this survey with a DIFFERENT aux hash than current → candidate.
  # But valid_aux_load returns changes only for BRA (not COL), so the
  # intersection with the candidate (COL) is empty → aux_changes_no_surveys_inf.
  master_changed <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1",
    aux_cpi_hash = "old_cpi_hash"
  )

  # Aux change for a country/year combination absent from inv (BRA, not COL).
  cpi_changes <- list(
    data.table::data.table(country_code = "BRA", surveyid_year = 2019L)
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_changed,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      stats::setNames(list(cpi_changes), measure)
    },
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

  # inv_to_process returns NULL (survey already cleaned, same DLW hash), and
  # the aux intersection is empty → nothing to clean → abort.
  suppressMessages(
    expect_error(
      valid_dlw_load(
        inv = inv,
        aux_measures = "cpi",
        aux_hashes = c(cpi = "new_cpi_hash"),
        force = FALSE,
        verbose = FALSE
      ),
      class = "piperr"
    )
  )

  log <- pipfun::log_get("pipdata_log")
  info_values <- vapply(
    log$logmeta,
    function(x) if (is.null(x) || is.null(x$info)) NA_character_ else x$info,
    character(1)
  )

  expect_true("aux_changes_no_surveys_inf" %in% info_values)
  expect_false("aux_no_changes_inf" %in% info_values)
  expect_false("aux_changes_inf" %in% info_values)
})

# ---------------------------------------------------------------------------
# valid_dlw_load: cli_abort (class "piperr") when nothing to clean
# ---------------------------------------------------------------------------

test_that("valid_dlw_load aborts with class piperr when nothing to clean", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master already has this survey with the SAME content_hash -- already clean.
  master_same <- make_master_hash("COL_2020_GEIH", "h_1")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_same,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  expect_error(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      force = FALSE,
      verbose = FALSE
    ),
    class = "piperr"
  )
})

# ---------------------------------------------------------------------------
# inv_to_process: content_hash vs content_hash_dlw comparison scenarios
# ---------------------------------------------------------------------------

test_that("inv_to_process keeps a survey absent from master (content_hash_dlw is NA)", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )
  master <- make_master_hash("BRA_2019_PNADC", "h_9")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )

  result <- pipdata:::inv_to_process(inv, verbose = FALSE)

  expect_false(is.null(result))
  expect_equal(nrow(result), 1L)
  expect_equal(result$survey_id, "COL_2020_GEIH")
})

test_that("inv_to_process excludes a survey whose content_hash matches content_hash_dlw", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )
  # make_dlw_inv() sets content_hash = "h_1" for the first (only) row.
  master <- make_master_hash("COL_2020_GEIH", "h_1")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )

  result <- pipdata:::inv_to_process(inv, verbose = FALSE)

  expect_null(
    result,
    info = "a survey whose content_hash is unchanged since last clean must be excluded"
  )
})

test_that("inv_to_process keeps a survey whose content_hash differs from content_hash_dlw", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )
  # content_hash is "h_1"; master's content_hash_dlw is different ("h_0").
  master <- make_master_hash("COL_2020_GEIH", "h_0")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )

  result <- pipdata:::inv_to_process(inv, verbose = FALSE)

  expect_false(is.null(result))
  expect_equal(nrow(result), 1L)
  expect_false(
    "content_hash_dlw" %in% names(result),
    info = "content_hash_dlw must be dropped from the result after comparison"
  )
})

test_that("inv_to_process does not fan out rows when master has multiple pip_id rows per survey_id", {
  inv <- make_dlw_inv(
    "BOL_2022_EH",
    country_codes = "BOL",
    surveyid_years = 2022L,
    survey_acronyms = "EH"
  )
  # Two pip_id rows (e.g. INC_ALL / INC_GPWG) for the same survey_id, both
  # carrying the same content_hash_dlw, and different from the DLW's "h_1".
  master_dup <- make_master_hash(
    c("BOL_2022_EH", "BOL_2022_EH"),
    c("h_0", "h_0")
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_dup,
    .package = "pipload"
  )

  result <- pipdata:::inv_to_process(inv, verbose = FALSE)

  expect_false(is.null(result))
  expect_equal(
    nrow(result),
    1L,
    info = "joining on survey_id against a non-deduplicated master must not fan out matching rows"
  )
})

test_that("inv_to_process returns all surveys when the master inventory cannot be loaded", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) stop("no master exists"),
    .package = "pipload"
  )

  result <- pipdata:::inv_to_process(inv, verbose = FALSE)

  expect_false(is.null(result))
  expect_equal(nrow(result), nrow(inv))
  expect_equal(result$survey_id, inv$survey_id)
})

test_that("inv_to_process result never carries a .joyn diagnostic column", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )
  master <- make_master_hash("BRA_2019_PNADC", "h_9")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )

  result <- pipdata:::inv_to_process(inv, verbose = FALSE)

  expect_false(".joyn" %in% names(result))
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: unchanged hash skips that measure's aux comparison
# ---------------------------------------------------------------------------

test_that("valid_dlw_load does not call valid_aux_load when aux hash is unchanged", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master has this survey with the SAME aux hash as current → no candidate.
  master_same <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1",
    aux_cpi_hash = "hash_cpi"
  )

  aux_called <- FALSE
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_same,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      aux_called <<- TRUE
      NULL
    },
    .package = "pipdata"
  )

  # inv_to_process returns NULL (same DLW hash) and no aux candidate → abort.
  suppressMessages(
    expect_error(
      valid_dlw_load(
        inv = inv,
        aux_measures = "cpi",
        aux_hashes = c(cpi = "hash_cpi"),
        force = FALSE,
        verbose = FALSE
      ),
      class = "piperr"
    )
  )

  expect_false(
    aux_called,
    info = "valid_aux_load must not be called when no aux hash changed"
  )
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: changed hash invokes comparison only for changed measures
# ---------------------------------------------------------------------------

test_that("valid_dlw_load calls valid_aux_load only for changed measures", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master: cpi hash changed (old vs new), ppp hash unchanged.
  master <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1",
    aux_cpi_hash = "old_cpi_hash",
    aux_ppp_hash = "hash_ppp"
  )

  measures_seen <- character(0)
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      measures_seen <<- c(measures_seen, measure)
      # Return a change for COL so the survey is affected.
      stats::setNames(
        list(list(data.table::data.table(country_code = "COL", surveyid_year = 2020L))),
        measure
      )
    },
    .package = "pipdata"
  )

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = c("cpi", "ppp"),
    aux_hashes = c(cpi = "new_cpi_hash", ppp = "hash_ppp"),
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(
    measures_seen,
    "cpi",
    info = "valid_aux_load must be called only for the changed measure (cpi), not ppp"
  )
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: changed CPI affecting non-requested countries does not
# re-clean requested surveys (COL/ARG vs USA/GER)
# ---------------------------------------------------------------------------

test_that("valid_dlw_load does not re-clean requested surveys when aux changed only for other countries", {
  # Requested surveys: COL 2020 and ARG 2019.
  inv <- make_dlw_inv(
    c("COL_2020_GEIH", "ARG_2019_EPH"),
    country_codes = c("COL", "ARG"),
    surveyid_years = c(2020L, 2019L),
    survey_acronyms = c("GEIH", "EPH")
  )

  # Both surveys already cleaned with the OLD cpi hash → both are candidates.
  master <- data.table::data.table(
    survey_id = c("COL_2020_GEIH", "ARG_2019_EPH"),
    content_hash_dlw = c("h_1", "h_2"),
    aux_cpi_hash = c("old_cpi_hash", "old_cpi_hash")
  )

  # CPI changed only for USA and GER — not for COL or ARG.
  cpi_changes <- list(
    data.table::data.table(country_code = c("USA", "GER"), surveyid_year = c(2020L, 2019L))
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      stats::setNames(list(cpi_changes), measure)
    },
    .package = "pipdata"
  )

  # inv_to_process returns NULL (both surveys already cleaned, same DLW hash),
  # and the aux intersection is empty (changes only for USA/GER) → abort.
  suppressMessages(
    expect_error(
      valid_dlw_load(
        inv = inv,
        aux_measures = "cpi",
        aux_hashes = c(cpi = "new_cpi_hash"),
        force = FALSE,
        verbose = FALSE
      ),
      class = "piperr"
    )
  )
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: changed CPI affecting a requested survey returns it
# ---------------------------------------------------------------------------

test_that("valid_dlw_load re-cleans a requested survey affected by changed aux", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Survey already cleaned with the OLD cpi hash → candidate.
  master <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1",
    aux_cpi_hash = "old_cpi_hash"
  )

  # CPI changed for COL 2020 → the requested survey is affected.
  cpi_changes <- list(
    data.table::data.table(country_code = "COL", surveyid_year = 2020L)
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      stats::setNames(list(cpi_changes), measure)
    },
    .package = "pipdata"
  )

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "new_cpi_hash"),
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(result$survey_id, "COL_2020_GEIH")
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: missing (NA) historical hash is ignored, not a candidate
# ---------------------------------------------------------------------------

test_that("valid_dlw_load ignores a survey with missing historical aux hash", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master has the survey but NO aux_cpi_hash column (cleaned before feature).
  master <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1"
  )

  aux_called <- FALSE
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      aux_called <<- TRUE
      NULL
    },
    .package = "pipdata"
  )

  # The survey has no stored aux hash → ignored for aux-change detection.
  # inv_to_process returns NULL (same DLW hash) and no aux candidate → abort.
  suppressMessages(
    expect_error(
      valid_dlw_load(
        inv = inv,
        aux_measures = "cpi",
        aux_hashes = c(cpi = "new_cpi_hash"),
        force = FALSE,
        verbose = FALSE
      ),
      class = "piperr"
    )
  )

  expect_false(
    aux_called,
    info = "valid_aux_load must not be called when the only survey has no stored aux hash"
  )
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: NA-hash count is logged when some surveys lack hashes
# ---------------------------------------------------------------------------

test_that("valid_dlw_load logs the number of surveys with missing aux hash", {
  # Two surveys: COL has a populated hash (unchanged), ARG has NA hash.
  inv <- make_dlw_inv(
    c("COL_2020_GEIH", "ARG_2019_EPH"),
    country_codes = c("COL", "ARG"),
    surveyid_years = c(2020L, 2019L),
    survey_acronyms = c("GEIH", "EPH")
  )

  master <- data.table::data.table(
    survey_id = c("COL_2020_GEIH", "ARG_2019_EPH"),
    content_hash_dlw = c("h_1", "h_2"),
    aux_cpi_hash = c("hash_cpi", NA_character_)
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

  # COL hash unchanged → no candidate; ARG has NA hash → ignored. Nothing to
  # process → abort.
  suppressMessages(
    expect_error(
      valid_dlw_load(
        inv = inv,
        aux_measures = "cpi",
        aux_hashes = c(cpi = "hash_cpi"),
        force = FALSE,
        verbose = FALSE
      ),
      class = "piperr"
    )
  )

  log <- pipfun::log_get("pipdata_log")
  na_entries <- Filter(
    function(x) !is.null(x) && identical(x$info, "aux_na_hash_inf"),
    log$logmeta
  )
  expect_length(na_entries, 1L)
  expect_equal(na_entries[[1]]$n_surveys_na_hash, 1L)
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: conflicting aux hashes for same survey/content abort
# ---------------------------------------------------------------------------

test_that("valid_dlw_load aborts on conflicting aux hashes for same survey and content_hash_dlw", {
  inv <- make_dlw_inv(
    "BOL_2022_EH",
    country_codes = "BOL",
    surveyid_years = 2022L,
    survey_acronyms = "EH"
  )

  # Two pip_id rows for the same survey_id + content_hash_dlw, but with
  # DIFFERENT aux_cpi_hash values → conflict.
  master_conflict <- data.table::data.table(
    survey_id = c("BOL_2022_EH", "BOL_2022_EH"),
    content_hash_dlw = c("h_1", "h_1"),
    aux_cpi_hash = c("hash_a", "hash_b")
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master_conflict,
    .package = "pipload"
  )

  expect_error(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      aux_hashes = c(cpi = "hash_c"),
      force = FALSE,
      verbose = FALSE
    ),
    class = "aux_hash_candidates_conflict"
  )
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: master inventory loaded exactly once
# ---------------------------------------------------------------------------

test_that("valid_dlw_load loads the master inventory exactly once", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  master <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1",
    aux_cpi_hash = "old_cpi_hash"
  )

  load_count <- 0L
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      load_count <<- load_count + 1L
      master
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      stats::setNames(
        list(list(data.table::data.table(country_code = "COL", surveyid_year = 2020L))),
        measure
      )
    },
    .package = "pipdata"
  )

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "new_cpi_hash"),
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(
    load_count,
    1L,
    info = "the master inventory must be loaded exactly once and shared"
  )
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: force mode skips master/aux comparisons
# ---------------------------------------------------------------------------

test_that("valid_dlw_load force mode skips master and aux comparisons and processes all rows", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  master_loaded <- FALSE
  aux_called <- FALSE
  candidates_called <- FALSE
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      master_loaded <<- TRUE
      NULL
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      aux_called <<- TRUE
      NULL
    },
    aux_hash_candidates = function(...) {
      candidates_called <<- TRUE
      NULL
    },
    .package = "pipdata"
  )

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "new_cpi_hash"),
    force = TRUE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(result$survey_id, "COL_2020_GEIH")
  expect_false(master_loaded, info = "force mode must not load the master inventory")
  expect_false(aux_called, info = "force mode must not call valid_aux_load")
  expect_false(candidates_called, info = "force mode must not call aux_hash_candidates")
})

# ---------------------------------------------------------------------------
# Two-stage aux gating: no .joyn column or duplicate survey IDs in output
# ---------------------------------------------------------------------------

test_that("valid_dlw_load output has no .joyn column and no duplicate survey_ids", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  master <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1",
    aux_cpi_hash = "old_cpi_hash"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      stats::setNames(
        list(list(data.table::data.table(country_code = "COL", surveyid_year = 2020L))),
        measure
      )
    },
    .package = "pipdata"
  )

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "new_cpi_hash"),
    force = FALSE,
    verbose = FALSE
  )

  expect_false(".joyn" %in% names(result))
  expect_equal(anyDuplicated(result$survey_id), 0L)
})

# ---------------------------------------------------------------------------
# P1.1 regression: multiple historical content_hash_dlw rows for one survey
# ---------------------------------------------------------------------------

test_that("aux_hash_candidates matches the current DLW content version, not a historical one", {
  # Survey has TWO historical DLW content hashes in the master. The current
  # DLW inventory corresponds to content_hash "h_2". Only the master row with
  # content_hash_dlw == "h_2" should be used for the aux comparison.
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )
  # make_dlw_inv sets content_hash = "h_1" for the first row; override to h_2.
  inv[, content_hash := "h_2"]

  # Master has two historical rows: h_1 (old aux hash) and h_2 (current aux
  # hash matching the current run). The current run's aux hash equals the
  # h_2 row's stored hash, so the survey should NOT be a candidate.
  master <- data.table::data.table(
    survey_id = c("COL_2020_GEIH", "COL_2020_GEIH"),
    content_hash_dlw = c("h_1", "h_2"),
    aux_cpi_hash = c("old_cpi_hash", "hash_cpi")
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      stop("valid_aux_load must not be called: current aux hash is unchanged")
    },
    .package = "pipdata"
  )

  # inv_to_process returns NULL (survey already cleaned, same DLW hash h_2),
  # and the current aux hash matches the h_2 master row → no candidate → abort.
  suppressMessages(
    expect_error(
      valid_dlw_load(
        inv = inv,
        aux_measures = "cpi",
        aux_hashes = c(cpi = "hash_cpi"),
        force = FALSE,
        verbose = FALSE
      ),
      class = "piperr"
    )
  )
})

# ---------------------------------------------------------------------------
# P2.1: failed master load must not trigger a second load
# ---------------------------------------------------------------------------

test_that("inv_to_process does not re-load the master when it was already unavailable", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  load_count <- 0L
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      load_count <<- load_count + 1L
      stop("no master exists")
    },
    .package = "pipload"
  )

  # master_available = FALSE means the caller already attempted and failed to
  # load the master; inv_to_process must return all surveys without re-loading.
  result <- pipdata:::inv_to_process(
    inv,
    dt_master = NULL,
    master_available = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(nrow(result), nrow(inv))
  expect_equal(
    load_count,
    0L,
    info = "inv_to_process must not re-load the master when it was already unavailable"
  )
})

# ---------------------------------------------------------------------------
# P2.2: aux_hashes input validation
# ---------------------------------------------------------------------------

test_that("valid_dlw_load aborts on unnamed aux_hashes", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  expect_error(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      aux_hashes = c("hash_cpi"),  # unnamed
      force = FALSE,
      verbose = FALSE
    ),
    class = "valid_dlw_load_bad_aux_hashes"
  )
})

test_that("valid_dlw_load aborts on duplicate aux_hashes names", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  expect_error(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      aux_hashes = c(cpi = "hash_a", cpi = "hash_b"),
      force = FALSE,
      verbose = FALSE
    ),
    class = "valid_dlw_load_bad_aux_hashes"
  )
})

test_that("valid_dlw_load aborts on missing aux_hashes values", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  expect_error(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      aux_hashes = c(cpi = NA_character_),
      force = FALSE,
      verbose = FALSE
    ),
    class = "valid_dlw_load_bad_aux_hashes"
  )
})


# ---------------------------------------------------------------------------
# force_surveys: forced already-cleaned survey is retained in the candidate set
# ---------------------------------------------------------------------------

test_that("valid_dlw_load retains a forced survey that is already cleaned", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Already cleaned: same content_hash_dlw and no aux hash columns -> no
  # normal candidate, but force_surveys must add it back.
  master <- make_master_hash("COL_2020_GEIH", "h_1")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = "COL_2020_GEIH",
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(result$survey_id, "COL_2020_GEIH")
  expect_equal(nrow(result), 1L)
})

test_that("valid_dlw_load unions forced surveys with normal candidates and dedups", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # DLW-content-changed so it is ALSO a normal candidate (kept by
  # inv_to_process). Forcing it again must not duplicate the row.
  master <- make_master_hash("COL_2020_GEIH", "h_0")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = "COL_2020_GEIH",
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(anyDuplicated(result$survey_id), 0L)
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# force_surveys: pip_id input reverse-maps to survey_id via the master
# ---------------------------------------------------------------------------

test_that("valid_dlw_load reverse-maps a pip_id to its survey_id", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master carries a pip_id column; the pip reverse-maps to COL_2020_GEIH.
  master <- make_master_hash_pip("COL_2020_GEIH", "h_1", "COL_INC_ALL")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = "col_inc_all",  # case-insensitive pip_id
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(result$survey_id, "COL_2020_GEIH")

  log <- pipfun::log_get("pipdata_log")
  inf <- Filter(
    function(x) !is.null(x) && identical(x$info, "force_surveys_inf"),
    log$logmeta
  )
  expect_length(inf, 1L)
  expect_equal(inf[[1]]$n_forced, 1L)
  expect_equal(inf[[1]]$n_from_pip_id, 1L)
  expect_equal(inf[[1]]$n_from_survey_id, 0L)
})

# ---------------------------------------------------------------------------
# force_surveys: unknown identifier warns, logs, and skips (no abort)
# ---------------------------------------------------------------------------

test_that("valid_dlw_load warns, logs, and skips an unknown force_surveys identifier", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # A normal candidate (DLW-content-changed) ensures the run does not abort,
  # isolating the unknown-id behavior.
  master <- make_master_hash("COL_2020_GEIH", "h_0")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = "NOPE_9999_FOO",
    force = FALSE,
    verbose = FALSE
  )

  # Run proceeds; the unknown identifier is skipped (appears in no candidate).
  expect_false(is.null(result))
  expect_equal(result$survey_id, "COL_2020_GEIH")

  log <- pipfun::log_get("pipdata_log")
  unknown <- Filter(
    function(x) !is.null(x) && identical(x$info, "force_surveys_unknown_inf"),
    log$logmeta
  )
  expect_length(unknown, 1L)
  expect_equal(unknown[[1]]$unknown_identifiers, "NOPE_9999_FOO")
})

# ---------------------------------------------------------------------------
# force_surveys: forced-only run (empty normal candidates) does not abort
# ---------------------------------------------------------------------------

test_that("valid_dlw_load does not abort on a forced-only run", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Already cleaned and no aux columns -> no DLW/aux candidate. Only the
  # forced survey keeps the run alive (P2 ordering: forced_inv computed BEFORE
  # the nothing-to-clean abort).
  master <- make_master_hash("COL_2020_GEIH", "h_1")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = "COL_2020_GEIH",
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(result$survey_id, "COL_2020_GEIH")
})

# ---------------------------------------------------------------------------
# force_surveys: forced survey that is also an aux candidate is deduplicated
# ---------------------------------------------------------------------------

test_that("valid_dlw_load dedups a forced survey that is also an aux candidate", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Survey already cleaned (same DLW hash) but WITH a changed aux hash -> it is
  # an aux candidate via aux_hash_candidates + valid_aux_load. Forcing it too
  # must not create a duplicate.
  master <- data.table::data.table(
    survey_id = "COL_2020_GEIH",
    content_hash_dlw = "h_1",
    aux_cpi_hash = "old_cpi_hash"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) {
      stats::setNames(
        list(list(data.table::data.table(country_code = "COL", surveyid_year = 2020L))),
        measure
      )
    },
    .package = "pipdata"
  )

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "new_cpi_hash"),
    force_surveys = "COL_2020_GEIH",
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(anyDuplicated(result$survey_id), 0L)
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# force_surveys: duplicate inputs are deduplicated (n_forced = 1)
# ---------------------------------------------------------------------------

test_that("valid_dlw_load deduplicates duplicate force_surveys inputs", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  master <- make_master_hash("COL_2020_GEIH", "h_1")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = c("COL_2020_GEIH", "COL_2020_GEIH"),
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(anyDuplicated(result$survey_id), 0L)

  log <- pipfun::log_get("pipdata_log")
  inf <- Filter(
    function(x) !is.null(x) && identical(x$info, "force_surveys_inf"),
    log$logmeta
  )
  expect_length(inf, 1L)
  expect_equal(inf[[1]]$n_forced, 1L)
})

# ---------------------------------------------------------------------------
# force_surveys: non-character input aborts with class piperr
# ---------------------------------------------------------------------------

test_that("valid_dlw_load aborts with piperr for non-character force_surveys", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  expect_error(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      force_surveys = 42L,  # numeric, not character
      force = FALSE,
      verbose = FALSE
    ),
    class = "piperr"
  )
})

# ---------------------------------------------------------------------------
# force_surveys: master lacks pip_id column -> pip_id treated as unknown
# ---------------------------------------------------------------------------

test_that("valid_dlw_load warns when the master lacks a pip_id column", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master WITHOUT a pip_id column (legacy fragment). A normal candidate keeps
  # the run alive so the pip_id-only force input is isolated.
  master <- make_master_hash("COL_2020_GEIH", "h_0")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  expect_message(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      aux_hashes = c(cpi = "hash_cpi"),
      force_surveys = "SOME_PIP",
      force = FALSE,
      verbose = TRUE
    ),
    "lacks pip_id column"
  )
})

# ---------------------------------------------------------------------------
# force_surveys: master unavailable + pip_id input -> pip_id resolution skipped
# ---------------------------------------------------------------------------

test_that("valid_dlw_load warns when the master is unavailable for pip_id resolution", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # Master load fails; a normal candidate keeps the run alive. The pip_id-like
  # force input cannot be resolved because there is no master.
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) stop("no master"),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  expect_message(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      aux_hashes = c(cpi = "hash_cpi"),
      force_surveys = "SOME_PIP",
      force = FALSE,
      verbose = TRUE
    ),
    "Master inventory unavailable; pip_id resolution skipped"
  )
})

# ---------------------------------------------------------------------------
# force_surveys: direct-call guard -- force=TRUE + force_surveys aborts
# ---------------------------------------------------------------------------

test_that("valid_dlw_load direct-call guard aborts when force and force_surveys are both set", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  expect_error(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      force = TRUE,
      force_surveys = "COL_2020_GEIH",
      verbose = FALSE
    ),
    class = "piperr"
  )
})

# ---------------------------------------------------------------------------
# force_surveys: load_pip_master_inventory is called exactly once (C5)
# ---------------------------------------------------------------------------

test_that("valid_dlw_load loads the master exactly once when forcing a pip_id", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  master <- make_master_hash_pip("COL_2020_GEIH", "h_1", "COL_INC_ALL")

  load_count <- 0L
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) {
      load_count <<- load_count + 1L
      master
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = "COL_INC_ALL",
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(
    load_count,
    1L,
    info = "the master must be loaded exactly once and reused for pip_id resolution"
  )
})

# ---------------------------------------------------------------------------
# force_surveys: output has no duplicates and no .joyn columns
# ---------------------------------------------------------------------------

test_that("valid_dlw_load force_surveys output has no duplicates and no .joyn column", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  master <- make_master_hash_pip("COL_2020_GEIH", "h_1", "COL_INC_ALL")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = c("COL_2020_GEIH", "COL_INC_ALL"),
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_false(".joyn" %in% names(result))
  expect_equal(anyDuplicated(result$survey_id), 0L)
})

# ---------------------------------------------------------------------------
# P2.1 regression: ambiguous pip_id -> survey_id (multiple distinct survey_ids)
# aborts with class piperr instead of silently picking the first
# ---------------------------------------------------------------------------

test_that("valid_dlw_load aborts when a pip_id maps to multiple distinct survey_ids", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # One pip_id assigned to two DISTINCT survey_ids in the master -> ambiguous.
  master <- data.table::data.table(
    survey_id = c("COL_2020_GEIH", "BRA_2019_PNADC"),
    content_hash_dlw = c("h_1", "h_9"),
    pip_id = c("COL_INC_ALL", "COL_INC_ALL")
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  expect_error(
    valid_dlw_load(
      inv = inv,
      aux_measures = "cpi",
      aux_hashes = c(cpi = "hash_cpi"),
      force_surveys = "COL_INC_ALL",
      force = FALSE,
      verbose = FALSE
    ),
    class = "piperr"
  )
})

# ---------------------------------------------------------------------------
# force_surveys: pip_id resolving to a survey outside the module/latest filter
# (R9) is excluded and logged as unknown
# ---------------------------------------------------------------------------

test_that("valid_dlw_load logs a pip_id-resolved survey outside the filter as unknown", {
  # inv only contains COL (module/latest-filtered inventory). The force_surveys
  # pip_id maps to BRA, which is NOT in inv_svy_full -> the R9 exclusion route.
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # BRA is in the master (with a pip_id) but absent from inv. COL is a normal
  # candidate (content_hash differs: inv h_1 vs master h_0) so the run stays
  # alive while the out-of-filter forced pip_id is excluded.
  master <- data.table::data.table(
    survey_id = c("BRA_2019_PNADC", "COL_2020_GEIH"),
    content_hash_dlw = c("h_9", "h_0"),
    pip_id = c("BRA_INC_ALL", "COL_INC_ALL")
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = "BRA_INC_ALL",
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  # COL is the normal candidate; the forced BRA is excluded from the set.
  expect_setequal(result$survey_id, "COL_2020_GEIH")

  log <- pipfun::log_get("pipdata_log")
  inf <- Filter(
    function(x) !is.null(x) && identical(x$info, "force_surveys_unknown_inf"),
    log$logmeta
  )
  expect_length(inf, 1L)
  expect_equal(inf[[1]]$unknown_identifiers, "BRA_INC_ALL")
})

# ---------------------------------------------------------------------------
# force_surveys: survey_id membership wins over pip_id reverse-map (lookup-first)
# ---------------------------------------------------------------------------

test_that("valid_dlw_load resolves an identifier via survey_id before pip_id", {
  inv <- make_dlw_inv(
    "COL_2020_GEIH",
    country_codes = "COL",
    surveyid_years = 2020L,
    survey_acronyms = "GEIH"
  )

  # The pip_id column contains a value that literally equals the survey_id
  # "COL_2020_GEIH". Because it is a survey_id, lookup-first resolves it as a
  # survey_id, NOT as a pip_id (so n_from_pip_id must be 0).
  master <- make_master_hash_pip("COL_2020_GEIH", "h_1", "COL_2020_GEIH")

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) master,
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    valid_aux_load = function(measure, compare, verbose = TRUE) NULL,
    .package = "pipdata"
  )

  pipfun::log_init("pipdata_log", overwrite = TRUE)

  result <- valid_dlw_load(
    inv = inv,
    aux_measures = "cpi",
    aux_hashes = c(cpi = "hash_cpi"),
    force_surveys = "COL_2020_GEIH",
    force = FALSE,
    verbose = FALSE
  )

  expect_false(is.null(result))
  expect_equal(result$survey_id, "COL_2020_GEIH")

  log <- pipfun::log_get("pipdata_log")
  inf <- Filter(
    function(x) !is.null(x) && identical(x$info, "force_surveys_inf"),
    log$logmeta
  )
  expect_length(inf, 1L)
  expect_equal(inf[[1]]$n_from_survey_id, 1L)
  expect_equal(inf[[1]]$n_from_pip_id, 0L)
})
