make_dlw_catalog <- function() {
  data.table::data.table(
    Country = c("BOL", "ZWE", "COL", "PER", "BRA", "ARG", "MEX"),
    Year = c(2020L, 2021L, 2022L, 2023L, 2020L, 2019L, 2018L),
    Survey_acronym = c("EH", "PICES", "GEIH", "ENAHO", "PNAD", "EPH", "ENIGH"),
    Vermast = "01",
    Veralt = "01",
    Module = c("ALL", "GPWG", "GROUP", "BIN", "HIST", "ASPIRE", "L"),
    Collection = "GMD",
    FileName = paste0(
      c(
        "BOL_2020_EH", "ZWE_2021_PICES", "COL_2022_GEIH",
        "PER_2023_ENAHO", "BRA_2020_PNAD", "ARG_2019_EPH",
        "MEX_2018_ENIGH"
      ),
      ".dta"
    ),
    Checksum = paste0("checksum-", 1:7),
    Ext = "DTA",
    server_note = paste0("current-", 1:7)
  )
}

make_dlw_prior_inventory <- function() {
  prior <- make_dlw_catalog()[c(1:3, 6)]
  prior[, data_available := c("Yes", "No", "Yes", "Yes")]
  prior[FileName == "COL_2022_GEIH.dta", Checksum := "old-checksum"]
  data.table::rbindlist(list(
    prior,
    data.table::data.table(
      Country = "URY",
      Year = 2017L,
      Survey_acronym = "ECH",
      Vermast = "01",
      Veralt = "01",
      Module = "ALL",
      Collection = "GMD",
      FileName = "URY_2017_ECH.dta",
      Checksum = "deleted-checksum",
      Ext = "dta",
      server_note = "stale",
      data_available = "Yes"
    )
  ), use.names = TRUE, fill = TRUE)
}

make_compare_validation_inventory <- function() {
  data.table::data.table(
    survey_id = "BOL_2020_EH_V01_M_V01_A_GMD_ALL",
    pipeline_version = 1L,
    latest_version_id = "v1",
    content_hash = "hash-1",
    file_path = "bol.qs2",
    status = "valid",
    data_available = "Yes",
    date_validated = as.POSIXct("2026-08-26 12:00:00", tz = "UTC"),
    Checksum = "checksum-1",
    country_code = "BOL",
    surveyid_year = 2020L,
    survey_acronym = "EH",
    vermast = "v01",
    veralt = "v01",
    collection = "GMD",
    module = "ALL",
    tool = "TB"
  )
}

test_that("DLW comparison utility signatures remain unchanged", {
  expect_identical(
    formals(dlw_gmd_match),
    pairlist()
  )
  expect_identical(
    formals(dlw_gmd_new),
    as.pairlist(alist(check_missing = TRUE, update_inventory = FALSE))
  )
  expect_identical(
    formals(dlw_gmd_unvalidated),
    as.pairlist(alist(check_missing = TRUE))
  )
  expect_identical(
    formals(dlw_gmd_list),
    as.pairlist(alist(inv_gmd_list = "dlw_gmd_inv"))
  )
})

test_that("acquisition catalogs are copied, normalized, and schema checked", {
  catalog <- make_dlw_catalog()
  catalog[, Year := as.character(Year)]
  catalog[, Vermast := factor(Vermast)]
  before <- data.table::copy(catalog)

  normalized <- .normalize_dlw_acquisition_catalog(catalog, source = "server")

  expect_identical(catalog, before)
  expect_type(normalized$Year, "integer")
  expect_type(normalized$Vermast, "character")
  expect_identical(unique(normalized$Ext), "dta")
  expect_named(
    normalized,
    c(
      "Country", "Year", "Survey_acronym", "Vermast", "Veralt",
      "Module", "Collection", "FileName", "Checksum", "Ext",
      "server_note"
    )
  )

  missing_checksum <- make_dlw_catalog()[, Checksum := NULL]
  expect_error(
    .normalize_dlw_acquisition_catalog(missing_checksum, source = "server"),
    class = "pipdata_dlw_catalog_schema_error"
  )

  malformed_year <- make_dlw_catalog()
  malformed_year[, Year := as.numeric(Year)]
  malformed_year[1L, Year := 2020.5]
  expect_error(
    .normalize_dlw_acquisition_catalog(malformed_year, source = "server"),
    class = "pipdata_dlw_catalog_schema_error"
  )

  malformed_availability <- make_dlw_prior_inventory()
  malformed_availability[1L, data_available := "Maybe"]
  expect_error(
    .normalize_dlw_acquisition_catalog(
      malformed_availability,
      source = "local"
    ),
    class = "pipdata_dlw_catalog_schema_error"
  )
})

test_that("catalog normalization requires one checksum and row per filename", {
  conflicting_checksum <- data.table::rbindlist(list(
    make_dlw_catalog(),
    make_dlw_catalog()[1L][, Checksum := "another-checksum"]
  ))
  conflicting_row <- data.table::rbindlist(list(
    make_dlw_catalog(),
    make_dlw_catalog()[1L][, server_note := "conflicting-current-row"]
  ))
  exact_duplicate <- data.table::rbindlist(list(
    make_dlw_catalog(),
    make_dlw_catalog()[1L]
  ))

  expect_error(
    .normalize_dlw_acquisition_catalog(
      conflicting_checksum,
      source = "server"
    ),
    class = "pipdata_dlw_catalog_schema_error"
  )
  expect_error(
    .normalize_dlw_acquisition_catalog(conflicting_row, source = "server"),
    class = "pipdata_dlw_catalog_schema_error"
  )
  expect_equal(
    nrow(.normalize_dlw_acquisition_catalog(
      exact_duplicate,
      source = "server"
    )),
    7L
  )
})

test_that("zero-row server catalogs are load failures", {
  testthat::local_mocked_bindings(
    dlw_server_catalog = function() make_dlw_catalog()[0L],
    .package = "dlw"
  )
  expect_error(
    .load_dlw_acquisition_server_catalog(),
    class = "pipdata_dlw_catalog_load_error"
  )

  testthat::local_mocked_bindings(
    get_pip_folders = function(...) list(dlw_inventory = tempdir()),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .package = "pipdata"
  )
  expect_error(
    dlw_gmd_list(),
    class = "pipdata_dlw_catalog_load_error"
  )
})

test_that("candidate selection is pure and honors retry and five modules", {
  server <- .normalize_dlw_acquisition_catalog(
    make_dlw_catalog(),
    source = "server"
  )
  prior <- .normalize_dlw_acquisition_catalog(
    make_dlw_prior_inventory(),
    source = "local"
  )
  server_before <- data.table::copy(server)
  prior_before <- data.table::copy(prior)

  retry <- .select_dlw_acquisition_candidates(
    server,
    prior,
    check_missing = TRUE
  )
  no_retry <- .select_dlw_acquisition_candidates(
    server,
    prior,
    check_missing = FALSE
  )

  expect_setequal(
    retry$FileName,
    c(
      "ZWE_2021_PICES.dta", "COL_2022_GEIH.dta",
      "PER_2023_ENAHO.dta", "BRA_2020_PNAD.dta"
    )
  )
  expect_setequal(
    no_retry$FileName,
    c(
      "COL_2022_GEIH.dta", "PER_2023_ENAHO.dta",
      "BRA_2020_PNAD.dta"
    )
  )
  expect_false(any(retry$Module %in% c("ASPIRE", "L")))
  expect_identical(server, server_before)
  expect_identical(prior, prior_before)
})

test_that("authoritative merge handles current stale changed and ASPIRE rows", {
  server <- .normalize_dlw_acquisition_catalog(
    make_dlw_catalog(),
    source = "server"
  )
  prior <- .normalize_dlw_acquisition_catalog(
    make_dlw_prior_inventory(),
    source = "local"
  )
  server_before <- data.table::copy(server)
  prior_before <- data.table::copy(prior)
  worker_results <- data.table::data.table(
    FileName = c("ZWE_2021_PICES.dta", "COL_2022_GEIH.dta"),
    data_available = c("No", "Yes")
  )

  intended <- .merge_dlw_acquisition_inventory(
    server,
    prior,
    worker_results
  )

  expect_equal(intended[FileName == "BOL_2020_EH.dta", data_available], "Yes")
  expect_equal(intended[FileName == "ZWE_2021_PICES.dta", data_available], "No")
  expect_equal(intended[FileName == "COL_2022_GEIH.dta", data_available], "Yes")
  expect_equal(intended[FileName == "ARG_2019_EPH.dta", data_available], "Yes")
  expect_false("MEX_2018_ENIGH.dta" %in% intended$FileName)
  expect_false("URY_2017_ECH.dta" %in% intended$FileName)
  expect_false("old-checksum" %in% intended$Checksum)
  expect_equal(
    intended[FileName == "COL_2022_GEIH.dta", server_note],
    "current-3"
  )
  expect_identical(
    anyDuplicated(toupper(fs::path_file(intended$FileName))),
    0L
  )
  expect_identical(server, server_before)
  expect_identical(prior, prior_before)
})

test_that("zero-worker merge still applies authoritative catalog changes", {
  server <- .normalize_dlw_acquisition_catalog(
    make_dlw_catalog(),
    source = "server"
  )
  prior <- .normalize_dlw_acquisition_catalog(
    make_dlw_prior_inventory(),
    source = "local"
  )
  intended <- .merge_dlw_acquisition_inventory(
    server,
    prior,
    data.table::data.table(
      FileName = character(),
      data_available = character()
    )
  )

  expect_false("URY_2017_ECH.dta" %in% intended$FileName)
  expect_false("old-checksum" %in% intended$Checksum)
  expect_equal(intended[FileName == "COL_2022_GEIH.dta", data_available], "No")
  expect_equal(intended[FileName == "ARG_2019_EPH.dta", data_available], "Yes")
})

test_that("stage inventory loading uses arbitrary ID and dlw_inv alias", {
  observed <- list()
  source <- make_dlw_prior_inventory()
  source_before <- data.table::copy(source)

  testthat::local_mocked_bindings(
    pip_read = function(id, alias, verbose, ...) {
      observed <<- list(id = id, alias = alias, verbose = verbose)
      source
    },
    .package = "pipload"
  )

  loaded <- .load_dlw_acquisition_inventory("custom_gmd_inv", verbose = TRUE)
  loaded[, data_available := "No"]

  expect_identical(
    observed,
    list(id = "custom_gmd_inv", alias = "dlw_inv", verbose = TRUE)
  )
  expect_identical(source, source_before)
})

test_that("acquisition persistence reconciles uncertain utility writes", {
  intended <- .normalize_dlw_acquisition_catalog(
    make_dlw_prior_inventory(),
    source = "local"
  )
  prior <- data.table::copy(intended)
  prior[1L, data_available := "No"]
  write_calls <- 0L

  testthat::local_mocked_bindings(
    pip_write = function(...) {
      write_calls <<- write_calls + 1L
      list(version_id = NULL, skipped = FALSE)
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    .reload_dlw_acquisition_inventory_state = function(...) {
      list(state = "present", value = intended, version_id = "recovered-v2")
    },
    .package = "pipdata"
  )

  persisted <- .persist_dlw_acquisition_inventory(
    intended = intended,
    prior = prior,
    id = "custom_gmd_inv",
    verbose = FALSE
  )

  expect_equal(write_calls, 1L)
  expect_identical(persisted$fact$success, TRUE)
  expect_identical(persisted$fact$reconciled, TRUE)
  expect_identical(persisted$fact$version_id, "recovered-v2")
})

test_that("retained update utility aborts when durable intent is unverified", {
  catalog <- make_dlw_catalog()
  prior <- make_dlw_prior_inventory()

  testthat::local_mocked_bindings(
    get_pip_folders = function(...) list(dlw_inventory = tempdir()),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    .load_dlw_acquisition_inventory = function(...) prior,
    .load_dlw_acquisition_server_catalog = function(...) catalog,
    .persist_dlw_acquisition_inventory = function(...) {
      list(
        value = prior,
        fact = list(success = FALSE, trustworthy = TRUE)
      )
    },
    .package = "pipdata"
  )

  expect_error(
    dlw_gmd_new(check_missing = TRUE, update_inventory = TRUE),
    class = "pipdata_dlw_inventory_save_error"
  )
})

test_that("dlw_gmd_unvalidated compares against completed rows only", {
  acquisition <- make_dlw_catalog()[1:2]
  acquisition[, `:=`(Ext = "dta", data_available = "Yes")]
  acquisition[, FileName := c(
    "BOL_2020_EH_V01_M_V01_A_GMD_ALL.dta",
    "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL.dta"
  )]
  completed <- make_compare_validation_inventory()
  completed[, survey_id := "BOL_2020_EH_V01_M_V01_A_GMD_ALL"]
  completed[, Checksum := acquisition$Checksum[[1L]]]

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_inventory = tempdir(), dlw_metadata = tempdir()
    ),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(id, ...) {
      expect_identical(id, "dlw_gmd_inv")
      acquisition
    },
    .load_current_dlw_validation_inventory = function(...) completed,
    .package = "pipdata"
  )

  candidates <- withVisible(dlw_gmd_unvalidated())
  expect_false(candidates$visible)
  expect_identical(
    candidates$value$FileName,
    "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL.dta"
  )
})

test_that("strict DLW version catalogs fail on warnings and malformed empties", {
  testthat::local_mocked_bindings(
    st_versions = function(...) {
      warning("Dropped corrupt version row with invalid created_at.")
      data.table::data.table(version_id = "hidden-version")
    },
    .package = "stamp"
  )
  expect_error(
    .strict_dlw_versions("gmd_valid_inv.qs2", "dlw_meta"),
    class = "pipdata_dlw_version_catalog_error"
  )

  expect_error(
    .normalize_dlw_validation_inventory(
      data.table::data.table(other = character())
    ),
    class = "pipdata_dlw_inventory_schema_error"
  )
  expect_identical(
    .normalize_dlw_validation_inventory(
      data.table::data.table(other = character()),
      allow_schema_light_empty = TRUE
    ),
    .empty_dlw_validation_inventory()
  )
})

test_that("dlw_gmd_list validates its ID before any storage operation", {
  touched <- FALSE
  testthat::local_mocked_bindings(
    get_pip_folders = function(...) {
      touched <<- TRUE
      rlang::abort("must not run")
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    dlw_server_catalog = function(...) {
      touched <<- TRUE
      rlang::abort("must not run")
    },
    .package = "dlw"
  )

  expect_error(dlw_gmd_list(""), class = "pipdata_dlw_argument_error")
  expect_false(touched)
})
