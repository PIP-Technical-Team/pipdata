public_clean_welfare_observe_error <- function(error, observer) {
  on.exit(stop(error))
  tryCatch(observer(), error = function(observation_error) {
    rlang::inform(paste("Boundary observation failed:",
                        conditionMessage(observation_error)))
  })
}

public_clean_welfare_case <- function(inventory_welfare = NULL) {
  root <- withr::local_tempdir(pattern = "pw-")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  state <- list(
    pipfun = getFromNamespace(".pipenv", "pipfun"),
    pipaux = getFromNamespace(".pipaux", "pipaux"),
    aliases = getFromNamespace(".stamp_aliases", "stamp"),
    stamp = getFromNamespace(".stamp_state", "stamp")
  )
  previous <- lapply(state, as.list)
  aux_branch <- paste0(previous$pipaux$wrk_release$release, "_",
                       previous$pipaux$wrk_release$identity)
  withr::defer({
    for (key in names(state)) {
      rm(list = ls(state[[key]], all.names = TRUE), envir = state[[key]])
      list2env(previous[[key]], envir = state[[key]])
    }
  })
  rm(list = ls(state$pipfun, all.names = TRUE), envir = state$pipfun)
  rm(list = ls(state$aliases, all.names = TRUE), envir = state$aliases)
  withr::local_options(list(
    pipfun.main_dir = root, pipfun.log.auto = FALSE, pipfun.verbose = FALSE,
    pipload.verbose = FALSE, pipaux.working_dir = root,
    pipdata.dependency_manifest_path = file.path(root, "manifest")
  ))
  withr::local_envvar(c(PIP_ROOT_DIR = root, DLW_local_dir = file.path(root, "dlw")))
  raw <- upstream_acceptance_raw()
  aux_raw <- upstream_acceptance_aux_raw()
  id <- upstream_acceptance_identity()
  testthat::local_mocked_bindings(
    get_file_from_gh = function(...) {
      data.table::data.table(release = "20261103", identity = "TEST")
    },
    download_from_gh = function(path, file) {
      stopifnot(grepl("/ppp_vintage.csv$", path))
      data.table::fwrite(data.table::data.table(ppp_year = c(2017L, 2021L),
        ppp_rv = "01", ppp_av = "02"), file)
    },
    get_file_info_from_gh = function(...) list(sha = "synthetic-ppp-vintage-v1"),
    load_from_gh = function(measure, branch, filename = measure, ...) {
      if (identical(measure, "ppp") && identical(filename, "ppp_vintage")) {
        stopifnot(identical(branch, "DEV"))
        return(data.table::data.table(ppp_year = c(2017L, 2021L),
          ppp_rv = "01", ppp_av = "02"))
      }
      stopifnot(measure %in% names(aux_raw), identical(branch, aux_branch))
      data.table::copy(aux_raw[[measure]])
    }, .package = "pipfun"
  )
  pipfun::setup_working_release("20261103", "TEST", ppp = 2017L,
    root_dir = root, verbose = FALSE)
  pipload::pip_write(data.table::data.table(country_code = "AAA", region_code = "SSA"),
    "country_list", alias = "aux", pk = "country_code", verbose = FALSE)
  # Supporting inputs are declared fixtures, not certified auxiliary producers.
  for (measure in c("pop", "gdp", "pce")) {
    value <- data.table::data.table(country_code = "AAA", year = 2010L,
                                     reporting_level = "national")
    data.table::set(value, j = paste0(measure, "_data_level"), value = "national")
    data.table::set(value, j = measure,
      value = switch(measure, pop = 1000, gdp = 3500, pce = 2400))
    pipload::pip_write(value, measure, alias = "aux",
      pk = c("country_code", "year", "reporting_level"), verbose = FALSE)
  }
  # Public wrappers use their cached release; transport observes that branch.
  # Explicit save aliases keep every producer write in this fixture.
  pipaux::aux_cpi(action = "update", alias = "aux",
                   detail = FALSE, verbose = FALSE)
  pipaux::aux_ppp(action = "update", alias = "aux",
                   detail = FALSE, verbose = FALSE)
  pipaux::aux_pfw(action = "update", alias = "aux",
                   detail = FALSE, verbose = FALSE)
  catalog <- data.table::data.table(Country = "AAA", Year = 2010L,
    Survey_acronym = "SYNA", Vermast = "V01", Veralt = "V01", Module = "GPWG",
    Collection = "GMD", FileName = id$file, Checksum = "synthetic-aaa-2010-syna-v1",
    Ext = "dta")
  testthat::local_mocked_bindings(
    dlw_server_catalog = function(...) data.table::copy(catalog),
    dlw_get_gmd = function(filename, ...) {
      stopifnot(identical(filename, id$file))
      pipload::pip_write(data.table::copy(raw), tolower(id$survey_id), alias = "dlw",
        pk = c("hhid", "pid"), verbose = FALSE)
      invisible(TRUE)
    }, .package = "dlw"
  )
  pipdata::dlw_gmd_list()
  validation <- pipdata::pipdata_dlw_process(release = "20261103", identity = "TEST",
                                             verbose = FALSE)
  expect_identical(validation$outcome, "success")
  expect_identical(validation$validation$inventory$status, "valid")
  inv <- data.table::copy(validation$validation$inventory)
  if (!is.null(inventory_welfare)) inv[, welfare_type := inventory_welfare]
  before <- data.table::copy(inv)
  check_saved_boundary <- function() {
    master <- pipload::pip_read("pip_master_inventory", alias = "pip_master",
                                 verbose = FALSE)
    clean <- pipload::pip_read(id$pip_id, version = master$version_id_data[[1L]],
                                alias = "pip", verbose = FALSE)
    expect_identical(attr(clean, "welfare_type", exact = TRUE), "income")
    expect_identical(master$welfare_type, "income")
    expect_identical(inv, before)
  }
  # Capture only the known strict key condition so its presence is an assertion
  # failure. Other errors propagate; no error is accepted as public success.
  processed <- tryCatch(
    pipdata::pd_process_data(inv = inv, bootstrap = TRUE, verbose = FALSE),
    pipdata_dependency_input_missing = identity,
    error = function(error) {
      # Observe the durable boundary even if a later stage fails, then preserve
      # that original error. A downstream error never becomes test success.
      public_clean_welfare_observe_error(error, check_saved_boundary)
    }
  )
  check_saved_boundary()
  expect_false(inherits(processed, "condition"))
}

test_that("public clean receipts propagate trusted welfare type to fresh master", {
  public_clean_welfare_case()
})

test_that("public clean welfare type overrides conflicting inventory input", {
  public_clean_welfare_case("consumption")
})

test_that("failed boundary observation preserves the original processing error", {
  primary <- simpleError("original processing failure")
  error <- NULL
  expect_message(
    error <- tryCatch(
      public_clean_welfare_observe_error(primary, function() stop("readback failure")),
      error = identity
    ),
    "Boundary observation failed: readback failure"
  )
  expect_identical(error, primary)
})
