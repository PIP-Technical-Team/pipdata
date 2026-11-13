test_that("public metadata reconstruction and refresh select exact artifacts", {
  root <- withr::local_tempdir(pattern = "pm-")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  state <- list(pipfun = getFromNamespace(".pipenv", "pipfun"),
    pipaux = getFromNamespace(".pipaux", "pipaux"),
    aliases = getFromNamespace(".stamp_aliases", "stamp"),
    stamp = getFromNamespace(".stamp_state", "stamp"))
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
  withr::local_options(list(pipfun.main_dir = root, pipfun.log.auto = FALSE,
    pipfun.verbose = FALSE, pipload.verbose = FALSE, pipaux.working_dir = root,
    pipdata.dependency_manifest_path = file.path(root, "manifest")))
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
  # Supporting country/pop/GDP/PCE values are declared fixtures.
  for (measure in c("pop", "gdp", "pce")) {
    value <- data.table::data.table(country_code = "AAA", year = 2010L,
                                     reporting_level = "national")
    data.table::set(value, j = paste0(measure, "_data_level"), value = "national")
    data.table::set(value, j = measure,
      value = switch(measure, pop = 1000, gdp = 3500, pce = 2400))
    pipload::pip_write(value, measure, alias = "aux",
      pk = c("country_code", "year", "reporting_level"), verbose = FALSE)
  }
  pipaux::aux_cpi(action = "update", alias = "aux", detail = FALSE, verbose = FALSE)
  pipaux::aux_ppp(action = "update", alias = "aux", detail = FALSE, verbose = FALSE)
  pipaux::aux_pfw(action = "update", alias = "aux", detail = FALSE, verbose = FALSE)
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
  inv <- data.table::copy(validation$validation$inventory)
  before <- data.table::copy(inv)
  loads <- list()
  observe <- function(id_name, country_code, version, metadata) {
    loads[[length(loads) + 1L]] <<- list(id_name = id_name,
      country_code = country_code, version = version, metadata = metadata)
  }
  expect_request <- function(request, metadata, version) {
    expect_identical(request$id_name, id$pip_id)
    expect_null(request$country_code)
    expect_identical(request$metadata, metadata)
    expect_identical(request$version, version)
  }
  # Observe the real exported loader; do not replace its return or any worker.
  trace("load_pip_data", tracer = bquote(.(observe)(id_name, country_code, version, metadata)),
    where = asNamespace("pipload"), print = FALSE)
  withr::defer(untrace("load_pip_data", where = asNamespace("pipload")))
  first <- pipdata::pd_process_data(inv = inv, bootstrap = TRUE, verbose = FALSE)
  expect_identical(nrow(first), 1L)
  expect_identical(first$welfare_type, "income")
  expect_length(loads, 1L)
  expect_request(loads[[1L]], FALSE, first$version_id_data[[1L]])
  expect_identical(loads[[1L]], list(id_name = id$pip_id, country_code = NULL,
    version = first$version_id_data[[1L]], metadata = FALSE))
  original_metadata <- pipload::pip_read(id$pip_id, version = first$version_id_metadata[[1L]],
                                         alias = "pip_meta", verbose = FALSE)

  # Change one survey-year CPI coefficient through the real public producer.
  aux_raw$cpi[year == 2010L, cpi2017 := cpi2017 * 1.1]
  gh <- attr(aux_raw$cpi, "gh")
  gh$gh_raw_sha <- "synthetic-cpi-raw-v2"
  data.table::setattr(aux_raw$cpi, "gh", gh)
  pipaux::aux_cpi(action = "update", alias = "aux", detail = FALSE, verbose = FALSE)
  second <- pipdata::pd_process_data(inv = inv, bootstrap = TRUE, verbose = FALSE)
  expect_length(loads, 2L)
  expect_request(loads[[2L]], TRUE, first$version_id_metadata[[1L]])
  expect_identical(loads[[2L]], list(id_name = id$pip_id, country_code = NULL,
    version = first$version_id_metadata[[1L]], metadata = TRUE))
  expect_identical(second$version_id_data, first$version_id_data)
  expect_false(identical(second$version_id_metadata, first$version_id_metadata))
  exact_old <- pipload::pip_read(id$pip_id, version = first$version_id_metadata[[1L]],
                                 alias = "pip_meta", verbose = FALSE)
  expect_identical(exact_old, original_metadata)
  expect_identical(inv, before)
  expect_identical(second$welfare_type, "income")
})
