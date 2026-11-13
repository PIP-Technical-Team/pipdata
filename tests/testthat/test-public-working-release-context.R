# Expected behavior: setup_working_release() supplies the working context used
# by public processing. Only external release/PPP transport is substituted.
test_that("public setup supplies processing context without hidden options", {
  root <- withr::local_tempdir(pattern = "public-context-TEST-")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  state <- list(
    pipfun = getFromNamespace(".pipenv", "pipfun"),
    aliases = getFromNamespace(".stamp_aliases", "stamp"),
    stamp = getFromNamespace(".stamp_state", "stamp")
  )
  previous <- lapply(state, as.list)
  withr::defer({
    for (key in names(state)) {
      rm(list = ls(state[[key]], all.names = TRUE), envir = state[[key]])
      list2env(previous[[key]], envir = state[[key]])
    }
  })
  rm(list = ls(state$pipfun, all.names = TRUE), envir = state$pipfun)
  rm(list = ls(state$aliases, all.names = TRUE), envir = state$aliases)
  withr::local_options(list(
    pipfun.main_dir = root, pipfun.verbose = FALSE,
    pipdata.dependency_manifest_path = file.path(root, "manifest")
  ))
  withr::local_envvar(PIP_ROOT_DIR = file.path(root, "unused-ambient-root"))
  testthat::local_mocked_bindings(
    get_file_from_gh = function(...) {
      data.table::data.table(release = "20260401", identity = "TEST")
    },
    download_from_gh = function(path, file) {
      stopifnot(grepl("/ppp_vintage.csv$", path))
      data.table::fwrite(data.table::data.table(
        ppp_year = 2021L, ppp_rv = "01", ppp_av = "01"
      ), file)
    },
    get_file_info_from_gh = function(...) {
      list(sha = "synthetic-ppp-vintage-v1")
    },
    .package = "pipfun"
  )
  release <- pipfun::setup_working_release(
    "20260401", "TEST", ppp = 2021L, verbose = FALSE, root_dir = root
  )
  expect_false(dir.exists(Sys.getenv("PIP_ROOT_DIR")))
  expect_identical(release$release, "20260401")
  expect_identical(release$identity, "TEST")
  expect_null(getOption("pipfun.working_release"))
  expect_null(getOption("pipfun.identity"))
  expect_equal(nrow(stamp::st_versions(
    "pip_master_inventory.qs2", alias = "pip_master"
  )), 0L)

  inv <- pipload::survey_id_to_vars(data.table::data.table(
    survey_id = "AAA_2010_SYNA_V01_M_V01_A_GMD_GPWG"
  ))
  inv[, `:=`(
    pipeline_version = 1L, latest_version_id = "validated-v1",
    content_hash = "validated-h1", file_path = "validated.qs2",
    status = "valid", data_available = "Yes",
    date_validated = as.POSIXct("2026-11-11 00:00:00", tz = "UTC"),
    Checksum = "fixture-checksum"
  )]
  observed <- new.env(parent = emptyenv())
  observed$context <- NULL
  observe <- function(value) observed$context <- value
  trace(
    "pd_dependency_context", exit = bquote(.(observe)(returnValue())),
    where = asNamespace("pipdata"), print = FALSE
  )
  withr::defer(untrace(
    "pd_dependency_context", where = asNamespace("pipdata")
  ))
  # No aux data is supplied: require the specific fixture boundary, not success
  # or an arbitrary error. The trace only observes the real context return.
  expect_error(
    pipdata::pd_process_data(inv = inv, bootstrap = TRUE, verbose = FALSE),
    class = "pipdata_dependency_aux_missing"
  )
  expect_identical(observed$context$release, release$release)
  expect_identical(observed$context$identity, release$identity)
  expect_identical(
    observed$context$roots$pip_master,
    pd_normalize_path(pipfun::get_pip_folders(
      "pip_master_inventory", verbose = FALSE
    ))
  )
  expect_equal(nrow(stamp::st_versions(
    "pip_master_inventory.qs2", alias = "pip_master"
  )), 0L)
})
