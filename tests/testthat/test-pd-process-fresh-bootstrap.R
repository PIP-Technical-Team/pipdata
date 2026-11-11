# Expected-behavior source: user-requirement. Explicit first-run bootstrap
# must cross the absent-master boundary with empty state, not populated history.
# Observe entry only: all workers and the public master reader remain real.
test_that("fresh bootstrap reaches preparation with genuinely empty master", {
  root <- withr::local_tempdir(pattern = "fresh-bootstrap-TEST-")
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
  assign("folder_paths", list(pip_master_inventory = root), state$pipfun)
  stamp::st_init(root, alias = "pip_master", verbose = FALSE)
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
    date_validated = as.POSIXct("2026-11-10 00:00:00", tz = "UTC"),
    Checksum = "fixture-checksum"
  )]
  observed <- new.env(parent = emptyenv())
  observed$entered <- FALSE
  observer <- function(frame) {
    observed$entered <- TRUE
    observed$master <- get("master", envir = frame, inherits = FALSE)
    observed$bootstrap <- get("bootstrap", envir = frame, inherits = FALSE)
  }
  withr::local_options(list(pipdata.fresh_bootstrap_observer = observer))
  trace(
    "pd_prepare_execution",
    tracer = quote(getOption("pipdata.fresh_bootstrap_observer")(environment())),
    where = asNamespace("pipdata"), print = FALSE
  )
  withr::defer(untrace(
    "pd_prepare_execution", where = asNamespace("pipdata")
  ))
  result <- tryCatch(
    pipdata::pd_process_data(inv = inv, bootstrap = TRUE, verbose = FALSE),
    error = identity
  )
  # No release/aux fixture is supplied: later pipeline acceptance is NOT tested.
  diagnostic <- if (inherits(result, "error")) conditionMessage(result) else ""
  expect_true(observed$entered, info = diagnostic)
  if (isTRUE(observed$entered)) {
    expect_true(is.data.frame(observed$master))
    expect_equal(nrow(observed$master), 0L)
    expect_identical(observed$bootstrap, TRUE)
  }
  expect_false(grepl(
    "No version files found in 'pip_master_inventory.qs2'",
    diagnostic, fixed = TRUE
  ), info = diagnostic)
  expect_equal(nrow(stamp::st_versions(
    "pip_master_inventory.qs2", alias = "pip_master"
  )), 0L)
})

test_that("existing history uses the normal master reader during bootstrap", {
  root <- "fixture/master"
  master <- data.table::data.table(survey_id = "S1", pip_id = "P1")
  observed <- NULL
  testthat::local_mocked_bindings(
    get_pip_folders = function(folder, verbose) root,
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_alias_list = function() data.frame(alias = "configured_master", root),
    st_versions = function(path, alias) {
      observed <<- list(path = path, alias = alias)
      data.table::data.table(version_id = "existing-version")
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(verbose) {
      expect_identical(verbose, TRUE)
      master
    },
    .package = "pipload"
  )

  expect_identical(pd_load_process_master(TRUE, TRUE), master)
  expect_identical(observed, list(
    path = "pip_master_inventory.qs2", alias = "configured_master"
  ))
})

test_that("non-bootstrap loading never treats missing history as empty", {
  testthat::local_mocked_bindings(
    get_pip_folders = function(...) stop("unexpected bootstrap history lookup"),
    .package = "pipfun"
  )
  failure <- rlang::error_cnd("fixture_master_read_failure", message = "read failed")
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) stop(failure),
    .package = "pipload"
  )
  expect_identical(tryCatch(pd_load_process_master(FALSE), error = identity),
                   failure)
})

test_that("bootstrap propagates version listing and existing master read errors", {
  root <- "fixture/master"
  failure <- rlang::error_cnd("fixture_master_read_failure", message = "read failed")
  testthat::local_mocked_bindings(
    get_pip_folders = function(...) root, .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) stop(failure),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_alias_list = function() data.frame(alias = "configured_master", root),
    st_versions = function(...) stop(failure), .package = "stamp"
  )
  expect_identical(tryCatch(pd_load_process_master(TRUE), error = identity),
                   failure)
  testthat::with_mocked_bindings({
    expect_identical(tryCatch(pd_load_process_master(TRUE), error = identity),
                     failure)
  }, st_versions = function(...) data.table::data.table(version_id = "v1"),
  .package = "stamp")
})

test_that("catalog warnings cannot turn damaged history into bootstrap absence", {
  root <- "fixture/master"
  corruption <- rlang::warning_cnd(
    "fixture_catalog_corruption", message = "Dropped corrupt version row"
  )
  testthat::local_mocked_bindings(
    get_pip_folders = function(...) root, .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_alias_list = function() data.frame(alias = "configured_master", root),
    st_versions = function(...) {
      warning(corruption)
      data.table::data.table(version_id = character())
    },
    .package = "stamp"
  )
  failure <- tryCatch(pd_load_process_master(TRUE), error = identity)
  expect_s3_class(failure, "pipdata_master_history_error")
  expect_identical(failure$parent, corruption)
})

test_that("bootstrap does not hide an uninitialized master alias", {
  root <- "fixture/master"
  failure <- rlang::error_cnd("fixture_master_setup_failure", message = "no alias")
  testthat::local_mocked_bindings(
    get_pip_folders = function(...) root, .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_alias_list = function() data.frame(alias = character(), root = character()),
    st_versions = function(...) stop("unexpected uninitialized alias query"),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) stop(failure), .package = "pipload"
  )
  expect_identical(tryCatch(pd_load_process_master(TRUE), error = identity),
                   failure)
})

test_that("surviving master storage contradicts apparently absent history", {
  root <- withr::local_tempdir(pattern = "m")
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
  rm(list = ls(state$aliases, all.names = TRUE), envir = state$aliases)
  assign("folder_paths", list(pip_master_inventory = root), state$pipfun)
  stamp::st_init(root, alias = "pip_master", verbose = FALSE)
  empty <- data.table::data.table(survey_id = character(), pip_id = character())
  pipload::pip_write(empty, "pip_master_inventory", alias = "pip_master",
                     verbose = FALSE)
  catalog <- file.path(stamp::st_alias_get("pip_master")$stamp_path, "catalog.qs2")
  fs::file_move(catalog, paste0(catalog, ".retained"))
  expect_equal(nrow(stamp::st_versions(
    "pip_master_inventory.qs2", alias = "pip_master"
  )), 0L)
  expect_true(fs::dir_exists(file.path(root, "pip_master_inventory.qs2")))
  expect_error(pd_load_process_master(TRUE), class = "pipdata_master_history_error")
})
