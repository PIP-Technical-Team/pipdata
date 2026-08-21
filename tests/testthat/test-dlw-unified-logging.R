make_dlw_logging_folders <- function() {
  root <- tempfile("pipdata-dlw-logging-")
  fs::dir_create(root)
  list(
    root = root,
    folders = list(
      dlw_data = root,
      dlw_inventory = root,
      dlw_metadata = root
    )
  )
}

make_dlw_acquisition_row <- function(
    module = "ALL",
    file_name = "BOL_2020_EH.dta"
) {
  data.table::data.table(
    Country = "BOL",
    Year = 2020L,
    Survey_acronym = "EH",
    Vermast = "01",
    Veralt = "01",
    Module = module,
    Collection = "GMD",
    FileName = file_name,
    Checksum = "checksum-1",
    data_available = "No"
  )
}

test_that("DLW public wrappers no longer expose log controls", {
  expect_false("log" %in% names(formals(pipdata_get_gmd)))
  expect_false("save_log" %in% names(formals(pipdata_get_gmd)))
  expect_false("log" %in% names(formals(pipdata_validate_gmd)))
  expect_false("save_log" %in% names(formals(pipdata_validate_gmd)))
  expect_false("log" %in% names(formals(pipdata_dlw_process)))
  expect_false("save_log" %in% names(formals(pipdata_dlw_process)))
})

test_that("removed logging arguments are rejected at the call boundary", {
  expect_error(pipdata_get_gmd(log = TRUE), "unused argument")
  expect_error(pipdata_validate_gmd(save_log = TRUE), "unused argument")
  expect_error(pipdata_dlw_process(log = TRUE), "unused argument")
})

test_that("acquisition no-op is logged and returns invisible NULL", {
  fixture <- make_dlw_logging_folders()
  events <- list()

  testthat::local_mocked_bindings(
    get_pip_folders = function(...) fixture$folders,
    log_info = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(message = message, logmeta = logmeta)))
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    dlw_gmd_new = function(...) data.table::data.table(),
    .package = "pipdata"
  )

  result <- withVisible(pipdata_get_gmd(verbose = FALSE))

  expect_null(result$value)
  expect_false(result$visible)
  expect_length(events, 1L)
  expect_equal(events[[1]]$logmeta$info, "dlw_acquisition_inf")
  expect_equal(events[[1]]$logmeta$phase, "no_new_data")
  expect_equal(events[[1]]$logmeta$n_surveys, 0L)
})

test_that("acquisition logs typed download failures and completion counts", {
  fixture <- make_dlw_logging_folders()
  events <- list()
  written <- list()
  gmd <- make_dlw_acquisition_row()

  testthat::local_mocked_bindings(
    get_pip_folders = function(...) fixture$folders,
    log_info = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(event = "info", logmeta = logmeta)))
      invisible(TRUE)
    },
    log_error = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(event = "error", logmeta = logmeta)))
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    dlw_gmd_new = function(...) gmd,
    dlw_gmd_match = function(...) data.table::data.table(),
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    dlw_get_gmd = function(...) rlang::abort("download failed"),
    .package = "dlw"
  )
  testthat::local_mocked_bindings(
    pip_write = function(x, id, ...) {
      written <<- list(x = x, id = id)
      list(version_id = paste0(id, "_version"), skipped = FALSE)
    },
    .package = "pipload"
  )

  result <- withVisible(pipdata_get_gmd(verbose = FALSE))

  expect_null(result$value)
  expect_false(result$visible)
  expect_equal(written$id, "dlw_gmd_inv")
  expect_equal(written$x$data_available, "No")

  error_entries <- Filter(
    function(x) identical(x$event, "error"),
    events
  )
  expect_length(error_entries, 1L)
  expect_equal(error_entries[[1]]$logmeta$error, "dlw_acquisition_inf")
  expect_equal(error_entries[[1]]$logmeta$phase, "download")
  expect_equal(error_entries[[1]]$logmeta$survey, "BOL_2020_EH")
  expect_equal(error_entries[[1]]$logmeta$condition_msg, "download failed")

  complete_entries <- Filter(
    function(x) identical(x$event, "info") &&
      identical(x$logmeta$phase, "complete"),
    events
  )
  expect_length(complete_entries, 1L)
  expect_equal(complete_entries[[1]]$logmeta$n_surveys, 1L)
  expect_equal(complete_entries[[1]]$logmeta$n_success, 0L)
  expect_equal(complete_entries[[1]]$logmeta$n_failed, 1L)
})

test_that("acquisition logs persistence results without a version", {
  fixture <- make_dlw_logging_folders()
  events <- list()
  gmd <- make_dlw_acquisition_row()

  testthat::local_mocked_bindings(
    get_pip_folders = function(...) fixture$folders,
    log_info = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(event = "info", logmeta = logmeta)))
      invisible(TRUE)
    },
    log_error = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(event = "error", logmeta = logmeta)))
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    dlw_gmd_new = function(...) gmd,
    dlw_gmd_match = function(...) data.table::data.table(),
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    dlw_get_gmd = function(...) invisible(TRUE),
    .package = "dlw"
  )
  testthat::local_mocked_bindings(
    pip_write = function(...) list(version_id = NULL, skipped = FALSE),
    .package = "pipload"
  )

  expect_error(
    pipdata_get_gmd(verbose = FALSE),
    "Failed to save the GMD inventory"
  )
  save_errors <- Filter(
    function(x) identical(x$event, "error") &&
      identical(x$logmeta$phase, "inventory_save"),
    events
  )
  expect_length(save_errors, 1L)
  expect_equal(save_errors[[1]]$logmeta$error, "dlw_acquisition_inf")
})

test_that("validation logs typed load failures and report availability", {
  fixture <- make_dlw_logging_folders()
  events <- list()
  writes <- list()
  gmd <- make_dlw_acquisition_row()
  gmd[, data_available := "Yes"]

  testthat::local_mocked_bindings(
    get_pip_folders = function(...) fixture$folders,
    log_info = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(event = "info", logmeta = logmeta)))
      invisible(TRUE)
    },
    log_error = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(event = "error", logmeta = logmeta)))
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    dlw_gmd_unvalidated = function(...) gmd,
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    is_file = function(...) FALSE,
    .package = "fs"
  )
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) rlang::abort("qs read failed"),
    pip_write = function(x, id, ...) {
      writes <<- c(writes, list(list(x = x, id = id)))
      list(version_id = paste0(id, "_version"), skipped = FALSE)
    },
    survey_id_to_vars = function(x, ...) x,
    .package = "pipload"
  )

  result <- withVisible(pipdata_validate_gmd(verbose = FALSE))

  expect_null(result$value)
  expect_false(result$visible)
  expect_equal(writes[[1]]$id, "gmd_valid_inv")
  expect_equal(writes[[1]]$x$data_available, "No")

  load_errors <- Filter(
    function(x) identical(x$event, "error") &&
      identical(x$logmeta$phase, "load"),
    events
  )
  expect_length(load_errors, 1L)
  expect_equal(load_errors[[1]]$logmeta$error, "dlw_validation_inf")
  expect_equal(load_errors[[1]]$logmeta$survey, "BOL_2020_EH")
  expect_equal(load_errors[[1]]$logmeta$condition_msg, "qs read failed")

  inventory_entries <- Filter(
    function(x) identical(x$event, "info") &&
      identical(x$logmeta$phase, "inventory_save"),
    events
  )
  expect_length(inventory_entries, 1L)
  expect_equal(inventory_entries[[1]]$logmeta$n_load_failed, 1L)

  report_errors <- Filter(
    function(x) identical(x$event, "error") &&
      identical(x$logmeta$phase, "report_unavailable"),
    events
  )
  expect_length(report_errors, 1L)
  expect_equal(report_errors[[1]]$logmeta$error, "dlw_validation_inf")
})

test_that("DLW wrapper emits summary and checkpoint even when both delegates are disabled", {
  fixture <- make_dlw_logging_folders()
  events <- list()
  checkpoints <- list()

  withr::local_options(pipfun.main_dir = fixture$root)
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(NULL),
    get_wrk_release = function(...) invisible(NULL),
    get_pip_folders = function(...) fixture$folders,
    log_info = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(message = message, logmeta = logmeta)))
      invisible(TRUE)
    },
    log_save_checkpoint = function(name, stage, alias, ...) {
      checkpoints <<- c(
        checkpoints,
        list(list(name = name, stage = stage, alias = alias))
      )
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(NULL),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    is_file = function(...) FALSE,
    .package = "fs"
  )
  testthat::local_mocked_bindings(
    menu = function(...) rlang::abort("menu should not run for a DLW no-op"),
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    pipdata_get_gmd = function(...) rlang::abort("acquisition should not run"),
    pipdata_validate_gmd = function(...) rlang::abort("validation should not run"),
    .package = "pipdata"
  )

  result <- withVisible(pipdata_dlw_process(
    release = "20260206",
    identity = "TEST",
    get_dlw_data = FALSE,
    validate_dlw_data = FALSE,
    verbose = FALSE
  ))

  expect_null(result$value)
  expect_false(result$visible)
  expect_length(checkpoints, 1L)
  expect_equal(
    checkpoints[[1]],
    list(name = "pipdata_log", stage = "dlw", alias = "dlw_meta")
  )

  summary_entries <- Filter(
    function(x) identical(x$logmeta$info, "dlw_summary_inf"),
    events
  )
  expect_length(summary_entries, 1L)
  expect_false(summary_entries[[1]]$logmeta$get_dlw_data)
  expect_false(summary_entries[[1]]$logmeta$validate_dlw_data)
})

test_that("DLW wrapper checkpoints real delegate no-op paths", {
  fixture <- make_dlw_logging_folders()
  events <- list()
  checkpoints <- list()

  withr::local_options(pipfun.main_dir = fixture$root)
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(NULL),
    get_wrk_release = function(...) invisible(NULL),
    get_pip_folders = function(...) fixture$folders,
    log_info = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(event = "info", logmeta = logmeta)))
      invisible(TRUE)
    },
    log_error = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(list(event = "error", logmeta = logmeta)))
      invisible(TRUE)
    },
    log_save_checkpoint = function(name, stage, alias, ...) {
      checkpoints <<- c(
        checkpoints,
        list(list(name = name, stage = stage, alias = alias))
      )
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(NULL),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    is_file = function(...) TRUE,
    .package = "fs"
  )
  testthat::local_mocked_bindings(
    dlw_gmd_new = function(...) data.table::data.table(),
    dlw_gmd_unvalidated = function(...) data.table::data.table(),
    .package = "pipdata"
  )

  pipdata_dlw_process(
    release = "20260206",
    identity = "TEST",
    get_dlw_data = TRUE,
    validate_dlw_data = FALSE,
    verbose = FALSE
  )
  pipdata_dlw_process(
    release = "20260206",
    identity = "TEST",
    get_dlw_data = FALSE,
    validate_dlw_data = TRUE,
    verbose = FALSE
  )

  expect_length(checkpoints, 2L)
  expect_true(any(vapply(
    events,
    function(x) identical(x$logmeta$phase, "no_new_data") &&
      identical(x$logmeta$info, "dlw_acquisition_inf"),
    logical(1)
  )))
  expect_true(any(vapply(
    events,
    function(x) identical(x$logmeta$phase, "no_new_data") &&
      identical(x$logmeta$info, "dlw_validation_inf"),
    logical(1)
  )))
})

test_that("pipeline checkpoint contract works with the registered piplog alias", {
  root <- tempfile("pipdata-pipeline-checkpoint-")
  fs::dir_create(root)
  stamp::st_init(root = root, alias = "piplog")
  pipfun::log_init("pipeline_checkpoint_test", overwrite = TRUE)
  pipfun::log_info(
    "Processing complete.",
    name = "pipeline_checkpoint_test",
    logmeta = list(
      info = "process_summary_inf",
      n_total = 0L,
      n_success = 0L,
      n_failed = 0L
    )
  )

  pipfun::log_save_checkpoint(
    name = "pipeline_checkpoint_test",
    stage = "pipeline",
    alias = "piplog"
  )

  expect_true(
    fs::file_exists(
      fs::path(root, "pipeline_checkpoint_test_checkpoint_pipeline.qs2")
    )
  )
  checkpoint_info <- stamp::st_info(
    fs::path(root, "pipeline_checkpoint_test_checkpoint_pipeline.qs2"),
    alias = "piplog"
  )
  expect_equal(checkpoint_info$sidecar$stage, "pipeline")
})

test_that("DLW checkpoint persists through the dlw_meta alias", {
  root <- tempfile("pipdata-dlw-checkpoint-")
  fs::dir_create(root)
  stamp::st_clear_builders()
  withr::defer(stamp::st_clear_builders())
  stamp::st_init(root = root, alias = "dlw_meta")
  pipfun::log_init("dlw_checkpoint_test", overwrite = TRUE)
  pipfun::log_info(
    "DLW complete.",
    name = "dlw_checkpoint_test",
    logmeta = list(
      info = "dlw_summary_inf",
      get_dlw_data = TRUE,
      validate_dlw_data = TRUE
    )
  )

  pipfun::log_save_checkpoint(
    name = "dlw_checkpoint_test",
    stage = "dlw",
    alias = "dlw_meta"
  )

  checkpoint_path <- fs::path(
    root,
    "dlw_checkpoint_test_checkpoint_dlw.qs2"
  )
  expect_true(fs::file_exists(checkpoint_path))
  checkpoint_info <- stamp::st_info(checkpoint_path, alias = "dlw_meta")
  expect_equal(checkpoint_info$sidecar$stage, "dlw")
})

test_that("pipeline checkpoint is after the process summary log", {
  source <- paste(
    deparse(pipdata::pd_process_data),
    collapse = "\n"
  )
  summary_pos <- regexpr("info = \"process_summary_inf\"", source, fixed = FALSE)
  checkpoint_pos <- regexpr("log_save_checkpoint", source, fixed = TRUE)
  inventory_pos <- regexpr("new_pip_inv <- build_pip_inventory", source, fixed = TRUE)

  expect_true(summary_pos[[1]] > 0L)
  expect_true(inventory_pos[[1]] > summary_pos[[1]])
  expect_true(checkpoint_pos[[1]] > inventory_pos[[1]])
})
