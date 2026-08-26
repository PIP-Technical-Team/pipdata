wrapper_failure_columns <- c(
  "survey_id", "phase", "error_type", "condition_msg"
)

make_wrapper_empty_failures <- function() {
  data.table::data.table(
    survey_id = character(),
    phase = character(),
    error_type = character(),
    condition_msg = character()
  )
}

make_wrapper_acquisition_result <- function(
    outcome = "no_work",
    inventory = data.table::data.table(id = "durable"),
    trustworthy = TRUE,
    reason = NULL
) {
  if (identical(outcome, "not_run")) {
    return(list(
      stage = "acquisition",
      outcome = "not_run",
      inventory = NULL,
      summary = list(reason = reason),
      failures = make_wrapper_empty_failures(),
      artifacts = list()
    ))
  }
  failed <- identical(outcome, "failed")
  partial <- identical(outcome, "partial")
  success <- identical(outcome, "success") || partial
  failures <- if (failed || partial) {
    data.table::data.table(
      survey_id = NA_character_,
      phase = "catalog_load",
      error_type = "test_error",
      condition_msg = "test acquisition failure"
    )
  } else {
    make_wrapper_empty_failures()
  }
  list(
    stage = "acquisition",
    outcome = outcome,
    inventory = inventory,
    summary = list(
      n_total = if (success) 1L else 0L,
      n_success = if (success) 1L else 0L,
      n_failed = 0L,
      surveys_success = if (success) "ACQ" else character(),
      surveys_failed = character()
    ),
    failures = failures,
    artifacts = list(inventory = list(
      id = "dlw_gmd_inv",
      alias = "dlw_inv",
      attempted = success,
      success = if (success) TRUE else if (failed) FALSE else NA,
      trustworthy = trustworthy,
      version_id = "acq-v1",
      skipped = if (success) TRUE else NA,
      reconciled = FALSE
    ))
  )
}

make_wrapper_validation_result <- function(
    outcome = "no_work",
    reason = NULL
) {
  if (identical(outcome, "not_run")) {
    return(list(
      stage = "validation",
      outcome = "not_run",
      inventory = NULL,
      summary = list(reason = reason),
      failures = make_wrapper_empty_failures(),
      artifacts = list()
    ))
  }
  failed <- identical(outcome, "failed")
  partial <- identical(outcome, "partial")
  success <- identical(outcome, "success") || partial
  failures <- if (failed || partial) {
    data.table::data.table(
      survey_id = NA_character_,
      phase = "report_load_fail",
      error_type = "test_error",
      condition_msg = "test validation failure"
    )
  } else {
    make_wrapper_empty_failures()
  }
  list(
    stage = "validation",
    outcome = outcome,
    inventory = data.table::data.table(id = "validated"),
    summary = list(
      n_total = if (success) 1L else 0L,
      n_valid = if (success) 1L else 0L,
      n_invalid = 0L,
      n_failed = 0L,
      surveys_valid = if (success) "VALID" else character(),
      surveys_invalid = character(),
      surveys_failed = character()
    ),
    failures = failures,
    artifacts = list(
      report = list(
        id = "validation_report", alias = "dlw_meta",
        attempted = success,
        success = if (success) TRUE else if (failed) FALSE else NA,
        trustworthy = TRUE, version_id = "report-v1",
        skipped = if (success) TRUE else NA,
        reconciled = FALSE
      ),
      inventory = list(
        id = "gmd_valid_inv", alias = "dlw_meta",
        attempted = success,
        success = if (success) TRUE else if (failed) FALSE else NA,
        trustworthy = TRUE, version_id = "valid-v1",
        skipped = if (success) TRUE else NA,
        reconciled = FALSE
      )
    )
  )
}

wrapper_valid_args <- function() {
  list(
    inv_gmd_list = "dlw_gmd_inv",
    get_dlw_data = TRUE,
    validate_dlw_data = TRUE,
    check_missing = TRUE,
    release = "20260206",
    identity = "TEST",
    verbose = FALSE
  )
}

test_that("pipdata_dlw_process retains exact public formals and defaults", {
  expect_identical(
    formals(pipdata_dlw_process),
    as.pairlist(alist(
      inv_gmd_list = "dlw_gmd_inv",
      get_dlw_data = TRUE,
      validate_dlw_data = TRUE,
      check_missing = TRUE,
      release = NULL,
      identity = NULL,
      verbose = getOption("pipdata.verbose", default = TRUE)
    ))
  )
})

test_that("wrapper validates every public argument before setup", {
  setup_calls <- 0L
  testthat::local_mocked_bindings(
    setup_working_release = function(...) {
      setup_calls <<- setup_calls + 1L
      rlang::abort("setup should not run")
    },
    .package = "pipfun"
  )

  invalid_values <- list(
    inv_gmd_list = list(NULL, 1, NA_character_, character(), "", c("a", "b")),
    get_dlw_data = list(NULL, 1, NA, logical(), c(TRUE, FALSE)),
    validate_dlw_data = list(NULL, 1, NA, logical(), c(TRUE, FALSE)),
    check_missing = list(NULL, 1, NA, logical(), c(TRUE, FALSE)),
    release = list(NULL, 1, NA_character_, character(), "", c("a", "b")),
    identity = list(NULL, 1, NA_character_, character(), "", "DEV", c("PROD", "INT")),
    verbose = list(NULL, 1, NA, logical(), c(TRUE, FALSE))
  )

  for (argument in names(invalid_values)) {
    for (value in invalid_values[[argument]]) {
      args <- wrapper_valid_args()
      args[argument] <- list(value)
      expect_error(
        do.call(pipdata_dlw_process, args),
        class = "pipdata_dlw_argument_error",
        info = paste("argument", argument, "value", deparse(value))
      )
    }
  }
  expect_identical(setup_calls, 0L)
})

test_that("wrapper setup failures and interrupts escape unchanged", {
  testthat::local_mocked_bindings(
    setup_working_release = function(...) {
      rlang::abort("setup failed", class = "wrapper_setup_error")
    },
    .package = "pipfun"
  )
  expect_error(
    do.call(pipdata_dlw_process, wrapper_valid_args()),
    class = "wrapper_setup_error"
  )

  testthat::local_mocked_bindings(
    setup_working_release = function(...) {
      rlang::abort(
        "setup cancelled",
        class = "pipdata_dlw_cancellation"
      )
    },
    .package = "pipfun"
  )
  expect_error(
    do.call(pipdata_dlw_process, wrapper_valid_args()),
    class = "pipdata_dlw_cancellation"
  )

  interrupt <- structure(
    list(message = "setup interrupted"),
    class = c("interrupt", "condition")
  )
  testthat::local_mocked_bindings(
    setup_working_release = function(...) signalCondition(interrupt),
    .package = "pipfun"
  )
  observed <- tryCatch(
    do.call(pipdata_dlw_process, wrapper_valid_args()),
    interrupt = function(e) e
  )
  expect_s3_class(observed, "interrupt")
})

test_that("not-run stages have the exact six-field shape", {
  for (stage in c("acquisition", "validation")) {
    for (reason in c("disabled", "dependency_failed")) {
      result <- .new_dlw_wrapper_not_run(stage, reason)
      expect_identical(
        names(result),
        c("stage", "outcome", "inventory", "summary", "failures", "artifacts")
      )
      expect_identical(result$stage, stage)
      expect_identical(result$outcome, "not_run")
      expect_null(result$inventory)
      expect_identical(result$summary, list(reason = reason))
      expect_s3_class(result$failures, "data.table")
      expect_identical(names(result$failures), wrapper_failure_columns)
      expect_identical(nrow(result$failures), 0L)
      expect_identical(result$artifacts, list())
    }
  }
})

test_that("aggregate outcome implements the exhaustive requested-stage matrix", {
  rows <- data.table::data.table(
    acquisition = c(
      "success", "success", "success", "success",
      "no_work", "no_work", "no_work", "no_work",
      rep("partial", 4L), rep("failed", 4L), "failed",
      rep("not_run", 4L),
      "success", "no_work", "partial", "failed", "not_run"
    ),
    acquisition_reason = c(
      rep(NA_character_, 17L), rep("disabled", 4L),
      rep(NA_character_, 4L), "disabled"
    ),
    validation = c(
      "success", "no_work", "partial", "failed",
      "success", "no_work", "partial", "failed",
      "success", "no_work", "partial", "failed",
      "success", "no_work", "partial", "failed", "not_run",
      "success", "no_work", "partial", "failed",
      rep("not_run", 5L)
    ),
    validation_reason = c(
      rep(NA_character_, 16L), "dependency_failed",
      rep(NA_character_, 4L),
      rep("disabled", 5L)
    ),
    expected = c(
      "success", "success", "partial", "partial",
      "success", "no_work", "partial", "partial",
      rep("partial", 4L),
      "partial", "partial", "partial", "failed", "failed",
      "success", "no_work", "partial", "failed",
      "success", "no_work", "partial", "failed", "no_work"
    )
  )

  for (index in seq_len(nrow(rows))) {
    acquisition <- list(
      outcome = rows$acquisition[[index]],
      summary = if (is.na(rows$acquisition_reason[[index]])) {
        list()
      } else {
        list(reason = rows$acquisition_reason[[index]])
      }
    )
    validation <- list(
      outcome = rows$validation[[index]],
      summary = if (is.na(rows$validation_reason[[index]])) {
        list()
      } else {
        list(reason = rows$validation_reason[[index]])
      }
    )
    expect_identical(
      .derive_dlw_wrapper_outcome(
        acquisition,
        validation,
        make_wrapper_empty_failures()
      ),
      rows$expected[[index]],
      info = paste(rows$acquisition[[index]], rows$validation[[index]])
    )
  }

  wrapper_failure <- data.table::data.table(
    survey_id = NA_character_, phase = "alias_init",
    error_type = "test_error", condition_msg = "alias failed"
  )
  expect_identical(
    .derive_dlw_wrapper_outcome(
      make_wrapper_acquisition_result("success"),
      make_wrapper_validation_result("success"),
      wrapper_failure
    ),
    "failed"
  )
})

test_that("validation continuation requires trustworthy non-NULL durable state", {
  for (outcome in c("no_work", "partial", "failed")) {
    result <- make_wrapper_acquisition_result(
      outcome,
      inventory = data.table::data.table(id = outcome),
      trustworthy = TRUE
    )
    expect_true(.dlw_wrapper_can_validate(result), info = outcome)
  }

  no_inventory <- make_wrapper_acquisition_result(
    "failed",
    inventory = NULL,
    trustworthy = TRUE
  )
  untrustworthy <- make_wrapper_acquisition_result(
    "failed",
    inventory = data.table::data.table(id = "unknown"),
    trustworthy = FALSE
  )
  expect_false(.dlw_wrapper_can_validate(no_inventory))
  expect_false(.dlw_wrapper_can_validate(untrustworthy))
})

test_that("wrapper summary uses the exact scalar 14-field schema", {
  acquisition <- make_wrapper_acquisition_result("success")
  validation <- make_wrapper_validation_result("partial")
  summary <- .new_dlw_wrapper_summary_logmeta(
    get_dlw_data = TRUE,
    validate_dlw_data = TRUE,
    outcome = "partial",
    acquisition = acquisition,
    validation = validation
  )

  expect_identical(names(summary), c(
    "info", "phase", "get_dlw_data", "validate_dlw_data", "outcome",
    "acquisition_outcome", "validation_outcome",
    "acquisition_n_total", "acquisition_n_success", "acquisition_n_failed",
    "validation_n_total", "validation_n_valid", "validation_n_invalid",
    "validation_n_failed"
  ))
  expect_identical(summary$info, "dlw_summary_inf")
  expect_identical(summary$phase, "complete")
  expect_identical(summary$acquisition_n_total, 1L)
  expect_identical(summary$validation_n_valid, 1L)
  expect_identical(summary$validation_n_failed, 0L)
  expect_identical(
    .validate_dlw_wrapper_summary_logmeta(summary),
    invisible(summary)
  )

  malformed <- list(
    append(summary, list(extra = TRUE)),
    within(summary, acquisition_n_total <- 1),
    within(summary, acquisition_n_total <- 2L),
    within(summary, validation_n_valid <- list(1L))
  )
  for (value in malformed) {
    expect_error(
      .validate_dlw_wrapper_summary_logmeta(value),
      class = "pipdata_dlw_contract_error"
    )
  }
})

test_that("both-disabled wrapper skips folders and returns exact invisible result", {
  logs <- list()
  checkpoints <- list()
  aliases <- character()
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) rlang::abort("folders must be skipped"),
    log_filter = function(...) rlang::abort("summary must not parse logs"),
    log_info = function(message, name, logmeta, ...) {
      logs <<- c(logs, list(logmeta))
      invisible(TRUE)
    },
    log_save_checkpoint = function(name, stage, alias, ...) {
      checkpoints <<- c(checkpoints, list(list(
        name = name, stage = stage, alias = alias
      )))
      list(version_id = "checkpoint-v1")
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(root, alias, ...) {
      aliases <<- c(aliases, alias)
      invisible(TRUE)
    },
    st_versions = function(...) data.table::data.table(
      version_id = character()
    ),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) rlang::abort("directories must be skipped"),
    pipdata_get_gmd = function(...) rlang::abort("acquisition must be skipped"),
    .pipdata_validate_gmd_core = function(...) {
      rlang::abort("validation must be skipped")
    },
    .package = "pipdata"
  )

  result <- withVisible(pipdata_dlw_process(
    release = "20260206",
    identity = "TEST",
    get_dlw_data = FALSE,
    validate_dlw_data = FALSE,
    verbose = FALSE
  ))

  expect_false(result$visible)
  expect_identical(names(result$value), c(
    "stage", "outcome", "acquisition", "validation", "failures",
    "checkpoint"
  ))
  expect_identical(result$value$stage, "dlw")
  expect_identical(result$value$outcome, "no_work")
  expect_identical(result$value$acquisition$summary$reason, "disabled")
  expect_identical(result$value$validation$summary$reason, "disabled")
  expect_identical(names(result$value$checkpoint), c(
    "summary_logged", "summary_condition_msg", "attempted", "success",
    "trustworthy", "alias", "stage", "version_id", "skipped",
    "reconciled", "condition_msg"
  ))
  expect_identical(aliases, c("pip_deflated", "piplog"))
  expect_length(logs, 1L)
  expect_identical(names(logs[[1L]]), c(
    "info", "phase", "get_dlw_data", "validate_dlw_data", "outcome",
    "acquisition_outcome", "validation_outcome",
    "acquisition_n_total", "acquisition_n_success", "acquisition_n_failed",
    "validation_n_total", "validation_n_valid", "validation_n_invalid",
    "validation_n_failed"
  ))
  expect_identical(checkpoints, list(list(
    name = "pipdata_log", stage = "dlw", alias = "dlw_meta"
  )))
})

test_that("validate-only delegates folders and custom inventory to validation", {
  core_id <- NULL
  menu_calls <- 0L
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) rlang::abort("wrapper folder lookup ran"),
    log_info = function(...) invisible(TRUE),
    log_save_checkpoint = function(...) list(version_id = "checkpoint-v1"),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    st_versions = function(...) data.table::data.table(version_id = character()),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    menu = function(...) {
      menu_calls <<- menu_calls + 1L
      rlang::abort("menu ran")
    },
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    .pipdata_validate_gmd_core = function(acquisition_inventory_id, ...) {
      core_id <<- acquisition_inventory_id
      result <- make_wrapper_validation_result("failed")
      result$failures$phase <- "inventory_missing"
      result$failures$error_type <- "inventory_missing_error"
      result
    },
    .package = "pipdata"
  )

  result <- pipdata_dlw_process(
    inv_gmd_list = "custom_gmd_inventory",
    release = "20260206",
    identity = "TEST",
    get_dlw_data = FALSE,
    validate_dlw_data = TRUE,
    verbose = FALSE
  )

  expect_identical(core_id, "custom_gmd_inventory")
  expect_identical(menu_calls, 0L)
  expect_identical(result$acquisition$summary$reason, "disabled")
  expect_identical(result$validation$outcome, "failed")
  expect_identical(result$validation$failures$phase, "inventory_missing")
  expect_identical(result$outcome, "failed")
})

test_that("interactive acquisition bootstrap uses the custom ID and preserves cancel", {
  reload_calls <- 0L
  bootstrap_id <- NULL
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(dlw_inventory = tempdir()),
    log_info = function(...) invisible(TRUE),
    log_save_checkpoint = function(...) list(version_id = "checkpoint-v1"),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    st_versions = function(...) data.table::data.table(version_id = character()),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    menu = function(...) 1L,
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    .dlw_wrapper_is_interactive = function() TRUE,
    check_directory = function(...) invisible(TRUE),
    .reload_dlw_acquisition_inventory_state = function(id, ...) {
      reload_calls <<- reload_calls + 1L
      if (reload_calls == 1L) {
        return(list(
          state = "absent", value = NULL, version_id = NA_character_
        ))
      }
      list(
        state = "present",
        value = data.table::data.table(id = id),
        version_id = "bootstrap-v1"
      )
    },
    dlw_gmd_list = function(inv_gmd_list) {
      bootstrap_id <<- inv_gmd_list
      invisible(data.table::data.table(id = inv_gmd_list))
    },
    pipdata_get_gmd = function(inv_gmd_list, ...) {
      make_wrapper_acquisition_result(
        "no_work",
        inventory = data.table::data.table(id = inv_gmd_list)
      )
    },
    .package = "pipdata"
  )

  result <- pipdata_dlw_process(
    inv_gmd_list = "custom_gmd_inventory",
    release = "20260206",
    identity = "TEST",
    get_dlw_data = TRUE,
    validate_dlw_data = FALSE,
    verbose = FALSE
  )
  expect_identical(bootstrap_id, "custom_gmd_inventory")
  expect_identical(reload_calls, 2L)
  expect_identical(result$acquisition$outcome, "no_work")

  testthat::local_mocked_bindings(
    menu = function(...) 2L,
    .package = "utils"
  )
  reload_calls <- 0L
  expect_error(
    pipdata_dlw_process(
      inv_gmd_list = "custom_gmd_inventory",
      release = "20260206",
      identity = "TEST",
      get_dlw_data = TRUE,
      validate_dlw_data = FALSE,
      verbose = FALSE
    ),
    class = "pipdata_dlw_cancellation"
  )
})

test_that("noninteractive missing inventory reaches acquisition failed result", {
  acquisition_calls <- 0L
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) rlang::abort("wrapper folder lookup ran"),
    log_info = function(...) invisible(TRUE),
    log_save_checkpoint = function(...) list(version_id = "checkpoint-v1"),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    st_versions = function(...) data.table::data.table(version_id = character()),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    menu = function(...) rlang::abort("menu must not run"),
    .package = "utils"
  )
  testthat::local_mocked_bindings(
    .dlw_wrapper_is_interactive = function() FALSE,
    check_directory = function(...) invisible(TRUE),
    .reload_dlw_acquisition_inventory_state = function(...) list(
      state = "absent", value = NULL, version_id = NA_character_
    ),
    pipdata_get_gmd = function(...) {
      acquisition_calls <<- acquisition_calls + 1L
      result <- make_wrapper_acquisition_result(
        "failed", inventory = NULL, trustworthy = TRUE
      )
      result$failures$phase <- "inventory_missing"
      result$failures$error_type <- "inventory_missing_error"
      result
    },
    .package = "pipdata"
  )

  result <- pipdata_dlw_process(
    release = "20260206", identity = "TEST",
    get_dlw_data = TRUE, validate_dlw_data = FALSE, verbose = FALSE
  )
  expect_identical(acquisition_calls, 1L)
  expect_identical(result$acquisition$outcome, "failed")
  expect_identical(result$acquisition$failures$phase, "inventory_missing")
  expect_identical(result$outcome, "failed")
})

test_that("wrapper infrastructure failures have one compact owner", {
  cases <- c("alias_init", "folder_resolve", "directory_check", "bootstrap_inventory")

  for (failure_phase in cases) {
    withr::local_options(pipfun.main_dir = tempdir())
    testthat::local_mocked_bindings(
      setup_working_release = function(...) invisible(TRUE),
      get_wrk_release = function(...) invisible(TRUE),
      get_pip_folders = function(...) {
        if (identical(failure_phase, "folder_resolve")) {
          rlang::abort("folder resolution failed")
        }
        list(dlw_inventory = tempdir())
      },
      log_info = function(...) invisible(TRUE),
      log_save_checkpoint = function(...) list(version_id = "checkpoint-v1"),
      .package = "pipfun"
    )
    testthat::local_mocked_bindings(
      st_init = function(...) {
        if (identical(failure_phase, "alias_init")) {
          rlang::abort("alias failed")
        }
        invisible(TRUE)
      },
      st_versions = function(...) data.table::data.table(version_id = character()),
      .package = "stamp"
    )
    testthat::local_mocked_bindings(
      .dlw_wrapper_is_interactive = function() {
        !identical(failure_phase, "alias_init")
      },
      check_directory = function(...) {
        if (identical(failure_phase, "directory_check")) {
          rlang::abort("directory failed")
        }
        invisible(TRUE)
      },
      .reload_dlw_acquisition_inventory_state = function(...) {
        if (identical(failure_phase, "bootstrap_inventory")) {
          rlang::abort("inventory inspection failed")
        }
        list(
          state = "present",
          value = data.table::data.table(id = "durable"),
          version_id = "acq-v1"
        )
      },
      pipdata_get_gmd = function(...) {
        rlang::abort("delegate must be dependency blocked")
      },
      .pipdata_validate_gmd_core = function(...) {
        rlang::abort("delegate must be dependency blocked")
      },
      .package = "pipdata"
    )

    result <- pipdata_dlw_process(
      release = "20260206", identity = "TEST",
      get_dlw_data = TRUE, validate_dlw_data = TRUE, verbose = FALSE
    )
    expect_identical(result$outcome, "failed", info = failure_phase)
    expect_identical(result$failures$phase, failure_phase)
    expect_identical(names(result$failures), wrapper_failure_columns)
    expect_identical(
      result$acquisition$summary$reason,
      "dependency_failed"
    )
    expect_identical(
      result$validation$summary$reason,
      "dependency_failed"
    )
  }
})

test_that("acquisition delegate retains ownership after bootstrap checks", {
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) rlang::abort("wrapper folder lookup ran"),
    log_info = function(...) invisible(TRUE),
    log_save_checkpoint = function(...) list(version_id = "checkpoint-v1"),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    st_versions = function(...) data.table::data.table(version_id = character()),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    .dlw_wrapper_is_interactive = function() FALSE,
    check_directory = function(...) invisible(TRUE),
    .reload_dlw_acquisition_inventory_state = function(...) list(
      state = "present",
      value = data.table::data.table(id = "durable"),
      version_id = "acq-v1"
    ),
    pipdata_get_gmd = function(...) {
      result <- make_wrapper_acquisition_result("failed")
      result$failures$phase <- "directory_check"
      result
    },
    .package = "pipdata"
  )

  result <- pipdata_dlw_process(
    release = "20260206", identity = "TEST",
    get_dlw_data = TRUE, validate_dlw_data = FALSE, verbose = FALSE
  )
  expect_identical(nrow(result$failures), 0L)
  expect_identical(result$acquisition$failures$phase, "directory_check")
  expect_identical(result$acquisition$outcome, "failed")
})

test_that("unexpected delegate errors become stage-owned delegate_error results", {
  validation_id <- NULL
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(dlw_inventory = tempdir()),
    log_info = function(...) invisible(TRUE),
    log_save_checkpoint = function(...) list(version_id = "checkpoint-v1"),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    st_versions = function(...) data.table::data.table(version_id = character()),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    .dlw_wrapper_is_interactive = function() FALSE,
    check_directory = function(...) invisible(TRUE),
    .reload_dlw_acquisition_inventory_state = function(...) list(
      state = "present",
      value = data.table::data.table(id = "durable"),
      version_id = "acq-v1"
    ),
    pipdata_get_gmd = function(...) rlang::abort("delegate acquisition boom"),
    .pipdata_validate_gmd_core = function(acquisition_inventory_id, ...) {
      validation_id <<- acquisition_inventory_id
      make_wrapper_validation_result("success")
    },
    .package = "pipdata"
  )

  result <- pipdata_dlw_process(
    inv_gmd_list = "custom_gmd_inventory",
    release = "20260206", identity = "TEST",
    get_dlw_data = TRUE, validate_dlw_data = TRUE, verbose = FALSE
  )
  expect_identical(result$acquisition$outcome, "failed")
  expect_identical(result$acquisition$failures$phase, "delegate_error")
  expect_identical(result$acquisition$inventory$id, "durable")
  expect_identical(validation_id, "custom_gmd_inventory")
  expect_identical(result$validation$outcome, "success")
  expect_identical(result$outcome, "partial")

  testthat::local_mocked_bindings(
    pipdata_get_gmd = function(...) make_wrapper_acquisition_result("success"),
    .pipdata_validate_gmd_core = function(...) {
      rlang::abort("delegate validation boom")
    },
    .package = "pipdata"
  )
  validation_failure <- pipdata_dlw_process(
    release = "20260206", identity = "TEST",
    get_dlw_data = TRUE, validate_dlw_data = TRUE, verbose = FALSE
  )
  expect_identical(validation_failure$validation$outcome, "failed")
  expect_identical(
    validation_failure$validation$failures$phase,
    "delegate_error"
  )
})

test_that("interactive delegate errors reload changed durable acquisition state", {
  reload_calls <- 0L
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(dlw_inventory = tempdir()),
    log_info = function(...) invisible(TRUE),
    log_save_checkpoint = function(...) list(version_id = "checkpoint-v1"),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    st_versions = function(...) data.table::data.table(version_id = character()),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    .dlw_wrapper_is_interactive = function() TRUE,
    check_directory = function(...) invisible(TRUE),
    .reload_dlw_acquisition_inventory_state = function(...) {
      reload_calls <<- reload_calls + 1L
      list(
        state = "present",
        value = data.table::data.table(id = if (reload_calls == 1L) "old" else "new"),
        version_id = if (reload_calls == 1L) "v1" else "v2"
      )
    },
    pipdata_get_gmd = function(...) rlang::abort("delegate changed state"),
    .package = "pipdata"
  )

  changed <- pipdata_dlw_process(
    release = "20260206", identity = "TEST",
    get_dlw_data = TRUE, validate_dlw_data = FALSE, verbose = FALSE
  )
  expect_identical(reload_calls, 2L)
  expect_identical(changed$acquisition$inventory$id, "new")
  expect_identical(changed$acquisition$artifacts$inventory$version_id, "v2")

  testthat::local_mocked_bindings(
    .reload_dlw_acquisition_inventory_state = function(...) {
      reload_calls <<- reload_calls + 1L
      if (reload_calls %% 2L == 1L) {
        return(list(
          state = "present",
          value = data.table::data.table(id = "old"),
          version_id = "v1"
        ))
      }
      rlang::abort("reload failed")
    },
    .package = "pipdata"
  )
  reload_calls <- 0L
  unknown <- pipdata_dlw_process(
    release = "20260206", identity = "TEST",
    get_dlw_data = TRUE, validate_dlw_data = TRUE, verbose = FALSE
  )
  expect_false(unknown$acquisition$artifacts$inventory$trustworthy)
  expect_null(unknown$acquisition$inventory)
  expect_identical(unknown$validation$summary$reason, "dependency_failed")
})

test_that("wrapper delegate boundary does not catch interrupts", {
  interrupt <- structure(
    list(message = "delegate interrupted"),
    class = c("interrupt", "condition")
  )
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(dlw_inventory = tempdir()),
    log_info = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    .dlw_wrapper_is_interactive = function() FALSE,
    check_directory = function(...) invisible(TRUE),
    .reload_dlw_acquisition_inventory_state = function(...) list(
      state = "present",
      value = data.table::data.table(id = "durable"),
      version_id = "acq-v1"
    ),
    pipdata_get_gmd = function(...) signalCondition(interrupt),
    .package = "pipdata"
  )

  observed <- tryCatch(
    pipdata_dlw_process(
      release = "20260206", identity = "TEST",
      get_dlw_data = TRUE, validate_dlw_data = FALSE, verbose = FALSE
    ),
    interrupt = function(e) e
  )
  expect_s3_class(observed, "interrupt")

  testthat::local_mocked_bindings(
    pipdata_get_gmd = function(...) {
      rlang::abort(
        "delegate cancelled",
        class = "pipdata_dlw_cancellation"
      )
    },
    .package = "pipdata"
  )
  expect_error(
    pipdata_dlw_process(
      release = "20260206", identity = "TEST",
      get_dlw_data = TRUE, validate_dlw_data = FALSE, verbose = FALSE
    ),
    class = "pipdata_dlw_cancellation"
  )
})

test_that("summary and checkpoint diagnostics are independent of outcome", {
  versions_calls <- 0L
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) rlang::abort("folders must be skipped"),
    log_info = function(...) rlang::abort("summary logger failed"),
    log_save_checkpoint = function(...) rlang::abort("checkpoint save failed"),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    st_versions = function(...) {
      versions_calls <<- versions_calls + 1L
      data.table::data.table(version_id = "checkpoint-v1")
    },
    st_load = function(...) structure(
      data.table::data.table(event = "info"),
      class = c("piplog", "data.table", "data.frame")
    ),
    .package = "stamp"
  )

  result <- pipdata_dlw_process(
    release = "20260206", identity = "TEST",
    get_dlw_data = FALSE, validate_dlw_data = FALSE, verbose = FALSE
  )

  expect_identical(result$outcome, "no_work")
  expect_false(result$checkpoint$summary_logged)
  expect_match(result$checkpoint$summary_condition_msg, "summary logger failed")
  expect_true(result$checkpoint$attempted)
  expect_false(result$checkpoint$success)
  expect_true(result$checkpoint$trustworthy)
  expect_true(result$checkpoint$reconciled)
  expect_match(result$checkpoint$condition_msg, "checkpoint save failed")
  expect_identical(versions_calls, 2L)
})

test_that("checkpoint reconciles advancement and accepts explicit skip", {
  versions <- c("checkpoint-v1", "checkpoint-v2")
  version_index <- 0L
  withr::local_options(pipfun.main_dir = tempdir())
  testthat::local_mocked_bindings(
    setup_working_release = function(...) invisible(TRUE),
    get_wrk_release = function(...) invisible(TRUE),
    log_info = function(...) invisible(TRUE),
    log_save_checkpoint = function(...) NULL,
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    st_init = function(...) invisible(TRUE),
    st_versions = function(...) {
      version_index <<- version_index + 1L
      data.table::data.table(version_id = versions[[version_index]])
    },
    st_load = function(...) structure(
      data.table::data.table(event = "info"),
      class = c("piplog", "data.table", "data.frame")
    ),
    .package = "stamp"
  )

  recovered <- pipdata_dlw_process(
    release = "20260206", identity = "TEST",
    get_dlw_data = FALSE, validate_dlw_data = FALSE, verbose = FALSE
  )
  expect_true(recovered$checkpoint$success)
  expect_true(recovered$checkpoint$trustworthy)
  expect_true(recovered$checkpoint$reconciled)
  expect_identical(recovered$checkpoint$version_id, "checkpoint-v2")
  expect_true(is.na(recovered$checkpoint$condition_msg))

  testthat::local_mocked_bindings(
    log_save_checkpoint = function(...) list(
      version_id = NULL, skipped = TRUE
    ),
    .package = "pipfun"
  )
  versions <- c("checkpoint-v1", "checkpoint-v1")
  version_index <- 0L
  skipped <- pipdata_dlw_process(
    release = "20260206", identity = "TEST",
    get_dlw_data = FALSE, validate_dlw_data = FALSE, verbose = FALSE
  )
  expect_true(skipped$checkpoint$success)
  expect_true(skipped$checkpoint$trustworthy)
  expect_true(skipped$checkpoint$skipped)
  expect_false(skipped$checkpoint$reconciled)
  expect_true(is.na(skipped$checkpoint$version_id))
})

test_that("checkpoint direct success accepts only missing or scalar false skipped", {
  for (skipped in list(NULL, FALSE)) {
    inspections <- 0L
    testthat::local_mocked_bindings(
      log_save_checkpoint = function(...) {
        result <- list(version_id = "checkpoint-v2")
        if (!is.null(skipped)) result$skipped <- skipped
        result
      },
      .package = "pipfun"
    )
    testthat::local_mocked_bindings(
      .inspect_dlw_wrapper_checkpoint = function() {
        inspections <<- inspections + 1L
        list(state = "present", version_id = "v1")
      },
      .package = "pipdata"
    )
    fact <- .save_dlw_wrapper_checkpoint(TRUE, NA_character_)
    expect_identical(inspections, 1L)
    expect_true(fact$success)
    expect_false(fact$reconciled)
    expect_false(fact$skipped)
  }

  malformed <- list(NA, "false", c(FALSE, FALSE))
  for (skipped in malformed) {
    inspections <- 0L
    testthat::local_mocked_bindings(
      log_save_checkpoint = function(...) list(
        version_id = "reported-v2", skipped = skipped
      ),
      .package = "pipfun"
    )
    testthat::local_mocked_bindings(
      .inspect_dlw_wrapper_checkpoint = function() {
        inspections <<- inspections + 1L
        list(
          state = "present",
          version_id = if (inspections == 1L) "v1" else "v2"
        )
      },
      .package = "pipdata"
    )
    fact <- .save_dlw_wrapper_checkpoint(TRUE, NA_character_)
    expect_identical(inspections, 2L)
    expect_true(fact$success)
    expect_true(fact$reconciled)
    expect_false(fact$skipped)
    expect_identical(fact$version_id, "v2")
  }
})

test_that("malformed checkpoint catalogs are unknown rather than absent", {
  testthat::local_mocked_bindings(
    st_versions = function(...) data.table::data.table(other = character()),
    .package = "stamp"
  )
  expect_identical(
    .inspect_dlw_wrapper_checkpoint(),
    list(state = "unknown", version_id = NA_character_)
  )
})
