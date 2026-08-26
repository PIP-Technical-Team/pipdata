has_raw_condition <- function(x) {
  if (inherits(x, "condition")) {
    return(TRUE)
  }
  if (!is.list(x)) {
    return(FALSE)
  }
  any(vapply(x, has_raw_condition, logical(1)))
}

make_acquisition_inventory <- function() {
  data.table::data.table(
    Country = c("ZWE", "BOL"),
    Year = c(2021L, 2020L),
    Survey_acronym = c("PICES", "EH"),
    Vermast = c("02", "01"),
    Veralt = c("01", "01"),
    Module = c("ALL", "GPWG"),
    Collection = "GMD",
    FileName = c("ZWE_2021_PICES.dta", "BOL_2020_EH.dta"),
    Checksum = c("checksum-z", "checksum-b"),
    Ext = "dta",
    server_note = c("z", "b"),
    data_available = "Yes"
  )
}

make_acquisition_summary <- function() {
  list(
    n_total = 2L,
    n_success = 1L,
    n_failed = 1L,
    surveys_success = "BOL_2020_EH",
    surveys_failed = "ZWE_2021_PICES"
  )
}

test_that("acquisition result has the pinned plain-list shape and copies inventory", {
  inventory <- make_acquisition_inventory()
  inventory_before <- data.table::copy(inventory)
  failures <- .new_dlw_acquisition_failure(
    survey_id = "ZWE_2021_PICES",
    phase = "download",
    condition = rlang::error_cnd(
      "dlw_download_error",
      message = "download failed"
    )
  )
  artifact <- .new_dlw_acquisition_artifact_fact(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    attempted = TRUE,
    success = TRUE,
    trustworthy = TRUE,
    version_id = "version-2",
    skipped = FALSE,
    reconciled = FALSE
  )

  result <- .new_dlw_acquisition_result(
    outcome = "partial",
    inventory = inventory,
    summary = make_acquisition_summary(),
    failures = failures,
    artifacts = list(inventory = artifact)
  )

  expect_named(
    result,
    c("stage", "outcome", "inventory", "summary", "failures", "artifacts")
  )
  expect_identical(result$stage, "acquisition")
  expect_identical(class(result), "list")
  expect_identical(result$summary, make_acquisition_summary())
  expect_s3_class(result$failures, "data.table")
  expect_false(has_raw_condition(result))

  inventory[, data_available := "No"]
  expect_identical(result$inventory, inventory_before)
})

test_that("acquisition no-work result retains trustworthy prior state", {
  result <- .new_dlw_acquisition_result(
    outcome = "no_work",
    inventory = make_acquisition_inventory(),
    summary = list(
      n_total = 0L,
      n_success = 0L,
      n_failed = 0L,
      surveys_success = character(),
      surveys_failed = character()
    ),
    failures = .new_dlw_acquisition_failure(),
    artifacts = list(inventory = .new_dlw_acquisition_artifact_fact(
      id = "dlw_gmd_inv",
      alias = "dlw_inv",
      attempted = FALSE,
      success = NA,
      trustworthy = TRUE,
      version_id = "prior-v1",
      skipped = NA,
      reconciled = FALSE
    ))
  )

  expect_identical(result$outcome, "no_work")
  expect_equal(result$summary$n_total, 0L)
  expect_identical(result$artifacts$inventory$success, NA)
})

test_that("acquisition failure helper returns compact typed tables", {
  empty <- .new_dlw_acquisition_failure()
  expect_s3_class(empty, "data.table")
  expect_named(
    empty,
    c("survey_id", "phase", "error_type", "condition_msg")
  )
  expect_identical(
    vapply(empty, typeof, character(1)),
    c(
      survey_id = "character",
      phase = "character",
      error_type = "character",
      condition_msg = "character"
    )
  )
  expect_equal(nrow(empty), 0L)

  typed <- .new_dlw_acquisition_failure(
    survey_id = "BOL_2020_EH",
    phase = "download",
    condition = rlang::error_cnd(
      "dlw_download_error",
      message = "network unavailable"
    )
  )
  expect_identical(typed$error_type, "dlw_download_error")
  expect_identical(typed$condition_msg, "network unavailable")
  expect_false(has_raw_condition(typed))

  workflow <- .new_dlw_acquisition_failure(
    phase = "catalog_schema",
    error_type = "catalog_schema_error",
    condition_msg = "Catalog column `Checksum` is missing."
  )
  expect_true(is.na(workflow$survey_id))
  expect_identical(workflow$error_type, "catalog_schema_error")
  expect_error(
    .new_dlw_acquisition_failure(survey_id = "BOL_2020_EH"),
    class = "pipdata_dlw_contract_error"
  )
})

test_that("acquisition result rejects malformed summaries and raw conditions", {
  artifact <- .new_dlw_acquisition_artifact_fact(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    attempted = FALSE,
    success = NA,
    trustworthy = TRUE,
    version_id = NA_character_,
    skipped = NA,
    reconciled = FALSE
  )
  bad_summary <- make_acquisition_summary()
  bad_summary$n_total <- 3L

  expect_error(
    .new_dlw_acquisition_result(
      outcome = "partial",
      inventory = make_acquisition_inventory(),
      summary = bad_summary,
      failures = .new_dlw_acquisition_failure(),
      artifacts = list(inventory = artifact)
    ),
    class = "pipdata_dlw_contract_error"
  )
  expect_error(
    .new_dlw_acquisition_result(
      outcome = "failed",
      inventory = NULL,
      summary = list(
        n_total = 0L,
        n_success = 0L,
        n_failed = 0L,
        surveys_success = character(),
        surveys_failed = character()
      ),
      failures = .new_dlw_acquisition_failure(),
      artifacts = list(inventory = artifact, raw = simpleError("retained"))
    ),
    class = "pipdata_dlw_contract_error"
  )
  expect_error(
    .new_dlw_acquisition_result(
      outcome = "no_work",
      inventory = make_acquisition_inventory(),
      summary = make_acquisition_summary(),
      failures = .new_dlw_acquisition_failure(
        survey_id = "ZWE_2021_PICES",
        phase = "download",
        error_type = "download_error",
        condition_msg = "download failed"
      ),
      artifacts = list(inventory = artifact)
    ),
    class = "pipdata_dlw_contract_error"
  )
})

test_that("acquisition artifact facts implement every truth-table state", {
  states <- list(
    prior = list(FALSE, NA, TRUE, "prior-v1", NA, FALSE),
    absent = list(FALSE, NA, TRUE, NA_character_, NA, FALSE),
    not_reached_known = list(FALSE, FALSE, TRUE, "prior-v1", NA, FALSE),
    not_reached_unknown = list(FALSE, FALSE, FALSE, NA_character_, NA, FALSE),
    returned = list(TRUE, TRUE, TRUE, "new-v2", FALSE, FALSE),
    skipped = list(TRUE, TRUE, TRUE, NA_character_, TRUE, FALSE),
    recovered_intended = list(TRUE, TRUE, TRUE, "new-v2", FALSE, TRUE),
    recovered_prior = list(TRUE, FALSE, TRUE, "prior-v1", FALSE, TRUE),
    ambiguous = list(TRUE, FALSE, FALSE, NA_character_, FALSE, TRUE)
  )

  facts <- lapply(states, function(state) {
    .new_dlw_acquisition_artifact_fact(
      id = "dlw_gmd_inv",
      alias = "dlw_inv",
      attempted = state[[1]],
      success = state[[2]],
      trustworthy = state[[3]],
      version_id = state[[4]],
      skipped = state[[5]],
      reconciled = state[[6]]
    )
  })

  expect_true(all(vapply(facts, is.list, logical(1))))
  expect_true(all(vapply(
    facts,
    function(x) identical(
      names(x),
      c(
        "id", "alias", "attempted", "success", "trustworthy",
        "version_id", "skipped", "reconciled"
      )
    ),
    logical(1)
  )))
  expect_error(
    .new_dlw_acquisition_artifact_fact(
      id = "dlw_gmd_inv",
      alias = "dlw_inv",
      attempted = FALSE,
      success = TRUE,
      trustworthy = TRUE,
      version_id = NA_character_,
      skipped = FALSE,
      reconciled = FALSE
    ),
    class = "pipdata_dlw_contract_error"
  )
})

test_that("acquisition canonicalization ignores ordering and transient attributes", {
  raw <- make_acquisition_inventory()
  raw_before <- data.table::copy(raw)
  reloaded <- raw[2:1, c(
    "data_available", "server_note", "Ext", "Checksum", "FileName",
    "Collection", "Module", "Veralt", "Vermast", "Survey_acronym",
    "Year", "Country"
  )]
  reloaded <- stamp::st_with_pk(reloaded, c("Checksum", "FileName"))
  data.table::setkey(reloaded, FileName)
  data.table::setindexv(reloaded, "Checksum")

  canonical_raw <- .canonicalize_dlw_acquisition_inventory(raw)
  canonical_reloaded <- .canonicalize_dlw_acquisition_inventory(reloaded)

  expect_identical(canonical_raw, canonical_reloaded)
  expect_identical(
    names(canonical_raw),
    c(
      "Country", "Year", "Survey_acronym", "Vermast", "Veralt",
      "Module", "Collection", "FileName", "Checksum", "Ext",
      "server_note", "data_available"
    )
  )
  expect_identical(
    attr(canonical_raw, "stamp_pk", exact = TRUE),
    list(keys = c("Checksum", "FileName"))
  )
  expect_null(attr(canonical_raw, ".internal.selfref", exact = TRUE))
  expect_null(attr(canonical_raw, "sorted", exact = TRUE))
  expect_null(attr(canonical_raw, "index", exact = TRUE))
  expect_identical(raw, raw_before)
})

test_that("DLW reconciler accepts returned and skipped writes without reload", {
  intended <- make_acquisition_inventory()
  reload_calls <- 0L
  reload <- function() {
    reload_calls <<- reload_calls + 1L
    rlang::abort("reload should not run")
  }

  returned <- .reconcile_dlw_persistence(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    write_result = list(version_id = "new-v2", skipped = FALSE),
    intended = intended,
    prior = NULL,
    reload = reload,
    canonicalize = .canonicalize_dlw_acquisition_inventory
  )
  skipped <- .reconcile_dlw_persistence(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    write_result = list(version_id = NULL, skipped = TRUE),
    intended = intended,
    prior = intended,
    reload = reload,
    canonicalize = .canonicalize_dlw_acquisition_inventory
  )

  expect_equal(reload_calls, 0L)
  expect_identical(returned$value, intended)
  expect_identical(returned$fact$success, TRUE)
  expect_identical(returned$fact$version_id, "new-v2")
  expect_identical(returned$fact$reconciled, FALSE)
  expect_identical(skipped$fact$skipped, TRUE)
  expect_identical(skipped$fact$version_id, NA_character_)
})

test_that("DLW reconciler verifies skipped writes against durable content", {
  intended <- make_acquisition_inventory()
  prior <- data.table::copy(intended)
  prior[FileName == "ZWE_2021_PICES.dta", data_available := "No"]
  reload_calls <- 0L

  reconciled <- .reconcile_dlw_persistence(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    write_result = list(version_id = NULL, skipped = TRUE),
    intended = intended,
    prior = prior,
    reload = function() {
      reload_calls <<- reload_calls + 1L
      list(
        state = "present",
        value = intended,
        version_id = "recovered-v2"
      )
    },
    canonicalize = .canonicalize_dlw_acquisition_inventory
  )

  expect_identical(reload_calls, 1L)
  expect_identical(reconciled$fact$success, TRUE)
  expect_identical(reconciled$fact$reconciled, TRUE)
  expect_identical(reconciled$fact$skipped, FALSE)
})

test_that("DLW reconciler proves intended, prior, absent, and unknown states", {
  intended <- make_acquisition_inventory()
  prior <- data.table::copy(intended)
  prior[FileName == "ZWE_2021_PICES.dta", data_available := "No"]
  malformed <- list(version_id = NULL, skipped = FALSE)

  intended_active <- .reconcile_dlw_persistence(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    write_result = simpleError("catalog write failed"),
    intended = intended,
    prior = prior,
    reload = function() list(
      state = "present",
      value = intended[2:1],
      version_id = "recovered-v2"
    ),
    canonicalize = .canonicalize_dlw_acquisition_inventory
  )
  prior_active <- .reconcile_dlw_persistence(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    write_result = malformed,
    intended = intended,
    prior = prior,
    prior_version_id = "prior-v1",
    reload = function() list(
      state = "present",
      value = prior,
      version_id = "prior-v1"
    ),
    canonicalize = .canonicalize_dlw_acquisition_inventory
  )
  absent <- .reconcile_dlw_persistence(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    write_result = malformed,
    intended = intended,
    prior = NULL,
    reload = function() list(
      state = "absent",
      value = NULL,
      version_id = NA_character_
    ),
    canonicalize = .canonicalize_dlw_acquisition_inventory
  )
  ambiguous <- .reconcile_dlw_persistence(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    write_result = malformed,
    intended = intended,
    prior = prior,
    reload = function() list(
      state = "present",
      value = intended[1],
      version_id = "other-v3"
    ),
    canonicalize = .canonicalize_dlw_acquisition_inventory
  )
  unreadable <- .reconcile_dlw_persistence(
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    write_result = malformed,
    intended = intended,
    prior = prior,
    reload = function() rlang::abort("cannot read active artifact"),
    canonicalize = .canonicalize_dlw_acquisition_inventory
  )

  expect_identical(intended_active$fact$success, TRUE)
  expect_identical(intended_active$fact$trustworthy, TRUE)
  expect_identical(intended_active$fact$version_id, "recovered-v2")
  expect_identical(intended_active$fact$reconciled, TRUE)
  expect_identical(prior_active$value, prior)
  expect_identical(prior_active$fact$success, FALSE)
  expect_identical(prior_active$fact$trustworthy, TRUE)
  expect_identical(prior_active$fact$version_id, "prior-v1")
  expect_null(absent$value)
  expect_identical(absent$fact$trustworthy, TRUE)
  expect_null(ambiguous$value)
  expect_identical(ambiguous$fact$trustworthy, FALSE)
  expect_null(unreadable$value)
  expect_identical(unreadable$fact$trustworthy, FALSE)
  expect_false(has_raw_condition(unreadable))
})

test_that("DLW reconciler supports checkpoint-specific comparison", {
  intended <- list(id = "pipdata_log_checkpoint_dlw", sequence = 2L)
  prior <- list(id = "pipdata_log_checkpoint_dlw", sequence = 1L)
  checkpoint_equal <- function(x, y) {
    identical(x$id, y$id) && identical(x$sequence, y$sequence)
  }

  result <- .reconcile_dlw_persistence(
    id = "pipdata_log_checkpoint_dlw",
    alias = "dlw_meta",
    write_result = simpleError("checkpoint sidecar failed"),
    intended = intended,
    prior = prior,
    reload = function() list(
      state = "present",
      value = intended,
      version_id = "checkpoint-v2"
    ),
    canonicalize = identity,
    compare = checkpoint_equal
  )

  expect_identical(result$value, intended)
  expect_identical(result$fact$success, TRUE)
  expect_identical(result$fact$version_id, "checkpoint-v2")
  expect_identical(result$fact$reconciled, TRUE)
})

make_acquisition_candidate <- function(
    file_name = "BOL_2020_EH.dta",
    checksum = "checksum-1"
) {
  data.table::data.table(
    Country = "BOL",
    Year = 2020L,
    Survey_acronym = "EH",
    Vermast = "01",
    Veralt = "02",
    Module = "ALL",
    Collection = "GMD",
    FileName = file_name,
    Checksum = checksum,
    Ext = "dta",
    data_available = "No"
  )
}

test_that("one-survey worker forces the exact catalog file download", {
  observed <- NULL
  testthat::local_mocked_bindings(
    dlw_get_gmd = function(...) {
      observed <<- list(...)
      data.table::data.table(value = 1L)
    },
    .package = "dlw"
  )

  result <- .acquire_one_gmd(
    make_acquisition_candidate(),
    local_dir = "local-dlw",
    verbose = FALSE
  )

  expect_true(result$success)
  expect_identical(result$data_available, "Yes")
  expect_identical(result$survey_id, "BOL_2020_EH")
  expect_identical(observed$country_code, "BOL")
  expect_identical(observed$year, 2020L)
  expect_identical(observed$survey, "EH")
  expect_identical(observed$module, "ALL")
  expect_identical(observed$vermast, "01")
  expect_identical(observed$veralt, "02")
  expect_identical(observed$filename, "BOL_2020_EH.dta")
  expect_identical(observed$local_dir, "local-dlw")
  expect_identical(observed$local_overwrite, TRUE)
  expect_identical(observed$verbose, FALSE)
  expect_false(has_raw_condition(result))
})

test_that("one-survey worker rejects ambiguous and failed downloads compactly", {
  candidate <- make_acquisition_candidate()
  ambiguous <- structure(list(quote(download_one()), quote(download_two())),
    class = c("dlw_call_list", "list")
  )

  testthat::local_mocked_bindings(
    dlw_get_gmd = function(...) ambiguous,
    .package = "dlw"
  )
  ambiguous_result <- .acquire_one_gmd(candidate, tempdir(), TRUE)

  expect_false(ambiguous_result$success)
  expect_identical(ambiguous_result$data_available, "No")
  expect_identical(ambiguous_result$failure$phase, "download")
  expect_identical(
    ambiguous_result$failure$error_type,
    "dlw_ambiguous_download_error"
  )
  expect_false(has_raw_condition(ambiguous_result))

  testthat::local_mocked_bindings(
    dlw_get_gmd = function(...) {
      rlang::abort("network down", class = "dlw_network_error")
    },
    .package = "dlw"
  )
  failed_result <- .acquire_one_gmd(candidate, tempdir(), TRUE)

  expect_false(failed_result$success)
  expect_identical(failed_result$failure$error_type, "dlw_network_error")
  expect_identical(failed_result$failure$condition_msg, "network down")
  expect_false(has_raw_condition(failed_result))
})

test_that("one-survey worker does not catch interrupts", {
  interrupt <- structure(
    list(message = "cancel acquisition"),
    class = c("interrupt", "condition")
  )
  testthat::local_mocked_bindings(
    dlw_get_gmd = function(...) signalCondition(interrupt),
    .package = "dlw"
  )

  observed <- tryCatch(
    .acquire_one_gmd(make_acquisition_candidate(), tempdir(), FALSE),
    interrupt = function(e) e
  )

  expect_s3_class(observed, "interrupt")
  expect_identical(conditionMessage(observed), "cancel acquisition")
})

test_that("acquisition completion metadata has the exact pinned schema", {
  logmeta <- .new_dlw_acquisition_completion_logmeta(
    outcome = "partial",
    summary = list(
      n_total = 2L,
      n_success = 1L,
      n_failed = 1L,
      surveys_success = "A",
      surveys_failed = "B"
    )
  )

  expect_identical(
    names(logmeta),
    c(
      "info", "phase", "outcome", "n_total", "n_success", "n_failed",
      "surveys_success", "surveys_failed"
    )
  )
  expect_identical(logmeta$info, .logtype_dlw_acquisition)
  expect_identical(logmeta$phase, "complete")
  expect_error(
    .validate_dlw_acquisition_completion_logmeta(
      c(logmeta, list(extra = TRUE))
    ),
    class = "pipdata_dlw_contract_error"
  )
})

test_that("pipdata_get_gmd validates arguments before release setup", {
  release_calls <- 0L
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) {
      release_calls <<- release_calls + 1L
      invisible(TRUE)
    },
    .package = "pipfun"
  )

  expect_error(
    pipdata_get_gmd(inv_gmd_list = character()),
    class = "pipdata_dlw_argument_error"
  )
  expect_error(
    pipdata_get_gmd(inv_gmd_list = ""),
    class = "pipdata_dlw_argument_error"
  )
  expect_error(
    pipdata_get_gmd(check_missing = NA),
    class = "pipdata_dlw_argument_error"
  )
  expect_error(
    pipdata_get_gmd(verbose = c(TRUE, FALSE)),
    class = "pipdata_dlw_argument_error"
  )
  expect_equal(release_calls, 0L)
})

test_that("pipdata_get_gmd retains exact public formals and defaults", {
  expect_identical(
    formals(pipdata_get_gmd),
    as.pairlist(alist(
      inv_gmd_list = "dlw_gmd_inv",
      check_missing = TRUE,
      verbose = getOption("pipdata.verbose", default = TRUE)
    ))
  )
})

test_that("pipdata_get_gmd leaves the working-release precondition escaping", {
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) {
      rlang::abort("working release missing", class = "working_release_error")
    },
    .package = "pipfun"
  )

  expect_error(
    pipdata_get_gmd(verbose = FALSE),
    class = "working_release_error"
  )
})

test_that("acquisition rejects an empty normalized server catalog", {
  prior <- make_acquisition_candidate()
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(), dlw_inventory = tempdir()
    ),
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    dlw_server_catalog = function() make_acquisition_candidate()[0L],
    .package = "dlw"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) prior,
    .package = "pipdata"
  )

  result <- pipdata_get_gmd(verbose = FALSE)

  expect_identical(result$outcome, "failed")
  expect_identical(result$failures$phase, "catalog_load")
  expect_identical(result$failures$error_type, "pipdata_dlw_catalog_load_error")
  expect_identical(result$inventory, prior)
})

test_that("acquisition orchestrator isolates siblings and persists once", {
  events <- list()
  persistence <- list()
  prior <- data.table::rbindlist(list(
    make_acquisition_candidate(),
    make_acquisition_candidate("ZWE_2021_PICES.dta", "checksum-2")[, `:=`(
      Country = "ZWE",
      Year = 2021L,
      Survey_acronym = "PICES"
    )]
  ))
  server <- data.table::copy(prior)
  server[, data_available := NULL]

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(),
      dlw_inventory = tempdir()
    ),
    log_info = function(message, name, logmeta, ...) {
      events <<- c(events, list(logmeta))
      invisible(TRUE)
    },
    log_error = function(message, name, logmeta, ...) {
      events <<- c(events, list(logmeta))
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(id, verbose) {
      expect_identical(id, "custom_inventory")
      data.table::copy(prior)
    },
    .load_dlw_acquisition_server_catalog = function() data.table::copy(server),
    .acquire_one_gmd = function(candidate, local_dir, verbose) {
      expect_identical(verbose, FALSE)
      survey_id <- fs::path_ext_remove(candidate$FileName[[1L]])
      if (identical(survey_id, "ZWE_2021_PICES")) {
        return(list(
          survey_id = survey_id,
          FileName = candidate$FileName[[1L]],
          success = FALSE,
          data_available = "No",
          failure = .new_dlw_acquisition_failure(
            survey_id = survey_id,
            phase = "download",
            error_type = "dlw_network_error",
            condition_msg = "network down"
          )
        ))
      }
      list(
        survey_id = survey_id,
        FileName = candidate$FileName[[1L]],
        success = TRUE,
        data_available = "Yes",
        failure = .new_dlw_acquisition_failure()
      )
    },
    .persist_dlw_acquisition_inventory = function(
        intended,
        prior,
        id,
        verbose,
        prior_version_id = NA_character_
    ) {
      persistence <<- c(persistence, list(list(
        intended = data.table::copy(intended),
        id = id,
        verbose = verbose
      )))
      list(
        value = intended,
        fact = .new_dlw_acquisition_artifact_fact(
          id = id,
          alias = "dlw_inv",
          attempted = TRUE,
          success = TRUE,
          trustworthy = TRUE,
          version_id = "version-2",
          skipped = FALSE,
          reconciled = FALSE
        )
      )
    },
    .dlw_acquisition_latest_version = function(...) "version-1",
    .package = "pipdata"
  )

  visible <- withVisible(pipdata_get_gmd(
    inv_gmd_list = "custom_inventory",
    check_missing = TRUE,
    verbose = FALSE
  ))
  result <- visible$value

  expect_false(visible$visible)
  expect_identical(result$outcome, "partial")
  expect_identical(result$summary$n_total, 2L)
  expect_identical(result$summary$n_success, 1L)
  expect_identical(result$summary$n_failed, 1L)
  expect_equal(nrow(result$failures), 1L)
  expect_equal(length(persistence), 1L)
  expect_identical(persistence[[1L]]$id, "custom_inventory")
  expect_identical(persistence[[1L]]$verbose, FALSE)
  expect_identical(events[[1L]]$phase, "attempt_start")
  completion <- events[[length(events)]]
  expect_identical(
    completion,
    .new_dlw_acquisition_completion_logmeta(
      outcome = result$outcome,
      summary = result$summary
    )
  )
})

test_that("zero-worker catalog changes persist while outcome remains no_work", {
  prior <- make_acquisition_candidate()
  prior[, data_available := "Yes"]
  server <- data.table::copy(prior)
  server[, `:=`(data_available = NULL, server_note = "refreshed")]
  write_calls <- 0L

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(),
      dlw_inventory = tempdir()
    ),
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) data.table::copy(prior),
    .load_dlw_acquisition_server_catalog = function(...) data.table::copy(server),
    .dlw_acquisition_latest_version = function(...) "version-1",
    .acquire_one_gmd = function(candidate, ...) list(
      survey_id = fs::path_ext_remove(candidate$FileName[[1L]]),
      FileName = candidate$FileName[[1L]],
      success = TRUE,
      data_available = "Yes",
      failure = .new_dlw_acquisition_failure()
    ),
    .persist_dlw_acquisition_inventory = function(intended, id, ...) {
      write_calls <<- write_calls + 1L
      list(
        value = intended,
        fact = .new_dlw_acquisition_artifact_fact(
          id = id,
          alias = "dlw_inv",
          attempted = TRUE,
          success = TRUE,
          trustworthy = TRUE,
          version_id = "version-2",
          skipped = FALSE,
          reconciled = FALSE
        )
      )
    },
    .package = "pipdata"
  )

  result <- pipdata_get_gmd(check_missing = TRUE, verbose = FALSE)

  expect_identical(result$outcome, "no_work")
  expect_equal(result$summary$n_total, 0L)
  expect_equal(write_calls, 1L)
  expect_identical(result$inventory$server_note, "refreshed")
})

test_that("failed attempt boundary suppresses later typed acquisition logs", {
  log_calls <- 0L
  error_calls <- 0L
  prior <- make_acquisition_candidate()
  server <- data.table::copy(prior)
  server[, data_available := NULL]

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(),
      dlw_inventory = tempdir()
    ),
    log_info = function(...) {
      log_calls <<- log_calls + 1L
      rlang::abort("logger unavailable")
    },
    log_error = function(...) {
      error_calls <<- error_calls + 1L
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) data.table::copy(prior),
    .load_dlw_acquisition_server_catalog = function(...) data.table::copy(server),
    .dlw_acquisition_latest_version = function(...) "version-1",
    .acquire_one_gmd = function(candidate, ...) list(
      survey_id = fs::path_ext_remove(candidate$FileName[[1L]]),
      FileName = candidate$FileName[[1L]],
      success = TRUE,
      data_available = "Yes",
      failure = .new_dlw_acquisition_failure()
    ),
    .persist_dlw_acquisition_inventory = function(intended, id, ...) {
      list(
        value = intended,
        fact = .new_dlw_acquisition_artifact_fact(
          id = id,
          alias = "dlw_inv",
          attempted = TRUE,
          success = TRUE,
          trustworthy = TRUE,
          version_id = "version-2",
          skipped = FALSE,
          reconciled = FALSE
        )
      )
    },
    .package = "pipdata"
  )

  result <- pipdata_get_gmd(verbose = FALSE)

  expect_equal(log_calls, 1L)
  expect_equal(error_calls, 0L)
  expect_true(any(result$failures$phase == "log_emit"))
  expect_identical(result$outcome, "partial")
})

test_that("stage-owned folder and schema failures return compact failed results", {
  events <- list()
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) rlang::abort("folder lookup failed"),
    log_info = function(message, name, logmeta, ...) {
      events <<- c(events, list(logmeta))
      invisible(TRUE)
    },
    log_error = function(message, name, logmeta, ...) {
      events <<- c(events, list(logmeta))
      invisible(TRUE)
    },
    .package = "pipfun"
  )

  folder_result <- pipdata_get_gmd(verbose = FALSE)

  expect_identical(folder_result$outcome, "failed")
  expect_identical(folder_result$failures$phase, "folder_resolve")
  expect_null(folder_result$inventory)
  expect_false(folder_result$artifacts$inventory$trustworthy)
  expect_identical(events[[1L]]$phase, "attempt_start")
  expect_identical(events[[length(events)]]$phase, "complete")
})

test_that("missing and unreadable inventories have distinct durable facts", {
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(),
      dlw_inventory = tempdir()
    ),
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) {
      rlang::abort(
        "No version files found.",
        class = "pipdata_dlw_inventory_missing_error"
      )
    },
    .package = "pipdata"
  )

  missing <- pipdata_get_gmd(verbose = FALSE)

  expect_identical(missing$failures$phase, "inventory_missing")
  expect_true(missing$artifacts$inventory$trustworthy)
  expect_false(missing$artifacts$inventory$success)

  testthat::local_mocked_bindings(
    .load_dlw_acquisition_inventory = function(...) {
      rlang::abort("active payload is corrupt", class = "stamp_read_error")
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    st_versions = function(...) data.table::data.table(
      version_id = "existing-version"
    ),
    .package = "stamp"
  )

  unreadable <- pipdata_get_gmd(verbose = FALSE)

  expect_identical(unreadable$failures$phase, "catalog_load")
  expect_false(unreadable$artifacts$inventory$trustworthy)
  expect_null(unreadable$inventory)
})

test_that("malformed version catalogs never prove acquisition absence", {
  read_error <- rlang::error_cnd(
    "stamp_read_error",
    message = "active payload is corrupt"
  )
  testthat::local_mocked_bindings(
    st_versions = function(...) data.table::data.table(other = character()),
    .package = "stamp"
  )
  expect_false(.is_missing_dlw_acquisition_error(read_error, "dlw_gmd_inv"))

  testthat::local_mocked_bindings(
    .load_dlw_acquisition_inventory = function(...) rlang::cnd_signal(read_error),
    .package = "pipdata"
  )
  expect_error(
    .reload_dlw_acquisition_inventory_state("dlw_gmd_inv"),
    class = "stamp_read_error"
  )
})

test_that("real stamp reloads reconcile prior intended absent and ambiguous states", {
  root <- withr::local_tempdir()
  alias <- paste0("dlw-reconcile-", as.integer(stats::runif(1, 1, 1e9)))
  stamp::st_init(root = root, alias = alias)
  prior <- make_acquisition_inventory()
  intended <- data.table::copy(prior)
  intended[FileName == "ZWE_2021_PICES.dta", data_available := "No"]
  other <- data.table::copy(prior)
  other[FileName == "BOL_2020_EH.dta", data_available := "No"]
  id <- "dlw_reconcile_inventory"
  file <- fs::path_ext_set(id, "qs2")
  reload <- function() {
    versions <- stamp::st_versions(file, alias = alias)
    if (nrow(versions) == 0L) {
      return(list(state = "absent", value = NULL, version_id = NA_character_))
    }
    list(
      state = "present",
      value = pipload::pip_read(id, alias = alias, verbose = FALSE),
      version_id = stamp::st_latest(file, alias = alias)
    )
  }

  absent <- .reconcile_dlw_persistence(
    id, alias, simpleError("payload failed"), intended, NULL, reload,
    .canonicalize_dlw_acquisition_inventory
  )
  expect_true(absent$fact$trustworthy)
  expect_false(absent$fact$success)

  pipload::pip_write(
    prior, id, alias = alias, pk = c("Checksum", "FileName"), verbose = FALSE
  )
  prior_active <- .reconcile_dlw_persistence(
    id, alias, simpleError("sidecar failed"), intended, prior, reload,
    .canonicalize_dlw_acquisition_inventory
  )
  expect_false(prior_active$fact$success)
  expect_true(prior_active$fact$trustworthy)

  pipload::pip_write(
    intended, id, alias = alias, pk = c("Checksum", "FileName"), verbose = FALSE
  )
  intended_active <- .reconcile_dlw_persistence(
    id, alias, simpleError("catalog failed"), intended, prior, reload,
    .canonicalize_dlw_acquisition_inventory
  )
  expect_true(intended_active$fact$success)
  expect_true(intended_active$fact$reconciled)

  pipload::pip_write(
    other, id, alias = alias, pk = c("Checksum", "FileName"), verbose = FALSE
  )
  ambiguous <- .reconcile_dlw_persistence(
    id, alias, simpleError("ambiguous failure"), intended, prior, reload,
    .canonicalize_dlw_acquisition_inventory
  )
  expect_false(ambiguous$fact$trustworthy)
  expect_null(ambiguous$value)
})

test_that("changed checksums reach forced replacement and cached results fail", {
  server <- make_acquisition_candidate(checksum = "new-checksum")
  prior <- make_acquisition_candidate(checksum = "old-checksum")
  prior[, data_available := "Yes"]
  candidate <- .select_dlw_acquisition_candidates(
    server,
    prior,
    check_missing = FALSE
  )
  expect_equal(nrow(candidate), 1L)

  calls <- 0L
  testthat::local_mocked_bindings(
    dlw_get_gmd = function(...) {
      calls <<- calls + 1L
      structure(list(cached = TRUE), class = "dlw_call_list")
    },
    .package = "dlw"
  )
  result <- .acquire_one_gmd(candidate, tempdir(), verbose = FALSE)

  expect_identical(calls, 1L)
  expect_false(result$success)
  expect_identical(result$data_available, "No")
})
