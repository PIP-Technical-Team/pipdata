has_validation_raw_condition <- function(x) {
  if (inherits(x, "condition")) {
    return(TRUE)
  }
  if (!is.list(x)) {
    return(FALSE)
  }
  any(vapply(x, has_validation_raw_condition, logical(1)))
}

make_validation_inventory <- function() {
  data.table::data.table(
    survey_id = c(
      "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL",
      "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
    ),
    pipeline_version = c(2L, 1L),
    latest_version_id = c("z-v2", "b-v1"),
    content_hash = c("z-hash", "b-hash"),
    file_path = c("z.qs2", "b.qs2"),
    status = c("invalid", "valid"),
    data_available = "Yes",
    date_validated = as.POSIXct(
      c("2026-08-26 12:00:00", "2026-08-26 11:00:00"),
      tz = "UTC"
    ),
    Checksum = c("checksum-z", "checksum-b"),
    country_code = c("ZWE", "BOL"),
    survey_acronym = c("PICES", "EH"),
    vermast = c("v02", "v01"),
    veralt = "v01",
    collection = "GMD",
    module = "ALL",
    tool = "TB",
    surveyid_year = c(2021L, 2020L)
  )
}

make_validation_report <- function() {
  data.table::data.table(
    table_name = c(
      "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL",
      "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
    ),
    message = c("missing variable", "checks passed"),
    type = c("error", "success"),
    description = c("required", "complete"),
    module_type = c("all", "gpwg"),
    vermast = c("02", "01"),
    veralt = "01",
    country_code = c("ZWE", "BOL"),
    rf_year = c("2021", "2020"),
    assertion_group = c("schema", "schema")
  )
}

make_validation_acquisition <- function() {
  data.table::data.table(
    Country = c("BOL", "ZWE"),
    Year = c(2020L, 2021L),
    Survey_acronym = c("EH", "PICES"),
    Vermast = c("01", "02"),
    Veralt = "01",
    Module = "ALL",
    Collection = "GMD",
    FileName = c(
      "BOL_2020_EH_V01_M_V01_A_GMD_ALL.dta",
      "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL.dta"
    ),
    Checksum = c("checksum-b", "checksum-z"),
    Ext = "dta",
    data_available = "Yes"
  )
}

make_validation_summary <- function() {
  list(
    n_total = 3L,
    n_valid = 1L,
    n_invalid = 1L,
    n_failed = 1L,
    surveys_valid = "BOL_2020_EH_V01_M_V01_A_GMD_ALL",
    surveys_invalid = "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL",
    surveys_failed = "NGA_2019_LSS_V01_M_V01_A_GMD_ALL"
  )
}

make_validation_fact <- function(id, attempted = FALSE, success = NA) {
  .new_dlw_validation_artifact_fact(
    id = id,
    alias = "dlw_meta",
    attempted = attempted,
    success = success,
    trustworthy = TRUE,
    version_id = NA_character_,
    skipped = if (attempted) TRUE else NA,
    reconciled = FALSE
  )
}

test_that("validation result has the pinned plain-list shape and copies inventory", {
  inventory <- make_validation_inventory()
  inventory_before <- data.table::copy(inventory)
  failures <- .new_dlw_validation_failure(
    survey_id = "NGA_2019_LSS_V01_M_V01_A_GMD_ALL",
    phase = "validation_engine",
    condition = rlang::error_cnd(
      "dlw_validation_engine_error",
      message = "engine failed"
    )
  )

  result <- .new_dlw_validation_result(
    outcome = "partial",
    inventory = inventory,
    summary = make_validation_summary(),
    failures = failures,
    artifacts = list(
      report = make_validation_fact("validation_report", TRUE, TRUE),
      inventory = make_validation_fact("gmd_valid_inv", TRUE, TRUE)
    )
  )

  expect_named(
    result,
    c("stage", "outcome", "inventory", "summary", "failures", "artifacts")
  )
  expect_identical(result$stage, "validation")
  expect_identical(class(result), "list")
  expect_identical(result$summary, make_validation_summary())
  expect_named(result$artifacts, c("report", "inventory"))
  expect_false(has_validation_raw_condition(result))

  inventory[, status := "valid"]
  expect_identical(result$inventory, inventory_before)
})

test_that("validation no-work result retains trustworthy prior artifacts", {
  result <- .new_dlw_validation_result(
    outcome = "no_work",
    inventory = make_validation_inventory(),
    summary = list(
      n_total = 0L,
      n_valid = 0L,
      n_invalid = 0L,
      n_failed = 0L,
      surveys_valid = character(),
      surveys_invalid = character(),
      surveys_failed = character()
    ),
    failures = .new_dlw_validation_failure(),
    artifacts = list(
      report = make_validation_fact("validation_report"),
      inventory = make_validation_fact("gmd_valid_inv")
    )
  )

  expect_identical(result$outcome, "no_work")
  expect_equal(result$summary$n_total, 0L)
  expect_true(all(vapply(
    result$artifacts,
    function(x) isTRUE(x$trustworthy),
    logical(1)
  )))
})

test_that("validation failure helper compacts condition and workflow failures", {
  empty <- .new_dlw_validation_failure()
  expect_s3_class(empty, "data.table")
  expect_named(
    empty,
    c("survey_id", "phase", "error_type", "condition_msg")
  )
  expect_equal(nrow(empty), 0L)

  typed <- .new_dlw_validation_failure(
    survey_id = "BOL_2020_EH_V01_M_V01_A_GMD_ALL",
    phase = "artifact_info_fail",
    condition = rlang::error_cnd(
      "stamp_sidecar_error",
      message = "sidecar unreadable"
    )
  )
  expect_identical(typed$error_type, "stamp_sidecar_error")
  expect_identical(typed$condition_msg, "sidecar unreadable")
  expect_false(has_validation_raw_condition(typed))

  logger <- .new_dlw_validation_failure(
    phase = "log_emit",
    error_type = "logger_error",
    condition_msg = "validation completion log failed"
  )
  expect_true(is.na(logger$survey_id))
  expect_identical(logger$phase, "log_emit")
  expect_error(
    .new_dlw_validation_failure(
      survey_id = "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
    ),
    class = "pipdata_dlw_contract_error"
  )
})

test_that("validation result rejects invalid arithmetic and overlapping IDs", {
  artifacts <- list(
    report = make_validation_fact("validation_report"),
    inventory = make_validation_fact("gmd_valid_inv")
  )
  invalid_arithmetic <- make_validation_summary()
  invalid_arithmetic$n_total <- 4L
  overlapping <- make_validation_summary()
  overlapping$surveys_failed <- overlapping$surveys_valid

  expect_error(
    .new_dlw_validation_result(
      outcome = "partial",
      inventory = make_validation_inventory(),
      summary = invalid_arithmetic,
      failures = .new_dlw_validation_failure(),
      artifacts = artifacts
    ),
    class = "pipdata_dlw_contract_error"
  )
  expect_error(
    .new_dlw_validation_result(
      outcome = "no_work",
      inventory = make_validation_inventory(),
      summary = make_validation_summary(),
      failures = .new_dlw_validation_failure(
        survey_id = "NGA_2019_LSS_V01_M_V01_A_GMD_ALL",
        phase = "validation_engine",
        error_type = "validation_engine_error",
        condition_msg = "engine failed"
      ),
      artifacts = artifacts
    ),
    class = "pipdata_dlw_contract_error"
  )
  expect_error(
    .new_dlw_validation_result(
      outcome = "partial",
      inventory = make_validation_inventory(),
      summary = overlapping,
      failures = .new_dlw_validation_failure(),
      artifacts = artifacts
    ),
    class = "pipdata_dlw_contract_error"
  )
  expect_error(
    .new_dlw_validation_result(
      outcome = "no_work",
      inventory = make_validation_inventory(),
      summary = list(
        n_total = 0L,
        n_valid = 0L,
        n_invalid = 0L,
        n_failed = 0L,
        surveys_valid = character(),
        surveys_invalid = character(),
        surveys_failed = character()
      ),
      failures = .new_dlw_validation_failure(),
      artifacts = list(
        report = simpleError("retained"),
        inventory = make_validation_fact("gmd_valid_inv")
      )
    ),
    class = "pipdata_dlw_contract_error"
  )

  completed <- list(
    n_total = 1L,
    n_valid = 1L,
    n_invalid = 0L,
    n_failed = 0L,
    surveys_valid = "BOL_2020_EH_V01_M_V01_A_GMD_ALL",
    surveys_invalid = character(),
    surveys_failed = character()
  )
  expect_error(
    .new_dlw_validation_result(
      outcome = "success",
      inventory = make_validation_inventory()[1L],
      summary = completed,
      failures = .new_dlw_validation_failure(),
      artifacts = list(
        report = .dlw_validation_no_write_fact(
          "validation_report",
          success = FALSE
        ),
        inventory = make_validation_fact("gmd_valid_inv", TRUE, TRUE)
      )
    ),
    class = "pipdata_dlw_contract_error"
  )
})

test_that("validation completion metadata has the exact terminal schema", {
  completion <- .new_dlw_validation_completion_logmeta(
    "partial",
    make_validation_summary()
  )
  expect_identical(
    names(completion),
    c(
      "info", "phase", "outcome", "n_total", "n_valid", "n_invalid",
      "n_failed", "surveys_valid", "surveys_invalid", "surveys_failed"
    )
  )
  completion$extra <- TRUE
  expect_error(
    .validate_dlw_validation_completion_logmeta(completion),
    class = "pipdata_dlw_contract_error"
  )
  completion$extra <- NULL
  completion$surveys_failed <- completion$surveys_valid
  expect_error(
    .validate_dlw_validation_completion_logmeta(completion),
    class = "pipdata_dlw_contract_error"
  )
})

test_that("validation artifact facts implement every truth-table state", {
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
    .new_dlw_validation_artifact_fact(
      id = "validation_report",
      alias = "dlw_meta",
      attempted = state[[1]],
      success = state[[2]],
      trustworthy = state[[3]],
      version_id = state[[4]],
      skipped = state[[5]],
      reconciled = state[[6]]
    )
  })

  expect_length(facts, 9L)
  expect_true(all(vapply(facts, is.list, logical(1))))
  expect_error(
    .new_dlw_validation_artifact_fact(
      id = "validation_report",
      alias = "dlw_meta",
      attempted = TRUE,
      success = FALSE,
      trustworthy = TRUE,
      version_id = "prior-v1",
      skipped = TRUE,
      reconciled = TRUE
    ),
    class = "pipdata_dlw_contract_error"
  )
})

test_that("validation inventory canonicalization is PK and order stable", {
  raw <- make_validation_inventory()
  raw_before <- data.table::copy(raw)
  reloaded <- raw[2:1, c(
    "surveyid_year", "tool", "module", "collection", "veralt", "vermast",
    "survey_acronym", "country_code", "Checksum", "date_validated", "data_available",
    "status", "file_path", "content_hash", "latest_version_id",
    "pipeline_version", "survey_id"
  )]
  reloaded <- stamp::st_with_pk(reloaded, "survey_id")
  data.table::setkey(reloaded, survey_id)
  data.table::setindexv(reloaded, "Checksum")

  canonical_raw <- .canonicalize_dlw_validation_inventory(raw)
  canonical_reloaded <- .canonicalize_dlw_validation_inventory(reloaded)

  expect_identical(canonical_raw, canonical_reloaded)
  expect_identical(
    names(canonical_raw),
    c(
      "survey_id", "pipeline_version", "latest_version_id", "content_hash",
      "file_path", "status", "data_available", "date_validated", "Checksum",
      "collection", "country_code", "module", "survey_acronym",
      "surveyid_year", "tool", "veralt", "vermast"
    )
  )
  expect_identical(
    attr(canonical_raw, "stamp_pk", exact = TRUE),
    list(keys = "survey_id")
  )
  expect_null(attr(canonical_raw, ".internal.selfref", exact = TRUE))
  expect_null(attr(canonical_raw, "sorted", exact = TRUE))
  expect_null(attr(canonical_raw, "index", exact = TRUE))
  expect_identical(raw, raw_before)
})

test_that("validation report canonicalization is row and column stable", {
  raw <- make_validation_report()
  raw_before <- data.table::copy(raw)
  reloaded <- raw[2:1, c(
    "assertion_group", "rf_year", "country_code", "veralt", "vermast",
    "module_type", "description", "type", "message", "table_name"
  )]
  reloaded[, table_name := factor(table_name)]
  data.table::setkey(reloaded, type)
  data.table::setindexv(reloaded, "table_name")

  canonical_raw <- .canonicalize_dlw_validation_report(raw)
  canonical_reloaded <- .canonicalize_dlw_validation_report(reloaded)

  expect_identical(canonical_raw, canonical_reloaded)
  expect_identical(
    names(canonical_raw),
    c(
      "table_name", "message", "type", "description", "module_type",
      "vermast", "veralt", "country_code", "rf_year", "assertion_group"
    )
  )
  expect_null(attr(canonical_raw, "stamp_pk", exact = TRUE))
  expect_null(attr(canonical_raw, ".internal.selfref", exact = TRUE))
  expect_identical(raw, raw_before)
})

test_that("validation report canonical ordering breaks ties on every column", {
  tied <- make_validation_report()[rep(1L, 3L)]
  tied[, `:=`(
    module_type = c("group", "all", "bin"),
    vermast = c("03", "01", "02"),
    veralt = c("02", "03", "01"),
    country_code = c("ZWE", "BOL", "COL"),
    rf_year = c("2022", "2020", "2021"),
    assertion_group = c("z", "x", "y")
  )]
  shuffled <- tied[c(3L, 1L, 2L), rev(names(tied)), with = FALSE]

  expect_identical(
    .canonicalize_dlw_validation_report(tied),
    .canonicalize_dlw_validation_report(shuffled)
  )
})

test_that("DLW reconciler uses validation-specific canonical equality", {
  intended <- make_validation_inventory()
  prior <- data.table::copy(intended)
  prior[
    survey_id == "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL",
    status := "valid"
  ]

  intended_active <- .reconcile_dlw_persistence(
    id = "gmd_valid_inv",
    alias = "dlw_meta",
    write_result = list(version_id = NULL, skipped = FALSE),
    intended = intended,
    prior = prior,
    reload = function() list(
      state = "present",
      value = intended[2:1, rev(names(intended)), with = FALSE],
      version_id = "validation-v2"
    ),
    canonicalize = .canonicalize_dlw_validation_inventory
  )
  prior_active <- .reconcile_dlw_persistence(
    id = "gmd_valid_inv",
    alias = "dlw_meta",
    write_result = simpleError("catalog update failed"),
    intended = intended,
    prior = prior,
    prior_version_id = "validation-v1",
    reload = function() list(
      state = "present",
      value = prior,
      version_id = NA_character_
    ),
    canonicalize = .canonicalize_dlw_validation_inventory
  )

  expect_identical(intended_active$fact$success, TRUE)
  expect_identical(intended_active$fact$trustworthy, TRUE)
  expect_identical(intended_active$fact$reconciled, TRUE)
  expect_identical(prior_active$value, prior)
  expect_identical(prior_active$fact$success, FALSE)
  expect_identical(prior_active$fact$trustworthy, TRUE)
  expect_identical(prior_active$fact$version_id, "validation-v1")
  expect_false(has_validation_raw_condition(prior_active))
})

test_that("completed validation schema prunes only canonical retry rows", {
  completed <- make_validation_inventory()
  completed[, pipeline_version := as.numeric(pipeline_version)]
  retry <- completed[1L]
  retry[, `:=`(
    latest_version_id = "",
    content_hash = "",
    file_path = "",
    status = "",
    data_available = "No"
  )]

  normalized <- .normalize_dlw_validation_inventory(
    data.table::rbindlist(list(completed, retry))
  )

  expect_identical(normalized$pipeline_version, c(1L, 2L))
  expect_setequal(normalized$survey_id, completed$survey_id)
  expect_true(all(normalized$data_available == "Yes"))
  expect_true(all(normalized$status %in% c("valid", "invalid")))
  expect_true(all(c("collection", "tool") %in% names(normalized)))

  malformed <- data.table::copy(completed)
  malformed[1L, tool := ""]
  expect_error(
    .normalize_dlw_validation_inventory(malformed),
    class = "pipdata_dlw_inventory_schema_error"
  )
  malformed_retry <- data.table::copy(retry)
  malformed_retry[, latest_version_id := "unexpected-version"]
  expect_error(
    .normalize_dlw_validation_inventory(malformed_retry),
    class = "pipdata_dlw_inventory_schema_error"
  )
})

test_that("historical scan reads every version and retains per-survey maxima", {
  history <- make_validation_inventory()[1L]
  reads <- list()
  testthat::local_mocked_bindings(
    st_versions = function(path, alias) {
      expect_identical(as.character(path), "gmd_valid_inv.qs2")
      expect_identical(alias, "dlw_meta")
      data.table::data.table(
        version_id = c("history-v1", "history-v2")
      )
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_read = function(id, version, alias, verbose) {
      reads[[length(reads) + 1L]] <<- list(
        id = id, version = version, alias = alias, verbose = verbose
      )
      row <- data.table::copy(history)
      row[, pipeline_version := if (version == "history-v1") 3L else 5L]
      row
    },
    .package = "pipload"
  )

  maxima <- .scan_dlw_validation_history(verbose = TRUE)

  expect_length(reads, 2L)
  expect_true(all(vapply(reads, `[[`, character(1), "alias") == "dlw_meta"))
  expect_identical(maxima$pipeline_version, 5L)
  expect_identical(maxima$survey_id, history$survey_id)
})

test_that("historical scan blocks unreadable and malformed listed versions", {
  history <- make_validation_inventory()[1L]
  testthat::local_mocked_bindings(
    st_versions = function(...) data.table::data.table(
      version_id = c("history-v1", "history-unreadable")
    ),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_read = function(id, version, ...) {
      if (version == "history-unreadable") {
        rlang::abort("unreadable history", class = "stamp_read_error")
      }
      history
    },
    .package = "pipload"
  )
  expect_error(
    .scan_dlw_validation_history(),
    class = "pipdata_dlw_history_load_error"
  )

  testthat::local_mocked_bindings(
    st_versions = function(...) data.table::data.table(
      version_id = c("history-v1", NA_character_)
    ),
    .package = "stamp"
  )
  expect_error(
    .scan_dlw_validation_history(),
    class = "pipdata_dlw_history_load_error"
  )

  testthat::local_mocked_bindings(
    st_versions = function(...) data.table::data.table(
      version_id = "history-malformed"
    ),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_read = function(...) data.table::data.table(survey_id = "bad"),
    .package = "pipload"
  )
  expect_error(
    .scan_dlw_validation_history(),
    class = "pipdata_dlw_inventory_schema_error"
  )
})

test_that("malformed version catalogs never prove validation artifacts absent", {
  testthat::local_mocked_bindings(
    pip_read = function(...) {
      rlang::abort("payload unreadable", class = "stamp_read_error")
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_versions = function(...) data.table::data.table(other = character()),
    .package = "stamp"
  )

  expect_error(
    .load_dlw_validation_artifact_state(
      "validation_report",
      .normalize_dlw_validation_report_durable,
      FALSE
    ),
    class = "stamp_read_error"
  )
  expect_error(
    .load_current_dlw_validation_inventory(FALSE),
    class = "stamp_read_error"
  )
})

test_that("authoritative validation state prunes stale keys and retries by absence", {
  acquisition <- make_validation_acquisition()
  prior <- make_validation_inventory()
  prior[survey_id == "BOL_2020_EH_V01_M_V01_A_GMD_ALL", Checksum := "old"]
  prior[
    survey_id == "BOL_2020_EH_V01_M_V01_A_GMD_ALL",
    pipeline_version := 5L
  ]
  historical <- prior[, .(
    pipeline_version = max(pipeline_version)
  ), by = survey_id]

  state <- .reconcile_dlw_validation_inventory(
    acquisition = acquisition,
    prior = prior,
    historical_max = historical
  )

  expect_identical(nrow(state$inventory), 1L)
  expect_identical(
    state$inventory$survey_id,
    "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL"
  )
  expect_identical(
    state$candidates$survey_id,
    "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
  )
  expect_identical(state$candidates$next_pipeline_version, 6L)
})

test_that("active validation duplicate ties resolve only when identical", {
  acquisition <- make_validation_acquisition()[1L]
  prior <- make_validation_inventory()[
    survey_id == "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
  ]
  prior[, Checksum := acquisition$Checksum[[1L]]]
  history <- prior[, .(survey_id, pipeline_version)]
  exact <- data.table::rbindlist(list(prior, prior))

  resolved <- .reconcile_dlw_validation_inventory(
    acquisition,
    exact,
    history
  )
  expect_identical(nrow(resolved$inventory), 1L)

  conflicting <- data.table::rbindlist(list(prior, prior))
  conflicting[2L, content_hash := "different-hash"]
  expect_error(
    .reconcile_dlw_validation_inventory(
      acquisition,
      conflicting,
      history
    ),
    class = "pipdata_dlw_inventory_schema_error"
  )
})

test_that("validation worker isolates execution failures and persists invalid data", {
  candidate <- make_validation_acquisition()[1L]
  observed_verbose <- NULL
  testthat::local_mocked_bindings(
    load_dlw_data = function(id_name, verbose) {
      observed_verbose <<- verbose
      data.table::data.table(welfare = 1)
    },
    survey_id_to_vars = function(x, ...) {
      parsed <- data.table::data.table(
        survey_id = x$survey_id,
        country_code = "BOL",
        surveyid_year = 2020,
        survey_acronym = "EH",
        vermast = "v01",
        veralt = "v01",
        collection = "GMD",
        module = "ALL",
        tool = "TB"
      )
      merge(x, parsed, by = "survey_id")
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_info = function(...) list(
      catalog = list(latest_version_id = "dlw-v1"),
      sidecar = list(content_hash = "hash-1", path = "survey.qs2")
    ),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    dlw_validation_engine = function(out, survey_id, module) {
      data.table::data.table(
        table_name = survey_id,
        message = "invalid",
        type = "error"
      )
    },
    .validation_report_for_survey = function(survey_id) {
      report <- make_validation_report()[1L]
      report[, table_name := survey_id]
      report
    },
    .package = "pipdata"
  )

  invalid <- .validate_one_gmd(
    candidate,
    next_pipeline_version = 6L,
    verbose = FALSE
  )
  expect_identical(invalid$status, "invalid")
  expect_identical(invalid$inventory_row$pipeline_version, 6L)
  expect_equal(nrow(invalid$failure), 0L)
  expect_gt(nrow(invalid$report_rows), 0L)
  expect_identical(observed_verbose, FALSE)

  testthat::local_mocked_bindings(
    load_dlw_data = function(...) rlang::abort("load failed"),
    .package = "pipload"
  )
  failed <- .validate_one_gmd(
    candidate,
    next_pipeline_version = 6L,
    verbose = FALSE
  )
  expect_identical(failed$status, "failed")
  expect_null(failed$inventory_row)
  expect_null(failed$report_rows)
  expect_identical(failed$failure$phase, "load")
})

test_that("validation worker rejects malformed and disagreeing engine types", {
  candidate <- make_validation_acquisition()[1L]
  engine_result <- data.table::data.table(type = "success")
  report_type <- "success"
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) data.table::data.table(welfare = 1),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_info = function(...) list(
      catalog = list(latest_version_id = "v1"),
      sidecar = list(content_hash = "h1", path = "survey.qs2")
    ),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    dlw_validation_engine = function(...) engine_result,
    .validation_report_for_survey = function(survey_id) {
      report <- make_validation_report()[1L]
      report[, `:=`(table_name = survey_id, type = report_type)]
      report
    },
    .package = "pipdata"
  )

  malformed <- list(
    data.table::data.table(type = character()),
    data.table::data.table(type = NA_character_),
    data.table::data.table(type = "unknown"),
    data.table::data.table(type = 1L)
  )
  for (value in malformed) {
    engine_result <- value
    result <- .validate_one_gmd(candidate, 1L, FALSE)
    expect_identical(result$failure$phase, "validation_engine")
    expect_null(result$report_rows)
  }

  engine_result <- data.table::data.table(type = "success")
  report_type <- "error"
  disagreement <- .validate_one_gmd(candidate, 1L, FALSE)
  expect_identical(disagreement$failure$phase, "validation_engine")
  expect_match(disagreement$failure$condition_msg, "do not agree")
})

test_that("validation report normalization enforces schema and exact coverage", {
  inventory <- make_validation_inventory()
  report <- make_validation_report()
  report <- data.table::rbindlist(list(report, report[1L]))
  report[, rf_year := as.integer(rf_year)]

  normalized <- .normalize_dlw_validation_report(report)
  expect_identical(nrow(normalized), 2L)
  expect_type(normalized$rf_year, "character")
  expect_silent(.assert_dlw_validation_report_consistency(inventory, normalized))
  durable <- .normalize_dlw_validation_report_durable(report)
  expect_identical(nrow(durable), 3L)
  expect_false(.dlw_validation_artifact_unchanged(
    normalized,
    durable,
    .canonicalize_dlw_validation_report
  ))

  expect_error(
    .assert_dlw_validation_report_consistency(inventory, normalized[-1L]),
    class = "pipdata_dlw_report_consistency_error"
  )
  expect_error(
    .normalize_dlw_validation_report(report[, description := NULL]),
    class = "pipdata_dlw_report_schema_error"
  )

  incompatible <- data.table::copy(report)
  incompatible[, assertion_group := 1L]
  expect_error(
    .merge_dlw_validation_reports(report, incompatible),
    class = "pipdata_dlw_report_schema_error"
  )
})

test_that("optional report columns enforce coercion-relevant attributes", {
  prior <- make_validation_report()[1L]
  current <- make_validation_report()[2L]

  prior[, elapsed := as.difftime(1, units = "secs")]
  current[, elapsed := as.difftime(1, units = "mins")]
  expect_error(
    .merge_dlw_validation_reports(prior, current),
    class = "pipdata_dlw_report_schema_error"
  )

  prior[, when := structure(as.POSIXct("2026-01-01", tz = "UTC"), tzone = "UTC")]
  current[, when := structure(
    as.POSIXct("2026-01-01", tz = "America/New_York"),
    tzone = "America/New_York"
  )]
  expect_error(
    .merge_dlw_validation_reports(prior[, elapsed := NULL], current[, elapsed := NULL]),
    class = "pipdata_dlw_report_schema_error"
  )

  prior[, rank := ordered("low", levels = c("low", "high"))]
  current[, rank := ordered("low", levels = c("high", "low"))]
  expect_error(
    .merge_dlw_validation_reports(prior[, when := NULL], current[, when := NULL]),
    class = "pipdata_dlw_report_schema_error"
  )

  additive <- make_validation_report()[1L]
  additive[, raw_flag := as.raw(1L)]
  expect_error(
    .merge_dlw_validation_reports(make_validation_report()[2L], additive),
    class = "pipdata_dlw_report_schema_error"
  )
})

test_that("report-list assembly aligns schemas once and preserves typed missing", {
  first <- make_validation_report()[1L]
  second <- make_validation_report()[2L]
  third <- data.table::copy(first)
  first[, elapsed := as.difftime(1, units = "secs")]
  third[, elapsed := as.difftime(2, units = "secs")]

  combined <- .merge_dlw_validation_report_list(list(first, second, third))

  expect_identical(nrow(combined), 3L)
  expect_s3_class(combined$elapsed, "difftime")
  expect_identical(attr(combined$elapsed, "units"), "secs")
  expect_true(is.na(combined[table_name == second$table_name, elapsed]))
})

test_that("per-survey report extraction ignores unrelated accumulator rows", {
  prior <- pd_env_get("validation_report")
  withr::defer({
    if (is.null(prior)) pd_env_rm("validation_report") else pd_env_set(
      "validation_report",
      prior
    )
  })
  target <- make_validation_report()[1L, .(
    table_name, message, type, description
  )]
  unrelated <- data.table::copy(target)
  unrelated[, table_name := NA_character_]
  pd_env_set(
    "validation_report",
    data.table::rbindlist(list(target, unrelated))
  )
  testthat::local_mocked_bindings(
    get_validation_report = function() rlang::abort("full report was rescanned"),
    .package = "pipdata"
  )

  rows <- .validation_report_for_survey(target$table_name)

  expect_identical(nrow(rows), 1L)
  expect_identical(rows$table_name, target$table_name)
  expect_true(all(.dlw_validation_report_columns %in% names(rows)))
})

test_that("validation core routes custom inventory and commits report first", {
  acquisition <- make_validation_acquisition()[1L]
  history <- data.table::data.table(
    survey_id = "BOL_2020_EH_V01_M_V01_A_GMD_ALL",
    pipeline_version = 5L
  )
  observed_id <- NULL
  observed_next <- NULL
  write_order <- character()
  events <- list()

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(),
      dlw_inventory = tempdir(),
      dlw_metadata = tempdir()
    ),
    log_info = function(message, name, logmeta = NULL, ...) {
      events <<- c(events, list(logmeta))
      invisible(TRUE)
    },
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(id, ...) {
      observed_id <<- id
      acquisition
    },
    .load_dlw_validation_artifact_state = function(id, ...) list(
      state = "absent", value = NULL, version_id = NA_character_
    ),
    .scan_dlw_validation_history = function(...) history,
    .validate_one_gmd = function(candidate, next_pipeline_version, verbose) {
      expect_identical(verbose, FALSE)
      observed_next <<- next_pipeline_version
      row <- make_validation_inventory()[
        survey_id == "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
      ]
      row[, `:=`(
        pipeline_version = next_pipeline_version,
        Checksum = candidate$Checksum[[1L]]
      )]
      report <- make_validation_report()[
        table_name == "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
      ]
      list(
        survey_id = row$survey_id[[1L]],
        status = "valid",
        inventory_row = row,
        report_rows = report,
        failure = .new_dlw_validation_failure()
      )
    },
    .persist_dlw_validation_artifact = function(intended, id, ...) {
      write_order <<- c(write_order, id)
      list(
        value = data.table::copy(intended),
        fact = .new_dlw_validation_artifact_fact(
          id = id,
          alias = "dlw_meta",
          attempted = TRUE,
          success = TRUE,
          trustworthy = TRUE,
          version_id = paste0(id, "-v1"),
          skipped = FALSE,
          reconciled = FALSE
        )
      )
    },
    .package = "pipdata"
  )

  visible <- withVisible(.pipdata_validate_gmd_core(
    acquisition_inventory_id = "custom_gmd_inv",
    verbose = FALSE
  ))

  expect_false(visible$visible)
  expect_identical(observed_id, "custom_gmd_inv")
  expect_identical(observed_next, 6L)
  expect_identical(write_order, c("validation_report", "gmd_valid_inv"))
  expect_identical(visible$value$outcome, "success")
  expect_identical(visible$value$inventory$pipeline_version, 6L)
  completion <- Filter(
    function(x) identical(x$phase, "complete"),
    events
  )
  expect_length(completion, 1L)
  expect_identical(completion[[1L]]$n_valid, 1L)
})

test_that("retry succeeds when report is unchanged and inventory is recovered", {
  acquisition <- make_validation_acquisition()[1L]
  report <- make_validation_report()[
    table_name == "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
  ]
  writes <- character()
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(), dlw_inventory = tempdir(), dlw_metadata = tempdir()
    ),
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) acquisition,
    .load_dlw_validation_artifact_state = function(id, ...) {
      if (id == "validation_report") {
        return(list(state = "present", value = report, version_id = "r-v1"))
      }
      list(state = "absent", value = NULL, version_id = NA_character_)
    },
    .scan_dlw_validation_history = function(...) data.table::data.table(
      survey_id = character(), pipeline_version = integer()
    ),
    .validate_one_gmd = function(candidate, next_pipeline_version, verbose) {
      row <- make_validation_inventory()[
        survey_id == "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
      ]
      row[, `:=`(
        pipeline_version = next_pipeline_version,
        Checksum = candidate$Checksum[[1L]]
      )]
      list(
        survey_id = row$survey_id[[1L]],
        status = "valid",
        inventory_row = row,
        report_rows = report,
        failure = .new_dlw_validation_failure()
      )
    },
    .persist_dlw_validation_artifact = function(intended, id, ...) {
      writes <<- c(writes, id)
      list(
        value = data.table::copy(intended),
        fact = .new_dlw_validation_artifact_fact(
          id = id,
          alias = "dlw_meta",
          attempted = TRUE,
          success = TRUE,
          trustworthy = TRUE,
          version_id = "inventory-recovered-v1",
          skipped = FALSE,
          reconciled = TRUE
        )
      )
    },
    .package = "pipdata"
  )

  result <- .pipdata_validate_gmd_core(verbose = FALSE)

  expect_identical(writes, "gmd_valid_inv")
  expect_identical(result$outcome, "success")
  expect_identical(result$artifacts$report$success, NA)
  expect_true(result$artifacts$report$trustworthy)
  expect_true(result$artifacts$inventory$reconciled)
})

test_that("validation core blocks inventory when report intent is unverified", {
  acquisition <- make_validation_acquisition()[1L]
  writes <- character()
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(), dlw_inventory = tempdir(), dlw_metadata = tempdir()
    ),
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) acquisition,
    .load_dlw_validation_artifact_state = function(id, ...) list(
      state = "absent", value = NULL, version_id = NA_character_
    ),
    .scan_dlw_validation_history = function(...) data.table::data.table(
      survey_id = character(), pipeline_version = integer()
    ),
    .validate_one_gmd = function(candidate, next_pipeline_version, verbose) {
      expect_identical(verbose, FALSE)
      row <- make_validation_inventory()[
        survey_id == "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
      ]
      row[, `:=`(
        pipeline_version = next_pipeline_version,
        Checksum = candidate$Checksum[[1L]]
      )]
      list(
        survey_id = row$survey_id[[1L]],
        status = "valid",
        inventory_row = row,
        report_rows = make_validation_report()[
          table_name == row$survey_id[[1L]]
        ],
        failure = .new_dlw_validation_failure()
      )
    },
    .persist_dlw_validation_artifact = function(intended, id, ...) {
      writes <<- c(writes, id)
      list(
        value = NULL,
        fact = .new_dlw_validation_artifact_fact(
          id = id,
          alias = "dlw_meta",
          attempted = TRUE,
          success = FALSE,
          trustworthy = TRUE,
          version_id = NA_character_,
          skipped = FALSE,
          reconciled = TRUE
        )
      )
    },
    .package = "pipdata"
  )

  result <- .pipdata_validate_gmd_core(verbose = FALSE)
  expect_identical(writes, "validation_report")
  expect_identical(result$outcome, "failed")
  expect_identical(result$artifacts$inventory$attempted, FALSE)
  expect_identical(result$artifacts$inventory$success, FALSE)
})

test_that("validation no-work repairs orphan report rows only", {
  acquisition <- make_validation_acquisition()[0L]
  orphan <- make_validation_report()[1L]
  writes <- character()
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(), dlw_inventory = tempdir(), dlw_metadata = tempdir()
    ),
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) acquisition,
    .load_dlw_validation_artifact_state = function(id, ...) {
      if (id == "validation_report") {
        return(list(state = "present", value = orphan, version_id = "r-v1"))
      }
      list(state = "absent", value = NULL, version_id = NA_character_)
    },
    .scan_dlw_validation_history = function(...) data.table::data.table(
      survey_id = character(), pipeline_version = integer()
    ),
    .persist_dlw_validation_artifact = function(intended, id, ...) {
      writes <<- c(writes, id)
      list(
        value = data.table::copy(intended),
        fact = .new_dlw_validation_artifact_fact(
          id = id,
          alias = "dlw_meta",
          attempted = TRUE,
          success = TRUE,
          trustworthy = TRUE,
          version_id = paste0(id, "-v2"),
          skipped = FALSE,
          reconciled = FALSE
        )
      )
    },
    .package = "pipdata"
  )

  result <- .pipdata_validate_gmd_core(verbose = FALSE)
  expect_identical(result$outcome, "no_work")
  expect_identical(writes, "validation_report")
  expect_identical(result$artifacts$inventory$attempted, FALSE)
})

test_that("failed attempt boundary suppresses all later validation logs", {
  acquisition <- make_validation_acquisition()[0L]
  info_calls <- 0L
  error_calls <- 0L
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(), dlw_inventory = tempdir(), dlw_metadata = tempdir()
    ),
    log_info = function(...) {
      info_calls <<- info_calls + 1L
      rlang::abort("logger failed")
    },
    log_error = function(...) {
      error_calls <<- error_calls + 1L
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) acquisition,
    .load_dlw_validation_artifact_state = function(id, ...) list(
      state = "absent", value = NULL, version_id = NA_character_
    ),
    .scan_dlw_validation_history = function(...) data.table::data.table(
      survey_id = character(), pipeline_version = integer()
    ),
    .package = "pipdata"
  )

  result <- .pipdata_validate_gmd_core(verbose = FALSE)
  expect_identical(info_calls, 1L)
  expect_identical(error_calls, 0L)
  expect_identical(result$outcome, "failed")
  expect_identical(result$failures$phase, "log_emit")
})

test_that("validation worker does not catch interrupts", {
  candidate <- make_validation_acquisition()[1L]
  interrupt <- structure(
    list(message = "cancelled"),
    class = c("interrupt", "condition")
  )
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) rlang::cnd_signal(interrupt),
    .package = "pipload"
  )

  caught <- tryCatch(
    .validate_one_gmd(candidate, 1L, FALSE),
    interrupt = function(e) e
  )
  expect_s3_class(caught, "interrupt")
})

test_that("validation worker isolates metadata engine and row failures", {
  candidate <- make_validation_acquisition()[1L]
  info_mode <- "error"
  engine_mode <- "valid"
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) data.table::data.table(welfare = 1),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_info = function(...) {
      if (info_mode == "error") {
        rlang::abort("metadata failed")
      }
      list(
        catalog = list(latest_version_id = if (info_mode == "missing") "" else "v1"),
        sidecar = list(content_hash = "h1", path = "survey.qs2")
      )
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    dlw_validation_engine = function(...) {
      if (engine_mode == "error") {
        rlang::abort("engine failed")
      }
      data.table::data.table(type = "success")
    },
    .validation_report_for_survey = function(survey_id) {
      report <- make_validation_report()[2L]
      report[, table_name := survey_id]
      report
    },
    .package = "pipdata"
  )

  metadata_failed <- .validate_one_gmd(candidate, 1L, FALSE)
  expect_identical(metadata_failed$failure$phase, "artifact_info_fail")
  expect_null(metadata_failed$inventory_row)

  info_mode <- "valid"
  engine_mode <- "error"
  engine_failed <- .validate_one_gmd(candidate, 1L, FALSE)
  expect_identical(engine_failed$failure$phase, "validation_engine")
  expect_null(engine_failed$report_rows)

  engine_mode <- "valid"
  info_mode <- "missing"
  row_failed <- .validate_one_gmd(candidate, 1L, FALSE)
  expect_identical(row_failed$failure$phase, "inventory_row")
  expect_null(row_failed$inventory_row)
})

test_that("validation worker identifies unavailable report rows", {
  candidate <- make_validation_acquisition()[1L]
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) data.table::data.table(welfare = 1),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_info = function(...) list(
      catalog = list(latest_version_id = "v1"),
      sidecar = list(content_hash = "h1", path = "survey.qs2")
    ),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    dlw_validation_engine = function(...) {
      data.table::data.table(type = "success")
    },
    .validation_report_for_survey = function(...) {
      rlang::abort("report rows missing")
    },
    .package = "pipdata"
  )

  result <- .validate_one_gmd(candidate, 1L, FALSE)

  expect_identical(result$status, "failed")
  expect_identical(result$failure$phase, "report_unavailable")
  expect_null(result$inventory_row)
  expect_null(result$report_rows)
})

test_that("malformed worker results become isolated inventory-row failures", {
  candidate <- make_validation_acquisition()[1L]
  candidate[, survey_id := fs::path_ext_remove(FileName)]
  malformed <- .normalize_dlw_validation_worker_result(
    list(status = "valid"),
    candidate
  )
  expect_identical(malformed$status, "failed")
  expect_identical(malformed$failure$phase, "inventory_row")
  expect_identical(
    malformed$failure$error_type,
    "validation_worker_result_error"
  )
})

test_that("public validation arguments abort before release discovery", {
  release_calls <- 0L
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) {
      release_calls <<- release_calls + 1L
      invisible(TRUE)
    },
    .package = "pipfun"
  )
  expect_error(
    pipdata_validate_gmd(verbose = NA),
    class = "pipdata_dlw_argument_error"
  )
  expect_error(
    .pipdata_validate_gmd_core(acquisition_inventory_id = "", verbose = FALSE),
    class = "pipdata_dlw_argument_error"
  )
  expect_identical(release_calls, 0L)
})

test_that("pipdata_validate_gmd retains exact public formals and defaults", {
  expect_identical(
    formals(pipdata_validate_gmd),
    as.pairlist(alist(verbose = getOption("pipdata.verbose", default = TRUE)))
  )
})

test_that("public validation routes the default acquisition inventory ID", {
  observed <- NULL
  testthat::local_mocked_bindings(
    .pipdata_validate_gmd_core = function(
        acquisition_inventory_id,
        verbose
    ) {
      observed <<- list(
        acquisition_inventory_id = acquisition_inventory_id,
        verbose = verbose
      )
      "result"
    },
    .package = "pipdata"
  )
  visible <- withVisible(pipdata_validate_gmd(verbose = FALSE))
  expect_false(visible$visible)
  expect_identical(visible$value, "result")
  expect_identical(
    observed,
    list(acquisition_inventory_id = "dlw_gmd_inv", verbose = FALSE)
  )
})

test_that("completed inventory without report coverage fails before mapping", {
  acquisition <- make_validation_acquisition()[1L]
  inventory <- make_validation_inventory()[
    survey_id == "BOL_2020_EH_V01_M_V01_A_GMD_ALL"
  ]
  inventory[, Checksum := acquisition$Checksum[[1L]]]
  worker_calls <- 0L
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) list(
      dlw_data = tempdir(), dlw_inventory = tempdir(), dlw_metadata = tempdir()
    ),
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    check_directory = function(...) invisible(TRUE),
    .load_dlw_acquisition_inventory = function(...) acquisition,
    .load_dlw_validation_artifact_state = function(id, ...) {
      if (id == "gmd_valid_inv") {
        return(list(state = "present", value = inventory, version_id = "i-v1"))
      }
      list(state = "absent", value = NULL, version_id = NA_character_)
    },
    .scan_dlw_validation_history = function(...) inventory[, .(
      survey_id, pipeline_version
    )],
    .validate_one_gmd = function(...) {
      worker_calls <<- worker_calls + 1L
    },
    .package = "pipdata"
  )

  result <- .pipdata_validate_gmd_core(verbose = FALSE)
  expect_identical(result$outcome, "failed")
  expect_identical(result$failures$phase, "report_consistency")
  expect_identical(result$summary$n_total, 0L)
  expect_identical(worker_calls, 0L)
})

test_that("validation worker smoke test uses the unchanged B1 engine", {
  candidate <- make_validation_acquisition()[1L]
  previous_report <- pd_env_get("validation_report")
  if (is.data.frame(previous_report)) {
    previous_report <- data.table::copy(previous_report)
  }
  withr::defer({
    if (is.null(previous_report)) {
      pd_env_rm("validation_report")
    } else {
      pd_env_set("validation_report", previous_report)
    }
  })
  pd_env_rm("validation_report")
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) dlw_fixture_data("all"),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_info = function(...) list(
      catalog = list(latest_version_id = "dlw-v1"),
      sidecar = list(content_hash = "hash-1", path = "survey.qs2")
    ),
    .package = "stamp"
  )

  result <- suppressWarnings(.validate_one_gmd(candidate, 1L, FALSE))
  expect_true(result$status %in% c("valid", "invalid"))
  expect_identical(result$inventory_row$pipeline_version, 1L)
  expect_gt(nrow(result$report_rows), 0L)
  expect_identical(
    unique(pd_env_get("validation_report")$table_name),
    result$survey_id
  )
})

test_that("validation report canonicalization distinguishes NA and NaN", {
  report <- make_validation_report()[rep(1L, 2L)]
  report[, metric := c(NA_real_, NaN)]

  forward <- .canonicalize_dlw_validation_report(report)
  reverse <- .canonicalize_dlw_validation_report(report[2:1])

  expect_identical(forward, reverse)
  expect_false(identical(forward$metric[[1L]], forward$metric[[2L]]))
})

test_that("completed worker facts must match their candidate", {
  candidate <- make_validation_acquisition()[1L]
  candidate[, `:=`(
    survey_id = fs::path_ext_remove(FileName),
    next_pipeline_version = 1L
  )]
  inventory <- make_validation_inventory()[survey_id == candidate$survey_id]
  report <- make_validation_report()[table_name == candidate$survey_id]
  base <- list(
    survey_id = candidate$survey_id[[1L]],
    status = "valid",
    inventory_row = inventory,
    report_rows = report,
    failure = .new_dlw_validation_failure()
  )

  expect_identical(
    .normalize_dlw_validation_worker_result(base, candidate)$status,
    "valid"
  )
  mutations <- list(
    function(x) x[, status := "invalid"],
    function(x) x[, Checksum := "stale-checksum"],
    function(x) x[, pipeline_version := 2L]
  )
  for (mutate in mutations) {
    malformed <- base
    malformed$inventory_row <- mutate(data.table::copy(inventory))
    normalized <- .normalize_dlw_validation_worker_result(malformed, candidate)
    expect_identical(normalized$status, "failed")
    expect_identical(normalized$failure$phase, "inventory_row")
  }
})

test_that("validation worker rejects missing and blank engine types", {
  candidate <- make_validation_acquisition()[1L]
  engine_mode <- "missing"
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) data.table::data.table(welfare = 1),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_info = function(...) list(
      catalog = list(latest_version_id = "v1"),
      sidecar = list(content_hash = "h1", path = "survey.qs2")
    ),
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    dlw_validation_engine = function(...) {
      if (engine_mode == "missing") {
        return(data.table::data.table(message = "missing type"))
      }
      data.table::data.table(type = "")
    },
    .package = "pipdata"
  )

  for (mode in c("missing", "blank")) {
    engine_mode <- mode
    result <- .validate_one_gmd(candidate, 1L, verbose = FALSE)
    expect_identical(result$status, "failed")
    expect_identical(result$failure$phase, "validation_engine")
    expect_null(result$inventory_row)
    expect_null(result$report_rows)
  }
})
