make_change_report_validation_inventory <- function() {
  data.table::data.table(
    survey_id = c(
      "BOL_2020_EH_V01_M_V01_A_GMD_ALL",
      "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL"
    ),
    pipeline_version = c(1L, 2L),
    latest_version_id = c("v1", "v2"),
    content_hash = c("hash-1", "hash-2"),
    file_path = c("bol.qs2", "zwe.qs2"),
    status = c("valid", "invalid"),
    data_available = "Yes",
    date_validated = as.POSIXct(
      c("2026-08-26 11:00:00", "2026-08-26 12:00:00"), tz = "UTC"
    ),
    Checksum = c("checksum-1", "checksum-2"),
    country_code = c("BOL", "ZWE"),
    surveyid_year = c(2020L, 2021L),
    survey_acronym = c("EH", "PICES"),
    vermast = c("v01", "v02"),
    veralt = "v01",
    collection = "GMD",
    module = "ALL",
    tool = "TB"
  )
}

test_that("change report returns the shared plan without writes", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  inv <- make_change_report_validation_inventory()[1L]
  output <- capture.output(plan <- pd_change_report(
    inv = inv,
    master = data.table::data.table(), manifest = manifest, context = context
  ))
  expect_match(paste(output, collapse = "\n"), "PIP dependency plan")
  expect_s3_class(plan, "pip_dependency_plan")
})

test_that("change report filters retry rows before dependency planning", {
  inv <- make_change_report_validation_inventory()
  retry <- inv[1L]
  retry[, `:=`(
    survey_id = "PER_2022_ENAHO_V01_M_V01_A_GMD_ALL",
    latest_version_id = "",
    content_hash = "",
    file_path = "",
    status = "",
    data_available = "No"
  )]
  inv <- data.table::rbindlist(list(inv, retry))
  observed <- NULL

  testthat::local_mocked_bindings(
    pd_dependency_plan = function(inv, ...) {
      observed <<- data.table::copy(inv)
      structure(
        list(actions = pd_empty_actions(), reasons = pd_empty_reasons()),
        class = "pip_dependency_plan"
      )
    },
    .package = "pipdata"
  )

  capture.output(pd_change_report(
    inv = inv,
    master = data.table::data.table(),
    manifest = list(),
    context = list(scope_id = "scope")
  ))
  expect_false(retry$survey_id %in% observed$survey_id)
})

test_that("change report filters its loaded durable inventory", {
  inv <- make_change_report_validation_inventory()
  retry <- inv[1L]
  retry[, `:=`(
    survey_id = "PER_2022_ENAHO_V01_M_V01_A_GMD_ALL",
    latest_version_id = "",
    content_hash = "",
    file_path = "",
    status = "",
    data_available = "No"
  )]
  durable <- data.table::rbindlist(list(inv, retry))
  observed <- NULL

  testthat::local_mocked_bindings(
    load_gmd_valid_inv = function(...) durable,
    load_pip_master_inventory = function(...) data.table::data.table(),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_plan = function(inv, ...) {
      observed <<- data.table::copy(inv)
      structure(
        list(actions = pd_empty_actions(), reasons = pd_empty_reasons()),
        class = "pip_dependency_plan"
      )
    },
    .package = "pipdata"
  )

  capture.output(pd_change_report(
    manifest = list(),
    context = list(scope_id = "scope")
  ))
  expect_false(retry$survey_id %in% observed$survey_id)
})
