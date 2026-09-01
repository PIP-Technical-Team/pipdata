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
  prepared_calls <- 0L
  snapshot <- list(
    inventory = data.table::copy(inv),
    master = data.table::data.table(),
    fingerprints = list(),
    current = data.table::data.table(),
    facts = data.table::data.table(),
    snapshot_identity = "snapshot-1"
  )

  testthat::local_mocked_bindings(
    pd_prepare_dependency_facts = function(...) {
      prepared_calls <<- prepared_calls + 1L
      list(
        context = context,
        manifest = manifest,
        snapshot = snapshot
      )
    },
    .package = "pipdata"
  )

  output <- capture.output(plan <- pd_change_report(
    inv = inv,
    master = data.table::data.table(), manifest = manifest, context = context
  ))
  expect_match(paste(output, collapse = "\n"), "PIP dependency plan")
  expect_s3_class(plan, "pip_dependency_plan")
  expect_identical(prepared_calls, 1L)
  expect_identical(plan$snapshot$snapshot_identity, "snapshot-1")
})

test_that("change report prints disposition and reason summaries", {
  plan <- structure(
    list(
      context = list(scope_id = "scope"),
      actions = data.table::data.table(
        stage = c("clean", "metadata"),
        entity_id = c("s1", "p1"),
        survey_id = "s1",
        pip_id = c(NA_character_, "p1"),
        action = c("none", "refresh")
      ),
      reasons = data.table::data.table(
        stage = "metadata", entity_id = "p1", reason = "aux_cpi_changed",
        input = "aux_cpi", old = "old", new = "new"
      ),
      snapshot = list()
    ),
    class = "pip_dependency_plan"
  )

  output <- capture.output(print(plan))

  expect_match(paste(output, collapse = "\n"), "Disposition summary")
  expect_match(paste(output, collapse = "\n"), "Reason summary")
  expect_match(paste(output, collapse = "\n"), "cached")
  expect_match(paste(output, collapse = "\n"), "runnable")
  expect_match(paste(output, collapse = "\n"), "aux_cpi_changed")
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
    pd_build_dependency_snapshot = function(inv, master, context, ...) {
      list(
        context = context,
        inventory = data.table::copy(inv),
        master = data.table::copy(master),
        fingerprints = list(),
        current = data.table::data.table()
      )
    },
    pd_snapshot_facts = function(...) data.table::data.table(),
    pd_snapshot_identity = function(...) "snapshot-1",
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
    manifest = pd_empty_manifest(list(scope_id = "scope")),
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
    pd_build_dependency_snapshot = function(inv, master, context, ...) {
      list(
        context = context,
        inventory = data.table::copy(inv),
        master = data.table::copy(master),
        fingerprints = list(),
        current = data.table::data.table()
      )
    },
    pd_snapshot_facts = function(...) data.table::data.table(),
    pd_snapshot_identity = function(...) "snapshot-1",
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
    manifest = pd_empty_manifest(list(scope_id = "scope")),
    context = list(scope_id = "scope")
  ))
  expect_false(retry$survey_id %in% observed$survey_id)
})

test_that("change report and advisory execution use identical prepared facts", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  inv <- make_change_report_validation_inventory()[1L]
  master <- data.table::data.table(
    survey_id = inv$survey_id,
    pip_id = "BOL_2020_EH_INC"
  )
  facts <- data.table::data.table(
    stage = "clean",
    entity_id = inv$survey_id,
    survey_id = inv$survey_id,
    pip_id = NA_character_,
    reason = "dlw_changed",
    input = "canonical",
    old = "old-hash",
    new = "new-hash"
  )
  current <- data.table::data.table(
    stage = "clean",
    entity_id = inv$survey_id,
    survey_id = inv$survey_id,
    pip_id = NA_character_,
    input_hash = "new-hash",
    code_hash = "clean-code",
    output_version_id = "output-version",
    output_hash = "output-hash"
  )
  prepared_calls <- 0L

  testthat::local_mocked_bindings(
    pd_prepare_dependency_facts = function(...) {
      prepared_calls <<- prepared_calls + 1L
      list(
        context = context,
        manifest = manifest,
        snapshot = list(
          inventory = data.table::copy(inv),
          master = data.table::copy(master),
          fingerprints = list(),
          current = data.table::copy(current),
          facts = data.table::copy(facts),
          snapshot_identity = "same-snapshot"
        )
      )
    },
    pd_lease_acquire = function(...) list(token = "lease"),
    .package = "pipdata"
  )

  report <- capture.output(report_plan <- pd_change_report(
    inv = inv,
    master = master,
    manifest = manifest,
    context = context
  ))
  execution <- pd_prepare_execution(
    inv = inv,
    master = master,
    context = context
  )

  expect_match(paste(report, collapse = "\n"), "PIP dependency plan")
  expect_identical(report_plan$context, execution$plan$context)
  expect_identical(report_plan$actions, execution$plan$actions)
  expect_identical(report_plan$reasons, execution$plan$reasons)
  expect_identical(
    report_plan$snapshot$snapshot_identity,
    execution$plan$snapshot$snapshot_identity
  )
  expect_identical(prepared_calls, 3L)
})

test_that("change report rejects a removed upstream survey before reads", {
  inv <- make_change_report_validation_inventory()[1L]
  master <- data.table::data.table(
    survey_id = make_change_report_validation_inventory()$survey_id,
    pip_id = c("BOL_2020_EH_INC", "ZWE_2021_PICES_INC")
  )
  context <- list(scope_id = "scope")
  household_reads <- 0L

  testthat::local_mocked_bindings(
    pip_read = function(...) {
      household_reads <<- household_reads + 1L
      NULL
    },
    .package = "pipload"
  )

  expect_error(
    pd_change_report(
      inv = inv,
      master = master,
      manifest = pd_empty_manifest(context),
      context = context
    ),
    class = "pipdata_upstream_survey_removed"
  )
  expect_identical(household_reads, 0L)
})

test_that("report parity uses real fact preparation with exact metadata", {
  context <- list(scope_id = "scope")
  inv <- make_change_report_validation_inventory()[1L]
  master <- data.table::data.table(
    survey_id = inv$survey_id, pip_id = "BOL_2020_EH_INC",
    version_id_data = "data-v1", content_hash_data = "data-h1",
    version_id_metadata = "meta-v1", content_hash_metadata = "meta-h1",
    version_id_deflated = "deflate-v1",
    content_hash_deflated = "deflate-h1", deflated = TRUE
  )
  stages <- c("clean", "metadata", "deflate")
  entities <- c(inv$survey_id, master$pip_id, master$pip_id)
  input_hashes <- c("clean-input", "metadata-input", "deflate-input")
  input_versions <- c("clean-input", "data-v1", "deflate-input-v1")
  inputs <- data.table::data.table(
    stage = stages, entity_id = entities, name = "canonical",
    version_id = input_versions, content_hash = input_hashes
  )
  receipts <- list(
    list(list(
      alias = "pip", artifact = master$pip_id, path = "clean.qs2",
      version_id = "data-v1", content_hash = "data-h1"
    )),
    list(list(
      alias = "pip_meta", artifact = master$pip_id, path = "meta.qs2",
      version_id = "meta-v1", content_hash = "meta-h1"
    )),
    list(list(
      alias = "pip_deflated", artifact = master$pip_id,
      path = "deflate.qs2", version_id = "deflate-v1",
      content_hash = "deflate-h1"
    ))
  )
  manifest <- pd_empty_manifest(context)
  manifest$records <- data.table::data.table(
    stage = stages, entity_id = entities,
    output_version_id = c("clean-set-v1", "meta-v1", "deflate-v1"),
    output_hash = c("clean-set-h1", "meta-h1", "deflate-h1"),
    input_hash = input_hashes,
    code_hash = c("clean-code", "metadata-code", "deflate-code"),
    output_receipts = receipts
  )
  manifest$inputs <- inputs
  current <- data.table::data.table(
    stage = stages, entity_id = entities,
    survey_id = inv$survey_id,
    pip_id = c(NA_character_, master$pip_id, master$pip_id),
    output_version_id = c("clean-set-v1", "meta-v1", "deflate-v1"),
    output_hash = c("clean-set-h1", "meta-h1", "deflate-h1"),
    input_hash = input_hashes, legacy_input_hash = input_hashes,
    legacy_input_version = input_versions,
    code_hash = c("clean-code", "metadata-code", "deflate-code"),
    input_rows = split(inputs, seq_len(nrow(inputs)))
  )
  fingerprints <- list(
    summary = data.table::data.table(
      stage = stages,
      hash = c("clean-code", "metadata-code", "deflate-code")
    ),
    components = data.table::data.table(
      stage = character(), component = character(), hash = character()
    ),
    audit = list()
  )
  build_calls <- 0L

  testthat::local_mocked_bindings(
    pd_manifest_read = function(...) manifest,
    pd_build_dependency_snapshot = function(inv, master, context, ...) {
      build_calls <<- build_calls + 1L
      list(
        context = context,
        inventory = data.table::copy(inv),
        master = data.table::copy(master),
        measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
        aux = list(
          catalog = data.table::data.table(), objects = list()
        ),
        catalogs = list(
          pip = data.table::data.table(),
          pip_meta = data.table::data.table(),
          pip_deflated = data.table::data.table(),
          pip_inv = data.table::data.table()
        ),
        fingerprints = fingerprints,
        captured_at = "2026-08-28 12:00:00 UTC",
        current = data.table::copy(current)
      )
    },
    pd_lease_acquire = function(...) list(token = "lease"),
    .package = "pipdata"
  )

  report_output <- capture.output(report_plan <- pd_change_report(
    inv = inv, master = master, manifest = manifest, context = context
  ))
  execution <- pd_prepare_execution(inv, master, context = context)

  expect_match(paste(report_output, collapse = "\n"), "PIP dependency plan")
  expect_identical(build_calls, 3L)
  expect_true(all(report_plan$actions$action == "none"))
  expect_identical(report_plan$reasons, pd_empty_reasons())
  expect_identical(report_plan$context, execution$plan$context)
  expect_identical(report_plan$actions, execution$plan$actions)
  expect_identical(report_plan$reasons, execution$plan$reasons)
  expect_identical(
    report_plan$snapshot$snapshot_identity,
    execution$plan$snapshot$snapshot_identity
  )
})
