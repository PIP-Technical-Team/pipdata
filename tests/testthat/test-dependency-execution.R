make_dependency_validation_inventory <- function() {
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

test_that("checkpoint scheduler publishes bounded successful batches", {
  batches <- list()
  units <- as.list(1:7)
  pd_run_checkpoint_batches(
    units,
    worker = function(x) list(success = TRUE, id = x),
    checkpoint = function(x) batches[[length(batches) + 1L]] <<- x,
    checkpoint_n = 3L,
    checkpoint_seconds = Inf
  )
  expect_identical(vapply(batches, length, integer(1)), c(3L, 3L, 1L))
})

test_that("failed units never enter checkpoints", {
  seen <- integer()
  pd_run_checkpoint_batches(
    as.list(1:4),
    worker = function(x) list(success = x %% 2L == 0L, id = x),
    checkpoint = function(x) seen <<- vapply(x, `[[`, integer(1), "id"),
    checkpoint_n = 25L,
    checkpoint_seconds = Inf
  )
  expect_identical(seen, c(2L, 4L))
})

test_that("slow failed units do not produce empty checkpoints", {
  checkpoints <- 0L
  times <- as.POSIXct(c(0, 61), origin = "1970-01-01", tz = "UTC")
  clock <- function() {
    value <- times[[1L]]
    times <<- times[-1L]
    value
  }
  pd_run_checkpoint_batches(
    list(1L), worker = function(x) list(success = FALSE),
    checkpoint = function(x) checkpoints <<- checkpoints + 1L,
    checkpoint_n = 25L, checkpoint_seconds = 60, clock = clock
  )
  expect_identical(checkpoints, 0L)
})

test_that("persisted failed deflation is missing on restart", {
  receipt <- list(version_id = "old-v", content_hash = "old-h", path = "old")
  invalidated <- list(
    deflated = FALSE, version_id_deflated = NA_character_,
    content_hash_deflated = NA_character_
  )
  restarted <- pd_deflate_current_receipt(receipt, invalidated)
  expect_true(is.na(restarted$version_id))
  expect_true(is.na(restarted$content_hash))

  current <- list(
    deflated = TRUE, version_id_deflated = "old-v",
    content_hash_deflated = "old-h"
  )
  expect_identical(pd_deflate_current_receipt(receipt, current), receipt)
})

test_that("write fence fails before work after lease loss", {
  root <- withr::local_tempdir()
  context <- list(scope_id = "scope")
  lease <- pd_lease_acquire(context, root)
  fs::dir_delete(lease$path)
  expect_error(
    pd_assert_execution_fence(list(lease = lease)),
    class = "pipdata_manifest_lease_lost"
  )
})

test_that("completed validation filter excludes retries and rejects malformed rows", {
  completed <- make_dependency_validation_inventory()
  retry <- completed[1L]
  retry[, `:=`(
    latest_version_id = "",
    content_hash = "",
    file_path = "",
    status = "",
    data_available = "No"
  )]

  filtered <- .filter_completed_dlw_validation_inventory(
    data.table::rbindlist(list(completed, retry))
  )
  expect_identical(nrow(filtered), 2L)
  expect_false(any(filtered$data_available == "No"))

  malformed <- data.table::copy(completed)
  malformed[1L, collection := ""]
  expect_error(
    .filter_completed_dlw_validation_inventory(malformed),
    class = "pipdata_dlw_inventory_schema_error"
  )

  empty <- data.table::data.table(survey_id = character())
  expect_identical(
    .filter_completed_dlw_validation_inventory(empty),
    .empty_dlw_validation_inventory()
  )

  exact <- data.table::rbindlist(list(completed, completed[1L]))
  expect_identical(
    nrow(.filter_completed_dlw_validation_inventory(exact)),
    2L
  )

  conflicting <- data.table::rbindlist(list(completed, completed[1L]))
  conflicting[3L, content_hash := "different-hash"]
  expect_error(
    .filter_completed_dlw_validation_inventory(conflicting),
    class = "pipdata_dlw_inventory_schema_error"
  )
})

test_that("pd_prepare_execution filters validation inventory before snapshots", {
  inv <- make_dependency_validation_inventory()
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
    pd_manifest_read = function(...) structure(
      list(), class = "pipdata_manifest_absent"
    ),
    pd_build_dependency_snapshot = function(inv, ...) {
      observed <<- data.table::copy(inv)
      list(
        inventory = data.table::copy(inv),
        master = data.table::data.table(),
        fingerprints = list(),
        current = data.table::data.table()
      )
    },
    pd_snapshot_facts = function(...) data.table::data.table(),
    pd_dependency_plan = function(...) structure(
      list(actions = pd_empty_actions(), reasons = pd_empty_reasons()),
      class = "pip_dependency_plan"
    ),
    pd_assert_bootstrap = function(plan, ...) plan,
    pd_lease_acquire = function(...) list(),
    pd_empty_manifest = function(...) list(),
    .package = "pipdata"
  )

  pd_prepare_execution(
    inv = inv,
    master = data.table::data.table(),
    context = list(scope_id = "scope")
  )
  expect_false(retry$survey_id %in% observed$survey_id)
})

test_that("read-only fact preparation accepts injected metadata without I/O", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  aux <- list(
    catalog = data.table::data.table(
      measure = character(), path = character(),
      version_id = character(), content_hash = character()
    ),
    objects = list()
  )
  catalogs <- stats::setNames(
    rep(list(data.table::data.table()), 4L),
    c("pip", "pip_meta", "pip_deflated", "pip_inv")
  )
  fingerprints <- list(
    summary = data.table::data.table(stage = character(), hash = character()),
    components = data.table::data.table(
      stage = character(), component = character(), hash = character()
    ),
    audit = list()
  )
  io_calls <- 0L

  testthat::local_mocked_bindings(
    pd_freeze_aux_snapshot = function(...) {
      io_calls <<- io_calls + 1L
      rlang::abort("unexpected auxiliary load")
    },
    pd_code_fingerprints = function(...) {
      io_calls <<- io_calls + 1L
      rlang::abort("unexpected fingerprint load")
    },
    pd_lease_acquire = function(...) {
      io_calls <<- io_calls + 1L
      rlang::abort("unexpected lease mutation")
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    st_catalog_query = function(...) {
      io_calls <<- io_calls + 1L
      rlang::abort("unexpected catalog query")
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pip_read = function(...) {
      io_calls <<- io_calls + 1L
      rlang::abort("unexpected household load")
    },
    pip_write = function(...) {
      io_calls <<- io_calls + 1L
      rlang::abort("unexpected write")
    },
    .package = "pipload"
  )

  prepared <- pd_prepare_dependency_facts(
    inv = data.table::data.table(survey_id = character()),
    master = data.table::data.table(),
    context = context,
    manifest = manifest,
    aux = aux,
    catalogs = catalogs,
    fingerprints = fingerprints
  )

  expect_identical(io_calls, 0L)
  expect_identical(prepared$context, context)
  expect_identical(prepared$manifest, manifest)
  expect_true(is.character(prepared$snapshot$snapshot_identity))
  expect_length(prepared$snapshot$snapshot_identity, 1L)
})

test_that("snapshot identity is deterministic over metadata facts", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  attr(manifest, "manifest_identity") <- list(
    filename = "manifest-v1.rds",
    uuid = "uuid",
    checksum = "checksum",
    generation = 1
  )
  inv <- make_dependency_validation_inventory()
  master <- data.table::data.table(
    survey_id = inv$survey_id,
    pip_id = c("BOL_2020_EH_INC", "ZWE_2021_PICES_INC"),
    version_id_data = c("data-1", "data-2"),
    content_hash_data = c("data-hash-1", "data-hash-2")
  )
  aux_catalog <- data.table::data.table(
    measure = c("pfw", "cpi"),
    path = c("pfw.qs2", "cpi.qs2"),
    version_id = c("pfw-v1", "cpi-v1"),
    content_hash = c("pfw-h1", "cpi-h1")
  )
  pip_catalog <- data.table::data.table(
    path = c("zwe.qs2", "bol.qs2"),
    version_id = c("out-v2", "out-v1"),
    content_hash = c("out-h2", "out-h1")
  )
  current <- data.table::data.table(
    stage = c("clean", "metadata"),
    entity_id = c(inv$survey_id[1L], master$pip_id[2L]),
    survey_id = c(inv$survey_id[1L], inv$survey_id[2L]),
    pip_id = c(NA_character_, master$pip_id[2L]),
    input_hash = c("input-1", "input-2"),
    code_hash = c("code-1", "code-2"),
    output_version_id = c("out-v1", "out-v2"),
    output_hash = c("out-h1", "out-h2"),
    aux_hashes = list(c(pfw = "pfw-1"), c(cpi = "cpi-2"))
  )
  facts <- data.table::data.table(
    stage = c("metadata", "clean"),
    entity_id = c(master$pip_id[2L], inv$survey_id[1L]),
    survey_id = rev(inv$survey_id),
    pip_id = c(master$pip_id[2L], NA_character_),
    reason = c("aux_cpi_changed", "dlw_changed"),
    input = c("canonical", "canonical"),
    old = c("old-2", "old-1"),
    new = c("new-2", "new-1")
  )
  fingerprints <- list(
    summary = data.table::data.table(
      stage = c("metadata", "clean"), hash = c("fp-2", "fp-1")
    ),
    components = data.table::data.table(
      stage = c("metadata", "clean"),
      component = c("metadata_fn", "clean_fn"),
      hash = c("component-2", "component-1")
    ),
    audit = list(stamp = "1", pipdata = "1")
  )
  first <- list(
    context = context,
    inventory = data.table::copy(inv),
    master = data.table::copy(master),
    aux = list(catalog = aux_catalog, objects = list(pfw = new.env())),
    catalogs = list(pip = pip_catalog),
    fingerprints = fingerprints,
    captured_at = "2026-08-28 10:00:00 UTC",
    current = current,
    facts = facts
  )
  second <- list(
    context = context,
    inventory = inv[2:1],
    master = master[2:1],
    aux = list(
      catalog = aux_catalog[2:1],
      objects = list(pfw = new.env())
    ),
    catalogs = list(pip = pip_catalog[2:1]),
    fingerprints = list(
      summary = fingerprints$summary[2:1],
      components = fingerprints$components[2:1],
      audit = list(pipdata = "1", stamp = "1")
    ),
    captured_at = "2026-08-28 11:00:00 UTC",
    current = current[2:1],
    facts = facts[2:1]
  )

  first_identity <- pd_snapshot_identity(first, manifest)
  second_identity <- pd_snapshot_identity(second, manifest)
  changed <- data.table::copy(second$facts)
  changed[1L, new := "different-fact"]
  second$facts <- changed

  expect_identical(first_identity, second_identity)
  expect_false(identical(first_identity, pd_snapshot_identity(second, manifest)))
})

test_that("whole-survey removal is pure and blocks execution before lease", {
  completed <- make_dependency_validation_inventory()[1L]
  master <- data.table::data.table(
    survey_id = make_dependency_validation_inventory()$survey_id,
    pip_id = c("BOL_2020_EH_INC", "ZWE_2021_PICES_INC")
  )
  context <- list(scope_id = "scope")
  lease_calls <- 0L
  household_reads <- 0L

  condition <- rlang::catch_cnd(
    pd_assert_no_removed_surveys(completed, master)
  )
  expect_s3_class(condition, "pipdata_upstream_survey_removed")
  expect_identical(
    condition$removed_surveys,
    "ZWE_2021_PICES_V02_M_V01_A_GMD_ALL"
  )

  testthat::local_mocked_bindings(
    pd_lease_acquire = function(...) {
      lease_calls <<- lease_calls + 1L
      list()
    },
    .package = "pipdata"
  )
  testthat::local_mocked_bindings(
    pip_read = function(...) {
      household_reads <<- household_reads + 1L
      NULL
    },
    .package = "pipload"
  )

  expect_error(
    pd_prepare_execution(completed, master, context = context),
    class = "pipdata_upstream_survey_removed"
  )
  expect_identical(lease_calls, 0L)
  expect_identical(household_reads, 0L)
})

test_that("execution rebuilds its authoritative plan after lease acquisition", {
  inv <- make_dependency_validation_inventory()[1L]
  master <- data.table::data.table(
    survey_id = inv$survey_id,
    pip_id = "BOL_2020_EH_INC"
  )
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  trace <- character()
  plan_calls <- 0L

  testthat::local_mocked_bindings(
    pd_prepare_dependency_facts = function(...) {
      trace <<- c(trace, "prepare")
      list(
        context = context,
        manifest = manifest,
        snapshot = list(
          inventory = data.table::copy(inv),
          master = data.table::copy(master),
          fingerprints = list(),
          current = data.table::data.table(),
          facts = data.table::data.table(),
          snapshot_identity = paste0("snapshot-", sum(trace == "prepare"))
        )
      )
    },
    pd_dependency_plan = function(..., snapshot) {
      trace <<- c(trace, "plan")
      plan_calls <<- plan_calls + 1L
      structure(
        list(
          context = context,
          actions = pd_empty_actions(),
          reasons = pd_empty_reasons(),
          snapshot = snapshot
        ),
        class = "pip_dependency_plan"
      )
    },
    pd_lease_acquire = function(...) {
      trace <<- c(trace, "lease")
      list(token = "lease")
    },
    .package = "pipdata"
  )

  execution <- pd_prepare_execution(inv, master, context = context)

  expect_identical(
    trace,
    c("prepare", "plan", "lease", "prepare", "plan")
  )
  expect_identical(plan_calls, 2L)
  expect_identical(
    execution$plan$snapshot$snapshot_identity,
    "snapshot-2"
  )
})

test_that("locked execution prepares once under the supplied lease", {
  context <- list(scope_id = "scope")
  lease <- list(token = "supplied-lease")
  inv <- make_dependency_validation_inventory()[1L]
  master <- data.table::data.table(
    survey_id = inv$survey_id, pip_id = "BOL_2020_EH_INC"
  )
  manifest <- pd_empty_manifest(context)
  snapshot <- list(
    inventory = data.table::copy(inv),
    master = data.table::copy(master),
    fingerprints = list(),
    current = data.table::data.table(),
    facts = data.table::data.table()
  )
  prepare_calls <- 0L
  plan_calls <- 0L
  lease_acquires <- 0L

  testthat::local_mocked_bindings(
    pd_prepare_dependency_facts = function(...) {
      prepare_calls <<- prepare_calls + 1L
      list(context = context, manifest = manifest, snapshot = snapshot)
    },
    pd_dependency_plan = function(...) {
      plan_calls <<- plan_calls + 1L
      structure(
        list(
          context = context,
          actions = pd_empty_actions(),
          reasons = pd_empty_reasons(),
          snapshot = snapshot
        ),
        class = "pip_dependency_plan"
      )
    },
    pd_lease_acquire = function(...) {
      lease_acquires <<- lease_acquires + 1L
      rlang::abort("locked preparation must not acquire another lease")
    },
    .package = "pipdata"
  )

  execution <- pd_prepare_execution_locked(
    inv, master, context, lease
  )

  expect_identical(execution$lease, lease)
  expect_identical(prepare_calls, 1L)
  expect_identical(plan_calls, 1L)
  expect_identical(lease_acquires, 0L)
})

test_that("partial manifests make every unrecorded node actionable", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  manifest$records <- data.table::data.table(
    stage = "clean", entity_id = "s1", output_version_id = "clean-v1",
    output_hash = "clean-h1", input_hash = "clean-input",
    code_hash = "clean-code", output_receipts = list(list())
  )
  manifest$inputs <- data.table::data.table(
    stage = "clean", entity_id = "s1", name = "canonical",
    version_id = "clean-input", content_hash = "clean-input"
  )
  current <- data.table::data.table(
    stage = c("clean", "metadata", "deflate"),
    entity_id = c("s1", "p1", "p1"),
    survey_id = "s1", pip_id = c(NA_character_, "p1", "p1"),
    output_version_id = c("clean-v1", NA_character_, "deflate-v1"),
    output_hash = c("clean-h1", NA_character_, "deflate-h1"),
    input_hash = c("clean-input", "metadata-input", "deflate-input"),
    legacy_input_hash = c(
      "clean-input", "metadata-input", "deflate-input"
    ),
    code_hash = c("clean-code", "metadata-code", "deflate-code"),
    input_rows = list(
      manifest$inputs,
      data.table::data.table(),
      data.table::data.table()
    )
  )
  snapshot <- list(
    current = current,
    fingerprints = list(
      components = data.table::data.table(
        stage = character(), component = character(), hash = character()
      )
    )
  )

  facts <- pd_snapshot_facts(snapshot, manifest)

  expect_identical(
    facts[, .(stage, reason)],
    data.table::data.table(
      stage = c("deflate", "metadata"),
      reason = c("unknown_provenance", "new_entity")
    )
  )
})

test_that("post-clean facts accept metadata before a metadata receipt exists", {
  inv <- make_dependency_validation_inventory()[1L]
  inv[, `:=`(
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", module = "ALL"
  )]
  pip_id <- "COL_2020_GEIH_INC_ALL"
  master <- data.table::data.table(
    survey_id = inv$survey_id, pip_id = pip_id,
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", module = "ALL", welfare_type = "income",
    version_id_data = "data-v1", content_hash_data = "data-h1",
    version_id_metadata = NA_character_, content_hash_metadata = NA_character_,
    deflated = FALSE
  )
  pfw <- data.table::data.table(
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", welfare_type = "income", inpovcal = 1L,
    cpi_domain = 1L, ppp_domain = 1L, pop_domain = 1L,
    gdp_domain = 1L, pce_domain = 1L
  )
  aux_objects <- list(
    pfw = pfw,
    cpi = data.table::data.table(
      country_code = "COL", year = 2020L, survey_acronym = "GEIH",
      cpi_year = 2017L, reporting_level = "national", cpi_value = 1
    ),
    ppp = data.table::data.table(
      country_code = "COL", ppp_year = 2017L, release_version = "v01",
      adaptation_version = "v01", reporting_level = "national", ppp = 2
    ),
    pop = data.table::data.table(
      country_code = "COL", year = 2020L,
      reporting_level = "national", pop = 3
    ),
    gdp = data.table::data.table(
      country_code = "COL", year = 2020L,
      reporting_level = "national", gdp = 4
    ),
    pce = data.table::data.table(
      country_code = "COL", year = 2020L,
      reporting_level = "national", pce = 5
    )
  )
  measures <- names(aux_objects)
  aux <- list(
    catalog = data.table::data.table(
      measure = measures, version_id = paste0(measures, "-v1"),
      content_hash = paste0(measures, "-h1")
    ),
    objects = aux_objects
  )
  catalogs <- list(
    pip = data.table::data.table(
      path = paste0(pip_id, ".qs2"), version_id = "data-v1",
      content_hash = "data-h1"
    ),
    pip_meta = data.table::data.table(),
    pip_deflated = data.table::data.table(),
    pip_inv = data.table::data.table()
  )
  fingerprints <- list(
    summary = data.table::data.table(
      stage = c("clean", "metadata", "deflate"),
      hash = c("clean-code", "metadata-code", "deflate-code")
    ),
    components = data.table::data.table(
      stage = character(), component = character(), hash = character()
    ),
    audit = list()
  )

  snapshot <- pd_build_dependency_snapshot(
    inv, master, list(scope_id = "scope"),
    aux = aux, catalogs = catalogs, fingerprints = fingerprints
  )

  expect_identical(nrow(snapshot$current), 3L)
  expect_true(
    nrow(snapshot$current[stage == "metadata"]$input_rows[[1L]]) > 1L
  )
  expect_identical(
    nrow(snapshot$current[stage == "deflate"]$input_rows[[1L]]), 0L
  )
})

test_that("checkpoint scheduler rejects current nodes before worker dispatch", {
  worker_calls <- 0L
  checkpoint_calls <- 0L
  current <- data.table::data.table(
    stage = "deflate", entity_id = "p1", survey_id = "s1",
    pip_id = "p1", action = "none"
  )

  expect_error(
    pd_run_checkpoint_batches(
      split(current, seq_len(nrow(current))),
      worker = function(unit) {
        worker_calls <<- worker_calls + 1L
        list(success = TRUE, unit = unit)
      },
      checkpoint = function(batch) {
        checkpoint_calls <<- checkpoint_calls + 1L
      }
    ),
    class = "pipdata_dependency_action_not_runnable"
  )
  expect_identical(worker_calls, 0L)
  expect_identical(checkpoint_calls, 0L)
})

test_that("checkpoint scheduler rejects named-list current nodes", {
  worker_calls <- 0L
  checkpoint_calls <- 0L
  current <- list(
    stage = "metadata", entity_id = "p1", survey_id = "s1",
    pip_id = "p1", action = "none"
  )

  expect_error(
    pd_run_checkpoint_batches(
      list(current),
      worker = function(unit) {
        worker_calls <<- worker_calls + 1L
        list(success = TRUE, unit = unit)
      },
      checkpoint = function(batch) {
        checkpoint_calls <<- checkpoint_calls + 1L
      }
    ),
    class = "pipdata_dependency_action_not_runnable"
  )
  expect_identical(worker_calls, 0L)
  expect_identical(checkpoint_calls, 0L)
})
