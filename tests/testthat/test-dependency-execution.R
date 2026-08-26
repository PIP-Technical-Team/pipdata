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
