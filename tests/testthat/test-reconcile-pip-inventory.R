test_that("stage-aware metadata reconciliation preserves siblings", {
  master <- data.table::data.table(survey_id = c("s", "s"), pip_id = c("p1", "p2"),
                                   version_id_data = c("d1", "d2"))
  result <- data.table::data.table(pip_id = "p1", version_id = "m1",
                                   content_hash = "h1", success = TRUE)
  out <- pd_reconcile_inventory(master, "metadata", result)
  expect_true(out$success)
  expect_identical(out$candidate[pip_id == "p2"]$version_id_data, "d2")
  expect_identical(out$candidate[pip_id == "p1"]$version_id_metadata, "m1")
})

test_that("metadata refresh invalidates only matching deflation provenance", {
  master <- data.table::data.table(
    survey_id = c("s", "s"), pip_id = c("p1", "p2"),
    version_id_data = c("d1", "d2"),
    version_id_deflated = c("f1", "f2"),
    content_hash_deflated = c("fh1", "fh2"), deflated = TRUE
  )
  result <- data.table::data.table(
    pip_id = "p1", version_id = "m1", content_hash = "mh1", success = TRUE
  )
  out <- pd_reconcile_inventory(master, "metadata", result)$candidate
  expect_false(out[pip_id == "p1", deflated])
  expect_true(out[pip_id == "p2", deflated])
  expect_identical(out[pip_id == "p2", version_id_deflated], "f2")
})

test_that("reconciliation rejects duplicate and unknown result keys", {
  master <- data.table::data.table(survey_id = "s", pip_id = "p")
  duplicate <- data.table::data.table(
    pip_id = c("p", "p"), version_id = c("v1", "v2"),
    content_hash = c("h1", "h2"), success = TRUE
  )
  expect_false(pd_reconcile_inventory(master, "metadata", duplicate)$success)
  unknown <- duplicate[1L][, pip_id := "other"]
  expect_identical(
    pd_reconcile_inventory(master, "metadata", unknown)$reason,
    "unknown_pip_id"
  )
})

test_that("clean reconciliation preserves invariants and clears downstream", {
  master <- data.table::data.table(
    survey_id = "s", pip_id = "p", country_code = "COL", year = 2020L,
    welfare_type = "inc", version_id_data = "old", content_hash_data = "oldh",
    version_id_metadata = "m", content_hash_metadata = "mh",
    version_id_deflated = "f", content_hash_deflated = "fh",
    deflated = TRUE, first_release_version_id = "first",
    latest_release_version_id = "latest"
  )
  result <- data.table::data.table(
    survey_id = "s", pip_id = "p", country_code = "COL", year = 2020L,
    welfare_type = "inc", version_id = "new", content_hash = "newh",
    success = TRUE
  )
  out <- pd_reconcile_inventory(master, "clean", result, "s", "p")$candidate
  expect_identical(out$country_code, "COL")
  expect_identical(out$year, 2020L)
  expect_identical(out$first_release_version_id, "first")
  expect_identical(out$version_id_data, "new")
  expect_true(is.na(out$version_id_metadata))
  expect_false(out$deflated)
})

test_that("incomplete clean reconciliation is a typed failure", {
  master <- data.table::data.table(survey_id = "s", pip_id = "old")
  result <- data.table::data.table(
    pip_id = "p1", version_id = "v1", content_hash = "h1", success = TRUE
  )
  out <- pd_reconcile_inventory(master, "clean", result, "s", c("p1", "p2"))
  expect_s3_class(out, "pipdata_reconciliation")
  expect_false(out$success)
  expect_identical(out$reason, "incomplete_output_set")
  expect_identical(out$candidate, master)
})

checkpoint_fixture <- function(root) {
  context <- list(scope_id = "scope")
  lease <- pd_lease_acquire(context, root)
  list(
    root = root, context = context, lease = lease,
    execution = list(
      context = context, lease = lease,
      manifest = pd_empty_manifest(context), manifest_identity = NULL
    ),
    master = data.table::data.table(survey_id = "s", pip_id = "p"),
    results = data.table::data.table(
      pip_id = "p", version_id = "m1", content_hash = "h", success = TRUE,
      input_hash = "input", code_hash = "code"
    )
  )
}

test_that("release receipt failure prevents later checkpoint writes", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  withr::defer(pd_lease_release(fixture$lease))
  master_called <- FALSE
  expect_error(
    pd_finalize_checkpoint(
      fixture$execution, fixture$master, "metadata", fixture$results,
      function(...) list(success = FALSE),
      function(...) {
        master_called <<- TRUE
        list(success = TRUE, version_id = "master")
      }, fixture$root
    ),
    class = "pipdata_checkpoint_release_error"
  )
  expect_false(master_called)
  expect_length(pd_manifest_files(fixture$context, fixture$root), 0L)
})

test_that("master receipt failure prevents manifest publication", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  withr::defer(pd_lease_release(fixture$lease))
  expect_error(
    pd_finalize_checkpoint(
      fixture$execution, fixture$master, "metadata", fixture$results,
      function(...) list(success = TRUE, version_id = "release"),
      function(...) list(success = FALSE), fixture$root
    ),
    class = "pipdata_checkpoint_master_error"
  )
  expect_length(pd_manifest_files(fixture$context, fixture$root), 0L)
})

test_that("lease loss at checkpoint boundary prevents publication", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  release_writer <- function(candidate, lease) {
    fs::dir_delete(lease$path)
    list(success = TRUE, version_id = "release")
  }
  expect_error(
    pd_finalize_checkpoint(
      fixture$execution, fixture$master, "metadata", fixture$results,
      release_writer, function(...) stop("must not run"), fixture$root
    ),
    class = "pipdata_manifest_lease_lost"
  )
  expect_length(pd_manifest_files(fixture$context, fixture$root), 0L)
})

test_that("manifest publication failure leaves prior generation current", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  withr::defer(pd_lease_release(fixture$lease))
  writer <- function(...) list(success = TRUE, version_id = "inventory")
  testthat::local_mocked_bindings(
    pd_manifest_publish = function(...) {
      rlang::abort("injected", class = "pipdata_manifest_write_error")
    },
    .package = "pipdata"
  )
  expect_error(
    pd_finalize_checkpoint(
      fixture$execution, fixture$master, "metadata", fixture$results,
      writer, writer, fixture$root
    ),
    class = "pipdata_manifest_write_error"
  )
  expect_length(pd_manifest_files(fixture$context, fixture$root), 0L)
})

test_that("checkpoint publishes only after release and master verify", {
  root <- withr::local_tempdir()
  context <- list(scope_id = "scope")
  lease <- pd_lease_acquire(context, root)
  withr::defer(pd_lease_release(lease))
  master <- data.table::data.table(survey_id = "s", pip_id = "p")
  results <- data.table::data.table(
    pip_id = "p", version_id = "m1", content_hash = "h", success = TRUE,
    input_hash = "input", code_hash = "code", alias = "pip_meta",
    artifact = "p", path = "p.qs2"
  )
  writer <- function(x, lease) {
    pd_lease_assert(lease)
    list(success = TRUE, version_id = "inventory-v1")
  }
  testthat::local_mocked_bindings(
    st_versions = function(path, alias) data.table::data.table(
      version_id = "m1", content_hash = "h"
    ), .package = "stamp"
  )
  out <- pd_checkpoint(master, "metadata", results, context, lease,
                       pd_empty_manifest(context), writer, writer, root)
  expect_identical(out$version_id_metadata, "m1")
  expect_length(pd_manifest_files(context, root), 1L)
})

test_that("checkpoint canonical inputs come from verified stage results", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  withr::defer(pd_lease_release(fixture$lease))
  fixture$results[, data_version_id := "fresh-data-v2"]
  fixture$execution$snapshot <- list(current = data.table::data.table(
    stage = "metadata", entity_id = "p", output_version_id = "stale-data-v1",
    input_hash = "stale-input"
  ))
  writer <- function(...) list(success = TRUE, version_id = "inventory")
  testthat::local_mocked_bindings(
    pd_assert_execution_fence = function(execution) invisible(execution),
    pd_manifest_publish = function(payload, ...) payload,
    .package = "pipdata"
  )
  finalized <- pd_finalize_checkpoint(
    fixture$execution, fixture$master, "metadata", fixture$results,
    writer, writer, fixture$root
  )
  canonical <- finalized$execution$manifest$inputs[name == "canonical"]
  expect_identical(canonical$version_id, "fresh-data-v2")
  expect_identical(canonical$content_hash, "input")
})
