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

test_that("inventory-ahead failure retains prior authority and reschedules", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  withr::defer(pd_lease_release(fixture$lease))
  prior <- pd_manifest_publish(
    pd_empty_manifest(fixture$context),
    fixture$context,
    fixture$lease,
    fixture$root,
    parent = NULL
  )
  prior_identity <- attr(prior, "manifest_identity")
  fixture$execution$manifest <- prior
  fixture$execution$manifest_identity <- prior_identity
  writes <- 0L
  writer <- function(...) {
    writes <<- writes + 1L
    list(success = TRUE, version_id = paste0("inventory-", writes))
  }
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

  retained <- pd_manifest_read(fixture$context, fixture$root)
  expect_identical(attr(retained, "manifest_identity"), prior_identity)
  expect_identical(writes, 2L)
  snapshot <- list(
    current = data.table::data.table(
      stage = "metadata", entity_id = "p", survey_id = "s", pip_id = "p",
      output_version_id = "m1", output_hash = "h", input_hash = "input",
      legacy_input_hash = "input", legacy_input_version = "m1",
      code_hash = "code", input_rows = list(data.table::data.table())
    ),
    fingerprints = list(
      components = data.table::data.table(
        stage = character(), component = character(), hash = character()
      )
    )
  )
  facts <- pd_snapshot_facts(snapshot, retained)
  expect_identical(facts$reason, "unknown_provenance")
})

test_that("inventory-ahead replay publishes manifest without duplicate writes", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  withr::defer(pd_lease_release(fixture$lease))
  fixture$master[, `:=`(
    version_id_metadata = "m1", content_hash_metadata = "h",
    version_id_deflated = NA_character_,
    content_hash_deflated = NA_character_, deflated = FALSE,
    latest_release_version_id = "release-v1"
  )]
  fixture$execution$snapshot <- list(catalogs = list(
    pip_inv = data.table::data.table(
      path = "pip_release_inventory.qs2", version_id = "release-v1",
      content_hash = "release-h1"
    )
  ))
  writer_calls <- 0L
  writer <- function(...) {
    writer_calls <<- writer_calls + 1L
    rlang::abort("Unchanged inventory must not be rewritten.")
  }
  testthat::local_mocked_bindings(
    pd_assert_execution_fence = function(...) invisible(NULL),
    pd_manifest_publish = function(payload, ...) payload,
    .package = "pipdata"
  )

  finalized <- pd_finalize_checkpoint(
    fixture$execution, fixture$master, "metadata", fixture$results,
    writer, writer, fixture$root
  )

  expect_identical(writer_calls, 0L)
  expect_identical(finalized$candidate, fixture$master)
  expect_identical(
    finalized$execution$manifest$records[
      stage == "metadata", output_version_id
    ],
    "m1"
  )
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

test_that("clean receipt-set canonicalization is symmetric and order stable", {
  receipts <- data.table::data.table(
    pip_id = c("P2", "P1"), alias = "pip", artifact = c("P2", "P1"),
    path = c("p2.qs2", "p1.qs2"), version_id = c("v2", "v1"),
    content_hash = c("h2", "h1"), success = TRUE
  )

  forward <- pd_clean_receipt_set(receipts)
  reverse <- pd_clean_receipt_set(receipts[2:1])

  expect_identical(forward, reverse)
  expect_identical(forward$receipts$pip_id, c("P1", "P2"))
  expect_true(nzchar(forward$output_version_id))
  expect_true(nzchar(forward$output_hash))

  expect_error(
    pd_clean_receipt_set(receipts[pip_id == "P1"], c("P1", "P2")),
    class = "pipdata_clean_output_incomplete"
  )
})

test_that("checkpoint publishes finalized named rows and advances one stage", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  withr::defer(pd_lease_release(fixture$lease))
  fixture$execution$manifest$records <- data.table::data.table(
    stage = "clean", entity_id = "s", output_version_id = "clean-set-v1",
    output_hash = "clean-set-h1", input_hash = "clean-input-h1",
    code_hash = "clean-code-h1", output_receipts = list(list(list(
      alias = "pip", artifact = "p", path = "p.qs2",
      version_id = "data-final", content_hash = "data-final-hash"
    )))
  )
  fixture$execution$manifest$inputs <- data.table::data.table(
    stage = "clean", entity_id = "s", name = "canonical",
    version_id = "clean-input-v1", content_hash = "clean-input-h1"
  )
  fixture$execution$manifest$fingerprints <- data.table::data.table(
    stage = c("clean", "metadata"), component = c("recode_spec.yml", "meta_fn"),
    hash = c("pending-clean-old", "metadata-old")
  )
  accepted <- pd_build_input_rows(
    "metadata", "p",
    data.table::data.table(
      name = c("clean_data", "aux_cpi"),
      version_id = c("data-old", "cpi-v1"),
      content_hash = c("data-old-hash", "cpi-h1")
    )
  )
  fixture$execution$snapshot <- list(
    current = data.table::data.table(
      stage = "metadata", entity_id = "p", input_rows = list(accepted)
    ),
    fingerprints = list(
      summary = data.table::data.table(stage = "metadata", hash = "code"),
      components = data.table::data.table(
        stage = c("clean", "metadata"),
        component = c("recode_spec.yml", "meta_fn"),
        hash = c("pending-clean-new", "metadata-new")
      )
    )
  )
  fixture$results[, `:=`(
    data_version_id = "data-final", data_hash = "data-final-hash"
  )]
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
  inputs <- finalized$execution$manifest$inputs[stage == "metadata"]
  fingerprints <- finalized$execution$manifest$fingerprints

  expect_identical(inputs$name, c("aux_cpi", "canonical", "clean_data"))
  expect_identical(
    inputs[name == "clean_data", .(version_id, content_hash)],
    data.table::data.table(
      version_id = "data-final", content_hash = "data-final-hash"
    )
  )
  expect_identical(
    finalized$execution$manifest$records[
      stage == "metadata", input_hash
    ],
    inputs[name == "canonical", content_hash]
  )
  expect_identical(
    fingerprints[stage == "clean", hash], "pending-clean-old"
  )
  expect_identical(
    fingerprints[stage == "metadata", hash], "metadata-new"
  )
})

test_that("committed upstream mismatch fails before inventory writers", {
  fixture <- checkpoint_fixture(withr::local_tempdir())
  withr::defer(pd_lease_release(fixture$lease))
  accepted <- pd_build_input_rows(
    "deflate", "p",
    data.table::data.table(
      name = c(
        "clean_data", "metadata", "aux_cpi", "aux_ppp", "aux_pop"
      ),
      version_id = c("data-v1", "meta-v1", "cpi-v1", "ppp-v1", "pop-v1"),
      content_hash = c("data-h1", "meta-h1", "cpi-h1", "ppp-h1", "pop-h1")
    )
  )
  fixture$execution$snapshot <- list(
    current = data.table::data.table(
      stage = "deflate", entity_id = "p", input_rows = list(accepted)
    ),
    fingerprints = list(components = data.table::data.table(
      stage = character(), component = character(), hash = character()
    ))
  )
  fixture$results[, `:=`(
    data_version_id = "data-v2", metadata_version_id = "meta-v1"
  )]
  writer_calls <- 0L
  writer <- function(...) {
    writer_calls <<- writer_calls + 1L
    list(success = TRUE, version_id = "inventory")
  }
  testthat::local_mocked_bindings(
    pd_assert_execution_fence = function(execution) invisible(execution),
    .package = "pipdata"
  )

  expect_error(
    pd_finalize_checkpoint(
      fixture$execution, fixture$master, "deflate", fixture$results,
      writer, writer, fixture$root
    ),
    class = "pipdata_checkpoint_provenance_error"
  )
  expect_identical(writer_calls, 0L)
})

test_that("finalized named provenance matches committed receipts and code", {
  run_case <- function(data_version, data_hash, code_hash) {
    root <- withr::local_tempdir()
    context <- list(scope_id = "scope")
    lease <- pd_lease_acquire(context, root)
    withr::defer(pd_lease_release(lease))
    manifest <- pd_empty_manifest(context)
    manifest$records <- data.table::data.table(
      stage = "clean", entity_id = "s", output_version_id = "clean-set-v1",
      output_hash = "clean-set-h1", input_hash = "clean-input-h1",
      code_hash = "clean-code-h1", output_receipts = list(list(list(
        alias = "pip", artifact = "p", path = "p.qs2",
        version_id = "data-v1", content_hash = "data-h1"
      )))
    )
    manifest$inputs <- data.table::data.table(
      stage = "clean", entity_id = "s", name = "canonical",
      version_id = "clean-input-v1", content_hash = "clean-input-h1"
    )
    pd_validate_manifest(manifest)
    accepted <- pd_build_input_rows(
      "metadata", "p",
      data.table::data.table(
        name = c("clean_data", "aux_cpi"),
        version_id = c("data-v1", "cpi-v1"),
        content_hash = c("data-h1", "cpi-h1")
      )
    )
    execution <- list(
      context = context, lease = lease, manifest = manifest,
      manifest_identity = NULL,
      snapshot = list(
        current = data.table::data.table(
          stage = "metadata", entity_id = "p", input_rows = list(accepted)
        ),
        fingerprints = list(
          summary = data.table::data.table(
            stage = "metadata", hash = "metadata-code-h1"
          ),
          components = data.table::data.table(
            stage = "metadata", component = "metadata_fn",
            hash = "metadata-code-h1"
          )
        )
      )
    )
    results <- data.table::data.table(
      pip_id = "p", version_id = "meta-v1", content_hash = "meta-h1",
      success = TRUE, input_hash = "claimed-input", code_hash = code_hash,
      data_version_id = data_version, data_hash = data_hash,
      alias = "pip_meta", artifact = "p", path = "p-meta.qs2"
    )
    writer_calls <- 0L
    writer <- function(...) {
      writer_calls <<- writer_calls + 1L
      list(success = TRUE, version_id = "inventory-v1")
    }
    testthat::local_mocked_bindings(
      pd_assert_execution_fence = function(execution) invisible(execution),
      .package = "pipdata"
    )
    testthat::local_mocked_bindings(
      st_versions = function(path, alias) data.table::data.table(
        version_id = "meta-v1", content_hash = "meta-h1"
      ),
      .package = "stamp"
    )

    expect_error(
      pd_finalize_checkpoint(
        execution,
        data.table::data.table(survey_id = "s", pip_id = "p"),
        "metadata", results, writer, writer, root
      ),
      class = "pipdata_checkpoint_provenance_error"
    )
    expect_identical(writer_calls, 0L)
  }

  run_case("invented-data-v2", "invented-data-h2", "metadata-code-h1")
  run_case("data-v1", "data-h1", "invented-metadata-code-h2")
})
