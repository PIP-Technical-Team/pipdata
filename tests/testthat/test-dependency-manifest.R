test_that("immutable manifest generations round-trip and retain three", {
  root <- withr::local_tempdir()
  context <- list(scope_id = "scope")
  payload <- pd_empty_manifest(context)
  lease <- pd_lease_acquire(context, root)
  withr::defer(pd_lease_release(lease))
  parent <- NULL
  for (i in 1:5) {
    payload$header$created_at <- as.character(i)
    payload <- pd_manifest_publish(payload, context, lease, root, parent)
    parent <- attr(payload, "manifest_identity")
  }
  files <- pd_manifest_files(context, root)
  expect_length(files, 3L)
  expect_identical(pd_manifest_generation(files), c(5, 4, 3))
  expect_identical(pd_manifest_read(context, root)$header$scope_id, "scope")
})

test_that("lease loss fences writes and offline recovery is explicit", {
  root <- withr::local_tempdir()
  context <- list(scope_id = "scope")
  lease <- pd_lease_acquire(context, root)
  expect_error(pd_lease_recover_offline(context, root),
               class = "pipdata_manifest_recovery_confirmation")
  expect_error(pd_lease_recover_offline(context, root, TRUE),
               class = "pipdata_manifest_recovery_owner_alive")
  expect_no_error(pd_lease_assert(lease))
  pd_lease_release(lease)
})

test_that("offline recovery requires demonstrated same-host owner death", {
  root <- withr::local_tempdir()
  context <- list(scope_id = "scope")
  lease <- pd_lease_acquire(context, root)
  owner_path <- fs::path(lease$path, "owner.rds")
  owner <- readRDS(owner_path)
  owner$pid <- .Machine$integer.max
  saveRDS(owner, owner_path)
  quarantine <- pd_lease_recover_offline(context, root, TRUE)
  expect_true(fs::dir_exists(quarantine))
  expect_false(fs::dir_exists(lease$path))
})

test_that("supported API cannot take over a live lease", {
  root <- withr::local_tempdir()
  context <- list(scope_id = "scope")
  lease <- pd_lease_acquire(context, root)
  withr::defer(pd_lease_release(lease))
  expect_error(pd_lease_acquire(context, root),
               class = "pipdata_manifest_lease_held")
  expect_error(pd_lease_recover_offline(context, root, TRUE),
               class = "pipdata_manifest_recovery_owner_alive")
  expect_no_error(pd_lease_assert(lease))
})

test_that("duplicate immutable generations tolerate one valid survivor", {
  root <- withr::local_tempdir()
  context <- list(scope_id = "scope")
  dir <- pd_manifest_dir(context, root)
  fs::dir_create(dir, recurse = TRUE)
  payload <- pd_empty_manifest(context)
  envelope <- list(
    schema_version = 1L, generation = 1, uuid = "valid", parent = NULL,
    payload = payload, checksum = pd_hash_object(payload, algo = "sha256")
  )
  saveRDS(envelope, fs::path(dir,
    "manifest-v1-00000000000000000001-valid.rds"))
  saveRDS(list(corrupt = TRUE), fs::path(dir,
    "manifest-v1-00000000000000000001-corrupt.rds"))
  expect_identical(pd_manifest_read(context, root)$header$scope_id, "scope")
})

test_that("manifest validates named canonical composites and legacy rows", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  manifest$records <- data.table::data.table(
    stage = "clean", entity_id = "survey", output_version_id = "out-v1",
    output_hash = "out-h1", input_hash = "placeholder", code_hash = "code",
    output_receipts = list(list(list(
      alias = "pip", artifact = "PIP_ID", path = "pip.qs2",
      version_id = "out-v1", content_hash = "out-h1"
    )))
  )
  components <- data.table::data.table(
    name = c("dlw", "pfw"), version_id = c("dlw-v1", "pfw-v1"),
    content_hash = c("dlw-h1", "pfw-h1")
  )
  manifest$inputs <- pd_build_input_rows("clean", "survey", components)
  manifest$records$input_hash <- manifest$inputs[
    name == "canonical", content_hash
  ]

  expect_no_error(pd_validate_manifest(manifest))

  corrupt <- data.table::copy(manifest$inputs)
  corrupt[name == "canonical", content_hash := "wrong"]
  manifest$inputs <- corrupt
  expect_error(
    pd_validate_manifest(manifest),
    class = "pipdata_dependency_manifest_invalid"
  )

  manifest$inputs <- data.table::data.table(
    stage = "clean", entity_id = "survey", name = "canonical",
    version_id = "legacy-input-version", content_hash = "legacy-input-hash"
  )
  manifest$records$input_hash <- "legacy-input-hash"
  expect_no_error(pd_validate_manifest(manifest))
})

test_that("manifest records and canonical inputs are bidirectionally exact", {
  context <- list(scope_id = "scope")
  valid <- pd_empty_manifest(context)
  valid$records <- data.table::data.table(
    stage = "clean", entity_id = "survey", output_version_id = "out-v1",
    output_hash = "out-h1", input_hash = "input-h1", code_hash = "code-h1",
    output_receipts = list(list(list(
      alias = "pip", artifact = "PIP_ID", path = "pip.qs2",
      version_id = "out-v1", content_hash = "out-h1"
    )))
  )
  valid$inputs <- data.table::data.table(
    stage = "clean", entity_id = "survey", name = "canonical",
    version_id = "input-v1", content_hash = "input-h1"
  )
  expect_no_error(pd_validate_manifest(valid))

  record_only <- valid
  record_only$inputs <- record_only$inputs[0L]
  expect_error(
    pd_validate_manifest(record_only),
    class = "pipdata_dependency_manifest_invalid"
  )

  input_only <- valid
  input_only$records <- input_only$records[0L]
  expect_error(
    pd_validate_manifest(input_only),
    class = "pipdata_dependency_manifest_invalid"
  )

  mismatched <- valid
  mismatched$records$input_hash <- "different-input-hash"
  expect_error(
    pd_validate_manifest(mismatched),
    class = "pipdata_dependency_manifest_invalid"
  )
})

test_that("legacy canonical-only comparison is current or exactly legacy changed", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  manifest$records <- data.table::data.table(
    stage = "clean", entity_id = "survey", output_version_id = "out-v1",
    output_hash = "out-h1", input_hash = "legacy-hash", code_hash = "code",
    output_receipts = list(list())
  )
  manifest$inputs <- data.table::data.table(
    stage = "clean", entity_id = "survey", name = "canonical",
    version_id = "legacy-version", content_hash = "legacy-hash"
  )
  current_inputs <- pd_build_input_rows(
    "clean", "survey",
    data.table::data.table(
      name = c("dlw", "pfw"), version_id = c("d1", "p1"),
      content_hash = c("dh1", "ph1")
    )
  )
  current <- data.table::data.table(
    stage = "clean", entity_id = "survey", survey_id = "survey",
    pip_id = NA_character_, output_version_id = "out-v1",
    output_hash = "out-h1", input_hash = current_inputs[
      name == "canonical", content_hash
    ], legacy_input_hash = "legacy-hash",
    legacy_input_version = "legacy-version", code_hash = "code",
    input_rows = list(current_inputs)
  )
  snapshot <- list(
    current = current,
    fingerprints = list(
      summary = data.table::data.table(stage = "clean", hash = "code"),
      components = data.table::data.table(
        stage = character(), component = character(), hash = character()
      )
    )
  )

  expect_identical(nrow(pd_snapshot_facts(snapshot, manifest)), 0L)

  snapshot$current[, legacy_input_hash := "changed-legacy-hash"]
  changed <- pd_snapshot_facts(snapshot, manifest)
  expect_identical(changed$reason, "legacy_input_changed")
  expect_identical(changed$input, "canonical")

  snapshot$current[, `:=`(
    legacy_input_hash = "legacy-hash",
    legacy_input_version = "different-legacy-version"
  )]
  changed_version <- pd_snapshot_facts(snapshot, manifest)
  expect_identical(changed_version$reason, "legacy_input_changed")
  expect_identical(changed_version$input, "canonical")
})

test_that("named component comparison owns the exact input reason", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  prior <- pd_build_input_rows(
    "metadata", "PIP_ID",
    data.table::data.table(
      name = c("clean_data", "aux_cpi"),
      version_id = c("data-v1", "cpi-v1"),
      content_hash = c("data-h1", "cpi-old")
    )
  )
  current_inputs <- pd_build_input_rows(
    "metadata", "PIP_ID",
    data.table::data.table(
      name = c("clean_data", "aux_cpi"),
      version_id = c("data-v1", "cpi-v1"),
      content_hash = c("data-h1", "cpi-new")
    )
  )
  manifest$inputs <- prior
  manifest$records <- data.table::data.table(
    stage = "metadata", entity_id = "PIP_ID",
    output_version_id = "meta-v1", output_hash = "meta-h1",
    input_hash = prior[name == "canonical", content_hash],
    code_hash = "metadata-code", output_receipts = list(list())
  )
  snapshot <- list(
    current = data.table::data.table(
      stage = "metadata", entity_id = "PIP_ID", survey_id = "survey",
      pip_id = "PIP_ID", output_version_id = "meta-v1",
      output_hash = "meta-h1",
      input_hash = current_inputs[name == "canonical", content_hash],
      legacy_input_hash = "unused", code_hash = "metadata-code",
      input_rows = list(current_inputs)
    ),
    fingerprints = list(
      summary = data.table::data.table(
        stage = "metadata", hash = "metadata-code"
      ),
      components = data.table::data.table(
        stage = character(), component = character(), hash = character()
      )
    )
  )

  facts <- pd_snapshot_facts(snapshot, manifest)
  expect_identical(facts$reason, "aux_cpi_changed")
  expect_identical(facts$input, "aux_cpi")
  expect_identical(facts$old, "cpi-old")
  expect_identical(facts$new, "cpi-new")
})

test_that("shared auxiliary versions invalidate only changed entity content", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  entity_ids <- c("P1", "P2")
  prior <- data.table::rbindlist(lapply(seq_along(entity_ids), function(i) {
    pd_build_input_rows(
      "metadata", entity_ids[[i]],
      data.table::data.table(
        name = c("clean_data", "aux_cpi"),
        version_id = c(paste0("data-v", i), "cpi-catalog-v1"),
        content_hash = c(paste0("data-h", i), paste0("entity-cpi-h", i))
      )
    )
  }))
  manifest$records <- data.table::data.table(
    stage = "metadata", entity_id = entity_ids,
    output_version_id = paste0("meta-v", seq_along(entity_ids)),
    output_hash = paste0("meta-h", seq_along(entity_ids)),
    input_hash = prior[name == "canonical", content_hash],
    code_hash = "metadata-code", output_receipts = list(list(), list())
  )
  manifest$inputs <- prior
  current_rows <- lapply(seq_along(entity_ids), function(i) {
    content <- if (i == 1L) "entity-cpi-h1" else "entity-cpi-h2-changed"
    inputs <- pd_build_input_rows(
      "metadata", entity_ids[[i]],
      data.table::data.table(
        name = c("clean_data", "aux_cpi"),
        version_id = c(paste0("data-v", i), "cpi-catalog-v2"),
        content_hash = c(paste0("data-h", i), content)
      )
    )
    data.table::data.table(
      stage = "metadata", entity_id = entity_ids[[i]], survey_id = "survey",
      pip_id = entity_ids[[i]], output_version_id = paste0("meta-v", i),
      output_hash = paste0("meta-h", i),
      input_hash = inputs[name == "canonical", content_hash],
      legacy_input_hash = "unused", code_hash = "metadata-code",
      input_rows = list(inputs)
    )
  })
  snapshot <- list(
    current = data.table::rbindlist(current_rows),
    fingerprints = list(
      components = data.table::data.table(
        stage = character(), component = character(), hash = character()
      )
    )
  )

  facts <- pd_snapshot_facts(snapshot, manifest)

  expect_identical(facts$entity_id, "P2")
  expect_identical(facts$reason, "aux_cpi_changed")
})
