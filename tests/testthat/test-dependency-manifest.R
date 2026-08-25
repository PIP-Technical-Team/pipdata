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
