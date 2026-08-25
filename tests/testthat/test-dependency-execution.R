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
