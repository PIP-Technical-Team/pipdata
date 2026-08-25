test_that("typed receipt rejects write failures", {
  local_mocked_bindings(
    pip_write = function(...) stop("write failed"), .package = "pipload"
  )
  local_mocked_bindings(st_hash_obj = function(x) "h", .package = "stamp")
  receipt <- pd_save_receipt(data.frame(x = 1), "id", "pip")
  expect_false(receipt$success)
  expect_match(receipt$error, "write failed")
})

test_that("typed receipt accepts one exact stamp row", {
  local_mocked_bindings(
    pip_write = function(...) list(path = "artifact.qs2", version_id = "v1",
                                   metadata = list(content_hash = "h")),
    .package = "pipload"
  )
  local_mocked_bindings(
    st_hash_obj = function(x) "h",
    st_versions = function(path, alias) data.table::data.table(
      version_id = "v1", content_hash = "h"
    ),
    .package = "stamp"
  )
  receipt <- pd_save_receipt(data.frame(x = 1), "id", "pip")
  expect_true(receipt$success)
  expect_identical(receipt$version_id, "v1")
})

test_that("receipt revalidation rejects ambiguous exact history", {
  receipt <- list(
    alias = "pip", artifact = "p", path = "p.qs2",
    version_id = "v1", content_hash = "h1", success = TRUE
  )
  local_mocked_bindings(
    st_versions = function(path, alias) data.table::data.table(
      version_id = c("v1", "v1"), content_hash = c("h1", "h1")
    ),
    .package = "stamp"
  )
  expect_error(pd_revalidate_receipt(receipt), class = "pipdata_receipt_stale")
})
