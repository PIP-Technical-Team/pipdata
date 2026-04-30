# Tests for the unified package-environment accessor helpers defined in aaa.R:
# pd_env_set(), pd_env_get(), pd_env_rm(), pd_env_reset(), pd_env_append()
#
# Each test cleans up after itself via withr::defer or explicit teardown so it
# does not affect other tests or the live .pipdataenv state.

# ---------------------------------------------------------------------------
# pd_env_set / pd_env_get
# ---------------------------------------------------------------------------

test_that("pd_env_set stores a value retrievable by pd_env_get", {
  withr::defer(pipdata:::pd_env_rm("test_key"))
  pipdata:::pd_env_set("test_key", 42L)
  expect_equal(pipdata:::pd_env_get("test_key"), 42L)
})

test_that("pd_env_get returns default when key is absent", {
  expect_null(pipdata:::pd_env_get("nonexistent_key"))
  expect_equal(
    pipdata:::pd_env_get("nonexistent_key", default = "fallback"),
    "fallback"
  )
})

test_that("pd_env_set overwrites an existing value", {
  withr::defer(pipdata:::pd_env_rm("test_overwrite"))
  pipdata:::pd_env_set("test_overwrite", "first")
  pipdata:::pd_env_set("test_overwrite", "second")
  expect_equal(pipdata:::pd_env_get("test_overwrite"), "second")
})

# ---------------------------------------------------------------------------
# pd_env_rm
# ---------------------------------------------------------------------------

test_that("pd_env_rm removes an existing key", {
  pipdata:::pd_env_set("test_rm_key", TRUE)
  pipdata:::pd_env_rm("test_rm_key")
  expect_null(pipdata:::pd_env_get("test_rm_key"))
})

test_that("pd_env_rm is a no-op for absent keys", {
  expect_null(pipdata:::pd_env_rm("never_existed"))
})

# ---------------------------------------------------------------------------
# pd_env_reset
# ---------------------------------------------------------------------------

test_that("pd_env_reset removes all keys from the environment", {
  pipdata:::pd_env_set("reset_a", 1)
  pipdata:::pd_env_set("reset_b", 2)
  pipdata:::pd_env_reset()
  expect_null(pipdata:::pd_env_get("reset_a"))
  expect_null(pipdata:::pd_env_get("reset_b"))
})

test_that("pd_env_reset on empty environment does not error", {
  pipdata:::pd_env_reset()
  expect_null(pipdata:::pd_env_reset())
})

# ---------------------------------------------------------------------------
# pd_env_append
# ---------------------------------------------------------------------------

test_that("pd_env_append creates a new entry when key is absent", {
  withr::defer(pipdata:::pd_env_rm("test_append"))
  dt <- data.table::data.table(x = 1:3)
  pipdata:::pd_env_append("test_append", dt)
  result <- pipdata:::pd_env_get("test_append")
  expect_equal(nrow(result), 3L)
  expect_equal(result$x, 1:3)
})

test_that("pd_env_append rbinds onto existing data.table", {
  withr::defer(pipdata:::pd_env_rm("test_append2"))
  dt1 <- data.table::data.table(x = 1:2)
  dt2 <- data.table::data.table(x = 3:4)
  pipdata:::pd_env_append("test_append2", dt1)
  pipdata:::pd_env_append("test_append2", dt2)
  result <- pipdata:::pd_env_get("test_append2")
  expect_equal(nrow(result), 4L)
  expect_equal(result$x, 1:4)
})
