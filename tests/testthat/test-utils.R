dt <- data.table::data.table(a = 1, b = 1:10, c = 5)

test_that("uniq_vars_to_attr works", {
  out <- uniq_vars_to_attr(dt)
  atr <- attributes(out)
  expect_equal(dim(out), c(10, 1))
  expect_length(atr, 6)
  expect_equal(atr$a, 1)
  expect_equal(atr$c, 5)
})

test_that("uniq_vars_to_attr returns error", {
  expect_error(uniq_vars_to_attr(dt, "a1"))
})

test_that("uniq_vars_to_attr works correctly with exclude_vars", {
  dt <- data.table::data.table(a = 1, b = 1:10, c = 5)
  out <- uniq_vars_to_attr(dt, "a")
  atr <- attributes(out)
  expect_equal(dim(out), c(10, 2))
  expect_length(atr, 5)
  expect_equal(atr$c, 5)
})

test_that("vars_to_attr works correctly", {
  dt <- data.table::data.table(a = c(1, 2), b = 1:10, c = 5)
  out <- vars_to_attr(dt, "a")
  atr <- attributes(out)
  expect_equal(dim(out), c(10, 2))
  expect_length(atr, 5)
  expect_equal(atr$a, c(1, 2))
})


test_that("num_vars_to_attr works correctly", {
  dt <- data.table(a = c(1, 2), b = 1:10, c = c("a", "b"))
  out <- num_vars_to_attr(dt, "a", "c")
  atr <- attributes(out)
  expect_equal(dim(out), c(10, 1))
  expect_length(atr, 5)
  expect_equal(atr$a, c(a = 1, b = 2))
})

test_that("num_vars_to_attr num and name vars are of different length", {
  dt <- data.table(a = c(1, 2), b = 1:10, c = c("a", "b"))
  expect_error(num_vars_to_attr(dt, c("a", "b"), "c"))
})


test_that("num_vars_to_attr num and name varshave different unique values", {
  dt <- data.table(a = c(1, 2), b = 1:10, c = c("a", "b", "c", "d", "e"))
  expect_error(num_vars_to_attr(dt, "a", "c"))
})

# add_log() key-naming contract -----------------------------------------------

test_that("add_log() stores entry under 'log_<class>' key in .pipdataenv", {
  on.exit(pd_env_rm("log_piperr"), add = TRUE)

  # Ensure clean state
  pd_env_rm("log_piperr")

  add_log("test message", error = "store_release_err", class = "piperr")

  result <- pd_env_get("log_piperr")
  expect_false(is.null(result))
  expect_true("store_release_err" %in% names(result))
})

test_that("add_log() appends to existing entry for the same error name", {
  on.exit(pd_env_rm("log_piperr"), add = TRUE)

  pd_env_rm("log_piperr")

  add_log("first", error = "my_err", class = "piperr")
  add_log("second", error = "my_err", class = "piperr")

  result <- pd_env_get("log_piperr")
  expect_length(result[["my_err"]][[1]], 2)
})

test_that("add_log() uses separate key for different class values", {
  on.exit({
    pd_env_rm("log_piperr")
    pd_env_rm("log_unk_err")
  }, add = TRUE)

  pd_env_rm("log_piperr")
  pd_env_rm("log_unk_err")

  add_log("msg1", error = "e1", class = "piperr")
  add_log("msg2", error = "e2", class = "unk_err")

  expect_false(is.null(pd_env_get("log_piperr")))
  expect_false(is.null(pd_env_get("log_unk_err")))
  expect_null(pd_env_get("log_piperr")[["e2"]])
})
