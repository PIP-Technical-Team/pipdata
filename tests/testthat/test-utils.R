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
  expect_error(uniq_vars_to_attr(dt, "a1"), "a1 is not a column name in data. Choose one of a, b, and c")
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
  expect_error(num_vars_to_attr(dt, c("a", "b"), "c"), "num_var and name_var should be of same length")
})


test_that("num_vars_to_attr num and name varshave different unique values", {
  dt <- data.table(a = c(1, 2), b = 1:10, c = c("a", "b", "c", "d", "e"))
  expect_error(num_vars_to_attr(dt, "a", "c"), "The unique values in num_var and name_var column are not equal")
})
