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
