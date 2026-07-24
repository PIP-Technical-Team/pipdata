# Helper: build a minimal pipmd data.table for recode tests
make_pipmd <- function(...) {
  dt <- data.table::data.table(...)
  data.table::setattr(dt, "class", c("pipmd", "data.table", "data.frame"))
  dt
}

# ── recode_edu ────────────────────────────────────────────────────────────────

test_that("recode_edu() clamps educy: negative → NA, 0-50 → pass-through, >50 → NA", {
  dt <- make_pipmd(educy = c(-1, 0, 25, 50, 51, NA_real_))
  result <- suppressWarnings(pipdata:::recode_edu(dt))
  expect_equal(result$educy, c(NA_real_, 0, 25, 50, NA_real_, NA_real_))
})

test_that("recode_edu() preserves educy boundary values 0 and 50 exactly", {
  dt <- make_pipmd(educy = c(0, 50))
  result <- suppressWarnings(pipdata:::recode_edu(dt))
  expect_equal(result$educy, c(0, 50))
})

test_that("recode_edu() handles all-NA educy column", {
  dt <- make_pipmd(educy = NA_real_)
  result <- suppressWarnings(pipdata:::recode_edu(dt))
  expect_true(is.na(result$educy))
})

test_that("recode_edu() returns dt unchanged when educy column is absent", {
  dt <- make_pipmd(welfare = 100)
  result <- suppressWarnings(pipdata:::recode_edu(dt))
  expect_equal(names(result), "welfare")
})

test_that("recode_edu() recodes literacy: 1 → 'yes', 0 → 'no', other → NA", {
  dt <- make_pipmd(literacy = c(1L, 0L, 99L, NA_integer_))
  result <- suppressWarnings(pipdata:::recode_edu(dt))
  expect_equal(result$literacy, c("yes", "no", NA_character_, NA_character_))
})

test_that("recode_edu() returns dt unchanged when literacy column is absent", {
  dt <- make_pipmd(welfare = 100)
  result <- suppressWarnings(pipdata:::recode_edu(dt))
  expect_false("literacy" %in% names(result))
})

test_that("recode_edu() recodes school: 1 → 'yes', 0 → 'no', other → NA", {
  dt <- make_pipmd(school = c(1L, 0L, NA_integer_))
  result <- suppressWarnings(pipdata:::recode_edu(dt))
  expect_equal(result$school, c("yes", "no", NA_character_))
})

test_that("recode_edu() returns dt unchanged when school column is absent", {
  dt <- make_pipmd(welfare = 100)
  result <- suppressWarnings(pipdata:::recode_edu(dt))
  expect_false("school" %in% names(result))
})

# ── recode_gndr ───────────────────────────────────────────────────────────────

test_that("recode_gndr() recodes male: 1 → 'male', 0 → 'female', NA → NA", {
  dt <- make_pipmd(male = c(1L, 0L, NA_integer_))
  result <- suppressWarnings(pipdata:::recode_gndr(dt))
  expect_equal(result$gender, c("male", "female", NA_character_))
})

test_that("recode_gndr() returns dt unchanged when male column is absent", {
  dt <- make_pipmd(welfare = 100)
  result <- suppressWarnings(pipdata:::recode_gndr(dt))
  expect_false("gender" %in% names(result))
})

test_that("recode_gndr() handles unexpected male values as NA", {
  dt <- make_pipmd(male = c(1L, 0L, 2L, 99L))
  result <- suppressWarnings(pipdata:::recode_gndr(dt))
  expect_equal(result$gender, c("male", "female", NA_character_, NA_character_))
})

# ── recode_age ────────────────────────────────────────────────────────────────

test_that("recode_age() clamps age: negative → NA, 0-110 → pass-through, >110 → NA", {
  dt <- make_pipmd(age = c(-1, 0, 55, 110, 111, NA_real_))
  result <- suppressWarnings(pipdata:::recode_age(dt))
  expect_equal(result$age, c(NA_real_, 0, 55, 110, NA_real_, NA_real_))
})

test_that("recode_age() preserves boundary values 0 and 110 exactly", {
  dt <- make_pipmd(age = c(0, 110))
  result <- suppressWarnings(pipdata:::recode_age(dt))
  expect_equal(result$age, c(0, 110))
})

test_that("recode_age() handles all-NA age column", {
  dt <- make_pipmd(age = NA_real_)
  result <- suppressWarnings(pipdata:::recode_age(dt))
  expect_true(is.na(result$age))
})

test_that("recode_age() returns dt unchanged when age column is absent", {
  dt <- make_pipmd(welfare = 100)
  result <- suppressWarnings(pipdata:::recode_age(dt))
  expect_false("age" %in% names(result))
})

# ── deprecation warnings ──────────────────────────────────────────────────────

test_that("recode_edu() is deprecated in favor of apply_recode_spec()", {
  dt <- make_pipmd(educy = 25)
  result <- NULL
  expect_warning(
    { result <- pipdata:::recode_edu(dt) },
    "apply_recode_spec"
  )
  expect_equal(result$educy, 25)
})

test_that("recode_gndr() is deprecated in favor of apply_recode_spec()", {
  dt <- make_pipmd(male = 1L)
  result <- NULL
  expect_warning(
    { result <- pipdata:::recode_gndr(dt) },
    "apply_recode_spec"
  )
  expect_equal(result$gender, "male")
})

test_that("recode_age() is deprecated in favor of apply_recode_spec()", {
  dt <- make_pipmd(age = 40)
  result <- NULL
  expect_warning(
    { result <- pipdata:::recode_age(dt) },
    "apply_recode_spec"
  )
  expect_equal(result$age, 40)
})

test_that("add_area.pipmd() is deprecated in favor of apply_recode_spec()", {
  dt <- make_pipmd(urban = c(1L, 0L))
  result <- NULL
  expect_warning(
    { result <- pipdata:::add_area.pipmd(dt) },
    "apply_recode_spec"
  )
  expect_equal(result$area, c("urban", "rural"))
})
