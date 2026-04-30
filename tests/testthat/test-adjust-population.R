# Tests for adjust_population() — internal helper in pd_deflation.R
#
# adjust_population() scales subnational survey weights to match national
# population accounts (WDI). It finds the closest year in `pop` for each
# reporting level, then scales `weight` by pop/survey_weight.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_df <- function(
  reporting_levels,
  weights,
  country = "ABC",
  survey_year = 2010L
) {
  n <- length(weights)
  lvls <- rep(reporting_levels, length.out = n)
  data.table::data.table(
    country_code = country,
    survey_year = survey_year,
    reporting_level = lvls,
    weight = as.numeric(weights)
  )
}

make_pop <- function(years, pops, levels, country = "ABC") {
  data.table::data.table(
    country_code = country,
    year = as.integer(years),
    pop_data_level = levels,
    pop = as.numeric(pops)
  )
}

# ---------------------------------------------------------------------------
# 1. Exact year match, single reporting level
# ---------------------------------------------------------------------------

test_that("exact year match scales weights by pop / sum(weight)", {
  df <- make_df("national", c(100, 200, 300)) # total = 600
  pop <- make_pop(2010L, 1200, "national") # factor = 1200/600 = 2

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  expect_equal(result$weight, c(200, 400, 600))
})

test_that("exact year match: wght = 1 (not 1/diff_year) when diff_year == 0", {
  # Two pop rows: exact year and a nearby year.
  # Only the exact-year row should survive the .SD[diff_year == min()] filter.
  df <- make_df("national", c(100, 200)) # total = 300
  pop <- make_pop(c(2010L, 2008L), c(900, 600), c("national", "national"))

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  # diff_year for 2010 = 0 (min), so only the 2010 row survives the filter.
  # pop_fact = 900 / 300 = 3
  expect_equal(result$weight, c(300, 600))
})

# ---------------------------------------------------------------------------
# 2. Closest-year selection when no exact match exists
# ---------------------------------------------------------------------------

test_that("no exact match: closest year is selected", {
  df <- make_df("national", c(100, 200)) # survey_year = 2010, total = 300
  # year 2009 (diff=1) closer than 2005 (diff=5)
  pop <- make_pop(c(2009L, 2005L), c(900, 600), c("national", "national"))

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  # Only 2009 row survives (diff=1 < diff=5).
  # pop_fact = 900 / 300 = 3
  expect_equal(result$weight, c(300, 600))
})

# ---------------------------------------------------------------------------
# 3. Multiple reporting levels with different factors
# ---------------------------------------------------------------------------

test_that("multiple reporting levels are scaled independently", {
  df <- data.table::data.table(
    country_code = "ABC",
    survey_year = 2010L,
    reporting_level = c("national", "national", "urban", "urban"),
    weight = c(100, 200, 50, 50) # national total=300, urban total=100
  )
  pop <- make_pop(
    years = c(2010L, 2010L),
    pops = c(900, 200), # national factor=3, urban factor=2
    levels = c("national", "urban")
  )

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  national_rows <- result[result$reporting_level == "national", ]$weight
  urban_rows <- result[result$reporting_level == "urban", ]$weight

  expect_equal(national_rows, c(300, 600)) # *3
  expect_equal(urban_rows, c(100, 100)) # *2
})

# ---------------------------------------------------------------------------
# 4. Return structure
# ---------------------------------------------------------------------------

test_that("output has the same number of rows as input", {
  df <- make_df("national", c(10, 20, 30, 40))
  pop <- make_pop(2010L, 1000, "national")

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  expect_equal(nrow(result), nrow(df))
})

test_that("output retains all original columns plus pop_fact", {
  df <- make_df("national", c(100, 200))
  pop <- make_pop(2010L, 600, "national")

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  expect_true(all(
    c("country_code", "survey_year", "reporting_level", "weight") %in%
      names(result)
  ))
})
