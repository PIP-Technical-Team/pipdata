---
date: 2026-05-22
title: "adjust_population silent NaN weights when 'year' column absent"
category: "bugs"
type: "bug"
language: "R"
tags: [adjust_population, deflation, year, survey_year, NaN, weights, ARG, subnational]
root-cause: "adjust_population() read df$survey_year[[1L]] which returned NULL for surveys whose column is named 'year', causing diffs = numeric(0), min(numeric(0)) = Inf, and all weights becoming NaN silently."
severity: "P1"
test-written: "yes"
fix-confirmed: "yes"
---

# adjust_population silent NaN weights when 'year' column absent

## Symptom

Deflating ARG 2003 (an urban-only subnational survey with `pop_data_level = "area"`)
produced a dataset where `welfare` and `weight` were both `NA`/`NaN` for every row.
The joyn merge report was visible in the console but no error was thrown, so the
survey silently passed through as corrupt output.

BOL 2022 was unaffected because its `pop_data_level = "national"`, so
`adjust_population()` was never called for it.

## Root Cause

In `adjust_population()` (named-vector path, `R/pd_deflation.R`), the survey year
was read as:

```r
survey_year <- df$survey_year[[1L]]
```

The canonical column name in survey data.tables is `year`, not `survey_year`.
For ARG 2003 (and likely other surveys), `df$survey_year` returns `NULL`.

This cascaded silently:

```r
diffs <- abs(pop_years - NULL)   # → numeric(0)
min_d <- min(diffs)              # → Inf  (with a "no non-missing" warning)
keep  <- diffs == min_d          # → logical(0)
pop_val <- weighted.mean(vals[keep], w = 1/diffs[keep])  # → NaN
# → weight := weight * NaN  → all weights NaN
```

No error was raised. The joyn report was the only visible symptom.

A secondary gap: `.validate_deflation_input()` did not check for NA/NaN values
in `welfare` or `weight`, so the corrupt output passed validation downstream.

## Reproduction Test

Added to `tests/testthat/test-pd-deflation.R`:

```r
test_that("adjust_population (named vector) aborts when 'year' column is missing", {
  df <- data.table::data.table(
    country_code = "ABC",
    area = c("urban", "urban"),
    weight = c(300, 700)
    # no 'year' column
  )
  pop <- c(`2003_urban` = 2000000)

  expect_error(
    suppressMessages(pipdata:::adjust_population(df, pop)),
    class = "adjust_population"
  )
})
```

Also added NA-check tests for `.validate_deflation_input()`:

```r
test_that(".validate_deflation_input aborts when welfare has NAs", {
  dt <- make_pipmd(welfare = c(5, NA, 15))
  expect_error(pipdata:::.validate_deflation_input(dt), class = "validate_deflation_input")
})

test_that(".validate_deflation_input aborts when weight has NAs", {
  dt <- make_pipmd(weight = c(100, NA, 100))
  expect_error(pipdata:::.validate_deflation_input(dt), class = "validate_deflation_input")
})
```

Tests confirmed failing on the original code before any fix was applied.

## Fix

**1. Standardize on `year` in `adjust_population()` (`R/pd_deflation.R`):**

Replace the silent `df$survey_year[[1L]]` read with an explicit guard:

```r
# Before (silent NULL when column is 'year'):
survey_year <- df$survey_year[[1L]]

# After (explicit abort if 'year' absent):
if (!"year" %in% names(df)) {
  cli::cli_abort(
    "{.fn adjust_population} requires a {.field year} column in {.arg df}.",
    class = c("adjust_population", "piperr")
  )
}
survey_year <- df$year[[1L]]
```

All test fixtures (`make_pipmd()`) and `adjust_population` unit tests updated to
use the column name `year` throughout.

**2. Add NA check to `.validate_deflation_input()` (`R/pd_deflation.R`):**

```r
na_cols <- required_cols[vapply(required_cols, function(col) anyNA(dt[[col]]), logical(1L))]
if (length(na_cols) > 0L) {
  cli::cli_abort(
    "Input has NA values in required columns: {.field {na_cols}}.",
    class = c("validate_deflation_input", "piperr")
  )
}
```

This provides a hard stop at the deflation entry point even if a future code
path re-introduces silent NaN production.

## Lessons Learned

- **`df$nonexistent_col[[1L]]` returns `NULL` silently in R** — never index into
  a potentially-absent column without first checking `%in% names(df)`. Use an
  explicit `cli::cli_abort()` guard.
- **Standardize column names and enforce them explicitly.** The pipeline uses
  `year`; any code reading a year column must use `year`, not `survey_year` or
  any other alias. When in doubt, abort loudly rather than falling back silently.
- **Validate outputs at entry gates, not just structure.** `.validate_deflation_input()`
  already checked for column presence; it now also checks for NA content so that
  corrupt inputs are caught before they silently corrupt outputs.

## Related

- `.cg-docs/solutions/bugs/2026-05-06-subnational-deflation-area-attribute-not-resolved.md` — prior subnational deflation bug (area attribute not resolved)
- `.cg-docs/solutions/bugs/2026-05-19-datatable-scoping-column-vs-argument.md` — related pattern of silent wrong-variable reads in data.table context
