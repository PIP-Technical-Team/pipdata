---
date: 2026-04-07
title: "Internal logmeta type markers polluting build_type_summary() table"
category: "bugs"
language: "R"
tags: [log_report, piplog, logmeta, build_type_summary, pipfun]
root-cause: "build_type_summary() aggregated all logmeta error_type values without filtering out internal pipeline markers, mixing diagnostic rows with genuine errors"
severity: "P2"
---

# Internal logmeta type markers polluting `build_type_summary()` table

## Problem

`log_report()` renders a "Summary by Type" table via `build_type_summary()`.
The table was supposed to show only genuine pipeline errors and warnings so
operators can quickly see what went wrong. Instead it also showed rows for
internal markers like `process_summary_inf`, `aux_changes_inf`,
`inv_update_inf`, etc. — entries that exist only to carry structured metadata
for other report sections, not to flag actual problems.

Symptoms:
- "Summary by Type" table had confusing rows labelled `process_summary_inf`,
  `aux_changes_inf`, `null_svys_inf` alongside real errors.
- Row count was inflated, making the table harder to read.

## Root Cause

`log_report()` uses `log_info()` / `log_error()` / `log_add()` with a
logmeta `info` or `error` key to carry structured metadata for specialised
report sections (processing summary, aux changes, inventory verification,
null surveys). `parse_log_meta()` extracts these keys into a unified
`error_type` column without distinguishing internal markers from genuine
pipeline errors. `build_type_summary()` then aggregated by `error_type`
without any exclusion filter, so those internal markers appeared in the
summary table.

## Solution

1. Added a character vector `.log_internal_types` in `R/aaa.R` listing all
   logmeta type markers that exist only to carry structured metadata:

```r
# R/aaa.R
.log_internal_types <- c(
  "process_summary_inf",
  "aux_changes_inf",
  "inv_update_inf",
  "null_svys_inf",
  "skipped_svys_data",
  "skipped_svys_metadata"
)
```

2. Applied the filter inside `build_type_summary()`:

```r
# R/log_report.R
build_type_summary <- function(dt) {
  tbl <- dt[
    !error_type %in% .log_internal_types,
    .N,
    by = .(event, error_type, message)
  ][order(event, -N)]
  # ... render table ...
}
```

3. Added two regression tests in `tests/testthat/test-log_report.R`:

```r
test_that("build_type_summary excludes internal logmeta types", {
  log <- make_piplog(
    make_entry("error", "There is no gd_type variable",
               list(error = "gd_type_miss", survey = "BOL_1990_EPF")),
    make_entry("info", "Processing complete.",
               list(info = "process_summary_inf", n_total = 1L, ...)),
    make_entry("info", "Aux changes.",
               list(info = "aux_changes_inf", measures = "cpi", ...))
  )
  dt <- parse_log_meta(log)
  out <- build_type_summary(dt)
  expect_true(any(grepl("gd_type_miss", out)))      # real error shown
  expect_false(any(grepl("process_summary", out)))  # internal type excluded
  expect_false(any(grepl("aux_changes", out)))      # internal type excluded
})

test_that("build_type_summary returns table with only genuine errors when mixed", {
  # Only internal types in the log -> empty data table body, header still present
  log <- make_piplog(
    make_entry("info", "Processing complete.",
               list(info = "process_summary_inf", n_total = 0L, ...))
  )
  dt <- parse_log_meta(log)
  out <- build_type_summary(dt)
  expect_true(any(grepl("Summary by Type", out)))   # section header present
  expect_equal(length(out), 4L)                     # header + divider only, no data rows
})
```

## Prevention

- **Every** new `log_info()` / `log_add()` call that uses the `info` or
  `error` logmeta key as a structured metadata carrier (not a real error)
  **must** also have its marker string added to `.log_internal_types` in
  `R/aaa.R`.
- When adding a new logmeta type marker, update the list and add a regression
  test verifying it is excluded from the summary table.
- Do not reuse the `error` key for informational metadata — prefer the `info`
  key for structured info entries to keep the separation of concerns clear.

## Related

- [2026-04-07-synthetic-piplog-testing-pattern.md](../testing-patterns/2026-04-07-synthetic-piplog-testing-pattern.md) — test helpers used in the regression tests above
- [2026-04-29-duplicate-logmeta-discriminator-key.md](./2026-04-29-duplicate-logmeta-discriminator-key.md) — later bug: two different events reusing the same discriminator string causes malformed report sections
