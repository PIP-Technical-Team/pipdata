---
date: 2026-04-07
title: "Synthetic piplog construction for unit testing log_report builders"
category: "testing-patterns"
language: "R"
tags: [log_report, piplog, pipfun, testthat, data.table, unit-testing, fixtures]
root-cause: "n/a — this is a reusable pattern, not a bug fix"
severity: "P3"
---

# Synthetic `piplog` construction for unit testing `log_report` builders

## Problem

`log_report()` and its builder functions (`build_type_summary()`,
`build_processing_summary()`, `build_aux_changes()`, etc.) all take a
`piplog` object as input. A real `piplog` is produced by running the full
`pd_process_data()` pipeline, which requires DLW credentials and network
access. This makes unit testing hard: tests cannot depend on external
resources, and coverage would be impossible in CI.

## Solution

Two lightweight helper functions defined in the test file create minimal
`piplog` objects with full column schema and controlled logmeta payloads.

### `make_entry()` — single log row

```r
make_entry <- function(event, message, logmeta) {
  data.table::data.table(
    time    = Sys.time(),
    package = "",
    fun     = "test_fun",
    event   = event,
    message = message,
    args    = list(list()),
    output  = list(NULL),
    trace   = list(NULL),
    logmeta = list(logmeta)      # logmeta is a list, stored as list-column
  )
}
```

### `make_piplog()` — combine rows into a `piplog`

```r
make_piplog <- function(...) {
  entries <- list(...)
  dt <- data.table::rbindlist(entries, fill = TRUE)
  data.table::setattr(dt, "class", c("piplog", class(dt)))
  dt
}
```

`fill = TRUE` is required because logmeta lists may differ in structure
across entries. `setattr` attaches the `piplog` class in-place without
copying, preserving data.table semantics.

### Usage pattern

```r
# Test that build_processing_summary() renders correctly
test_that("build_processing_summary renders counts", {
  log <- make_piplog(
    make_entry(
      "info",
      "Processing complete.",
      list(
        info      = "process_summary_inf",
        n_total   = 10L,
        n_success = 8L,
        n_failed  = 2L,
        surveys_success = paste0("SVY_", seq_len(8))
      )
    )
  )
  dt  <- parse_log_meta(log)
  out <- build_processing_summary(dt)

  expect_true(any(grepl("Processing Summary", out)))
  expect_true(any(grepl("10", out)))   # n_total
  expect_true(any(grepl("8", out)))    # n_success
  expect_true(any(grepl("2", out)))    # n_failed
})
```

### Testing absence (section omitted when entry missing)

```r
test_that("build_processing_summary returns empty when no entry", {
  log <- make_piplog(
    make_entry("error", "Bad things", list(error = "gd_type_miss"))
  )
  dt  <- parse_log_meta(log)
  out <- build_processing_summary(dt)
  expect_length(out, 0L)
})
```

## Prevention

- Always define `make_piplog()` / `make_entry()` helpers at the top of any
  test file that exercises `log_report` builders. Keep them co-located with
  the tests — do not move them to a shared `helper-*.R` unless multiple test
  files need them.
- Match the logmeta list structure exactly to what the production code writes.
  If the production code changes a key name, the helper calls in tests will
  surface the mismatch immediately.
- Use `fill = TRUE` in `rbindlist()` when combining entries with heterogeneous
  logmeta shapes — this is normal and expected.
- When testing builder functions directly, always call `parse_log_meta(log)`
  first; builders accept a parsed `data.table`, not the raw `piplog`.

## Related

- [2026-04-07-internal-logmeta-types-polluting-type-summary.md](../bugs/2026-04-07-internal-logmeta-types-polluting-type-summary.md) — uses these helpers for regression tests
- [2026-04-06-documenting-internal-pipeline-functions.md](../testing-patterns/2026-04-06-documenting-internal-pipeline-functions.md)
