---
date: 2026-04-27
title: "Testing functions that read from package environments (.pipdata, .pipenv)"
category: "testing-patterns"
language: "R"
tags: [testing, environment, injection, .pipdata, withr, on.exit, fixtures]
root-cause: "Functions reading from package-level environments (.pipdata, .pipenv) cannot be tested without injecting test state into those environments"
severity: "P2"
---

# Testing Functions That Read from Package Environments

## Problem

Several pipdata functions (e.g., `get_data_status()`, `get_validation_report()`)
read state from the package-level environment `.pipdata`. These functions cannot
be unit tested without either:
1. Running the full pipeline (integration test — slow, fragile), or
2. Directly injecting state into `.pipdata` (unit test — fast, isolated)

## Root Cause

Package-level environments (`.pipdata`, `.pipenv`) are the persistence mechanism
for pipeline state. Functions that read from them are not pure — they depend on
ambient state that doesn't exist in a test session.

## Solution

Use a **scope-limited helper** that injects state into the package environment
and cleans up via `on.exit()`:

```r
# In tests/testthat/helper-*.R or at the top of the test file

with_validation_report <- function(dt, code) {
  .pipdata$validation_report <- dt
  on.exit(rm("validation_report", envir = .pipdata), add = TRUE)
  force(code)
}
```

Usage:

```r
test_that("get_data_status() returns a data.table with correct columns", {
  vr <- data.table::data.table(
    table_name   = c("BOL_1990_A", "BOL_1990_A", "CHL_2000_B"),
    type         = c("error", "success", "success"),
    assertion.id = c("a1", "a2", "a3"),
    call         = c("", "", ""),
    error_df     = list(NULL, NULL, NULL)
  )
  with_validation_report(vr, {
    result <- get_data_status()
    expect_s3_class(result, "data.table")
    expect_named(result, c("data_status", "n"))
  })
})
```

### Why `on.exit` + `force`?

- `on.exit(..., add = TRUE)` guarantees cleanup even if the test body throws
  an error — no leaked state between tests.
- `force(code)` evaluates the passed expression in the calling frame, giving
  access to test fixtures defined outside the helper.

### Abort path test

Always test the guard branch (function called without state set):

```r
test_that("get_data_status() aborts when validation_report absent", {
  if (rlang::env_has(.pipdata, "validation_report")) {
    rm("validation_report", envir = .pipdata)
  }
  expect_error(get_data_status(), class = "rlang_error")
})
```

### Alternative: `withr::defer`

For test cleanup in `testthat` 3rd edition, `withr::defer()` is equivalent:

```r
test_that("...", {
  .pipdata$validation_report <- make_test_vr()
  withr::defer(rm("validation_report", envir = .pipdata))
  result <- get_data_status()
  expect_s3_class(result, "data.table")
})
```

## Prevention

- When adding a new function that reads from `.pipdata` or `.pipenv`, write
  a corresponding `with_<state>()` helper at the same time.
- Keep helpers in `tests/testthat/helper-env.R` so they're available to all test files.
- Never leave test state in `.pipdata` between tests — always use `on.exit` or `withr::defer`.

## Related

- [testing-patterns/2026-04-16-mocking-external-package-calls-at-function-startup.md](./2026-04-16-mocking-external-package-calls-at-function-startup.md) — related pattern for mocking external calls
- [testing-patterns/2026-04-07-synthetic-piplog-testing-pattern.md](./2026-04-07-synthetic-piplog-testing-pattern.md) — similar injection pattern for piplog state
