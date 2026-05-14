---
date: 2026-04-27
title: "Testing functions that read from the unified package environment (.pipdataenv)"
category: "testing-patterns"
language: "R"
tags: [testing, environment, injection, .pipdataenv, pd_env_set, withr, on.exit, fixtures]
root-cause: "Functions reading from the package-level environment (.pipdataenv) cannot be tested without injecting test state, because the environment is empty in a fresh test session"
severity: "P2"
---

# Testing Functions That Read from the Package Environment

## Problem

Several pipdata functions (e.g., `get_data_status()`, `get_validation_report()`)
read state from the unified package-level environment `.pipdataenv`. These
functions cannot be unit tested without either:
1. Running the full pipeline (integration test — slow, fragile), or
2. Directly injecting state via accessor helpers (unit test — fast, isolated)

## Root Cause

`.pipdataenv` is the persistence mechanism for pipeline state (see
[environment-issues/2026-04-30-unified-package-environment-accessor-pattern.md](../environment-issues/2026-04-30-unified-package-environment-accessor-pattern.md)).
Functions that read from it are not pure — they depend on ambient state that
doesn't exist in a fresh test session.

## Solution

Use a **scope-limited helper** that injects state via `pd_env_set()` and cleans
up via `on.exit(pd_env_rm(...))`. **Do not** use direct `$`-assignment or raw
`assign()` even in tests — go through the accessors.

```r
# In tests/testthat/test-pipdata_validation_report.R (or helper-env.R)

with_validation_report <- function(dt, code) {
  pd_env_set("validation_report", dt)
  on.exit(pd_env_rm("validation_report"), add = TRUE)
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
  if (!is.null(pd_env_get("validation_report"))) {
    pd_env_rm("validation_report")
  }
  expect_error(get_data_status(), class = "rlang_error")
})
```

### Alternative: `withr::defer`

For test cleanup in `testthat` 3rd edition, `withr::defer()` is equivalent:

```r
test_that("...", {
  pd_env_set("validation_report", make_test_vr())
  withr::defer(pd_env_rm("validation_report"))
  result <- get_data_status()
  expect_s3_class(result, "data.table")
})
```

## Prevention

- When adding a new function that reads from `.pipdataenv`, write a
  corresponding `with_<state>()` helper at the same time.
- Keep helpers in `tests/testthat/helper-env.R` so they're available to all test files.
- Never leave test state in `.pipdataenv` between tests — always use
  `on.exit(pd_env_rm(...))` or `withr::defer(pd_env_rm(...))`.
- Use `pd_env_set()` / `pd_env_rm()` in tests — never raw `assign()`/`$`/`rm()`.

## Related

- [environment-issues/2026-04-30-unified-package-environment-accessor-pattern.md](../environment-issues/2026-04-30-unified-package-environment-accessor-pattern.md)
  — the full accessor-helper architecture: `pd_env_set`, `pd_env_get`,
  `pd_env_rm`, `pd_env_reset`, `pd_env_append`
- [testing-patterns/2026-04-16-mocking-external-package-calls-at-function-startup.md](./2026-04-16-mocking-external-package-calls-at-function-startup.md)
  — related pattern for mocking external calls
- [testing-patterns/2026-04-07-synthetic-piplog-testing-pattern.md](./2026-04-07-synthetic-piplog-testing-pattern.md)
  — similar injection pattern for piplog state
