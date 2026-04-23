---
date: 2026-04-16
title: "Mocking external package calls made at function startup so tests reach the actual logic"
category: "testing-patterns"
language: "R"
tags: [testthat, mocking, local_mocked_bindings, pipfun, unit-testing, startup-guard]
root-cause: "Function calls an external dependency (e.g. pipfun::get_wrk_release()) before any business logic, aborting all tests before they can exercise the code under test"
severity: "P2"
---

# Mocking external package calls made at function startup

## Problem

`dlw_dta_to_qs()` calls `pipfun::get_wrk_release()` as its very first line to
assert that a working PIP release is configured. In the test environment no real
release exists, so every test aborted immediately with:

```
Error in `dlw_dta_to_qs(...)`: No working release set. ...
```

All 4 tests in `test-dlw_dta_to_qs.R` failed — not because of a logic bug, but
because the tests never reached the code they were meant to exercise.

## Root Cause

Guard calls like `pipfun::get_wrk_release()` are correct in production but poison
unit tests. In `devtools::test()` there is no active PIP release, so the guard
always fires and the function aborts before any file logic runs.

An additional symptom: the original tests were written assuming `skip_err` and
`log_err` parameters existed on `dlw_dta_to_qs()`, but those parameters were
removed during a refactor. Tests referencing removed API also fail immediately.

## Solution

Use `testthat::local_mocked_bindings()` to replace the external call with a no-op
for the duration of each test:

```r
test_that("Normal flow with a valid .dta file", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir  <- withr::local_tempdir()

  # Mock away the startup guard — no real release needed in tests
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(NULL),
    .package = "pipfun"
  )

  df_test <- data.frame(x = 1:5, y = letters[1:5])
  haven::write_dta(df_test, file.path(local_dta_dir, "test_ok.dta"))

  dlw_dta_to_qs(dlw_raw_folder = local_dta_dir, dlw_qs_folder = local_qs_dir)

  qs_files <- list.files(local_qs_dir, pattern = "\\.qs$", full.names = TRUE)
  expect_length(qs_files, 1)
})
```

Key points:
- `local_mocked_bindings()` is scoped to the `test_that()` block — it restores
  automatically when the block exits.
- `.package = "pipfun"` is required when mocking a function from another package;
  omit it only when mocking something in the package under test.
- The mock must match the **exported** function name, not its internal alias.

### When tests reference a removed/changed API

Rewrite tests to match the current function signature. If the old behaviour
(e.g. `skip_err = TRUE`) no longer exists, test the actual current behaviour:

```r
# Old (broken): tested a removed parameter
expect_message(
  dlw_dta_to_qs(..., skip_err = TRUE),
  regexp = "Skipping file 'corrupt.dta' due to read error"
)

# New (correct): tests the current hardcoded-skip behaviour
expect_message(
  dlw_dta_to_qs(...),          # no skip_err parameter
  regexp = "corrupt\\.dta"     # message still mentions the file
)
```

## Prevention

- When adding a startup guard to a function (environment checks, release checks,
  auth checks), also add a `local_mocked_bindings()` fixture in the test file so
  tests continue to work.
- After any function signature change (adding/removing parameters), update tests
  immediately — stale tests with wrong parameter names cause confusing failures.
- Prefer `withr::local_*` and `testthat::local_mocked_bindings()` over `mockery`
  or manual `on.exit()` teardown; they compose cleanly with `test_that()` scoping.

## Related

- [testthat: Mocking](https://testthat.r-lib.org/reference/local_mocked_bindings.html)
- [withr: local functions](https://withr.r-lib.org/)
- See also `2026-04-16-r-cmd-check-no-visible-binding-datatable-nse.md`
- See also `../git-workflows/2026-04-23-guard-at-entry-point-pattern.md` — the upstream design decision that reduces how many functions need mocking in the first place
