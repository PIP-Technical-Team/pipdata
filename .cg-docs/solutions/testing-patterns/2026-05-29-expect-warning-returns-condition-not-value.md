---
date: 2026-05-29
title: "expect_warning() returns the condition object, not the expression value"
category: "testing-patterns"
language: "R"
tags: [testthat, expect_warning, return-value, condition, mocking]
root-cause: "testthat 3.2.1.1 changed expect_warning() to return the captured condition, not the expression's return value"
severity: "P2"
---

# `expect_warning()` Returns the Condition Object, Not the Expression Value

## Problem

A test that captures `expect_warning()`'s return value and then inspects
fields on it silently receives `NULL` for all fields:

```r
result <- expect_warning(
  .load_deflation_aux("ABC_2015_TST_INC_D1"),
  class = "load_deflation_aux_stale_version"
)
expect_equal(result$cpi, cpi_vec)  # FAIL — result$cpi is NULL
expect_equal(result$ppp, ppp_vec)  # FAIL — result$ppp is NULL
expect_equal(result$pop, pop_vec)  # FAIL — result$pop is NULL
```

The failures look like:

```
── Failure: .load_deflation_aux falls back to latest when version_id is stale ──
result$cpi (`actual`) not equal to `cpi_vec` (`expected`).
`actual` is NULL
`expected` is a double vector (100)
```

## Root Cause

In **testthat 3.2.1.1**, `expect_warning(expr)` returns the **warning
condition** captured from `expr`, not `expr`'s return value. This changed
from the earlier behaviour where the expression's return value was returned.

`result` above is a `<simpleWarning>` object — a list-like condition with
`$message` and `$call` fields, not the list returned by `.load_deflation_aux()`.
So `result$cpi` resolves to `NULL` (no such field on a condition).

## Solution

Use a block `{ }` inside `expect_warning` that assigns to an outer variable:

```r
result <- NULL
expect_warning(
  { result <- .load_deflation_aux("ABC_2015_TST_INC_D1") },
  class = "load_deflation_aux_stale_version"
)
expect_equal(result$cpi, cpi_vec)  # PASS
expect_equal(result$ppp, ppp_vec)  # PASS
expect_equal(result$pop, pop_vec)  # PASS
```

The `<<-` operator can also be used if inside a nested function, but for
`test_that()` blocks, `<-` is sufficient because `{ }` is evaluated in the
same environment as the surrounding `test_that` block.

Equivalent pattern for `expect_error()` (which has the same return behaviour):

```r
err <- NULL
expect_error(
  { err <- tryCatch(bad_call(), error = identity) },
  ...
)
```

## Prevention

- **Never** write `result <- expect_warning(expr)` and then access fields on
  `result` expecting them to come from `expr`. Only the warning condition is
  returned.
- Initialise the capture variable to `NULL` before the `expect_warning` block
  so a failed assignment is immediately visible (the `NULL` check will fail
  rather than silently skipping).
- The same applies to `expect_message()`, `expect_condition()`, and
  `expect_error()` — all return the captured condition, not the expression value.

## Related

- [2026-04-29-logging-in-trycatch-handlers.md](../testing-patterns/2026-04-29-logging-in-trycatch-handlers.md) — related pattern for handling tryCatch frames in tests
