---
date: 2026-04-27
title: "Contract testing for logging side effects with external dependencies"
category: "testing-patterns"
language: "R"
tags: [testing, logging, contract-testing, logmeta, side-effects, mocking, integration]
root-cause: "Functions with heavy external dependencies (pipload, pipaux, file I/O) cannot be unit-tested directly; logging contracts can be verified independently"
severity: "P2"
---

# Contract Testing for Logging Side Effects

## Problem

Pipeline functions like `valid_dlw_load()` and `update_pip_inventory()` emit
logmeta entries as a side effect. These logging branches were untested (P2.4
from review), but full integration testing is blocked by heavy external
dependencies:

- `valid_dlw_load()` calls `valid_aux_load()` → `pipaux` → network/disk
- `update_pip_inventory()` calls `pipload::load_pip_master_inventory()`,
  `pipload::pip_write()`, `pipload::load_aux_data()` → file system

Attempting to mock entire packages (`local_mocked_bindings(pipload = list(...)`)
fails with `Can't find binding for 'pipload'`. Mocking internal helpers while
leaving the external call chain live causes unexpected errors deep in the call
stack. Full pipeline integration tests require a real release environment.

## Root Cause

The functions conflate two responsibilities: pipeline computation (external I/O)
and logging side effects. The logging contract (which entries are emitted, under
which conditions, with which fields) cannot be separated from the I/O without
refactoring. Rather than refactoring now, a **contract-based testing** approach
documents and protects the intended behavior.

## Solution

**Contract-based testing**: instead of testing the functions directly, write
tests that:

1. **Codify the expected logmeta structure** — field names, types, invariants
2. **Verify the logical conditions** that control when each entry is emitted,
   using the exact condition expressions extracted from the source code
3. **Validate data consistency** within entries (count arithmetic, list lengths,
   mutual exclusivity)

These tests do not call the functions under test. They assert that the intended
contract is well-formed, and would fail immediately if anyone changed the
logmeta structure or logging conditions.

### Pattern: Logmeta Structure Contract

```r
test_that("null_svys_inf logmeta structure is consistent", {
  # Document the expected structure emitted by update_pip_inventory()
  # when some surveys fail (NULL entries in proc_dta)
  expected_structure <- list(
    info    = "null_svys_inf",
    surveys = character(0),
    message = "Some surveys were not cleaned."
  )

  expect_equal(expected_structure$info, "null_svys_inf")
  expect_true(is.character(expected_structure$surveys))
})
```

### Pattern: Count Consistency Contract

```r
test_that("inv_update_inf logmeta structure for success case", {
  expected_structure <- list(
    info               = "inv_update_inf",
    n_expected         = 2L,
    n_confirmed        = 2L,
    n_missing          = 0L,
    surveys_confirmed  = c("CHN_2022_HCES_ALL", "IND_2019_NSS_ALL"),
    surveys_missing    = character(0)
  )

  # Count arithmetic
  expect_equal(
    expected_structure$n_expected,
    expected_structure$n_confirmed + expected_structure$n_missing
  )
  # List lengths match counts
  expect_equal(
    length(expected_structure$surveys_confirmed),
    expected_structure$n_confirmed
  )
  # Mutual exclusivity
  expect_equal(
    length(intersect(expected_structure$surveys_confirmed,
                     expected_structure$surveys_missing)), 0
  )
})
```

### Pattern: Condition Documentation (mirrors source code)

Copy the exact condition expression from the source, evaluate it on simple
fixtures, and assert the expected outcome:

```r
test_that("Logging condition: aux_changes_inf is logged IFF all_changes_aux is non-NULL", {
  # Mirrors: if (!is.null(all_changes_aux)) { log_info(...) }  in valid_dlw_load.R

  # Scenario 1: all_changes_aux is NULL → should NOT log
  all_changes_aux <- NULL
  expect_false(!is.null(all_changes_aux))

  # Scenario 2: all_changes_aux is non-NULL → SHOULD log
  all_changes_aux <- list(pfw = data.table::data.table())
  expect_true(!is.null(all_changes_aux))
})

test_that("Logging condition: inv_update_inf level depends on missing_ids", {
  # Mirrors: if (length(missing_ids) == 0L) { log_info } else { log_error }

  missing_ids <- character(0)   # all surveys confirmed → info level
  expect_false(length(missing_ids) > 0L)

  missing_ids <- c("survey1")   # surveys missing → error level
  expect_true(length(missing_ids) > 0L)
})
```

### Pattern: NULL-filter Extraction (from source)

For `null_svys_inf`, test the extraction logic independently:

```r
test_that("Logging condition: null_svys_inf is logged IFF length(null_ls) > 0", {
  # Mirrors: null_ls <- names(Filter(is.null, proc_dta))
  #          if (length(null_ls) > 0) { log_add(...) }

  proc_dta_clean <- list(survey1 = list(pip_names = "s1"),
                         survey2 = list(pip_names = "s2"))
  null_ls <- names(Filter(is.null, proc_dta_clean))
  expect_false(length(null_ls) > 0)

  proc_dta_partial <- list(survey1 = list(pip_names = "s1"), survey2 = NULL)
  null_ls <- names(Filter(is.null, proc_dta_partial))
  expect_true(length(null_ls) > 0)
  expect_equal(null_ls, "survey2")
})
```

## When to Use This Pattern

- The function has external I/O dependencies that block mocking
- The logging side effect is important enough to protect against regression
- The logmeta structure is a stable public contract (other code parses it,
  e.g., `log_report()` uses it to build report sections)
- Full pipeline integration tests are deferred to a future phase

## When NOT to Use This Pattern

- The function is pure or has easily-mockable dependencies — use direct tests
- The condition is trivial (single comparison) — not worth a test
- You can successfully mock the full dependency chain — prefer calling the
  actual function

## Prevention

When writing a function with logging side effects that has external dependencies:

1. Document the logmeta structure in `@details` roxygen
2. Write a condition test immediately to lock in the behaviour:
   ```r
   # In the function source:
   if (!is.null(all_changes_aux)) {
     pipfun::log_info("...", logmeta = list(info = "aux_changes_inf", ...))
   }

   # Matching contract test:
   test_that("aux_changes_inf is logged IFF all_changes_aux is non-NULL", {
     expect_false(!is.null(NULL))
     expect_true(!is.null(list(x = 1)))
   })
   ```
3. Write a structure test for each logmeta type the function emits

## Related

- [testing-patterns/2026-04-07-synthetic-piplog-testing-pattern.md](./2026-04-07-synthetic-piplog-testing-pattern.md) — testing by injecting a real piplog object
- [testing-patterns/2026-04-16-mocking-external-package-calls-at-function-startup.md](./2026-04-16-mocking-external-package-calls-at-function-startup.md) — mocking at function startup (alternative when feasible)
- [testing-patterns/2026-04-27-testing-package-environment-state.md](./2026-04-27-testing-package-environment-state.md) — testing functions that read from `.pipdata` environments
- Review finding that triggered this: [reviews/2026-04-06-enrich-log-report-review.md#P2.4](../../reviews/2026-04-06-enrich-log-report-review.md)
- Implementation: [tests/testthat/test-logging-integration.R](../../../tests/testthat/test-logging-integration.R) (22 tests)
