---
date: 2026-05-20
title: "Package-imported operators (%||%, %in%, etc.) are not available in test helper functions"
category: "testing-patterns"
language: "R"
tags: [testthat, rlang, %||%, operator, test-helpers, namespace, devtools]
root-cause: "rlang::`%||%` is imported into the package namespace via NAMESPACE/Imports, making it available inside package functions. But test helper functions defined at the top of test files run in the testthat evaluation environment, which does not automatically inherit the package's imported operators."
severity: "P2"
---

> **UPDATED 2026-08-04**: `tests/testthat/test-update_pip_inventory.R` has
> been replaced by `tests/testthat/test-build_pip_inventory.R`. The scoping
> gotcha and fix described below are dialect-neutral and still apply to any
> test helper in that (or any other) test file.

# Package-imported operators (%||%) are not available in test helper functions

## Problem

A test helper in `tests/testthat/test-update_pip_inventory.R` (now
`tests/testthat/test-build_pip_inventory.R`) used `%||%`:

```r
make_ventry <- function(content_hash = "abc123", version_id = NULL) {
  list(
    version_id = version_id %||% paste0("ver_", content_hash),
    ...
  )
}
```

Running `devtools::test_active_file()` produced:

```
Error in `version_id %||% paste0("ver_", content_hash)`:
  could not find function "%||%"
```

## Root Cause

`%||%` is `rlang::`%||%`` imported into the package namespace (listed in `NAMESPACE` as `importFrom(rlang, '%||%')`). Inside **package source** (`R/`) it is always in scope. However, test helper functions defined at the top of test files are evaluated in the testthat runner environment, which is the **package namespace as seen by tests** — not the full import environment. Infix operators imported via `importFrom` are accessible inside exported/internal package functions but are **not** automatically in scope for plain R functions defined in test files.

This also affects:
- `%>%` (magrittr) if imported but not attached
- Any custom infix operator defined in another file and not re-exported

## Solution

Replace package-imported infix operators in test helpers with base R equivalents:

```r
# Instead of:
version_id = version_id %||% paste0("ver_", content_hash)

# Use:
version_id = if (is.null(version_id)) paste0("ver_", content_hash) else version_id
```

Alternatively, qualify explicitly if the operator must be reused:

```r
`%||%` <- rlang::`%||%`   # at top of test file
```

But the base R form is preferred — it has no dependency and is unambiguous.

## Prevention

- **Test helpers are plain R** — treat them as if no package imports are loaded. Use only base R operators and explicitly qualified `pkg::fn()` calls.
- **`devtools::load_all()` vs. `devtools::test_active_file()`**: `load_all()` attaches the package to the search path, which *does* make imported operators available interactively via `:::`. But `test_active_file()` (and CI `R CMD check`) evaluates tests in a stricter environment — the discrepancy hides this class of bug during interactive development.
- If a test helper genuinely needs a non-base operator, add an explicit `library(rlang)` or `requireNamespace` at the top of the helper file, or use a `testthat::local_...()` fixture.

## Related

- `.cg-docs/solutions/testing-patterns/2026-04-16-mocking-external-package-calls-at-function-startup.md` — related namespace scoping in tests
