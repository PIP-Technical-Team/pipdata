---
date: 2026-04-28
title: "Replace explicit loops with vectorized/lapply patterns"
status: active
scope: "Lightweight"
brainstorm: null
language: R
estimated-effort: small
tags: [performance, vectorization, code-quality, validation]
---

# Plan: Replace Explicit Loops with Vectorized/Apply Patterns

## Objective

Audit all `for`/`while` loops in pipdata R source files and replace those
that are amenable to `lapply()`, `vapply()`, `Map()`, or data.table
vectorized operations. Loops that are inherently sequential (side-effect
iteration, metaprogramming, condition traversal) are documented and retained.

## Context

The codebase has **20 `for` loops** and **2 `while` loops** across 4 R files.
The bulk (16 loops) live in `pipdata_dlw_validation.R` where the same
validate-per-variable pattern repeats across 7 validation functions. The
remaining loops are in `utils.R` (2 attribute-setting loops),
`pipdata_get_gmd.R` (1 row-wise API-call loop), and `pd_deflation.R`
(1 formals-copy loop).

### Loop Census

| File | Loops | Pattern | Action |
|------|-------|---------|--------|
| `pipdata_dlw_validation.R` | 16 | Validate each variable name in a list via `data.validator` chain | **Replace**: extract helper, use `lapply()` |
| `utils.R` | 2 | Set attributes on a data.table in a loop | **Replace**: use `lapply()` or `for` is idiomatic here (side-effect on ref) — **document only** |
| `pipdata_get_gmd.R` | 1 | Row-wise iteration with side effects (API calls + tryCatch + logging) | **Keep**: inherently sequential, side-effect loop |
| `pd_deflation.R` | 1 | Copy formals into local scope via `get()`/`assign()` | **Keep**: metaprogramming pattern, not vectorizable |
| `utils.R` (`find_condition`) | 1 while | Traverse condition parent chain | **Keep**: linked-list traversal |
| `pd_process_data.R` | 1 while | Traverse condition parent chain | **Keep**: linked-list traversal |

## Requirements

| ID  | Requirement | Source |
|-----|-------------|--------|
| R1  | Extract shared validation-loop pattern into a reusable helper function | roadmap/loop-to-apply |
| R2  | Replace 16 validation loops with `lapply()` calls to the helper | roadmap/loop-to-apply |
| R3  | Document loops intentionally kept as-is | audit |
| R4  | All existing tests pass after refactoring | qa |

## Implementation Steps

### 1. Create validation helper functions

- **Requirements**: R1
- **Files**: `R/pipdata_dlw_validation.R`
- **Details**: The 16 loops fall into 3 recurring patterns:
  1. **Numeric variable validation**: `is_numeric` → `is_greaterthanzero` → `not_na` → `num_row_NAs` (with optional `check_urban` for "urban" variable)
  2. **Weight/welfare validation**: Same as numeric but with `error_append` instead of `warning_append` for the NAs-within-threshold check
  3. **Character variable validation**: `is_character` → `not_na`

  Extract 3 internal helpers:
  ```r
  validate_numeric_vars <- function(dlw_data, svy_id, var_names, na_threshold, report) {
    lapply(var_names, \(var) {
      labelled::var_label(dlw_data[[var]]) <- NULL
      validate(dlw_data, name = svy_id) |>
        is_numeric(var) |>
        is_greaterthanzero(var) |>
        validate_cols(...) |>
        validate_rows(...) |>
        add_results(report)
      if (var == "urban") {
        validate(dlw_data, name = svy_id) |>
          check_urban("urban") |>
          add_results(report)
      }
    })
    invisible(NULL)
  }
  ```
  Similar for `validate_wgt_welfare_vars()` and `validate_char_vars()`.

- **Tests**: Existing validation tests must pass unchanged — this is a pure refactor.
- **Acceptance criteria**: Zero `for` loops remain in `pipdata_dlw_validation.R` except where a loop body has branching logic that cannot be cleanly extracted.

### 2. Replace loops in each validation function

- **Requirements**: R2, R4
- **Files**: `R/pipdata_dlw_validation.R`
- **Details**: In each of the 7 `dlw_validation_*()` functions, replace the loop blocks with calls to the new helpers. Example:
  ```r
  # Before:
  for (i in seq_along(num_var_list)) { ... }
  
  # After:
  validate_numeric_vars(dlw_data, svy_id, num_var_list, na_threshold, report)
  ```
- **Acceptance criteria**: `devtools::check()` passes. Test suite passes with 0 failures.

### 3. Document retained loops

- **Requirements**: R3
- **Files**: `R/pipdata_get_gmd.R`, `R/pd_deflation.R`, `R/utils.R`, `R/pd_process_data.R`
- **Details**: Add a brief inline comment above each retained loop explaining why it is kept:
  ```r
  # Loop retained: row-wise API call with side effects; not vectorizable.
  for (i in seq_along(1:nrow(inv_gmd))) { ... }
  ```
- **Acceptance criteria**: Every `for`/`while` in pipdata has either been replaced or has a retention comment.

## Testing Strategy

- **Pure refactor**: No new logic introduced. Existing validation tests are the primary safety net.
- Run `devtools::test()` after each step.
- Run `devtools::check()` after completing all steps.

## Documentation Checklist

- [ ] New helper functions have `@noRd` roxygen headers with `@param`/`@return`
- [ ] Retained loops have inline comments explaining retention
- [ ] No README changes needed (internal refactor)

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| `data.validator::add_results()` relies on `report` environment side effect — lapply may break reference semantics | Verify `report` is an environment (not a value); `add_results()` mutates in place. Test immediately. |
| Some validation functions have per-variable branching (e.g., `check_urban` only for "urban") | Handle via `if` inside the `lapply` body — straightforward |

## Out of Scope

- Refactoring the `data.validator` validation chain API itself
- Replacing the row-wise loop in `pipdata_get_gmd.R` (sequential API calls)
- Replacing the `pd_deflation.R` formals loop (metaprogramming)
- Adding new validation tests (separate roadmap item)
