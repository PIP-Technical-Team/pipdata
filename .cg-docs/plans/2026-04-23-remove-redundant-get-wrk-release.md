---
date: 2026-04-23
title: "Remove redundant get_wrk_release() guards from DLW call chains"
status: active
scope: "Lightweight"
brainstorm: ~
language: R
estimated-effort: small
tags: [dlw, refactoring, get_wrk_release, guard-pattern]
---

# Plan: Remove redundant get_wrk_release() guards from DLW call chains

## Objective

Eliminate redundant `pipfun::get_wrk_release()` calls in DLW functions
that are always invoked from a parent that already guards. Keep the guard
only at genuine entry points — functions a user is likely to call
directly. This reduces noise, simplifies mocking in tests, and clarifies
the intended call hierarchy.

## Context

`pipfun::get_wrk_release()` does two things:
1. **Asserts** a working release is configured (aborts if not).
2. **Assigns** `wrk_release` into the caller's frame.

In pipdata, **no function uses the assigned `wrk_release` variable** —
every call is purely a guard. All 13 call sites across 8 files treat it
as an assertion, then use `pipfun::get_pip_folders()` or other functions
that internally access the release from `.pipenv`.

### Call hierarchy (DLW pipeline)

```
pipdata_dlw_process()          ← entry point, calls setup_working_release()
  ├─ get_wrk_release()         ← GUARD (keep)
  ├─ pipdata_get_gmd()
  │    ├─ get_wrk_release()    ← REDUNDANT
  │    └─ dlw_gmd_new()
  │         └─ get_wrk_release()  ← REDUNDANT (nested)
  └─ pipdata_validate_gmd()
       └─ get_wrk_release()    ← REDUNDANT
```

All functions are exported, so any can be called standalone. The plan
keeps guards at functions likely to be standalone entry points and
removes them from functions that are primarily delegates.

## Requirements

| ID  | Requirement                                              | Source |
|-----|----------------------------------------------------------|--------|
| R1  | Keep `get_wrk_release()` at top-level and standalone entry points | user |
| R2  | Remove from delegate functions called by a guarded parent | user |
| R3  | No behaviour change when functions are called standalone (user gets a clear error if no release is set) | implicit |
| R4  | Update tests that mock `get_wrk_release` to reflect removals | implicit |

## Implementation Steps

### 1. Classify functions: keep vs. remove

Based on the call graph, usage patterns, and whether a function is
typically called standalone:

**KEEP guard (7 functions, 9 call sites):**

| Function | File | Reason |
|----------|------|--------|
| `pipdata_dlw_process()` | `pipdata_dlw_process.R` | Top-level wrapper, calls `setup_working_release()` first |
| `dlw_gmd_match()` | `pipdata_dlw_compare.R` | Standalone utility |
| `dlw_gmd_unvalidated()` | `pipdata_dlw_compare.R` | Standalone utility |
| `dlw_get_dta()` | `dlw_get_dta.R` | Standalone utility |
| `dlw_dta_to_qs()` | `dlw_dta_to_qs.R` | Standalone utility |
| `dlw_scan_and_validate()` | `dlw_scan_and_validate.R` | Standalone utility |
| `pipdata_copy_dlw_meta()` (2 fns) | `pipdata_copy_dlw_meta.R` | Standalone utilities |
| `pipdata_validation_report()` (2 fns) | `pipdata_validation_report.R` | Standalone utilities |

**REMOVE guard (4 functions, 4 call sites):**

| Function | File | Line | Called from |
|----------|------|------|------------|
| `pipdata_get_gmd()` | `pipdata_get_gmd.R` | 48 | `pipdata_dlw_process()` |
| `pipdata_validate_gmd()` | `pipdata_validate_gmd.R` | 34 | `pipdata_dlw_process()` |
| `dlw_gmd_new()` | `pipdata_dlw_compare.R` | 72 | `pipdata_get_gmd()` |
| `dlw_gmd_list()` (save section) | `pipdata_dlw_compare.R` | 270 | Called after data is already fetched |

- **Requirements**: R1, R2
- **Files**: `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`, `R/pipdata_dlw_compare.R`
- **Details**: Delete the `pipfun::get_wrk_release()` line from each function.
  For `dlw_gmd_new()` and `dlw_gmd_list()`, verify that
  `pipfun::get_pip_folders()` (called immediately after) will itself
  abort if no release is set, preserving R3.
- **Test Scenarios**:
  - ✅ `pipdata_dlw_process()` still works end-to-end (guard at top)
  - ✅ `pipdata_get_gmd()` called standalone without release → aborts
    (via `get_pip_folders()` or downstream)
  - 🛑 `dlw_gmd_new()` called standalone without release → verify it
    still fails with a clear error
- **Acceptance criteria**: `devtools::check()` passes; no test regressions.

### 2. Add `@note` to delegate functions documenting the guard expectation

- **Requirements**: R2, R3
- **Files**: Same 4 files as Step 1
- **Details**: Add a roxygen `@note` to each delegate function:
  ```r
  #' @note This function expects a working release to be configured via
  #'   [pipfun::setup_working_release()]. When called from
  #'   [pipdata_dlw_process()], the release is already set. When called
  #'   standalone, ensure `setup_working_release()` has been invoked first.
  ```
- **Acceptance criteria**: `devtools::document()` runs cleanly; help
  pages show the note.

### 3. Update tests that mock `get_wrk_release`

- **Requirements**: R4
- **Files**: `tests/testthat/test-dlw_dta_to_qs.R` and any others
  mocking `get_wrk_release` for the 4 affected functions
- **Details**: If tests for the 4 delegate functions mock
  `get_wrk_release`, the mock is no longer needed since the call was
  removed. Remove or simplify those mock bindings.
- **Test Scenarios**:
  - ✅ Tests for `dlw_dta_to_qs()` still mock (guard kept)
  - ✅ Tests for delegate functions no longer need the mock
- **Acceptance criteria**: `devtools::test()` passes with zero failures.

## Testing Strategy

- Run `devtools::test()` after each step to catch regressions.
- Run `devtools::check()` after all steps to verify no R CMD check issues.
- Manual verification: call `dlw_gmd_new()` without a release configured
  and confirm a clear error is raised (from `get_pip_folders()` or
  downstream).

## Documentation Checklist

- [x] Function documentation for guarded functions (already has roxygen)
- [ ] Add `@note` to delegate functions (Step 2)
- [ ] Inline comment at `pipdata_dlw_process()` explaining the guard pattern

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Standalone call to `dlw_gmd_new()` silently proceeds without release | Verify `get_pip_folders()` aborts; if not, add explicit guard back |
| Future caller bypasses the guarded entry point | `@note` documents the expectation; `get_pip_folders()` provides a safety net |

## Out of Scope

- Changing `pipfun::get_wrk_release()` itself.
- Removing guards from non-DLW pipeline functions (`pd_process_data`, etc.).
- Refactoring the guard into a lighter-weight assertion function.
