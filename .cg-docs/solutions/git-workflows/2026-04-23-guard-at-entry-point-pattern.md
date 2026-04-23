---
date: 2026-04-23
title: "Guard-at-entry-point pattern for release validation in pipdata"
category: "git-workflows"
language: "R"
tags: [get_wrk_release, guard-pattern, call-hierarchy, pipfun, refactoring, entry-point]
root-cause: "get_wrk_release() was called redundantly in delegate functions that are always invoked from a parent that already guards, because the assigned wrk_release variable is never used — the call is purely an assertion"
severity: "P3"
---

# Guard-at-entry-point pattern for `pipfun::get_wrk_release()`

## Problem

`pipfun::get_wrk_release()` appeared in 13 call sites across 8 files in
pipdata. Functions called from `pipdata_dlw_process()` re-called the
guard even though their parent had already asserted the release was set.
This produced:
- Redundant assertions adding noise to call stacks
- Every exported function needing to mock `get_wrk_release()` in tests
  (see related: mocking-external-package-calls)
- An unclear picture of the intended call hierarchy

## Root Cause

`pipfun::get_wrk_release()` does two things:
1. **Asserts** a working release is configured (aborts if not)
2. **Assigns** `wrk_release` into the caller's frame as a named list

In pipdata, **no function uses the assigned `wrk_release` variable**
after calling `get_wrk_release()`. Every call is purely an assertion.
Because all exported functions were defensive, the calls propagated into
delegates even when redundant.

## Solution

Apply the **guard-at-entry-point** pattern:

- Keep `get_wrk_release()` only in functions that users are likely to
  call directly (top-level wrappers and standalone utilities)
- Remove it from delegate functions that are always called from a guarded
  parent
- Document the expectation with `@note` in delegate roxygen blocks

**Call hierarchy for the DLW pipeline:**

```
pipdata_dlw_process()       ← GUARD (keep) — also calls setup_working_release()
  ├─ pipdata_get_gmd()      ← REMOVE (always called from guarded parent)
  │    └─ dlw_gmd_new()     ← REMOVE (nested delegate)
  └─ pipdata_validate_gmd() ← REMOVE (always called from guarded parent)
```

**Standalone functions (keep guard):**
`dlw_gmd_match()`, `dlw_gmd_unvalidated()`, `dlw_get_dta()`,
`dlw_dta_to_qs()`, `dlw_scan_and_validate()`, `pipdata_copy_dlw_meta()`,
`pipdata_validation_report()`

**Delegate functions (remove guard):**
`pipdata_get_gmd()`, `pipdata_validate_gmd()`, `dlw_gmd_new()`,
`dlw_gmd_list()` (save section)

**`@note` for delegate docs:**
```r
#' @note This function expects a working release to be configured via
#'   [pipfun::setup_working_release()]. When called from
#'   [pipdata_dlw_process()], the release is already set. When called
#'   standalone, ensure `setup_working_release()` has been invoked first.
```

## Prevention

When adding a new exported function to pipdata:
- If it is a **top-level entry point or standalone utility**: add `pipfun::get_wrk_release()` at the top
- If it is a **delegate** always called from a guarded parent: omit the guard and add a `@note` documenting this
- Never use `wrk_release$...` without calling `get_wrk_release()` first — the assignment is a side effect, not a return value

## Related

- [`.cg-docs/solutions/testing-patterns/2026-04-16-mocking-external-package-calls-at-function-startup.md`](../testing-patterns/2026-04-16-mocking-external-package-calls-at-function-startup.md) — the testing consequence of having `get_wrk_release()` in every function
- [`.cg-docs/plans/2026-04-23-remove-redundant-get-wrk-release.md`](../../plans/2026-04-23-remove-redundant-get-wrk-release.md) — implementation plan for the cleanup
