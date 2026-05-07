---
review-date: 2026-05-04
plan: 2026-05-04-integrate-deflation
depth: standard
reviewers: [cg-code-quality, cg-testing, cg-documentation, cg-version-control, cg-reproducibility, cg-performance, cg-architecture, cg-data-quality]
status: open
findings:
  P1.1: fixed
  P1.2: open
  P2.1: fixed
  P2.2: skipped
  P2.3: fixed
  P3.1: fixed
  P3.2: fixed
---

# Review: integrate-deflation implementation

**Scope**: `R/pd_deflation.R` (rewrite, 816 lines), `tests/testthat/test-pd-deflation.R`
(new, 28 tests), `roadmap.json`, `.cg-docs/plans/2026-05-04-integrate-deflation.md`

---

## P1 — CRITICAL (must fix before merge)

### P1.1 — `%||%` in `cpi_ppp_years` (not in namespace)

**Agent**: cg-code-quality  
**File**: `R/pd_deflation.R` line 575  
**Status**: ✅ FIXED in this review session

`cpi_ppp_years` used `%||%` which is not exported from pipdata's namespace.
The identical bug caused a test failure earlier in `pd_deflation()`. The fix
replaces `%||%` with an explicit `if (is.null(...))` guard, consistent with
the rest of the file.

```r
# Before (broken — could not find function "%||%")
ppp_versions <- attr(dt, "ppp_versions") %||% attr(ppp, "ppp_versions")

# After (fixed)
ppp_versions <- attr(dt, "ppp_versions")
if (is.null(ppp_versions)) {
  ppp_versions <- attr(ppp, "ppp_versions")
}
```

---

## P2 — IMPORTANT (should fix)

### P2.1 — `deflate_wlf` silently relies on `data.table` reference semantics

**Agent**: cg-code-quality  
**File**: `R/pd_deflation.R` — `deflate_wlf()` and `get_welfare_ppp()`

`deflate_wlf()` calls `purrr::map()` over `base_years`, invoking
`get_welfare_ppp()` for each. The mutation happens via `:=` on `dt_c` by
reference inside `get_welfare_ppp`, but:

1. The `dt_w` list (return values of `purrr::map`) is never used.
2. The behavior depends on the undocumented side effect that `dt_c` is the
   same R object as `dt_wlcu` inside `get_welfare_ppp` (copy-of-reference).
3. `get_welfare_ppp` also does `dt_wlcu <- dt_wlcu[, ..welf_vars]` at the end
   — this local re-assignment has no effect outside the function, making the
   return value a red herring.

The code works today because `data.table::copy(dt)` is passed by reference and
`:=` mutates it in place, but this is fragile and hard to understand.

**Recommendation**: remove `dt_w <-` assignment in `deflate_wlf()` and add an
inline comment explaining the reference semantics dependency. Alternatively,
refactor `get_welfare_ppp()` to return the full modified `dt_wlcu` and merge
the columns back explicitly.

```r
# Current (confusing)
dt_w <- purrr::map(.x = base_years, .f = get_welfare_ppp, dt = dt_c)
return(dt_c)

# Minimal fix (clarifies intent)
# get_welfare_ppp mutates dt_c by reference via :=
purrr::walk(base_years, get_welfare_ppp, dt = dt_c)
return(dt_c)
```

### P2.2 — Test file is untracked (`??`) in git

**Agent**: cg-version-control  
**File**: `tests/testthat/test-pd-deflation.R`  
**Status**: ⏭ SKIPPED — already resolved

`git status` showed this file as `??` (untracked) at review time. However, a
check of `git log` confirmed the file had already been committed in the prior
session (commit `d4778f2`: "Defaltion task with tests and doc"). No action
needed.

### P2.3 — `.load_deflation_aux` uses `utils::head(row, 1L)` to select "most recent" without ordering

**Agent**: cg-data-quality  
**File**: `R/pd_deflation.R` — `.load_deflation_aux()`  
**Status**: ✅ FIXED in fix-triage session

When `version = NULL`, the code picked `utils::head(row, 1L)` from the
inventory — "the first row" — with no explicit sort. Additionally, the
`version_id_data` and `version_id_metadata` column names used throughout this
function did not exist in the real inventory (`old_pip_inv`). The real
columns are `content_hash_data`, `content_hash_metadata`, and
`created_at_metadata`.

All three column references were corrected and an explicit descending sort on
`created_at_metadata` was added before `head()`. Matching mock objects in
`tests/testthat/test-pd-deflation.R` were updated to use the real column names.

```r
# Fixed: sort on real timestamp column, use real hash column for version lookup
row <- row[order(row$created_at_metadata, decreasing = TRUE), ]
row <- utils::head(row, 1L)
# ...
meta_version <- row$content_hash_metadata[[1L]]
```

---

## P3 — MINOR (nice to have)

### P3.1 — `cpi_ppp_years` keeps dead `log_err`/`skip_err` parameters

**Agent**: cg-documentation  
**File**: `R/pd_deflation.R` — `cpi_ppp_years()`

The function signature has `log_err = TRUE, skip_err = TRUE` documented as
"kept for backward compatibility" but neither parameter is used in the body.
This creates noise for callers.

**Recommendation**: add a `lifecycle::deprecate_warn()` call or, if no
external code depends on these args, remove them in a follow-up PR with a
`NEWS.md` entry.

### P3.2 — `get_welfare_ppp` is not `@noRd` and lacks roxygen `@keywords`

**Agent**: cg-documentation  
**File**: `R/pd_deflation.R` — `get_welfare_ppp()`

The function has a roxygen block but no `@export`, `@keywords internal`, or
`@noRd` tag. `devtools::document()` will not generate a man page for it (no
`@export`), but the omission is inconsistent with the rest of the internal
helpers in this file (all marked `@noRd` or `@keywords internal`).

**Recommendation**: add `@noRd` for full consistency.

---

## Summary

| Priority | Count | Status |
|----------|-------|--------|
| P1       | 1     | ✅ all fixed |
| P2       | 3     | ✅ P2.1 fixed, P2.2 skipped (already committed), P2.3 fixed |
| P3       | 2     | 🔲 optional |

**Blocking**: None.

---

## Applied fixes in this session

- **P1.1**: Replaced `%||%` with `if (is.null(...))` in `cpi_ppp_years()` (`R/pd_deflation.R`).
- **P2.1**: Replaced `purrr::map` with `purrr::walk` in `deflate_wlf()` to make the side-effect intent explicit.
- **P2.2**: Skipped — `tests/testthat/test-pd-deflation.R` was already committed in the prior session (commit `d4778f2`).
- **P2.3**: Fixed wrong inventory column names (`version_id_data`/`version_id_metadata` → `content_hash_data`/`content_hash_metadata`); added explicit sort on `created_at_metadata` before `head()`. Mock objects in tests updated to match.
