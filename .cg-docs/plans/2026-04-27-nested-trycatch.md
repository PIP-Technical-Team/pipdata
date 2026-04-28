---
date: 2026-04-27
title: "Audit nested tryCatch patterns"
status: completed
completed-date: 2026-04-28
scope: "Light"
language: R
estimated-effort: small
tags: [error-handling, code-quality, refactoring]
roadmap-id: nested-tryCatch
---

# Plan: Nested tryCatch Audit

## Objective

Review all tryCatch blocks in `R/` for nesting, necessity, and clarity.
Consolidate where appropriate.

## What Is Already Done

**Full scan completed** (2026-04-27):
- 20+ tryCatch instances found and catalogued (22 after `dlw_get_dta.R` correction below)
- Confirmed **zero true nested tryCatch** (tryCatch inside tryCatch)
- Identified one conditional pattern worth reviewing

## Complete Inventory

| File | Lines | Pattern | Status |
|------|-------|---------|--------|
| `valid_dlw_load.R` | 258 | Independent — load with NULL fallback | ✅ Acceptable |
| `update_pip_inventory.R` | 167, 213 | Independent — two separate loads | ✅ Acceptable |
| `save_pip.R` | 34 | Independent — stamp write guard | ✅ Acceptable |
| `pipdata_validate_gmd.R` | 69, 146, 321 | Independent — three separate loads | ✅ Acceptable |
| `pipdata_get_gmd.R` | 96 | Independent — download loop body | ✅ Acceptable |
| `pipdata_dlw_compare.R` | 21, 83, 192, 219 | Independent — four separate loads | ✅ Acceptable |
| `pd_process_data.R` | 137 | Independent — main pipeline guard | ✅ Acceptable |
| `pd_deflation.R` | 106, 200, 447 | Independent — two S3 methods + one utility | ⚠️ Duplicate pattern (see below) |
| `dlw_get_dta.R` | 71, 92 | Independent — one load + one loop download | ✅ Acceptable (missed in first scan) |
| `dlw_scan_and_validate.R` | 61, 180 | **Conditional** — `if (log)` guards tryCatch | ⚠️ Asymmetric (see below) |

## Open Issues

### Issue 1: Conditional tryCatch in `dlw_scan_and_validate.R:54–77`

**Pattern**: `if (log) { tryCatch(...) } else { direct_call() }`

When `log = FALSE`, the file read has no error guard — a corrupt inventory
file would crash the process with no message. When `log = TRUE` it returns
`NULL` gracefully.

**Decision needed**: Is asymmetric error handling intentional?

- **Option A** (recommended): Always guard the read; log only if `log = TRUE`:
  ```r
  old_inv <- tryCatch(
    qs::qread(pip_raw_inventory_path),
    error = function(e) {
      if (log) pipfun::log_add("error", "Failed to load inventory file",
                               name = "pipdata_log",
                               logmeta = list(error = e$message))
      NULL
    }
  )
  ```
- **Option B**: Leave as-is — treat `log = FALSE` as a "crash-fast" debug mode.

### Issue 2: Duplicate tryCatch structure in `pd_deflation.R`

`deflation.pipmd()` (line 106) and `deflation.pipgd()` (line 200) have
near-identical tryCatch blocks and error handlers. The only difference is
the method class. This is not a nesting problem but a DRY issue.

**Decision needed**: Extract shared `safe_deflation()` helper, or leave as
acceptable S3 method duplication?

- Deferred — this overlaps with the `subfunctions` roadmap item.

## Implementation Steps

### Step 1: Resolve Issue 1 (conditional tryCatch)

- Get a decision on Option A vs B above
- If Option A: refactor `dlw_scan_and_validate.R:54–77`
- Run `devtools::check()` to verify

### Step 2: Mark Issue 2 as deferred

- Add a comment in `pd_deflation.R` referencing the `subfunctions` roadmap item
- No code change required now

### Step 3: Close the audit

- Update plan status to `completed`
- Mark `roadmap.json` `nested-tryCatch` as `"status": "done"`

## Acceptance Criteria

- [ ] Decision made on Issue 1 (conditional tryCatch) — either fixed or explicitly accepted
- [ ] Issue 2 formally deferred to `subfunctions` roadmap item
- [ ] `devtools::check()` passes after any changes
- [ ] `roadmap.json` `nested-tryCatch` updated to `"status": "done"`

## Out of Scope

- Changing the overall error handling strategy
- Refactoring S3 method duplication (belongs in `subfunctions` roadmap item)
- `dlw_scan_and_validate.R` Phase 2 dplyr migration (separate roadmap item)
