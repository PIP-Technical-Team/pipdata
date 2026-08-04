---
date: 2026-05-19
title: "Guard before join: dropping reentrant columns in load-then-recompute functions"
category: "data-quality"
language: "R"
tags: [joyn, collapse, ftransform, duplicate-column, reentrant, load-then-recompute, data.table, re-run]
root-cause: "Functions that load existing state from disk and later re-derive columns leave those columns in the merged table; the subsequent join that produces the same column as a non-key output creates a duplicate that collapse::ftransform_core() rejects"
severity: "P2"
---

> **UPDATED 2026-08-04**: `update_pip_inventory()` / `R/update_pip_inventory.R`
> have been replaced by `build_pip_inventory()` / `R/build_pip_inventory.R`.
> That rewrite goes further than a rename: `build_pip_inventory()` no longer
> computes `reporting_level` at all (it's dropped as a legacy column; see
> `.cg-docs/solutions/bugs/2026-05-19-reporting-level-duplicate-column-rerun.md`
> for the update note). The "Concrete instance" below is therefore now
> illustrative-only of the general reentrant-column pattern, not a live code
> path — the general guard-before-join principle remains valid for any new
> load-then-recompute function.

# Guard before join: dropping reentrant columns in load-then-recompute functions

## Problem

A pipeline function that:
1. loads an existing artifact from disk (`old_x <- load_...()`)
2. row-binds it with freshly computed data (`new_x <- rowbind(fresh, old_x)`)
3. computes column `Y` and joins a lookup table that also contains column `Y`

…works on the first run (disk artifact doesn't exist yet → `old_x` is NULL)
but errors on every subsequent run with:

```
Error in `ftransform_core()`:
! All columns of .data have to be uniquely named
```

Traceback traces through `joyn::left_join` → `collapse::ftransform`.

### Concrete instance

`update_pip_inventory()` in `R/update_pip_inventory.R`:

- `old_pip_inv` loaded from master inventory already has `reporting_level`.
- After `rowbind(pip_inv, old_pip_inv)`, `new_pip_inv` carries `reporting_level`.
- `joyn::left_join(new_pip_inv, pfw_rl_unq, ...)` puts `reporting_level` (from
  `pfw_rl_unq`) as a non-key output column → duplicate → crash.

See `.cg-docs/solutions/bugs/2026-05-19-reporting-level-duplicate-column-rerun.md`
for the full diagnosis and fix.

## Root Cause

`joyn` (backed by `collapse`) does not silently drop or coalesce duplicate
non-key columns — it inserts both into the result data.table, then
`ftransform_core()` sees non-unique names and hard-errors.

This only surfaces on re-runs because the first run has no disk artifact to
load, so the column is absent before the join.

## Solution

Drop the column from the target table immediately before the join that
re-derives it:

```r
# Drop any existing <col> before joining the lookup that also produces <col>.
# On a re-run the loaded artifact already carries it; retaining it causes
# collapse::ftransform_core() to error with "non-unique column names".
if ("<col>" %in% names(target_dt)) {
  target_dt[, <col> := NULL]
}
target_dt <- joyn::left_join(
  target_dt,
  lookup_dt,     # contains <col> as a non-key output
  by = <keys>,
  ...
)
```

The column is always re-derived from authoritative source data, so dropping
the cached value is safe and correct — it is never carried forward untouched.

### Applied fix in `update_pip_inventory()`

```r
if ("reporting_level" %in% names(new_pip_inv)) {
  new_pip_inv[, reporting_level := NULL]
}
new_pip_inv <- joyn::left_join(
  new_pip_inv,
  pfw_rl_unq,
  by = c("country_code", "surveyid_year", "survey_acronym"),
  relationship = "many-to-one",
  reportvar = FALSE,
  verbose = FALSE
)
```

## Prevention

### Pattern to follow

Any function that follows the **load-then-recompute** structure:

```r
old   <- tryCatch(load_something(), error = function(e) NULL)
new   <- rowbind(fresh_data, old, fill = TRUE)
# ... later ...
new   <- joyn::left_join(new, lookup_with_col_X, by = keys)
```

must guard against `col_X` already being present in `new`:

```r
if ("col_X" %in% names(new)) new[, col_X := NULL]
new <- joyn::left_join(new, lookup_with_col_X, by = keys)
```

### Anti-pattern to avoid

Assuming join inputs are "clean" (free of the output columns):

```r
# WRONG — silently correct on first run, crashes on re-run
new_pip_inv <- joyn::left_join(new_pip_inv, pfw_rl_unq, by = ...)
```

### Detection heuristic

When writing or reviewing a function that:
- calls `load_*()` or `pip_read()` inside a `tryCatch`
- row-binds the result with fresh data
- then does a `joyn::left_join` that adds non-key columns

…ask: "Could any of those non-key columns already exist in the row-bound
result?" If yes, add the guard.

## Related

- `.cg-docs/solutions/bugs/2026-05-19-reporting-level-duplicate-column-rerun.md` — concrete bug instance with full reproduction test
- `.cg-docs/solutions/bugs/2026-04-29-conditional-column-init-inconsistent-schema.md` — related: conditional column initialisation leading to schema drift across runs
