---
date: 2026-05-19
title: "data.table scoping: column name shadows function argument in DT[i, ] filter"
category: "bugs"
language: "R"
tags: [data.table, scoping, filter, pip_id, load_deflation_aux]
root-cause: "data.table evaluates i-expressions in the table's own scope first. When a function argument shares a name with a column (pip_id), the column wins and the filter becomes a tautology (all rows match)."
severity: "P1"
test-written: "no"
fix-confirmed: "yes"
---

# data.table scoping: column name shadows function argument

## Problem

`.load_deflation_aux(pip_id = "BOL_2022_EH_INC_ALL")` returned the wrong metadata
version. The function appeared to use `4dd1d6d669a5894d` (the GPWG survey's
`version_id_metadata`) instead of `dd051b004b8ef091` (the ALL survey's).

Diagnosis showed that `inv[inv$pip_id == pip_id, ]` returned ALL 4146 rows instead
of 1. After sorting by `created_at_metadata` descending and taking `head(1)`, the
row with the most recent timestamp — which happened to be GPWG — was selected.

## Root Cause

Inside `DT[i, ]`, data.table evaluates the `i` expression in the table's own scope.
Bare names resolve to **columns** before looking up in the enclosing environment.

```r
.load_deflation_aux <- function(pip_id, version = NULL) {
  inv <- pipload::load_pip_master_inventory()
  row <- inv[inv$pip_id == pip_id, ]  # BUG: pip_id on RHS = column, not argument
  # Equivalent to: inv[inv$pip_id == inv$pip_id, ] → all rows TRUE
```

The `$` operator on the LHS correctly uses the external `inv` object, but the bare
`pip_id` on the RHS resolves to the column `inv$pip_id` because data.table's `[`
method evaluates both sides of `==` in the table's scope.

**This only triggers when the column and function argument share the same name.**

## Solution

Rename the local binding to something that cannot conflict with a column name:

```r
.load_deflation_aux <- function(pip_id, version = NULL) {
  inv <- pipload::load_pip_master_inventory()
  target_id <- pip_id          # <-- breaks the name collision
  row <- inv[inv$pip_id == target_id, ]
```

Similarly for `version`:
```r
  if (!is.null(version)) {
    target_ver <- version
    row <- row[row$content_hash_data == target_ver, ]
```

## Prevention

**Never use a bare function argument name as the RHS of `==` inside `DT[i, ]`
when the argument name matches a column.**

Two safe patterns:

```r
# Pattern 1: assign to a local with a distinct name
target_id <- pip_id
dt[dt$col == target_id, ]

# Pattern 2: force parent-frame lookup with double-dot (data.table idiom)
dt[col == ..pip_id, ]
# (requires data.table >= 1.10.2; ..x reads from the parent frame)
```

The `..` prefix approach is more idiomatic data.table but is easy to forget.
Renaming the local binding is more explicit and works in all versions.

**Code review checklist item**: Any `DT[DT$col == <arg>, ]` where `<arg>` is
also a column name in `DT` is a latent scoping bug. Prefer `..arg` or rename.

## Related

- `R/pd_deflation.R` — `.load_deflation_aux()` where fix was applied
- data.table NSE scoping rules: `vignette("datatable-reference-semantics")`
- `.cg-docs/solutions/build-errors/2026-04-16-r-cmd-check-no-visible-binding-datatable-nse.md`
