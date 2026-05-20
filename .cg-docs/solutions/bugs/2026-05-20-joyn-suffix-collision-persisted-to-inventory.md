---
date: 2026-05-20
title: "joyn suffix columns persisted to master inventory cause duplicate reporting_level on re-run"
category: "bugs"
language: "R"
tags: [joyn, data.table, inventory, reporting_level, column-naming, suffix, left_join]
root-cause: "joyn::left_join() adds .x/.y suffixes when both tables share a column name and the column is not in the join key. A past execution persisted these suffixed columns to the master inventory on disk. Subsequent runs loaded the artifact via rowbind, producing three reporting_level columns."
severity: "P2"
---

# joyn suffix columns persisted to master inventory cause duplicate reporting_level on re-run

## Problem

After running `pd_process_data()`, `names(new_pip_inv)` contained:
```
"reporting_level.x"  "reporting_level.y"  "reporting_level"
```
`reporting_level.x` and `reporting_level.y` were all `NA`; only `reporting_level` had correct values.

## Root Cause

At some point a `joyn::left_join()` was called when both the left table (`new_pip_inv`) and the right table already carried a `reporting_level` column, but it was not listed in `by`. joyn suffixed the two copies as `.x` and `.y`. This result was saved to the master inventory via `pip_write()`.

On every subsequent run, `update_pip_inventory()`:
1. Loads `old_pip_inv` from disk — it carries `reporting_level.x`, `reporting_level.y`, `reporting_level`.
2. `rowbind`s `old_pip_inv` into `new_pip_inv` (with `fill = TRUE`) — all three columns survive.
3. The cleanup guard only dropped the **exact** name `"reporting_level"`, leaving `.x`/`.y` artifacts.
4. The PFW join adds a fresh `reporting_level`, producing all three columns again.

The artifact is self-perpetuating: each write re-saves the suffixed columns.

## Solution

Replace the exact-match drop with a pattern-based drop that removes **all** columns whose name starts with `reporting_level`:

```r
# Before (fragile — leaves .x/.y survivors):
if ("reporting_level" %in% names(new_pip_inv)) {
  new_pip_inv[, reporting_level := NULL]
}

# After (defensive — removes exact + suffixed variants):
drop_rl_cols(new_pip_inv)   # internal helper
```

The `drop_rl_cols()` helper (extracted for testability):

```r
drop_rl_cols <- function(dt) {
  rl_cols <- grep("^reporting_level", names(dt), value = TRUE)
  if (length(rl_cols) > 0L) {
    dt[, (rl_cols) := NULL]
  }
  invisible(dt)
}
```

Extracting to a named helper means both the production code and the tests call the
same function — if the production code regresses, the tests break.

## Prevention

**General rule for any re-run cleanup guard**: when dropping a column before a re-join, use `grep("^<col>", names(dt), value = TRUE)` rather than an exact `%in%` check. joyn (and merge/dplyr joins) can leave suffixed copies if the column was accidentally included in both tables on a prior run.

**Schema validation gate**: consider asserting `length(grep("^reporting_level", names(new_pip_inv))) == 0L` after the drop and before the join, so any regression is a hard error rather than silently cascading.

## Related

- `.cg-docs/solutions/bugs/2026-05-19-datatable-scoping-column-vs-argument.md` — related data.table column-name collision class of bugs
