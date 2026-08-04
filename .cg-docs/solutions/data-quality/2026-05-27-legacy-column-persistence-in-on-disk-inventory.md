---
date: 2026-05-27
title: "Legacy columns persist in on-disk master inventory across rebuilds"
category: "data-quality"
language: "R"
tags: [inventory, master-inventory, legacy-cleanup, reporting_level, data-migration]
root-cause: "build_pip_inventory() upserts from old master without stripping stale columns, so any column written by old update_pip_inventory() that the new assembler never produces is carried forward indefinitely."
severity: "P2"
---

# Legacy Columns Persist in On-Disk Master Inventory

## Problem

After replacing `update_pip_inventory()` with `build_pip_inventory()`,
`reporting_level` (and other stale columns) continued to appear in the
loaded master inventory even though the new assembler never produces it.
The column surfaced in `load_pip_master_inventory()` results and confused
downstream consumers that expected a clean schema.

## Root Cause

`build_pip_inventory()` loads the old master inventory and upserts fresh
rows into it (`collapse::rowbind(new_versions, old_retained, fill = TRUE)`).
Any column present in `old_retained` propagates into `run_inv` via `fill = TRUE`.
The new assembler never produces those columns, but they are not dropped
either — so they survive every run until every row in the master has been
reprocessed.

For `reporting_level` specifically: old `update_pip_inventory()` derived and
persisted it. The new approach computes it on-the-fly at load time via
`pip_inv_enrich()`, so it must not be on disk.

## Solution

Add all stale columns to a `legacy_cols` drop-list applied to `old_inv`
in Step 1 of `build_pip_inventory()`, **before** the upsert:

```r
legacy_cols <- c(
  "reporting_level",         # now computed on-the-fly via pip_inv_enrich()
  "code_hash_data",          # never populated by update_pip_inventory()
  "file_hash_data",
  "code_label_data",
  "code_hash_metadata",
  "file_hash_metadata",
  "code_label_metadata",
  "format_data",
  "format_metadata"
)
drop_cols <- intersect(legacy_cols, names(old_inv))
if (length(drop_cols) > 0L) {
  old_inv[, (drop_cols) := NULL]
}
```

This migrates any on-disk master to the new schema on the next run,
without requiring a full rebuild.

## Prevention

- **Schema migration belongs in Step 1** of any assembler that upserts from
  an old master. Add a `legacy_cols` block whenever a column is removed from
  the inventory schema.
- **Write a test**: inject an old master with the stale column and assert
  it is absent in the result. The test caught a stale `load_all()` cache
  masking the real fix — always call `devtools::load_all()` before running
  tests after editing production code.
- **Document on-disk schema changes** in `DESCRIPTION` or a changelog note
  so future developers know migration runs automatically.

## Related

- `.cg-docs/solutions/bugs/2026-05-27-fs-bytes-class-collapse-rowbind-abort.md` — same Step 1
  cleanup block also strips non-standard `fs_bytes` class from size columns
- `.cg-docs/solutions/data-quality/2026-05-19-guard-before-join-reentrant-column.md` — related
  pattern: cleaning stale columns before a join to avoid `.x`/`.y` suffix collisions
