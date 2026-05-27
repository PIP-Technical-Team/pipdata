---
date: 2026-05-27
title: "fs_bytes class on old master size columns causes collapse::rowbind abort"
category: "bugs"
type: "bug"
language: "R"
tags: [collapse, rowbind, fs_bytes, class-mismatch, inventory, upsert, schema-drift]
root-cause: "old master persisted size_bytes_* columns with class c('fs_bytes','numeric') via the fs package; new_versions has them as plain numeric from st_catalog_query(); collapse::rowbind aborts on class attribute mismatch even when storage type is compatible"
severity: "P1"
test-written: "yes"
fix-confirmed: "yes"
---

# fs_bytes class on old master size columns causes collapse::rowbind abort

## Symptom

`pd_process_data()` aborts with:

```
Error in `collapse::rowbind()`:
! Class attribute on column 6 of item 2 does not match with column 4 of item 1.
```

Traceback points to `build_pip_inventory.R` Step 9 upsert:
`collapse::rowbind(new_versions, old_retained, fill = TRUE)`.

## Root Cause

The old `update_pip_inventory()` produced its inventory using `fs` I/O
functions that return `fs_bytes` objects. The `size_bytes_data` and
`size_bytes_metadata` columns were therefore persisted to the master
inventory on disk with class `c("fs_bytes", "numeric")`.

`build_pip_inventory()` (the new delta assembler) derives those same columns
from `st_catalog_query()`, which returns plain `numeric`. When Step 9 calls
`collapse::rowbind(new_versions, old_retained, fill = TRUE)`, collapse checks
class attribute equality and aborts — it does not silently coerce, unlike
`data.table::rbindlist`.

`date_validated` (also from the old master, class `POSIXct`) does not cause
the crash because it is absent from `new_versions` — `fill = TRUE` handles
missing columns. Only columns **present in both** with **different classes**
are fatal.

## Reproduction Test

File: `tests/testthat/test-build_pip_inventory.R`

```r
test_that("build_pip_inventory rowbinds correctly when old master has fs_bytes size columns", {
  size_val <- structure(1000, class = c("fs_bytes", "numeric"))
  old_master <- data.table::data.table(
    survey_id = "ECU_2018_ENEMDU",
    pip_id = "ECU_2018_ENEMDU_INC_ALL",
    version_id_data = "old_v",
    size_bytes_data = size_val,         # fs_bytes, not plain numeric
    version_id_metadata = "old_m",
    size_bytes_metadata = size_val,     # fs_bytes, not plain numeric
    welfare_type = "INC",
    ...
  )
  # ... mocks ...
  result <- build_pip_inventory(inv_to_clean, pip_id_map)
  expect_equal(nrow(result), 2L)
})
```

## Fix

Strip `fs_bytes` class from `old_inv` columns **once**, immediately after
loading the old master in Step 1, before any use:

```r
if (!is.null(old_inv)) {
  fs_cols <- names(old_inv)[vapply(
    old_inv, \(x) inherits(x, "fs_bytes"), logical(1L)
  )]
  for (col in fs_cols) {
    data.table::set(old_inv, j = col,
      value = `class<-`(old_inv[[col]], "numeric"))
  }
}
```

This is preferable to a per-rowbind class-alignment loop because:
- It operates once at load time, not at every call site.
- It is schema-agnostic: any `fs_bytes` column is normalised regardless of name.
- `old_inv` is also used in the early-return path (`nrow(pip_id_map) == 0L`);
  normalising at load keeps callers consistent.

File changed: `R/build_pip_inventory.R` (Step 1 block).

## Lessons Learned

`collapse::rowbind` enforces strict class equality on overlapping columns and
aborts rather than coercing. This is stricter than `data.table::rbindlist`
(which coerces silently). Any time an old artifact written by a different
function is rowbound with a freshly assembled table, normalise S3 classes on
the loaded artifact **before** the bind — do not rely on `fill = TRUE` to
handle type drift.

Pattern to follow: after loading any persisted table that may have been
written by legacy code, detect and strip non-standard S3 classes (e.g.
`fs_bytes`, `difftime`, custom classes) for columns that will participate
in a `collapse::rowbind`.

## Related

- [2026-04-29-conditional-column-init-inconsistent-schema.md](./../2026-04-29-conditional-column-init-inconsistent-schema.md) — same
  class of bug: schema drift between runs causes bind failures.
- [2026-05-19-reporting-level-duplicate-column-rerun.md](./../2026-05-19-reporting-level-duplicate-column-rerun.md) — old master
  persisting computed columns that cause conflicts on re-run.
