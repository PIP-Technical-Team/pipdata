---
date: 2026-05-05
title: "Stale content_hash_metadata in .load_deflation_aux() aborts deflation"
category: "bugs"
type: "bug"
language: "R"
tags: [deflation, stamp, inventory, content-hash, pd-deflation, load-deflation-aux]
root-cause: "format_vrs() drops version_id from pip_write() return value; only content_hash is stored in the master inventory. When a subsequent pd_process_data() run replaces the artifact, the hash in the inventory no longer matches any available stamp version."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

> **UPDATED 2026-08-04**: The graceful-fallback principle described here is
> still correct, but the implementation has moved on. The later fix in
> [2026-05-19-version-id-vs-metadata-version-id-in-format-vrs.md](./2026-05-19-version-id-vs-metadata-version-id-in-format-vrs.md)
> replaced the `content_hash_metadata` + `pip_read(version = "available")`
> hash-matching approach entirely: `.load_deflation_aux()` now resolves the
> version via `version_id_metadata` directly (confirmed in `R/pd_deflation.R`
> lines ~105-134), with the same try-then-fall-back-to-latest behavior. Read
> `content_hash_metadata` below as historical; current code uses
> `version_id_metadata`.

# Stale content_hash_metadata in .load_deflation_aux() aborts deflation

## Symptom

```
Error in `.load_deflation_aux("BOL_2022_EH_INC_ALL")`:
Could not find a stamp version matching content hash "95aebaad1b534585"
for "BOL_2022_EH_INC_ALL". The artifact may have been deleted or the
inventory may be stale.
```

Calling `pd_deflation(pip_id = "BOL_2022_EH_INC_ALL")` aborts even though the
`pip_meta` artifact for that survey exists in stamp.

## Root Cause

`.load_deflation_aux()` resolves the metadata stamp version by:
1. Loading the master inventory to get `content_hash_metadata`
2. Calling `pip_read(..., version = "available")` to list all available `pip_meta` versions
3. Matching `content_hash_metadata` against `avail_meta$content_hash`

This fails when `pd_process_data()` is re-run: the new run saves a fresh
artifact with a new content hash. The master inventory is updated to reflect
the new hash, but if the session loaded an older inventory (or the master
inventory has an older row), the recorded hash is no longer present in stamp.

The underlying design flaw is that `format_vrs()` only saves `ventry$metadata`
(the stamp metadata dict) into the master inventory — which contains
`content_hash` — but silently drops `version_id` from the top level of the
`pip_write()` return value. This forces a hash→version lookup that is fragile.

## Reproduction Test

File: `tests/testthat/test-pd-deflation.R`

```r
test_that(".load_deflation_aux falls back gracefully when master inventory hash is stale", {
  stale_hash   <- "95aebaad1b534585"   # hash recorded in inventory — artifact gone
  current_hash <- "b12a64d9c220df0d"   # hash of currently available pip_meta

  fake_inv <- data.table::data.table(
    pip_id                = "BOL_2022_EH_INC_ALL",
    content_hash_data     = "data_hash_xyz",
    content_hash_metadata = stale_hash,
    created_at_metadata   = "2026-04-01T00:00:00Z"
  )
  ...
  # Should NOT abort — expected to fall back and return valid aux list
  result <- pipdata:::.load_deflation_aux("BOL_2022_EH_INC_ALL")
  expect_named(result, c("cpi", "ppp", "pop"))
})
```

## Fix

Replaced hard `cli_abort` with `cli_warn` + fallback to row 1 (newest available)
when the exact hash is absent. `pip_read(..., "available")` returns rows
newest-first, so `idx <- 1L` selects the most recent version.

```r
# R/pd_deflation.R — .load_deflation_aux()
if (length(idx) == 0L) {
  if (nrow(avail_meta) == 0L) {
    cli::cli_abort(...)  # truly nothing available — hard abort
  }
  cli::cli_warn(
    "Could not find a stamp version matching content hash ...",
    class = c("load_deflation_aux_stale_hash", "pipwrn")
  )
  idx <- 1L  # fall back to newest available
}
```

## Lessons Learned

- **Never use content_hash as a version lookup key** across pd_process_data()
  runs. Hash→version lookups are only safe when the artifact is immutable.
- **The proper fix** (tracked as roadmap item `store-version-id-in-inventory`)
  is to capture `ventry$version_id` in `format_vrs()` and store
  `version_id_data` / `version_id_metadata` in the master inventory, then
  call `pip_read(version = row$version_id_metadata)` directly — no hash lookup.
- The fallback warning (`load_deflation_aux_stale_hash`) is intentionally a
  distinct class so callers and tests can assert or suppress it selectively.

## Related

- `.cg-docs/solutions/data-quality/2026-05-04-master-inventory-real-column-names.md`
- Roadmap: `store-version-id-in-inventory` (Pipeline Alignment Audit milestone)
