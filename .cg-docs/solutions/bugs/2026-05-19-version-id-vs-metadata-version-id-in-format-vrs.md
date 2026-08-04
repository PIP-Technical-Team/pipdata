---
date: 2026-05-19
title: "Store version_id (not just content_hash) in master inventory for reliable deflation loading"
category: "bugs"
language: "R"
tags: [deflation, stamp, inventory, version_id, format_vrs, load_deflation_aux, content_hash]
root-cause: "format_vrs() extracted ventry$metadata dict into the master inventory, which already contained a version_id key (from stamp's serialised metadata). However that version_id belonged to a prior artifact write, not the current run—making it chronically stale. Fix: capture ventry$version_id from the top-level pip_write() return instead."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

> **UPDATED 2026-08-04**: `format_vrs()` no longer exists as a separate named
> function — its logic (capturing `ventry$version_id` from the top-level
> `pip_write()` return) has been inlined directly into
> `R/build_pip_inventory.R` (see the "Column provenance" section of its
> roxygen header: `version_id_data`/`version_id_metadata` from the `"pip"`/
> `"pip_meta"` catalogs). The version_id-capture principle described below is
> unchanged, just relocated.

# Store version_id from pip_write() return in master inventory

## Problem

`pd_deflation()` always emitted a stale-version warning and fell back to loading
the latest `pip_meta` artifact, even immediately after running `pd_process_data()`.

Inspection showed the master inventory had `version_id_metadata = "4dd1d6d669a5894d"`
for `BOL_2022_EH_INC_ALL`, but `pip_read()` could not find that version —
it was actually the version_id of `BOL_2022_EH_INC_GPWG`.

## Root Cause

`format_vrs()` built inventory rows from `ventry$metadata` (the stamp metadata
dict saved alongside the artifact). This dict contains a `version_id` key —
but that key is stamp's internal metadata-file version-id, **not** the artifact's
own version-id from the current `st_save()` call.

```r
# OLD — extracts metadata dict, which has a stale/wrong version_id
vlist <- ventry$metadata
vlist$pip_id <- pip_name
# vlist$version_id now = metadata-record's own version, not the artifact's
```

`pip_write()` (via `stamp::st_save()`) returns:

```
list(
  version_id = "dd051b004b8ef091",   # <-- the correct, current artifact version
  metadata   = list(version_id = "...", content_hash = "...", ...),
  path       = "..."
)
```

The metadata sub-dict's `version_id` points to the internal metadata file,
not the data artifact. The top-level `ventry$version_id` is what should be
stored and used for `pip_read(version = ...)`.

## Solution

Capture `ventry$version_id` explicitly in `format_vrs()`:

```r
ventry <- x[[version]][[pip_name]]
vlist <- NULL
if (!is.null(ventry$metadata) && length(ventry$metadata) > 0) {
  vlist <- ventry$metadata
}
vlist$pip_id <- pip_name
# NEW — capture the data artifact's version_id from the top-level return
if (!is.null(ventry$version_id)) {
  vlist$version_id <- ventry$version_id
}
```

After the suffixed `left_join(..., suffix = c("_data", "_metadata"))`, this
becomes `version_id_data` and `version_id_metadata` in the master inventory.

`.load_deflation_aux()` then uses it directly:

```r
meta_version <- if ("version_id_metadata" %in% names(row) &&
    !is.na(row$version_id_metadata[[1L]])) {
  row$version_id_metadata[[1L]]
} else {
  NULL  # falls back to latest
}
meta <- tryCatch(
  pipload::pip_read(id = pip_id, alias = "pip_meta", version = meta_version),
  error = function(e) NULL
)
if (is.null(meta)) {
  # stale version_id — load latest instead
  warning(...)
  meta <- pipload::pip_read(id = pip_id, alias = "pip_meta", version = NULL)
}
```

The hash-based `pip_read(version = "available")` fallback path was removed
entirely — it is not needed when `version_id` is stored correctly.

## Prevention

- **`pip_write()` returns `list(version_id, metadata, path)`**. Always read
  `ventry$version_id` (top level) for the artifact version to store in
  inventory. Do **not** rely on `ventry$metadata$version_id` — that is the
  metadata file's own version, not the artifact's.
- When in doubt, confirm with:
  ```r
  result <- pipload::pip_write(x, id = "test", alias = "...")
  names(result)          # should show: version_id, path, metadata, ...
  result$version_id      # the artifact version to store
  ```
- When loading by version, always use `pip_read(version = <version_id>)`.
  `version_id` and `content_hash` are distinct stamp concepts and are not
  interchangeable.

## Related

- `R/update_pip_inventory.R` — `format_vrs()` where fix was applied
- `R/pd_deflation.R` — `.load_deflation_aux()` updated to use `version_id_metadata`
- `.cg-docs/solutions/bugs/2026-05-05-stamp-version-id-vs-content-hash.md`
- `.cg-docs/solutions/bugs/2026-05-05-stale-content-hash-in-load-deflation-aux.md`
