---
date: 2026-05-05
title: "stamp version_id ≠ content_hash: resolving the correct version for pip_read"
category: "bugs"
language: "R"
tags: [stamp, pip_read, version_id, content_hash, metadata, pipload, versioning]
root-cause: "The PIP master inventory stores content_hash_metadata (stamp's content_hash field), not stamp's internal version_id. Passing content_hash_metadata directly to pip_read() causes a 'version not found' error because pip_read expects version_id."
severity: "P1"
---

# stamp `version_id` ≠ `content_hash`: resolving the correct version for `pip_read`

## Problem

`pd_deflation()` in `R/pd_deflation.R` failed with:

```
✖ Version "b12a64d9c220df0d" not found for BOL_2022_EH_INC_ALL.qs2
```

(The exact hash varies per survey.) The function was passing
`row$content_hash_metadata` directly as the `version` argument to `pip_read()`.

## Root Cause

Stamp maintains **two separate identifiers** for each stored version:

| Field | Description | Example |
|-------|-------------|---------|
| `version_id` | Stamp's internal opaque ID — what `pip_read(version=)` expects | `2a7390b149cf8e5b` |
| `content_hash` | SHA hash of the artifact content — what the master inventory stores in `content_hash_metadata` | `b12a64d9c220df0d` |

The PIP master inventory column `content_hash_metadata` maps to stamp's
`content_hash`, **not** to `version_id`. These are always different strings.
Passing `content_hash` to `pip_read(version=)` silently looks for a version
with that ID, finds none, and aborts.

A secondary trap: calling `stamp::st_versions(raw_unc_path)` resolves
against a **different stamp registry** than `pip_read()` uses internally
(which goes through the alias/root system). The `version_id` obtained from
the raw path may then fail when pip_read uses its own root resolution.

## Solution

Use the `pip_read(..., version = "available")` pattern to list versions in
**the same stamp context** that the subsequent load will use, then match
on `content_hash`:

```r
# Step 1 — list available versions via pip_read's own resolver
avail_meta <- pipload::pip_read(
  id      = pip_id,
  alias   = "pip_meta",
  version = "available"
)
# avail_meta has columns: version_id, artifact_id, content_hash, ...

# Step 2 — match content_hash from inventory to find the correct version_id
idx <- which(avail_meta$content_hash == meta_content_hash)
meta_version <- avail_meta$version_id[[idx[[1L]]]]

# Step 3 — load with the resolved version_id
meta <- pipload::pip_read(id = pip_id, alias = "pip_meta", version = meta_version)
```

This ensures `version_id` and the subsequent `pip_read` load use the same
stamp registry.

### Stale inventory fallback

When `content_hash_metadata` in the inventory no longer matches any available
version (artifact was overwritten by a newer run), `idx` will be empty. The
correct fallback is the **most recent** available version (`vintage == 0`):

```r
if (length(idx) == 0L) {
  cli::cli_warn(
    paste0(
      "Could not find a stamp version matching content hash ",
      "{.val {meta_content_hash}} for {.val {pip_id}}. ",
      "The artifact may have been replaced by a newer run. ",
      "Falling back to the most recent available version."
    )
  )
  # vintage 0 = most recent (pip_read assigns vintage = (row - 1) * -1)
  meta_version <- avail_meta$version_id[[which(avail_meta$vintage == 0)[[1L]]]]
}
```

## stamp versioning model (summary)

```
pip_read("BOL_2022_EH_INC_ALL", alias="pip_meta", version="available")
→ returns data.table with:
    version_id    content_hash
    2a7390b…      b12a64d9…     ← content_hash matches inventory content_hash_metadata
```

- `version_id` is opaque and is what stamp/`pip_read` accepts as a `version=` arg
- `content_hash` is the artifact hash stored in the master inventory
- `vintage` is assigned by `pip_read` as `(row_index - 1) * -1` so `vintage == 0` = most recent

## Anti-patterns

```r
# ❌ Wrong: pass content_hash directly — always fails
pip_read(id, alias = "pip_meta", version = row$content_hash_metadata)

# ❌ Wrong: use st_versions(raw_unc_path) — different registry than pip_read
st_vers <- stamp::st_versions(row$path_metadata)

# ✅ Correct: resolve via pip_read's own "available" mechanism
avail <- pip_read(id, alias = "pip_meta", version = "available")
ver   <- avail$version_id[avail$content_hash == row$content_hash_metadata]
pip_read(id, alias = "pip_meta", version = ver)
```

## Prevention

- **Never pass `content_hash_*` columns from the master inventory directly to
  `pip_read(version=)`** — always resolve to `version_id` first via
  `pip_read(..., version = "available")`.
- Always handle the stale-inventory case (empty `idx`) with a warning + fallback
  to `vintage == 0`, not a hard abort — the inventory may legitimately lag
  artifact updates during active pipeline runs.
- When writing tests that mock `pip_read`, make the mock aware of
  `version = "available"` and return a table with `version_id` / `content_hash`
  columns, otherwise tests pass while production code fails.

## Related

- `.cg-docs/solutions/data-quality/2026-05-04-master-inventory-real-column-names.md`
  — sister finding: `content_hash_metadata` column name (not `version_id_metadata`)
