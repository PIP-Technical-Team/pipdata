---
date: 2026-04-29
title: "Replace date_valid filter with stamp-based vintage tracking"
status: decided
scope: "Lightweight"
chosen-approach: "Approach 1: Remove filter, add release version columns to master inventory"
tags: [vintage, stamp, release-inventory, master-inventory, cleanup]
---

# Replace date_valid filter with stamp-based vintage tracking

## Context

The `date_valid` / `date_validated` mechanism in `valid_dlw_load()` (commented out)
and `update_pip_inventory()` (active) was intended to create "vintages" within a
release — only including surveys validated before a cutoff timestamp. In practice,
the intent is simpler: all validated surveys should be cleaned, the release inventory
is a point-in-time snapshot, and stamp's version history provides the vintage trail.

The question was whether to keep the filtering mechanism or rely on stamp for
vintage retrieval, and whether to track release membership in the master inventory.

## Requirements

1. All validated surveys should be cleaned (master inventory) — no date filtering.
2. The release inventory is a snapshot; vintages are stamp versions.
3. The master inventory should record which release inventory version each survey
   first appeared in (`first_release_version_id`) and was last confirmed in
   (`latest_release_version_id`), using stamp version IDs.
4. Vintage retrieval: `pip_read("pip_release_inventory", version = -N, alias = "pip_inv")`
   or by specific version ID.

## Approaches Considered

### Approach 1: Lightweight — add columns, remove filter, use st_save() return value (Chosen)

- Remove `date_validated < date_valid` filter from `update_pip_inventory()`
- Remove commented `date_valid` parameter from `valid_dlw_load()`
- Remove `date_valid` parameter from `update_pip_inventory()` signature
- Capture version_id from `pipload::pip_write()` return value
- Add `first_release_version_id` and `latest_release_version_id` columns

Pros: Minimal changes, stamp does the heavy lifting, no new dependencies
Cons: None significant — pip_write() already returns version_id
Effort: Small

### Approach 2: Explicit vintage registry — separate artifact

Store release membership in a separate artifact rather than master inventory columns.
Pros: Lean master inventory, full event history
Cons: Extra artifact, more complex queries
Effort: Medium

### Approach 3: Human-readable vintage labels

Use labels like "20260401_v03" mapped to stamp version IDs.
Pros: Readable
Cons: Extra indirection, stamp IDs are already timestamped
Effort: Medium

## Decision

Approach 1 selected. `pipload::pip_write()` already returns `stamp::st_save()` output
which includes `version_id`. Implementation is straightforward.

## Next Steps

1. Remove `date_valid` parameter and filter from `update_pip_inventory()`
2. Remove commented `date_valid` lines from `valid_dlw_load()`
3. Capture `version_id` from the `pip_write()` call for the release inventory
4. Add `first_release_version_id` / `latest_release_version_id` columns to master inventory logic
5. Update roxygen documentation
6. Update/add tests
