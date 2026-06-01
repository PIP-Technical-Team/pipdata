---
date: 2026-05-27
title: "Inventory assembler: delta/update strategy"
status: decided
scope: "Lightweight"
chosen-approach: "Delta/Update from old master"
tags: [inventory, architecture, build_pip_inventory, simplification]
---

# Inventory Assembler: Delta/Update Strategy

## Context

The `build_pip_inventory()` function (implemented 2026-05-22) used a
"rebuild-from-scratch" strategy: query entire catalogs, validate all pip_ids,
deduplicate all versions, join everything, then recover old surveys at the end.

This produced cascading errors:
- Multi-version catalogs → `joyn` uniqueness failure
- Malformed pip_ids from old runs → `tstrsplit` out-of-bounds
- Empty catalog edge cases → column-missing after joins
- Complex multi-table joins → hard to debug

## Requirements

Same as original plan (R1–R10). No requirement changes — only implementation
strategy changes.

## Approaches Considered

### Approach 1: Rebuild from scratch (current, broken)

Query entire catalogs → validate/filter all rows → deduplicate → join all →
filter to current run → recover old master.

**Pros**: Single source of truth (catalog is authoritative).
**Cons**: Fights catalog complexity (multi-version, malformed entries, duplicate
paths). ~400 lines with defensive checks. Repeatedly broken in testing.
**Effort**: Already spent (broken).

### Approach 2: Delta/Update from old master (CHOSEN)

Start from old master. Query catalog once, filter to only current-run pip_ids.
Extract version info for just those. Upsert into old master.

**Pros**: Small working set (only this run's surveys), avoids all catalog-wide
validation issues, simpler joins (known-good pip_ids), preserves crash-safety
(catalog is still the authoritative source for version info).
**Cons**: None identified — strictly simpler while preserving all guarantees.
**Effort**: Small (rewrite inner logic, ~60 lines core).

## Decision

**Approach 2: Delta/Update**. The catalog still provides crash-safety (we
query it for version facts), but we only look up the pip_ids we just processed
rather than scanning/validating the whole catalog.

Key insight: `pip_id_map` tells us exactly which pip_ids to look up. We don't
need to derive pip_ids from paths and validate them for the entire history.

## Next Steps

1. Rewrite `build_pip_inventory()` core logic with delta strategy
2. Update plan document Phase 2 to reflect new approach
3. Simplify test suite (fewer edge cases now)
