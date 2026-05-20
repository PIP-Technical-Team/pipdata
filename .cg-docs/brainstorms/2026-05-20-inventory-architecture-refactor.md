---
date: 2026-05-20
title: "Refactor inventory architecture: catalog-based assembler"
status: decided
scope: "Deep"
chosen-approach: "Full Catalog-Based Assembler"
tags: [inventory, architecture, stamp, pipload, maintainability, correctness]
---

# Refactor Inventory Architecture

## Context

`update_pip_inventory()` is ~350 lines handling 8+ responsibilities: version
extraction from in-memory `proc_dta`, skipped-survey removal, DLW inventory
merging, PFW reporting_level computation, release inventory creation, release
version tracking, master inventory persistence, and verification logging.

Adding `reporting_level` was cumbersome and generated multiple bugs. The function
depends on `proc_dta` (in-memory return from processing 2500 surveys) which is
lost if the pipeline crashes before reaching inventory update.

## Requirements

1. **Crash-safety**: Version info must be persisted per-survey as a side effect
   of saving, not held in memory until the end of a 2500-survey run.
2. **Maintainability**: The inventory assembler should be simple — read version
   facts, join, write. Adding metadata columns (like reporting_level) should not
   require touching the assembler.
3. **Correctness**: Master inventory = latest version for every pip_id ever
   cleaned. Release inventory = master subset filtered by PFW `inpovcal == 1`.
   Skipped surveys stay in master with their prior version; they only appear in
   logs.
4. **Extensibility (secondary)**: Future metadata columns (CPI, PPP, etc.)
   should be addable without modifying the inventory builder.

## Approaches Considered

### Approach 1: Full Catalog-Based Assembler (CHOSEN)

Replace `update_pip_inventory()` + `format_vrs()` with `build_pip_inventory()`
that reads stamp catalogs via a new `st_catalog_query()` export.

- stamp already records every `st_save()` in a per-alias `catalog.qs2`
- Assembler queries catalog for latest versions, derives pip_id from artifact
  paths, joins with DLW inventory, merges with old master
- Enrichment (reporting_level, etc.) moves to pipload as `pip_inv_enrich()`
- `process_data()` return simplified (only pip_names + success/fail for logging)

**Pros**: Crash-safe, ~80 lines vs ~350, no dual source of truth, adding metadata
fields isolated in pipload.
**Cons**: Cross-package (stamp + pipdata + pipload), catalog path-to-pip_id
derivation assumes stable naming, stamp API coupling (acceptable since we own it).
**Effort**: Medium (2–4 days).

### Approach 2: Pipdata Version Log (No stamp changes)

Accumulate version rows in pipdata env after each `pip_write()`, consume in
simplified assembler at end of run.

**Pros**: No stamp changes, single-package scope.
**Cons**: Still in-memory (crash vulnerability unchanged), dual source of truth.
**Effort**: Small (1–2 days).

### Approach 3: Hybrid — Catalog + Run Manifest

Same as Approach 1, plus a `pip_run_manifest` artifact for run-level auditing.

**Pros**: All benefits of Approach 1 + run-level audit trail.
**Cons**: Manifest may duplicate logging's `process_summary_inf` role.
**Effort**: Same as Approach 1.

## Decision

**Approach 1: Full Catalog-Based Assembler**, with the run manifest deferred as
a roadmap idea to evaluate against existing logging.

Key design decisions:
- `st_catalog_query(alias)` added to stamp — returns latest version per artifact
- `build_pip_inventory(inv_to_clean)` in pipdata — reads catalogs, assembles
  master + release inventories
- `pip_inv_enrich(inv, fields)` in pipload — enriches inventory with metadata
  columns on load
- `format_vrs()` and `drop_rl_cols()` removed from pipdata
- Skipped surveys: not removed from master (catalog has their valid prior version)
- Failed surveys: never reached `pip_write()`, not in catalog, handled by logging

## Next Steps

1. Add `st_catalog_query(alias)` export to stamp package
2. Implement `build_pip_inventory(inv_to_clean)` in pipdata (replaces
   `update_pip_inventory()` + `format_vrs()` + `drop_rl_cols()`)
3. Implement `pip_inv_enrich(inv, fields)` in pipload
4. Simplify `process_data()` return value — remove version tracking, keep
   `pip_names` for logging
5. Update tests across all three packages
6. Archive `update_pip_inventory.R` and `format_vrs()` to old_files/
