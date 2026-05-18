---
date: 2026-05-18
title: "Switch Arrow partitions to deflated data input with multi-PPP welfare columns"
status: decided
scope: "Standard"
chosen-approach: "Refactor prepare_for_arrow() and full pipeline in-place"
tags: [arrow, deflation, parquet, manifest, ppp, piptm]
---

# Switch Arrow Partitions to Deflated Data Input

## Context

The current Arrow partition creation flow loads raw (un-deflated) survey data via `pipload::load_pip_data()` and writes a single `welfare` column to Parquet. However, the pipeline already produces fully deflated datasets via `pipload::load_pip_deflated_data()` with multiple welfare columns for different PPP base years. The Arrow partitions should use this deflated output directly, and the manifest should carry the PPP metadata so `{piptm}` can select the correct welfare column at query time.

## Requirements

1. **Data source**: `generate_arrow_dataset()` must load data via `pipload::load_pip_deflated_data(id_name = pip_id)` instead of `pipload::load_pip_data()`.
2. **Multiple welfare columns**: Parquet files store all `welfare_*` columns (discovered via `attr(data, "welfare_vars")`) — e.g. `welfare_lcu`, `welfare_ppp_2017_01_02`, `welfare_ppp_2021_01_02`, etc.
3. **Metadata from attributes**: `prepare_for_arrow()` extracts `country_code`, `surveyid_year`, `welfare_type`, `vermast`, `veralt` from dataset attributes rather than a separate `metadata` list. `pip_id` remains a required argument (from inventory).
4. **Manifest enrichment**: Each manifest entry gains `welfare_vars` (character vector of welfare column names) and `ppp_sort` (integer base year used for row sorting).
5. **Schema update in piptm**: `pip_arrow_schema()` becomes a "base schema" (fixed columns without `welfare`). A new helper `pip_welfare_schema(welfare_vars)` generates welfare field definitions dynamically.
6. **PPP-aware loading in piptm**: `load_survey_microdata()` and `load_surveys()` gain a `ppp` argument. They validate against the manifest's `welfare_vars`, select only the matching `welfare_ppp_*` column, and rename it to `welfare` for backward compatibility with compute functions.
7. **`table_maker()` in piptm**: Gains a `ppp` argument, validates against manifest, passes to loading functions. Compute functions remain unchanged (they operate on the generic `welfare` column).
8. **Breakdown dimensions**: Same as today (`gender`, `area`, `educat4`, `educat5`, `educat7`, `age`) — no expansion.
9. **Auxiliary columns**: Dropped (not stored in Parquet) — same as today.

## Approaches Considered

### Approach 1: Refactor `prepare_for_arrow()` in-place (Chosen)

Modify the full chain: `prepare_for_arrow()` accepts deflated data.table directly, `generate_arrow_dataset()` uses `load_pip_deflated_data()`, manifest gains PPP fields, `{piptm}` schema/loading/table_maker updated for PPP selection.

**Pros**: Single code path, no dead code, end-to-end consistency, no interim broken state.
**Cons**: Breaking change to `prepare_for_arrow()` API signature; larger scope touching both packages.
**Effort**: Medium-large (estimated 4–5 days across both packages).

### Approach 2: New `prepare_deflated_for_arrow()` alongside old function

Keep old function intact, add new parallel path.

**Pros**: Non-breaking.
**Cons**: Two parallel code paths, duplication, confusing API, old path becomes dead code anyway.
**Effort**: Medium.

### Approach 3: Adapter shim — convert deflated data to old format

Thin wrapper extracts single welfare + metadata, feeds existing function unchanged.

**Pros**: Minimal changes.
**Cons**: Defeats the purpose; doesn't store multi-PPP welfare columns; hacky.
**Effort**: Small.

## Decision

**Approach 1** — refactor in-place across both `{pipdata}` and `{piptm}`.

Rationale: The old code path will be dead once the switch is made. Parallel functions create maintenance burden. Including `{piptm}` changes avoids an interim state where Parquet files exist that nothing can consume correctly.

## Next Steps

### pipdata changes

1. Refactor `prepare_for_arrow(data, pip_id)` — read metadata from `attributes(data)`, retain all `welfare_*` columns, cast them to double, update validation to handle dynamic welfare columns.
2. Update `inject_metadata_cols()` to pull from attributes.
3. Update `cast_data_cols()` to cast all welfare columns.
4. Update `validate_pre_write()` — dynamic allowed columns list, validate each welfare column.
5. Update `generate_arrow_dataset()` loop — switch to `pipload::load_pip_deflated_data(id_name = pip_id)`.
6. Update `.build_arrow_schema()` — build schema dynamically from actual columns (no longer relying solely on piptm's fixed schema for welfare columns).
7. Update `build_manifest_entry()` — add `welfare_vars` and `ppp_sort` fields.
8. Update `generate_release_manifest()` — discover welfare_vars/ppp_sort from Parquet schema or pass from attributes.

### piptm changes

9. Refactor `pip_arrow_schema()` — remove `welfare` from fixed fields; add `pip_welfare_schema(welfare_vars)` helper.
10. Update `pip_required_cols()` / `pip_allowed_cols()` — make welfare-column-aware (accept welfare_vars argument or return base-only).
11. Update `load_survey_microdata()` — add `ppp` argument; validate against manifest `welfare_vars`; select + rename column to `welfare`.
12. Update `load_surveys()` — add `ppp` argument; same logic.
13. Update `table_maker()` — add `ppp` argument; pass to loading functions.
14. Manifest loading (`.load_manifests()`) — verify `welfare_vars` and `ppp_sort` are parsed correctly from JSON (likely no code changes needed — jsonlite handles new fields automatically).
