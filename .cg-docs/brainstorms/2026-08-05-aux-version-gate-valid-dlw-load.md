---
date: 2026-08-05
title: "Gate aux-change detection in valid_dlw_load on aux version change"
status: draft
chosen-approach: "Per-survey aux content_hash columns in master inventory, compared via st_catalog_query"
participants:
  - User
  - Copilot
tags: [valid_dlw_load, valid_aux_load, aux, stamp, content_hash, master-inventory, pipeline]
---

# Gate aux-change detection in `valid_dlw_load()` on aux version change

## Context

`valid_dlw_load()` always calls `valid_aux_load()`, which invokes
`pipaux::compare_aux_releases()` and `pipaux::compare_aux_vintages()`. These
compare the *current* aux data against the *previous release/vintage* in the
aux data's own history — **not** against what the pipeline actually used in
its last run. As a result, every pipeline run detects "changes" relative to
the previous aux release/vintage and re-cleans all affected surveys, even
when the pipeline already cleaned them with that same aux data.

Goal: only run the `compare_aux_*` changes functions when the aux data
version actually differs from the version used in the last pipeline run.

## Requirements

- R1: Record the version (content hash) of each aux measure used when a
  survey was cleaned.
- R2: Before running `compare_aux_*`, compare the current aux content hash
  against the stored last-run hash; only run the changes functions when they
  differ.
- R3: Track all aux measures (`pfw`, `cpi`, `ppp`, `pop`, `gdp`, `pce`).
- R4: Obtain the current aux content hash from `st_catalog_query()` on the
  aux alias (no full aux data load).
- R5: Gate only the aux-change detection path. DLW-new surveys are still
  always detected and processed.
- R6: Store the aux versions per-survey in the PIP master inventory (new
  columns), not in survey metadata attributes.

## Approaches Considered

### Approach 1: Per-survey aux content_hash columns in master inventory (CHOSEN)

Add columns to the master inventory such as `aux_cpi_hash`, `aux_ppp_hash`,
`aux_pfw_hash`, `aux_pop_hash`, `aux_gdp_hash`, `aux_pce_hash`. These are
populated per-survey during processing (captured in `pd_aux_attr()` from the
aux data actually used) and surfaced into the master inventory by
`build_pip_inventory()`.

`valid_dlw_load()` then:
1. Queries the current aux content hashes via `st_catalog_query()` on the
   aux alias.
2. Loads the master inventory and compares each survey's stored aux hashes
   against the current hashes.
3. Only runs `valid_aux_load()` / `compare_aux_*` when a survey's stored aux
   hash differs from the current aux hash.

**Pros**: Precise per-survey tracking (surveys cleaned at different times
with different aux versions are handled correctly). Reuses the existing
master-inventory read path (`load_pip_master_inventory()`). Content-hash
comparison is robust to re-saves with identical content. No per-survey stamp
reads at comparison time (fast).
**Cons**: Adds columns to the master inventory schema (requires a
`legacy_cols` migration note if ever removed). Requires capturing aux hashes
during processing.
**Effort**: Medium.

### Approach 2: Run-level aux-version snapshot artifact

Store a single metadata artifact (e.g. `aux_versions` in `pip_meta`) holding
the aux hashes used in the last run, written at the end of each run.

**Pros**: Single record, simple to read.
**Cons**: Loses per-survey precision; a survey cleaned in an earlier run with
older aux would not be re-cleaned when only that survey's aux changed.
**Effort**: Small.

### Approach 3: Read stored aux versions directly from each survey's pip_meta

**Pros**: No inventory schema change.
**Cons**: Requires per-survey stamp reads at comparison time (slower);
couples `valid_dlw_load()` to metadata internals.
**Effort**: Medium.

## Decision

**Approach 1: Per-survey aux content_hash columns in the master inventory.**

- Aux versions are captured per-survey during processing (in `pd_aux_attr()`),
  from the aux data actually used, and surfaced into the master inventory by
  `build_pip_inventory()`.
- Current aux hashes come from `st_catalog_query()` on the aux alias.
- Comparison is content-hash based.
- Only the aux-change detection path is gated; DLW-new surveys are always
  processed.

## Consequences

- Master inventory schema gains aux hash columns (one per tracked measure).
- `valid_dlw_load()` needs the current aux hashes (via `st_catalog_query`)
  and the stored per-survey hashes (via `load_pip_master_inventory()`).
- The `compare_aux_*` functions are only invoked when a real aux change is
  detected, avoiding unnecessary re-cleaning.
- Existing logmeta entries (`aux_changes_inf`, `aux_no_changes_inf`,
  `aux_changes_no_surveys_inf`, `surveys_to_clean_inf`) remain, but their
  trigger conditions change to be based on the version comparison.

## Next Steps

1. Add aux content-hash capture in `pd_aux_attr()` (per-survey, from aux data
   used).
2. Surface aux hash columns into the master inventory in `build_pip_inventory()`.
3. Add a helper to fetch current aux hashes via `st_catalog_query()`.
4. Rewrite the aux-change detection in `valid_dlw_load()` to compare stored
   vs current hashes before calling `valid_aux_load()`.
5. Update tests for `valid_dlw_load()`, `valid_aux_load()`, `pd_aux_attr()`,
   and `build_pip_inventory()`.
6. Update roxygen `@details` and `compound-gpid.context.md` logmeta notes.
