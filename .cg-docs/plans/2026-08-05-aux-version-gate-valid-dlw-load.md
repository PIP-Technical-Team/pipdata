---
date: 2026-08-05
title: "Gate aux-change detection in valid_dlw_load on per-survey aux content-hash comparison"
status: active
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-08-05-aux-version-gate-valid-dlw-load.md"
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [valid_dlw_load, valid_aux_load, aux, stamp, content_hash, master-inventory, pipeline, incremental-processing]
phases: 2  # convenience hint -- may be stale; always recount from ## Phase headers

# Plan: Gate aux-change detection in `valid_dlw_load()` on per-survey aux content-hash comparison

## Objective

Make `valid_dlw_load()` only re-clean surveys whose auxiliary data actually
changed since they were last cleaned. Today it always runs
`valid_aux_load()` → `compare_aux_releases()`/`compare_aux_vintages()`, which
compare the current aux data against the previous aux release/vintage — not
against what the pipeline used last run — so every run re-cleans affected
surveys unnecessarily.

## Context

- `valid_dlw_load()` (R/valid_dlw_load.R) currently calls `valid_aux_load()`
  unconditionally, then `filter_aux_inv()` to find affected surveys.
- `valid_aux_load()` (R/valid_aux_load.R) calls
  `pipaux::compare_aux_releases()` and `pipaux::compare_aux_vintages()`, which
  accept `measure` (not a survey set) — pipaux is a separate package and must
  not be modified.
- Aux data is versioned via **stamp**. `st_catalog_query(alias)` returns latest
  version metadata per artifact: `path`, `version_id`, `content_hash`,
  `code_hash`, `size_bytes`, `created_at`.
- Aux data is loaded via `pipload::load_aux_data(measure)`, which resolves the
  aux alias from the working release's `aux_data` folder.
- The master inventory is built by `build_pip_inventory()` (R/build_pip_inventory.R),
  which upserts per-`pip_id` from stamp catalogs and already carries
  `content_hash_dlw` (DLW source hash) per survey. It has a `legacy_cols`
  drop-list applied to `old_inv` in Step 1.
- `inv_to_process()` already compares DLW `content_hash` vs `content_hash_dlw`
  on `survey_id` (the `content-hash-reclean-trigger` feature, done). This plan
  extends the same content-hash pattern to **aux** data.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Capture the content hash of each aux measure used when a survey is cleaned, per-survey | Brainstorm |
| R2 | Surface per-survey aux hash columns into the master inventory | Brainstorm |
| R3 | Fetch current aux content hashes via `st_catalog_query()` (no full aux data load) | Brainstorm |
| R4 | In `valid_dlw_load()`, build a candidate set of previously-cleaned surveys whose stored aux hash differs from the current aux hash (per measure) | Brainstorm |
| R5 | New surveys (no master row) are always processed | Brainstorm |
| R6 | Previously-cleaned surveys with no stored aux hash are treated as changed (re-clean) | Brainstorm |
| R7 | Run `valid_aux_load()`/`compare_aux_*` only for the changed measures, then intersect affected surveys with the candidate set | Brainstorm |
| R8 | Gating applies to every measure in the `aux_measures` param | Brainstorm |
| R9 | DLW-new survey detection path (`inv_to_process()`) unchanged | Brainstorm |
| R10 | Existing logmeta discriminators (`aux_no_changes_inf`, `aux_changes_no_surveys_inf`, `aux_changes_inf`, `surveys_to_clean_inf`) preserved with updated trigger conditions | Brainstorm |

## Phase 1: Capture and persist aux hashes

### 1. Capture per-survey aux content hashes in `pd_aux_attr()`
- **Requirements**: R1
- **Files**: `R/pd_aux_attr.R`
- **Details**: `pd_aux_attr()` receives `aux_list` (named list of aux
  data.tables, one per measure). For each measure in `aux_list`, obtain its
  current content hash via `st_catalog_query()` on the aux alias (resolved the
  same way `load_aux_data()` does — from the working release's `aux_data`
  folder). Attach the hash as a scalar attribute on each survey's attribute
  list, e.g. `attr(ls, "aux_cpi_hash")` / a named element `aux_cpi_hash` in the
  returned attribute list. Do **not** add these as columns to the cleaned
  survey data.table — they belong in the metadata attribute list returned by
  `pd_aux_attr()`.
  - Add a small internal helper (e.g. `get_aux_hash(measure)`) that resolves
    the aux alias and calls `st_catalog_query()` to return the latest
    `content_hash` for the measure's artifact. Reuse the alias-resolution logic
    pattern from `pipload::load_aux_data()` (via `pipfun::get_pip_folders("aux_data")`
    + `stamp::st_alias_list()`).
  - If a measure's hash cannot be resolved (artifact missing), fail loudly
    (`cli::cli_abort`) per project convention — no silent fallback.
- **Test Scenarios**:
  - happy path: `pd_aux_attr()` returns attribute lists each carrying the aux
    hash for every measure in `aux_list`.
  - edge case: `aux_list` contains a measure not in the catalog → abort.
  - error path: `st_catalog_query()` fails → abort with clear message.
- **Tests**: `tests/testthat/test-pd-aux-attr.R` (new or extended).
- **Acceptance criteria**: Each survey's metadata attribute list carries the
  aux content hash for every measure used.

### 2. Surface aux hash columns into the master inventory in `build_pip_inventory()`
- **Requirements**: R2
- **Files**: `R/build_pip_inventory.R`
- **Details**: `build_pip_inventory()` reads version facts from stamp catalogs
  (`"pip"`, `"pip_meta"`, `"pip_inv"`) and joins them per `pip_id`. The aux
  hashes are stored in the `"pip_meta"` metadata artifacts (as attributes from
  Step 1). Add columns to the master inventory such as `aux_cpi_hash`,
  `aux_ppp_hash`, `aux_pfw_hash`, `aux_pop_hash`, `aux_gdp_hash`,
  `aux_pce_hash`, populated from the metadata attributes of each survey's
  `pip_meta` artifact.
  - Determine the cleanest read path: either (a) read the metadata attributes
    from the `pip_meta` catalog/artifact for current-run pip_ids, or (b) if
    `st_catalog_query` does not expose attributes, load each current-run
    `pip_meta` artifact via `pip_read()` and extract the aux hash attributes.
    Prefer the least-expensive path that is reliable.
  - Add the new columns to the `legacy_cols` drop-list in Step 1 of
    `build_pip_inventory()` so that if they are ever removed, on-disk masters
    migrate cleanly (see
    `.cg-docs/solutions/data-quality/2026-05-27-legacy-column-persistence-in-on-disk-inventory.md`).
  - Ensure the upsert (`collapse::rowbind(new_versions, old_retained, fill = TRUE)`)
    does not break on class mismatches for the new columns (they are character
    hashes — no special class).
- **Test Scenarios**:
  - happy path: master inventory rows for current-run pip_ids carry the aux
    hash columns.
  - edge case: a survey's `pip_meta` artifact lacks an aux hash attribute →
    column is NA (survey cleaned before this feature).
  - error path: `pip_meta` artifact cannot be read → abort or warn per existing
    catalog-failure handling.
- **Tests**: `tests/testthat/test-build_pip_inventory.R` (extend).
- **Acceptance criteria**: Master inventory has one aux hash column per tracked
  measure, populated for current-run surveys.

## Phase 2: Compare and gate in valid_dlw_load

### 3. Implement two-stage aux-change gating in `valid_dlw_load()`
- **Requirements**: R3, R4, R5, R6, R7, R8, R9, R10
- **Files**: `R/valid_dlw_load.R`
- **Details**: Rewrite the aux-change detection portion of `valid_dlw_load()`.
  The comparison happens **inside `valid_dlw_load()`** (not `valid_aux_load()`),
  because it is the only place with access to both the stored aux hashes
  (master inventory) and the current aux hashes (`st_catalog_query`).

  **Stage 1 — build candidate set (cheap)**:
  - Load the master inventory (already done for `inv_to_process()`; reuse the
    loaded `dt_master`).
  - For each measure in `aux_measures`, fetch the current aux content hash via
    `st_catalog_query()` (helper from Step 1, or a shared helper).
  - For each previously-cleaned survey (present in master), compare its stored
    per-survey aux hash (from the master's `aux_<measure>_hash` columns) against
    the current hash for that measure. If any measure's stored hash differs from
    current → the survey is a **candidate**.
  - New surveys (no master row) are always processed (unchanged path).
  - Previously-cleaned surveys with **no stored aux hash** (NA) are treated as
    changed → candidate (R6).
  - Track which measures changed (for Stage 2).

  **Stage 2 — actual aux changes (detailed)**:
  - For the **changed measures only**, call `valid_aux_load(measure = <changed
    measures>, compare = "all")` → `compare_aux_*` → `filter_aux_inv()` to find
    surveys with actual changes inside the aux file.
  - **Intersect** the affected surveys with the candidate set. Only surveys in
    both sets are re-cleaned.
  - If no measures changed → skip `valid_aux_load()` entirely, log
    `aux_no_changes_inf`, `inv_aux = NULL`.

  **Logging**: preserve the existing discriminators with updated trigger
  conditions:
  - `aux_no_changes_inf` — no measure's aux hash changed (Stage 1 empty).
  - `aux_changes_no_surveys_inf` — measures changed but no candidate survey is
    actually affected (Stage 2 empty).
  - `aux_changes_inf` — at least one survey is affected and will be re-cleaned.
  - `surveys_to_clean_inf` — after combining/dedup, with counts.

  **Constraints**: all `joyn::` joins use `reportvar = FALSE`; `verbose`
  propagated to all downstream I/O; `logmeta$info` stays a string discriminator.
- **Test Scenarios**:
  - happy path: survey with unchanged stored aux hash is NOT re-cleaned.
  - happy path: survey with changed stored aux hash IS re-cleaned.
  - happy path: new survey always processed.
  - edge case: previously-cleaned survey with no stored aux hash → re-cleaned.
  - edge case: measure changed but no candidate actually affected → no re-clean,
    `aux_changes_no_surveys_inf`.
  - error path: `st_catalog_query()` fails → abort.
  - regression: DLW-new detection (`inv_to_process()`) still runs and is
    unchanged.
- **Tests**: `tests/testthat/test-valid_dlw_load.R` (extend), plus
  `tests/testthat/test-valid_aux_load.R` if the helper is shared.
- **Acceptance criteria**: `valid_dlw_load()` only re-cleans surveys whose aux
  actually changed; DLW-new surveys always processed; all logmeta
  discriminators fire correctly.

## Testing Strategy

- Unit tests with `local_mocked_bindings()` for `st_catalog_query`,
  `load_pip_master_inventory`, `valid_aux_load`, `filter_aux_inv` (existing
  pattern in `test-valid_dlw_load.R`).
- Extend `test-build_pip_inventory.R` for the new aux hash columns.
- Extend/add `test-pd-aux-attr.R` for aux hash capture.
- Run `devtools::test(filter = "valid_dlw")` and `devtools::test(filter =
  "build_pip_inventory")` after each phase; full `devtools::test()` at the end.

## Documentation Checklist

- Update roxygen `@details` on `valid_dlw_load()` describing the two-stage
  gating and updated logmeta trigger conditions.
- Update `compound-gpid.context.md` canonical logmeta list if trigger
  conditions change.
- Update `man/valid_dlw_load.Rd` via `devtools::document()`.

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| `st_catalog_query()` cannot resolve aux artifact hashes (aux not in a queryable catalog) | Verify aux alias resolution; fall back to loading aux data and hashing if needed (documented decision) |
| Adding aux columns breaks the master-inventory upsert/rowbind (class mismatch) | New columns are character hashes; add to `legacy_cols` drop-list; test the upsert |
| Gating accidentally skips DLW-new surveys | Keep `inv_to_process()` path unchanged; add regression test (C5) |
| Per-survey hash comparison misses a survey cleaned with a different aux version | Per-survey columns handle this precisely; test multi-version scenario |
| `compare_aux_*` results not intersecting correctly with candidate set | Explicit intersection step; test both empty and non-empty intersections |

## Out of Scope

- Changing the DLW `content_hash` comparison in `inv_to_process()` (already done).
- Modifying `compare_aux_*` / `valid_aux_load()` behavior in pipaux.
- Deflation changes.
- Roadmap writes (handled by `@cg-roadmap`).

## Completion Contract

### Outcome

`valid_dlw_load()` gates aux-change detection using a two-stage filter: (1)
per-survey stored-vs-current aux content-hash comparison (master inventory vs
`st_catalog_query`) builds a candidate set of surveys whose aux version
changed; (2) `valid_aux_load()`/`compare_aux_*` runs only for the changed
measures and its results are intersected with the candidate set. Aux versions
are captured per-survey during processing and persisted as new columns in the
master inventory.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required |
|----|-------------------|------------------|----------|
| V1 | Aux content hashes captured per-survey in `pd_aux_attr()` | Code review + unit test | yes |
| V2 | Aux hash columns surfaced into master inventory by `build_pip_inventory()` | Code review + unit test | yes |
| V3 | Helper fetches current aux hashes via `st_catalog_query()` | Code review + unit test | yes |
| V4 | Stage 1: survey with unchanged stored aux hash is NOT a candidate | Unit test (mock) | yes |
| V5 | Stage 1: survey with changed stored aux hash IS a candidate | Unit test (mock) | yes |
| V6 | Stage 1: new survey (no master row) always processed | Unit test (mock) | yes |
| V7 | Stage 1: previously-cleaned survey with no stored aux hash treated as changed | Unit test (mock) | yes |
| V8 | Stage 2: `valid_aux_load()` called only for changed measures | Unit test (mock) | yes |
| V9 | Stage 2: affected surveys intersected with candidate set | Unit test (mock) | yes |
| V10 | `devtools::test(filter = "valid_dlw")` passes | Test command | yes |
| V11 | `devtools::test()` full suite passes, no regressions | Test command | yes |
| V12 | Roxygen `@details` + `compound-gpid.context.md` logmeta notes updated | Code review | yes |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | All `joyn::` joins use `reportvar = FALSE` | Code review |
| C2 | `logmeta$info`/`logmeta$error` remain string discriminators | Code review |
| C3 | New master-inventory columns added to `legacy_cols` drop-list if ever removed | Code review |
| C4 | Content-hash comparison (not version_id) is the change signal | Code review |
| C5 | DLW-new survey detection path unchanged (still always runs) | Code review |
| C6 | `verbose` propagated to all downstream I/O calls | Code review |
| C7 | `compare_aux_*` (pipaux) not modified; only measures passed, results intersected | Code review |
| C8 | Gating applies to every measure in `aux_measures` param | Code review |

### Boundaries

- Allowed: New aux hash columns in master inventory; new helper for current aux
  hashes; two-stage gating logic in `valid_dlw_load()`; capture in
  `pd_aux_attr()`; tests; docs.
- Out of scope: Changing the DLW `content_hash` comparison in `inv_to_process()`
  (already done); modifying `compare_aux_*` in pipaux; deflation changes;
  roadmap writes (handled by `@cg-roadmap`).

### Iteration Policy

1. Implement capture (Step 1) → persist (Step 2) → compare/gate (Step 3) in
   order.
2. After each step, run targeted tests; fix failures before proceeding.
3. If a step reveals a schema or API conflict, stop and consult the user.
4. Final full-suite run must pass before completion.

### Blocked-Stop Conditions

- If `st_catalog_query()` cannot resolve aux artifact hashes (aux not in a
  queryable catalog), stop and re-evaluate the "current aux hash" source.
- If adding aux columns to the master inventory breaks the upsert/rowbind
  (class mismatch), stop and address schema migration.
- If the aux-change gating causes DLW-new surveys to be skipped, stop (this
  violates C5).
