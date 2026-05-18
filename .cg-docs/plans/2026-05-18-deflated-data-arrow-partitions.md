---
date: 2026-05-18
title: "Switch Arrow partitions to deflated data input with multi-PPP welfare columns"
status: completed
completed-date: 2026-05-18
scope: "Standard"
phases: 3
completed-phases: [1, 2, 3]
current-phase: ~
brainstorm: ".cg-docs/brainstorms/2026-05-18-deflated-data-arrow-partitions.md"
language: "R"
estimated-effort: "large"
tags: [arrow, deflation, parquet, manifest, ppp, piptm, schema]
---

# Plan: Switch Arrow Partitions to Deflated Data Input

## Objective

Refactor the Arrow partition creation pipeline in `{pipdata}` to use `pipload::load_pip_deflated_data()` as input (instead of `load_pip_data()`), store all deflated welfare columns in Parquet, enrich the manifest with `welfare_vars` and `ppp_sort`, and update `{piptm}` to consume the new schema with PPP-aware welfare column selection.

## Context

Currently `generate_arrow_dataset()` loads raw survey data via `pipload::load_pip_data()` and `prepare_for_arrow()` writes a single `welfare` column. The pipeline already produces deflated datasets via `load_pip_deflated_data()` with multiple `welfare_ppp_*` columns and attributes (`welfare_vars`, `ppp_sort`). This plan eliminates the disconnect by using the deflated output directly.

The brainstorm decided on Approach 1: refactor in-place across both packages to avoid dead code and interim broken states.

## Requirements

| ID  | Requirement                                                         | Source     |
|-----|---------------------------------------------------------------------|------------|
| R1  | `generate_arrow_dataset()` uses `load_pip_deflated_data(id_name=)` | brainstorm |
| R2  | All `welfare_*` columns stored in Parquet (from `attr(data, "welfare_vars")`) | brainstorm |
| R3  | `prepare_for_arrow(data, pip_id)` reads metadata from dataset attributes | brainstorm |
| R4  | `pip_id` remains a required argument (from inventory)               | brainstorm |
| R5  | Manifest entries gain `welfare_vars` (char vector) and `ppp_sort` (integer) | brainstorm |
| R6  | `pip_arrow_schema()` becomes base schema (no `welfare`); new `pip_welfare_schema(welfare_vars)` helper | brainstorm |
| R7  | `load_survey_microdata()` / `load_surveys()` gain `ppp` arg; select + rename to `welfare` | brainstorm |
| R8  | `table_maker()` gains `ppp` arg; validates against manifest; passes to loaders | brainstorm |
| R9  | Breakdown dimensions unchanged; auxiliary columns still dropped     | brainstorm |

## Phase 1: pipdata — Arrow Prep & Generation

### 1. Refactor `inject_metadata_cols()` to read from attributes

- **Requirements**: R3
- **Files**: `pipdata/R/arrow_prep.R`
- **Details**: Change signature to `inject_metadata_cols(dt, pip_id)`. Extract `country_code`, `surveyid_year`, `welfare_type`, `vermast`, `veralt` from `attributes(dt)` instead of a `metadata` list argument. Build `version` from `vermast`/`veralt` as before. Inject `pip_id` from the argument.
- **Test Scenarios**:
  - ✅ Deflated dt with correct attributes → columns injected
  - 🛑 Missing required attribute → informative error
  - ❌ `pip_id = NA` → abort
- **Acceptance criteria**: `inject_metadata_cols()` works without a `metadata` list; all metadata columns injected correctly from attributes.

### 2. Update `cast_data_cols()` to handle multiple welfare columns

- **Requirements**: R2
- **Files**: `pipdata/R/arrow_prep.R`
- **Details**: Instead of casting a single `welfare` column, discover welfare columns from `attr(dt, "welfare_vars")` and cast all of them to `double`. Also cast `weight` as before.
- **Test Scenarios**:
  - ✅ dt with 4 welfare columns → all cast to double
  - 🛑 welfare_vars attribute missing → informative error
  - 🛑 A welfare column listed in attr but not in dt → error
- **Acceptance criteria**: All welfare columns in `welfare_vars` are double after calling.

### 3. Update `validate_pre_write()` for dynamic welfare columns

- **Requirements**: R2, R9
- **Files**: `pipdata/R/arrow_prep.R`
- **Details**: Replace the fixed `allowed_cols` vector with a dynamic one that includes all welfare columns from `attr(dt, "welfare_vars")`. Validate each welfare column (non-negative, finite). Remove the single `welfare` required-column check; instead require at least one welfare column. Keep all other checks (partition key consistency, weight validity, factor conformance, etc.).
- **Test Scenarios**:
  - ✅ dt with welfare_lcu + 3 welfare_ppp_* → passes
  - 🛑 Extra column not in schema or welfare_vars → abort
  - ❌ welfare_ppp column with NA values → abort
  - ❌ No welfare columns at all → abort
- **Acceptance criteria**: Validation passes for multi-welfare dt and rejects invalid data with clear messages.

### 4. Refactor `prepare_for_arrow(data, pip_id)` orchestrator

- **Requirements**: R2, R3, R4, R9
- **Files**: `pipdata/R/arrow_prep.R`
- **Details**: New signature: `prepare_for_arrow(data, pip_id)`. Remove `metadata` parameter. Steps:
  1. Copy data
  2. `inject_metadata_cols(dt, pip_id)` — from attributes
  3. `cast_data_cols(dt)` — all welfare columns
  4. Standardise breakdowns (unchanged)
  5. Column selection: allowed = fixed partition/id cols + welfare_vars + weight + optional dimensions. Drop everything else. Drop all-NA optional dimensions.
  6. `validate_pre_write(dt)`
  Preserve `welfare_vars` and `ppp_sort` as attributes on the output dt (needed by manifest generation).
- **Test Scenarios**:
  - ✅ Full deflated dt → correct output with all welfare cols + dimensions
  - ✅ Attributes preserved on output
  - 🛑 Input not a data.table → error
  - 🛑 Missing welfare_vars attribute → error
- **Acceptance criteria**: Output dt has welfare columns, partition keys, dimensions, and welfare_vars/ppp_sort attributes.

### 5. Update `generate_arrow_dataset()` to use `load_pip_deflated_data()`

- **Requirements**: R1, R4
- **Files**: `pipdata/R/arrow_generation.R`
- **Details**: In the per-survey loop, replace the two `load_pip_data()` calls (raw + meta) with a single `pipload::load_pip_deflated_data(id_name = pip_id_i)`. Pass the result directly to `prepare_for_arrow(data, pip_id = pip_id_i)`. Remove the `meta` variable. Update the file header comments.
- **Test Scenarios**:
  - ✅ Single pip_id → loads deflated data, writes multi-welfare Parquet
  - ❌ pip_id not found by load_pip_deflated_data → caught in tryCatch, status = "error"
- **Acceptance criteria**: `generate_arrow_dataset()` produces Parquet files with multiple welfare columns.

### 6. Update `.build_arrow_schema()` for dynamic welfare columns

- **Requirements**: R2, R6
- **Files**: `pipdata/R/arrow_generation.R`
- **Details**: Currently calls `piptm::pip_arrow_schema()` which has a fixed `welfare` field. After piptm is updated (Phase 2), the base schema won't include welfare. For now, build the schema locally: fixed fields from piptm base schema + all welfare columns as `float64()`. This makes pipdata self-sufficient for schema construction during writes.
- **Test Scenarios**:
  - ✅ col_names with 4 welfare columns → schema includes all as float64
  - ✅ Base fields still get correct types (dict for gender/area, int32 for age)
- **Acceptance criteria**: Arrow schema matches actual data columns exactly.

## Phase 2: piptm — Schema, Loading & table_maker

### 7. Refactor `pip_arrow_schema()` and add `pip_welfare_schema()`

- **Requirements**: R6
- **Files**: `piptm/R/schema.R`
- **Details**: Remove the `welfare` field from `pip_arrow_schema()$fields`. Add a new exported function `pip_welfare_schema(welfare_vars)` that returns a list of field specs (each `float64`, required = TRUE). Update `pip_required_cols()` to no longer include `welfare` (or accept a `welfare_vars` argument). Update `pip_allowed_cols()` similarly — base + welfare_vars.
- **Test Scenarios**:
  - ✅ `pip_arrow_schema()$fields` does not contain `welfare`
  - ✅ `pip_welfare_schema(c("welfare_lcu", "welfare_ppp_2017"))` returns 2 float64 fields
  - ✅ `pip_required_cols()` returns 6 base cols (no welfare)
  - ✅ `pip_allowed_cols(welfare_vars = c("welfare_lcu"))` returns base + welfare_lcu
- **Acceptance criteria**: Schema functions work for both base-only and welfare-enriched usage.

### 8. Update manifest loading to parse `welfare_vars` and `ppp_sort`

- **Requirements**: R5
- **Files**: `piptm/R/manifest.R`
- **Details**: In `.load_manifests()`, after parsing entries, extract `welfare_vars` as a list column (each element = character vector, like `dimensions`) and `ppp_sort` as an integer column. Update `.empty_manifest_dt()` to include these columns. Update `piptm_manifest()` docstring.
- **Test Scenarios**:
  - ✅ Manifest JSON with welfare_vars/ppp_sort → parsed correctly
  - ✅ Legacy manifest without these fields → `welfare_vars = list(character(0))`, `ppp_sort = NA_integer_` (graceful fallback)
- **Acceptance criteria**: `piptm_manifest()` returns dt with `welfare_vars` list column and `ppp_sort` integer column.

### 9. Update `load_survey_microdata()` with `ppp` argument

- **Requirements**: R7
- **Files**: `piptm/R/load_data.R`
- **Details**: Add `ppp = NULL` parameter. When non-NULL:
  1. Look up manifest entry's `welfare_vars` for the matched survey.
  2. Construct expected column name: `paste0("welfare_ppp_", ppp)` (e.g. `"welfare_ppp_2017_01_02"`). Or validate that at least one welfare_var matches the ppp pattern.
  3. After loading Parquet, select only that welfare column + all non-welfare columns.
  4. Rename the selected welfare column to `welfare`.
  When `ppp = NULL`: use `ppp_sort` from manifest as default; if `ppp_sort` is also NA, error.
  Attach `ppp_sort` attribute on result.
- **Test Scenarios**:
  - ✅ `ppp = 2017` with matching column → loads and renames to `welfare`
  - 🛑 `ppp = 2005` but column missing in welfare_vars → informative error listing available PPPs
  - ✅ `ppp = NULL` → defaults to ppp_sort from manifest
- **Acceptance criteria**: Returned dt has a single `welfare` column regardless of PPP choice.

### 10. Update `load_surveys()` with `ppp` argument

- **Requirements**: R7
- **Files**: `piptm/R/load_data.R`
- **Details**: Same logic as Step 9 but for batch loading. The `ppp` applies uniformly to all surveys in `entries_dt`. Validate that all entries have the requested PPP in their `welfare_vars` before loading. After loading, select + rename.
- **Test Scenarios**:
  - ✅ Batch of 3 surveys, all have ppp 2017 → combined dt with `welfare`
  - ❌ One survey missing the requested PPP → error listing which survey lacks it
- **Acceptance criteria**: Batch load works with PPP selection.

### 11. Update `table_maker()` with `ppp` argument

- **Requirements**: R8
- **Files**: `piptm/R/table_maker.R`
- **Details**: Add `ppp = NULL` parameter. Pass through to `load_surveys(entries, release, ppp = ppp)`. No changes to compute functions. Update docstring and examples.
- **Test Scenarios**:
  - ✅ `table_maker(pip_id = ..., ppp = 2017, measures = "gini")` → computes on correct welfare
  - ✅ `ppp = NULL` → uses manifest default (ppp_sort)
- **Acceptance criteria**: `table_maker()` orchestrates PPP selection end-to-end.

## Phase 3: Tests & Validation

### 12. Update pipdata tests for new `prepare_for_arrow()` signature

- **Requirements**: R2, R3, R4
- **Files**: `pipdata/tests/testthat/test-arrow-generation.R` (+ new test file if needed)
- **Details**: Update `make_arrow_dt()` helper to produce multi-welfare data with attributes. Update all tests that called `prepare_for_arrow(data, metadata, pip_id)` to use new `(data, pip_id)` signature. Add tests for:
  - welfare_vars attribute discovery
  - Multiple welfare columns written to Parquet
  - Round-trip: write multi-welfare Parquet, read back, verify columns
- **Acceptance criteria**: All pipdata arrow tests pass with new signature.

### 13. Update piptm tests for schema and loading changes

- **Requirements**: R6, R7, R8
- **Files**: `piptm/tests/testthat/test-schema.R`, `test-load-data.R`, `test-table-maker.R`, `test-manifest.R`
- **Details**:
  - `test-schema.R`: Update expected column counts; add tests for `pip_welfare_schema()`.
  - `test-manifest.R`: Add test for parsing welfare_vars/ppp_sort from JSON.
  - `test-load-data.R`: Add tests for `ppp` argument (mock Parquet with multi-welfare cols).
  - `test-table-maker.R`: Add test for `ppp` pass-through.
- **Acceptance criteria**: All piptm tests pass. R CMD check clean on both packages.

### 14. Update manifest generation to include `welfare_vars` and `ppp_sort`

- **Requirements**: R5
- **Files**: `pipdata/R/manifest_generation.R`
- **Details**: Update `build_manifest_entry()` to accept and include `welfare_vars` and `ppp_sort` fields. Update `generate_release_manifest()` to:
  - After reading Parquet schema, discover welfare columns (any column matching `^welfare_`)
  - Read `ppp_sort` from the Parquet file metadata or infer from column names (prefer the manifest being built from the same deflated data attributes — pass through from inventory or from the Parquet schema).
  - For now: discover welfare columns from schema; set `ppp_sort = NULL` (to be populated when the inventory carries this info).
- **Test Scenarios**:
  - ✅ Manifest entry includes welfare_vars array and ppp_sort integer
  - ✅ JSON output has correct structure
- **Acceptance criteria**: Generated manifest JSON includes welfare_vars and ppp_sort per entry.

## Testing Strategy

- **Unit tests**: Each modified function gets targeted tests with mock data.tables carrying the right attributes.
- **Integration test**: End-to-end: create a mock deflated dt → `prepare_for_arrow()` → `write_survey_parquet()` → read back with `load_survey_microdata(ppp = ...)` → verify single `welfare` column with correct values.
- **Backward compatibility**: Verify that legacy manifests (without welfare_vars/ppp_sort) load gracefully with NA defaults.
- **R CMD check**: Both packages must pass `devtools::check()` cleanly.

## Documentation Checklist

- [ ] `prepare_for_arrow()` roxygen updated (new signature, new examples)
- [ ] `generate_arrow_dataset()` roxygen updated (no more metadata loading)
- [ ] `pip_arrow_schema()` / `pip_welfare_schema()` roxygen
- [ ] `load_survey_microdata()` / `load_surveys()` — ppp parameter documented
- [ ] `table_maker()` — ppp parameter documented
- [ ] `build_manifest_entry()` — new fields documented
- [ ] `compound-gpid.context.md` updated with new manifest schema example

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| `load_pip_deflated_data()` not available for all surveys in inventory | Medium | High | Graceful skip with `status = "error"` in results; log which pip_ids failed |
| Breaking change to `prepare_for_arrow()` affects external callers | Low | Medium | Package is internal; no CRAN exposure. Document in NEWS.md |
| `welfare_vars` naming convention varies across surveys | Low | Medium | Use attribute as authoritative source; don't parse names |
| piptm tests depend on fixed 13-column schema | High | Low | Update test expectations in same PR |
| PPP column name matching logic fragile | Medium | Medium | Use exact match against manifest `welfare_vars` — no regex guessing |

## Out of Scope

- Migrating existing Parquet files written with old single-`welfare` schema
- Changes to `pd_deflation()` or `pd_deflate_pipeline()` themselves
- Adding `welfare_lcu` to the PPP selection logic (it's always stored but not selectable via `ppp` arg — it's the LCU welfare, not PPP-deflated)
- Updating the Plumber API layer in piptm (will inherit `ppp` from `table_maker()` naturally)
- Grouped data (`pipgd`) support — currently only microdata (`pipmd`) flows through Arrow
