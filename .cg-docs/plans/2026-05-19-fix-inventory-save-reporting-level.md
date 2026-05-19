---
date: 2026-05-19
title: "Fix inventory save: primary key, version cross-join, reporting_level"
status: completed
completed-date: 2026-05-19
scope: "Standard"
brainstorm: null
language: "R"
estimated-effort: "medium"
tags: [inventory, save, deflation, metadata, reporting_level, fix]
---

# Plan: Fix inventory save — primary key, version cross-join, reporting_level

## Objective

Fix three related bugs in the pipeline save/inventory flow:

1. **Metadata version cross-contamination**: For surveys with multiple pip_ids (e.g., BOL_2022_EH → BOL_2022_EH_INC_ALL + BOL_2022_EH_INC_GPWG), the `joyn::left_join(..., relationship = "many-to-many")` in `update_pip_inventory()` can cross-join data versions with metadata versions from different pip_ids. Additionally, pip_ids for which metadata was not successfully saved must be excluded from the inventory and logged. When `pd_deflation()` later uses `content_hash_metadata` to resolve the correct metadata version, mismatched entries cause failures.
2. **Missing reporting_level in inventory**: The `reporting_level` (computed from PFW domain columns in `report_lvl()`) is never propagated to the master inventory. Downstream consumers (e.g., deflation helpers, release filtering) need this to distinguish national from subnational surveys without re-reading the PFW.
3. **Noisy pk warning on load**: `load_pip_data()` emits a "No primary key recorded" stamp warning for every survey load because microdata has no natural column pk (`pk = NULL` is intentional). This should be suppressed in pipload.

## Context

- `update_pip_inventory()` joins `vrs_dt` (data versions) to `vrs_mdt` (metadata versions) by `(survey_id, pip_id)` with `relationship = "many-to-many"`. This is correct *only* if both tables have exactly one row per `(survey_id, pip_id)` — which is the expected behaviour of `format_vrs()`. However, `"many-to-many"` masks integrity violations silently.
- The `reporting_level` column is computed in `get_country_pfw.R` → `report_lvl()` and set as an attribute on each cleaned survey (via `add_main_att()`). It is available in `pd_aux_attr()` output metadata lists but never explicitly extracted to a column in the inventory.
- The master inventory already has `welfare_type` extracted from `pip_id`; `reporting_level` should follow a similar pattern.
- stamp's `warn_missing_pk_on_load = TRUE` (default) causes a warning on every `st_load()` when the artifact has no pk sidecar. `pip_read()` forwards `verbose` to `st_load()` but the pk warning is controlled by a separate stamp option, not `verbose`.

## Requirements

| ID  | Requirement                                                              | Source      |
|-----|--------------------------------------------------------------------------|-------------|
| R1  | Every pip_id in the inventory must have a corresponding metadata entry; entries without metadata must be excluded and logged | user/review |
| R2  | The version join must not produce cross-products for multi-pip_id surveys | user/error  |
| R3  | `reporting_level` must appear as a column in the master inventory        | user        |
| R4  | `pd_deflation()` must be able to resolve correct metadata per pip_id     | user/error  |
| R5  | The "No primary key recorded" warning must be suppressed when loading pip survey data via pipload | user/review |

## Implementation Steps

### 1. Fix version join to prevent cross-contamination and enforce metadata presence

- **Requirements**: R1, R2, R4
- **Files**: `R/update_pip_inventory.R`
- **Details**:

  **Diagnostic first**: Before implementing the fix, verify the actual data shape for BOL 2022. Run `format_vrs()` on the real `proc_dta` and check:
  - Does `vrs_dt` have exactly one row per `(survey_id, pip_id)`?
  - Does `vrs_mdt` have exactly one row per `(survey_id, pip_id)`?
  - If one side has 0 rows for a pip_id (e.g., skipped metadata save), is that the source of the mismatch?

  **Fix the join**:
  1. Change `relationship = "many-to-many"` to `relationship = "one-to-one"` in the `vrs_dt` ↔ `vrs_mdt` join. This will **error** if duplicates exist, making bugs visible.
  2. Before the join, add an informative guard using `cli::cli_abort()` (not `stopifnot()`) to check for duplicates in each table by `c("survey_id", "pip_id")`. Message should include which pip_ids have duplicates.
  3. In `format_vrs()`, add `pip_names <- unique(pip_names)` to prevent duplicates from propagating.

  **Enforce metadata presence**:
  4. After the join, check that every row in `vrs` has non-NA metadata columns (specifically `content_hash` from the metadata side). Rows where metadata is entirely missing indicate a failed or skipped metadata save.
  5. Exclude pip_ids with missing metadata from the inventory and log them:
     ```r
     missing_meta <- vrs[is.na(content_hash_metadata), .(survey_id, pip_id)]
     if (nrow(missing_meta) > 0L) {
       pipfun::log_add(
         event = "error",
         message = "Some pip_ids have no metadata. They will be excluded from the inventory.",

         name = "pipdata_log",
         logmeta = list(
           error = "missing_metadata_err",
           pip_ids = missing_meta$pip_id,
           surveys = missing_meta$survey_id
         )
       )
       vrs <- vrs[!is.na(content_hash_metadata)]
     }
     ```
  6. Add `"missing_metadata_err"` to the `log_report()` section builder so it appears in pipeline reports.

- **Test Scenarios**:
  - ✅ Happy path: BOL 2022 with 2 pip_ids → exactly 2 rows in joined result, both with metadata
  - 🛑 Edge case: single pip_id survey → 1 row (no change)
  - ❌ Error path: duplicate pip_names → caught by assertion, not silently cross-joined
  - ❌ Missing metadata: pip_id excluded from inventory, logged as error
- **Tests**: Unit test with mock `proc_dta` containing 2 pip_ids; verify joined result has exactly 2 rows. Test with one pip_id having NULL metadata → excluded and logged.
- **Acceptance criteria**: `relationship = "one-to-one"` in the join; pip_ids without metadata are excluded and logged; `log_report()` reports them.

### 2. Add `reporting_level` to the master inventory via metadata extraction

- **Requirements**: R3
- **Files**: `R/update_pip_inventory.R`, possibly `pipload/R/` (helper)
- **Details**: The `reporting_level` for each survey is available in the saved metadata (the attribute list stored via `pd_aux_attr()` under `alias = "pip_meta"`). Rather than duplicating the `report_lvl()` logic from `get_country_pfw.R`, create a helper that extracts a specific attribute value from already-saved metadata for a given pip_id.

  **Approach**: Create a helper function (evaluate whether it belongs in pipdata or pipload) that, given a pip_id and an attribute name, loads the metadata and returns the value. For the inventory build, we can either:
  - **(Option A — preferred)** Extract `reporting_level` from the PFW that is already loaded in `update_pip_inventory()` (the `pfw` object is read for the release inventory step). Use `report_lvl()` from `get_country_pfw.R` on the relevant PFW subset, then join the resulting `reporting_level` by `(country_code, surveyid_year, survey_acronym)`. This reuses the existing function rather than duplicating logic.
  - **(Option B — future)** Build a generic `get_inv_attr(pip_id, attr_name)` helper that loads pip_meta and returns a specific value. This would be useful if more metadata attributes need to be surfaced in the inventory later. Evaluate whether this is worth it now or defer to a separate plan.

  **Implementation (Option A)**:
  - After loading `pfw` in `update_pip_inventory()`, compute `reporting_level` on the PFW subset (filter `inpovcal == 1`, take `unique()` by survey keys, apply `pmax()` across domain columns as `report_lvl()` does).
  - Left-join to `new_pip_inv` by `(country_code, surveyid_year, survey_acronym)`.
  - Convert to character (`"1"` or `"2"`) for consistency with the attribute format.
  - Handle missing PFW rows gracefully → `reporting_level = NA_character_`.

  **Note on P3.2**: The PFW may have multiple `welfare_type` rows per survey. Filter to `inpovcal == 1` and take `unique()` over `(country_code, surveyid_year, survey_acronym)` before joining, since domain columns are the same across welfare types for the same survey.

- **Test Scenarios**:
  - ✅ Happy path: national survey (all domains = 1) → reporting_level = "1"
  - ✅ Happy path: subnational survey (cpi_domain = 2) → reporting_level = "2"
  - 🛑 Edge case: PFW row missing for a survey → reporting_level = NA
  - 🛑 Edge case: Multiple PFW welfare_type rows → same reporting_level (no duplication)
- **Tests**: Unit test with mock inventory and PFW; verify reporting_level column present and correct.
- **Acceptance criteria**: `new_pip_inv` has a `reporting_level` column with values `"1"` or `"2"`; logic reuses `report_lvl()` or `.DOMAIN_COLS` constant rather than hardcoding domain column names.

### 3. Document `reporting_level` in `update_pip_inventory()` roxygen

- **Requirements**: R3
- **Files**: `R/update_pip_inventory.R`
- **Details**: Add a bullet to the `@return` and/or `@details` roxygen block explaining:
  - `reporting_level`: Character `"1"` or `"2"`. Derived from PFW domain columns (`cpi_domain`, `ppp_domain`, `gdp_domain`, `pce_domain`, `pop_domain`). `"1"` = national (all domains equal 1); `"2"` = subnational (at least one domain equals 2, meaning urban/rural-specific auxiliary data are available for that survey). `NA` when the survey has no matching PFW row.
- **Test Scenarios**:
  - ✅ `devtools::document()` succeeds without warnings
- **Tests**: None (documentation only).
- **Acceptance criteria**: Roxygen block for `update_pip_inventory()` describes `reporting_level`.

### 4. Suppress "No primary key" warning in pipload when loading pip survey data

- **Requirements**: R5
- **Files**: `pipload/R/load_pip_data.R`, `pipload/tests/testthat/test-pip_load_data.R`
- **Details**: stamp's `st_load()` emits a "No primary key recorded" warning when `warn_missing_pk_on_load = TRUE` (the default) and the artifact has no pk in its sidecar. This is always the case for pip survey data (`alias = "pip"`) since microdata has no natural column pk.

  **Fix**: The suppression is applied in `load_pip_data()` — **not** in `pip_read()` — because `pip_read()` is a generic wrapper used for inventories, metadata, and other artifacts that may legitimately have pk requirements. Scoping the suppression to `load_pip_data()` targets only pip microdata loads.

  Implementation: save and restore `warn_missing_pk_on_load` via `on.exit()` just before the `pip_read()` call inside `load_pip_data()`.

- **Test Scenarios**:
  - ✅ Loading pip survey data (no pk) → no warning emitted
  - ✅ Loading inventory data (has pk) → no false positive (no warning to suppress anyway)
  - 🛑 Edge case: user explicitly sets `warn_missing_pk_on_load = TRUE` globally → restored after load
- **Tests**: Unit test in `test-pip_load_data.R` using `local_mocked_bindings` to capture the stamp option value at the moment `pip_read` is called, verifying it is `FALSE` during the call and restored to `TRUE` after.
- **Acceptance criteria**: `load_pip_data("BOL_2022_EH_INC_ALL")` produces no pk warning; option is restored on return.

## Testing Strategy

- Mock-based unit tests for each step (avoid hitting Y-drive storage).
- Integration test: run `process_data()` on a known multi-pip_id survey (BOL 2022) and verify:
  - `content_hash_metadata` in inventory matches the actual metadata for that specific pip_id
  - `reporting_level` column is populated
  - No spurious rows from cross-joining

## Documentation Checklist

- [ ] Function documentation (roxygen2) — update `update_pip_inventory()` with `reporting_level` description
- [ ] Inline comments for the join fix explaining why `one-to-one`
- [ ] Add `"missing_metadata_err"` logmeta type to `compound-gpid.context.md` canonical list
- [ ] Update `compound-gpid.context.md` with any new domain rule

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Changing `relationship` to `one-to-one` may error on existing data with legitimate duplicates | Guard with `funique()` before the join; log a warning if dedup was required |
| Excluding pip_ids without metadata may silently drop surveys | Logged as error-level entry with `missing_metadata_err` type; surfaced in `log_report()` |
| PFW may not have rows for all surveys in inventory (e.g., historical surveys without inpovcal) | Use left join for reporting_level; allow NA |
| Suppressing pk warning in pipload may mask legitimate missing-pk issues for other artifacts | Suppression is scoped to `load_pip_data()` only — not `pip_read()`; `on.exit()` restores the option on return so other callers (inventories, metadata) are unaffected |

## Out of Scope

- Capturing `version_id` in `format_vrs()` (tracked separately as `store-version-id-in-inventory` in roadmap)
- Handling skipped saves in `format_vrs()` (related but separate concern)
- Changing the deflation lookup path from content_hash to version_id
- Stamp-level support for positional/row-number primary keys (microdata has no natural column pk; `pk = NULL` is intentional and correct — a future plan could add a synthetic `row_id` or a stamp option to suppress the "No primary key" warning)
