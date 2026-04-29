---
date: 2026-04-29
title: "Replace date_valid filter with stamp-based vintage tracking"
status: completed
completed-date: 2026-04-29
scope: "Lightweight"
brainstorm: ".cg-docs/brainstorms/2026-04-29-date-valid-vintage.md"
language: "R"
estimated-effort: "small"
tags: [vintage, stamp, release-inventory, master-inventory, cleanup]
---

# Plan: Replace date_valid filter with stamp-based vintage tracking

## Objective

Remove the `date_valid` / `date_validated` filtering mechanism from `update_pip_inventory()`
and `valid_dlw_load()`. All validated surveys should be cleaned unconditionally. Release
inventory vintages are handled by stamp's version history. Add `first_release_version_id`
and `latest_release_version_id` columns to the master inventory to track when each survey
entered and was last confirmed in the release inventory.

## Context

- `valid_dlw_load()` has a commented-out `date_valid` parameter and filter (line 39, 112)
- `update_pip_inventory()` has an active `date_valid` parameter (defaulting to
  `max(inv_to_clean$date_validated)`) used to filter the release inventory with
  `date_validated < date_valid` — which has the bug of always excluding the most
  recently validated survey
- `pipload::pip_write()` returns `stamp::st_save()` output invisibly, which includes
  `version_id` — confirmed in `pipload/R/pip_read-write.R`
- `pd_process_data()` calls `update_pip_inventory(inv_to_clean, proc_dta)` without
  passing `date_valid` (uses default)

## Requirements

| ID  | Requirement                                                     | Source     |
|-----|-----------------------------------------------------------------|------------|
| R1  | Remove `date_valid` parameter from `update_pip_inventory()`     | brainstorm |
| R2  | Remove `date_validated < date_valid` filter from release inv    | brainstorm |
| R3  | Remove commented `date_valid` lines from `valid_dlw_load()`     | brainstorm |
| R4  | Capture `version_id` from `pip_write()` for release inventory   | brainstorm |
| R5  | Add `first_release_version_id` column to master inventory       | brainstorm |
| R6  | Add `latest_release_version_id` column to master inventory      | brainstorm |
| R7  | Update roxygen docs to reflect new behaviour                    | implied    |
| R8  | Existing tests continue to pass                                 | implied    |

## Implementation Steps

### 1. Remove dead code from `valid_dlw_load()`

- **Requirements**: R3
- **Files**: `R/valid_dlw_load.R`
- **Details**:
  - Delete commented line 39: `# date_valid = .pipdataenv$date_valid,`
  - Delete commented line 112: `# inv_to_clean <- inv_to_clean[date_validated < date_valid]`
  - Delete the comment above line 112: `# Only those after specific date validated`
- **Tests**: Existing tests for `valid_dlw_load()` should pass unchanged
- **Acceptance criteria**: No reference to `date_valid` remains in `valid_dlw_load.R`

### 2. Refactor `update_pip_inventory()` — remove filter, capture version, add columns

- **Requirements**: R1, R2, R4, R5, R6
- **Files**: `R/update_pip_inventory.R`
- **Details**:
  1. Remove `date_valid = max(inv_to_clean$date_validated)` parameter
  2. Remove the `if (!inherits(date_valid, "POSIXct"))` defense
  3. Remove `date_validated < date_valid` filter from the release inventory subsetting
     (keep the PFW join as-is)
  4. Capture the return value of `pipload::pip_write()` for the release inventory:
     ```r
     release_result <- pipload::pip_write(
       x = release_pip_inv,
       id = "pip_release_inventory",
       alias = "pip_inv",
       pk = c("survey_id", "pip_id")
     )
     release_vid <- release_result$version_id
     ```
  5. After computing `new_pip_inv` (row-bind of `pip_inv` and `old_pip_inv`), add columns:
     - For newly processed surveys (those in `pip_inv`): set
       `first_release_version_id = release_vid` (only if currently `NA`)
     - For all surveys present in the release inventory: set
       `latest_release_version_id = release_vid`
     - For surveys NOT in release (not in PFW `inpovcal == 1`): leave both columns as-is
  6. The master inventory `pip_write()` call moves AFTER the release inventory write
     (since we need the release `version_id` to populate the columns)
- **Test Scenarios**:
  - ✅ Happy path: New surveys get both columns set to the same version_id
  - ✅ Repeat run: `first_release_version_id` is preserved, `latest_release_version_id` updated
  - 🛑 Edge case: Survey in master but NOT in release (not in PFW) — columns stay NA
  - 🛑 Edge case: `pip_write()` returns `skipped = TRUE` (no content change) — use `st_latest()` fallback
  - ❌ Error path: `pip_write()` returns NULL version_id — log warning, leave columns NA
- **Tests**: Update `test-logging-integration.R` contract tests to remove expectation
  of `date_valid` parameter; add unit test for column population logic
- **Acceptance criteria**: 
  - No `date_valid` parameter in function signature
  - Release inventory contains all PFW-eligible surveys (no date filter)
  - Master inventory has both new columns populated correctly

### 3. Update documentation

- **Requirements**: R7
- **Files**: `R/update_pip_inventory.R`, `R/valid_dlw_load.R`, `man/` (auto via roxygen)
- **Details**:
  - Remove `@param date_valid` from roxygen in `update_pip_inventory()`
  - Update `@return` to mention the new columns
  - Mention in `@details` that release vintages are tracked via stamp versions
  - Remove `date_validated` from `globalVariables()` in `aaa.R` if no longer used
    in NSE context (check first — it may still be used in data.table subsetting
    elsewhere)
- **Tests**: `R CMD check` passes without documentation warnings
- **Acceptance criteria**: `devtools::document()` + `devtools::check()` clean

## Testing Strategy

- Existing `test-logging-integration.R` tests should pass (they test logmeta structure,
  not the `date_valid` parameter)
- Add a focused test for the version_id column logic:
  - Mock `pipload::pip_write()` to return a known version_id
  - Verify `first_release_version_id` is set on first run
  - Verify `latest_release_version_id` is updated on second run
  - Verify `first_release_version_id` is NOT overwritten on second run

## Documentation Checklist

- [x] Function documentation (roxygen2) — updated in Step 3
- [ ] Pipeline overview (`docs/pipeline_overview.qmd`) — brief note about vintage model
- [ ] Inline comments for the column-population logic

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| `pip_write()` returns `skipped = TRUE` when content unchanged | No version_id available | Fall back to `stamp::st_latest("pip_release_inventory", alias = "pip_inv")` |
| Existing master inventories lack the new columns | NA columns on first load | `collapse::rowbind(..., fill = TRUE)` already handles this — new columns will be NA for old rows |
| Reordering master/release writes changes error semantics | Master not saved if release write fails | Wrap release write in tryCatch; if it fails, still save master without version columns |

## Out of Scope

- Migrating historical master inventories to populate the new columns retroactively
- Adding a user-facing API to query "which surveys entered after release X"
- Changing `pipload::pip_write()` internals (it already returns what we need)
- The `date_validated` column in `aaa.R` globalVariables — keep it if used elsewhere
