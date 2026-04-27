---
date: 2026-04-27
title: "Migrate dplyr to collapse/data.table (Phase 1: 3 files)"
status: completed
completed-date: 2026-04-27
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-04-27-dplyr-to-collapse-dt.md"
language: R
estimated-effort: medium
tags: [refactoring, performance, dependencies, dplyr, collapse, data.table]
---

# Plan: Migrate dplyr to collapse/data.table (Phase 1)

## Objective

Replace all `dplyr::`, `tidyr::`, and `tibble::` calls in three pipdata
source files with collapse/data.table equivalents. These packages are used
via `::` but are **not declared in DESCRIPTION Imports** — phantom
dependencies that will break if dplyr is not loaded in the session.

## Context

The project style guide mandates collapse for statistics and data.table for
data manipulation. Several files (`update_pip_inventory.R`,
`pd_wbpip_clean.R`, `pd_dlw_clean.R` partially) already follow this
convention. This plan covers the remaining 3 simpler files. Phase 2
(`dlw_scan_and_validate.R`, ~20 call sites) is a separate roadmap item.

Existing test coverage for the affected functions is minimal:
`test-pd_dlw_gd_clean.R` contains only a placeholder test. No tests exist
for `pipdata_validation_report.R` or `pipdata_validate_gmd.R` helper logic.

## Requirements

| ID  | Requirement                                                   | Source      |
|-----|---------------------------------------------------------------|-------------|
| R1  | Replace all `dplyr::case_when` with `data.table::fcase()`    | brainstorm  |
| R2  | Use `default = NA_real_` (or `NA_character_`) in all `fcase`  | user        |
| R3  | Replace `dplyr::bind_rows` with `data.table::rbindlist()`    | brainstorm  |
| R4  | Replace `dplyr::count` with `[, .N, by = ...]`               | brainstorm  |
| R5  | Remove `tidyr::as_tibble()` (data is already data.table)     | brainstorm  |
| R6  | Leave commented-out dplyr lines untouched                     | user        |
| R7  | Add regression tests for migrated recode functions            | user        |
| R8  | Pure mechanical translation — no logic changes                | user        |

## Implementation Steps

### 1. Add regression tests for `pd_dlw_clean.R` recode functions

- **Requirements**: R7
- **Files**: `tests/testthat/test-pd_dlw_clean.R` (new)
- **Details**: Write unit tests for `recode_edu()`, `recode_gndr()`, and
  `recode_age()` **before migration**, running against the current
  dplyr-based code. This establishes a baseline.
  - Build small data.tables with known values, feed them through each
    function, assert output matches expectations.
  - Functions expect `pipmd`-class data.tables (set class attribute in
    test fixtures).
- **Test Scenarios**:
  - ✅ `recode_edu()`: educy values -1, 0, 25, 50, 51, NA → expected
    NA, 0, 25, 50, NA, NA
  - ✅ `recode_edu()`: literacy 0, 1, 99, NA → "no", "yes", NA, NA
  - ✅ `recode_edu()`: school 0, 1, NA → "no", "yes", NA
  - ✅ `recode_gndr()`: male 0, 1, NA → "female", "male", NA
  - ✅ `recode_age()`: age values -1, 0, 55, 110, 111, NA → NA, 0, 55,
    110, NA, NA
  - 🛑 Edge case: column missing entirely (function should return dt
    unchanged)
  - 🛑 Edge case: all values NA
- **Acceptance criteria**: All tests pass with the current dplyr-based
  code. Tests do not depend on external data or packages beyond
  data.table and testthat.

### 2. Migrate `pipdata_validation_report.R`

- **Requirements**: R4
- **Files**: `R/pipdata_validation_report.R`
- **Details**: Single replacement in `get_data_status()`:
  ```r
  # Before
  valid_data |> dplyr::count(data_status)

  # After
  valid_data[, .N, by = data_status]
  ```
  Note: `dplyr::count()` returns a tibble with column `n`;
  `[, .N, by = ...]` returns a data.table with column `N`. If any
  downstream code references `$n`, rename the column:
  `valid_data[, .(n = .N), by = data_status]`.
- **Test Scenarios**:
  - ✅ Output has correct column names (`data_status`, `n`)
  - ✅ Counts match for "Valid" and "In valid" groups
  - 🛑 Edge case: all surveys valid (one group only)
- **Tests**: Not practical to unit test in isolation (requires
  `.pipdata$validation_report` env). Verify manually or with integration
  test.
- **Acceptance criteria**: Function returns a data.table with the same
  structure as before. `devtools::check()` passes.

### 3. Migrate `pd_dlw_clean.R`

- **Requirements**: R1, R2, R6
- **Files**: `R/pd_dlw_clean.R`
- **Details**: Replace 6 active `dplyr::case_when` calls with
  `data.table::fcase()`:

  **In `recode_edu()`** (3 replacements):
  ```r
  # educy: case_when → fcase
  educy = fcase(
    educy < 0, NA_real_,
    educy >= 0 & educy <= 50, educy,
    educy > 50, NA_real_,
    default = NA_real_
  )

  # literacy: case_when → fcase
  literacy = fcase(
    literacy == 1, "yes",
    literacy == 0, "no",
    default = NA_character_
  )

  # school: case_when → fcase
  school = fcase(
    school == 1, "yes",
    school == 0, "no",
    default = NA_character_
  )
  ```

  **In `recode_gndr()`** (1 replacement):
  ```r
  gender = fcase(
    male == 1, "male",
    male == 0, "female",
    default = NA_character_
  )
  ```

  **In `recode_age()`** (1 replacement):
  ```r
  age = fcase(
    age < 0, NA_real_,
    age >= 0 & age <= 110, age,
    age > 110, NA_real_,
    default = NA_real_
  )
  ```

  Keep the `collapse::fmutate()` and `collapse::ftransform()` wrappers
  — they work fine with `fcase()` inside.

  Leave all commented-out `dplyr::case_when` lines (educat4, educat5,
  educat7) as-is per R6.

- **Test Scenarios**:
  - ✅ All tests from Step 1 still pass
  - 🛑 Type consistency: `fcase` returns same types as `case_when`
- **Tests**: Re-run Step 1 test suite — all must pass unchanged.
- **Acceptance criteria**: Zero test regressions. No `dplyr::` calls
  remain in active (non-commented) code.

### 4. Migrate `pipdata_validate_gmd.R`

- **Requirements**: R3, R5
- **Files**: `R/pipdata_validate_gmd.R`
- **Details**: Two locations to update.

  **Line ~246** — merge new_inv list into final_inv:
  ```r
  # Before
  final_inv <- dplyr::bind_rows(new_inv) |>
    pipload::survey_id_to_vars() |>
    tidyr::as_tibble() |>
    data.table::as.data.table()

  # After
  final_inv <- data.table::rbindlist(new_inv, fill = TRUE) |>
    pipload::survey_id_to_vars()
  ```
  The `tidyr::as_tibble() |> data.table::as.data.table()` round-trip is
  unnecessary — `rbindlist` already returns a data.table, and
  `survey_id_to_vars()` preserves data.table class. Remove both
  conversions.

  **Line ~341** — append validation report:
  ```r
  # Before
  valid_report <- old_valid_report |> dplyr::bind_rows(valid_report)

  # After
  valid_report <- data.table::rbindlist(
    list(old_valid_report, valid_report),
    fill = TRUE
  )
  ```

- **Test Scenarios**:
  - ✅ `rbindlist(fill = TRUE)` handles mismatched columns same as
    `bind_rows`
  - 🛑 Edge case: `new_inv` contains NULL entries (from failed loads) —
    `rbindlist` skips NULLs, same as `bind_rows`
  - 🛑 Edge case: `old_valid_report` is NULL — guard already exists
    (`if (!is.null(old_valid_report))`)
- **Tests**: No new test needed — existing guard logic is unchanged.
  Verify with `devtools::check()`.
- **Acceptance criteria**: Zero `dplyr::` or `tidyr::` calls in active
  code. `devtools::check()` passes.

### 5. Final verification

- **Requirements**: R8
- **Files**: All R files
- **Details**:
  1. Run `grep -r "dplyr::\|tidyr::\|tibble::" R/` to confirm zero
     remaining active calls in Phase 1 files.
  2. Run `devtools::check()` to verify no NOTEs, WARNINGs, or ERRORs.
  3. Confirm `dlw_scan_and_validate.R` still has its dplyr calls (Phase 2
     scope).
- **Acceptance criteria**: Only `dlw_scan_and_validate.R` retains
  dplyr/tidyr/tibble calls. R CMD check passes cleanly.

## Testing Strategy

- **Pre-migration tests** (Step 1): Write regression tests for recode
  helpers against current dplyr code. Establishes the behavioral contract.
- **Post-migration verification** (Steps 3–4): Same tests must pass with
  zero changes — proves equivalence.
- **R CMD check** (Step 5): Catches namespace issues, missing imports,
  type mismatches.
- **Test data**: Small, self-contained data.tables built in test
  fixtures. No external data dependencies.

## Documentation Checklist

- [ ] No roxygen changes needed (internal function signatures unchanged)
- [ ] Inline comments updated where `case_when` → `fcase` if the old
      comment referenced dplyr
- [ ] No README changes needed

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| `fcase` NA handling differs from `case_when` | Pre-migration tests catch any difference; `default = NA_*` makes behavior explicit |
| `rbindlist` column-type coercion differs from `bind_rows` | `fill = TRUE` handles missing columns; types already consistent (data.table pipeline) |
| Downstream code expects tibble from `pipdata_validate_gmd` | Checked: `survey_id_to_vars()` and subsequent code use data.table syntax; no tibble assumption |

## Out of Scope

- `dlw_scan_and_validate.R` (Phase 2 — separate roadmap item)
- Commented-out `dplyr::case_when` lines in `pd_dlw_clean.R`
- Removing dplyr from renv.lock (it may be used by other packages)
- Refactoring loop structures (separate roadmap item: loop-to-apply)
