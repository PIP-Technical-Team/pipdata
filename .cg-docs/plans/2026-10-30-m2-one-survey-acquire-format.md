---
date: 2026-10-30
title: "M2 Phase 2 Steps 5 and 6: One-Survey Acquire and Format-Validate"
status: completed
scope: "Standard"
brainstorm: null
language: "R"
estimated-effort: "medium"
deviation-policy: "autonomous"
artifact-schema-version: 1
phases: 2
execution-report: ".cg-docs/work-reports/2026-10-30-m2-one-survey-acquire-format.md"
failing-steps: []
completed-phases: [1, 2]
tags: [m2, pipdata, dlw, acquisition, validation, clean, stamp]
---

# Plan: M2 Phase 2 Steps 5 and 6: One-Survey Acquire and Format-Validate

## Objective
Implement M2 Phase 2 Steps 5 (`download-one-survey`) and 6 (`format-and-validate-one-survey`) in `pipdata` so an explicit single-survey module-bearing selection can be acquired and recorded in durable inventories, `status == "invalid"` completed-validation rows are excluded before entering the clean stage, and active post-format validation enforces LCU/day welfare, finite non-missing `welfare` and `weight`, and required `pipmd`/`pipgd` classes and domain attributes before any `save_pip()` or `stamp` write occurs.

## Context
- **Charter & Architecture**: Aligns with `SYSTEM_DESIGN.md:133-139` (module-bearing artifact identity) and `SYSTEM_DESIGN.md:209-213` (boundary checks: invalid DLW downloads do not proceed; formatted data failing validation are never saved).
- **Inherited Contradiction (`notes/pipdata.md:95`)**: `.filter_completed_dlw_validation_inventory()` (`R/dependency_execution.R:214-231`) currently retains both `status == "valid"` and `status == "invalid"` rows from `.normalize_dlw_validation_inventory()`, allowing invalid surveys to enter `pd_snapshot_current()`, `pd_dependency_plan()`, and `pd_execute_clean()`. Furthermore, post-clean validation before saving in `R/pd_process_data.R:225-240,330-342` is commented out or absent.
- **Dependency Pins**: `stamp` at `4a1fa5d` (content `5d1ddebdc926dad9c6ec99c08a65e74baee0da92`) and `pipload` adapter at `4ef0d90` in the `E:/PovcalNet/01.personal/wb384996/PIP/pipdata-m2-engine-runtime/library/phase2` overlay.

## Requirements
| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Accept an explicit single-survey module-bearing selection (`country_code`, `surveyid_year`, `survey_acronym`, `welfare_type`, `module`, plus pinned DLW `FileName`, `Checksum`, `Vermast`, `Veralt`), execute bounded single-survey acquisition and validation, and record download availability and validation status in `dlw_gmd_inv` and `gmd_valid_inv` so failed, unavailable, or mismatched-checksum downloads cannot enter the clean/format path. | Umbrella Step 5 (E5 / V8) |
| R2 | Exclude `status == "invalid"` completed-validation rows from entering the clean stage (`.filter_completed_dlw_validation_inventory()` and clean-stage execution boundary) while preserving `gmd_valid_inv` durable storage of invalid rows. | Umbrella Step 6 (E6 / V8, `notes/pipdata.md:95`) |
| R3 | Enforce active post-format validation before `save_pip_data()` and `pd_save_receipt()` (checking `data.table`, `pipmd` or `pipgd` class, finite non-missing numeric `welfare` and `weight` with LCU/day conversion `welfare / 365`, and valid scalar survey/domain attributes) and include the validator and any helper in `.PD_FINGERPRINT_COMPONENTS$clean`. | Umbrella Step 6 (E6 / V8 & V9) |
| R4 | Verify with a focused `testthat` suite in an isolated `E:` temporary storage root (no live DLW, `Y:`, or `C:` writes) that valid one-survey data saves with verified `stamp` receipts and metadata, `status == "invalid"` rows produce `0` clean saves/versions, and post-format validation failures abort/record recoverable failure before `save_pip()` with `0` clean versions and blocked downstream stages. | Check L-acquire + L-format / V8 |

## Implementation Steps

## Phase 1: Single-Survey Acquisition Selection and Invalid-Row Clean Gate

### 1. Add Explicit Single-Survey Acquisition/Validation Seam and Gate Invalid Validation Rows
- **Requirements**: R1, R2
- **Files**: `R/pipdata_dlw_compare.R`, `R/pipdata_get_gmd.R`, `R/dependency_execution.R`, `R/pd_process_data.R`, `R/pd_change_report.R`, `tests/testthat/test-dependency-execution.R`, `tests/testthat/test-pd_process_data.R`
- **Details**:
  - **Identifier contract (resolves P1.1)**: Preserve DLW source `survey_id = .dlw_survey_id(FileName)` (e.g., `"BOL_2020_EH_V01_M_V01_A_GMD_GPWG"`) and verify its parsed fields (`pipload::survey_id_to_vars()`) against the explicit selection (`country_code`, `surveyid_year`, `survey_acronym`, `module`, `Vermast`, `Veralt`) and pinned catalog row (`FileName`, `Checksum`). Derive the expected clean artifact `pip_id` separately using the existing PFW mapping and `INC`/`CON` convention (`<COUNTRY>_<YEAR>_<ACRONYM>_<INC|CON>_<MODULE>`). Never overwrite source `survey_id` with clean `pip_id`.
  - **Executable single-survey acquisition and validation flow (resolves P1.2)**: Keep all exported signatures unchanged. Add internal `.resolve_m2_one_survey_selection(selection, server_catalog)` in `R/pipdata_dlw_compare.R` and `.acquire_and_validate_m2_one_survey(selection, inv_gmd_list = "dlw_gmd_inv", check_missing = TRUE, verbose = FALSE)` in `R/pipdata_get_gmd.R`:
    1. Resolve exactly one matching pinned row in the server catalog (rejecting missing, duplicate, or checksum/version/module mismatches before download).
    2. Narrow the server catalog passed to `.select_dlw_acquisition_candidates()` to that single selected row so unrelated server catalog rows are not downloaded, while preserving existing unrelated prior rows in `dlw_gmd_inv` during merge/persistence.
    3. Permit an unresolved row (`data_available == "No"` or absent in prior `dlw_gmd_inv`) to be downloaded via `.acquire_one_gmd()`, persist the updated `dlw_gmd_inv` via `.persist_dlw_acquisition_inventory()`, and gate on post-attempt `data_available == "Yes"` and verified checksum.
    4. When acquisition succeeds (`data_available == "Yes"`), invoke `.validate_one_gmd()` and persist the resulting `valid` or `invalid` row to `gmd_valid_inv` (`alias = "dlw_meta"`). If download fails or `data_available == "No"`, record the acquisition failure, skip validation/clean, and return the structured outcome.
  - **Invalid-row exclusion at clean boundary**: In `R/dependency_execution.R`, update `.filter_completed_dlw_validation_inventory()` to filter normalized completed rows to `completed[status == "valid" & data_available == "Yes"]` (excluding `status == "invalid"` rows while keeping durable `gmd_valid_inv` storage intact). Add a defense-in-depth guard in `pd_execute_clean()` (`R/pd_process_data.R`) and `process_data()` rejecting any `inv_row` where `!"status" %in% names(inv_row)` or `inv_row$status[[1L]] != "valid"` or `inv_row$data_available[[1L]] != "Yes"` before `inv_dlw_load()`.
  - Update existing unit tests in `tests/testthat/test-dependency-execution.R` and `tests/testthat/test-pd_process_data.R` that previously expected `status == "invalid"` rows to remain after `.filter_completed_dlw_validation_inventory()`.
- **Test Scenarios**: Multi-row server catalog where only the explicitly selected survey is downloaded; mismatched checksum or download failure records `data_available = "No"` and never enters validation or clean; `.filter_completed_dlw_validation_inventory()` excludes `status == "invalid"` and retry rows while rejecting malformed rows.
- **Tests**: `tests/testthat/test-dependency-execution.R`, `tests/testthat/test-pd_process_data.R`, `tests/testthat/test-m2-one-survey-acquire-format.R`
- **Acceptance criteria**: `.filter_completed_dlw_validation_inventory()` returns only `status == "valid" & data_available == "Yes"` rows; invalid/unavailable/mismatched-checksum surveys produce `0` planned or executed clean actions.

## Phase 2: Active Post-Format Validation Before Save and End-to-End Verification

### 2. Enforce Active Post-Format Validation Before Clean Save and Verify L-acquire + L-format
- **Requirements**: R3, R4
- **Files**: `R/pd_dlw_clean.R`, `R/pd_process_data.R`, `R/pipeline_stage_cores.R`, `R/code_fingerprint.R`, `tests/testthat/test-code-fingerprint.R`, `tests/testthat/test-m2-one-survey-acquire-format.R`
- **Details**:
  - **Post-format validator contract (resolves P2.1 & P2.2)**: Implement `pd_validate_clean_output(clean)` and `.validate_clean_survey(dt, pip_id = NULL)` in `R/pd_dlw_clean.R`:
    1. `clean` must be a non-empty named list of `data.table` objects where each table has `nrow(dt) > 0L` and inherits from `"pipmd"` or `"pipgd"`.
    2. Columns `welfare` and `weight` must exist, be `double` (`is.double()`), contain zero `NA`/`NaN`/`Inf` values (`all(is.finite(dt$welfare))` and `all(is.finite(dt$weight))`), and satisfy `all(dt$weight >= 0)` and `sum(dt$weight) > 0`. Also check in `format_wlf()` / `format_wgt()` that non-finite or all-NA raw `welfare`/`weight` values abort with `c("pipdata_clean_validation_error", "piperr")` before `wbpip:::md_clean_data()` can silently drop rows.
    3. Identity and domain attributes must be scalar, non-missing, and validly typed: `survey_id`, `country_code`, `survey_acronym`, `welfare_type` (`"income"` or `"consumption"`), `reporting_level` (`"1"` or `"2"`), `distribution_type` (`"micro"`, `"imputed"`, `"group"`, or `"aggregate"`) as non-empty character scalars; `surveyid_year` as a finite whole number; `ppp_data_level`, `cpi_data_level`, `pop_data_level`, `gdp_data_level`, `pce_data_level` each in `c("national", "area")`; and `aux_data_levels` in `c("same", "different")`.
    4. When any of `ppp_data_level`, `cpi_data_level`, `pop_data_level`, `gdp_data_level`, or `pce_data_level` equals `"area"`, column `area` must exist in `dt`, be character/factor, and contain non-missing, non-empty values (`all(!is.na(area) & nzchar(trimws(as.character(area))))`). For national-only surveys (all data levels `"national"`), blank `area` values (`""`) remain acceptable.
    5. On any violation, abort with `rlang::abort(..., class = c("pipdata_clean_validation_error", "piperr"))`.
  - Add `"pipdata_clean_validation_error"` to `.PD_CLEAN_RECOVERABLE_CLASSES` in `R/pipeline_stage_cores.R`.
  - Call `pd_validate_clean_output(clean)` in `pd_execute_clean()` (before `pd_aux_attr()` and `pd_save_receipt()`) and in `process_data()` (before `pd_aux_attr()` and `save_pip_data()`).
  - Register **both** `"pd_validate_clean_output"` and `".validate_clean_survey"` in `.PD_FINGERPRINT_COMPONENTS$clean` in `R/code_fingerprint.R`, and add a test in `tests/testthat/test-code-fingerprint.R` verifying that mutating `.validate_clean_survey` invalidates only `"clean"`.
  - Create `tests/testthat/test-m2-one-survey-acquire-format.R` testing all required scenarios (including both national and subnational `"area"` domain fixtures and malformed domain rejection) in an isolated `E:` temp directory.
- **Test Scenarios**: Valid module-bearing microdata fixture acquired, formatted (`welfare / 365`), validated (both national and subnational `"area"` domain cases), and saved to `stamp` with verified receipt and metadata; `status == "invalid"` row in `gmd_valid_inv` excluded before clean (`0` save calls, `0` versions); non-finite/missing `welfare` or `weight` or malformed domain attribute/blank subnational `area` aborts before `save_pip()` (`0` versions in `stamp` catalog) and blocks downstream `metadata`/`deflate` stages.
- **Tests**: `tests/testthat/test-m2-one-survey-acquire-format.R`, `tests/testthat/test-code-fingerprint.R`, `tests/testthat/test-dependency-api-contract.R`
- **Acceptance criteria**: All tests pass with zero `C:` writes, zero live DLW/`Y:` calls, `0` clean-save calls or `stamp` versions on invalid/failed-format surveys, and verified `stamp` receipt/metadata on the valid survey.

## Testing Strategy
Run this exact PowerShell / `Rscript` command (resolves P2.3: redirects `TEMP`, `TMP`, `TMPDIR` to `E:` before starting R, prepends the Phase 2 dependency overlay containing `stamp` 0.1.0 at `4a1fa5d` and `pipload` at `4ef0d90`, loads the worktree `pipdata` source and test helpers via `pkgload::load_all()`, and fails on any test failure):

```powershell
$tmp = "E:/PovcalNet/01.personal/wb384996/PIP/pipdata/.tmp"
New-Item -ItemType Directory -Force -Path $tmp | Out-Null
$env:TMP = $tmp; $env:TEMP = $tmp; $env:TMPDIR = $tmp
& "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe" -e "
  stopifnot(startsWith(normalizePath(tempdir(), winslash = '/'), 'E:/'))
  .libPaths(c('E:/PovcalNet/01.personal/wb384996/PIP/pipdata-m2-engine-runtime/library/phase2', .libPaths()))
  options(pipfun.log.auto = FALSE, pipfun.main_dir = file.path(tempdir(), 'pip-root'), pipdata.dependency_manifest_path = file.path(tempdir(), 'manifest'))
  pkgload::load_all('.', helpers = TRUE, quiet = TRUE)
  files <- c('tests/testthat/test-dependency-execution.R', 'tests/testthat/test-pd_process_data.R', 'tests/testthat/test-code-fingerprint.R', 'tests/testthat/test-dependency-api-contract.R', 'tests/testthat/test-m2-one-survey-acquire-format.R')
  for (f in files) testthat::test_file(f, stop_on_failure = TRUE)
"
```

## Documentation Checklist
- [x] Update `@param inv` and `@details` comments in `R/pd_process_data.R` and `R/pd_change_report.R` to reflect that only `status == "valid"` completed validation rows enter cleaning.
- [x] Keep exported function signatures and `NAMESPACE` unchanged so `test-dependency-api-contract.R` passes.

## Risks & Mitigations
| Risk | Impact | Mitigation |
|------|--------|------------|
| Existing tests in `test-dependency-execution.R` and `test-pd_process_data.R` use a 2-row fixture where row 2 has `status = "invalid"` and expect 2 rows after `.filter_completed_dlw_validation_inventory()` | High — breaks existing unit tests | Update those specific test expectations to assert that the `status == "invalid"` row is excluded (`nrow == 1L`) and add dedicated tests for invalid-row exclusion. |
| Adding a new helper call inside `pd_execute_clean()` or `dlw_clean()` without registering all called helpers in `.PD_FINGERPRINT_COMPONENTS$clean` breaks `pd_fingerprint_audit()` | High — fails `test-code-fingerprint.R` | Register both `pd_validate_clean_output` and `.validate_clean_survey` in `.PD_FINGERPRINT_COMPONENTS$clean` and run `test-code-fingerprint.R`. |
| `wbpip:::md_clean_data()` drops rows with `NA` welfare or weight before post-format validation runs | Medium — could mask missing welfare/weight | Check finite non-missing `welfare` and `weight` in `format_wgt()` / `format_wlf()` before `wbpip_clean()` AND in `pd_validate_clean_output()` before `save_pip()`. |

## Out of Scope
- Building the full M3 multi-survey selector or GPWG/HIST/BIN preference rule (`SYSTEM_DESIGN.md:117-121`).
- Modifying `stamp` or `pipload` repositories or touching live DLW servers or `Y:`.
- Implementing M2 Phase 3 (deflation `welfare_YYYY` column rename and `pipster` mean calculation) or Phase 4 (test release manifest).

## Later
- Full M3 multi-survey selection table generation across competing survey acronyms and HIST vs BIN modules.
- Extending `inst/extdata/validation_spec.yml` with declarative post-format rules once M3 schema unification is scheduled.

## Completion Contract

### Outcome
`pipdata` acquires and validates a bounded one-survey module-bearing fixture on `E:`, excludes `status == "invalid"` validation inventory rows before clean planning/execution, and enforces active post-format validation before saving so invalid formatted data never reach `save_pip()` or create `stamp` versions while valid data convert welfare to LCU/day and persist with verified `stamp` receipts and metadata.

### Verification Surface
| ID | Phase | Evidence Required | Command/Artifact | Required |
|----|-------|-------------------|------------------|----------|
| V1 | 1 | `.filter_completed_dlw_validation_inventory()` excludes `status == "invalid"` rows and single-survey selection gates unavailable/mismatched downloads | `Rscript` running `test-dependency-execution.R` and `test-pd_process_data.R` | yes |
| V2 | 2 | Valid one-survey fixture converts `welfare / 365`, passes post-format validation, and saves to `stamp` with verified receipt and metadata; `status == "invalid"` and failed post-format surveys create `0` clean saves/versions and block downstream stages | `Rscript` running `test-m2-one-survey-acquire-format.R`, `test-code-fingerprint.R`, and `test-dependency-api-contract.R` | yes |
| V3 | final | Code review and verification pass (`review:auto`, `/cg-fix-triage`, `/cg-review mode:verify`) and handoff report finalized | `.cg-docs/work-reports/2026-10-30-m2-one-survey-acquire-format.md` and orchestration `handoff.md` | yes |

### Constraints
| ID | Phase | Constraint | Check |
|----|-------|------------|-------|
| C1 | 1 | Mandatory Storage Rule: zero writes to `C:`; all temp dirs and test stores on `E:` | Verify `tempdir()` starts with `E:/` and all test roots use `E:/PovcalNet/01.personal/wb384996/PIP/pipdata/.tmp` |
| C2 | 2 | Public API signatures and `NAMESPACE` exports remain unchanged | `test-dependency-api-contract.R` passes with 0 failures |
| C3 | final | No commits or pushes; leave changes uncommitted on `feat/m2-one-survey-acquire-format` | `git status` and `git rev-parse HEAD` match `7f4a98471d9c2350c3537fcae8448856896cd9f9` |

### Boundaries
- Allowed: `R/pipdata_get_gmd.R`, `R/pipdata_dlw_compare.R`, `R/pd_dlw_clean.R`, `R/pd_process_data.R`, `R/dependency_execution.R`, `R/pipeline_stage_cores.R`, `R/code_fingerprint.R`, `R/pd_change_report.R`, `R/pipdata_dlw_validation.R`, `R/save_pip.R`, `tests/testthat/`, `.cg-docs/`, and the orchestration handoff report on `E:`.
- Out of scope: Committing, pushing, merging, modifying `SYSTEM_DESIGN.md` Decided items, or accessing live DLW/`Y:`.

### Iteration Policy
1. Operate under `deviation-policy: autonomous` (`deviate:auto`), recording any justified tactical adjustments in the execution report.
2. Fix all `@cg-plan-critic` and `/cg-review` findings (P0-P3) within the 2-round correction budget.

### Blocked-Stop Conditions
- Any required verification check fails after allowed recovery attempts.
- Any operation requires writing to `C:` or accessing live DLW/`Y:`.
- A protected Decided item in `SYSTEM_DESIGN.md` would need to be contradicted.
