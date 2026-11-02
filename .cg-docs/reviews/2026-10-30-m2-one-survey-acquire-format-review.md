---
date: 2026-10-30
title: "Code Review: M2 Phase 2 Steps 5 and 6 (One-Survey Acquire and Format-Validate)"
scope: "branch"
branch: "feat/m2-one-survey-acquire-format"
plan: ".cg-docs/plans/2026-10-30-m2-one-survey-acquire-format.md"
artifact-schema-version: 1
findings:
  - id: P1.1
    severity: P1
    status: fixed
    agent: cg-code-quality
    file: R/pipdata_get_gmd.R
    line: 1450
    summary: "Single-survey acquisition invoked inventory-wide validation instead of restricting validation candidates to the selected survey_id."
  - id: P1.2
    severity: P1
    status: fixed
    agent: cg-data-quality
    file: R/pd_process_data.R
    line: 230
    summary: "Clean eligibility guard permitted missing status or data_available columns or non-tabular inputs in process_data()."
  - id: P2.1
    severity: P2
    status: fixed
    agent: cg-testing
    file: tests/testthat/test-m2-one-survey-acquire-format.R
    line: 657
    summary: "Downstream-blocking test manually set scheduling_state := 'blocked' rather than exercising production pd_process_data() propagation."
  - id: P2.2
    severity: P2
    status: fixed
    agent: cg-reproducibility
    file: tests/testthat/test-m2-one-survey-acquire-format.R
    line: 47
    summary: "m2_step56_fixture() modified global stamp::st_opts() without restoring them on exit."
  - id: P2.3
    severity: P2
    status: fixed
    agent: cg-architecture
    file: R/pipdata_get_gmd.R
    line: 1379
    summary: ".acquire_and_validate_m2_one_survey() duplicated acquisition workflow logic instead of sharing .pipdata_get_gmd_core()."
  - id: P2.4
    severity: P2
    status: fixed
    agent: cg-data-quality
    file: tests/testthat/test-m2-one-survey-acquire-format.R
    line: 530
    summary: "Negative test coverage omitted negative, NA, and all-zero weight cases."
  - id: P2.5
    severity: P2
    status: fixed
    agent: cg-documentation
    file: R/pd_dlw_clean.R
    line: 620
    summary: "Internal roxygen documentation was missing on .acquire_and_validate_m2_one_survey(), pd_validate_clean_output(), .validate_clean_survey(), and process_data()."
  - id: P2.6
    severity: P2
    status: fixed
    agent: cg-adversarial
    file: R/pipdata_get_gmd.R
    line: 1432
    summary: "Single-survey acquisition reloaded the server catalog inside .pipdata_get_gmd_core() instead of passing the verified pinned catalog_row."
  - id: P3.1
    severity: P3
    status: fixed
    agent: cg-documentation
    file: R/pipdata_dlw_compare.R
    line: 1148
    summary: ".resolve_m2_one_survey_selection() lacked internal roxygen documentation."
  - id: P3.2
    severity: P3
    status: fixed
    agent: cg-version-control
    file: man/pd_process_data.Rd
    line: 65
    summary: "Generated Rd files (man/pd_process_data.Rd, man/pd_change_report.Rd, man/process_data.Rd) were out of sync with updated roxygen comments."
  - id: P3.3
    severity: P3
    status: fixed
    agent: cg-fix-problems
    file: tests/testthat/test-m2-one-survey-acquire-format.R
    line: 275
    summary: "Unused local variable assignment res <- stamp::st_save(...) in m2_step56_seed_aux()."
---

# Code Review: M2 Phase 2 Steps 5 and 6

## Summary
Reviewed uncommitted changes on `feat/m2-one-survey-acquire-format` across 9 review agents (`@cg-code-quality`, `@cg-testing`, `@cg-documentation`, `@cg-version-control`, `@cg-reproducibility`, `@cg-performance`, `@cg-architecture`, `@cg-data-quality`, `@cg-fix-problems`) plus `@cg-adversarial`. Found 0 P0, 2 P1, 6 P2, and 3 P3 issues. All 11 findings were resolved via `/cg-fix-triage P0 P1 P2 P3` and verified clean in `/cg-review mode:verify`.

## Findings & Resolutions

### P1 Findings
- **P1.1 (`R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`)**: Added `selected_survey_ids = NULL` to `.pipdata_validate_gmd_core()` and passed `selected_survey_ids = resolved$survey_id` from `.acquire_and_validate_m2_one_survey()`, plus a regression test with an unrelated prior available survey (`PRY`).
- **P1.2 (`R/pd_process_data.R`)**: Extracted `.assert_clean_eligible_inv_row(inv_row)` requiring a 1-row `data.frame` containing `survey_id`, `status == "valid"`, and `data_available == "Yes"`, shared by `pd_execute_clean()` and `process_data()`, and registered in `.PD_FINGERPRINT_COMPONENTS$clean`.

### P2 Findings
- **P2.1 (`tests/testthat/test-m2-one-survey-acquire-format.R`)**: Replaced manual `scheduling_state := "blocked"` assignment with a test calling production `pd_process_data()` and verifying it automatically marks downstream `metadata` actions `"blocked"` with `0` metadata worker calls.
- **P2.2 (`tests/testthat/test-m2-one-survey-acquire-format.R`)**: Saved and restored `stamp::st_opts()` via `withr::defer()` in `m2_step56_fixture()`.
- **P2.3 (`R/pipdata_get_gmd.R`)**: Extracted `.pipdata_get_gmd_core(..., selected_filenames = NULL)` so `pipdata_get_gmd()` and `.acquire_and_validate_m2_one_survey()` share a single acquisition core.
- **P2.4 (`tests/testthat/test-m2-one-survey-acquire-format.R`)**: Added explicit failure tests for negative, `NA`, and all-zero `weight` vectors and missing eligibility columns.
- **P2.5 (`R/pd_dlw_clean.R`, `R/pipdata_get_gmd.R`, `R/pd_process_data.R`)**: Added roxygen documentation to `.acquire_and_validate_m2_one_survey()`, `.validate_clean_survey()`, `pd_validate_clean_output()`, and updated `process_data()`.
- **P2.6 (`R/pipdata_get_gmd.R`)**: Added `server_catalog_override = NULL` to `.pipdata_get_gmd_core()` and passed `server_catalog_override = resolved$catalog_row` from `.acquire_and_validate_m2_one_survey()` so the server catalog is read once and pinned checksum/version checks are preserved through candidate selection and persistence.

### P3 Findings
- **P3.1 (`R/pipdata_dlw_compare.R`)**: Added internal roxygen documentation to `.resolve_m2_one_survey_selection()`.
- **P3.2 (`man/pd_process_data.Rd`, `man/pd_change_report.Rd`, `man/process_data.Rd`)**: Synced `.Rd` files with updated roxygen comments and verified with `tools::parse_Rd()`.
- **P3.3 (`tests/testthat/test-m2-one-survey-acquire-format.R`)**: Removed unused `res <-` assignment in `m2_step56_seed_aux()`.
