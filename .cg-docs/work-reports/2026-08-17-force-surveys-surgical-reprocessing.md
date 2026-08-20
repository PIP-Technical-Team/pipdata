# Execution Report: force-surveys-surgical-reprocessing

## Plan reference

`.cg-docs/plans/2026-08-17-force-surveys-surgical-reprocessing.md`

## Run 2026-08-17

### Active deviation policy

- Stored policy: `ask`
- Runtime override: none

### Completed steps/phases

- Phase 1: Core implementation (steps 1-3) — completed 2026-08-17
- Phase 2: Tests and documentation (steps 4-6) — completed 2026-08-17

### Deviations

None.

### Accepted exceptions

None.

### Evidence table

| ID | Evidence Required | Status | Artifact |
|----|-------------------|--------|----------|
| V1 | Forced survey already-cleaned retained in candidate set | passed | `test-valid_dlw_load.R` |
| V2 | Forced + normal candidates union and dedup via unique() | passed | `test-valid_dlw_load.R` |
| V3 | force=TRUE + force_surveys -> cli_abort(class="piperr") | passed | `test-pd_process_data.R` |
| V4 | pip_id input reverse-mapped to survey_id | passed | `test-valid_dlw_load.R` |
| V5 | Unknown identifier -> warn + log + skip, no abort | passed | `test-valid_dlw_load.R` |
| V6 | Forced-only run does NOT abort nothing-to-clean | passed | `test-valid_dlw_load.R` |
| V7 | Stamp versioning stays on content w/ force_surveys | passed | `test-pd_process_data.R` |
| V8 | force_surveys_inf / force_surveys_unknown_inf logged | passed | `test-valid_dlw_load.R` |
| V9 | Forced survey outside module filter excluded | passed | `test-valid_dlw_load.R` |
| V10 | No duplicate survey_ids or .joyn columns | passed | `test-valid_dlw_load.R` |
| V11 | force=TRUE behavior unchanged | passed | Regression test in `test-valid_dlw_load.R` |
| V12 | Direct-call guard aborts with class piperr | passed | `test-valid_dlw_load.R` |
| V13 | Duplicate force_surveys deduplicated; n_forced=1 | passed | `test-valid_dlw_load.R` |
| V14 | Non-character force_surveys aborts class piperr | passed | `test-valid_dlw_load.R` |
| V15 | dt_master lacks pip_id column -> warn + unknown | passed | `test-valid_dlw_load.R` |
| V16 | dt_master NULL + pip_id-like input -> warn | passed | `test-valid_dlw_load.R` |
| V17 | Targeted tests pass | passed | `devtools::test(filter="valid_dlw_load")` and `test-pd_process_data.R` (92 + 5 tests) |
| V18 | Full suite passes | passed | `devtools::test()` — 554 pass, 0 fail, 2 skip |
| V19 | Roxygen docs updated; devtools::document() succeeds | passed | `devtools::document()` regenerated both `.Rd` files |

### Constraints check

| ID | Constraint | Check | Status |
|----|------------|-------|--------|
| C1 | force_surveys never calls stamp::st_opts() | Code review + st_opts counter test | passed |
| C2 | Forced surveys bypass inv_to_process() only | Test | passed |
| C3 | force=TRUE + force_surveys is hard error (piperr) | Test | passed |
| C4 | Identifier resolution is lookup-first | Code review | passed |
| C5 | pip_id reverse-map reuses already-loaded master | Call-count test | passed |
| C6 | No .joyn/.x/.y columns; unique() dedup | Output assertions | passed |
| C7 | Nothing-to-clean abort includes forced set | Test | passed |
| C8 | force=TRUE path not altered | Regression test | passed |
| C9 | Mutual-exclusivity guard in BOTH functions | Code review + direct-call test | passed |
| C10 | Non-character force_surveys aborts piperr | Type-validation test | passed |
| C11 | dt_master lacking pip_id column graceful | Defensive-column test | passed |

### Remaining uncertainty

- review:auto (8 data-risk agents) found no P0/P1. Notable P2 findings carried to `/cg-review mode:verify`:
  - non-unique `pip_id`→`survey_id` reverse-map is not asserted as a blocked-stop (silent wrong-survey risk on `force_surveys` pip_id inputs);
  - `test-pd_process_data.R` mutual-exclusivity test does not mock `stamp::st_opts` and does not assert guard-ordering.
  - P3: unused `verbose` param in `resolve_force_surveys()`; warnings not gated on `verbose`; quadratic `c()` growth in resolve loop; duplicated guard message literal.

### Final status

`completed`