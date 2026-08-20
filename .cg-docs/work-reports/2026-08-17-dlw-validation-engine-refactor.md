---
date: 2026-08-17
plan: ".cg-docs/plans/2026-08-17-dlw-validation-engine-refactor.md"
status: completed
completed-date: 2026-08-17
deviation-policy: ask
---

# Execution Report: DLW Validation Engine Refactor

## Plan Reference
`.cg-docs/plans/2026-08-17-dlw-validation-engine-refactor.md`

## Active Deviation Policy
- Stored: `ask`
- Runtime override: none

## Completed Steps/Phases

### Phase 1 (Steps 1-4): Faithful spec + engine (legacy functions intact)
- Step 1: Rewrote `inst/extdata/validation_spec.yml` with split selection
  semantics (`prefix:` for `startsWith` avail checks, `pattern:` for `grep`
  loops), per-check `severity` (only on `not_missing`/`na_threshold`),
  hhid/pid 3-entry gating with `condition:` fields, skip `severity: critical`,
  `na_threshold_min` on group/bin/hist only, aspire hhweight `pattern: "hhweight$"`.
- Step 2: Added `load_package_validation_spec()`, `validate_validation_spec()`,
  lazy memoized `dlw_validation_spec()` (cached in `.pipdataenv` to avoid
  locked-binding issues), `dlw_validation_spec_reset()`. Schema tests:
  12 tests.
- Step 3: Added `dlw_validation_engine(dlw_data, svy_id, module)` dispatching
  over 10 validation types. Reuses check helpers. Emits no per-survey
  `log_info()` (OOM constraint C1/R8).
- Step 4: Golden differential vs legacy + fixture capture. 49 engine tests.

### Phase 2 (Steps 5-8): Switch over, cleanup, docs
- Step 4.5: Captured 16 committed `.rds` fixtures from legacy functions
  (`tests/testthat/fixtures/validation_*.rds`, incl. 7 error fixtures + skip
  blank fixture) via `tests/testthat/generate-fixtures.R` (deterministic `seed`).
- Step 5: Rewired dispatch in `R/pipdata_validate_gmd.R`: `validation_functions`
  → `validation_modules` map (`GPWG = "gpwg"`, ... `DEFAULT = "skip"`);
  dispatch site calls `dlw_validation_engine(out, nm, module_id)`.
- Step 6: Converted all 8 legacy functions to thin deprecated wrappers around
  the engine. Removed dead code: `core_var` assignments, `is_var_endwith_avail`
  (function + doc + helper list), `is_uniq_cols` (unused after inline
  uniqueness handling). Retained `labelled::var_label(...) <- NULL` clearing in
  the engine (C8).
- Step 7: Replaced live differential with fixture-comparison tests +
  data-driven spec iteration test + `get_validation_report()`/
  `get_data_status()` report-format compatibility test. 162 engine tests pass.
- Step 8: `devtools::document()` regenerated `man/*.Rd` + NAMESPACE (exports
  `dlw_validation_engine`). Added NEWS.md entry. des codetools usage check clean.

## Deviations
- Engine uniqueness rows emit `is_uniq(...)` message/call format matching
  legacy byte-for-byte (initially `uniqueness(...)`; corrected in Step 7).
- `in_set(entry$valid_values)` deparsed to the variable reference; fixed via
  `bquote`/`eval` to inline literal `c(...)` so the message matches legacy.
- Fixture comparison compares on the deterministic subset
  (`table_name, description, num.violations, message, type` normalized via
  per-row collapse + sort), per plan Step 4.4 fallback, because `assertion.id`
  and `error_df` are non-deterministic.

## Accepted Exceptions
(none)

## Evidence Table
| ID | Phase | Status | Artifact |
|----|-------|--------|----------|
| V1 | 1 | passed | test-dlw_validation_spec.R (12 tests) |
| V2 | 1 | passed | test-dlw_validation_engine.R (162 tests) |
| V3 | 1 | passed | devtools::test(filter="dlw_validation") 174 pass |
| V4 | 2 | passed | R/pipdata_validate_gmd.R dispatch rewired; blank→invalid (skip blank fixture) |
| V5 | 2 | passed | grep confirms core_var/is_var_endwith_avail/emp_status gone; labelled::var_label retained |
| V6 | final | passed | fixture test: engine(input) == readRDS(fixtures/validation_<module>.rds) all 8 + errors |
| V7 | final | passed | full devtools::test(): 689 pass, 0 fail, 2 pre-existing skips |
| V8 | final | passed | devtools::document() regenerated man/Rd + NAMESPACE; codetools clean |

## Constraints Check
| ID | Phase | Status | Check |
|----|-------|--------|-------|
| C1 | 1 | passed | engine body contains no log_info |
| C2 | 1 | passed | table_name + description match legacy glue templates (fixture comparison) |
| C3 | 1 | passed | 8 data.validator report columns unchanged |
| C4 | 1 | passed | severity rejected on helper-fixed entries (schema test) |
| C5 | 1 | passed | variable_availability uses prefix; grep-regex for loops |
| C6 | 2 | passed | dispatch keys remain case-sensitive Module values |
| C7 | 2 | passed | only is_var_endwith_avail removed from check helpers |
| C8 | 2 | passed | labelled::var_label clearing retained in engine |
| C9 | 1 | passed | no new package dependencies (DESCRIPTION unchanged) |

## Remaining Uncertainty
- `labelled::var_label` side effect downstream: grep of `pd_dlw_clean.R`,
  `recode_spec.R`, `build_pip_inventory.R`, `pipdata_validate_gmd.R` shows no
  downstream label reads on numeric columns that depend on clearing. The
  clearing is retained to preserve byte-identical `dlw_data` state.

## Final Status
completed