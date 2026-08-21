---
date: 2026-08-20
plan: ".cg-docs/plans/2026-08-20-explicit-data-level-sentinel-semantics.md"
status: completed
---

# Work Report — Explicit data_level sentinel semantics

## Plan Reference

`.cg-docs/plans/2026-08-20-explicit-data-level-sentinel-semantics.md`

## Active Deviation Policy

- Stored policy: `ask`
- Runtime override: none provided

## Run 1 (2026-08-20)

- Roadmap feature `explicit-data-level-semantics` set to `active` (was `planned`).
- Artifact validation preflight (`cg-render-artifact --validate-only`) passed after corrective edits to the plan body:
  - Step 3 `Requirements` mapping changed from `R4, R7, R9 (no changes to ...)` to `R4, R7, R9` with the note moved into its own bullet.
  - Step 6 `Requirements` changed from `R10, V2, V3, V4, V5` to `R10` (V-IDs are verification surface IDs, not requirement IDs).

## Completed Steps / Phases

- **Phase 1 (2026-08-20)**:
  1. Added `.data_level_columns` registry + `data_level_column()` resolver to `R/aaa.R` (after `.log_internal_types`, ~line 90). Red-phase confirmed: contract test failed with `object 'data_level_column' not found` before implementation; passed after.
  2. Rewrote `adj_pop` guard in `.deflation_pipmd_core` (`R/pd_deflation.R:397`) to `!is.na(data_level_column(attr(dt_c, "pop_data_level")))`.
  3. Rewrote `add_ppp()` pointer branch (`R/pd_deflation.R:636`) to use `data_level_column()`, hoisting `ppp_col` out of the version loop; abort keyed off resolved column name.
  4. Rewrote `add_cpi()` pointer branch (`R/pd_deflation.R:710`) to use `data_level_column()`, hoisting `cpi_col` out of the year loop; abort keyed off resolved column name.
- **Phase 2 (2026-08-20)**:
  5. Added `data_level_column()` contract test block to `tests/testthat/test-pd-deflation.R` (after fixture helpers).
  6. Verified: deflation family (`test-pd-deflation.R` 71 tests, `test-adjust-population.R` 7 tests) green; full suite 316 test blocks, 0 failures, 0 warnings, 0 skips.

## Deviations

- None.

## Accepted Exceptions

- None.

## Evidence Table

| ID | Evidence Required | Status | Artifact |
|----|-------------------|--------|----------|
| V1 | data_level_column contract (area/national/NULL/character(0)/NA) | passed | test-pd-deflation.R contract test (executed, green) |
| V2 | Full test suite passes, zero regressions | passed | full suite 316 blocks / 0 fail / 0 warn / 0 skip |
| V3 | Subnational add_ppp/add_cpi per-row lookup values | passed | test-pd-deflation.R "resolves subnational via area column" (:375, :426) (executed, green) |
| V4 | adj_pop guard fires subnational / not national | passed | test-pd-deflation.R "adj_pop = TRUE/FALSE" (:699, :723) (executed, green) |
| V5 | add_dom_vars(), pd_aux_attr(), adjust_population() untouched | passed | git diff: pd_cpfw_merge.R/pd_aux_attr.R empty; pd_deflation.R diff limited to 3 consumer regions |

## Constraints Check

| ID | Constraint | Status |
|----|------------|--------|
| C1 | attr value stays the string "area" | passed — test fixtures unchanged, all pass |
| C2 | no @export, NAMESPACE unchanged | passed — NAMESPACE diff empty |
| C3 | "area column absent" aborts fire | passed — abort tests (:380, :430) green, keyed off resolved column name |
| C4 | only 3 consumer sites change | passed — grep: no `identical(*_lvl, "area")` remains; diff limited to :397, :636, :710 |

## Remaining Uncertainty

- None material. Pre-existing tidyselect deprecation warnings in `test-dlw_validation_engine.R` are unrelated to this change (present before, counted as 0 test warnings).

## Final Status

completed
