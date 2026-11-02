# Work Execution Report: M2 Phase 2 Steps 5 and 6

- **Plan reference**: `.cg-docs/plans/2026-10-30-m2-one-survey-acquire-format.md`
- **Active deviation policy**: `autonomous` (`deviate:auto`)
- **Final status**: `completed`

## Completed Steps / Phases
- [x] Phase 1: Single-Survey Acquisition Selection and Invalid-Row Clean Gate (Step 1)
- [x] Phase 2: Active Post-Format Validation Before Save and End-to-End Verification (Step 2)

## Deviations
1. **Shared acquisition/validation core restriction**: During `/cg-review` and `/cg-fix-triage`, `.pipdata_get_gmd_core()` was extracted with `selected_filenames` and `server_catalog_override` parameters and `.pipdata_validate_gmd_core()` gained `selected_survey_ids = NULL` so `.acquire_and_validate_m2_one_survey()` shares the canonical acquisition/validation workflows while strictly restricting download and validation workers to the selected survey and pinned catalog row.
2. **Shared row-level clean eligibility guard**: Extracted `.assert_clean_eligible_inv_row()` in `R/pd_process_data.R` (and registered in `.PD_FINGERPRINT_COMPONENTS$clean`) so both `pd_execute_clean()` and `process_data()` enforce the exact 1-row `status == "valid"` and `data_available == "Yes"` contract before `inv_dlw_load()`.
3. **Clean receipt join column hygiene**: In `pd_execute_clean()` (`R/pd_process_data.R`), dropped overlapping receipt column names (`content_hash`, `version_id`, `path`, `alias`, `artifact`, `skipped`, `success`, `error`) from `invariant` before `invariant[receipts, on = "pip_id"]` so the clean artifact's `content_hash` and `version_id` are never shadowed by DLW source columns.

## Accepted Exceptions
None.

## Evidence Table
| ID | Phase | Evidence Required | Command/Artifact | Required | Status |
|----|-------|-------------------|------------------|----------|--------|
| V1 | 1 | `.filter_completed_dlw_validation_inventory()` excludes `status == "invalid"` rows and single-survey selection gates unavailable/mismatched downloads | `Rscript` running `test-dependency-execution.R` (50 pass) and `test-pd_process_data.R` (31 pass) | yes | verified |
| V2 | 2 | Valid one-survey fixture converts `welfare / 365`, passes post-format validation, and saves to `stamp` with verified receipt and metadata; `status == "invalid"` and failed post-format surveys create `0` clean saves/versions and block downstream stages | `Rscript` running `test-m2-one-survey-acquire-format.R` (84 pass), `test-code-fingerprint.R` (37 pass), `test-dependency-api-contract.R` (56 pass), `test-dependency-inputs.R` (37 pass) | yes | verified |
| V3 | final | Code review and verification pass (`review:auto`, `/cg-fix-triage`, `/cg-review mode:verify`) and handoff report finalized | `.cg-docs/reviews/2026-10-30-m2-one-survey-acquire-format-review.md`, `.cg-docs/work-reports/2026-10-30-m2-one-survey-acquire-format.md`, and orchestration `handoff.md` | yes | verified |

## Constraints Check
| ID | Phase | Constraint | Check | Status |
|----|-------|------------|-------|--------|
| C1 | 1 | Mandatory Storage Rule: zero writes to `C:`; all temp dirs and test stores on `E:` | Verified `tempdir()` starts with `E:/` and all test roots use `E:/PovcalNet/01.personal/wb384996/PIP/pipdata/.tmp` | verified |
| C2 | 2 | Public API signatures and `NAMESPACE` exports remain unchanged | `test-dependency-api-contract.R` passes with 0 failures (56 pass) | verified |
| C3 | final | No commits or pushes; leave changes uncommitted on `feat/m2-one-survey-acquire-format` | `git status` and `git rev-parse HEAD` match `7f4a98471d9c2350c3537fcae8448856896cd9f9` | verified |

## Remaining Uncertainty
- Live DLW server and `Y:` drive access remain outside this isolated `E:` verification scope.
- M2 Phase 3 (`deflate` stage `welfare_YYYY` column rename and `pipster` mean calculation) and Phase 4 (test release manifest) remain scheduled for downstream steps in the umbrella walking skeleton.
