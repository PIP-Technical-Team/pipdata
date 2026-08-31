---
date: 2026-08-28
plan: ".cg-docs/plans/2026-08-27-executable-staged-invalidation-dag.md"
status: active
---

# Work Report: Executable Staged Invalidation DAG

## Run 1 - 2026-08-28

### Plan Reference

`.cg-docs/plans/2026-08-27-executable-staged-invalidation-dag.md`

### Active Deviation Policy

- Stored: `ask`
- Runtime override: none

### Completed Steps And Phases

- Phase 1, Step 1: unified read-only fact preparation and removal checks.
- Phase 1, Step 2: complete selected-node planning and cached-node guards.
- Phase 1, Step 3: named input provenance, exact reasons, and receipt-set canonicalization.
- Phase 1 completed at `2026-08-28T14:43:10Z`.

### Deviations

- None.

### Accepted Exceptions

- None.

### Evidence

| ID | Phase | Status | Artifact |
| --- | ---: | --- | --- |
| V1 | 1 | passed | `test-pd-change-report.R`; `test-dependency-execution.R` |
| V2 | 1 | passed | `test-dependency-plan.R`; `test-pipeline-context.R`; action-consumer regressions |
| V3 | 1 | passed | `test-dependency-inputs.R`; `test-dependency-manifest.R`; `test-code-fingerprint.R` |
| V19 | 1 | passed | Legacy/C4 comparison tests in `test-dependency-manifest.R` |
| V4-V6 | 2 | pending | Phase 2 targeted tests |
| V7-V11, V20-V21 | 3 | pending | Phase 3 targeted tests |
| V12-V14, V22 | 4 | pending | Phase 4 targeted tests |
| V15-V18 | final | pending | Final verification gates |

### Constraints Check

- Phase 1 constraints C1-C5 and C19: passed by targeted tests and the full package test gate.
- Remaining constraints: pending later phases and final verification.

### Verification Runs

- Phase 1 targeted suites: passed with no remaining failures.
- Full package tests: passed; 354 existing tidyselect lifecycle warnings, no failures.
- `git diff --check`: passed; line-ending notices only.

### Remaining Uncertainty

- Target Windows/SMB fencing and immutable unique-rename evidence remain outside local verification and continue to block production activation.

### Final Status

`active`
