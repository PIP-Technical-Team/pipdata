---
date: 2026-08-28
plan: ".cg-docs/plans/2026-08-27-executable-staged-invalidation-dag.md"
status: completed
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

## Run 2 - 2026-08-31

### Plan Reference

`.cg-docs/plans/2026-08-27-executable-staged-invalidation-dag.md`

### Active Deviation Policy

- Stored: `ask`
- Runtime override: none

### Requested Scope

- Resume and complete Phases 2 through 4.
- Review mode: `auto`.

### Completed Steps And Phases

- Phase 1 was completed in Run 1.
- Phase 2, Step 4: extracted the prepared clean stage core with cached-unit,
  exact receipt-set, and fail-closed accounting.
- Phase 2, Step 5: extracted the prepared metadata stage core with exact clean
  prerequisites, reconstruction rules, blocked units, and checkpoint batches.
- Phase 2, Step 6: split deflation into standalone preparation and shared
  prepared execution paths.
- Phase 2, Step 7: added stage-result schema v2 and the typed pipeline run
  aggregate.
- Phase 2 completed at `2026-09-01T01:07:36Z`.
- Phase 3, Step 8: implemented one post-lease authoritative stage-wave
  executor with clean, metadata, and deflate replanning under one run ID and
  writer lease.
- Phase 3, Step 9: exported the frozen `pd_run_pipeline()` API with strict
  preflight, additive force, and dependency-closed bootstrap semantics.
- Phase 3, Step 10: completed fail-closed unit accounting and the full
  crash/restart matrix, including inventory-ahead convergence.
- Phase 3, Step 11: integrated one compact `pipeline_run_summary_inf`, latest
  run reporting, and dependency disposition/reason summaries.
- Phase 3 completed at `2026-09-01T02:30:18Z`.
- Phase 4, Step 12: added exact Colombia 2018 and table-driven invalidation,
  force, convergence, tombstone, and non-effect scenarios.
- Phase 4, Step 13: completed exact API compatibility, logging-retention, and
  fixed 1,250/2,500-entity operation-count audits.
- Phase 4, Step 14: updated package and operational documentation, regenerated
  Rd/NAMESPACE, and completed targeted, full-suite, package-check, and scope
  verification.
- Phase 4 and the full plan completed at `2026-09-01T05:47:33Z`.

### Deviations

- None. The merged and deleted Phase 1 branch was replaced with the follow-up
  branch `feat/executable-staged-invalidation-dag-phase2-4` from current
  `PROD` so the requested commit and pull-request workflow remains isolated.

### Accepted Exceptions

- None.

### Evidence

- Phase 1 evidence from Run 1 remains valid.
- V4: passed by clean compatibility, multi-output, and prepared-core tests.
- V5: passed by metadata exact-prerequisite and prepared-core tests.
- V6: passed by stage-result v2, pipeline aggregate, and recursive portable
  projection tests.
- Phase 2 targeted gate: 288 passed, 0 failed, 0 warnings, 0 skipped.
- Phase 2 full suite: 1,920 passed, 0 failed, 354 existing warnings, 2 existing
  skips.
- V7-V11: passed by ordered wave, additive force, failure accounting,
  crash/restart, and logging/report tests.
- V20: passed by post-lease survey-removal and selector re-resolution tests.
- V21: passed by stage-result v1/v2 and four-later-publication retained-evidence
  tests.
- Phase 3 targeted gate: 802 passed, 0 failed, 0 warnings, 0 skipped.
- Phase 3 full suite: 2,142 passed, 0 failed, 354 existing warnings, 2 existing
  skips.
- V12: passed by the exact Colombia 2018 CPI executor scenario.
- V13: passed by immediate no-op, force, receipt-set, tombstone, and exact
  non-effect scenarios.
- V14: passed by complete formals/defaults, export/S3, return, alias, and
  sentinel compatibility tests.
- V22: passed with `c1250 = 12500`, `c2500 = 25000`, fixed `C = 1`, identical
  one-per-alias bulk queries, and zero household or per-entity catalog reads.
- V15 expanded targeted gate: 1,792 passed, 0 failed, 0 warnings, 0 skipped.
- V16 full suite: 2,363 passed, 0 failed, 354 existing warnings, 2 existing
  skips.
- V17: `devtools::document()` passed; package check passed with 0 errors,
  0 warnings, and 5 notes. Three are baseline code notes. The additional
  note groups report existing `.kilo`/`.compound-gpid` paths and
  `sessionInfoLog`; no approved package-build configuration was changed to
  hide them.
- V18: `git diff --check`, protected-file, dependency/schema, prohibited
  framework, run-cursor, production-block, and artifact validation audits
  passed.

### Constraints Check

- Phase 2 constraints C6 and C7 passed through frozen-formal, return-contract,
  prepared-execution, and portable-result tests.
- Phase 3 constraints C8-C11 and C16-C18 passed through fault-injection,
  selector, fence, additive-force, and final-evidence tests.
- Phase 4 and final constraints C12-C15 and C19 passed through load counters,
  dependency/export diffs, bootstrap tests, documentation assertions, and the
  protected-charter audit.
- Mechanical self-review found no debug hooks, broken imports, unresolved
  TODO/FIXME/HACK/XXX markers, secrets, or hardcoded workspace paths.

### Remaining Uncertainty

- Target Windows/SMB fencing and immutable unique-rename evidence remain
  outside local verification and continue to block production activation.

### Final Status

`completed`
