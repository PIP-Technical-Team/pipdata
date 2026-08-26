---
date: 2026-08-26
depth: light
parent-review: .cg-docs/reviews/2026-08-24-pipdata-staged-dependency-manifest-review.md
type: verification
findings:
  P0.1: fixed
  P0.2: fixed
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P2.1: fixed
  P2.2: fixed
---

# Review Report

**Review mode**: light (verification)
**Files reviewed**: 23
**Findings**: 7 (P0: 2, P1: 3, P2: 2, P3: 0)

The command-selected parent review is the newest eligible standard review with
fixed findings. It predates and is not the source of the C3 findings recorded
in `.cg-docs/work-reports/2026-08-26-pipeline-stage-interface.md`; therefore,
none of the findings below fall within its fixed-finding suppression scope.

## P0 - BLOCKING (immediate remediation required)

- **[P0.1]** [cg-code-quality] `R/pd_deflate_pipeline.R:152` - The accepted
  dependency-plan action is joined to the mutable inventory by `pip_id` only,
  allowing same-named inventory columns to replace the plan's exact input
  versions and hashes; `nomatch = 0L` also silently drops accepted actions.
  **Why**: Execution can consume inputs other than those accepted by the
  dependency plan or omit planned work, violating the exact-input and
  fail-closed integrity boundary.
  **Fix**: Preserve plan fields as authoritative, validate a one-to-one match,
  and compare inventory values explicitly before execution.

- **[P0.2]** [cg-code-quality] `R/pd_deflate_pipeline.R:422-443` - Any error
  whose class does not match the hard-coded fatal prefixes is normalized as
  `unknown_error` with `recoverable = TRUE`.
  **Why**: Fence, lease, write, or integrity failures from dependencies can
  have unanticipated classes and then be treated as survey-scoped failures,
  allowing later writes after shared-state integrity is uncertain.
  **Fix**: Permit continuation only for an explicit allowlist of known
  survey-domain failures; fail closed for unknown errors.

## P1 - CRITICAL (must fix before merge)

- **[P1.1]** [cg-code-quality] `R/pd_deflate_pipeline.R:160-223` - A terminal
  checkpoint failure leaves successful-but-uncommitted `pending_ids` absent
  from `units`, and actions not yet iterated are also absent.
  **Why**: Counts, status, reason codes, and audit output do not represent all
  selected units after a terminal failure, so the typed result can understate
  affected work and cannot distinguish uncommitted from unattempted units.
  **Fix**: Materialize pending units as `checkpoint_uncommitted` and remaining
  selected units as skipped with a terminal reason before constructing the
  result.

- **[P1.2]** [cg-code-quality] `R/pipeline_stage_result.R:268-319` - Result
  validation does not validate each warning/error record, `log_ref`,
  provenance, or consistency of artifacts and hashes with units; portable
  conversion formats only selected timestamps and does not recursively
  validate/canonicalize nested content.
  **Why**: Malformed, non-portable, or contradictory nested state can pass the
  advertised fail-closed schema and produce unstable RDS projections.
  **Fix**: Freeze and validate each nested schema and recursively canonicalize
  all portable fields before serialization.

- **[P1.3]** [cg-testing] `tests/testthat/test-pd-deflate-pipeline.R:75-93` -
  The top-level pipeline test mocks `pd_deflate_pipeline_core()` itself, and no
  focused test invokes the real core through checkpoint success, recoverable
  failure, or terminal failure paths.
  **Why**: The focused suite passes while the P0 exact-input and fatal-error
  paths and P1 terminal accounting behavior remain untested.
  **Fix**: Exercise the real shared core with only external I/O boundaries
  mocked, including unmatched/changed inventory input, unknown errors, and
  failed checkpoints with pending and unattempted units.

## P2 - IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality] `R/pd_deflate_pipeline.R:201-205` - Successful
  unit timing uses the stage-wide `started_at` rather than each unit's worker
  start time.
  **Why**: Per-unit durations become cumulative and are not meaningful for
  diagnostics or future scheduling.
  **Fix**: Retain the worker's start timestamp with each pending receipt and
  use it when the checkpoint commits.

- **[P2.2]** [cg-testing] `tests/testthat/test-pipeline-stage-result.R:40-54` -
  Portable determinism is tested with one row and only `pd_portable_table()`,
  not the complete stage-result projection with reordered nested conditions,
  provenance, artifacts, and hashes.
  **Why**: The test cannot detect order-sensitive or non-portable nested
  projections promised by the contract.
  **Fix**: Serialize complete semantically equivalent results with deliberately
  reordered nested content and assert identical version-3 bytes.

## P3 - MINOR (nice to have)

No P3 findings.

## Passed

- [cg-code-quality]: Changed R files parse successfully; `.Rbuildignore`
  excludes `.cg-docs/`; `git diff --check` reported no whitespace errors.
- [cg-testing]: Focused pipeline context, stage result, and deflation pipeline
  tests passed (29 expectations, 0 failures).

## Verification Context

Parent fixed findings checked under the required suppression policy: P0.1,
P0.2, P0.3, P1.1, P1.2, P1.3, and P1.4 from the parent review. P0/P1 findings
were never suppressed. Cross-file contract failures were never suppressed.

Parsed 7 finding IDs. The parsed count matches the total findings above.
