---
date: 2026-09-01
depth: light
parent-review: .cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-review.md
type: verification
findings:
  P0.1: fixed
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P2.1: fixed
---

# Verification Review Report

**Review mode**: light (`mode:verify`)
**Parent review**: `.cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-review.md`
**Files reviewed**: 75
**Findings**: 6 (P0: 1, P1: 4, P2: 1, P3: 0)

## P0 - Blocking

- **[P0.1]** `cg-code-quality` `R/pipeline_stage_cores.R:273` -
  `legacy_input_changed` is not a metadata reconstruction reason. A changed
  canonical-only clean input can reuse stale clean-derived metadata and publish
  it as current. **Fix**: Reconstruct from the exact committed clean receipt and
  frozen auxiliary snapshot; add a canonical-only migration test.

## P1 - Critical

- **[P1.1]** `cg-code-quality` `R/pd_deflate_pipeline.R:433` - Failed
  invalidation writes the release inventory and then fences against the old
  catalog without passing the new receipt. **Fix**: Revalidate and accumulate
  release/master receipts as `advanced_receipts` for subsequent fences; test
  with real Stamp storage.
- **[P1.2]** `cg-code-quality`, `cg-testing`
  `R/dependency_execution.R:383`, `R/dependency_execution.R:464` - Restart
  planning trusts old clean and metadata catalog receipts after durable master
  pointers are cleared. The failed node can be cached while its descendant
  terminates on a missing prerequisite. **Fix**: Require current receipts to
  match nonmissing durable inventory pointers, or persist a C2 failure marker;
  verify public recoverable-failure restarts for all stages.
- **[P1.3]** `cg-code-quality` `R/pd_metadata_refresh.R:3` - Metadata validation
  always requires CPI, PPP, and population, breaking valid legacy standalone
  `aux_measures` subsets. **Fix**: Require the canonical deflation set for the
  top-level pipeline but only requested measures for the legacy adapter; add an
  active public-path subset test.
- **[P1.4]** `cg-code-quality`, `cg-testing`
  `tests/testthat/test-pd-run-pipeline.R:1284` - The crash/restart matrix does
  not compare each recovered state with the independently uninterrupted final
  state. **Fix**: Canonicalize temporary roots and aliases, then compare exact
  receipt tuples, inventory versions, manifest records, inputs, fingerprints,
  tombstones, identity, and exact fault statuses/reasons.

## P2 - Important

- **[P2.1]** `cg-code-quality`, `cg-testing`
  `R/pd_run_pipeline.R:496`, `R/pipeline_stage_cores.R:238`,
  `R/pd_deflate_pipeline.R:312` - Every checkpoint rebuilds the full dependency
  snapshot, facts, and plan. With fixed-size batches this becomes quadratic,
  while the performance test mocks that path. **Fix**: Advance accepted
  execution state incrementally or refresh only at wave boundaries; audit the
  real public cores at 1,250 and 2,500 entities with repeated batches.

## Passed

- Review outputs were complete and referenced changed files.
- Targeted tests passed but did not prove the listed restart and scaling paths.
- No protected-artifact deletion, replacement, rename, or move was recommended.

## Brain Context

- Controlled failure state, durable invalidation, and independently retained
  final evidence remain mandatory. Sources:
  `.cg-docs/solutions/bugs/2026-09-01-separate-stage-failure-status-from-condition-provenance.md`
  and `.cg-docs/plans/2026-08-27-executable-staged-invalidation-dag.md`.
