---
date: 2026-09-01
depth: light
parent-review: .cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-review.md
type: verification
findings:
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P1.5: fixed
  P1.6: fixed
  P1.7: fixed
  P1.8: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
---

# Verification Review Report

**Review mode**: light (`mode:verify`)
**Parent review**: `.cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-review.md`
**Files reviewed**: 67
**Findings**: 12 (P0: 0, P1: 8, P2: 4, P3: 0)

## P1 - Critical

- **[P1.1]** `cg-code-quality` `R/pipeline_stage_cores.R:184`,
  `R/pipeline_stage_cores.R:395`, `R/pd_deflate_pipeline.R:269` - Recoverable
  failures put worker condition codes in `reason_codes`, but the stage-result
  validator accepts only controlled dependency or stage reasons. Result
  construction can abort after work instead of returning a partial aggregate.
  **Fix**: Keep condition codes in condition records and use a controlled unit
  failure reason, with end-to-end recoverable-failure tests for all stages.
- **[P1.2]** `cg-code-quality` `R/pd_run_pipeline.R:427` - The post-lease
  bootstrap resolution result is discarded. A PIP selector can lose its survey
  owner after clean changes the output set, causing strict refresh or retry to
  fail. **Fix**: Retain the resolved survey-ID set and use it for locked
  preparation and all fact refreshes.
- **[P1.3]** `cg-code-quality` `R/pd_metadata_refresh.R:66` - Metadata base
  validation accepts any uniquely named list and does not require fields and
  types needed by deflation. **Fix**: Validate the complete metadata base schema
  and reconstruct from the exact clean artifact when invalid.
- **[P1.4]** `cg-code-quality` `R/pipeline_stage_cores.R:190` and
  `R/pipeline_stage_cores.R:401` - Recoverable clean and metadata failures clear
  only the in-memory master. If no later sibling checkpoint succeeds, durable
  inventories retain stale pointers. **Fix**: Persist and verify narrowed
  release/master invalidation before stage return.
- **[P1.5]** `cg-code-quality` `R/pd_run_pipeline.R:643` - A failed retained
  manifest verification still allows stage results and `manifest_after` to bind
  to stale in-memory evidence. **Fix**: Build artifact-bearing results only from
  a successfully reloaded retained manifest; otherwise propagate the integrity
  error without claimed final evidence.
- **[P1.6]** `cg-testing` `tests/testthat/test-pd-run-pipeline.R:1061` - The
  crash/restart matrix calls checkpoint and replan helpers instead of restarting
  through `pd_run_pipeline()`. It does not prove public terminal accounting,
  descendant blocking, lease cleanup, or versioning restoration. **Fix**:
  inject faults through the public pipeline and restart with a second public
  call against the same durable fixture.
- **[P1.7]** `cg-testing` `tests/testthat/test-pd-run-pipeline.R:700` - Restart
  comparison removes output receipt and inventory version IDs, so a retry can
  publish the wrong immutable versions and still pass. **Fix**: Compare exact
  canonical receipt tuples and inventory version fields; normalize only
  temporary root prefixes.
- **[P1.8]** `cg-testing` `tests/testthat/test-pipeline-stage-result.R:222` -
  The final-evidence test constructs the manifest and outcome from the same
  receipt and calls the internal binder directly. **Fix**: Run a public
  multi-wave pipeline with more than three later publications, independently
  reload the final manifest, and compare all artifact tuples and generations.

## P2 - Important

- **[P2.1]** `cg-code-quality` `R/log_report.R:915` - The report can combine the
  latest pipeline run with the oldest `process_summary_inf`. **Fix**: Select
  clean, deflate, and header summaries by the latest pipeline summary `run_id`.
- **[P2.2]** `cg-code-quality` `R/pd_deflate_pipeline.R:387` - The shared failure
  logger writes `pip_id` to the survey field, so clean failures with no PIP ID
  lose their survey identifier. **Fix**: Use `pip_id` for downstream stages and
  fall back to `survey_id` for clean conditions.
- **[P2.3]** `cg-testing` `tests/testthat/test-pd-run-pipeline.R:1532` - No-match
  and outside-selection tests do not assert manifest identity or table equality.
  **Fix**: Compare canonical records, inputs, fingerprints, tombstones, and
  manifest identity before and after each no-effect run.
- **[P2.4]** `cg-testing` `tests/testthat/test-dependency-performance.R:154` -
  The performance audit covers one fact build, not repeated public executor
  checkpoint refreshes. **Fix**: Exercise 1,250/2,500-entity public runs and
  count full snapshot builds, joins, queries, and household reads across the
  complete checkpoint path.

## Passed

- The verification outputs were complete and referenced changed files.
- No protected-artifact deletion, replacement, rename, or move was recommended.

## Brain Context

- Exact accepted action fields and fail-closed shared-state handling remain the
  primary review rules. Sources:
  `.cg-docs/plans/2026-08-27-executable-staged-invalidation-dag.md` and
  `.cg-docs/solutions/bugs/2026-08-26-fail-closed-typed-stage-orchestration.md`.
