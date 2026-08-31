---
date: 2026-08-28
depth: light
parent-review: .cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-review.md
type: verification
findings:
  P0.1: fixed
  P0.2: fixed
  P0.3: skipped
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P1.5: fixed
  P1.6: skipped
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
---

# Verification Review Report

**Review mode**: light verification
**Parent review**: `.cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-review.md`
**Files reviewed**: 18 implementation and test files
**Findings**: 12 (P0: 3, P1: 6, P2: 3, P3: 0)

## P0 - Blocking

- **[P0.1]** `cg-code-quality`, `cg-testing` -
  `R/dependency_execution.R:434`; `R/dependency_plan.R:212` - Missing manifest
  records are classified as current because fact construction skips them and
  complete planning defaults the selected nodes to `action = "none"`.
  **Why**: New or partially checkpointed entities can be silently cached
  without authoritative provenance.
  **Fix**: Emit `new_entity` or `unknown_provenance` facts for every selected
  node without a record and test partial-manifest restart behavior.
- **[P0.2]** `cg-testing` - `R/dependency_manifest.R:250` - Finalized named
  provenance accepts nonblank upstream versions, content hashes, and code hashes
  without proving them against exact committed receipts and fingerprints.
  **Why**: A checkpoint can publish false-current metadata or deflate state.
  **Fix**: Match every finalized upstream component and stage code hash to
  accepted committed evidence before inventory writes and test wrong nonblank
  values without mocking manifest validation.
- **[P0.3]** `cg-testing` - `R/dependency_execution.R:589` - The post-lease plan
  reuses the pre-lease `master` argument instead of reloading durable master
  state.
  **Why**: A survey removed during the race window can reach workers or writes.
  **Fix**: Reload master state after lease acquisition, rerun removal and
  selector checks, and add a race test that requires zero workers and writes.

## P1 - Critical

- **[P1.1]** `cg-code-quality`, `cg-testing` -
  `R/dependency_manifest.R:233`; `R/pd_process_data.R:146` - New clean outputs
  cannot finalize metadata named inputs because finalization searches only the
  pre-clean snapshot.
  **Why**: A new `pip_id` can commit clean output, then fail metadata
  checkpointing and leave an orphan artifact.
  **Fix**: Refresh and accept downstream facts after the clean checkpoint under
  the same lease before metadata execution.
- **[P1.2]** `cg-code-quality` - `R/dependency_contract.R:161` - Manifest
  validation does not enforce bidirectional record/input completeness or always
  match canonical input hashes to `record$input_hash`.
  **Why**: Records without canonical rows and input groups without records can
  pass validation.
  **Fix**: Require exact key equality, one canonical row per record, and an
  unconditional canonical-to-record hash match.
- **[P1.3]** `cg-code-quality` - `R/dependency_inputs.R:170` - Dependency
  projection validates CPI and PPP domains independently but does not reproduce
  the worker's cross-measure domain agreement checks.
  **Why**: Planning can accept a PFW row that execution later rejects.
  **Fix**: Share one pure domain-resolution helper between planning and worker
  attribute construction, including mixed-domain checks.
- **[P1.4]** `cg-testing` - `R/dependency_inputs.R:308` and
  `tests/testthat/test-dependency-manifest.R:111` - Legacy canonical-only
  compatibility is not tested against golden output from the prior algorithm;
  the current helper changes legacy field and version semantics.
  **Why**: Existing clean, metadata, or deflate rows can rebuild unnecessarily
  or receive the wrong fallback reason.
  **Fix**: Reproduce the prior canonical algorithm exactly and add golden rows
  for all three stages, including canonical `version_id` comparisons.
- **[P1.5]** `cg-testing` - `R/dependency_inputs.R:278` - A shared auxiliary
  artifact version change enters every entity's canonical hash even when an
  entity's keyed projection is unchanged.
  **Why**: One-country CPI or PFW changes can invalidate unrelated entities.
  **Fix**: Separate exact source identity from semantic per-entity change
  comparison and add a two-entity realistic catalog-version test.
- **[P1.6]** `cg-testing` - `tests/testthat/test-pd-change-report.R:230` - The
  zero-household-load assertion mocks `pip_read()` rather than the active clean
  load boundary and does not exercise current-node stage loops.
  **Why**: Cached work can still load household artifacts or call workers while
  the test passes.
  **Fix**: Use a nonempty fully current fixture and count `inv_dlw_load`,
  `load_dlw_data`, all stage workers, saves, and inventory writers.

## P2 - Important

- **[P2.1]** `cg-code-quality` - `R/dependency_execution.R:655` - The cached-node
  worker guard checks data-frame units only; a named list with
  `action = "none"` reaches the worker.
  **Why**: Internal unit shape differences can bypass the safety guard.
  **Fix**: Validate one unit shape or inspect `action` for tabular and named-list
  units before dispatch.
- **[P2.2]** `cg-code-quality` - `R/dependency_execution.R:420` - Fact
  construction repeatedly binds the full accumulated table inside nested loops.
  **Why**: Planning approaches quadratic copying at larger inventory sizes.
  **Fix**: Accumulate rows in a list and bind once, or use keyed vectorized joins.
- **[P2.3]** `cg-testing` - `tests/testthat/test-pd-change-report.R:187` - The
  report/execution parity test mocks the helper that creates every compared
  fact.
  **Why**: It passes even when report and execution use different manifests,
  catalogs, fingerprints, or master inventories.
  **Fix**: Exercise the real preparation helper with deterministic injected
  metadata facts and compare exact actions, reasons, context, and identity.

## Passed Checks

- `.Rbuildignore` excludes `.cg-docs`.
- Both review agents returned complete, file-specific output.
- No protected-artifact deletion, replacement, rename, or move was recommended.

## Verification Scope

This pass used the parent review suppression policy. No P0/P1 finding was
suppressed. P2 findings are outside the exact prior fixed blocks or describe
cross-file behavior and therefore remain reportable.
