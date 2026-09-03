---
date: 2026-08-26
title: "Fail-closed durable reconciliation for staged data pipelines"
category: "data-quality"
language: "R"
tags: [dlw, orchestration, reconciliation, inventory, validation, retry, stamp, logging]
root-cause: "Pipeline control flow trusted in-memory and write-return state instead of canonical durable state, while completed inventories also carried retry-control rows."
severity: "P0"
---

# Fail-Closed Durable Reconciliation for Staged Data Pipelines

## Problem

The DLW acquisition and validation wrappers combined discovery, per-survey
execution, persistence, and logging. Several failure modes could silently make
control state disagree with durable data:

- a write could report failure after activating intended content, or return a
  malformed/null version without proving rollback;
- validation execution failures were stored as unavailable inventory rows,
  allowing retry-control state to leak into cleaning;
- stale checksums, deleted catalog rows, and orphan report rows survived reruns;
- unreadable historical inventory versions could be omitted when computing the
  next pipeline version;
- accumulated logs and validation-report rows from older attempts could be
  counted as current work.

These are silent integrity failures because ordinary control flow can continue
while returned results, inventories, and reports describe different states.

## Root Cause

The wrappers treated a successful function return, an error, or the latest
in-memory table as authoritative. Those signals are not sufficient for
versioned persistence. A payload, sidecar, or catalog write can fail at a
different boundary, and an existing artifact can be readable, absent, corrupt,
or active at either the prior or intended content.

The validation inventory also served two incompatible roles: completed-data
handoff and retry ledger. This made absence unavailable as the retry signal and
allowed failed executions to become downstream candidates.

## Solution

Use explicit stage-owned orchestration with compact one-survey workers and a
narrow artifact-specific reconciliation boundary.

1. Validate caller arguments and preconditions before runtime error conversion.
2. Discover candidates from copied, normalized inventories; never mutate loaded
   or caller-visible `data.table` objects by reference.
3. Return one compact worker outcome per survey. Conditions, traces, and survey
   data never enter the stage result.
4. Build the complete intended artifact in memory and canonicalize row order,
   column order, types, primary-key attributes, and transient table attributes.
5. Treat thrown, malformed, null-version, or inconsistent skipped writes as
   uncertain. Reload durable state and compare canonical content:
   - intended active: reconciled success;
   - prior active: reconciled failure with trustworthy prior state;
   - confirmed absence: trustworthy absence;
   - unreadable or different content: untrustworthy failure.
6. Keep `gmd_valid_inv` completed-only. Persist only `valid` or `invalid` rows;
   execution failures retry because their acquisition key remains absent.
7. Reconcile validation inventory and report to authoritative current
   acquisition keys on every call, including zero-worker calls. Require exact
   report coverage for completed IDs and write the report before inventory.
8. Compute the next pipeline version from all catalog-listed historical
   inventory versions. Any corrupt catalog warning, unreadable version, or
   malformed historical artifact blocks the run instead of lowering the
   maximum.
9. Emit a positional `attempt_start` boundary and exact completion metadata.
   Reports segment from the latest boundary before selecting a valid completion
   or using legacy fallback.

The resulting public stage objects expose outcome, durable inventory, compact
failures, summary counts, and artifact facts without introducing a generic
pipeline class or retry token.

## Prevention

- Do not infer rollback from an exception or null version ID.
- Do not mix retry-control rows with completed-data inventories.
- Do not accept schema-light empty durable artifacts; distinguish them from
  lenient empty caller inputs.
- Do not suppress warnings from version-catalog readers when warnings indicate
  dropped corruption.
- Make canonical ordering total, including `NA` versus `NaN` and classed
  optional fields.
- Verify worker inventory rows against the selected candidate's ID, checksum,
  status, and pipeline version before persistence.
- Keep typed logging at orchestration boundaries so persistent logs cannot
  retain survey-sized arguments.
- Test uncertain persistence against real temporary versioned storage and run
  changed tests with shuffled order.

## Related

- [Separate Stage Failure Status From Condition Provenance](../bugs/2026-09-01-separate-stage-failure-status-from-condition-provenance.md)
- `.cg-docs/plans/2026-08-25-dlw-wrapper-rewrite.md`
- `.cg-docs/solutions/performance-issues/2026-07-22-per-survey-logging-retains-large-survey-objects.md`
- `.cg-docs/solutions/testing-patterns/2026-04-27-contract-testing-for-logging-side-effects.md`
- `.cg-docs/solutions/testing-patterns/2026-08-21-built-package-and-checkpoint-verification.md`
- `.cg-docs/solutions/bugs/2026-04-29-duplicate-logmeta-discriminator-key.md`
- `.cg-docs/solutions/data-quality/2026-05-27-legacy-column-persistence-in-on-disk-inventory.md`
