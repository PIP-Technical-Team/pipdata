---
date: 2026-09-01
title: "Separate Stage Failure Status From Condition Provenance"
category: "bugs"
language: "R"
tags: [pipeline, stage-result, recoverable-failure, invalidation, manifest]
root-cause: "Recoverable condition classes were stored as unit reason codes, while failed clean and metadata invalidation existed only in the returned in-memory inventory."
severity: "P1"
---

# Separate Stage Failure Status From Condition Provenance

## Problem

A recoverable clean, metadata, or deflate worker could finish with a typed
condition record but still prevent construction of the pipeline aggregate. The
stage unit used the condition class, such as `report_lvl` or `deflation_na`, as
its `reason_codes` value. The stage-result validator accepts only controlled
dependency and execution reasons, so validation failed after worker activity.

Clean and metadata failures also cleared current pointers only in the returned
master inventory. If no later sibling checkpoint succeeded, the release and
master inventories on disk retained the old pointers. A restart could therefore
plan from stale durable state even though the prior run reported a failure.

## Root Cause

Three separate concerns were represented by one field and one in-memory
mutation:

- unit state explains how the scheduler accounted for the entity;
- condition provenance identifies the exact worker error; and
- durable invalidation prevents a failed attempt from remaining current.

Condition classes are an open set owned by worker implementations. Unit reason
codes are a closed result schema. Treating them as interchangeable made the
aggregate invalid. Treating an in-memory inventory update as durable made retry
behavior depend on whether a later sibling happened to checkpoint.

## Solution

Use one controlled unit reason for recoverable entity failure and retain the
exact condition separately:

```r
unit <- pd_stage_unit_row(
  action,
  stage,
  status = "failed",
  reasons = "entity_failed",
  started_at,
  completed_at
)

outcome$errors[[length(outcome$errors) + 1L]] <- condition_record
```

The condition record remains the authority for `code`, classes, message,
operation, entity identifiers, and recoverability. `entity_failed` says only
that the selected unit attempted work and failed with a recorded entity-domain
condition.

Persist narrowed invalidation immediately under the active execution fence:

```r
master <- pd_persist_failed_invalidation(
  execution,
  master,
  action,
  release_writer,
  master_writer
)
```

The helper clears only fields owned by the failed stage, writes and verifies the
release inventory, writes and verifies the master inventory, and rechecks the
lease and manifest parent. It does not publish successful manifest provenance
for the failed unit.

Finally, construct artifact-bearing stage results only after independently
reloading the final retained manifest. If that read or identity check fails,
propagate the integrity condition. Do not bind `final_evidence_manifest` to a
stale in-memory identity.

## Prevention

- Keep unit reason codes closed and condition codes open.
- Store exact condition identity only in typed condition records.
- Persist failed invalidation even when the failed entity is the last selected
  unit and no sibling checkpoint follows.
- Verify release and master inventory receipts under the same lease and parent
  fence used by successful checkpoints.
- Do not return artifact evidence when the final retained manifest cannot be
  loaded and verified.
- Test recoverable failure for clean, metadata, and deflate through final
  aggregate validation.
- Test restart after an isolated failure by reloading durable inventories, not
  only by inspecting the returned in-memory table.

## Related

- [Consume Durable Invalidation During Restart Planning](../data-quality/2026-09-01-consume-durable-invalidation-during-restart-planning.md)
- [Typed Stage Orchestration Must Fail Closed and Account for Every Unit](2026-08-26-fail-closed-typed-stage-orchestration.md)
- [Authoritative Staged Provenance Requires Result-Bound Checkpoints](../data-quality/2026-08-25-authoritative-staged-provenance-checkpoints.md)
- [Fail-Closed Durable Reconciliation for Staged Data Pipelines](../data-quality/2026-08-26-durable-stage-reconciliation.md)
- `.cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-verify-review-3.md`
