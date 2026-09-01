---
date: 2026-09-01
title: "Consume Durable Invalidation During Restart Planning"
category: "data-quality"
language: "R"
tags: [pipeline, restart, invalidation, inventory, receipt, currentness]
root-cause: "Restart planning trusted surviving Stamp catalog receipts without requiring them to match nonmissing durable master pointers cleared by a failed stage attempt."
severity: "P1"
---

# Consume Durable Invalidation During Restart Planning

## Problem

A recoverable clean or metadata failure correctly cleared stage-owned pointers
in the durable release and master inventories. The immutable prior artifacts
remained in Stamp, as required. On the next run, however, currentness logic read
those old catalog receipts without checking the cleared master pointers.

The failed stage could therefore be marked cached. Its descendant then failed
because the exact prerequisite version and hash were missing from the master.
The invalidation was durable, but the planner did not consume it.

## Root Cause

Immutable artifact existence and current pipeline membership are different
facts. Stamp proves that an exact historical artifact exists. The master pointer
proves that the artifact is accepted by the current durable pipeline view.

Currentness used only the first fact. Failure invalidation updated only the
second. Because the planner did not require both facts to agree, a restart could
resurrect stale historical output.

## Solution

Require every current catalog receipt to match a complete durable pointer:

```r
pd_receipt_matches_pointer <- function(
  receipt, master_row, version_field, hash_field
) {
  pointer <- as.list(master_row)[c(version_field, hash_field)]
  complete <- all(vapply(pointer, function(value) {
    is.character(value) && length(value) == 1L &&
      !is.na(value) && nzchar(value)
  }, logical(1L)))

  complete &&
    identical(receipt$version_id, pointer[[version_field]]) &&
    identical(receipt$content_hash, pointer[[hash_field]])
}
```

For metadata, one receipt must match the row's metadata version and hash. For
clean, verify the complete accepted multi-output set atomically: every expected
PIP ID must have exactly one master row and one catalog receipt, and every
receipt must match its data pointer. One missing or cleared pointer makes the
clean survey stale.

Deflate already depends on clean and metadata pointers and uses the same rule.
Historical artifacts remain immutable and loadable, but they cannot become
current until a successful checkpoint republishes accepted pointers.

When a failure path writes release and master inventories, treat those writes as
self-advancement. Revalidate each receipt and pass the accumulated receipts to
later fences so the run does not reject its own verified inventory writes.

## Prevention

- Separate artifact existence from accepted currentness.
- Require exact nonmissing master version/hash pointers for every current stage
  receipt.
- Validate clean multi-output sets atomically; never cache a partial set.
- Preserve prior immutable artifacts after failure, but do not infer currentness
  from their catalog presence.
- Test recoverable clean, metadata, and deflate failures with a fresh public
  invocation. The failed node must retry before any descendant.
- Compare restarted durable state with an independently uninterrupted run,
  including exact receipt tuples and inventory version histories.
- Refresh complete facts between stage waves. Within a wave, advance only the
  live manifest, master, and verified receipt state to avoid quadratic replans.

## Related

- [Separate Stage Failure Status From Condition Provenance](../bugs/2026-09-01-separate-stage-failure-status-from-condition-provenance.md)
- [Authoritative Staged Provenance Requires Result-Bound Checkpoints](2026-08-25-authoritative-staged-provenance-checkpoints.md)
- [Fail-Closed Durable Reconciliation for Staged Data Pipelines](2026-08-26-durable-stage-reconciliation.md)
- `.cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-verify-review-4.md`
