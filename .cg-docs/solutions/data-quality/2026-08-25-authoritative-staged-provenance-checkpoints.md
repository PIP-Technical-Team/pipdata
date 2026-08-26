---
date: 2026-08-25
title: "Authoritative Staged Provenance Requires Result-Bound Checkpoints"
category: "data-quality"
language: "R"
tags: [provenance, dependency-manifest, checkpoint, fencing, exact-version]
root-cause: "Planning snapshots and successful writes were not consistently rebound to exact result provenance before inventory and manifest publication."
severity: "P0"
---

# Authoritative Staged Provenance Requires Result-Bound Checkpoints

## Problem

A metadata-only dependency planner can correctly identify stale work while the
executor still publishes false-current state. This occurs when execution trusts
an advisory plan, reloads latest inputs, infers success from catalog presence,
or writes manifest inputs from the pre-execution snapshot after upstream work
has produced a new exact version.

Restart boundaries make the problem more visible. A clean artifact may commit
before metadata, or a deflation attempt may fail after a prior successful
version. Without result-bound provenance, the next run can use stale metadata
or retain a persisted current flag that no longer describes the attempted
inputs.

## Root Cause

Planning, artifact writes, inventory reconciliation, and manifest publication
were treated as related operations rather than one fenced state transition.
The missing invariant was that every published record must be derived from the
exact verified receipts produced by the current attempt, using the same
canonical input representation used by planning.

## Solution

Use one authoritative execution sequence:

1. Build and validate a fresh immutable snapshot before any write-side effect.
2. Freeze exact auxiliary and artifact versions and semantic projections.
3. Revalidate the snapshot and lease immediately before each write boundary.
4. Save through a typed receipt containing exact path, version, and hash.
5. Reconcile only complete verified stage results. Multi-output clean work is
   all-or-nothing.
6. Write and verify release inventory, then master inventory.
7. Publish manifest records, inputs, and code fingerprints from canonical
   result provenance, not stale snapshot rows.
8. On restart, regenerate metadata from the pinned cleaned artifact when the
   clean checkpoint committed but metadata did not.
9. Persist failure invalidation when a failed forced attempt must clear stale
   current pointers.

Code fingerprints require the same completeness discipline. Hash the reviewed
value-affecting closure, including constants, active S3 methods, recode content,
and allowlisted external implementations. Test mutation and stage isolation;
testing the hash mechanism alone does not prove closure completeness.

## Prevention

- Never let advisory plans suppress newly detected authoritative work.
- Never infer current-attempt success from alias-wide latest catalog rows.
- Reject missing input or code hashes at checkpoint publication.
- Use the same canonical semantic hashes in planning and execution results.
- Test interruption after every durable boundary and assert restart selection.
- Test forced failures against reloaded persisted inventory, not only returned
  in-memory state.
- Instrument large synthetic runs for bounded external I/O and checkpoint
  counts.
- Permit offline lease recovery only after demonstrating that the prior owner
  is no longer live.

## Related

- `.cg-docs/plans/2026-08-24-pipdata-staged-dependency-manifest.md`
- `.cg-docs/solutions/data-quality/2026-08-07-aux-content-hash-gated-recleaning.md`
- `.cg-docs/solutions/data-quality/2026-05-27-legacy-column-persistence-in-on-disk-inventory.md`
- [Typed Stage Orchestration Must Fail Closed and Account for Every Unit](../bugs/2026-08-26-fail-closed-typed-stage-orchestration.md)
