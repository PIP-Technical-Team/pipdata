---
date: 2026-08-28
title: "Separate Exact Provenance From Semantic Invalidation"
category: "data-quality"
language: "R"
tags: [dependency-manifest, semantic-invalidation, exact-version, content-hash, missing-provenance, incremental-processing]
root-cause: "The planner used exact shared artifact versions as per-entity change signals and treated absent manifest records as current, while checkpoint code accepted unverified nonblank provenance."
severity: "P0"
---

# Separate Exact Provenance From Semantic Invalidation

## Problem

An incremental planner can fail in two opposite directions:

- A shared auxiliary artifact gets a new exact `version_id`, but only one keyed
  country/year projection changes. If the version enters every entity's
  semantic comparison, all entities rebuild.
- A selected node has no manifest record. If fact construction skips the node,
  complete planning can default it to `action = "none"` and falsely cache work
  with no authoritative provenance.

Checkpoint publication can then compound either error if it accepts any
nonblank version/hash pair rather than proving it against committed receipts.

## Root Cause

One representation was asked to answer two different questions:

1. Which exact immutable artifact version must execution read and record?
2. Did the keyed, value-affecting input for this entity change?

Exact source identity and semantic entity content are both required, but they
are not interchangeable. A valid manifest also needs bidirectional completeness:
every record has one canonical input group and every input group has one record.

## Solution

Keep exact provenance in each named component, but calculate the canonical
semantic content from component content hashes:

```r
canonical <- data.table::data.table(
  name = "canonical",
  version_id = pd_hash_object(components[, .(name, version_id)]),
  content_hash = pd_hash_object(components[, .(name, content_hash)])
)
```

For keyed auxiliary and PFW components, compare `content_hash` to decide whether
the entity is stale. Retain and publish the exact `version_id` so execution can
load and prove the immutable source. For upstream stage artifacts and code,
compare both exact identity and semantic content because those are entity-bound
receipts, not shared catalog vintages.

Fail closed when a selected node lacks one manifest record:

```r
reason <- if (has_output) "unknown_provenance" else "new_entity"
```

Before any inventory writer, validate finalized named inputs and code hashes
against accepted C2 fingerprints and exact committed output receipts. Refresh
execution facts after a clean checkpoint before finalizing new downstream
`pip_id` inputs.

Manifest validation must require:

- identical `(stage, entity_id)` keys in records and input groups;
- exactly one `canonical` row for each record;
- `record$input_hash == canonical$content_hash`;
- a canonical row that recomputes from the sorted named components; and
- complete exact output receipts and stage fingerprints.

Preserve legacy schema-1 behavior separately. Reproduce the prior canonical
hash and version algorithms exactly for canonical-only records. Do not infer a
legacy comparison from the new named-component algorithm.

## Prevention

- Never use a shared artifact `version_id` as the only per-entity invalidation
  signal. Compare the exact keyed semantic projection.
- Never drop a selected node from fact construction because its manifest record
  is absent. Emit `new_entity` or `unknown_provenance`.
- Never accept a nonblank version/hash as proof. Match it to an accepted or
  committed receipt before durable publication.
- Test a two-entity shared-catalog version change where only one keyed projection
  changes.
- Test partial manifests, record/input key asymmetry, wrong nonblank receipt
  values, new clean-output fan-out, and legacy golden hashes for every stage.
- Accumulate fact rows in a list and bind once so complete-node planning remains
  linear enough for large metadata inventories.

## Related

- `.cg-docs/solutions/data-quality/2026-08-25-authoritative-staged-provenance-checkpoints.md`
- `.cg-docs/solutions/data-quality/2026-08-07-aux-content-hash-gated-recleaning.md`
- `.cg-docs/plans/2026-08-27-executable-staged-invalidation-dag.md`
- `.cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-verify-review-2.md`
