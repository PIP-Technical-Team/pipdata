---
date: 2026-08-26
title: "Typed Stage Orchestration Must Fail Closed and Account for Every Unit"
category: "bugs"
language: "R"
tags: [pipeline, orchestration, fail-closed, checkpoint, provenance]
root-cause: "The typed stage boundary trusted mutable joined inputs, classified unknown failures as recoverable, and could omit selected units after a terminal checkpoint failure."
severity: "P0"
plan: ".cg-docs/plans/2026-08-25-pipeline-stage-interface.md"
reviewed-in: ".cg-docs/reviews/2026-08-24-pipdata-staged-dependency-manifest-verify-review-2.md"
related: [".cg-docs/solutions/data-quality/2026-08-25-authoritative-staged-provenance-checkpoints.md", ".cg-docs/solutions/bugs/2026-09-01-separate-stage-failure-status-from-condition-provenance.md"]
---

# Typed Stage Orchestration Must Fail Closed and Account for Every Unit

## Problem

A typed pipeline result can look complete and auditable while execution has
already crossed an unsafe boundary. The deflation pilot exposed four related
symptoms:

- joining accepted actions to the current inventory by `pip_id` allowed
  same-named inventory fields to replace the exact versions and hashes accepted
  by the dependency plan;
- unknown fence, lease, write, or integrity errors could be normalized as
  recoverable survey failures, allowing later writes;
- a failed checkpoint omitted successful-but-uncommitted and not-yet-attempted
  units from result accounting; and
- focused wrapper tests mocked the shared execution core, so they could pass
  without exercising these integrity paths.

The result therefore risked consuming unapproved inputs, continuing after
shared state became uncertain, and understating the selected workload.

## Root Cause

The orchestration boundary did not consistently distinguish authoritative
planning data from mutable enrichment data, survey-domain failures from
shared-state failures, or worker completion from committed success. Testing at
the adapter boundary reinforced the intended return shape but did not prove the
real core's state transitions.

## Solution

Treat the accepted plan as immutable authority and enrich it without allowing
column replacement:

```r
inventory_rows <- inv[match(actions$pip_id, inv$pip_id)]

comparisons <- list(
  data_version_id = "version_id_data",
  data_hash = "content_hash_data",
  metadata_version_id = "version_id_metadata",
  metadata_hash = "content_hash_metadata"
)

stopifnot(!anyDuplicated(actions$pip_id))
stopifnot(!anyDuplicated(inv$pip_id))
stopifnot(all(vapply(names(comparisons), function(plan_field) {
  identical(actions[[plan_field]], inventory_rows[[comparisons[[plan_field]]]])
}, logical(1L))))
```

Only append inventory columns absent from the accepted action schema. Reject
missing, duplicate, unmatched, or changed inputs before invoking a worker.

Classify recoverable failures with an explicit allowlist of known survey-domain
conditions. Treat every unrecognized error as fatal. Fatal conditions stop all
later workers, checkpoints, and writes; capture policy may change the outer
return value, but never continuation behavior.

Bind success to checkpoint commitment. If a checkpoint fails, materialize each
pending receipt as `failed` with `checkpoint_uncommitted`, materialize the
active pre-checkpoint unit as `fatal_uncommitted` when applicable, and mark all
remaining selected units `skipped` with `upstream_failed`. Preserve the
invariant:

```r
selected == attempted + skipped + cached
```

Finally, test the real shared core while mocking only external I/O boundaries.
Inject changed inputs, unknown errors, and checkpoint failure with pending and
unattempted units, then assert worker/write call counts and complete accounting.

## Prevention

- Keep accepted plan fields authoritative; never resolve same-named columns by
  an unconstrained join.
- Validate one-to-one identity and exact version/hash equality before any side
  effect.
- Use recoverable allowlists, not fatal denylists. Unknown errors fail closed.
- Distinguish worker output, pending receipt, and committed success as separate
  states.
- Require every selected entity to appear exactly once in terminal accounting.
- Test adapters for compatibility and the real core for state-machine safety.
- Mock storage and logging boundaries, not the orchestration function under
  test.

## Related

- [Authoritative Staged Provenance Requires Result-Bound Checkpoints](../data-quality/2026-08-25-authoritative-staged-provenance-checkpoints.md)
- [Separate Stage Failure Status From Condition Provenance](2026-09-01-separate-stage-failure-status-from-condition-provenance.md)
- `.cg-docs/plans/2026-08-25-pipeline-stage-interface.md`
- `.cg-docs/work-reports/2026-08-26-pipeline-stage-interface.md`
