---
date: 2026-08-25
title: "Typed Pipeline Stage Interface"
status: decided
scope: "Deep"
artifact-schema-version: 1
chosen-approach: "Validated S3 envelopes with a deflation-first adoption slice"
tags: [pipeline, orchestration, interface, s3, context, results, provenance, logging, serialization]
---
<!-- Valid status values: decided, in-progress, abandoned -->

# Typed Pipeline Stage Interface

## Context

Pipdata needs a minimal contract through which acquisition, validation, clean,
metadata, and deflate stages can expose outcomes to a future top-level
orchestrator. The active wrappers currently return incompatible shapes:
acquisition and validation return invisible `NULL`, cleaning and batch
deflation return the master inventory, single-survey deflation returns a
`data.table` or failure sentinel, and internal C2 workers return ad hoc lists or
`NULL`.

The prerequisite audit established the following baseline before this decision:

- The C3 branch was clean and identical to current `origin/PROD` at `a55a089`.
- C2 staged dependency invalidation completed its brainstorm, plan, work,
  review, verification, and compound cycle and is committed on this branch.
- C2 remains subject to its production activation boundary: the target
  Windows/SMB fencing and immutable unique-rename smoke test is still required.
- B2 unified logging is present from `origin/PROD`. Active pipeline code writes
  to `pipdata_log`, uses string logmeta discriminators, and does not expose the
  old DLW `log` or `save_log` arguments.
- C1 targeted `force_surveys` and C2 stage plans, actions, reasons, hashes,
  receipts, checkpoints, and manifest generations are existing contracts that
  C3 must consume rather than replace.

The historical roadmap entry names a generic `safe_pipeline_step()` as a
prerequisite, but no such implementation exists. C3 will not create a generic
wrapper speculatively. The deflation pilot will define the smallest safe
entity-execution boundary; a reusable helper may be extracted later if more
than one stage demonstrates the same behavior.

## Requirements

- Provide validated S3 contracts named `pipeline_context` and
  `pipdata_stage_result`, backed by ordinary R lists and no new package
  dependency.
- Keep the immutable C2 dependency context intact as a nested subobject. Do not
  add runtime options to it or change its scope hash implicitly.
- Carry release, identity, storage aliases, execution options, resolved survey
  selection, dependency hashes, manifest identity, and log correlation through
  one shared context.
- Represent stage status with `success`, `partial`, `skipped`, `cached`, and
  `failed` semantics derived from per-entity outcomes.
- Continue after independent survey failures and capture them as stable
  condition records for later review.
- Stop the run after shared-state integrity failures such as lease loss,
  context drift, parent-generation races, or failed checkpoint publication.
  A top-level boundary may convert the stopped run to a terminal result, but it
  must not continue writes.
- Return verified artifact references by default. Large household data,
  inventories, and log objects must not be retained in result objects.
- Preserve existing exported signatures, positional arguments, return shapes,
  and side effects through adapters.
- Integrate with B2 by correlating to `pipdata_log` and emitting existing
  summary types once. Do not copy piplog rows into results.
- Integrate with C1 by carrying the resolved forced selection and the C2
  `forced` reason; do not add another `force_surveys` resolver.
- Integrate with C2 by reusing exact actions, reason codes, input hashes, code
  hashes, receipts, checkpoint generations, and currentness decisions.
- Define a versioned, deterministic, RDS-serializable projection suitable for
  a future run-manifest artifact.
- Pilot the contract on batch deflation before changing acquisition,
  validation, or cleaning internals.
- Leave step topology, step hashes, graph propagation, and step-level cache
  state to C4.

## Approaches Considered

### Approach 1: Validated S3 Envelopes

Define lightweight S3 classes backed by named lists. Constructors establish
canonical field order and defaults; validators enforce types, controlled
vocabularies, cross-field invariants, and serializable projections. Compact
print methods expose stage, status, counts, timing, and failure summaries
without printing payloads.

**Pros:** No new dependency, explicit API identity, useful dispatch and print
behavior, strict validation, and an incremental migration path around current
wrappers.

**Cons:** Validation and schema migration are maintained manually. Extension
fields require discipline to avoid an unvalidated catch-all.

**Effort:** Medium for the foundation and deflation pilot; large for complete
cross-stage adoption.

### Approach 2: Validated Structured Lists

Use the same fields and validators but return ordinary named lists without S3
classes.

**Pros:** Smallest implementation and straightforward RDS serialization.

**Cons:** Callers cannot reliably distinguish a valid stage result from a
coincidentally shaped list. Printing, coercion, and future methods are weaker,
and internal code can bypass validation more easily.

**Effort:** Small to medium.

### Approach 3: Stage-Specific Class Hierarchy

Define a base result plus acquisition-, validation-, clean-, metadata-, and
deflate-specific subclasses.

**Pros:** Strong stage-specific dispatch and specialized validation.

**Cons:** Prematurely expands the API, duplicates schemas, and risks locking in
stage boundaries that C4 may later refine into a step DAG.

**Effort:** Large.

## Decision

Choose **Approach 1: Validated S3 Envelopes**.

This approach provides a real type boundary without a new dependency or a
breaking wrapper migration. The initial implementation will prove the contract
with deflation only. Existing exported wrappers will retain their current
returns while internal or new orchestration entry points return typed results.

## Proposed `pipeline_context` Schema

`pipeline_context` is a named list with class
`c("pipeline_context", "list")`. Its descriptor fields are immutable after
construction. Its optional runtime handle is internal and excluded from the
serializable projection.

| Field | Type | Required | Semantics |
|---|---|---:|---|
| `schema_version` | integer scalar | yes | Context schema, initially `1L` |
| `run_id` | non-empty character scalar | yes | Correlation identifier shared by results and B2 logs |
| `release` | non-empty character scalar | yes | Working release |
| `identity` | character scalar | yes | One of `PROD`, `INT`, or `TEST` |
| `dependency_context` | validated C2 context | yes | Unchanged result of `pd_dependency_context()` |
| `storage` | named list | yes | Resolved aliases and normalized roots used by the run |
| `options` | named list | yes | Execution controls, not dependency identity |
| `selection` | named list | yes | Canonical resolved `survey_id` and `pip_id` vectors |
| `dependency` | named list | yes | Scope, manifest, plan, and code-hash references |
| `logging` | named list | yes | Log name and correlation metadata, not log rows |
| `created_at` | UTC time scalar | yes | Observational context creation time |
| `runtime` | internal list/environment or `NULL` | no | Prepared C2 execution state and live lease; never serialized |

### `storage`

The storage descriptor records logical aliases and resolved roots needed by the
run, including applicable DLW, DLW metadata, `pip`, `pip_meta`,
`pip_deflated`, `pip_inv`, `pip_master`, and `piplog` locations. Storage
descriptors are compact strings only. They do not contain open connections,
catalogs, or loaded datasets.

### `options`

The initial controlled options are `verbose`, `force`, `force_surveys`,
`bootstrap`, `bootstrap_entities`, `checkpoint_size`, `checkpoint_seconds`,
`entity_error_policy`, and `fatal_error_policy`.

- `entity_error_policy` defaults to `"continue"` so one survey failure does
  not stop independent surveys. `"abort"` may be supported for diagnostic
  fail-fast runs.
- `fatal_error_policy` may be `"abort"` or `"capture_at_run_boundary"`.
  Both stop execution immediately. The latter permits only the outer run
  boundary to return a terminal result after all writing has stopped.
- `force` and `force_surveys` remain mutually exclusive.
- Resolved force selections come from C1/C2 planning, not from context-specific
  identifier logic.

### `selection`

Selection contains sorted, unique, canonical character vectors for
`survey_id` and `pip_id`. It records the resolved execution scope, not the raw
user request. Unknown or ambiguous identifiers remain warnings/errors produced
by the authoritative planner.

### `dependency`

The dependency descriptor contains compact references only:

- `scope_id` and `context_hash` from the unchanged C2 dependency context;
- manifest identity before execution and, when available, after checkpoint;
- a deterministic hash of the accepted plan;
- named C2 stage code hashes;
- the snapshot capture time;
- bootstrap state.

Per-entity input and output hashes belong in stage results, not in the shared
context.

## Proposed `pipdata_stage_result` Schema

`pipdata_stage_result` is a named list with class
`c("pipdata_stage_result", "list")`.

| Field | Type | Required | Semantics |
|---|---|---:|---|
| `schema_version` | integer scalar | yes | Result schema, initially `1L` |
| `stage` | character scalar | yes | Controlled pipeline stage |
| `status` | character scalar | yes | Derived aggregate status |
| `terminal` | logical scalar | yes | Whether execution must not continue |
| `run_id` | character scalar | yes | Links context, results, and B2 logs |
| `data` | small object or `NULL` | yes | Explicit non-artifact payload only |
| `artifacts` | normalized `data.table` | yes | Verified committed artifact references |
| `units` | normalized `data.table` | yes | Per-entity outcomes from which status/counts derive |
| `counts` | named integer vector | yes | Derived stage metrics |
| `log_ref` | named list | yes | B2 log/checkpoint correlation, never log copies |
| `warnings` | list of condition records | yes | Stable serializable warning records |
| `errors` | list of condition records | yes | Stable serializable error records |
| `provenance` | named list | yes | C2 plan/checkpoint/reason references |
| `input_hashes` | named character vector | yes | Canonical C2 hashes by entity |
| `output_hashes` | named character vector | yes | Canonical C2 hashes by entity |
| `started_at` | UTC time scalar | yes | Observational start time |
| `completed_at` | UTC time scalar | yes | Observational completion time |

The controlled stage vocabulary is `acquisition`, `validation`, plus the
existing C2 stages `clean`, `metadata`, and `deflate`. C3 does not rename or
modify C2's `.PD_STAGES`; it extends that vocabulary only at the orchestration
interface.

### Unit Outcome Schema

Each row in `units` contains:

| Field | Semantics |
|---|---|
| `stage` | Controlled stage name |
| `entity_id` | Canonical C2 entity key or DLW survey key |
| `survey_id` | Survey identifier or `NA_character_` |
| `pip_id` | PIP identifier or `NA_character_` |
| `status` | `success`, `skipped`, `cached`, or `failed` |
| `action` | Existing C2 action or `NA_character_` outside C2 stages |
| `reason_codes` | List-column of controlled C2 or stage-boundary reasons |
| `input_hash` | Canonical aggregate input hash or `NA_character_` |
| `output_hash` | Canonical aggregate output hash or `NA_character_` |
| `started_at` | UTC unit start time |
| `completed_at` | UTC unit completion time |

`partial` is an aggregate stage status only. It is never a unit status and
never describes a partially committed multi-output survey.

### Artifact Reference Schema

Each row in `artifacts` contains `entity_id`, `alias`, `artifact`, `path`,
`version_id`, `content_hash`, `role`, and `manifest_generation`. These fields
are derived from verified C2 receipts and checkpoint publication. A successful
artifact-backed unit cannot contain an unverified or uncommitted reference.

`data` defaults to `NULL`. It may contain a small explicit value only when the
stage has no durable artifact and the constructor is called deliberately with
that value. The first implementation does not infer safety from an object-size
threshold. Household datasets, inventories, catalogs, and piplog objects are
prohibited from the serializable result.

### Stable Condition Record

Warnings and errors use a versioned plain-list record with these fields:

| Field | Semantics |
|---|---|
| `schema_version` | Condition-record schema, initially `1L` |
| `condition_id` | Stable identifier referenced by logs |
| `severity` | `warning` or `error` |
| `code` | Stable domain condition code |
| `classes` | Ordered character vector of R condition classes |
| `message` | Condition message |
| `stage` | Stage where the condition was normalized |
| `entity_id` | Entity key or `NA_character_` |
| `survey_id` | Survey identifier or `NA_character_` |
| `pip_id` | PIP identifier or `NA_character_` |
| `operation` | Stable operation identifier |
| `recoverable` | Whether independent unit execution may continue |
| `timestamp` | UTC observation time |
| `parent_code` | Bounded parent summary code or `NA_character_` |
| `parent_message` | Bounded parent message or `NA_character_` |
| `details` | Allowlisted named atomic values only |

Raw condition objects, calls, environments, and backtraces are excluded. The
record remains stable across R sessions and can be embedded in a future run
manifest. B2 log entries contain the same `condition_id`, code, message, and
compact entity context, but the result never embeds a copied piplog row.

## Status Semantics

| Status | Required meaning |
|---|---|
| `success` | At least one unit committed successfully and no attempted unit failed; cached or skipped units may coexist |
| `partial` | At least one unit committed successfully and at least one independent unit failed |
| `failed` | At least one unit was attempted, no unit committed, and one or more units failed |
| `cached` | No unit executed because all selected outputs were verified current by C2 |
| `skipped` | No unit executed because selection, policy, or an upstream outcome excluded it |

`terminal` is orthogonal to status. A shared-state integrity failure after some
successful checkpoints produces a terminal partial result only if the outer
boundary is configured to capture it. With no committed success it produces a
terminal failed result. In both cases execution has already stopped.

Counts are derived as `selected`, `attempted`, `succeeded`, `failed`,
`skipped`, `cached`, `warnings`, and `errors`. For normal entity-based stages:

```text
attempted = succeeded + failed
selected = attempted + skipped + cached
```

## Invariants And Validation Rules

1. Constructors return canonical field names in canonical order and call the
   corresponding validator before returning.
2. Unknown top-level fields, stages, statuses, actions, and reason codes are
   rejected unless introduced by a new schema version.
3. Stage status and counts are computed from `units`; callers cannot supply a
   contradictory status or count.
4. `completed_at` is not earlier than `started_at`. Timestamps never determine
   cache freshness or dependency ordering.
5. `run_id`, release, identity, scope, and context hash agree between context,
   result, plan, and checkpoint references.
6. Successful artifact-backed units contain complete verified references and
   committed checkpoint provenance.
7. Failed units do not publish success receipts or advance C2 last-success
   records.
8. A clean unit with multiple outputs is successful only when C2 verifies and
   reconciles the complete expected output set.
9. `partial` never marks an incomplete survey output current.
10. C2 remains authoritative for dependency actions, reasons, hashes,
    currentness, receipts, lease fencing, and manifest publication.
11. B2 remains authoritative for persistent logs and reports. Results carry
    correlation references and normalized conditions, not log history.
12. Runtime handles, leases, loaded auxiliary objects, household data,
    inventories, catalogs, and raw conditions are absent from serialized
    projections.
13. Condition `details` values are bounded atomic scalars or vectors with stable
    names; arbitrary nested objects are rejected.
14. Serialization schema versions are validated before use. New readers reject
    unsupported future versions rather than guessing.
15. Fatal integrity conditions cannot be downgraded to recoverable entity
    failures by stage code.

## Logging Integration

C3 does not change the `piplog` table schema or create a second log store.

- `pipeline_context$logging$name` is always `"pipdata_log"` for pipdata stages.
- `run_id` and `condition_id` are added as compact logmeta correlation fields.
- Each entity failure is normalized once and logged once at the stage boundary.
- Each stage emits its existing summary discriminator once from the completed
  result counts.
- The deflation pilot restores the existing `deflate_summary_inf` contract on
  the active C2 path.
- Typed pipfun logging helpers are not called from frames containing household
  data or large inventories because caller-formal capture can retain them.
- `log_ref` may contain log name, run ID, summary discriminator, checkpoint
  alias, and exact checkpoint version when available. It never contains log
  rows.

## C1 And C2 Integration

- The context records the planner-resolved selection. It does not parse or
  reverse-map `force_surveys` independently.
- Existing `force` and `force_surveys` mutual exclusion and content-versioning
  semantics remain unchanged.
- C2 `actions`, `reasons`, `input_hash`, `code_hash`, exact input versions, and
  output receipts flow into unit outcomes and provenance without renaming.
- A result reports only committed successes. Receipt verification alone is not
  equivalent to a successful result until inventory reconciliation and manifest
  publication complete.
- Cached status comes only from C2 currentness. A stage cannot declare itself
  cached because a file happens to exist.
- C3 does not alter the C2 dependency-context shape, scope ID, manifest schema,
  checkpoint order, or production activation boundary.

## Compatibility And Migration Plan

| Existing surface | Current return | C3 compatibility behavior |
|---|---|---|
| `pipdata_get_gmd()` | invisible `NULL` | Keep unchanged; later delegate to typed acquisition runner |
| `pipdata_validate_gmd()` | invisible `NULL` | Keep unchanged; later delegate to typed validation runner |
| `pipdata_dlw_process()` | invisible `NULL` | Keep unchanged; later aggregate typed acquisition/validation results internally |
| `pd_process_data()` | updated master inventory | Keep unchanged; later unwrap typed clean/metadata outcomes |
| `pd_deflate_pipeline()` | updated master inventory | Keep unchanged; first adapter around typed deflate runner |
| `pd_deflation()` | deflated `data.table` or existing failure behavior | Keep unchanged; call from the typed worker |

New typed entry points are additive. The first deflation entry point may remain
internal while the contract is piloted. Promotion to an exported orchestration
API is deferred until at least two stages use the same validated contract.

No compatibility flag is added to existing wrappers. A flag that changes a
function's return type would create two contracts on one public surface and
make positional/implicit callers harder to reason about.

## Stage-By-Stage Adoption Strategy

### Phase 1: Contract Foundation

- Add context, result, artifact-reference, and condition-record constructors.
- Add strict validators, status/count derivation, compact print methods, and
  RDS projections.
- Reuse existing C2 constants rather than creating duplicate action/reason
  registries.

### Phase 2: Deflation Pilot

- Add one typed internal deflation-stage runner around existing C2 actions,
  exact-version loading, `pd_execute_deflate()`, verified receipts, and
  `pd_finalize_checkpoint()`.
- Continue to the next survey after a survey-scoped error.
- Aggregate committed, failed, skipped, and cached units into one result.
- Emit `deflate_summary_inf` once from result counts.
- Keep `pd_deflate_pipeline()` returning the updated master inventory through
  a compatibility adapter.

### Phase 3: Clean And Metadata

- Produce separate `clean` and `metadata` results because C2 already models
  them as distinct stages.
- Preserve multi-output clean atomicity and exact clean-to-metadata handoff.
- Keep `pd_process_data()` returning the updated master inventory.
- Restore existing cleaning summary/checkpoint behavior from compact result
  counts without duplicate logs.

### Phase 4: Acquisition And Validation

- Adapt acquisition and validation side-effect wrappers to internal typed
  results.
- Represent no-new-data as `cached` only when verified currentness exists;
  otherwise use `skipped` with a controlled reason.
- Keep exported wrapper returns as invisible `NULL`.

### Phase 5: Orchestrator Consumption

- Let a future top-level runner consume typed stage results directly.
- Add a run-level aggregate only when the orchestration design is planned.
- Do not redesign stage results when C4 adds step-level dependencies.

## Minimal First Implementation Slice

The first implementation is intentionally limited to:

1. `pipeline_context` constructor, validator, print method, and serializable
   projection.
2. `pipdata_stage_result` constructor, validator, print method, and
   serializable projection.
3. Stable artifact-reference and condition-record constructors.
4. Deterministic unit-to-count and unit-to-status aggregation.
5. A typed internal deflation runner and a legacy-return adapter in
   `pd_deflate_pipeline()`.
6. One compact B2 deflation summary emitted from the result.
7. Tests and documentation for the new contract and unchanged public return.

The slice does not modify acquisition, validation, cleaning, C2 manifest
schemas, or external packages.

## Serialization Requirements

- The canonical source object remains an R list with validated S3 class.
- The initial durable projection uses RDS serialization version 3, matching the
  package's current durable R workflow and avoiding a new JSON dependency.
- The projection includes `schema_version`, canonical field ordering, and UTC
  timestamps with explicit timezone.
- The projection strips S3 runtime handles and excludes raw conditions, calls,
  environments, external pointers, loaded data, catalogs, leases, and log rows.
- Hash maps are sorted by entity key before serialization.
- Artifact and unit tables use stable column order and explicit character,
  integer, logical, or UTC time types.
- Round-trip validation is mandatory before a result can be written as a
  future run-manifest artifact.
- A future JSON representation, if required by an external orchestrator, is a
  separate versioned projection and not part of the first slice.
- Result serialization is observational. It does not replace C2 manifests or
  certify artifact currentness.

## Test Strategy

### Contract Tests

- Required names, canonical order, classes, scalar types, and controlled values.
- Rejection of missing, unknown, malformed, or contradictory fields.
- Context consistency across release, identity, scope, and context hash.
- Compact print output that never prints data, full errors, or runtime state.

### Status And Count Tests

- All success, success plus cached/skipped, mixed success/failure, all failed,
  all cached, all skipped, and zero-selection cases.
- Terminal failure before any commit and terminal failure after prior committed
  units.
- Count equations and rejection of caller-supplied contradictions.

### Error Tests

- Survey-scoped deflation, load, and save failures are captured and the next
  survey executes.
- Stable condition codes, class vectors, messages, entity IDs, and bounded
  parent summaries survive an RDS round trip.
- Raw conditions, calls, environments, and backtraces are absent.
- Lease loss, context drift, parent-generation races, and checkpoint failures
  stop subsequent writes.

### C1/C2 Integration Tests

- `force_surveys` resolved actions and `forced` reasons appear unchanged.
- Cached units come from a C2 no-action/current plan.
- Exact data/metadata versions and hashes flow into deflation provenance.
- Only verified, reconciled, checkpointed receipts appear as successful
  artifact references.
- Failed units never advance C2 last-success records.

### B2 Logging Tests

- One entity failure produces one compact log entry linked by `condition_id`.
- One deflation result produces one `deflate_summary_inf` entry linked by
  `run_id`.
- Results do not contain piplog rows.
- Logging frames do not retain survey data or full inventories.

### Compatibility Tests

- Snapshot exported function formals and positional order.
- `pd_deflate_pipeline()` still returns the updated master inventory.
- No-action deflation still returns the supplied/current inventory.
- Existing `pd_deflation()` behavior remains unchanged.
- Existing deflation, dependency, logging, and full package tests pass.

### Serialization Tests

- RDS-v3 round trip preserves the canonical portable projection.
- Unsupported schema versions fail closed.
- Field/table/map ordering is deterministic.
- Serialized results contain no prohibited runtime or large-data classes.

## Acceptance Criteria

1. Both S3 classes have constructors, strict validators, compact print methods,
   and versioned serializable projections.
2. The deflation pilot returns correct typed results internally for success,
   partial, failed, cached, skipped, and terminal cases.
3. A survey failure never prevents later independent surveys from being
   attempted under the default entity error policy.
4. A shared-state integrity failure prevents every later write in that run.
5. Successful artifact references are exact, verified, reconciled, and tied to
   committed C2 checkpoint provenance.
6. B2 receives one compact per-failure entry and one stage summary without
   duplicated log objects or retained survey data.
7. C1 force selection and C2 actions/reasons/hashes are reused without a second
   resolver or cache system.
8. `pd_deflate_pipeline()` retains its public signature, positional order,
   side effects, and master-inventory return shape.
9. Portable context/result records pass deterministic RDS round-trip tests and
   reject unsupported schemas.
10. Targeted tests, the full package suite, generated documentation, and package
    check pass with no new errors or warnings.
11. The implementation does not claim production activation before C2's
    Windows/SMB fencing and unique-rename evidence is complete.

## Explicit Decisions Deferred To C4

- The step graph and `depends_on` topology.
- Whether load, PFW merge, recode, auxiliary attachment, save, and deflation
  are separate cache nodes.
- Per-step actions, reason codes, input hashes, output hashes, and code
  fingerprint ownership.
- Storage of step cache records in inventory columns or a separate artifact.
- Graph propagation, fan-in/fan-out, optional nodes, tombstones, and step-level
  resume semantics.
- Parallel and graph-aware scheduling.
- Whether acquisition and validation participate in the dependency DAG.
- Migration from C2's `clean`/`metadata`/`deflate` manifest to step-level state.
- Any generic DAG framework or external orchestrator selection.

The C3 result schema remains graph-neutral: it reports a controlled stage,
entity outcomes, references, conditions, and provenance without encoding graph
edges.

## Devil's Advocate Resolution

- **Problem validation:** Pre-validated by incompatible active return shapes,
  loss of structured failure detail, and missing summary emission on current C2
  cleaning/deflation paths.
- **Simplicity:** A plain validated list could work, but S3 is retained because
  constructors, validation, print behavior, and future coercion provide a real
  contract. Stage-specific subclasses are rejected.
- **Effort-value:** Full simultaneous adoption is rejected. The foundation and
  deflation pilot provide most architectural learning with a bounded regression
  surface.
- **Charter alignment:** The decision supports the charter's pipeline objective
  and R-package-quality constraint. The charter's Current Focus is stale and
  should later reflect C2/C3 orchestration work, but there is no conflicting
  constraint.

## Next Steps

1. Turn this decision into a phased implementation plan with the deflation pilot
   as the only first-stage adoption.
2. Re-read the active C2 result, receipt, reconciliation, and checkpoint shapes
   during planning; do not rely only on this schema summary.
3. Specify exact constructor names and exported/internal visibility in the plan.
4. Define the controlled stage-boundary skip reasons needed by acquisition and
   validation without extending C2 reason codes prematurely.
5. Add the compatibility and serialization tests before adapting deflation.
6. Keep C4 DAG decisions explicitly out of the C3 implementation diff.
