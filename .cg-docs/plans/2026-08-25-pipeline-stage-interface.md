---
date: 2026-08-25
title: "Implement Typed Pipeline Stage Interface"
status: completed
completed-date: 2026-08-26
completed-phases: [1, 2, 3]
scope: "Deep"
brainstorm: ".cg-docs/brainstorms/2026-08-25-pipeline-stage-interface.md"
language: "R"
estimated-effort: "large"
deviation-policy: "ask"
artifact-schema-version: 1
execution-report: ".cg-docs/work-reports/2026-08-26-pipeline-stage-interface.md"
tags: [pipeline, orchestration, interface, s3, context, results, deflation, provenance, logging, serialization]
phases: 3
---

# Plan: Implement Typed Pipeline Stage Interface

## Objective

Implement validated, versioned `pipeline_context` and
`pipdata_stage_result` S3 contracts and prove them on the batch deflation
stage. The typed path must capture independent survey failures and continue,
while shared-state integrity failures stop all later writes. Existing public
deflation signatures, side effects, and master-inventory returns remain
unchanged through a shared internal execution core. The sole additive legacy
side effect is restoration of the already-documented deflation summary for
attempted, completed runs.

## Context

The approved brainstorm selected lightweight S3 envelopes backed by ordinary
lists, artifact references by default, stable condition records, and a
deflation-first adoption slice. Acquisition, validation, clean, and metadata
adoption are deliberately deferred.

The prerequisite audit established that this branch is based on current
`origin/PROD`, C2 completed its full workflow cycle, C1 targeted forcing is
present, and B2 unified logging is active. C2 is the authority for dependency
context, stages, actions, reasons, exact input hashes, code fingerprints,
receipts, reconciliation, lease fencing, checkpoints, and currentness. B2 is
the authority for persistent logs. C3 must expose those facts without creating
a second cache or log store.

The active implementation has four planning-critical characteristics:

- `pd_deflate_pipeline()` already builds the C2 execution snapshot and plan,
  executes exact versions, persists failed invalidation, and publishes
  checkpoints.
- `pd_execute_deflate()` currently catches broad conditions and reduces
  failures to logs plus `NULL`, so stable failure details are unavailable to a
  caller.
- `pd_run_checkpoint_batches()` checkpoints only successful worker returns and
  discards aggregate outcomes. Any typed result accumulator must mark success
  only after the checkpoint callback commits.
- `pd_deflate_pipeline()` documents `deflate_summary_inf` but the active C2
  path returns without emitting it.

The result-bound checkpoint solution is authoritative: a write receipt alone
does not certify success. A successful stage unit must be tied to exact receipt
verification, inventory reconciliation, and committed manifest provenance.

The historical roadmap description names a generic `safe_pipeline_step()` as
a prerequisite. No such implementation exists. This plan uses a deflation-
specific safe boundary and does not create a generic helper unless a later
stage proves identical behavior.

Production activation remains outside this plan. C2 still requires the signed
target Windows/SMB fencing and immutable unique-rename smoke test described in
the repository documentation.

## Requirements

| ID | Requirement | Source |
|---|---|---|
| R1 | Define validated list-backed S3 classes `pipeline_context` and `pipdata_stage_result` without a new dependency | Approved brainstorm |
| R2 | Keep the existing C2 dependency context unchanged as a nested context field; runtime options must not alter its scope/hash | Brainstorm; C2 contract |
| R3 | Derive `success`, `partial`, `failed`, `cached`, and `skipped` stage status plus counts from normalized unit outcomes | Approved status semantics |
| R4 | Return compact verified artifact references by default; never place household data or inventories in stage results | User decision; memory constraint |
| R5 | Represent warnings/errors as stable serializable condition records without raw conditions, calls, environments, or backtraces | User decision |
| R6 | Continue processing independent surveys after survey-scoped load, deflation, or save failures | User objective |
| R7 | Propagate shared integrity failures and stop later writes; optionally capture them only at the outer typed run boundary after preparation/context construction succeeds | User clarification; C2 safety; plan review P2.4 |
| R8 | Mark a unit successful only after exact receipts, reconciliation, inventory writes, and manifest checkpoint publication complete | Result-bound checkpoint solution |
| R9 | Correlate results to B2 `pipdata_log`, log each failure once with explicit compact `args`, and emit one compact `deflate_summary_inf` for attempted completed runs without embedding log rows | B2 integration; plan review P1.4/P2.2 |
| R10 | Reuse C1/C2 resolved force selection, actions, reason codes, exact versions, input/code hashes, and receipts without another resolver or registry | C1/C2 integration |
| R11 | Preserve exported signatures, positional argument order, no-action behavior, durable side effects, and legacy return shapes; summary restoration is the only additive legacy side effect | Backward compatibility decision; plan review P2.2 |
| R12 | Provide canonical deterministic versioned RDS-v3 portable projections with fail-closed schema validation, without promising cross-R-version byte identity | Run-manifest readiness; plan review P2.7 |
| R13 | Limit initial stage adoption to deflation through one typed internal/new entry point and one legacy adapter | Minimal first slice |
| R14 | Keep logs/results compact and release large deflated objects before continuing | Existing OOM/log retention history |
| R15 | Document the internal contract, deflation behavior, status semantics, and production boundary accurately | R package standards |
| R16 | Exclude C4 DAG topology, step hashes, scheduling, step cache state, and manifest migration | Explicit C4 deferral |
| R17 | Do not claim production activation before C2's Windows/SMB evidence is complete | C2 execution boundary |
| R18 | Do not extract a generic `safe_pipeline_step()` during this slice without approved evidence of reuse | Simplicity check |

## Plan Review Resolution

The full `/cg-plan-review` on 2026-08-25 produced 6 P1, 11 P2, and 1 P3
findings. Its verification passes found three partial resolutions plus one new
P1 and two new P2 findings. This revision incorporates all 21 distinct findings
and closes the partial resolutions. The highest-impact changes are:

- exact top-level and nested schemas are frozen before implementation;
- a strict internal exact-deflation path preserves conditions while the public
  sentinel behavior remains unchanged;
- exact-input/hash/action and C2 infrastructure failures are explicitly fatal;
- C3 logging uses explicit compact `args` and suppresses duplicate legacy
  failure emission on the deflation path;
- pending units in a failed checkpoint are represented without claiming
  success, and later units become skipped after the terminal stop;
- artifact references are proven against the finalized current manifest, not a
  caller-supplied generation;
- every final artifact reference uses the retained latest manifest identity,
  avoiding references to pruned intermediate generations;
- preflight failures always propagate; typed fatal capture begins only after a
  full context exists;
- warnings are captured without changing legacy signaling;
- sentinel condition classes and terminal between-unit accounting are fully
  deterministic;
- fatal errors during an active pre-checkpoint entity are represented as
  `fatal_uncommitted` accounting rows without downgrading the stage failure;
- existing `checkpoint_seconds = Inf` timeout-disable behavior is preserved;
- prepared-context reuse is deferred and the core owns its lease;
- package-loaded targeted test commands replace standalone `test_file()` calls.

## Frozen Schema Appendix

The first implementation must use these exact schemas. Any field addition,
rename, type change, or relaxed bound requires a schema-version change and an
approved deviation.

### Constructor Signatures

```r
new_pipeline_context(
  execution,
  run_id,
  options,
  selection,
  storage,
  created_at = Sys.time(),
  runtime = NULL
)

validate_pipeline_context(x, portable = FALSE)

new_pipdata_stage_result(
  context,
  stage,
  terminal,
  units,
  artifacts,
  warnings = list(),
  errors = list(),
  log_ref,
  provenance,
  started_at,
  completed_at,
  data = NULL
)

validate_pipdata_stage_result(x, context = NULL, portable = FALSE)

new_stage_condition_record(
  condition = NULL,
  severity,
  code = NULL,
  message = NULL,
  classes = NULL,
  stage,
  entity_id = NA_character_,
  survey_id = NA_character_,
  pip_id = NA_character_,
  operation,
  recoverable,
  timestamp = Sys.time(),
  details = list()
)

validate_stage_condition_record(x, portable = FALSE)
```

The stage core must supply `context` to result validation so `run_id`, release,
identity, scope ID, context hash, and plan hash are checked together. A
standalone caller may omit it only for portable-record inspection.

### `pipeline_context`

Top-level field order and types are:

| Field | Exact in-memory type |
|---|---|
| `schema_version` | integer scalar `1L` |
| `run_id` | nonempty UTF-8 character scalar |
| `release` | nonempty UTF-8 character scalar |
| `identity` | character scalar in `PROD`, `INT`, `TEST` |
| `dependency_context` | unchanged validated C2 context list |
| `storage` | exact named list below |
| `options` | exact named list below |
| `selection` | exact named list below |
| `dependency` | exact named list below |
| `logging` | exact named list below |
| `created_at` | length-one `POSIXct` with UTC timezone |
| `runtime` | core-owned environment or `NULL` |

`storage` has exact order:

```text
aliases  named character vector ordered pip, pip_meta, pip_deflated,
         pip_master, pip_inv; values equal those alias names
roots    named normalized UTF-8 character vector in the same order, derived
         from the unchanged dependency_context
log_name character scalar "pipdata_log"
```

`options` has exact order and types:

```text
verbose               logical scalar
force                 logical scalar
force_surveys         sorted unique UTF-8 character vector
bootstrap             logical scalar
bootstrap_entities    sorted unique UTF-8 character vector
checkpoint_size       positive integer scalar
checkpoint_seconds    positive non-missing numeric scalar; Inf disables the
                      elapsed-time checkpoint trigger
entity_error_policy   "continue" or "abort"
fatal_error_policy    "abort" or "capture_at_run_boundary"
```

`selection` has exact order and sorted unique UTF-8 vectors:

```text
survey_id       character vector
pip_id          character vector
force_requested character vector
```

`dependency` has exact order:

```text
scope_id             nonempty character scalar
context_hash         nonempty character scalar
manifest_before      manifest identity or NULL
plan_hash            nonempty character scalar
stage_code_hashes    named character vector ordered by stage
snapshot_captured_at length-one POSIXct UTC
bootstrap            logical scalar
```

A manifest identity is an exact list ordered `filename`, `uuid`, `checksum`,
`generation`. The first three fields are nonempty character scalars;
`generation` is a finite positive whole-number numeric scalar.

`logging` has exact order `name`, `run_id`, both nonempty character scalars;
`name` is `"pipdata_log"` and the run ID equals the context run ID.

`plan_hash` is computed from a canonical projection containing only the
unchanged dependency context, sorted accepted actions, and sorted accepted
reasons after bootstrap restriction. It excludes `plan$snapshot`, loaded
objects, catalogs, inventories, receipts, auxiliary projections, and
observational timestamps.

The accepted-action projection always has this exact ordered character schema,
adding `NA_character_` for unavailable fields:

```text
stage, entity_id, survey_id, pip_id, action, input_hash, code_hash,
output_version_id, output_hash, data_version_id, data_hash,
metadata_version_id, metadata_hash
```

The accepted-reason projection always has exact ordered character fields
`stage`, `entity_id`, `reason`, `input`, `old`, `new`. Sort actions by
`stage`, `entity_id` and reasons by all six fields before hashing. Reordering
source rows or changing `captured_at` must not change the hash; changing an
action, reason, input hash, or code hash must change it.

### `pipdata_stage_result`

The exact top-level field order is:

```text
schema_version, stage, status, terminal, run_id, data, artifacts, units,
counts, log_ref, warnings, errors, provenance, input_hashes, output_hashes,
started_at, completed_at
```

`schema_version` is `1L`; `terminal` is a logical scalar; timestamps are
length-one `POSIXct` values in UTC; and `data` is required to be `NULL` for the
deflation pilot.

`counts` is an integer vector with exact ordered names:

```text
selected, attempted, succeeded, failed, skipped, cached, warnings, errors
```

`log_ref` has exact ordered fields:

```text
name                  character scalar "pipdata_log"
run_id                character scalar equal to result run_id
summary_discriminator character scalar "deflate_summary_inf" or NA_character_
log_checkpoint        NULL in this slice
```

Do not call or extend `pipfun::log_save_checkpoint()` for deflation. C2 manifest
identities belong only in result provenance.

`provenance` has exact order:

```text
release                nonempty character scalar
identity               PROD, INT, or TEST
scope_id               nonempty character scalar
context_hash           nonempty character scalar
plan_hash              nonempty character scalar
manifest_before        manifest identity or NULL
manifest_after         latest retained manifest identity or NULL
checkpoint_generations sorted unique finite positive whole-number numeric vector
stage_reason_codes     sorted unique UTF-8 character vector
```

For a zero-selection result, `stage_reason_codes` contains `no_selection`.

Allowed artifact `role` values are `primary`, `metadata`, `inventory`, and
`log`; deflation outputs use `primary`. In-memory unit/artifact tables use exact
schemas from Step 1 and UTC `POSIXct` timestamps. Portable `units` and
`artifacts` are plain named column lists in canonical column order, never
`data.table` or `data.frame`, so `.internal.selfref` cannot enter the record.

Unit columns use these exact types:

```text
stage          character
entity_id      character
survey_id      character (NA allowed)
pip_id         character (NA allowed)
status         character
action         character (NA allowed outside C2 stages)
reason_codes   list of sorted unique character vectors
input_hash     character (NA allowed when unavailable/not attempted)
output_hash    character (NA unless committed success/cached provenance exists)
started_at     POSIXct UTC (NA allowed only for unattempted skipped/cached)
completed_at   POSIXct UTC (NA allowed only for unattempted skipped/cached)
```

Artifact columns use these exact types:

```text
entity_id           character
alias               character
artifact            character
path                character
version_id          character
content_hash        character
role                controlled character
manifest_generation finite positive whole-number numeric
```

`input_hashes` and `output_hashes` are named UTF-8 character vectors sorted by
entity ID. Names are unique and match represented units; failed/unattempted
entities are omitted from `output_hashes` rather than represented by fake
hashes. `warnings` and `errors` are ordered lists of exact condition records,
sorted by `condition_id` in the portable projection.

Condition records limit `message` and `parent_message` to 4,096 UTF-8
characters, `classes` to 32 values, and `details` to 32 uniquely named atomic
fields. Each detail vector has at most 100 values and each character value has
at most 4,096 UTF-8 characters. Parent capture is one summary level only.

Condition records have this exact field order and types:

```text
schema_version integer scalar 1L
condition_id   nonempty UTF-8 character scalar
severity       "warning" or "error"
code           nonempty UTF-8 character scalar
classes        ordered UTF-8 character vector, 1 to 32 values
message        UTF-8 character scalar, at most 4,096 characters
stage          controlled pipeline stage character scalar
entity_id      character scalar; NA allowed for stage/preflight conditions
survey_id      character scalar; NA allowed
pip_id         character scalar; NA allowed
operation      nonempty UTF-8 character scalar
recoverable    logical scalar
timestamp      length-one POSIXct UTC
parent_code    character scalar; NA allowed
parent_message character scalar; NA allowed, otherwise at most 4,096 characters
details        exact bounded named atomic list
```

When `condition` is supplied, `classes` must be `NULL` and is derived from
`class(condition)`; explicit code/message may only override normalized values
deliberately. When `condition` is `NULL`, explicit `code` and `message` are
required for a sentinel record. Sentinel `classes = NULL` defaults exactly to
`unique(c(code, paste0("pipdata_stage_", severity), "condition"))`; an explicit
sentinel class vector must satisfy the same nonempty bounded UTF-8 validation.
Portable records convert the timestamp to an explicit UTC character scalar and
retain this exact order.

Portable projections recursively normalize declared field order, UTF-8
strings, unit/artifact order, reason vectors, condition order, details, and
named hash maps. In-memory `POSIXct` values become UTC character scalars.
Validation runs before write and after read. Determinism means identical RDS-v3
bytes for semantically equivalent canonical projections under the supported R
serialization contract; cross-R-version byte stability is not promised.

### Status Precedence

Apply this order exactly:

1. `terminal && succeeded > 0` produces `partial`; terminal with no success
   produces `failed`.
2. Nonterminal success plus failure produces `partial`.
3. Nonterminal success without failure produces `success`.
4. Nonterminal attempts with no success produce `failed`.
5. No attempts with all selected units cached and no skips produces `cached`.
6. No attempts with any skipped units, including cached plus skipped, produces
   `skipped`.
7. Zero selected produces `skipped` with stage reason `no_selection`.

## Phase 1: Portable Contract Foundation

### 1. Define Controlled Vocabularies And Empty Schemas

- **Requirements**: R1, R3, R10, R16
- **Files**: `R/pipeline_stage_result.R`, `tests/testthat/test-pipeline-stage-result.R`
- **Details**:
  1. Define orchestration stage vocabulary as `acquisition`, `validation`, and
     the existing `.PD_STAGES`. Do not modify `.PD_STAGES` or C2 validators.
  2. Define aggregate statuses `success`, `partial`, `failed`, `cached`, and
     `skipped`; unit statuses exclude `partial`.
  3. Define exact column/type constructors for unit outcomes and artifact
     references. Use `data.table` and explicit empty column types.
  4. Unit rows contain `stage`, `entity_id`, `survey_id`, `pip_id`, `status`,
     `action`, `reason_codes`, `input_hash`, `output_hash`, `started_at`, and
     `completed_at`.
  5. Artifact rows contain `entity_id`, `alias`, `artifact`, `path`,
     `version_id`, `content_hash`, `role`, and `manifest_generation`.
  6. Add one stage-boundary reason vocabulary for interface-only outcomes such
     as `no_selection`, `current`, `upstream_failed`, `policy_excluded`,
     `checkpoint_uncommitted`, and `fatal_uncommitted`.
     Keep it separate from `.PD_REASON_CODES`; never pass these values to C2
     manifest validation.
  7. Reject duplicate `(stage, entity_id)` unit keys, unknown controlled
     values, malformed list columns, and non-scalar IDs.
- **Test Scenarios**:
  - Happy path: valid empty and populated unit/artifact tables preserve exact
    type and column order.
  - Edge case: `NA_character_` is accepted only for inapplicable survey/pip
    identifiers, not for stage/entity/status.
  - Error path: duplicate keys and mixed C2/interface reason registries fail
    with typed `pipdata_stage_result_invalid` conditions.
- **Tests**: `devtools::test(filter = "pipeline-stage-result")`
- **Acceptance criteria**: Controlled values and table schemas are centralized,
  strict, and do not change any C2 constant or schema.

### 2. Add Stable Condition And Artifact Reference Records

- **Requirements**: R4, R5, R7, R8, R9, R12, R14
- **Files**: `R/pipeline_stage_result.R`, `tests/testthat/test-pipeline-stage-result.R`
- **Details**:
  1. Add an internal constructor for a versioned condition record containing
     `condition_id`, severity, stable code, ordered classes, message, stage,
     entity identifiers, operation, recoverability, UTC timestamp, bounded
     parent code/message, and allowlisted atomic details.
  2. Generate `condition_id` with the existing `pd_random_id()` helper. The ID
     is correlation metadata, not a reproducibility input or cache key.
  3. Derive a domain code by excluding generic R/rlang condition classes while
     retaining the most specific stable package class. Permit an explicit code
     override for sentinel failures such as `deflation_na` and
     `deflate_save_error`.
  4. Limit parent capture to stable code/message summaries. Do not retain the
     parent object, call, environment, trace, or arbitrary condition fields.
  5. Validate `details` recursively as a named list of bounded atomic scalars or
     vectors. Reject nested data frames, environments, functions, and external
     pointers.
  6. Add `new_artifact_reference()` accepting a receipt plus finalized
     execution/manifest evidence, stage, entity, and role. Do not accept a
     caller-supplied generation scalar.
  7. Verify that the finalized current manifest contains the exact
     stage/entity record and a matching output receipt with the same alias,
     artifact, path, version ID, and content hash. Derive generation from
     `finalized$execution$manifest_identity`.
  8. Require every serialized artifact generation to be represented by result
     checkpoint provenance. Reject a fabricated generation, a receipt absent
     from manifest records, and any receipt/manifest mismatch.
  9. Keep condition records canonical in results. B2 logs receive the same
     `condition_id`, code, message, and compact entity fields, but no copied
     piplog row is stored in the result.
- **Test Scenarios**:
  - Happy path: a nested `piperr` becomes a stable bounded record and survives
    RDS round trip.
  - Edge case: no parent, unknown non-piperr, sentinel failure, and character
    vector details.
  - Error path: raw condition in `details`, missing receipt hash, fabricated
    generation, manifest-absent receipt, or mismatched receipt is rejected.
- **Tests**: `devtools::test(filter = "pipeline-stage-result")`
- **Acceptance criteria**: Stable records preserve reviewable failure context
  across sessions without retaining runtime objects; artifact references cannot
  represent uncommitted success.

### 3. Implement `pipeline_context`

- **Requirements**: R1, R2, R5, R7, R10, R12, R14, R17
- **Files**: `R/pipeline_context.R`, `tests/testthat/test-pipeline-context.R`
- **Details**:
  1. Generate `run_id` before `pd_prepare_execution()` and capture relevant
     planning/entity warnings with scoped `withCallingHandlers()` into stable
     records. Preserve normal warning signaling for the legacy wrapper; do not
     create a second B2 log entry solely because a warning was captured.
  2. Implement internal `new_pipeline_context()` and
     `validate_pipeline_context()` constructors/validators plus
     `print.pipeline_context()`.
  3. Use canonical fields from the Frozen Schema Appendix: schema version, `run_id`,
     release, identity, unchanged `dependency_context`, storage descriptors,
     execution options, resolved selection, dependency references, B2 logging
     reference, creation time, and optional runtime handle.
  4. Create the full context after `pd_prepare_execution()` so dependency fields can
     contain exact scope/context hash, manifest identity, accepted plan hash,
     named stage code hashes, snapshot time, and bootstrap state.
  5. Define the capture boundary explicitly: preparation, context validation,
     and lease acquisition failures always propagate and return no partial
     context/result. `capture_at_run_boundary` starts only after full context
     construction succeeds.
  6. Store aliases and normalized roots as compact character descriptors only.
     Never store catalogs or loaded objects in the portable descriptor.
  7. Put the prepared C2 execution state in an internal runtime environment so
     checkpoint callbacks can replace the current execution identity without
     mutating descriptor identity.
  8. Enforce `force`/`force_surveys` mutual exclusion and bootstrap selector
     rules, but do not resolve identifiers. Record resolved selection from the
     accepted C2 plan/reasons.
  9. Support `entity_error_policy = c("continue", "abort")` and
     `fatal_error_policy = c("abort", "capture_at_run_boundary")`. Both fatal
     policies stop execution; capture changes only the outer return behavior.
  10. Compute `plan_hash` only from the canonical accepted-plan projection in
       the Frozen Schema Appendix. Test that row reordering and `captured_at`
       changes are irrelevant while action/reason/input/code changes alter it;
       assert the exact action/reason projection columns and inserted NA values.
  11. Recompute and compare the nested C2 context hash during validation to prove
     it has not been widened or changed.
- **Test Scenarios**:
  - Happy path: context built from a synthetic C2 execution validates and
    prints only compact descriptor information.
  - Edge case: empty force selection, TEST/INT/PROD identities, absent prior
    manifest identity, and runtime omitted from a portable context.
  - Error path: preflight failure propagates without a typed result; mismatched
    release/scope, modified nested context, invalid policies, and conflicting
    force controls fail closed.
- **Tests**: `devtools::test(filter = "pipeline-context")`
- **Acceptance criteria**: One context can be shared by the typed stage while
  its portable projection preserves C2 identity and excludes runtime state.

### 4. Implement `pipdata_stage_result`, Aggregation, Printing, And Serialization

- **Requirements**: R1, R3, R4, R5, R8, R9, R12, R14
- **Files**: `R/pipeline_stage_result.R`, `tests/testthat/test-pipeline-stage-result.R`, `NAMESPACE`
- **Details**:
  1. Implement internal `new_pipdata_stage_result()` and
     `validate_pipdata_stage_result()` plus
     `print.pipdata_stage_result()` with roxygen S3 registration.
  2. Use this exact top-level field order and spelling: `schema_version`,
     `stage`, `status`, `terminal`, `run_id`, `data`, `artifacts`, `units`,
     `counts`, `log_ref`, `warnings`, `errors`, `provenance`, `input_hashes`,
     `output_hashes`, `started_at`, `completed_at`.
  3. Derive counts from units. For entity stages enforce
     `attempted = succeeded + failed` and
     `selected = attempted + skipped + cached`.
  4. Derive aggregate status with the exact Status Precedence table in the
     Frozen Schema Appendix. Store zero-selection and other stage-level reasons
     in `provenance$stage_reason_codes`.
  5. Treat `terminal` as orthogonal while applying the precedence table.
  6. Default `data` to `NULL`. Reject data frames, `data.table`, inventories,
     catalogs, piplog objects, environments, functions, and external pointers
     from both the in-memory `data` field and portable projection.
  7. Implement portable unit/artifact tables as plain named canonical column
     lists, never `data.table`/`data.frame`, and recursively canonicalize all
     declared nested ordering and UTF-8 content.
  8. Validate the canonical portable projection before writing and after
     reading. Compare `serialize(projection, NULL, version = 3L)` with
     `identical()` for semantically equivalent permuted inputs. Do not promise
     cross-R-version byte stability.
  9. Add recursive prohibited-class/type checks. Unsupported future schema
     versions abort rather than attempting compatibility guesses.
  10. Print only class/schema, stage/status/terminal, timing, counts, and compact
     condition codes. Never print paths, payloads, full messages, or runtime.
- **Test Scenarios**:
  - Happy path: all status combinations produce canonical counts and compact
    output.
  - Edge case: all seven status-precedence cases, terminal after prior committed
    success, zero warnings/errors, and permuted semantically equivalent input.
  - Error path: caller-supplied contradictory count/status, timestamp reversal,
    prohibited data, mismatched run ID, or unknown schema version.
- **Tests**: `devtools::test(filter = "pipeline-stage-result")`
- **Acceptance criteria**: Result construction is deterministic, invalid states
  are unrepresentable through constructors, and RDS-v3 portable records
  validate after round trip.

## Phase 2: Deflation Pilot

### 5. Extract One Shared Deflation Execution Core

- **Requirements**: R2, R5, R8, R10, R11, R13, R18
- **Files**: `R/pd_deflate_pipeline.R`, `R/pipeline_context.R`, `tests/testthat/test-pd-deflate-pipeline.R`
- **Details**:
  1. Extract an internal `pd_deflate_pipeline_core()` from the active public
     wrapper. It owns inventory loading/normalization, C2 context/execution,
     lease lifetime, action validation, unit execution, checkpoints, result
     aggregation, and current master state.
  2. Generate the run ID before preparation and retain normalized preflight
     warnings locally. If preparation succeeds, pass those warnings into the
     final typed result; if preparation fails, propagate the failure and return
     no partial result.
  3. Return an internal execution bundle with exact names `result`, `master`,
     and `context`. This bundle is not a public class and is never serialized.
  4. Add internal/new `pd_run_deflate_stage()` that accepts append-only context
     options, calls the core, and returns only the typed `result`. It does not
     accept a caller-prepared runtime context in this pilot.
  5. Make public `pd_deflate_pipeline()` call the same core and return only
     `master`, preserving current formals and positional ordering exactly.
  6. Keep public wrapper defaults and bootstrap validation in their current
     order. The legacy wrapper uses fatal abort behavior.
  7. The core always creates, owns, and releases its C2 execution/lease. Preserve
     a primary condition during cleanup: attempt lease release, but never let a
     release error replace the condition already being propagated or captured.
  8. Allow typed append-only options, including C1/C2 `force_surveys`, without
     appending a new public wrapper argument in this slice.
  9. Build selected/cached unit rows from the accepted C2 snapshot and plan.
     Deflate entities with no action are `cached` only when C2 currentness says
     they are current. An empty eligible universe is `skipped` with
     `no_selection`.
  10. Do not modify `pd_run_checkpoint_batches()` unless the existing helper
     cannot support a deflation-local outcome accumulator. Prefer a local
     accumulator and checkpoint callback over a generic helper redesign.
- **Test Scenarios**:
  - Happy path: public and typed paths share one mocked core execution; public
    returns master and typed returns S3 result.
  - Edge case: no actions with current entities is cached; empty inventory is
    skipped; absent manifest with approved bootstrap; unknown `force_surveys`
    preserves the original warning and adds one normalized warning record.
  - Error path: invalid exact action inputs abort before any write; lease-release
    failure never masks an earlier primary condition.
  - Compatibility: `options(pipdata.manifest_checkpoint_seconds = Inf)` builds
    a valid context and disables elapsed-time checkpoints as before C3.
- **Tests**: `devtools::test(filter = "pd-deflate-pipeline|pipeline-context")`
- **Acceptance criteria**: There is one execution implementation, no inventory
  is stored in the stage result, and public return compatibility is exact.

### 6. Normalize Survey Failures Without Swallowing Integrity Failures

- **Requirements**: R5, R6, R7, R9, R14, R18
- **Files**: `R/pd_deflate_pipeline.R`, `R/pd_deflation.R`, `R/pd_process_data.R`, `R/code_fingerprint.R`, `R/pipeline_stage_result.R`, `tests/testthat/test-pd-deflate-pipeline.R`, `tests/testthat/test-pd-deflation.R`, `tests/testthat/test-pd_process_data.R`, `tests/testthat/test-code-fingerprint.R`
- **Details**:
  1. Refactor `pd_execute_deflate()` into a unit worker that returns either the
     existing checkpoint-ready success fields or a structured recoverable
     failure carrying one condition record. Do not return bare `NULL` for a
     known survey failure.
  2. Add an internal strict exact-deflation path used only by
     `pd_execute_deflate()`. It reuses exact version/hash loading but lets
     deflation conditions propagate without invoking `safe_deflation()` or
     legacy `log_failure()`. Keep exported `pd_deflation()` and public
     `deflation.*()` formals and sentinel behavior unchanged.
  3. Add the strict helper to the curated deflate fingerprint closure if it is
     not already reached. Record and test that changing this value-affecting
     path changes only the expected C2 deflate code fingerprint and may produce
     `deflate_code_changed` actions. Audit the logging-only failed-invalidation
     change so it does not spuriously enter unrelated value-stage fingerprints.
  4. Keep exact action validation and execution fences outside broad entity
     error handlers whenever possible.
  5. Apply an explicit classification matrix. Recoverable outcomes are
     allowlisted entity-domain transform failures and returned
     `success = FALSE` save receipts. Fatal conditions include
     `pd_deflation_exact_hash`, `pipdata_deflation_action_invalid`,
     `pipdata_dependency_*` snapshot/integrity classes,
     `pipdata_manifest_*`, `pipdata_receipt_invalid`,
     `pipdata_receipt_stale`, `pipdata_checkpoint_*`, and
     `pipdata_failed_invalidation_*`. Unrecognized errors from integrity
     operations are fatal by default.
  6. Preserve the most specific recoverable `piperr` class and bounded parent
     summary. Represent a non-`data.table` strict-path sentinel as
     `deflation_na` only when no condition exists.
  7. Preserve lease assertion immediately before the artifact write. If a
     write-side catch is needed, re-signal lease/context/manifest integrity
     classes explicitly and normalize only returned receipt/write failures as
     `deflate_save_error`.
  8. Leave `pd_persist_failed_invalidation()` and checkpoint publication
     outside recoverable handlers. Their write/fence errors remain fatal.
  9. Add internal `emit_log = TRUE` to `pd_invalidate_failed_action()` and use
     `FALSE` only from the deflation failed-invalidation path. Existing
     clean/metadata callers retain default legacy logging.
  10. The stage core logs a normalized failure exactly once. Every C3
      `pipfun::log_add()` call passes explicit compact
      `args = list(run_id, stage, entity_id, condition_id)`; never rely on
      caller-formal capture. Apply the same explicit-args rule to the stage
      summary.
  11. Release the deflated `data.table` before creating the compact outcome and
     preserve the existing GC threshold behavior.
  12. Under `entity_error_policy = "continue"`, append the failure and proceed.
     Under `"abort"`, signal after recording/logging the condition.
- **Test Scenarios**:
  - Happy path: first survey fails deflation, second succeeds, and both outcomes
    are present in order.
  - Edge case: specific `add_ppp`/`add_cpi` nested `piperr`, generic survey
    error, sentinel without a condition, and returned save failure.
  - Error path: exact hash drift, malformed action, unknown fence error, lease
    loss, and stale snapshot escape the worker and no later write executes.
  - Compatibility: direct public `pd_deflation()` retains its legacy sentinel;
    existing clean/metadata failed-action calls still emit their default log.
- **Tests**: `devtools::test(filter = "pd-deflate-pipeline|pd-deflation|pd_process_data|code-fingerprint")`
- **Acceptance criteria**: Independent failures remain inspectable and do not
  stop later units; integrity failures cannot be reduced to failed unit rows.

### 7. Bind Success To Checkpoints And Emit The B2 Summary

- **Requirements**: R3, R4, R7, R8, R9, R10, R11, R12, R17
- **Files**: `R/pd_deflate_pipeline.R`, `R/pipeline_stage_result.R`, `R/log_report.R`, `tests/testthat/test-pd-deflate-pipeline.R`, `tests/testthat/test-log_report.R`, `tests/testthat/test-logging-integration.R`
- **Details**:
  1. Maintain a deflation-local outcome accumulator. A worker success remains
     pending until the checkpoint callback returns successfully.
  2. In each checkpoint callback, pass only verified success rows to
     `pd_finalize_checkpoint()`. After it succeeds, update the current master,
     replace runtime execution identity, and mark the corresponding units
     committed.
  3. If a checkpoint fails, mark every member of that pending batch as unit
     status `failed`, reason `checkpoint_uncommitted`, with no output hash or
     artifact reference. Store the shared fatal condition once at stage level
     with `recoverable = FALSE`; these rows are accounting and never downgrade
     the terminal failure. Mark later selected-but-unattempted units `skipped`
     with `upstream_failed`.
  4. If a fatal condition occurs while an entity attempt is active but before
     it enters a pending checkpoint batch, add one failed accounting row for
     that entity with reason `fatal_uncommitted`, no output hash/artifact, and
     retain the shared fatal condition only at stage level. A fatal condition
     outside an active entity does not create a synthetic failed unit. For every
     terminal failure after selection is established, mark all remaining
     selected action units that were never attempted as `skipped` with
     `upstream_failed`, regardless of whether a unit was active.
  5. Mark units committed after each successful callback, but delay final
     artifact-reference construction until normal or captured-terminal result
     construction.
  6. Revalidate every committed receipt against the latest current execution
     manifest and assign that retained manifest identity/generation to all
     committed references. This latest manifest contains the accumulated
     last-success records and avoids references to pruned intermediate
     generations. Never use pre-execution snapshot receipts as current-attempt
     output evidence.
  7. Carry each action's C2 input hash, code hash, action, and reason codes into
     unit/result provenance without renaming C2 values.
  8. Preserve failed deflation invalidation writes. A successful failure-
     invalidation write is not a successful deflation artifact and does not
     advance last-success result provenance.
  9. If a fatal condition occurs after context construction and policy is
     `"abort"`, re-signal it. If the typed boundary uses
     `"capture_at_run_boundary"`, stop execution, then
     construct a terminal `failed` or `partial` result from already committed
     units and the fatal stable condition record.
  10. Build the final stage result only after all nonfatal units and pending
     checkpoints complete.
  11. Preserve no-action behavior: cached/skipped runs with no attempts emit no
      `deflate_summary_inf`. Nonterminal runs with `attempted > 0` emit one.
      `n_total = attempted`, `n_success = succeeded`, `n_failed = failed`, and
      success/failure survey vectors contain attempted deflate `pip_id` values;
      selected/cached/skipped are additive fields.
  12. A captured terminal result emits one terminal summary only after all
      stage writes have stopped. Abort-mode fatal runs emit no completion
      summary. Every summary passes explicit compact `args`.
  13. Update `log_report()` to select the latest applicable deflation summary,
      not the first matching entry, and test repeated runs.
  14. Emit `deflate_summary_inf` from compact scalar/vector fields,
     retaining documented keys `n_total`, `n_success`, `n_failed`,
     `surveys_success`, and `surveys_failed`; add `run_id`, status, cached, and
     skipped counts without changing the piplog table schema.
  15. Set `log_ref` exactly as frozen: name, run ID, summary discriminator, and
      `log_checkpoint = NULL`. C2 identities remain only in provenance. Do not
      call or extend `pipfun::log_save_checkpoint()` for deflation.
- **Test Scenarios**:
  - Happy path: successful receipts become artifact refs only after callback
    returns and carry the committed generation.
  - Edge case: mixed committed/failed/cached units yield `partial`; at least
    five checkpoint batches all reference the retained latest manifest; repeated
    runs render the latest summary.
  - Error path: a multi-unit pending checkpoint fails after worker receipts;
    pending rows become failed/uncommitted, later rows become skipped, one fatal
    record exists, no pending artifacts exist, and no later writes occur.
  - Fatal-active-unit path: exact hash drift and lease loss on the first and a
    later unit produce `fatal_uncommitted`, valid counts, one stage fatal record,
    and no synthetic row for a failure outside an active unit.
  - Fatal-between-units path: a failure before the first worker and between two
    workers marks every remaining selected action unit `upstream_failed` and
    preserves `selected = attempted + skipped + cached`.
- **Tests**: `devtools::test(filter = "pd-deflate-pipeline|log_report|logging-integration")`
- **Acceptance criteria**: Typed success is result-bound and checkpoint-bound,
  while the B2 summary is emitted once with no log duplication or large caller
  objects.

## Phase 3: Compatibility, Documentation, And Verification

### 8. Lock Compatibility And Cross-Contract Behavior

- **Requirements**: R3, R6, R7, R8, R10, R11, R13, R14
- **Files**: `tests/testthat/test-pipeline-context.R`, `tests/testthat/test-pipeline-stage-result.R`, `tests/testthat/test-pd-deflate-pipeline.R`, `tests/testthat/test-pd-deflation.R`, `tests/testthat/test-pd_process_data.R`, `tests/testthat/test-code-fingerprint.R`, `tests/testthat/test-dependency-api-contract.R`, `tests/testthat/test-logging-integration.R`, `tests/testthat/test-log_report.R`
- **Details**:
  1. Snapshot `formals(pd_deflate_pipeline)` and assert positional ordering and
     defaults against the pre-C3 public contract.
  2. Assert public all-success, partial, all-failed, no-action, empty, force,
     and bootstrap paths still return the updated/current master inventory.
  3. Assert typed and legacy entry points produce the same durable side effects
     from one core execution; do not run both against the same live action set
     in one test.
  4. Add a full status aggregation matrix and cross-check counts against unit
     rows.
  5. Add force integration tests proving accepted C2 actions/reasons/hashes are
     copied, not re-resolved or recomputed by C3.
  6. Add warning integration tests proving unknown forced identifiers preserve
     legacy signaling and produce exactly one normalized warning record/count.
  7. Add logging tests proving one failure entry, one stage summary, stable
     correlation IDs, unchanged existing report sections, and no piplog rows in
     results. Inspect persisted `args` and `logmeta` recursively and assert no
     inventory or `data.table` object is retained.
  8. Add recursive object tests proving portable results contain no prohibited
     types/classes and compact print methods do not expose paths or messages.
  9. Add call-count tests showing a recoverable failure continues and a fatal
     failure prevents every subsequent worker/checkpoint/write.
  10. Assert direct public `pd_deflation()` retains legacy sentinel behavior
      while the strict internal exact path preserves a specific condition.
- **Test Scenarios**:
  - Happy path: typed result and legacy return satisfy their distinct contracts
    over the same mocked core behavior.
  - Edge case: mixed cached/skipped entities, case-insensitive forced IDs, and
    unknown force warning as supplied/signaled by the C2 planner.
  - Error path: duplicate logging, pre-checkpoint success, changed formals, or
    prohibited serialized content fails a targeted test.
- **Tests**: `devtools::test(filter = paste("pipeline-context", "pipeline-stage-result", "pd-deflate-pipeline", "pd-deflation", "pd_process_data", "code-fingerprint", "logging-integration", "log_report", "dependency-api-contract", sep = "|"))`
- **Acceptance criteria**: Compatibility and integration requirements are
  executable tests, not documentation-only claims.

### 9. Document The Contract And Regenerate Package Surfaces

- **Requirements**: R9, R11, R12, R15, R16, R17
- **Files**: `R/pipeline_context.R`, `R/pipeline_stage_result.R`, `R/pd_deflate_pipeline.R`, `R/pd_deflation.R`, `man/pd_deflate_pipeline.Rd`, `NAMESPACE`, `NEWS.md`, `compound-gpid.context.md`
- **Details**:
  1. Add concise roxygen comments for S3 print methods and internal constructors;
     keep the first typed entry point internal/`@noRd` until another stage
     adopts the contract.
  2. Update `pd_deflate_pipeline()` details to describe internal typed outcomes,
     per-survey continuation, fatal integrity propagation, and actual summary
     emission while preserving the documented public return.
  3. Document the strict internal exact-deflation path and explicitly state that
     public `pd_deflation()`/`deflation.*()` sentinel behavior is unchanged.
  4. Document status semantics, portable projection limits, condition records,
     B2 correlation, C2 authority, and the no-large-data rule in tactical
     project context.
  5. Add a NEWS entry describing the internal contract foundation, restored
     deflation summary, and unchanged public API.
  6. Regenerate NAMESPACE/Rd files with roxygen. Do not hand-edit generated
     files.
  7. Keep the C2 production activation warning intact and do not imply that C3
     satisfies the outstanding Windows/SMB smoke test.
  8. Do not update `compound-gpid.md` in this implementation. Its stale Current
     Focus is a separate charter decision.
- **Test Scenarios**:
  - Happy path: generated docs describe actual signatures and returns.
  - Edge case: internal entry point does not appear as an unintended exported
    function.
  - Error path: stale Rd/NAMESPACE or an activation claim fails final checks.
- **Tests**: `devtools::document()`; targeted documentation/NAMESPACE assertions
- **Acceptance criteria**: Generated surfaces are current, internal visibility
  is intentional, and operator-facing compatibility remains accurate.

### 10. Run Final Verification And Scope Audit

- **Requirements**: R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12,
  R13, R14, R15, R16, R17, R18
- **Files**: all files changed by this plan; no additional implementation scope
- **Details**:
  1. Run the exact package-loaded targeted command from Completion Contract V11.
  2. Run the full package test suite and record pass/fail/skip counts in the
     execution report.
  3. Run package documentation generation and package check. Compare any notes
     with the C2 baseline rather than silently classifying new notes as old.
  4. Execute a scope audit that asserts no C2 manifest/context fields changed,
     no new package dependency appeared, no existing exported formal changed,
     no C4 graph/hash/cache code was added, and no external package file was
     modified.
  5. Execute serialization audits over representative success, partial,
     cached, failed, and terminal results.
  6. Inspect final git status/diff and ensure only intended implementation,
     tests, generated docs, NEWS, tactical context, and workflow artifacts are
     present. Do not modify unrelated concurrent changes.
- **Test Scenarios**:
  - Happy path: all required evidence passes and the execution report links
    exact commands/results.
  - Edge case: pre-existing notes are recorded separately from new behavior.
  - Error path: any required evidence failure blocks completion unless the user
    explicitly accepts an exception under the goal-execution contract.
- **Tests**: commands in Completion Contract Verification Surface
- **Acceptance criteria**: All required evidence passes, no constraint is
  violated, and no out-of-scope implementation is present.

## Testing Strategy

Testing follows a micro/mezzo/macro sequence:

1. Contract constructors and validators are exercised with direct unit tests
   before deflation changes.
2. Deflation uses synthetic inventories, mocked C2 plans/receipts/checkpoints,
   and ordered worker/write counters. Tests must not require household data or
   production storage.
3. Failure injection occurs at deflation, receipt, failed invalidation,
   checkpoint, lease, context, and parent-generation boundaries.
4. Serialization tests use temporary files via `withr` and RDS version 3.
5. Logging tests capture compact arguments and verify exact count, discriminator,
   condition ID, and run ID behavior.
6. Compatibility tests pin formals and return classes/shapes before and after
   the shared core extraction.
7. Phase-level evidence uses package-loaded `devtools::test(filter = ...)` only;
   final evidence uses full tests and package check. Do not use standalone
   `testthat::test_file()` for tests of unexported package functions.

No live production run or Windows/SMB activation test is part of this plan.

## Documentation Checklist

- [ ] Internal constructors and validators have concise roxygen comments.
- [ ] S3 print methods are registered through roxygen/NAMESPACE.
- [ ] `pd_deflate_pipeline()` documents the unchanged public return.
- [ ] Public `pd_deflation()` sentinel behavior and the internal strict path are
  distinguished explicitly.
- [ ] Status and terminal semantics are documented.
- [ ] Stable condition and artifact-reference schemas are documented.
- [ ] B2 correlation, explicit compact `args`, no-log-copy behavior, and latest
  summary selection are documented.
- [ ] C1/C2 authority and no duplicate force/cache logic are documented.
- [ ] RDS-v3 portable projection and prohibited runtime content are documented.
- [ ] NEWS records the internal contract and restored summary behavior.
- [ ] Tactical context records the new convention.
- [ ] C2 production activation caveat remains explicit.
- [ ] No C4 behavior is described as implemented.

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| Broad worker handlers swallow lease/context/checkpoint failures and continue unsafe writes | Medium | Critical | Move fences outside broad catches, re-signal explicit integrity classes, and use later-write counters |
| A worker receipt is labeled success before checkpoint publication | High | Critical | Keep success pending; mark committed only in successful checkpoint callback with manifest generation |
| Exact pinned-input hash drift is misclassified as a recoverable survey failure | Medium | Critical | Fatal classification matrix with exact-hash/action/infrastructure failure tests |
| Pending receipts from a failed checkpoint disappear from counts or appear successful | Medium | Critical | Mark pending batch failed/uncommitted, later units skipped, and store one terminal condition |
| Artifact references point to pruned intermediate manifest generations | Medium | High | Revalidate all committed receipts against the retained latest manifest at result construction |
| Shared core extraction changes public deflation returns or no-action behavior | Medium | High | Internal `{result, master, context}` bundle plus formals/return snapshot tests |
| Strict exact deflation changes public sentinel behavior or misses C2 fingerprinting | Medium | High | Separate internal strict path, direct-public regression tests, and code-hash mutation test |
| Cached and skipped semantics are inferred from file existence instead of C2 currentness | Medium | High | Build unit universe from accepted C2 snapshot/plan and test no-action/empty/bootstrap cases |
| Result or logging retains large survey/inventory objects | Medium | High | Prohibit classes recursively, log from compact frames, remove data before result creation, test object structure |
| Legacy failed-invalidation logging duplicates C3 errors and captures the master inventory | High | High | Deflation-only `emit_log = FALSE`, explicit compact `args`, and persisted-log object assertions |
| Condition serialization retains calls/environments through nested fields | Medium | High | Rebuild allowlisted plain records; reject raw/nested objects; RDS round-trip tests |
| Runtime options are added to C2 context and alter scope identity | Low | Critical | Nest unchanged C2 context and verify its hash/scope during validation |
| Whole-plan hashing retains snapshots or changes with timestamps/row order | Medium | High | Hash only canonical context/actions/reasons projection and test semantic invariance |
| Warning signals are visible to callers but missing from typed results | Medium | Medium | Scoped warning capture before preparation while preserving legacy signaling |
| Preflight failure is incorrectly represented as a typed terminal stage result | Low | High | Start fatal capture only after preparation and full context construction |
| Duplicate failure logs appear during worker-to-stage migration | Medium | Medium | Normalize/log once at stage boundary and assert exact event counts |
| Generic helper extraction expands scope or constrains later stages | Medium | Medium | Keep deflation-local accumulator; `ask` approval required before generic extraction |
| New docs imply C2 is production-active | Low | High | Preserve activation caveat and add final assertion/scope audit |
| Deep plan produces an unreviewable diff | Medium | Medium | Three evidence-gated phases and no adoption beyond deflation |

### Residual Risks After Review

- Production remains blocked on the signed Windows/SMB fencing and immutable
  unique-rename evidence.
- C2 writes inventories before manifest publication. A manifest failure can
  leave durable inventory ahead of provenance; this plan verifies restart
  recovery but does not redesign C2 atomicity.
- Canonical RDS-v3 projections are deterministic under the supported R
  serialization contract, not guaranteed byte-identical across R versions.
- `pipfun`, `pipload`, and `stamp` behavior remains an external compatibility
  risk despite contract and failure-injection tests.
- The interface remains provisional until a second stage demonstrates that the
  context/result abstractions generalize without schema expansion.

## Out of Scope

- Acquisition, validation, clean, or metadata stage adoption.
- Exporting a top-level orchestration engine or run aggregate class.
- Stage-specific result subclasses.
- A generic `safe_pipeline_step()` abstraction without demonstrated reuse.
- Caller-prepared `pipeline_context` runtime/lease reuse; the pilot core owns
  execution and cleanup.
- Changes to C2 dependency-context, manifest, action, reason, receipt, or
  checkpoint schemas.
- Changes to pipfun, pipload, pipaux, stamp, or other external packages.
- New package dependencies or a JSON representation.
- Household data in stage results or run manifests.
- C4 DAG nodes, edges, step hashes, propagation, scheduling, step cache state,
  or manifest migration.
- Production activation or the outstanding Windows/SMB smoke test.
- Changes to `compound-gpid.md` or roadmap structure during implementation.

## Completion Contract

### Outcome

Pipdata has validated, versioned `pipeline_context` and
`pipdata_stage_result` S3 contracts plus a deflation-stage pilot that captures
independent survey failures, continues remaining surveys, and returns compact
committed provenance internally. Existing public deflation APIs retain their
signatures and master-inventory returns, while shared integrity failures stop
subsequent writes. The only additive legacy side effect is the already-
documented summary for attempted completed runs.

### Verification Surface

| ID | Phase | Evidence Required | Command/Artifact | Required |
|---|---:|---|---|---|
| V1 | 1 | Context/result constructors enforce exact names, types, vocabularies, and cross-field invariants | `test-pipeline-context.R`, `test-pipeline-stage-result.R` | yes |
| V2 | 1 | Canonical pointer-free RDS-v3 projections produce identical bytes for semantically equivalent permuted inputs under the supported R serialization contract and exclude runtime handles, inventories, raw conditions, logs, and large data | Canonical serialization and recursive prohibited-class tests | yes |
| V3 | 1 | Stable condition and warning records preserve code/class/message/entity context across sessions while legacy warnings still signal | Condition/warning round-trip and signaling tests | yes |
| V4 | 2 | Deflation status matrix covers success, partial, failed, cached, skipped, and terminal outcomes | `test-pd-deflate-pipeline.R` | yes |
| V5 | 2 | A survey-scoped failure is recorded and later independent surveys still execute | Ordered worker/call-count test | yes |
| V6 | 2 | Exact-input hash drift, malformed actions, lease loss, stale context, parent change, unknown fence errors, or checkpoint failure stops every later write | Classification matrix, failure injection, and write-counter tests | yes |
| V7 | 2 | Success/artifact references are recorded only after exact receipt revalidation against the latest retained finalized manifest; failed pending batches remain accounted without artifacts | Checkpoint-bound provenance, fabricated-reference, five-batch retention, and terminal-accounting tests | yes |
| V8 | 2 | C1 force selection/warnings and C2 actions, reasons, exact input hashes, code hashes, and receipts flow through unchanged | Planner/result/warning integration tests | yes |
| V9 | 2 | Attempted completed runs emit one compact `deflate_summary_inf`, cached/skipped no-action runs emit none, repeated reports use the latest summary, and failures are not logged twice or with captured inventories | B2 log/log-report capture tests including persisted `args` | yes |
| V10 | 3 | `pd_deflate_pipeline()` and public `pd_deflation()` formals, positional order, durable side effects, no-action/sentinel behavior, `checkpoint_seconds = Inf`, and return shapes remain compatible except the documented summary restoration | API snapshot and regression tests | yes |
| V11 | 3 | Targeted contract, dependency, deflation, fingerprint, cleaning-compatibility, logging, and report tests pass | `devtools::test(filter = paste("pipeline-context", "pipeline-stage-result", "pd-deflate-pipeline", "pd-deflation", "pd_process_data", "code-fingerprint", "logging-integration", "log_report", "dependency-api-contract", sep = "|"))` | yes |
| V12 | final | Full package tests pass | `devtools::test()` | yes |
| V13 | final | Generated documentation is current and package check has no new errors or warnings | `devtools::document()` and `devtools::check()` | yes |
| V14 | final | Final diff contains no C2 manifest-schema change, external package change, C4 DAG work, or production-activation claim | Executed scope audit against allowed paths and schemas | yes |

### Constraints

| ID | Phase | Constraint | Check |
|---|---:|---|---|
| C1 | 1 | The nested C2 dependency context remains byte-for-byte semantically unchanged | Context hash/scope regression tests |
| C2 | 1 | Portable projections contain no raw conditions, environments, external pointers, datasets, inventories, catalogs, leases, or piplog rows | Recursive serialization assertions |
| C3 | 2 | Survey failures continue by default; shared integrity failures are never downgraded to recoverable outcomes | Condition-classification matrix and write counters |
| C4 | 2 | A successful unit means committed C2 provenance, not merely a successful artifact write | Checkpoint failure-injection tests |
| C5 | 2 | C1/C2 remains authoritative for force resolution, actions, reasons, hashes, receipts, and currentness | No duplicate resolver/registry audit |
| C6 | 2 | B2 remains the sole persistent log; result objects contain only log references and normalized condition records | Result-shape and log-count tests |
| C7 | 2 | Logging never captures household data or full inventories in persistent caller arguments | Logging argument/memory assertions |
| C8 | 3 | Existing exported wrapper signatures and return shapes remain unchanged | Formal/return snapshot tests |
| C9 | all | No new package dependency and no speculative stage-specific class hierarchy | `DESCRIPTION` and class registry diff |
| C10 | all | No C4 step graph, step hash, scheduling, or cache-storage decision enters the implementation | Final scope audit |
| C11 | final | C2 production activation remains explicitly blocked pending Windows/SMB evidence | README/activation-boundary assertion |
| C12 | 2 | The strict exact-deflation path preserves conditions internally while public deflation sentinel behavior remains unchanged | Internal-condition and direct-public regression tests |
| C13 | 2 | Preparation, context validation, and lease acquisition failures always propagate without a partial typed result | Preflight failure tests |
| C14 | 2 | Final artifact references are proven against the latest retained manifest, not caller-supplied or pruned generations | Manifest-record and multi-batch tests |
| C15 | 2 | Existing infinite checkpoint timeout remains valid and disables elapsed-time checkpoint triggering | Context/core compatibility test with `checkpoint_seconds = Inf` |

### Boundaries

- Allowed: new internal contract/context R files, targeted deflation
  integration, generated NAMESPACE/Rd changes, focused tests, NEWS, and tactical
  context documentation.
- Allowed: an internal core returning exactly `{result, master, context}` so the
  typed entry point and legacy wrapper share one execution path.
- Out of scope: acquisition, validation, cleaning, and metadata adoption.
- Out of scope: C2 manifest/dependency-context schema changes,
  pipfun/pipload/stamp changes, new dependencies, public top-level
  orchestrator, and C4 DAG work.
- Out of scope: updating the protected project charter during implementation.

### Iteration Policy

1. Implement and verify portable contracts before changing deflation execution.
2. Keep typed results separate from the internal legacy master-inventory value.
3. Normalize allowlisted survey failures once; log once with explicit compact
   `args`; rethrow exact-input/action/shared integrity failures.
4. Mark units successful only in the checkpoint callback after committed
   provenance is available.
5. Represent failed pending checkpoints and later skipped units explicitly;
   build references only against the retained latest manifest.
6. Do not extract a generic `safe_pipeline_step()` unless a second stage proves
   identical behavior; under `ask`, that requires approval.
7. Run phase-targeted package-loaded tests before advancing and full tests/check only at the
   final gate.
8. Any need to alter C2 schemas, external packages, public returns, or C4
   boundaries is a deviation requiring approval.

### Blocked-Stop Conditions

- The current C2 baseline or required targeted tests fail for reasons caused by
  the planned changes and cannot be recovered within scope.
- Exact committed manifest generation cannot be associated with successful
  deflation receipts.
- The latest retained manifest cannot prove every returned artifact reference.
- Shared integrity conditions cannot be distinguished safely from survey-
  scoped failures.
- Compatibility requires changing an existing exported signature or return
  shape.
- Implementation requires changing the C2 dependency-context/manifest schema
  or another package.
- Required verification cannot run, or fails after allowed recovery attempts.
- A required deviation is found while approval is unavailable.
- The execution report or required plan evidence cannot be recorded durably.

### Deviation Policy

The stored deviation policy is `ask`. Pause before any departure from the
approved steps, boundaries, completion contract, or file surface and record the
decision in the execution report.
