---
date: 2026-08-24
title: "Rewrite the DLW wrapper around explicit acquisition and validation stages"
status: decided
scope: "Deep"
artifact-schema-version: 1
chosen-approach: "Stage-owned delegates with survey workers"
tags: [dlw, pipeline, architecture, acquisition, validation, orchestration, logging, resumability]
---
<!-- Valid status values: decided, in-progress, abandoned -->

# Rewrite the DLW Wrapper Around Explicit Acquisition and Validation Stages

## Context

Stream B, stage B3 addresses the roadmap feature `dlw-wrapper-rewrite`.
The current public DLW surface consists of:

- `pipdata_dlw_process()` as the acquisition and validation wrapper;
- `pipdata_get_gmd()` as the acquisition delegate; and
- `pipdata_validate_gmd()` as the validation delegate.

These functions remain structurally different from the canonical pipeline
pattern established by `pd_process_data()` and reinforced by
`pd_deflate_pipeline()`. They rely primarily on persisted side effects and
typed logs, return invisible `NULL`, expose inconsistent failure boundaries,
and do not give callers a direct way to distinguish success, partial success,
no work, or failure.

The B3 branch was verified after `git fetch --prune origin`. The worktree was
clean and exactly aligned with current `origin/PROD` at commit `c06f772` when
the brainstorm began. The required upstream integrations are present:

| Stream item | Merge | Role in B3 |
|---|---|---|
| B1: data-driven DLW validation engine | `4bd90fe` | Provides the single per-survey validation engine. |
| B2: unified logging and reporting | `5370bf6` | Provides canonical DLW logmeta types, reports, and checkpoints. |
| C1: targeted `force_surveys` | `f6d213f` | Provides a reference pattern for explicit candidate ownership and compatibility. |
| A1: batch deflation | `3faf8f0` | Provides a reference map/worker/partial-success stage design. |
| A2: explicit data-level semantics | `c06f772` | Confirms the branch contains current Stream A contracts; no direct B3 dependency. |

The project charter defines DLW acquisition and validation as an explicit
pipeline stage and requires R-package quality. The design must preserve that
stage boundary while preparing, but not preempting, Stream C's future generic
pipeline-stage interface and top-level orchestrator.

### Current-State Findings

- `pipdata_get_gmd()` isolates individual download failures but returns no
  result. Catalog, matching, and inventory-write errors abort.
- `pipdata_validate_gmd()` isolates data-load failures, but artifact metadata
  and validation-engine errors can still abort the entire survey map.
- Validation outcomes are semantically mixed: an invalid survey is a valid
  execution outcome, while a load or engine error is an execution failure.
- `pipdata_dlw_process()` cannot inspect delegate outcomes. It writes a summary
  marker containing only the requested flags and then saves a log checkpoint.
- Acquisition and validation candidate selection is already inventory-driven:
  unresolved downloads and unvalidated checksums are selected again on rerun.
- Current repository callers are tests and vignettes. All three functions are
  exported, so external callers may exist even though none are visible in the
  repository.
- `vignettes/articles/Validating-Data.Rmd` still names the deprecated
  per-module validation functions and must be corrected to describe B1's
  `dlw_validation_engine()`.

## Requirements

### Functional Requirements

1. Keep acquisition and validation as distinct, ordered stages.
2. Preserve the public signatures and parameter order of all three exported
   functions.
3. Keep normal calls quiet, but invisibly return meaningful structured values
   when callers assign the result.
4. Return stage summaries plus updated inventories. Return the validation
   report by artifact reference rather than returning the full report table.
5. Isolate per-survey acquisition and validation execution failures.
6. Distinguish invalid data from failed validation execution.
7. Use the B2 typed logging contracts and existing logmeta discriminators.
8. Build logs from explicit worker and stage outcomes. Do not parse logs to
   reconstruct return values.
9. Preserve existing aliases, artifact IDs, primary keys, and checkpoint
   locations.
10. Support inventory-driven retry on a later call without introducing run
    IDs, resume tokens, or exact-once crash recovery.
11. Continue validation after partial acquisition only when the acquisition
    inventory was persisted successfully and therefore represents trustworthy
    durable state.
12. Capture runtime stage failures in returned results after input and release
    setup succeeds. Do not convert user interrupts into ordinary results.
13. Keep `pipdata_dlw_process()` public after a future top-level orchestrator
    exists so operators can rerun the DLW stage independently.
14. Avoid implementing a generic `pipdata_stage_result` class, shared pipeline
    context, generic safe-step wrapper, run manifest, or top-level orchestrator
    in Stream B.

### Compatibility Policy

- Existing unassigned calls continue to print no return object.
- Existing function arguments, defaults, and positional binding remain stable.
- Existing side effects remain: acquisition inventory, validation inventory,
  validation report, logs, and checkpoints are still persisted under their
  current names and aliases.
- Assigned return values deliberately change from `NULL` to documented plain
  lists. This is a public return-contract change and requires a `NEWS.md` entry.
- Runtime catalog, inventory, schema, and persistence failures deliberately
  change from thrown errors to failed results once setup has succeeded.
- Caller errors and precondition failures still abort before stage execution.

### Explicitly Out of Scope

- A reusable result class or generic constructor shared by all pipeline stages.
- `pipeline_context`, `safe_pipeline_step`, `run_pipeline()`, or another
  top-level orchestration API.
- Run IDs, resume tokens, exact-once processing, or per-survey durable
  checkpoints.
- Parallel acquisition or validation.
- Changes to B1 validation rules or `validation_spec.yml`.
- Replacing the validation engine's report accumulation mechanism.
- Typed validation-report fields or removal of `table_name` parsing.
- Consolidating log folders or changing checkpoint aliases.
- Redesigning `pd_process_data()` or `pd_deflate_pipeline()` return contracts.

## Approaches Considered

### Approach 1: Thin-Wrapper Refactor

Keep the current delegate internals, add local counters and structured returns,
and aggregate those returns in `pipdata_dlw_process()`.

**Pros**

- Smallest implementation diff.
- Lowest immediate compatibility risk.
- Reuses B2 logging with little movement of code.

**Cons**

- Validation still lacks a complete per-survey error boundary.
- Return values remain coupled to hidden internal side effects.
- The architecture still does not mirror the canonical map/worker pattern.
- Wrapper-level error capture cannot provide reliable per-survey detail for
  failures that escape the current validation loop.

**Effort**: Medium.

**Recommended**: No. It improves observability but does not resolve the
roadmap feature's structural problem.

### Approach 2: Stage-Owned Delegates With Survey Workers

Redesign acquisition and validation as explicit stage orchestrators. Each
stage discovers its candidates, maps an internal one-survey worker, aggregates
worker outcomes, persists its artifacts, emits B2 logs, and invisibly returns
a stage-specific plain list. `pipdata_dlw_process()` aggregates the two stage
results and applies explicit continuation rules.

**Pros**

- Mirrors the proven `pd_process_data()` and `pd_deflate_pipeline()` pattern.
- Creates a complete survey-level failure boundary where appropriate.
- Makes partial success and trustworthy persistence explicit.
- Supports deterministic tests without requiring real DLW or stamp I/O.
- Gives Stream C an adapter-friendly result without creating a generic class.
- Keeps candidate selection, persistence, logging, and summary ownership at
  the stage boundary.

**Cons**

- Requires a larger migration than a return-only patch.
- Changes assigned public return values from `NULL` to lists.
- Changes runtime infrastructure failures from thrown errors to returned
  failures after setup.
- Must preserve the B1 validation engine's report side effect carefully.

**Effort**: Medium-Large.

**Recommended**: Yes. This provides the intended architecture without taking
ownership of Stream C's generic abstraction.

### Approach 3: Implement the Generic Stage Framework in B3

Create shared result constructors or an S3 class, a pipeline context, a generic
safe-step wrapper, and resumable run metadata, then migrate the DLW functions
onto that framework.

**Pros**

- Produces the most uniform long-term architecture immediately.
- Could be reused by cleaning and deflation without an adapter.

**Cons**

- Implements `pipeline-stage-interface` before Stream C owns its design.
- Pulls cross-stage context, error, and resume policy into Stream B.
- Greatly expands scope and sequencing risk.
- Makes B3 dependent on a `safe_pipeline_step` concept that is referenced in
  the roadmap but is not currently represented as its own roadmap feature.

**Effort**: Large.

**Recommended**: No. It violates the explicit stream boundary.

## Decision

Choose **Approach 2: Stage-Owned Delegates With Survey Workers**.

The design introduces plain, DLW-specific return contracts and internal
workers. It deliberately stops before introducing a shared result type or
cross-stage execution framework.

### Public Function Roles

| Function | Responsibility after B3 | Return behavior |
|---|---|---|
| `pipdata_get_gmd()` | Discover acquisition candidates, map the download worker, merge outcomes into the acquisition inventory, persist it, and emit acquisition logs. | Invisibly return an acquisition result. |
| `pipdata_validate_gmd()` | Discover validation candidates, load prior artifacts, map the validation worker, merge and persist inventory/report outputs, and emit validation logs. | Invisibly return a validation result. |
| `pipdata_dlw_process()` | Validate setup inputs, configure the standalone release environment, run requested stages, enforce continuation rules, emit the aggregate summary, and save the checkpoint. | Invisibly return a DLW aggregate result. |

### B3 Plain Result Shape

The acquisition and validation results use the same top-level concepts for
easy inspection, but B3 must not add a shared class or generic constructor.

```r
list(
  stage = "acquisition",       # or "validation"
  outcome = "partial",        # success, partial, failed, no_work
  inventory = updated_inv,     # last trustworthy inventory, or NULL
  summary = list(...),         # stage-specific counts and identifiers
  failures = failure_dt,       # compact data.table, never condition objects
  artifacts = list(...)        # persisted artifact identities/write facts
)
```

`failures` has a stable compact schema:

| Field | Meaning |
|---|---|
| `survey_id` | Survey identifier when the failure is survey-specific; otherwise `NA_character_`. |
| `phase` | Operation such as `catalog_load`, `download`, `artifact_info`, `validation`, `inventory_save`, or `report_save`. |
| `error_type` | Stable character discriminator. |
| `condition_msg` | Human-readable condition message. |

The result must not contain raw R condition objects. This matches B2's logging
contract, is safe to inspect or serialize, and leaves Stream C free to define
its own generic error representation.

### Acquisition Result

The acquisition `summary` contains at least:

- `n_total`;
- `n_success`;
- `n_failed`;
- `surveys_success`; and
- `surveys_failed`.

The returned `inventory` is the full updated acquisition inventory after a
successful write. When discovery fails before any trustworthy inventory is
available, it is `NULL`. When a prior trustworthy inventory was loaded but a
later stage fails, the result may return that prior inventory while marking
the artifact write as unsuccessful.

The internal one-survey worker receives one inventory row and returns a compact
outcome. It does not own candidate discovery, inventory merging, stage summary
logging, or persistence.

### Validation Result

The validation `summary` contains at least:

- `n_total`;
- `n_valid`;
- `n_invalid`;
- `n_failed`;
- `surveys_valid`;
- `surveys_invalid`; and
- `surveys_failed`.

`n_failed` means execution failures such as load, artifact metadata, or engine
errors. A survey that the engine successfully classifies as invalid increments
`n_invalid`, not `n_failed`.

The returned `inventory` is the full updated validation inventory after a
successful write. The full validation report remains a persisted artifact and
is represented in `artifacts`; it is not duplicated in the return value.

The internal validation worker places the complete per-survey execution
boundary around:

1. loading DLW data;
2. loading stamp artifact metadata;
3. calling `dlw_validation_engine()`; and
4. constructing the validation inventory row.

The B1 engine remains the only validation-rule implementation. B3 does not
restore per-module validation functions or alter YAML semantics.

### DLW Aggregate Result

```r
list(
  stage = "dlw",
  outcome = "partial",
  acquisition = acquisition_result,
  validation = validation_result,
  checkpoint = list(success = TRUE, alias = "dlw_meta", stage = "dlw")
)
```

Nested stages may also use `outcome = "not_run"` with a `reason` of
`"disabled"` or `"dependency_failed"`. The aggregate result uses:

| Situation | Aggregate outcome |
|---|---|
| All requested stages succeed or have no work | `success` or `no_work` |
| Useful durable work exists, but a requested survey or stage failed | `partial` |
| No requested stage produced trustworthy useful output | `failed` |
| Both acquisition and validation are disabled | `no_work` |

### Error Boundaries

Caller and configuration errors abort before execution begins. These include
invalid arguments, missing `release` or `identity`, and calling a standalone
delegate without a configured working release.

After setup succeeds:

- survey-level errors become compact failures and do not stop sibling surveys;
- catalog, inventory, schema, and persistence errors become failed stage
  results rather than escaping conditions;
- user interrupts propagate immediately;
- checkpoint failure is recorded in the aggregate result; and
- logs use existing B2 discriminators and condition-message fields.

### Stage Continuation

| Acquisition state | Validation behavior |
|---|---|
| Disabled | Run validation independently against existing local state when requested. |
| Success | Run validation when requested. |
| No work | Run validation when requested; existing downloaded data may still need validation. |
| Partial and inventory persisted | Run validation for durable available surveys. |
| Failed without trustworthy inventory | Return validation as `not_run` with `reason = "dependency_failed"`. |

### Resume and Retry Semantics

B3 uses inventory-driven, at-least-once reruns:

- acquisition recomputes candidates from checksum and `data_available` state;
- validation recomputes candidates from downloaded files and the validated
  inventory;
- unresolved surveys are retried automatically on the next call;
- successful persisted work is skipped by the existing selectors; and
- a hard process crash before the final inventory write may cause completed
  work to repeat on the next call.

The DLW log checkpoint remains an observability artifact. It is not a resume
cursor, transaction log, or exactly-once guarantee. Explicit run identity and
resume tokens belong to Stream C.

### Logging Contract

- Preserve `.logtype_dlw_acquisition`, `.logtype_dlw_validation`, and
  `.logtype_dlw_summary` values.
- Preserve existing B2 phases consumed by `log_report()`.
- Add fields or phases only when required to represent a new explicit outcome;
  do not rename existing report-consumed values.
- Emit stage start, no-work, completion, and per-survey failure entries from
  explicit outcomes.
- Build the wrapper summary from delegate results, not from log inspection.
- Keep typed logging at orchestration boundaries and compact worker outcomes to
  avoid retaining large survey objects in persistent log state.

### Public Lifecycle

`pipdata_dlw_process()` remains a public expert-stage entry point after a
future `run_pipeline()` becomes the recommended end-to-end API. This preserves
independent DLW reruns, debugging, and operational recovery without forcing
auxiliary refresh, cleaning, or deflation.

`pipdata_get_gmd()` and `pipdata_validate_gmd()` remain public in B3 because
they are already exported and documented as standalone operations. Any future
deprecation requires a separate compatibility decision.

## Phased Implementation Scope

### Phase 1: Characterize and Pin Existing Contracts

- Add characterization tests for signatures, parameter order, visibility,
  aliases, artifact IDs, primary keys, logging discriminators, phases, and
  current candidate-selection behavior.
- Pin the existing no-op and stage-marker behavior.
- Record the deliberate return and runtime-error migration in tests before
  rewriting internals.

### Phase 2: Redesign Acquisition

- Introduce an internal one-survey download worker.
- Separate candidate discovery from execution and final inventory persistence.
- Aggregate worker outcomes into explicit success/failure sets.
- Preserve current module filtering and `check_missing` behavior.
- Persist the acquisition inventory once per completed stage attempt.
- Return the acquisition result invisibly.
- Capture runtime infrastructure failures in the result and B2 logs.

### Phase 3: Redesign Validation

- Introduce an internal one-survey validation worker.
- Put load, artifact-info, engine, and inventory-row construction inside the
  survey boundary.
- Preserve B1 engine dispatch and report accumulation.
- Distinguish valid, invalid, and execution-failed surveys.
- Persist validation inventory and report under current IDs and aliases.
- Return the validation result invisibly.
- Capture inventory/report runtime failures in the result and B2 logs.

### Phase 4: Redesign DLW Orchestration

- Aggregate acquisition and validation results directly.
- Implement the stage-continuation matrix.
- Derive and log the aggregate DLW outcome and counts.
- Save the existing DLW checkpoint after an aggregate result exists.
- Capture checkpoint failure in the result.
- Keep standalone release setup and current alias registration behavior for
  compatibility; Stream C later owns shared setup in the top-level path.

### Phase 5: Migration, Documentation, and Release Surface

- Update roxygen `@return` and `@details` for all three public functions.
- Update `NEWS.md` with the assigned-return and runtime-error changes.
- Update `Validating-Data.Rmd` to describe `dlw_validation_engine()` instead of
  deprecated per-module validators.
- Update orchestration and processing vignettes with the structured-return and
  partial-success semantics.
- Regenerate documentation and verify the built package surface.

## Dependency Map

```text
B1 validation engine (done) -----------+
                                       |
B2 unified logging/reporting (done) ---+--> B3 DLW wrapper rewrite
                                       |          |
C1 targeted force_surveys (done) ------+          +--> Stream C stage adapter
                                       |          +--> future run_pipeline()
A1 batch deflation (done) -------------+          +--> future generic resume

A2 data-level semantics (done) -------- current PROD baseline only

typed-validation-report --------------- optional follow-on, not a B3 dependency
pipeline-stage-interface -------------- downstream Stream C owner
safe_pipeline_step -------------------- Stream C dependency gap to resolve
```

### Dependency Rules

- B3 depends directly on B1 and B2.
- C1 and A1 are reference implementations, not code dependencies.
- A2 is part of the verified branch baseline and must not be regressed.
- B3 must not wait for `typed-validation-report`.
- Stream C must not require B3 to return raw condition objects or implement an
  S3 class retroactively; its adapter must accept compact failure descriptors.
- Stream C must resolve the roadmap's reference to `safe_pipeline_step`, which
  is not currently represented as a separate roadmap feature.

## Required Tests

### Return Contract Tests

- Each public function returns invisibly.
- Each assigned result has the documented names and field types.
- Inventories are `data.table` objects or documented `NULL` values.
- Failure tables have the pinned compact schema.
- No returned failure contains an R condition object.

### Acquisition Tests

- No candidates returns `no_work` and the current inventory.
- All downloads succeed.
- Some downloads fail and siblings continue.
- All downloads fail.
- Catalog discovery fails after setup and returns `failed`.
- Inventory matching fails and returns `failed`.
- Inventory persistence fails and marks the inventory untrustworthy.
- Existing module filtering and `check_missing` behavior remain unchanged.
- B2 logs and result counts agree.

### Validation Tests

- No candidates returns `no_work`.
- Valid and invalid surveys are both successful executions.
- A load failure does not stop sibling surveys.
- An artifact-info failure does not stop sibling surveys.
- A validation-engine error does not stop sibling surveys.
- Inventory persistence failure returns `failed`.
- Report compilation or persistence failure is captured explicitly.
- Existing B1 engine output and persisted inventory schema remain compatible.
- B2 logs and result counts agree.

### Wrapper Tests

- All combinations of `get_dlw_data` and `validate_dlw_data` are covered.
- Partial acquisition with durable inventory continues to validation.
- Failed acquisition without trustworthy inventory blocks validation.
- Disabled acquisition allows standalone validation.
- Both stages disabled returns `no_work` and retains the stage marker.
- Checkpoint success and failure are represented in the aggregate result.
- Aggregate outcome is correct for success, partial, failure, and no-work runs.

### Compatibility and Migration Tests

- Public signatures, defaults, and parameter order remain unchanged.
- Existing aliases, artifact IDs, primary keys, and checkpoint locations remain
  unchanged.
- Existing B2 logmeta discriminators and report-consumed phases remain stable.
- Unassigned calls remain quiet.
- Vignette examples and roxygen describe the new return contract.
- Built-package tests verify exports, documentation, and checkpoint integration.

## Acceptance Criteria

1. All three public signatures and parameter order remain unchanged.
2. Normal unassigned calls remain quiet; assigned calls receive documented
   structured results.
3. Every requested stage yields an explicit outcome without runtime stage
   errors escaping after setup.
4. Per-survey acquisition and validation execution failures do not stop
   sibling surveys.
5. Invalid data is distinguished from failed validation execution.
6. Validation continues after partial acquisition only when the acquisition
   inventory write succeeded.
7. Existing aliases, artifact IDs, primary keys, B2 logmeta discriminators,
   report behavior, and checkpoint locations remain unchanged.
8. Inventory-driven reruns retry unresolved surveys without run IDs or resume
   tokens.
9. Result values are the source for summary logging; logs are not reparsed to
   create results.
10. The full validation report is not duplicated in the return value.
11. Vignettes accurately describe B1's validation engine and B3's result and
    partial-success contracts.
12. No generic stage-result class, pipeline context, safe-step wrapper, or
    top-level orchestrator is introduced in B3.

## Explicit Stream C Handoff Contract

### Facts Stream C May Rely On

- `pipdata_dlw_process()` remains public and independently runnable.
- DLW functions return invisible plain lists with explicit stage identity and
  outcome.
- Delegate results expose the last trustworthy inventory, stage-specific
  summary counts and identifiers, compact failure descriptors, and artifact
  write facts.
- The aggregate result preserves nested acquisition and validation outcomes.
- Acquisition and validation remain independently callable and retry from
  persisted inventory state.
- B2 logs remain available for human reporting, but orchestration does not need
  to parse them to understand stage outcomes.

### Responsibilities Reserved for Stream C

- Define and validate `pipdata_stage_result`, whether as an S3 class or a
  structured list.
- Define the final generic `status` vocabulary and map B3 outcomes into it.
- Decide whether generic errors are conditions, descriptors, or both.
- Add generic constructors, validators, print methods, and adapters.
- Define `pipeline_context` and move shared release, identity, folder, alias,
  and auxiliary setup into the top-level execution path.
- Define run IDs, resume tokens, run manifests, and exact checkpoint semantics.
- Define `safe_pipeline_step` or replace that prerequisite with another named
  error-boundary mechanism.
- Chain DLW, cleaning, and deflation in `run_pipeline()`.
- Decide how generic log-entry snapshots relate to the existing global
  `pipdata_log` and checkpoint artifacts.

### Expected Adapter Direction

Stream C should wrap or translate B3 results rather than require B3 to
instantiate a generic class. A future adapter can map:

| B3 field | Stream C concept |
|---|---|
| `stage` | Generic stage name. |
| `outcome` | Generic `status`. |
| `inventory` | Generic stage `data`. |
| `summary` | Generic metrics/metadata. |
| `failures` | Generic errors. |
| `artifacts` | Generic persisted-output references. |

`no_work` maps to a successful generic stage with zero processed units.
`not_run` remains nested execution metadata and maps according to whether the
reason was caller-disabled or dependency-failed.

## Devil's Advocate Record

### Problem Validation

Pre-validated by the live source audit. Logs are currently the only way to
infer most DLW outcomes, and validation lacks a complete survey-level error
boundary.

### Simplicity Check

A thin return-value patch would provide much of the immediate observability
benefit. It was rejected because it would preserve the structural mismatch and
leave validation failures capable of aborting sibling surveys.

### Effort-Value Check

The deeper redesign is proportionate only if implemented in phases. Acquisition
must be independently completed and verified before validation and wrapper
aggregation are changed.

### Charter Alignment

The design aligns with the charter's explicit DLW acquisition/validation stage
and R-package quality constraint. The main compatibility risk is the chosen
policy to return runtime infrastructure failures instead of throwing them after
setup. This requires explicit documentation and migration tests.

## Next Steps

1. Run `/cg-plan` using this brainstorm as the source of truth and inherit its
   `Deep` scope.
2. Plan Phase 1 as characterization tests before any production refactor.
3. Plan acquisition, validation, and wrapper migration as separately verifiable
   implementation phases.
4. Include a Stream C review checkpoint for the handoff contract without
   allowing Stream C's generic abstraction into the B3 implementation.
5. Track the missing `safe_pipeline_step` roadmap dependency as a separate
   orchestration idea if Stream C confirms it remains required.
