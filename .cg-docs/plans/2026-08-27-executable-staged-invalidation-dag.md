---
date: 2026-08-27
title: "Implement Executable Staged Invalidation DAG"
status: completed
completed-date: 2026-09-01
scope: "Deep"
brainstorm: ".cg-docs/brainstorms/2026-08-27-executable-staged-invalidation-dag.md"
language: "R"
estimated-effort: "large"
deviation-policy: "ask"
artifact-schema-version: 1
phases: 4
completed-phases: [1, 2, 3, 4]
tags: [pipeline, invalidation, orchestration, dependency-graph, caching, provenance, resumability]
execution-report: ".cg-docs/work-reports/2026-08-28-executable-staged-invalidation-dag.md"
---

# Plan: Implement Executable Staged Invalidation DAG

## Objective

Implement a pipdata-owned incremental executor for the existing durable
`clean`, `metadata`, and `deflate` stages. The executor must use the C2
dependency manifest and exact Stamp receipts as its only currentness authority,
map execution through C3 typed contexts and results, report every selected node,
and run only directly affected descendants.

The public addition is a thin `pd_run_pipeline()` wrapper. Existing public
stage wrappers remain backward compatible. C4 begins from B3's trustworthy
completed-validation inventory and does not execute acquisition or validation.

## Context

The approved design is recorded in
`.cg-docs/brainstorms/2026-08-27-executable-staged-invalidation-dag.md`.
The worktree was synchronized before brainstorming: `HEAD` and `origin/PROD`
both resolved to `d79cefa084bcf45ecd1306fb7d6e962725df9c3b` with divergence
`0/0`. Planning continues on `feat/step-level-invalidation-dag`.

C2 already owns the manifest, input/code hashes, exact receipts, writer lease,
execution fence, inventory reconciliation, and checkpoint publication. C3
already owns `pipeline_context` and `pipdata_stage_result` and proves the typed
boundary on deflation. C4 must compose and extend those contracts rather than
introduce another graph, manifest, provenance model, or run cursor.

### Current Architecture References

| Concern | Current implementation |
| --- | --- |
| Manifest, stages, actions, reasons | `R/dependency_contract.R:1-134` |
| Dependency context and scope | `R/dependency_contract.R:137-176` |
| Curated stage/component fingerprints | `R/code_fingerprint.R:1-100` |
| Input projections | `R/dependency_inputs.R` |
| Snapshot and current facts | `R/dependency_execution.R:47-207` |
| Lease, snapshot, code, and parent fences | `R/dependency_execution.R:209-275` |
| Plan, force resolution, bootstrap guard | `R/dependency_plan.R:1-123` |
| Checkpoint batching | `R/dependency_execution.R:277-295` |
| Manifest lease and publication | `R/dependency_manifest.R:53-195` |
| Result-bound checkpoint | `R/dependency_manifest.R:225-350` |
| Exact Stamp receipt verification | `R/save_pip.R:91-148` |
| Stage inventory reconciliation | `R/reconcile_pip_inventory.R:1-71` |
| Clean/metadata active path | `R/pd_process_data.R:73-200` |
| Clean/metadata helpers | `R/pd_process_data.R:332-408`; `R/pd_metadata_refresh.R` |
| Deflation shared typed core | `R/pd_deflate_pipeline.R:90-317` |
| Strict exact deflation | `R/pd_deflation.R:333-363` |
| Typed context | `R/pipeline_context.R:23-144` |
| Typed stage result | `R/pipeline_stage_result.R:259-424` |
| Read-only report | `R/pd_change_report.R:20-37` |
| Unified logging/reporting | `R/aaa.R:84-103`; `R/log_report.R` |

### Planning Findings

- `pd_change_report()` currently calls the planner without the executor's full
  snapshot/fact preparation, so report and execution can disagree.
- C2 normally emits actionable rows only. Current entities must become explicit
  `action = "none"` rows or an equally authoritative complete-node projection.
- The C2 manifest `inputs` table already supports multiple names per
  `(stage, entity_id)` but current publication writes only `canonical`.
- `pd_process_data()` returns at line 200. The historical pipeline summary and
  checkpoint block after that return is unreachable and cannot be reused.
- Clean and metadata need reusable internal typed cores equivalent to the
  deflation pilot.
- `pd_run_deflate_stage()` currently prepares and owns its own execution state;
  top-level orchestration needs an internal path that accepts the shared C2
  execution state without changing the public deflation return.
- C3's `plan_hash` is stage-plan-specific. A dynamic multi-wave run must retain
  separate accepted wave hashes under one run ID rather than mutate an earlier
  stage context.
- Production activation remains blocked by signed target Windows/SMB fencing
  and immutable unique-rename evidence.
- The protected charter Current Focus is stale relative to completed A1, A2,
  C2, C3, and the content-hash trigger. Treat those features as the verified
  baseline. Do not modify `compound-gpid.md` during implementation.

### Critic-Verified Baseline Defects

- `pd_prepare_execution()` currently reads the manifest and accepts a plan
  before it acquires the writer lease. C4 must acquire the lease and then
  rebuild the authoritative plan, including for an all-cached result.
- Clean output receipt sets are hashed with different structures during
  snapshot construction and checkpoint publication. C4 must introduce one
  symmetric receipt-set canonicalizer before no-op behavior can be trusted.
- `expected_pip_ids()` does not reproduce the IDs generated by
  `get_country_pfw()`/`cache_id()`, and the clean worker currently derives its
  expected set from its own output. C4 must derive and accept the independent
  expected set before worker execution.
- Current dependency projections refer to non-authoritative `year` and
  `reporting_level` fields and can fall back to a full auxiliary-object hash.
  C4 must replace that behavior with one exact key adapter and fail closed on
  missing or ambiguous keys.

### Dependency Graph

```text
completed-validation inventory
        |
        v
clean:<survey_id>
        |
        +--> metadata:<pip_id-1> --> deflate:<pip_id-1>
        |
        +--> metadata:<pip_id-2> --> deflate:<pip_id-2>
```

The clean node commits the complete verified receipt set for its expected
`pip_id` outputs. Metadata and deflate commit one exact receipt per `pip_id`.
Internal load, PFW merge, recode, auxiliary attachment, and save functions are
fingerprint/input components, not independently cached artifacts.

## Requirements

| ID | Requirement | Source |
| --- | --- | --- |
| R1 | Use one shared metadata-only snapshot/fact path for `pd_change_report()` and execution | Brainstorm; C2 parity gap |
| R2 | Represent every selected stage/entity node, including current/cached work | Brainstorm state model |
| R3 | Preserve C2 actions/reasons and derive current, stale, forced, cached, runnable, and blocked without a second planner | Brainstorm state model |
| R4 | Publish named per-entity input components through the existing C2 manifest `inputs` table | Brainstorm executable DAG |
| R5 | Map DLW, PFW, CPI, PPP, population, GDP/PCE, upstream output, and code changes to exact affected entities | Required invalidation matrix |
| R6 | Use `clean:<survey_id>`, `metadata:<pip_id>`, and `deflate:<pip_id>` as the only durable C4 nodes | Approved granularity |
| R7 | Execute topological waves under one C2 scope lease and refresh facts after committed upstream checkpoints | Approved Approach 1 |
| R8 | Bind metadata to actual committed clean receipts and deflate to actual committed clean/metadata receipts | C2/C3 provenance solutions |
| R9 | Extract reusable clean and metadata cores while preserving `pd_process_data()` behavior | Existing execution boundary |
| R10 | Adapt deflation to shared execution without changing `pd_deflate_pipeline()` behavior | C3 pilot boundary |
| R11 | Add a compact typed aggregate result with per-wave C3 results and plan/manifest identities | Approved output decision |
| R12 | Export a thin `pd_run_pipeline()` API with the approved minimal arguments | Approved API decision |
| R13 | Keep `force_surveys` additive to ordinary invalidation while adding the forced reason only to selected clean nodes and their descendants | C1 and approved force semantics |
| R14 | Report cached nodes without loading household artifacts or invoking stage workers | Performance guardrail |
| R15 | Continue independent survey-domain failures but fail closed on shared integrity failures | C3 failure contract |
| R16 | Block descendants after prerequisite failure and account every selected node exactly once | Required production behavior |
| R17 | Commit success only after exact receipt, inventory, and manifest checkpoint finalization | C2 authoritative checkpoint solution |
| R18 | Resume by authoritative replan without a persisted run cursor or exactly-once claim | Approved persistence decision |
| R19 | Preserve prior immutable Stamp artifacts and explicit C2 bootstrap/baseline policy | Required migration behavior |
| R20 | Integrate compact run/stage summaries with `pipdata_log` and align `pd_change_report()` output | B2 and required summaries |
| R21 | Prove the exact Colombia 2018 CPI scenario and immediate no-op rerun | Required acceptance behavior |
| R22 | Preserve all existing public signatures, positional order, return types, aliases, and data-level semantics | C1/A1/A2/B2/B3 guardrails |
| R23 | Add no dependency, external framework, second manifest/DAG, unsafe parallelism, or production activation claim | Guardrails |
| R24 | Retain the explicit Windows/SMB production activation block | C2 completion boundary |
| R25 | Check whole-survey removal preliminarily before lease and authoritatively after lease/master reload; fail before worker or artifact/inventory/manifest writes until a retirement policy is approved | Plan review |

## Phase 1: Authoritative Planning And Explainability

### 1. Unify Read-Only Fact Preparation

- **Requirements**: R1, R5, R14, R25
- **Files**: `R/dependency_execution.R`, `R/pd_change_report.R`, `R/dependency_plan.R`, `tests/testthat/test-pd-change-report.R`, `tests/testthat/test-dependency-execution.R`
- **Details**: Extract or expose one read-only helper around the existing dependency snapshot and fact construction. It must accept the same completed-validation inventory, dependency context, manifest, master inventory, catalogs, auxiliary projections, and code fingerprints used by execution preparation.
- **Details**: Keep lease acquisition and write fencing in the execution-only layer. The shared helper must perform no write, no lease mutation, and no household `pip_read()`.
- **Details**: Define one pure whole-survey removal helper used by
  `pd_change_report()`, preliminary execution preflight, and authoritative
  post-lease validation. For identical static completed-validation/master facts,
  the report and preliminary execution path raise the same
  `pipdata_upstream_survey_removed` condition.
- **Details**: Make `pd_change_report()` call the shared helper before `pd_dependency_plan()` so its initial actions and reasons are byte-for-byte semantically aligned with execution preparation under the same injected facts.
- **Details**: Define `snapshot_identity` as a deterministic hash over the
  manifest identity, canonical catalog rows, completed-validation and master
  inventory keys, per-entity auxiliary component hashes, current facts, and
  fingerprints. Exclude `captured_at`, loaded objects, environments, and row
  order.
- **Details**: Preserve `pd_change_report()` public formals and invisible structured return. Additive printed state/reason summaries are allowed.
- **Details**: Add dependency injection points only where existing tests need deterministic catalogs/manifests; do not add a public advisory-plan execution path.
- **Test Scenarios**: Current release, changed DLW fact, changed auxiliary
  projection, changed code fingerprint, missing output, absent manifest,
  corrupt manifest, empty completed-validation inventory, and a whole survey
  removed from otherwise-identical static report/execution facts.
- **Error Paths**: Catalog ambiguity, invalid context, malformed manifest, missing exact auxiliary version, and injected household load must fail or be detected consistently in report and execution.
- **Tests**: Extend `test-pd-change-report.R` with report/execution parity and zero-household-load counters. Extend `test-dependency-execution.R` with shared-helper invariants.
- **Acceptance criteria**: For identical injected metadata facts, report and
  pre-lease advisory preparation produce identical context, actions, reasons,
  and deterministic `snapshot_identity`. The execution plan rebuilt after
  lease acquisition is authoritative. No household artifact loader is called.

### 2. Plan The Complete Selected Node Universe

- **Requirements**: R2, R3, R6, R14, R16
- **Files**: `R/dependency_contract.R`, `R/dependency_plan.R`, `R/dependency_execution.R`, `R/pipeline_context.R`, `tests/testthat/test-dependency-plan.R`, `tests/testthat/test-pipeline-context.R`
- **Details**: Extend the authoritative plan so every selected applicable node is represented. Current nodes must use the existing controlled `action = "none"` or an additive complete-node table derived from the same fact rows. Do not maintain a second independent action list.
- **Details**: Prefer retaining the existing plan top-level shape (`context`, `actions`, `reasons`, `snapshot`) and existing action columns. If an additive node projection is required, derive and validate it from those tables in one function and keep actions/reasons authoritative.
- **Details**: Define a deterministic derived-state mapping: matching facts plus `none` become `current/cached`; changed facts become `stale/runnable`; a selected forced reason becomes `forced/runnable`; unmet accepted prerequisites become `blocked` at scheduling time.
- **Details**: Do not add `runnable`, `running`, `blocked`, or `forced` to the durable manifest record schema. They are plan/runtime states.
- **Details**: Audit every existing action consumer and make worker loops explicitly filter actionable rows. Prevent `none` rows from reaching workers or checkpoint batches.
- **Details**: Update `pd_plan_hash()` behavior only through its existing canonical action/reason projection. Adding deterministic `none` rows is an intentional plan identity change; row order and timestamps must not affect the hash.
- **Details**: Preserve bootstrap restriction and C1 force resolution. A current forced node must have one forced reason and an actionable action, never `none`.
- **Details**: Distinguish forecast nodes from accepted wave nodes. Initial
  downstream rows are forecasts when their owning clean node is actionable.
  After clean commits, replace those forecasts with the accepted metadata
  universe derived from verified receipts and tombstones. Exact-once accounting
  applies to accepted stage-wave keys, not obsolete forecast keys.
- **Test Scenarios**: Fully current release, mixed current/actionable nodes, zero selection, forced current node, new entity, unknown provenance, missing output, duplicate plan facts, row permutation.
- **Error Paths**: Duplicate `(stage, entity_id)`, invalid `none` reason combinations, action consumer attempting to execute `none`, and inconsistent entity mapping.
- **Tests**: Expand `test-dependency-plan.R`; add action-consumer regression assertions across clean, metadata, and deflate tests as later phases land.
- **Acceptance criteria**: Every selected applicable node appears exactly once in the complete plan and deterministically maps to cached or actionable state. No current node reaches a worker.

### 3. Persist Named Input Components And Exact Reasons

- **Requirements**: R4, R5, R17, R19, R21
- **Files**: `R/dependency_inputs.R`, `R/dependency_contract.R`, `R/dependency_execution.R`, `R/dependency_manifest.R`, `R/reconcile_pip_inventory.R`, `R/code_fingerprint.R`, `tests/testthat/test-dependency-inputs.R`, `tests/testthat/test-dependency-manifest.R`, `tests/testthat/test-reconcile-pip-inventory.R`, `tests/testthat/test-code-fingerprint.R`
- **Details**: Define deterministic named component projections within the
  existing `inputs` table using the frozen table below. Code and recode
  specification changes remain in `fingerprints` and are not duplicated as
  input rows.
- **Details**: Retain each stage record's canonical `input_hash` as the sorted
  composite equality check. Named rows explain and localize changes; they do
  not replace the canonical hash.
- **Details**: Define one canonical key adapter using `country_code`,
  `surveyid_year`, `survey_acronym`, module, welfare type, and the exact PFW
  row. Derive CPI, PPP, population, GDP, and PCE projections with the same
  `filter_aux_data()`/`create_attr()` semantics used by metadata creation.
  Derive each `*_data_level` independently from its corresponding PFW domain.
  Do not use a shared master `reporting_level`, parse year from `pip_id`, or
  fall back to a global auxiliary hash. Missing or ambiguous keys fail closed.
- **Details**: Preserve exact `version_id` versus `content_hash` semantics.
  Named artifact-component `version_id`s select exact Stamp artifacts. The
  `canonical` row is the sole exception: its nonblank `version_id` is a
  deterministic composite version token and is not an artifact selector.
  Content hashes prove component and canonical content.
- **Details**: Add the exact controlled reason `legacy_input_changed` to
  `.PD_REASON_CODES`. Use it only when a legacy canonical-only record changes
  and no named prior component can prove a narrower reason.
- **Details**: Support two schema-1 manifest comparison paths. If an entity has only the
  legacy `canonical` row, recompute and compare the exact legacy canonical
  algorithm. If noncanonical named rows exist, use the C4 named-component
  canonical algorithm. A matched legacy record remains current. A changed
  legacy record receives `legacy_input_changed` and publishes named rows only
  after successful execution.
- **Details**: Populate named rows from verified execution results inside `pd_finalize_checkpoint()`, after upstream versions are final. Never persist intended pre-execution versions after same-run upstream work.
- **Details**: Freeze fingerprint reason ownership. Compare old/current
  component rows before stage summary hashes. A changed `recode_spec.yml`
  component produces only `recode_spec_changed`; other changed clean components
  produce `clean_code_changed`; metadata and deflate components produce their
  corresponding stage-code reasons. Use generic stage-code reasons for legacy
  manifests without component evidence.
- **Details**: At checkpoint, advance fingerprint component rows only for
  stages that committed successfully in that checkpoint. Do not replace
  uncommitted stage components after an unrelated stage checkpoint.
- **Details**: Preserve multi-output clean atomicity and tombstones. Clean named output/input rows must correspond to the finalized complete receipt set.
- **Details**: Introduce one clean receipt-set canonicalizer used by both
  snapshot construction and checkpoint publication. Sort exact receipts by
  `pip_id` and derive aggregate output version/hash from the same canonical
  tuples `(pip_id, alias, artifact, path, version_id, content_hash)`.
- **Details**: Derive accepted expected clean `pip_id`s before worker execution
  through the same pure PFW filtering, welfare abbreviation, module, and ID
  builder used by `get_country_pfw()`/`cache_id()`. Store the set in the clean
  action. Before any write, require exact equality between the accepted set,
  `names(clean)`, and `names(metadata)`. The worker must not define expected
  output from its own result.
- **Test Scenarios**: Single-survey DLW change, single-key PFW change,
  one-country/year CPI/PPP/population change, metadata-only GDP/PCE change,
  code-only changes by stage, output drift, valid canonical-only legacy record,
  one/multiple clean outputs, missing welfare output, and receipt row
  permutation. Include a recode change followed by an unrelated metadata
  checkpoint before clean execution; the recode reason must remain pending.
- **Error Paths**: Ambiguous auxiliary key, duplicate named input, unsorted projection, intended/committed version mismatch, and incomplete clean receipt set.
- **Tests**: Expand input, manifest, reconciliation, and fingerprint tests with exact reason and backward-compatibility assertions.
- **Acceptance criteria**: A named component change invalidates only entities
  whose canonical projection changes, committed rows contain exact finalized
  versions/hashes, clean receipt sets converge to `none/current` on immediate
  rerun, worker output cannot redefine its expected set, and matched legacy
  canonical-only records require no rebuild.

#### Frozen Named Input Contract

| Stage | `inputs$name` | `version_id` source | `content_hash` source | Required | Change reason |
| --- | --- | --- | --- | --- | --- |
| All | `canonical` | Legacy rows use the existing stage-specific nonblank `input_version()` algorithm; C4 rows hash sorted noncanonical `(name, version_id)` component tuples | Stage canonical projection hash | yes | `legacy_input_changed` only for changed legacy rows |
| Clean | `dlw` | Exact completed-validation DLW artifact version | Exact DLW artifact content hash | yes | `dlw_changed` |
| Clean | `pfw` | Exact PFW auxiliary artifact version | Hash of the exact keyed PFW projection | top-level always; standalone when requested | `pfw_changed` |
| Metadata | `clean_data` | Exact committed `pip` version | Exact committed `pip` content hash | yes | `upstream_output_changed` |
| Metadata | `aux_cpi` | Exact CPI auxiliary artifact version | Hash of keyed CPI projection | top-level always; standalone when requested | `aux_cpi_changed` |
| Metadata | `aux_ppp` | Exact PPP auxiliary artifact version | Hash of keyed PPP projection | top-level always; standalone when requested | `aux_ppp_changed` |
| Metadata | `aux_pop` | Exact population auxiliary artifact version | Hash of keyed population projection | top-level always; standalone when requested | `aux_pop_changed` |
| Metadata | `aux_gdp` | Exact GDP auxiliary artifact version | Hash of keyed GDP projection | top-level always; standalone when requested | `aux_gdp_changed` |
| Metadata | `aux_pce` | Exact PCE auxiliary artifact version | Hash of keyed PCE projection | top-level always; standalone when requested | `aux_pce_changed` |
| Deflate | `clean_data` | Exact committed `pip` version | Exact committed `pip` content hash | yes | `upstream_output_changed` |
| Deflate | `metadata` | Exact committed `pip_meta` version | Exact committed `pip_meta` content hash | yes | `upstream_output_changed` |
| Deflate | `aux_cpi` | Exact CPI auxiliary artifact version | Hash of keyed CPI projection | yes | `aux_cpi_changed` |
| Deflate | `aux_ppp` | Exact PPP auxiliary artifact version | Hash of keyed PPP projection | yes | `aux_ppp_changed` |
| Deflate | `aux_pop` | Exact population auxiliary artifact version | Hash of keyed population projection | yes | `aux_pop_changed` |

Every `version_id` and `content_hash` is nonmissing and nonblank and must pass
the real `pd_validate_manifest()`. For the top-level API, the exact expected
named-row set follows the canonical six measures. For the legacy standalone
adapter, metadata auxiliary rows follow normalized `aux_measures`; unrequested
rows are absent. Tests must cover subset-to-default and default-to-subset
transitions as real input changes without loading unrequested measures.

## Phase 2: Reusable Stage Cores And Typed Run Contracts

### 4. Extract A Shared Clean Stage Core

- **Requirements**: R6, R7, R8, R9, R13, R15, R17, R22
- **Files**: `R/pd_process_data.R`, `R/save_pip.R`, `R/reconcile_pip_inventory.R`, `R/pipeline_context.R`, `R/pipeline_stage_result.R`, `tests/testthat/test-pd_process_data.R`, `tests/testthat/test-save_pip.R`, `tests/testthat/test-reconcile-pip-inventory.R`, `tests/testthat/test-pipeline-stage-result.R`
- **Details**: Isolate the active clean execution loop from `pd_process_data()` behind an internal core that accepts an already prepared authoritative execution object, accepted clean plan slice, run ID, stage context, master inventory, checkpoint policy, and C1 options.
- **Details**: Include normalized `aux_measures` in the standalone preparation
  and clean/metadata core bundle. The new top-level API uses the canonical six
  measures, while the legacy `pd_process_data(aux_measures = ...)` adapter
  forwards its public subset and order unchanged.
- **Details**: The core must not prepare a second planner, acquire another scope lease, or reload latest mutable inputs behind the accepted plan.
- **Details**: Keep cached clean nodes in typed unit accounting without calling `inv_dlw_load()`, `pd_cpfw_merge()`, `pd_dlw_clean()`, or household `pip_read()`.
- **Details**: Keep successful clean receipt sets pending until the C2 checkpoint
  callback finalizes inventory and manifest state. Then mark internal outcome
  accumulators committed, but defer final stage-result/artifact-reference
  construction to Step 7's final-manifest binding.
- **Details**: Treat one clean survey and its complete multi-output receipt set
  as one checkpoint. Do not batch multiple clean surveys through the scalar C2
  finalizer. Apply `checkpoint_size` and `checkpoint_seconds` only to metadata
  and deflate batches unless a separately approved grouped finalizer is added.
- **Details**: Preserve expected `pip_id` set verification, multi-output atomicity, removed-output tombstones, per-survey cleanup, and memory guards.
- **Details**: Consume the independently accepted expected `pip_id` set from
  Step 3 and verify it against both clean and metadata worker outputs before any
  artifact write.
- **Details**: Normalize only existing allowlisted survey-domain failures. Unknown receipt, lease, fence, reconciliation, or checkpoint conditions must escape to the run boundary.
- **Details**: Keep `pd_process_data()` as a public adapter with identical formals, positional order, force/versioning guards, side effects, logging behavior, and master-inventory return. Standalone use may prepare its own execution and run clean plus metadata as today.
- **Details**: Remove or replace the unreachable code after the active return. Do not leave duplicate dead orchestration or a misleading pipeline checkpoint.
- **Test Scenarios**: All cached, one runnable survey, multiple welfare outputs,
  missing/removed output, selected force, recoverable survey failure with
  sibling continuation, checkpoint success, and legacy `aux_measures` subset,
  order, missing-PFW, and default behavior.
- **Error Paths**: Receipt mismatch, incomplete output set, lease loss, stale snapshot, checkpoint failure, unknown worker condition, and `none` row reaching the worker.
- **Tests**: Extend clean, save, reconciliation, API-contract, and typed-result tests with exact call/write counters.
- **Acceptance criteria**: The shared core returns committed clean stage results for top-level orchestration while the public wrapper remains behaviorally compatible and cached surveys trigger zero household loads.

### 5. Extract A Shared Metadata Stage Core

- **Requirements**: R6, R7, R8, R9, R15, R16, R17, R22
- **Files**: `R/pd_process_data.R`, `R/pd_metadata_refresh.R`, `R/pd_aux_attr.R`, `R/reconcile_pip_inventory.R`, `R/pipeline_stage_result.R`, `tests/testthat/test-pd-metadata-refresh.R`, `tests/testthat/test-pd_process_data.R`, `tests/testthat/test-reconcile-pip-inventory.R`
- **Details**: Extract metadata execution behind an internal core that accepts the shared execution state and accepted metadata wave plan.
- **Details**: Require exact committed clean versions/hashes from the refreshed authoritative facts. Do not use intended clean placeholders or a latest-artifact fallback.
- **Details**: Keep cached metadata nodes in result accounting without loading clean household artifacts or auxiliary objects beyond the compact planning projection.
- **Details**: Execute runnable metadata nodes only after their clean prerequisite is current or succeeded in a committed manifest generation.
- **Details**: A failed clean node must produce a blocked metadata unit with `upstream_failed`; no metadata worker or write may occur for that descendant.
- **Details**: Freeze this reason-to-base matrix. Aux-only reasons may refresh
  one exact verified `pip_meta` base. `output_missing`, `output_drift`,
  `metadata_code_changed`, `upstream_output_changed`, and an invalid metadata
  base schema must reconstruct metadata from the exact committed clean artifact
  and frozen auxiliary snapshot. A missing or hash-drifted clean prerequisite
  is fatal or blocked according to the prerequisite contract.
- **Details**: Carry the normalized legacy `aux_measures` selection into
  metadata projection and execution. Do not load, hash, or attach measures that
  a standalone caller did not request.
- **Details**: Preserve current metadata receipts, reconciliation, deflation pointer invalidation after metadata change, and standalone `pd_process_data()` behavior.
- **Test Scenarios**: Cached metadata, clean-success fan-out, changed
  CPI/PPP/population/GDP/PCE projection, metadata output missing/drift,
  metadata-only code change, invalid base schema, aux-only exact-base refresh,
  clean failure block, mixed success/failure pip IDs, and `aux_measures` subset.
- **Error Paths**: Missing exact clean receipt, hash drift, lease loss, reconciliation failure, and checkpoint publication failure.
- **Tests**: Expand metadata refresh, process-data, reconciliation, and stage-result tests.
- **Acceptance criteria**: Metadata runs only for accepted pip IDs with exact committed clean prerequisites, blocked descendants do no work, and public cleaning behavior remains compatible.

### 6. Generalize The Deflation Core For Shared Execution

- **Requirements**: R7, R8, R10, R15, R16, R17, R22
- **Files**: `R/pd_deflate_pipeline.R`, `R/pd_deflation.R`, `R/pipeline_context.R`, `R/pipeline_stage_result.R`, `tests/testthat/test-pd-deflate-pipeline.R`, `tests/testthat/test-pd-deflation.R`
- **Details**: Split the current typed deflation core into a shared prepared-execution path and standalone adapters. The prepared path accepts the shared live execution state and accepted deflate plan rather than invoking `pd_prepare_execution()` itself.
- **Details**: Preserve current exact data/metadata version and hash validation, recoverable allowlist, pending receipt accounting, checkpoint-bound artifact references, and fail-closed integrity behavior.
- **Details**: Represent current deflate nodes as cached units and ensure they do not invoke exact household/metadata loads.
- **Details**: A failed or blocked metadata prerequisite must produce a blocked/skipped deflate unit and no deflate read or write.
- **Details**: Keep `pd_deflate_pipeline()` public formals, positional order,
  summary compatibility, durable aliases, and master-inventory return unchanged.
  Preserve support for
  `options(pipdata.manifest_checkpoint_seconds = Inf)`; do not add checkpoint
  formals to the legacy public wrapper.
- **Details**: Keep `pd_run_deflate_stage()` available internally as an adapter that owns preparation when called independently.
- **Test Scenarios**: Shared prepared execution, standalone public path, all cached, mixed success/failure, blocked metadata, exact pinned inputs, checkpoint batches.
- **Error Paths**: Incomplete action, exact hash drift, unknown worker error, lease loss, and checkpoint failure.
- **Tests**: Extend deflate pipeline and exact-deflation tests while retaining all C3 regression scenarios.
- **Acceptance criteria**: Top-level orchestration can call deflation without a second planner/lease, and standalone public/internal APIs retain current behavior.

### 7. Define The Typed Pipeline Run Aggregate

- **Requirements**: R3, R11, R15, R16, R18, R20
- **Files**: `R/pipeline_context.R`, `R/pipeline_stage_result.R`, `R/pipeline_run_result.R` (new), `R/aaa.R`, `NAMESPACE`, `tests/testthat/test-pipeline-context.R`, `tests/testthat/test-pipeline-stage-result.R`, `tests/testthat/test-pipeline-run-result.R` (new)
- **Details**: Add one compact S3 aggregate with the exact frozen schema below.
- **Details**: Derive counts and aggregate status from validated stage results; do not accept caller-supplied counts that can drift.
- **Details**: Allow one run ID across immutable stage-specific contexts. Each context retains its accepted wave plan hash and manifest parent. Never mutate an earlier stage context after its result is built.
- **Details**: Introduce `pipdata_stage_result` schema version 2 with one
  additive provenance field, `final_evidence_manifest`. Preserve each stage's
  true wave `manifest_before`, wave `manifest_after`, and
  `checkpoint_generations`. Bind artifact references to the final retained
  evidence manifest, whose generation is validated independently from stage
  checkpoint generations. Keep validator/read compatibility for schema-v1
  in-memory results during the transition; new results emit v2.
- **Details**: Keep runtime execution environments out of the aggregate. Portable projection must exclude environments, household data, inventories, catalogs, leases, raw conditions, external pointers, and copied log rows.
- **Details**: Do not treat the aggregate or its serialized projection as currentness, a cache, an exactly-once token, or a durable run manifest.
- **Details**: Return the aggregate visibly from `pd_run_pipeline()`. Register
  only the required S3 print method. Keep constructors and validators internal.
- **Details**: During execution, retain immutable per-wave contexts and mutable
  internal outcome accumulators. Defer construction of all final stage results
  and artifact references until the run stops. Verify every committed receipt
  against the final retained manifest and bind all stage references to that
  final manifest identity, including when more than three later generations
  were published.
- **Details**: A wave that was never accepted after an early terminal failure
  has no fabricated context or stage result. Its aggregate slot is `NULL` and
  its plan hash is `NA_character_`.
- **Test Scenarios**: All success, partial recoverable failures, terminal
  integrity failure after prior success, all cached, blocked descendants, zero
  selection, pre-context failure that propagates without an aggregate, terminal
  failure after clean-context acceptance, during clean, and between clean and
  metadata, deterministic portable projection, and clean success followed by
  more than three metadata/deflate manifest publications.
- **Error Paths**: Duplicate stages, mismatched run IDs, invalid per-wave plan hash map, manifest chain mismatch, prohibited runtime object, inconsistent counts/status.
- **Tests**: New aggregate contract tests plus recursive prohibited-class and serialization tests.
- **Acceptance criteria**: The aggregate accounts for every stage result, validates cross-stage/run provenance, and has a deterministic pointer-free portable projection.

#### Frozen Pipeline Run Result Contract

Constructor:

```r
new_pipdata_pipeline_result(
  run_id,
  stage_results,
  warnings,
  errors,
  plan_hashes,
  manifest_before,
  manifest_after,
  log_ref,
  started_at,
  completed_at,
  terminal = FALSE
)
```

Validator and projection:

```r
validate_pipdata_pipeline_result(x, portable = FALSE)
pd_pipeline_result_portable(x)
```

Exact ordered fields and types:

| Field | Type and invariant |
| --- | --- |
| `schema_version` | Integer scalar `1L` |
| `run_id` | Nonempty character scalar shared by all stage results |
| `status` | Derived character scalar: `success`, `partial`, `failed`, `cached`, or `skipped` |
| `terminal` | Logical scalar |
| `stage_results` | Exact named list in order `clean`, `metadata`, `deflate`; each value is one validated `pipdata_stage_result` or `NULL` when that wave was never accepted |
| `counts` | Exact named integer list: `selected`, `attempted`, `succeeded`, `failed`, `skipped`, `cached`, `blocked`, `warnings`, `errors` |
| `warnings` | List of stable C3 condition records for run-boundary warnings only |
| `errors` | List of stable C3 condition records for run-boundary errors only |
| `plan_hashes` | Exact named character vector `initial`, `clean`, `metadata`, `deflate`; unavailable waves use `NA_character_` |
| `manifest_before` | Valid C2 manifest identity or `NULL` |
| `manifest_after` | Valid final C2 manifest identity or `NULL` only when no valid manifest exists |
| `log_ref` | Exact list `name`, `run_id`, `summary_discriminator`, `log_checkpoint`; discriminator is `pipeline_run_summary_inf` |
| `started_at` | UTC `POSIXct` scalar in-memory and canonical UTC string when portable |
| `completed_at` | UTC `POSIXct` scalar not before `started_at`; canonical UTC string when portable |

Stage order is always clean, metadata, deflate. Counts are derived from accepted
wave units; `blocked` counts units with the C3 `upstream_failed` reason and is a
subset of `skipped`. Warning/error counts equal all stage-condition totals plus
the lengths of the run-boundary warning/error lists. A run-boundary condition
uses the active wave, or the next wave being accepted, as its valid C3 `stage`
and sets `operation = "run_boundary"`; do not add a `pipeline` stage.
`plan_hashes[[stage]]` is `NA_character_` exactly when the corresponding stage
result is `NULL`. Aggregate counts exclude unaccepted `NULL` waves.
Status precedence is: terminal plus any committed success
is `partial`; terminal without committed success is `failed`. For nonterminal
results, if `attempted > 0` and both success and failure exist, status is
`partial`; if `attempted > 0` and all attempted units failed, status is
`failed`; if `attempted > 0` and all attempted units succeeded, status is
`success`. Otherwise, if `selected == 0`, status is `skipped`; if all selected
units are cached, status is `cached`; otherwise status is `skipped`.
The portable projection unclasses objects, canonicalizes table/list
order, removes runtime state, and uses R serialization version 3 for byte-level
determinism tests.

#### Stage Result V2 Evidence Rule

`pipdata_stage_result` v2 keeps its existing exact top-level fields. Its exact
`provenance` names become `release`, `identity`, `scope_id`, `context_hash`,
`plan_hash`, `manifest_before`, `manifest_after`, `checkpoint_generations`,
`final_evidence_manifest`, and `stage_reason_codes`. For units with artifacts,
`final_evidence_manifest` is a valid C2 identity and every artifact reference
uses that generation. For cached/skipped/failed-only results without artifacts,
it may equal the final run manifest or be `NULL` only when no valid manifest
exists. The validator proves the receipt against the supplied final manifest
at construction, validates reference generation against
`final_evidence_manifest`, and does not add that generation to the stage's true
`checkpoint_generations` unless the stage itself published it.

## Phase 3: Stage-Wave Orchestration, API, And Operations

### 8. Implement The Internal Stage-Wave Executor

- **Requirements**: R1, R2, R3, R6, R7, R8, R14, R15, R16, R17, R18, R25
- **Files**: `R/pd_run_pipeline.R` (new), `R/dependency_execution.R`, `R/dependency_manifest.R`, `R/pipeline_context.R`, `R/pipeline_run_result.R`, `R/pd_process_data.R`, `R/pd_deflate_pipeline.R`, `tests/testthat/test-pd-run-pipeline.R` (new), `tests/testthat/test-dependency-execution.R`
- **Details**: Build one internal run boundary that validates pure arguments and
  the completed-validation inventory, creates the C2 dependency context,
  acquires the scope writer lease, then re-reads the manifest and builds the
  authoritative execution snapshot, facts, and plan while the lease is held.
  Any pre-lease report or advisory plan is non-authoritative and must be
  discarded or compared only.
- **Details**: Assert lease ownership and manifest parent identity before the
  first worker and before returning an all-cached result.
- **Details**: Treat `inv` as the full authoritative completed-validation state.
  Preliminary pre-lease validation may anti-join the supplied master, but after
  lease acquisition reload the exact master inventory and rerun whole-survey
  removal detection before accepting the plan. If a whole survey has
  disappeared, release the lease, fail with
  `pipdata_upstream_survey_removed`, and make no worker, artifact, inventory, or
  manifest write.
  Survey retirement/tombstoning is a separate deferred policy.
- **Details**: After lease acquisition and exact master reload, rerun targeted
  force and bootstrap selector resolution. Do not carry pre-lease survey/pip
  mappings into the accepted plan. Add a race fixture that changes master state
  between preliminary preflight and lease acquisition.
- **Details**: Generate one run ID. Build immutable clean, metadata, and deflate contexts as each accepted wave becomes authoritative.
- **Details**: Execute clean first. After every finalized clean checkpoint set, update the live execution state and refresh facts under the same lease, code, snapshot, and parent fencing rules.
- **Details**: Build metadata actions from actual committed clean receipt sets. New or removed pip IDs must come from verified outputs/tombstones, not stale inventory fan-out.
- **Details**: After metadata checkpoints, refresh facts again and build deflate actions from actual committed clean and metadata versions/hashes.
- **Details**: Never let an initial forecast override a freshly accepted downstream wave plan. Record initial and per-wave plan hashes in the run aggregate.
- **Details**: Keep current nodes cached, changed nodes runnable, and
  failed-prerequisite descendants blocked. Initial downstream forecasts are not
  result units; every accepted stage-wave node appears exactly once.
- **Details**: `pd_run_pipeline()` uses fixed `capture_at_run_boundary` behavior
  after the first complete stage context exists. It stops all later writes and
  returns a terminal aggregate with complete accepted-unit accounting.
  Argument, inventory, context, initial manifest, lease-acquisition, and
  context-construction failures propagate without a result. User interrupts
  and cancellation always propagate. Existing public stage wrappers retain
  abort behavior.
- **Details**: Do not persist a run cursor. Restart is a new authoritative plan over the last valid manifest and current Stamp facts.
- **Test Scenarios**: All current, clean-only work, metadata-only work,
  deflate-only work, full chain, clean fan-out change, mixed recoverable
  failure, terminal failure after partial checkpoint, and master/manifest
  change between preliminary preflight and lease acquisition.
- **Error Paths**: Advisory plan mismatch, lease loss between waves, manifest parent change, code drift, catalog drift, checkpoint failure, and interrupted run.
- **Tests**: New pipeline executor tests with ordered call traces, plan hashes, manifest identities, exact load/write counters, and injected faults.
- **Acceptance criteria**: The executor runs topological waves under one authoritative scope, binds every downstream worker to committed upstream receipts, and converges by replan after interruption.

### 9. Export The Thin `pd_run_pipeline()` API

- **Requirements**: R12, R13, R19, R22, R23
- **Files**: `R/pd_run_pipeline.R` (new), `R/dependency_plan.R`, `R/pipdata-options.R`, `NAMESPACE`, `man/pd_run_pipeline.Rd` (generated), `tests/testthat/test-pd-run-pipeline.R`, `tests/testthat/test-dependency-api-contract.R`
- **Details**: Export exactly the approved arguments: `inv`, `force`, `verbose`, `force_surveys`, `bootstrap`, `bootstrap_entities`, `checkpoint_size`, and `checkpoint_seconds`.
- **Details**: Freeze the public signature exactly as:

```r
pd_run_pipeline(
  inv = NULL,
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE),
  force_surveys = NULL,
  bootstrap = FALSE,
  bootstrap_entities = NULL,
  checkpoint_size = 25L,
  checkpoint_seconds = Inf
)
```

- **Details**: The top-level API always uses the canonical six measures `cpi`,
  `ppp`, `pop`, `gdp`, `pce`, and `pfw`. The legacy `pd_process_data()` adapter
  retains its public `aux_measures` selection.
- **Details**: Freeze defaults and validation: `checkpoint_size = 25L` must be a
  positive whole-number scalar; `checkpoint_seconds = Inf` or a positive
  numeric scalar. Invalid values fail before lease acquisition.
- **Details**: Preserve global-force versus targeted-force mutual exclusion and guard ordering before any versioning or storage mutation.
- **Details**: Reuse C1 lookup-first resolution and ambiguity behavior. Do not add a second force resolver.
- **Details**: `force_surveys` adds one forced clean action for each resolved
  selected survey but does not suppress ordinary stale actions elsewhere. Only
  selected chains receive the `forced` reason. In an otherwise-current release,
  no unselected worker runs; if another survey is independently stale, both run.
- **Details**: Preserve explicit bootstrap. `bootstrap_entities` requires
  `bootstrap = TRUE`. A `pip_id` selector maps to its unique owning survey and
  includes the owning clean node plus the complete atomic clean output chain.
  Unselected surveys remain unknown. Unknown or ambiguous selectors fail before
  any worker or artifact/inventory write after authoritative post-lease
  re-resolution. Do not infer or adopt pre-C2 provenance.
- **Details**: After all pure guards and authoritative setup succeed, global
  `force = TRUE` switches Stamp versioning to `"timestamp"` for the full
  write-capable run and restores the prior setting on every exit.
  `force_surveys` and bootstrap retain content versioning.
- **Details**: Return the typed run aggregate, not the master inventory. Document that the new top-level API differs intentionally from legacy stage-wrapper returns.
- **Details**: Do not expose a mutable `dependency_plan` execution argument. `pd_change_report()` remains the dry-run/read-only inspection surface.
- **Test Scenarios**: Default inventory load, injected inventory, global force,
  targeted survey ID, targeted pip ID, forced survey plus independently stale
  survey, unknown ID, ambiguous reverse map, survey-ID canary, pip-ID
  dependency-closed canary, no selection, and Stamp versioning restoration
  after success/preflight/terminal failure.
- **Error Paths**: Conflicting force arguments, invalid checkpoint values, invalid bootstrap combination, malformed completed-validation inventory, preflight setup failure.
- **Tests**: New public API tests and exact formal/positional snapshots.
- **Acceptance criteria**: Operator and batch scripts have one exported
  incremental entry point, but production activation remains blocked pending
  signed target Windows/SMB fencing and immutable unique-rename evidence.
  Targeted force is additive, affects only selected chains with the forced
  reason, and all older exports remain unchanged.

### 10. Finalize Failure, Retry, And Resume Semantics

- **Requirements**: R15, R16, R17, R18, R19
- **Files**: `R/pd_run_pipeline.R`, `R/dependency_execution.R`, `R/dependency_manifest.R`, `R/reconcile_pip_inventory.R`, `R/pd_process_data.R`, `R/pd_deflate_pipeline.R`, `R/pipeline_run_result.R`, `tests/testthat/test-pd-run-pipeline.R`, `tests/testthat/test-dependency-manifest.R`, `tests/testthat/test-reconcile-pip-inventory.R`
- **Details**: Define a closed allowlist of survey-domain failures for each stage. Unknown worker or storage errors are fatal by default.
- **Details**: Preserve three distinct success states: worker completed, exact receipt pending, and checkpoint committed. Only the last becomes `success`.
- **Details**: On recoverable failure, keep prior immutable Stamp versions, record one stable condition, and block only graph descendants. Independent siblings may continue under the existing entity policy.
- **Details**: On terminal integrity failure, mark pending units `checkpoint_uncommitted`, the active unit `fatal_uncommitted`, and later selected units `upstream_failed` or the approved equivalent. Stop all later writes.
- **Details**: Verify that any durable current-view invalidation is narrowly reconciled and does not erase artifact history or publish false success.
- **Details**: Inject faults before worker, after write, after receipt, after release inventory, after master inventory, before/after manifest rename, and on lease loss.
- **Details**: After each fault, start a new invocation and prove convergence to the uninterrupted final manifest/inventory/artifact state.
- **Details**: A manifest publication failure may leave inventories ahead. Preserve the C2 rule that the prior valid manifest remains authoritative and causes safe rescheduling.
- **Test Scenarios**: One failure at each checkpoint boundary, partial batch success, same-content retry, orphan receipt, prior valid generation after corrupt latest, all generations corrupt.
- **Error Paths**: Failure classification ambiguity, missing condition identity, manifest chain break, and retry attempting to trust file existence.
- **Tests**: Expand manifest/reconciliation tests and add end-to-end restart cases to the pipeline test file.
- **Acceptance criteria**: A crash may leave verified release or master
  inventory versions ahead of the authoritative manifest. No stage unit is
  successful without a discoverable manifest generation. A fresh invocation
  uses the prior valid manifest, detects the inventory-ahead state, and
  converges to the uninterrupted final state without a run cursor.

### 11. Integrate Unified Logging, Reporting, And Instrumentation

- **Requirements**: R1, R2, R14, R20, R21, R24
- **Files**: `R/aaa.R`, `R/pd_run_pipeline.R`, `R/pd_change_report.R`, `R/log_report.R`, `R/pd_process_data.R`, `tests/testthat/test-logging-integration.R`, `tests/testthat/test-log_report.R`, `tests/testthat/test-pd-change-report.R`, `tests/testthat/test-pd-run-pipeline.R`
- **Details**: Reuse `pipdata_log` and one run ID. Add exactly one new internal
  discriminator, `pipeline_run_summary_inf`, for the top-level run. Preserve
  `process_summary_inf` and `deflate_summary_inf` for stage compatibility.
- **Details**: Emit one `pipeline_run_summary_inf` after a run context exists,
  with exact fields `run_id`, `status`, `terminal`, `n_selected`, `n_attempted`,
  `n_success`, `n_failed`, `n_cached`, `n_blocked`, `clean_status`,
  `metadata_status`, `deflate_status`, `manifest_before_generation`,
  `manifest_after_generation`, `started_at`, and `completed_at`.
- **Details**: Freeze summary storage types and sentinels. Each `*_status` is a
  validated stage status or `NA_character_` when its stage result is `NULL`.
  Manifest generation fields are integer scalars or `NA_integer_`. Every `n_*`
  field is an integer scalar, including zero.
- **Details**: Preserve existing stage summary contracts such as `deflate_summary_inf`. Avoid duplicate failure logs between worker adapters, stage cores, and run boundary.
- **Details**: Store string discriminators in `logmeta$info`/`error`; store condition messages in `condition_msg`; correlate failures with compact condition IDs.
- **Details**: Do not call typed log functions from frames whose formals hold household data or full inventories. Log compact counts, IDs, hashes, durations, and reason summaries only.
- **Details**: Extend `pd_change_report()` printed output with stage/disposition and reason counts while preserving invisible structured access.
- **Details**: Capture compact instrumentation: planning duration, selected/current/stale/forced counts, cached/runnable/blocked counts, reason counts, worker/checkpoint durations, alias read/write counts, and household load counts where instrumentation is available.
- **Details**: Keep logging independent of `pipdata.verbose`.
- **Details**: Add `pipeline_run_summary_inf` to `.log_internal_types`, define
  latest-run selection by `run_id` plus completion timestamp, and add one
  dedicated `log_report()` section without leaking it into generic summaries.
- **Details**: Replace or remove the unreachable historical pipeline summary/checkpoint code after `pd_process_data()`'s active return.
- **Test Scenarios**: Full success, all cached, partial failure, terminal before
  metadata acceptance, repeated runs, latest report selection, empty/zero
  selection, and verbose false.
- **Error Paths**: Logging/checkpoint failure must not change business outcome; malformed discriminator or captured large object must fail tests.
- **Tests**: Logging integration, report rendering, argument-capture, duplicate-event, and no-large-object tests.
- **Acceptance criteria**: One compact run summary and correlated stage/failure records describe the run, reporting matches the authoritative plan, and persistent logs retain no household data or full inventories.

## Phase 4: End-To-End Verification And Documentation

### 12. Build The Exact Invalidation Scenario Suite

- **Requirements**: R5, R13, R14, R16, R18, R21, R25
- **Files**: `tests/testthat/test-pd-run-pipeline.R`, `tests/testthat/test-dependency-plan.R`, `tests/testthat/test-dependency-inputs.R`, `tests/testthat/helper-dependency-fixtures.R` (new only if shared fixtures are necessary)
- **Details**: Build small deterministic Stamp/catalog/inventory fixtures for multiple countries, multiple years, multiple survey IDs, and multiple pip IDs per clean survey.
- **Details**: Assert exact worker calls, household loads, artifact writes, receipt sets, plan states, reason codes, and manifest records. Do not assert only aggregate counts.
- **Details**: Cover DLW source change, clean/recode code change, keyed PFW change, CPI change, PPP change, population change, GDP/PCE metadata change, metadata code change, deflate code change, and output missing/drift.
- **Details**: Implement the Colombia 2018 CPI fixture with at least one other Colombia year and one unrelated country/year. Assert every clean node is cached and only matching Colombia 2018 metadata/deflate nodes run.
- **Details**: Cover selected `force_surveys` by survey ID and pip ID in an
  otherwise-current fixture. Assert the full selected chain runs and no
  unselected worker/write occurs in that fixture.
- **Details**: Add the mixed additive-force fixture: one otherwise-current
  survey is forced while another survey is independently stale. Both execute;
  only the selected chain carries the forced reason.
- **Details**: Re-run every successful incremental scenario without changes and assert all selected nodes are cached with zero artifact writes.
- **Details**: Assert cached clean nodes cause zero household artifact loads.
- **Details**: Prove clean no-op convergence with one output, multiple outputs,
  and permuted receipt/catalog row order using the shared receipt canonicalizer.
- **Details**: Remove one whole survey from a static authoritative
  completed-validation fixture and assert the preliminary check raises
  `pipdata_upstream_survey_removed` before lease acquisition. Add a race fixture
  where removal appears only in the post-lease exact master reload; assert the
  lease is released and the condition occurs before any worker, artifact,
  inventory, or manifest write. Do not implement retirement.
- **Test Scenarios**: All required production invalidation cases plus mixed data-level area/national projections and multi-output clean fan-out.
- **Error Paths**: Auxiliary change with no matching entities, ambiguous mapping, changed input outside selection, and removed pip output.
- **Tests**: New C4 scenario sections in `test-pd-run-pipeline.R` plus focused input/plan unit tests.
- **Acceptance criteria**: The scenario suite proves direct affected descendants and exact non-effects for every required input type, including Colombia 2018 and immediate no-op reruns.

### 13. Run Compatibility, Performance, And Scope Audits

- **Requirements**: R14, R22, R23, R24
- **Files**: `tests/testthat/test-dependency-api-contract.R`, `tests/testthat/test-dependency-performance.R`, `tests/testthat/test-pd_process_data.R`, `tests/testthat/test-pd-deflate-pipeline.R`, `tests/testthat/test-pipeline-context.R`, `tests/testthat/test-pipeline-stage-result.R`, `DESCRIPTION`, `NAMESPACE`
- **Details**: Snapshot all existing exported formals, positional order, return shapes, aliases, and sentinel behavior touched by refactoring.
- **Details**: Prove planner cost depends on metadata/catalog/entity size, not household row count. Add explicit zero-load assertions for cached work.
- **Details**: Audit plan joins and accumulators for quadratic table growth, repeated full catalog scans, and repeated `rbind` copies.
- **Details**: Audit all typed logging call frames for household/inventory retention.
- **Details**: Audit `DESCRIPTION`, imports, class registrations, files, and schemas for no new package dependency, external framework, second DAG/manifest, run cursor, or parallel implementation.
- **Details**: Preserve the production activation warning and ensure local tests do not claim target Windows/SMB readiness.
- **Details**: Use fixed 1,250-entity and 2,500-entity metadata fixtures. Require
  zero household reads, zero per-entity external catalog calls, and at most one
  bulk query per required alias in each authoritative snapshot. Let `c1250` and
  `c2500` be the total instrumented projection/join call counts. Require
  `c2500 <= 2 * c1250 + C`, where `C` is one documented fixed setup count that
  is asserted unchanged between fixtures. Bulk catalog-query counts must be
  identical. Record elapsed time as diagnostics only; do not use a fragile
  wall-clock pass threshold.
- **Test Scenarios**: Fixed 1,250/2,500-entity metadata fixtures, row-order
  permutations, repeated checkpoint batches, and existing stage public calls.
- **Error Paths**: Unexpected export, changed formal order, prohibited dependency, large object in result/log, and planner household read.
- **Tests**: API contract, dependency performance, stage regression, typed contract, and executed diff/schema audits.
- **Acceptance criteria**: Existing APIs remain compatible, planner/cache
  behavior satisfies the fixed two-size operation-count inequality and catalog
  bounds, and the implementation stays inside C4 boundaries.

### 14. Update Documentation And Complete Package Verification

- **Requirements**: R12, R19, R20, R22, R24
- **Files**: `R/pd_run_pipeline.R`, generated `man/pd_run_pipeline.Rd`, `README.md`, `NEWS.md`, relevant `vignettes/` or `inst/doc/` source if one already owns pipeline operation, `compound-gpid.context.md`, generated `NAMESPACE`
- **Details**: Document the new exported wrapper, arguments, return contract, complete state vocabulary, force semantics, explicit bootstrap, and no-op behavior.
- **Details**: Document the three durable nodes and clarify that internal functions remain fingerprint components, not independent cached artifacts.
- **Details**: Document Stamp versus manifest authority, result-bound checkpointing, failure/block/resume behavior, and the absence of a run cursor.
- **Details**: Document the keyed auxiliary invalidation model with the Colombia 2018 example.
- **Details**: Keep the target Windows/SMB fencing and immutable unique-rename requirement explicit. Do not claim production activation.
- **Details**: Update tactical context to replace stale statements about the C4 boundary. Do not modify the protected project charter during `/cg-work`.
- **Details**: Run targeted tests, full tests, documentation generation, package check, and final diff/schema scope audit.
- **Test Scenarios**: Roxygen examples are side-effect-free or appropriately guarded; generated help matches exported formals; documented states and discriminators match code.
- **Error Paths**: Stale generated docs, check warning/error, undocumented export, broken example, and stale activation language.
- **Tests**: `devtools::document()`, targeted `devtools::test(filter = ...)`, `devtools::test()`, and `devtools::check()`.
- **Acceptance criteria**: Documentation and generated surfaces are current, all required evidence passes, and the final diff contains no prohibited scope expansion.

## Testing Strategy

### Unit Level

- Validate complete-node planning, state derivation, named input projections,
  reason derivation, plan hashing, aggregate result validation, and portable
  serialization with small in-memory `data.table` fixtures.
- Test exact types, exact names, row-order determinism, duplicate rejection,
  and invalid cross-field combinations.
- Test public formals and positional order with exact snapshots.

### Integration Level

- Use temporary package-local Stamp roots and deterministic mocked catalogs.
- Exercise real shared planning, stage cores, receipts, reconciliation, and
  manifest publication wherever feasible.
- Count worker calls, household reads, alias writes, checkpoint callbacks, and
  manifest generations rather than relying on output summaries alone.
- Keep each test self-contained with local options, roots, clocks, and log state.

### Failure Injection

- Inject failures at every preflight, worker, receipt, inventory, fence, and
  manifest boundary.
- Verify prior artifacts, last-success records, blocked descendants, terminal
  accounting, and restart convergence.
- Treat interrupts/cancellation as escaping conditions; do not normalize them
  as recoverable stage failures.

### Performance

- Assert planning does not call household loaders.
- Compare planner behavior across metadata entity counts, not household rows.
- Trace repeated catalog and table operations to detect quadratic growth.
- Verify typed results, logs, and runtime cleanup do not retain large objects.

### Verification Commands

Targeted package-loaded tests:

```r
devtools::test(
  filter = paste(
    "dependency-contract",
    "dependency-inputs",
    "dependency-plan",
    "dependency-execution",
    "dependency-manifest",
    "dependency-bootstrap",
    "dependency-performance",
    "save_pip",
    "reconcile-pip-inventory",
    "code-fingerprint",
    "pd-change-report",
    "pd_process_data",
    "pd-metadata-refresh",
    "pd-deflate-pipeline",
    "pd-deflation",
    "pd-run-pipeline",
    "pipeline-context",
    "pipeline-stage-result",
    "pipeline-run-result",
    "pipdata_get_gmd",
    "pipdata_validate_gmd",
    "pipdata_dlw_process",
    "pipdata_dlw_compare",
    "dlw-unified-logging",
    "logging-integration",
    "log_report",
    "dependency-api-contract",
    sep = "|"
  )
)
```

Final package verification:

```r
devtools::test()
devtools::document()
devtools::check()
```

The implementation workflow must run these commands, not mark evidence passed
from static inspection. Tests that need unavailable target Windows/SMB storage
must remain a documented production block rather than an accepted local proxy.

## Documentation Checklist

- [ ] `pd_run_pipeline()` has complete roxygen parameters, return, details, and examples.
- [ ] Public formals and generated `NAMESPACE`/Rd files match implementation.
- [ ] README explains the executable three-stage pipeline and manifest authority.
- [ ] NEWS records the new exported API and preserves compatibility statements.
- [ ] Operational documentation explains states, invalidation, force, bootstrap, failure, and restart.
- [ ] Colombia 2018 is included as a targeted invalidation example.
- [ ] Internal substeps are described as fingerprint components, not cached artifacts.
- [ ] No run manifest, run cursor, parallelism, or exactly-once claim is implied.
- [ ] Windows/SMB production activation evidence remains explicitly outstanding.
- [ ] Tactical context reflects the active C4 architecture after implementation.

## Plan Review Resolution

All findings from the 2026-08-28 `cg-plan-critic` review are incorporated.
No P1, P2, or P3 finding is accepted or deferred.

| Findings | Resolution location |
| --- | --- |
| P1.1-P1.2 renderer headings | Exact `Risks & Mitigations` and `Out of Scope` headings below |
| P1.3 lease/plan race | Step 8 lease-before-authoritative-plan contract |
| P1.4 clean receipt symmetry | Step 3 shared receipt-set canonicalizer |
| P1.5 independent expected pip IDs | Steps 3 and 4 pre-worker accepted set |
| P1.6 exact entity keys | Step 3 canonical key adapter |
| P1.7 legacy hash compatibility | Step 3 dual schema-1 comparison paths |
| P1.8 metadata reconstruction | Step 5 reason-to-base matrix |
| P1.9 additive force | R13 and Step 9 mixed-force contract |
| P1.10 fatal public behavior | Step 8 fixed capture boundary |
| P1.11 retained references | Step 7 final-manifest binding |
| P2.1 aggregate schema | Step 7 frozen result contract |
| P2.2 durable names/reasons | Step 3 frozen named-input table |
| P2.3 forecasts versus accepted nodes | Steps 2 and 8 accepted-wave accounting |
| P2.4 clean batch granularity | Step 4 one-survey checkpoint rule |
| P2.5 inventory-ahead crash state | Step 10 acceptance criteria |
| P2.6 bootstrap closure | Step 9 dependency-closed selector contract |
| P2.7 survey removal | R25 and Steps 8/12 fail-closed policy |
| P2.8 legacy auxiliary subsets | Steps 4 and 5 normalized `aux_measures` |
| P2.9 global force versioning | Step 9 timestamp switch/restoration |
| P2.10 logging discriminator | Step 11 `pipeline_run_summary_inf` schema |
| P2.11 targeted suites | Expanded verification filter |
| P2.12 snapshot identity | Step 1 deterministic identity contract |
| P2.13 checkpoint API | Steps 6 and 9 corrected surfaces |
| P2.14 stale charter | Context and final protected-charter constraint |
| P3.1 measurable performance | Step 13 fixed 1,250/2,500-entity operation-count test |
| P3.2 activation wording | Step 9 operator/batch wording and activation block |
| P1.12 manifest-valid input rows | Step 3 nonblank version contract and recode fingerprint ownership |
| P1.13 stage/final evidence provenance | Step 7 stage-result v2 `final_evidence_manifest` rule |
| P1.14 mutable master race | Step 8 post-lease master reload and selector re-resolution |
| P1.15 unaccepted waves | Step 7 nullable stage-result slots and exact hash invariant |
| P2.15 auxiliary subset rows | Step 3 invocation-dependent named-row set |
| P2.16 aggregate derivation | Step 7 skipped/status/condition/count rules |
| P3.3 linear evidence | Step 13 fixed 1,250/2,500 count inequality |
| P2.17 survey-removal timing | R25 and Steps 8/12 preliminary plus post-lease checks |
| P2.18 canonical version token | Step 3 explicit non-artifact-selector exception |
| P2.19 recode reason ownership | Step 3 component-level fingerprint comparison/publication |
| P2.20 zero-attempt status | Step 7 ordered `attempted > 0` predicates |
| P2.21 report/removal parity | Step 1 shared pure removal check |
| P2.22 summary sentinels | Step 11 exact nullable-stage/log field types |

## Risks & Mitigations

| Risk | Impact | Mitigation |
| --- | --- | --- |
| Complete `none` rows reach existing workers | Current artifacts recompute | Audit every action consumer and assert zero worker calls for `none` |
| Report and execution drift again | Operators trust a non-executable plan | One shared fact helper with parity tests |
| Downstream plan uses intended upstream versions | False-current manifest records | Refresh facts after committed checkpoints and bind exact receipts |
| Named inputs accidentally change manifest authority | Competing provenance semantics | Keep canonical record hash authoritative and use existing input table only |
| Legacy canonical-only records are treated as corrupt | Unnecessary baseline rebuild | Validate/read them unchanged and use generic reason fallback only when changed |
| Clean multi-output fan-out is inferred from stale inventory | Missing or ghost descendants | Derive fan-out from finalized receipt set and tombstones |
| Stage core extraction changes public behavior | Breaking release | Shared core plus exact signature/return/side-effect regressions |
| One mutable context is reused across different plan hashes | Invalid C3 provenance | Freeze immutable context per stage wave under one run ID |
| Recoverable normalization swallows integrity failure | Corrupt later writes | Closed allowlist and default-fatal classification |
| Partial write is mistaken for success | False cache hit | Checkpoint-bound success and restart fault matrix |
| Logging retains household objects | OOM regression | Log only from compact orchestration frames and inspect captured args |
| Planner scans global auxiliary data repeatedly | Slow large runs | Precompute keyed projections once per snapshot and join by entity |
| Typed table accumulation becomes quadratic | Large-run slowdown | Preallocate/list-accumulate and `rbindlist()` once per boundary |
| C4 expands into generic orchestration | Delivery delay and duplicate DAG | Enforce three durable nodes and final scope audit |
| Local tests are mistaken for production storage proof | Unsafe activation | Keep explicit Windows/SMB block in docs and completion constraints |
| Authoritative plan is built before lease acquisition | Stale cached result or unnecessary work | Acquire lease, then rebuild plan and assert parent even for all-cached runs |
| Clean receipt sets use asymmetric hashes | Immediate rerun reports output drift | One shared canonical tuple/hash function with permutation tests |
| Expected clean outputs come from worker output | Missing welfare artifact is accepted | Derive accepted IDs before worker with shared cache-ID builder |
| Missing entity keys trigger global auxiliary hash | Unrelated countries/years recompute | Exact key adapter and fail-closed missing/ambiguous behavior |
| Removed upstream survey remains silently current | Obsolete artifacts stay active | Check preliminarily before lease and authoritatively after lease/master reload; fail before pipeline writes and defer retirement |
| Early stage references point to pruned generations | Returned provenance is unverifiable | Bind every stage reference to final retained manifest |
| Artifact-component rows contain blank or synthetic artifact versions | Manifest validation or exact loading fails | Require nonblank real artifact versions; reserve the synthetic token exception for `canonical` and keep recode spec in fingerprints |
| Pre-lease master mappings survive into accepted plan | Force/bootstrap/removal targets are stale | Reload master and rerun all mapping after lease acquisition |
| Early terminal failure fabricates later wave results | Aggregate claims unaccepted work | Use exact nullable stage slots and exclude them from counts |

## Out of Scope

- DLW acquisition and validation execution inside `pd_run_pipeline()`.
- Independently cached load, PFW merge, recode, auxiliary attachment, or save nodes.
- A second dependency DAG, step-hash artifact, manifest, provenance store, or inventory authority.
- Persisted observational run manifests, resumable run cursors, or exactly-once claims.
- Parallel survey or pip-ID execution.
- Cross-release or cross-scope scheduling.
- External workflow/orchestration frameworks.
- Estimation, regional/global poverty aggregation, and downstream indicator production.
- Changes to pipfun, pipload, stamp, pipaux, wbpip, or other external packages.
- New data-level sentinels beyond the existing `area` pointer.
- Adoption of unverifiable pre-C2 artifacts.
- Retirement/tombstoning of a whole survey removed from completed-validation state.
- Protected project charter edits during implementation.
- Production activation before target Windows/SMB evidence is complete.

## Completion Contract

### Outcome

Pipdata exposes a thin `pd_run_pipeline()` orchestration API that incrementally
executes the existing `clean`, `metadata`, and `deflate` artifact stages through
one C2-authoritative stage-wave executor. It reports every selected node, binds
downstream execution to committed upstream receipts, returns compact
C3-compatible typed results, and safely converges after failures or
interruptions without loading cached household data.

### Verification Surface

| ID | Phase | Evidence Required | Command/Artifact | Required |
| --- | ---: | --- | --- | --- |
| V1 | 1 | `pd_change_report()` and pre-lease advisory preparation share deterministic facts/`snapshot_identity`; execution rebuilds the authoritative plan after lease acquisition | Report/execution parity and lease-order tests | yes |
| V2 | 1 | Every selected node has deterministic current/stale/forced and cached/runnable/blocked disposition | Complete-node planner/state tests | yes |
| V3 | 1 | Frozen named DLW, PFW, CPI, PPP, population, GDP/PCE, and upstream-output inputs produce exact per-entity reasons through the canonical key adapter | Dependency input/plan matrix tests | yes |
| V4 | 2 | Clean shared core preserves `pd_process_data()` formals, `aux_measures`, force guards, independently accepted output sets, receipts, tombstones, and master return | Clean compatibility and multi-output tests | yes |
| V5 | 2 | Metadata shared core uses exact committed clean receipts and preserves standalone behavior | Metadata compatibility and exact-input tests | yes |
| V6 | 2 | Typed run aggregate and stage contexts/results contain no data, inventories, environments, raw conditions, or log rows in portable form | Contract and recursive serialization tests | yes |
| V7 | 3 | One stage-wave executor acquires the lease before authoritative planning and replans after committed clean and metadata checkpoints under one fenced scope | Ordered lease/plan/worker/checkpoint trace tests | yes |
| V8 | 3 | `force_surveys` adds selected chains without suppressing independently stale work; only selected chains carry `forced` | Mixed force call-count/reason tests | yes |
| V9 | 3 | Recoverable failures block only descendants; integrity failures stop later writes; every selected node is terminally accounted | Failure classification and write-counter tests | yes |
| V10 | 3 | Fault injection at every receipt/inventory/manifest boundary, including inventory-ahead state, resumes to the uninterrupted final state | Crash/restart matrix | yes |
| V11 | 3 | Unified logging emits one exact `pipeline_run_summary_inf`; `pd_change_report()` reports cache/reason counts | Logging and report tests | yes |
| V12 | 4 | Colombia 2018 CPI change runs only matching metadata/deflate nodes | End-to-end Colombia fixture and exact call/write assertions | yes |
| V13 | 4 | Immediate rerun is fully cached under one/multiple/permuted clean receipts, with zero stage writes and zero cached clean household loads | No-op/canonicalizer/load-counter tests | yes |
| V14 | 4 | Existing exported APIs retain exact formals, positional order, return types, and durable aliases | API snapshot/regression tests | yes |
| V15 | final | Expanded targeted C1/C2/C3/A/B/C4 receipt, reconciliation, fingerprint, strict-deflation, B3-wrapper, logging, and orchestration tests pass | Exact expanded `devtools::test(filter = ...)` command in Testing Strategy | yes |
| V16 | final | Full package tests pass | `devtools::test()` | yes |
| V17 | final | Documentation is current and package check has no new errors or warnings | `devtools::document()` and `devtools::check()` | yes |
| V18 | final | Final scope audit finds no second manifest/DAG, external framework, parallel execution, run cursor, or production-activation claim | Executed diff and schema audit | yes |
| V19 | 1 | Legacy canonical-only rows remain current when unchanged and use `legacy_input_changed` only after an actual legacy hash change | Legacy/C4 dual-comparison tests | yes |
| V20 | 3 | Survey-ID and pip-ID bootstrap selectors are dependency-closed after post-lease master reload; whole-survey upstream removal fails before any worker or artifact/inventory write | Bootstrap/removal/race tests | yes |
| V21 | 3 | Stage-result v2 preserves true wave manifests/checkpoints while all artifact references are proven by the final retained evidence manifest after more than three later publications | Cross-stage manifest-retention and v1/v2 validator tests | yes |
| V22 | 4 | Fixed 1,250/2,500-entity plans perform zero household reads, zero per-entity catalog calls, identical bounded bulk queries, and `c2500 <= 2*c1250+C` projection/join calls | Operation-count performance test | yes |

### Constraints

| ID | Phase | Constraint | Check |
| --- | ---: | --- | --- |
| C1 | all | Stamp remains authoritative for immutable artifact versions | Receipt/catalog assertions |
| C2 | all | The C2 manifest remains the only pipdata currentness/provenance store | Schema and file-surface audit |
| C3 | 1 | Planning and reporting remain metadata-only | Household-load counters |
| C4 | 1 | Current nodes are explicit without weakening exact hash/receipt comparisons | Planner invariants |
| C5 | 1 | Named inputs use the existing manifest `inputs` dimension | Manifest validation tests |
| C6 | 2 | Existing stage wrapper signatures and returns remain unchanged | Formal and return snapshots |
| C7 | 2 | C3 results never become a durable cache or resume token | Result/projection assertions |
| C8 | 3 | Worker success is pending until inventory and manifest checkpoint completion | Fault-injection assertions |
| C9 | 3 | Unknown infrastructure failures fail closed | Condition-class matrix |
| C10 | 3 | `force_surveys` reuses C1 resolution and remains mutually exclusive with global force | Resolver/API tests |
| C11 | 3 | Failed prerequisites do not delete prior immutable artifacts | Version-history assertions |
| C12 | 4 | Cached clean nodes do not load household artifacts | Load-counter tests |
| C13 | all | No new package dependency, external workflow framework, or unsafe parallelism | `DESCRIPTION` and diff audit |
| C14 | all | Pre-C2 provenance still requires explicit canary/baseline bootstrap | Bootstrap tests |
| C15 | final | Production remains blocked pending Windows/SMB fencing evidence | Documentation assertion |
| C16 | 3 | The authoritative execution plan is built after lease acquisition and all-cached return rechecks lease/parent | Ordered trace and race tests |
| C17 | 3 | Final stage references resolve in `final_evidence_manifest` while stage wave manifests/checkpoints remain truthful | Cross-stage retention and stage-result-v2 tests |
| C18 | 3 | Targeted force is additive to normal invalidation | Mixed forced/stale fixture |
| C19 | all | The protected charter remains unchanged; completed A1/A2/C2/C3 are treated as baseline | Final diff audit |

### Boundaries

- Allowed: shared planner/fact preparation, named C2 input rows, clean/metadata
  core extraction, stage-wave execution, typed aggregate result, exported thin
  wrapper, compact logging/report changes, tests, generated package
  documentation, NEWS, and tactical context documentation.
- Allowed: backward-compatible internal refactoring of `pd_process_data()` and
  `pd_deflate_pipeline()` around shared cores.
- Out of scope: acquisition/validation execution, independently cached internal
  substeps, second DAG/manifest/provenance storage, run manifests/cursors,
  parallel execution, external frameworks, estimation, poverty aggregation,
  and changes to other packages.
- Out of scope: claiming production activation or substituting local tests for
  target Windows/SMB evidence.
- Out of scope: whole-survey retirement/tombstoning after upstream removal; C4
  fails closed until that separate policy is approved.
- Out of scope: modifying the protected `compound-gpid.md`; only tactical
  context may be updated.

### Iteration Policy

1. Establish one shared fact path, but acquire the writer lease before building any authoritative execution plan.
2. Add complete-node and named-input evidence before consuming it in orchestration.
3. Extract clean and metadata cores behind unchanged public adapters.
4. Implement and validate compact run contracts before exporting the wrapper.
5. Execute topological waves only against post-lease freshly fenced plans and committed upstream receipts.
6. Mark success only after `pd_finalize_checkpoint()` returns the finalized manifest identity.
7. Normalize only allowlisted survey-domain failures; fail closed on unknown shared-state errors.
8. Verify each phase with targeted tests before proceeding to the next.
9. Run the full suite, documentation, check, and scope audit only at the final gate.
10. Pause under `deviation-policy: ask` before changing C2 schema, public compatibility, package dependencies, or approved scope.

### Blocked-Stop Conditions

- C2 exact receipt, lease, fence, parent, or checkpoint semantics cannot be reused safely.
- Complete-node planning requires a second currentness or provenance authority.
- Exact per-entity auxiliary mapping cannot be derived without loading household data.
- Independent expected clean `pip_id`s cannot be derived before worker execution with the production cache-ID semantics.
- A complete upstream survey removal cannot be checked preliminarily before
  lease and authoritatively after lease/master reload while still stopping
  before any worker, artifact, inventory, or manifest write.
- Successful downstream work cannot be bound to committed upstream receipts.
- Final stage artifact references cannot be proven against the final retained manifest.
- Compatibility requires changing an existing exported signature or return type.
- Restart correctness requires a persisted run cursor or exactly-once claim.
- Implementation requires another package change, unsafe parallelism, or production activation.
- Required evidence fails after scoped recovery attempts.
- A deviation is required while approval is unavailable.
- The plan or execution report cannot be recorded durably.

### Deviation Policy

The stored deviation policy is `ask`. Pause before any departure from the
approved steps, boundaries, completion contract, or file surface and record the
decision in the execution report.
