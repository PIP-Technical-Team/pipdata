---
date: 2026-08-27
title: "Executable Staged Invalidation DAG"
status: decided
scope: "Deep"
artifact-schema-version: 1
chosen-approach: "Stage-wave executor over the C2 manifest and C3 typed contracts"
tags: [pipeline, invalidation, orchestration, dependency-graph, caching, provenance, resumability]
---
<!-- Valid status values: decided, in-progress, abandoned -->

# Executable Staged Invalidation DAG

## Context

Stream C stage C4 must turn pipdata's existing read-only dependency planning
and change-report capability into an executable incremental pipeline. The
pipeline must identify current, stale, cached, runnable, failed, and blocked
work for each durable stage entity, execute only affected descendants, and
remain correct across partial failure and interruption.

The worktree was synchronized before analysis. On 2026-08-27, `HEAD` and
`origin/PROD` both resolved to
`d79cefa084bcf45ecd1306fb7d6e962725df9c3b`, with divergence `0/0` and a clean
worktree. The brainstorm branch is `feat/step-level-invalidation-dag`.

The approved design starts after B3 validation has produced a trustworthy
completed-validation inventory. C4 executes the durable `clean`, `metadata`,
and `deflate` stages. Acquisition, validation, estimation, and poverty
aggregation remain outside this feature.

## Prior Decisions And Evidence

The following completed streams were inspected before selecting the C4
architecture.

| Stream | Completion evidence | Production contract C4 must preserve |
| --- | --- | --- |
| C1 targeted forcing | `.cg-docs/brainstorms/2026-08-15-force-surveys-surgical-reprocessing.md`; `.cg-docs/plans/2026-08-17-force-surveys-surgical-reprocessing.md`; `.cg-docs/work-reports/2026-08-17-force-surveys-surgical-reprocessing.md` | `force_surveys` is additive, lookup-first, mutually exclusive with global `force`, and bounded to selected surveys |
| C2 staged dependency manifest | `.cg-docs/brainstorms/2026-08-24-pipdata-staged-dependency-manifest.md`; `.cg-docs/plans/2026-08-24-pipdata-staged-dependency-manifest.md`; `.cg-docs/executions/2026-08-24-pipdata-staged-dependency-manifest.md` | One pipdata-owned manifest, exact Stamp receipts, stage/entity granularity, result-bound checkpoints, explicit bootstrap |
| C3 typed stage interface | `.cg-docs/brainstorms/2026-08-25-pipeline-stage-interface.md`; `.cg-docs/plans/2026-08-25-pipeline-stage-interface.md`; `.cg-docs/work-reports/2026-08-26-pipeline-stage-interface.md` | Graph-neutral `pipeline_context` and `pipdata_stage_result`; no household data or log rows in portable results |
| A1 batch deflation | `.cg-docs/brainstorms/2026-08-17-deflate-pipeline-wrapper.md`; `.cg-docs/plans/2026-08-17-deflate-pipeline-wrapper.md`; `.cg-docs/work-reports/2026-08-17-deflate-pipeline-wrapper.md` | `pd_deflate_pipeline()` remains independently callable and returns the master inventory; deflated output uses `pip_deflated` |
| A2 data-level semantics | `.cg-docs/brainstorms/2026-08-20-data-level-sentinel-semantics.md`; `.cg-docs/plans/2026-08-20-explicit-data-level-sentinel-semantics.md`; `.cg-docs/work-reports/2026-08-20-explicit-data-level-sentinel-semantics.md` | `*_data_level` values remain scalar attributes and each domain resolves independently |
| B2 unified logging | `.cg-docs/brainstorms/2026-04-28-unified-logging.md`; `.cg-docs/plans/2026-04-28-unified-logging.md`; `.cg-docs/work-reports/2026-08-21-unified-logging.md` | `pipdata_log` is the only persistent pipdata log; discriminators are strings; logging stays at compact orchestration boundaries |
| B3 DLW wrappers | `.cg-docs/brainstorms/2026-08-24-dlw-wrapper-rewrite.md`; `.cg-docs/plans/2026-08-25-dlw-wrapper-rewrite.md`; `.cg-docs/work-reports/2026-08-26-dlw-wrapper-rewrite.md` | Completed-validation state is durable input; wrapper results remain plain validated contracts and are not silently reclassed |

The most relevant prior solutions are:

- `.cg-docs/solutions/data-quality/2026-08-25-authoritative-staged-provenance-checkpoints.md`
- `.cg-docs/solutions/bugs/2026-08-26-fail-closed-typed-stage-orchestration.md`
- `.cg-docs/solutions/data-quality/2026-08-26-durable-stage-reconciliation.md`
- `.cg-docs/solutions/data-quality/2026-08-17-pip-id-reverse-map-dedup-uniqueness.md`
- `.cg-docs/solutions/bugs/2026-08-21-explicit-data-level-sentinel-registry.md`

## Current-State Architecture

### Authoritative Dependency Layer

| Concern | Current implementation |
| --- | --- |
| Manifest schema, stage/action/reason vocabulary | `R/dependency_contract.R:1-134` |
| Release/identity/root scope | `R/dependency_contract.R:137-176` |
| Curated code fingerprints | `R/code_fingerprint.R:1-100` |
| Per-entity input projections | `R/dependency_inputs.R` |
| Frozen metadata snapshot and current facts | `R/dependency_execution.R:47-207` |
| Snapshot, parent, code, and lease fencing | `R/dependency_execution.R:209-275` |
| Planner, targeted force, and bootstrap | `R/dependency_plan.R:1-123` |
| Checkpoint batching | `R/dependency_execution.R:277-295` |
| Exact Stamp receipt and revalidation | `R/save_pip.R:91-148` |
| Stage-aware inventory reconciliation | `R/reconcile_pip_inventory.R:1-71` |
| Release, master, and manifest finalization | `R/dependency_manifest.R:225-350` |
| Read-only public report | `R/pd_change_report.R:20-37` |

The C2 payload is a last-success index. Its `records` table is unique by
`(stage, entity_id)`, its `inputs` table is unique by
`(stage, entity_id, name)`, and its `fingerprints` table is unique by
`(stage, component)`. The manifest does not replace Stamp catalogs or artifact
history.

Current durable granularity is already suitable for C4:

| Stage | Entity | Output |
| --- | --- | --- |
| `clean` | `survey_id` | An atomic set of one or more `pip` receipts |
| `metadata` | `pip_id` | One `pip_meta` receipt |
| `deflate` | `pip_id` | One `pip_deflated` receipt |

### Current Stage Execution

`pd_process_data()` prepares and executes C2 clean and metadata work in
`R/pd_process_data.R:73-200`. Same-run metadata binds to verified clean output
in `R/pd_process_data.R:135-169`. Supporting failure and worker helpers remain
at `R/pd_process_data.R:332-408` and in `R/pd_metadata_refresh.R`.

`pd_deflate_pipeline()` has an internal typed entry point and shared core in
`R/pd_deflate_pipeline.R:90-317`. It keeps successful workers pending until
the C2 checkpoint succeeds, then creates manifest-bound artifact references.
Exact deflation loads pinned `pip` and `pip_meta` versions and checks hashes in
`R/pd_deflation.R:333-363`.

### Current Typed Boundaries

`new_pipeline_context()` and its validator are in
`R/pipeline_context.R:23-107`. The context carries the unchanged C2 dependency
context, accepted plan identity, selection, options, logging identity, and a
runtime environment that may hold the live execution object.

`new_pipdata_stage_result()` and its validator are in
`R/pipeline_stage_result.R:259-365`. A result carries compact unit outcomes,
artifact references proven against a finalized manifest generation, stable
condition records, counts, hashes, and provenance. It must not contain
household data, inventories, raw conditions, or copied log rows.

### Gaps C4 Must Close

- `pd_change_report()` does not currently use the executor's full
  snapshot/fact preparation path, so advisory reporting and executable planning
  can disagree.
- C2 generally returns actionable rows only. It does not yet expose every
  selected current node as an explicit `action = "none"` row, which prevents a
  complete cached/runnable/blocked report.
- The active `pd_process_data()` returns at `R/pd_process_data.R:200`. Its
  historical `process_summary_inf` and pipeline checkpoint block at lines
  202-329 is unreachable and must not be treated as the C4 execution boundary.
- Current manifest publication writes only a canonical input row. The schema
  already permits named component inputs, but C4 must publish them to explain
  exact DLW, PFW, CPI, PPP, population, and other auxiliary invalidations.
- Clean and metadata do not yet expose a reusable typed core equivalent to the
  deflation pilot.
- Production activation remains blocked by signed target Windows/SMB fencing
  and immutable unique-rename evidence. The default user-cache manifest path is
  development-only.

## Requirements

- Add one exported thin orchestration wrapper for production scripts.
- Begin from B3's completed-validation inventory.
- Execute only `clean`, `metadata`, and `deflate` durable stages.
- Return a compact typed aggregate result, never household data or a master
  inventory.
- Preserve the public signatures and return values of existing stage wrappers.
- Use the C2 manifest, planner vocabulary, exact receipts, lease, fencing,
  reconciliation, and bootstrap policy.
- Use C3 contexts and stage results at execution boundaries.
- Report every selected node, including cached and blocked work.
- Make `force_surveys` force the selected survey's complete clean -> metadata
  -> deflate chain while leaving unselected entities cached.
- Do not persist a separate run manifest or resumable run cursor.
- Do not add an external workflow framework.
- Do not add parallel execution in C4.
- Preserve the C2 policy for pre-provenance artifacts: explicit canary and
  baseline rebuild, with no inferred historical adoption.

## Approaches Considered

### Approach 1: Stage-Wave Executor

Add an exported `pd_run_pipeline()` wrapper over one internal topological
executor. The executor uses the existing C2 manifest and planner, prepares one
shared run, freezes an exact context for each accepted stage wave, and returns
a typed aggregate of C3 stage results.

Pros:

- Keeps one currentness and provenance authority.
- Rebinds downstream work to committed upstream receipts.
- Gives one coherent run identity, failure boundary, and summary.
- Supports complete cached/runnable/blocked reporting.
- Preserves existing public stage APIs through adapters over shared cores.

Cons:

- Requires extracting reusable clean and metadata cores.
- Requires complete-node planning rather than actionable-only planning.
- Requires careful handling of stage-specific plan hashes within one run.

Effort: large.

Recommended: yes. This is the smallest architecture that proves targeted
invalidation, atomic provenance, and safe restart end to end.

### Approach 2: Existing-Wrapper Sequencer

Add a thin wrapper that invokes `pd_process_data()` and
`pd_deflate_pipeline()` as independent operations.

Pros:

- Smaller initial refactor.
- Reuses public wrappers directly.

Cons:

- Repeats planning, leases, and inventory loads.
- Cannot provide one authoritative accepted plan or coherent partial-run state.
- Makes prerequisite blocking and exact downstream rebinding harder to prove.
- Risks reporting a stage as current under a different snapshot than execution.

Effort: medium.

Recommended: no.

### Approach 3: Generic Internal-Step DAG

Create a generic graph engine and independently cache load, PFW merge, recode,
auxiliary attachment, save, and deflation substeps.

Pros:

- Provides maximum future scheduling flexibility.
- Could eventually resume inside clean-stage computation.

Cons:

- Introduces new durable artifact boundaries that do not exist today.
- Requires a larger manifest migration and more crash-consistency protocols.
- Risks creating a competing DAG or provenance authority beside C2.
- Does not add value needed by the current acceptance scenarios.

Effort: large.

Recommended: no.

## Decision

Use Approach 1: a stage-wave executor over the existing C2 manifest and C3
typed contracts.

The feature retains the roadmap ID `step-level-invalidation-dag`, but C4's
durable nodes are the three existing stages. Internal functions are dependency
and code-fingerprint components, not independent cached artifacts. This
interpretation delivers executable step-level invalidation at the stable
artifact boundary without inventing intermediate storage.

## Executable DAG

### Node Identity

The canonical node key is `(stage, entity_id)`.

| Node | Canonical ID | Cardinality | Commit unit |
| --- | --- | --- | --- |
| Clean | `clean:<survey_id>` | One per selected survey | Complete verified set of expected `pip_id` receipts |
| Metadata | `metadata:<pip_id>` | One per selected PIP artifact | One verified `pip_meta` receipt |
| Deflate | `deflate:<pip_id>` | One per selected PIP artifact | One verified `pip_deflated` receipt |

### Edges

```text
clean:<survey_id>
    -> metadata:<pip_id-1>
    -> metadata:<pip_id-2>

metadata:<pip_id-1> -> deflate:<pip_id-1>
metadata:<pip_id-2> -> deflate:<pip_id-2>
```

The clean fan-out is determined from the verified clean receipt set and
`expected_pip_ids`. It must not be inferred from stale inventory rows. Removed
prior clean outputs remain C2 tombstones and are not scheduled as current
descendants.

### Input Components

The existing manifest `inputs` table should publish named components while
retaining the canonical composite `input_hash` in each stage record.

| Stage | Named components |
| --- | --- |
| Clean | Exact DLW version/hash, keyed PFW projection, recode specification, clean-stage code hash |
| Metadata | Exact clean output version/hash, keyed CPI/PPP/population/GDP/PCE projections, metadata code hash |
| Deflate | Exact clean and metadata versions/hashes, keyed CPI/PPP/population projections, deflate code hash |

Named rows support reason attribution. The canonical hash remains the
deterministic equality check and must be derived from a sorted, versioned
projection of those components.

## Stage-State Transition Model

The design separates planning facts, scheduling disposition, and terminal
execution outcomes rather than overloading one status field.

| Dimension | Value | Meaning |
| --- | --- | --- |
| Freshness | `current` | Inputs, code, and output receipt match the last committed record |
| Freshness | `stale` | At least one exact comparison differs or provenance is unknown |
| Modifier | `forced` | Caller intentionally overrides currentness for a selected survey chain |
| Disposition | `cached` | Current node is reported and not executed |
| Disposition | `runnable` | Stale or forced node has satisfied prerequisites |
| Disposition | `blocked` | A prerequisite failed or shared state is uncertain |
| Runtime | `running` | Worker started; no durable success is implied |
| Terminal | `succeeded` | Exact receipt and checkpoint are committed |
| Terminal | `failed` | Worker or checkpoint failed |

Allowed transitions are:

```text
current -> cached
current + forced -> runnable
stale -> runnable
runnable -> running
runnable -> blocked
running -> succeeded
running -> failed
failed prerequisite -> descendant blocked
succeeded -> current on the next authoritative replan
```

`forced` is a modifier and reason, not a terminal state. A node cannot be both
`cached` and `forced` in the same accepted plan.

C3 final-unit mappings remain:

| C4 outcome | C3 unit status | C3 reason |
| --- | --- | --- |
| `succeeded` | `success` | Existing C2 reason codes |
| `failed` | `failed` | Stable condition record |
| `cached` | `cached` | `current` |
| `blocked` | `skipped` | `upstream_failed` |

`runnable` and `running` are planner/runtime states and do not need to become
new durable `pipdata_stage_result` statuses.

## Planner-To-Executor Contract

### Shared Preparation

`pd_change_report()` and execution must call the same read-only snapshot and
fact preparation path. The path may inspect catalogs, manifests, inventories,
fingerprints, and compact auxiliary projections. It must not load household
artifacts.

The plan must retain C2's exact shape and controlled actions/reasons while
including all selected nodes. Current nodes use `action = "none"`; actionable
nodes continue to use `create`, `refresh`, or `rebuild`. A derived state view
maps the controlled action/reason facts to `cached`, `runnable`, or `blocked`
without becoming another planner.

### Authoritative Execution

1. Validate arguments and completed-validation input before any mutation.
2. Build the C2 dependency context and acquire one writer lease for the scope.
3. Capture the authoritative initial snapshot and complete selected-node plan.
4. Create one run ID and a compact root run descriptor.
5. Freeze a clean-wave `pipeline_context` with the accepted clean plan hash.
6. Execute runnable clean nodes without loading cached surveys.
7. Commit successful clean receipts through `pd_finalize_checkpoint()`.
8. Refresh authoritative facts under the same lease and parent fence.
9. Freeze and execute the metadata wave against actual committed clean receipts.
10. Refresh authoritative facts again after metadata checkpoints.
11. Freeze and execute deflate against actual committed clean and metadata receipts.
12. Construct stage results only from committed outcomes and manifest-bound references.
13. Return the typed aggregate with initial/final manifest identities and per-wave plan hashes.

An advisory plan may be used for comparison or display, but it must never
replace the freshly prepared executable plan. A mismatch must fail before a
worker runs.

### Pipeline Context Mapping

Each stage wave gets an immutable C3 context descriptor with:

- The same top-level `run_id`, release, identity, scope, options, and logging name.
- The exact accepted stage-wave `plan_hash` and `manifest_before` identity.
- A runtime environment referencing the shared live C2 execution state only
  while execution is active.
- No household data, inventory, log rows, or raw conditions.

Earlier stage contexts must not be mutated after their results are constructed.
The top-level aggregate records each wave's plan hash rather than pretending
that a dynamic multi-wave run has one immutable stage plan.

### Stage Result Mapping

Each wave returns a validated `pipdata_stage_result`. Every selected node must
appear exactly once as success, failed, cached, or skipped/blocked. A worker
receipt is not enough for success; success requires a finalized manifest
generation containing the exact receipt.

The top-level aggregate should be a new compact S3 result with exact fields for
schema version, run ID, status, terminal flag, stage results, derived counts,
stable conditions, plan hashes, manifest before/after identities, log
reference, and timestamps. It must contain no master inventory or household
data.

## Minimal API Proposal

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

Contract:

- `inv = NULL` loads the B3 completed-validation inventory through the existing
  authoritative path.
- `force` and nonempty `force_surveys` are mutually exclusive.
- Global `force = TRUE` preserves its current versioning semantics.
- `force_surveys` accepts existing C1 survey and pip identifiers, resolves them
  through the existing lookup-first resolver, and forces each selected clean
  node plus its resulting descendants.
- `bootstrap_entities` requires `bootstrap = TRUE`.
- The function returns a typed aggregate result visibly or invisibly as decided
  during planning; it never returns household data or the master inventory.
- Existing `pd_process_data()` and `pd_deflate_pipeline()` signatures, defaults,
  positional order, and return types remain unchanged.
- Existing wrappers should delegate to shared internal cores rather than call
  the exported top-level wrapper recursively.

## Exact Invalidation Matrix

| Trigger | Entity mapping | Direct invalidation | Descendants | Unaffected work |
| --- | --- | --- | --- | --- |
| DLW source version/hash changes | Exact `survey_id` from completed-validation state | `clean:<survey_id>` | All verified output pip IDs: metadata then deflate | Other surveys and pip IDs |
| Clean implementation changes | Stage fingerprint scope | Matching clean nodes, normally all selected surveys for a global component | Their metadata and deflate nodes | Metadata/deflate entities outside changed clean fan-out |
| Recode specification changes | Recode applicability; global if the packaged spec fingerprint is global | Matching clean nodes | Their metadata and deflate nodes | Unmatched surveys |
| PFW input rows change | Existing PFW join keys projected per survey | Clean nodes whose PFW component hash changed | Their metadata and deflate nodes | Surveys with unchanged PFW projection |
| CPI rows change | Country/year/data-level keys projected per pip ID | Matching metadata and deflate nodes | Deflate waits for matching metadata success | Every clean node and unmatched pip IDs |
| PPP rows change | Country/year/data-level keys projected per pip ID | Matching metadata and deflate nodes | Deflate waits for matching metadata success | Every clean node and unmatched pip IDs |
| Population rows change | Country/year/data-level keys projected per pip ID | Matching metadata and deflate nodes | Deflate waits for matching metadata success | Every clean node and unmatched pip IDs |
| GDP/PCE rows change | Existing metadata keys projected per pip ID | Matching metadata nodes | Deflate only where committed metadata output changes | Every clean node and unmatched pip IDs |
| Metadata implementation changes | Metadata stage fingerprint scope | Matching metadata nodes | Matching deflate nodes | Clean nodes |
| Deflation implementation changes | Deflate stage fingerprint scope | Matching deflate nodes | None | Clean and metadata nodes |
| Clean output missing or drifted | Exact receipt set for `survey_id` | Owning clean node | Its metadata and deflate nodes | Unrelated entities |
| Metadata output missing or drifted | Exact `pip_meta` receipt | Owning metadata node | Matching deflate node | Clean and unrelated pip IDs |
| Deflate output missing or drifted | Exact `pip_deflated` receipt | Owning deflate node | None | Clean, metadata, and unrelated pip IDs |
| `force_surveys` selection | Existing C1 lookup-first survey/pip resolver | Selected clean nodes even when current | Full verified metadata/deflate fan-out | Every unselected node |
| Unknown pre-C2 provenance | Explicit bootstrap selection only | Selected clean, metadata, and deflate baseline work | Normal graph order | No implicit adoption |

### Colombia 2018 CPI Scenario

A CPI change that alters only the keyed CPI projection used by Colombia 2018
must produce this plan:

| Node set | Expected disposition |
| --- | --- |
| Every clean node, including Colombia 2018 | `cached` |
| Colombia 2018 metadata pip IDs | `runnable` |
| Colombia 2018 deflate pip IDs | `runnable` after metadata success |
| Other Colombia years | `cached` |
| Other countries and years | `cached` |

The executor must not call the clean worker, load Colombia household data for
cleaning, or write unrelated artifacts in this scenario.

## Persistence And Atomicity

Stamp remains authoritative for immutable artifact versions. The C2 manifest
remains the only pipdata-owned currentness/provenance index. C4 does not add a
second DAG file, step-hash artifact, run manifest, or inventory current flag.

The existing manifest schema can represent C4 because the durable node
granularity remains unchanged. C4 extends use of the existing `inputs$name`
dimension to publish named component inputs. A schema version change is needed
only if implementation discovers that the current validator or checksum
contract cannot safely accept those rows; it must not be introduced merely to
rename the same information.

The commit sequence remains result-bound:

1. Assert lease ownership, context, code fingerprints, snapshot, and manifest parent.
2. Verify every successful stage receipt against the exact Stamp catalog row.
3. Reconcile the candidate master inventory in memory.
4. Write and verify the release inventory.
5. Write and verify the master inventory.
6. Revalidate receipts and execution fence.
7. Publish a unique immutable manifest generation.
8. Mark typed units successful only after the finalized generation is known.

The sequence is not a cross-artifact transaction. Inventory writes can precede
a failed manifest publication. Correctness comes from retaining the prior
manifest as authoritative and replanning against exact current facts. C4 must
not weaken that conservative recovery model.

## Failure, Retry, And Resume Semantics

### Survey-Domain Failure

A known recoverable survey-domain condition becomes a failed unit with a
stable C3 condition record. Independent sibling units may continue according
to the existing entity error policy. Descendants of the failed node become
blocked and are not loaded or written.

### Integrity Failure

Lease loss, context drift, exact-hash mismatch, receipt ambiguity, inventory
reconciliation failure, manifest-parent mismatch, or checkpoint failure is
terminal for later writes. Unknown infrastructure errors fail closed rather
than being normalized as recoverable survey failures.

### Prior Artifacts

Failure must not delete or overwrite prior immutable Stamp artifacts. The
last-success manifest record remains available for audit and rollback but does
not make the node current under changed inputs. Any current-view invalidation
must be narrowly reconciled and must not fabricate a successful output.

### Interrupted Runs

- Work committed in a finalized manifest generation remains current.
- Pending receipts not included in a finalized generation are not successful.
- An unreferenced content-identical Stamp write may be resolved to one exact
  receipt on retry, but file existence alone is never a cache hit.
- The next invocation replans from Stamp facts and the last valid manifest.
- No persisted run cursor is required for correctness.
- A rerun after complete success must produce a no-op plan with all selected
  nodes cached and zero stage artifact writes.

## Backward Compatibility And Baseline Strategy

### Existing Public APIs

- Preserve `pd_process_data()` argument order, including `verbose` as the fourth
  positional argument and `force_surveys` after it.
- Preserve `pd_process_data()` and `pd_deflate_pipeline()` master-inventory
  return values.
- Preserve `pd_deflate_pipeline()` as an independently callable stage.
- Preserve B3 acquisition, validation, and aggregate plain-result contracts.
- Preserve the `pip_deflated` alias and all current deflation output fields and
  attributes.
- Preserve `pipdata_log` as the sole persistent log.

### Existing C2 Manifests

No C4 migration is required for valid existing C2 stage records because the
chosen durable nodes and entity IDs are unchanged. New named component input
rows are populated only by successful C4 checkpoints. Until a selected node
has those rows, the canonical record remains authoritative and reason detail
may be less granular.

### Pre-Provenance Artifacts

Preserve C2's explicit bootstrap policy. Do not infer historical input or code
provenance from current catalogs. An absent manifest makes applicable nodes
`unknown_provenance`; execution requires `bootstrap = TRUE`. Use a restrictive
`bootstrap_entities` canary before the one-time complete baseline rebuild.
Corrupt manifest generations fail closed and are never treated as absence.

## Logging And Change Reporting

`pd_change_report()` must use the same snapshot/fact construction as the
executor and return the complete selected node universe without loading
household artifacts. Its printed summary should include counts by stage and
disposition plus reason counts. Its invisible return remains the structured
dependency plan or a backward-compatible extension of it.

The top-level executor should emit compact run and stage summaries to
`pipdata_log` with one shared `run_id`. Per-unit failures should use stable
condition IDs. `logmeta$info` and `logmeta$error` remain string discriminators;
caught messages belong in `condition_msg`.

Do not emit typed log calls from frames containing household data or full
inventories because pipfun captures caller formals. Detailed per-unit timing
belongs in compact result rows or orchestration-level log metadata.

The existing unreachable pipeline summary/checkpoint block after
`R/pd_process_data.R:200` must be removed or replaced during implementation;
it must not remain as misleading dead architecture.

## Performance Risks And Instrumentation

### Required Properties

- Planning loads catalogs, manifests, compact inventories, fingerprints, and
  keyed auxiliary projections only.
- Cached clean nodes never load household artifacts.
- Keyed auxiliary comparisons operate on per-entity projections rather than
  global auxiliary hashes.
- Plan and result tables use keyed `data.table` joins and deterministic sorting.
- No result, context, or log row retains household data by reference.

### Metrics

| Metric | Purpose |
| --- | --- |
| Snapshot and fact-construction duration | Detect planner regressions |
| Selected/current/stale/forced counts by stage | Explain workload |
| Cached/runnable/blocked counts by stage | Verify scheduling |
| Reason counts by input component | Explain invalidation |
| Worker duration by entity | Locate expensive stages |
| Checkpoint duration and batch size | Tune checkpoint policy |
| Stamp reads and writes by alias | Prove cache effectiveness |
| Household artifact loads by stage | Prove cached stages are not loaded |
| Manifest generations and parent identities | Audit publication sequence |
| Failure and retry counts with condition IDs | Diagnose resumability |

Potential risks include repeated full-catalog scans, quadratic plan joins,
repeated `rbind` growth, retaining data tables through logging frames, and
checkpoint batches that are too large for practical recovery. Tests should
instrument call counts and use performance budgets based on metadata size, not
household row counts.

## Test Matrix

### Planner And State Tests

| Scenario | Required assertion |
| --- | --- |
| Fully current release | Every selected node is `action = "none"` and reported cached |
| New survey | Clean create plus metadata/deflate descendants only |
| Missing clean output | Owning clean node and descendants are actionable |
| Missing metadata output | Owning metadata and matching deflate are actionable |
| Missing deflate output | Only owning deflate is actionable |
| Clean code fingerprint change | Clean and descendants invalidate; unrelated entity keys remain stable |
| Metadata code fingerprint change | Metadata and deflate invalidate; clean stays cached |
| Deflate code fingerprint change | Only deflate invalidates |
| Unknown pre-C2 provenance | Default blocks; explicit bootstrap selection proceeds |
| Advisory/executable mismatch | Abort before any worker call |
| `pd_change_report()` parity | Report and executor preparation produce identical initial node facts |

### Input Mapping Tests

| Scenario | Required assertion |
| --- | --- |
| One DLW survey changes | Only that clean node and verified descendants invalidate |
| One PFW key changes | Only consuming survey clean nodes and descendants invalidate |
| Colombia 2018 CPI changes | Only Colombia 2018 metadata/deflate nodes run; all clean nodes remain cached |
| Colombia 2019 CPI changes | Colombia 2018 remains cached |
| One-country PPP change | Only matching metadata/deflate nodes invalidate |
| One-country population change | Only matching metadata/deflate nodes invalidate |
| GDP/PCE metadata-only change | Matching metadata runs; deflate runs only if committed metadata output changes |
| Mixed area/national data levels | Each CPI/PPP/pop projection follows its own attribute semantics |

### Execution Tests

| Scenario | Required assertion |
| --- | --- |
| Full three-stage success | Exact receipts, inventories, manifest, and typed results agree |
| Selected force | Only selected clean nodes and descendants execute |
| Unchanged rerun | All nodes cached; no worker or artifact write occurs |
| Clean failure | Its metadata and deflate descendants are blocked; siblings follow policy |
| Metadata failure | Matching deflate is blocked; clean success remains committed |
| Deflate failure | Earlier stages remain committed and current |
| Multi-output clean | Expected receipt set commits atomically; removed outputs become tombstones |
| No household load for cached clean | Mock/trace exact load calls and assert zero cached loads |
| Existing wrapper compatibility | Signatures, positional order, and return types remain unchanged |
| Aggregate result portability | No environments, inventories, data, conditions, or log rows survive projection |

### Crash And Resume Tests

| Injection point | Required restart behavior |
| --- | --- |
| Before worker | No mutation; all prior state remains authoritative |
| After artifact write, before receipt verification | No committed success; exact retry is safe |
| After pending receipt, before checkpoint | Pending unit is not successful |
| After release inventory write | Prior manifest drives safe replan |
| After master inventory write | Prior manifest drives safe replan |
| Before manifest rename | No new generation is current |
| After manifest rename, before result construction | New generation is discoverable and rerun is cached |
| Lease loss during a wave | Later writes stop; unattempted nodes become blocked/skipped |
| Corrupt generation with older valid generation | Reader selects valid history according to C2 rules |
| All generations corrupt | Fail closed; do not bootstrap implicitly |

### Recorded Regression Suites

C4 planning should retain and extend these suites:

- `tests/testthat/test-dependency-contract.R`
- `tests/testthat/test-dependency-plan.R`
- `tests/testthat/test-dependency-execution.R`
- `tests/testthat/test-dependency-manifest.R`
- `tests/testthat/test-dependency-inputs.R`
- `tests/testthat/test-dependency-bootstrap.R`
- `tests/testthat/test-dependency-performance.R`
- `tests/testthat/test-pd-change-report.R`
- `tests/testthat/test-pd_process_data.R`
- `tests/testthat/test-pd-metadata-refresh.R`
- `tests/testthat/test-pd-deflate-pipeline.R`
- `tests/testthat/test-pipeline-context.R`
- `tests/testthat/test-pipeline-stage-result.R`
- `tests/testthat/test-logging-integration.R`
- `tests/testthat/test-log_report.R`

Production activation also requires the separate target Windows/SMB fencing
and immutable unique-rename smoke test. Local temporary-directory tests do not
substitute for that evidence.

## Acceptance Criteria

1. One exported thin wrapper incrementally executes clean, metadata, and deflate.
2. The wrapper returns a validated compact typed aggregate with no household data or inventories.
3. `pd_change_report()` and execution preparation use the same authoritative facts.
4. Every selected node is represented exactly once as cached, succeeded, failed, or blocked.
5. A DLW or clean/recode change executes only the affected clean node and its verified descendants.
6. A PFW change executes only consuming clean nodes and their descendants.
7. A Colombia 2018 CPI/PPP/population change executes only matching Colombia 2018 metadata/deflate nodes.
8. The Colombia auxiliary scenario performs no clean worker call and no unrelated artifact write.
9. `force_surveys` executes the selected full chain and no unselected node.
10. Failed prerequisites block descendants without deleting prior immutable artifacts or publishing false success.
11. A successful incremental run immediately followed by the same run is a complete no-op with all nodes cached.
12. Cached clean nodes cause zero household artifact loads.
13. Only exact Stamp receipts included in finalized manifest generations become successful typed units.
14. Restart after every checkpoint fault converges to the same final state as an uninterrupted run.
15. Existing stage APIs, signatures, and return values remain compatible.
16. No second DAG, manifest, provenance store, run cursor, or inventory version authority is introduced.
17. Pre-C2 artifacts remain subject to explicit canary and baseline rebuild.
18. Production readiness is not claimed without the outstanding Windows/SMB evidence.

## Concrete Implementation Slices

### Slice 1: One Planning Path

Factor a shared metadata-only snapshot/fact preparation path. Make
`pd_change_report()` and execution use it. Emit complete selected-node actions,
including `none` for current nodes, and add deterministic derived state views.

### Slice 2: Explainable Inputs

Publish named per-entity input components through the existing manifest
`inputs` table. Derive exact reason codes from component comparisons and test
keyed DLW, PFW, and auxiliary mappings.

### Slice 3: Shared Stage Cores

Extract clean and metadata internal cores that accept an authoritative
execution object and accepted stage plan. Preserve `pd_process_data()` as an
adapter returning the master inventory. Keep the current deflation shared-core
pattern.

### Slice 4: Stage-Wave Executor

Add the internal topological executor with one run ID, shared lease, stage-wave
contexts, exact checkpoint transitions, failure blocking, and no-load cache
behavior.

### Slice 5: Typed Run Aggregate And API

Define and validate the compact aggregate result. Add `pd_run_pipeline()` with
the minimal approved signature. Add portable projection and print behavior
without introducing durable run state.

### Slice 6: Logging And Reporting

Add compact run summaries and per-stage correlations to `pipdata_log`. Update
`pd_change_report()` and `log_report()` without copying log rows into results.
Remove or replace the unreachable legacy pipeline summary/checkpoint block.

### Slice 7: End-To-End Correctness

Implement the exact invalidation, Colombia 2018, force, no-op, failure,
checkpoint-fault, baseline, and no-household-load test matrix. Verify existing
public API contracts.

### Slice 8: Production Boundary

Document the activation procedure and retain the production block until the
target Windows/SMB fencing and immutable unique-rename evidence is signed off.
Do not mix unsafe parallel execution into this slice.

## Explicitly Deferred Follow-Up Work

- Independently cached internal substeps such as load, merge, recode, and attach.
- Acquisition and validation nodes in the C4 DAG.
- A persisted observational run manifest.
- A resumable exact-run cursor or exactly-once execution claim.
- Parallel worker execution or graph-aware parallel scheduling.
- Cross-scope or cross-release scheduling.
- External workflow frameworks.
- Estimation, regional/global aggregation, and poverty calculation stages.
- JSON serialization of typed contexts or results.
- Generalized data-level sentinels beyond the currently supported `area` pointer.
- Migration or adoption of unverifiable pre-C2 artifacts.

Potential future concurrency is limited to independent clean survey nodes and
independent metadata/deflate pip nodes after their prerequisites commit. Any
future parallel design must keep one serialized commit coordinator under the
scope lease, avoid shared by-reference inventory mutation, and prove Stamp and
logging safety before activation.

## Devil's Advocate Review

Problem validation is pre-established by explicit DLW, PFW, Colombia 2018,
force, no-op, failure, and restart scenarios and by C2/C3's deliberate C4
deferral.

The simplest viable solution is not a generic internal-step engine. The three
existing durable stages deliver the requested invalidation behavior while
keeping internal functions as fingerprint components. This captures most of
the value with substantially less persistence and crash-consistency risk.

The approach is proportionate only if C4 defers generic DAG infrastructure,
run cursors, and parallel execution. The primary value is complete-node
planning, keyed auxiliary invalidation, exact stage-wave rebinding, and safe
checkpointed execution.

The decision aligns with `compound-gpid.md`: it improves the initial pipdata
ingestion and cleaning pipeline, follows R package standards, and leaves
estimation outside pipdata. Production activation remains conditional on the
existing C2 storage evidence.

## Next Steps

1. Run `/cg-plan` against this brainstorm and inherit the `Deep` scope.
2. Require plan review before any C4 implementation begins.
3. Implement the approved slices through `/cg-work` only after review approval.
4. Run `/cg-review` against the completed C4 implementation and acceptance matrix.
5. Run `/cg-compound` only after review findings are resolved.
