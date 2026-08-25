---
date: 2026-08-24
title: "Implement Pipdata Staged Dependency Manifest"
status: completed
completed-date: 2026-08-25
scope: "Deep"
brainstorm: ".cg-docs/brainstorms/2026-08-24-pipdata-staged-dependency-manifest.md"
language: "R"
estimated-effort: "large"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [pipeline, dependency-manifest, provenance, invalidation, caching, stamp, data-table]
phases: 4
completed-phases: [1, 2, 3, 4]
execution-report: ".cg-docs/executions/2026-08-24-pipdata-staged-dependency-manifest.md"
---

# Plan: Implement Pipdata Staged Dependency Manifest

## Objective

Implement a self-contained dependency-planning layer in pipdata that detects
stale clean, metadata, and deflated artifacts without loading household data,
explains every planned action, and persists exact successful provenance in a
release-scoped manifest. Execution must update only affected artifacts, preserve
stamp version history, recover safely from partial failures, and never mark an
unverified artifact current.

## Context

The approved brainstorm replaces the original narrow code-hash idea with a
staged provenance design. Current invalidation handles DLW and some auxiliary
changes, but it has four important gaps:

- `valid_dlw_load()` treats all auxiliary changes as cleaning triggers even
  though CPI, PPP, population, GDP, and PCE alter `pip_meta`, not cleaned
  household data.
- `pd_deflate_pipeline()` uses only the `deflated` flag and can resolve latest
  inputs rather than an exact planned `pip`/`pip_meta` pair.
- `save_pip_data()` discards stamp receipts, so an old catalog row can be
  mistaken for the result of a failed current write.
- There is no durable code/input provenance or read-only explanation of why an
  entity is stale.

Stamp remains the authoritative version store. The pipdata manifest is a
rebuildable provenance index and decision input; it must not replace or modify
stamp internals. A future orchestrator may consume the planner, but orchestrator
selection and estimation dependencies are outside this plan.

Relevant prior patterns:

- `.cg-docs/plans/2026-05-20-inventory-architecture-refactor.md`: use persisted
  catalog facts and delta updates rather than holding version state in memory.
- `.cg-docs/plans/2026-08-17-force-surveys-surgical-reprocessing.md`: preserve
  `force_surveys`, content versioning, and single-master-load behavior.
- `.cg-docs/solutions/performance-issues/2026-07-22-per-survey-logging-retains-large-survey-objects.md`:
  do not retain survey/inventory objects through typed logging.
- `.cg-docs/plans/2026-06-24-stamp-atomic-catalog-write.md`: never pre-delete a
  canonical file on Windows/SMB; verify temporary writes before publication.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Model `clean`, `metadata`, and `deflate` as explicit stages with stable entity keys, actions, and reason codes | Brainstorm stage contract |
| R2 | Derive a deterministic scope identity from release, identity, actual configured repository/alias roots, optional namespace, and schema version so TEST/INT/PROD or different releases cannot share state accidentally | Architecture review; plan review |
| R3 | Persist the manifest as verified immutable RDS generations with one writer lease, checksum/schema validation, fallback to older valid generations, and no overwrite/delete commit step | Brainstorm; Windows/SMB history |
| R4 | Store last-success records per stage/entity; successful partial work may advance while failed work retains prior/unknown provenance and stays stale | Brainstorm partial-success requirement |
| R5 | Compute deterministic stage fingerprints from value-affecting function formals/bodies, constants, S3 methods, recode-spec content, and relevant external implementations | Brainstorm; plan critique |
| R6 | Build plans from inventory/catalog/aux/manifest metadata only; planning and reporting must never load `pip` household artifacts | User efficiency objective |
| R7 | Export a strictly read-only `pd_change_report()` backed by the same internal planner consumed by execution | Brainstorm visibility decision |
| R8 | DLW, PFW, recode-spec, or clean-code changes trigger clean and downstream actions only for affected entities; clean-code changes initially affect all clean entities | Brainstorm invalidation rules |
| R9 | CPI/PPP/pop changes trigger metadata plus deflation; GDP/PCE changes trigger metadata only; selectors and semantic hashes must be shared with metadata construction | Brainstorm; plan critique |
| R10 | Deflation currentness uses exact clean version plus semantic CPI/PPP/pop metadata inputs and deflation code, not the whole `pip_meta` content hash | Plan critique |
| R11 | Pipeline deflation loads exact planned `version_id_data` and `version_id_metadata` and fails closed; no latest fallback is allowed on the pipeline path | Existing provenance drift gap |
| R12 | Every artifact success must be backed by a verified stamp receipt/version/hash; catalog existence alone is insufficient | Architecture review P0 |
| R13 | Reconcile clean output sets and metadata/deflation artifact updates with stage-aware inventory semantics before publishing corresponding manifest records | Plan critique |
| R14 | Preserve `force`, `force_surveys`, module filtering, positional compatibility, content versioning, and bounded per-survey memory behavior | Existing public behavior |
| R15 | Missing legacy provenance requires explicit, resumable bootstrap under content versioning; missing/corrupt state must never silently start a full rebuild | Brainstorm bootstrap decision; plan critique |
| R16 | Structured logs contain compact IDs, counts, stages, and reason codes only; no survey or whole-inventory objects may be retained | Memory solution |
| R17 | Stamp remains authoritative and no changes are made inside stamp, pipload, pipfun, or their persisted schemas | User boundary |
| R18 | Each phase has targeted tests; final evidence includes full tests, generated documentation, package check, and a 2,500-entity metadata-only planning benchmark/call-count audit | Deep-plan verification |
| R19 | Preserve exported function signatures, positional argument order, documented return shapes, and side effects through internal adapters; append any new public arguments | Plan review |
| R20 | Freeze exact auxiliary artifact versions before planning/execution and hash canonical per-entity projections with deterministic ordering, types, NA handling, and duplicate-key rejection | Plan review |
| R21 | Treat base-metadata extraction and expected clean output IDs as clean-coupled semantics; aux-only metadata refresh is independently executable, while missing/corrupt base metadata escalates to clean | Plan review |
| R22 | Fence every artifact/inventory/manifest write with lease ownership and publish a checkpoint only after verified release-inventory and master-inventory finalization | Plan review |

## Implementation Steps

## Phase 1: Contracts and Deterministic Fingerprints

### 1. Define stage, public-API, plan, manifest, and scope contracts

- **Requirements**: R1, R2, R4, R14, R17, R19, R21, R22
- **Files**: `R/aaa.R`, `R/dependency_contract.R` (new),
  `R/pipdata-options.R`, `R/zzz.R`, `DESCRIPTION`,
  `tests/testthat/test-dependency-contract.R` (new),
  `tests/testthat/test-dependency-api-contract.R` (new)
- **Details**:
  - Declare the three stages and entity units:
    - `clean`: one `survey_id`, with a complete expected set of one-or-more
      resulting `pip_id`s; no usable PFW/output mapping is a typed failure, not
      a successful zero-output clean;
    - `metadata`: one `pip_id`;
    - `deflate`: one `pip_id`.
  - Treat compact base-metadata extraction from a newly cleaned object as part
    of clean semantics. The independently refreshable metadata stage owns only
    auxiliary selection/encoding and cannot certify base-metadata changes.
  - Define controlled action values (`create`, `refresh`, `rebuild`, `none`) and
    controlled reason codes, including `new_entity`, `dlw_changed`,
    `pfw_changed`, `recode_spec_changed`, `<stage>_code_changed`,
    `aux_<measure>_changed`, `upstream_output_changed`, `output_missing`,
    `output_drift`, `forced`, and `unknown_provenance`.
  - Define `pip_dependency_plan` as a list with `context`, `actions`, and
    `reasons` data.tables. `actions` has one row per stage/entity; `reasons` is
    normalized to one row per action/reason/input with old/new provenance.
  - Define manifest schema version 1 as a list with `header`, `records`,
    `inputs`, `fingerprints`, and `tombstones` tables. Tombstones remove prior
    outputs from current master/release views while retaining stamp history.
    Do not persist a mutable `current` flag; currentness is derived by comparing
    exact inputs and code hashes.
  - Define one pure context resolver using actual exported working-release and
    alias state (isolating any caller-environment mutation) plus
    `stamp::st_alias_list()` or another verified public alias-root API. Always
    include release, identity, schema, and actual `pip`, `pip_meta`,
    `pip_deflated`, `pip_master`, and `pip_inv` roots. Treat
    `pipdata.dependency_scope` as an additional repository namespace, never as
    a replacement for release/identity/root facts.
  - Canonicalize paths using existing-path resolution where available,
    normalized separators/trailing delimiters, Windows drive/UNC case folding,
    and stable encoding before hashing the context to `scope_id`. Validate the
    full persisted header context on every read. If public state cannot establish
    an unambiguous context, abort instead of falling back globally.
  - Add defaults for `pipdata.dependency_manifest_path`,
    `pipdata.dependency_scope`, `pipdata.manifest_checkpoint_n` (25), and
    `pipdata.manifest_checkpoint_seconds` (60). `R_user_dir("pipdata",
    "cache")` is the development default; production documentation requires a
    durable shared override scoped to the release repository.
  - Record the checkpoint state machine: verify stage outputs; reconcile the
    candidate master in memory; write and verify the release inventory; apply
    its exact version ID to the candidate master; write and verify the master;
    revalidate receipts and lease fencing; then publish manifest records. If
    any boundary fails, do not publish the batch and stop additional writes.
  - Add a Phase-1 public-API compatibility spike for `stamp::st_versions()`,
    `stamp::st_hash_obj()`, `stamp::st_alias_list()`, exact artifact paths,
    `pipload::load_aux_data(version=)`, and exact `pip_read(version=)` behavior.
    Verify their formals and real return shapes, then pin `DESCRIPTION` to the
    earliest tested stamp/pipload versions (at least the stamp version that
    exports `st_catalog_query()`). If a required fact is unavailable publicly,
    stop before pipeline implementation rather than use `:::` or infer latest.
  - Preserve exported API contracts by placing typed receipts, attempts,
    reconciliation, and exact deflation interfaces behind internal helpers.
    New optional public arguments must be appended, and API snapshot tests must
    pin existing formals, positional calls, failure shapes, and side effects.
- **Test Scenarios**:
  - happy path: two releases and TEST/PROD derive different scope IDs;
  - edge case: custom manifest root remains separated by scope subdirectory;
  - error path: missing/ambiguous release or identity aborts with a typed class;
  - explicit `main_dir` differing from the global option, path case/slash/UNC
    variants, and optional namespace produce correct stable isolation;
  - declared minimum stamp/pipload versions expose all required public APIs and
    expected receipt/version shapes;
  - schema validation rejects duplicate stage/entity keys or unknown stages and
    reason codes.
- **Tests**: `devtools::test(filter = "dependency-(contract|api-contract)")`
- **Acceptance criteria**: schemas and invariants are executable validators;
  scope collisions are impossible for supported contexts; dependency floors
  are explicit; exported API compatibility is pinned; no pipeline behavior
  changes yet.

### 2. Implement and audit value-affecting stage fingerprints

- **Requirements**: R5, R8, R9, R10, R18, R21
- **Files**: `R/aaa.R`, `R/code_fingerprint.R` (new), `DESCRIPTION`,
  `tests/testthat/test-code-fingerprint.R` (new)
- **Details**:
  - Define explicit curated components in `aaa.R`; do not hash all exports or
    orchestration/writer/reporting functions.
  - Initial `clean` closure includes DLW loading/class restoration, PFW
    selection/splitting, `pd_cpfw_merge()` and methods/helpers,
    `pd_dlw_clean()` and S3 methods/helpers, `apply_recode_spec()` plus every
    active recode handler, active `wbpip_clean` methods, relevant constants
    such as `.DOMAIN_COLS`, package recode YAML content, and explicitly invoked
    `wbpip` cleaning/quantile implementations. It also owns base-metadata
    extraction/schema from cleaned-object attributes, because that output is
    not independently reconstructible when `pip_meta` is missing or stale.
  - Initial `metadata` closure is limited to independently refreshable aux-only
    selectors and encoding helpers. Refactor `pd_aux_attr()` ownership so a
    change to base-attribute extraction affects `clean`, while a change to
    CPI/PPP/pop/GDP/PCE selection/encoding affects `metadata`.
  - Initial `deflate` closure includes `pd_deflation()`, active S3 methods and
    cores, input validation/finalization, CPI/PPP/pop/welfare helpers, and
    explicitly invoked `wbpip` deflation implementations.
  - Canonicalize each function before hashing: recursively remove `srcref`,
    `srcfile`, `wholeSrcref`, source-location, and byte-code/source attributes;
    preserve meaningful formal argument order and language structure; serialize
    with one fixed R serialization version; then hash the raw serialization
    with `xxhash64`. Fingerprint constants/data and external functions
    separately, then build a deterministic composite from sorted component
    names/hashes.
  - Record package/external versions and pipdata Git/package version as audit
    fields, but do not invalidate on a blanket pipdata version bump.
  - Recompute once at each top-level plan/execution entry. `.onLoad()` may cache
    diagnostics but is not authoritative, ensuring `load_all()` and in-session
    modifications are detected.
  - Use `codetools::findGlobals()` in tests (add to `Suggests` if needed) to
    audit untracked functions/constants against an explicit reviewed exclusion
    list. The runtime closure remains curated, not dynamically inferred.
- **Test Scenarios**:
  - identical source and component order produce identical hashes;
  - clean-only, metadata-only, and deflate-only mutations affect only the
    expected stage;
  - changed formals/defaults, constants, S3 methods, recode YAML, and allowlisted
    `wbpip` implementations alter the owning stage;
  - comment/whitespace-only changes do not alter parsed body fingerprints;
  - installed and `load_all()` representations of unchanged source agree,
    including fixtures with source references and different file locations;
  - base-metadata extraction mutation affects `clean`, while aux-only encoding
    mutation affects `metadata`.
- **Tests**: `devtools::test(filter = "code-fingerprint")`
- **Acceptance criteria**: every active value-affecting closure member has a
  test-proven owner; orchestration-only changes do not invalidate artifacts.

## Phase 2: Durable Manifest and Read-Only Planning

### 3. Implement immutable-generation manifest persistence and recovery

- **Requirements**: R2, R3, R4, R15, R17, R18, R22
- **Files**: `R/dependency_manifest.R` (new), `R/pipdata-options.R`, `R/zzz.R`,
  `tests/testthat/test-dependency-manifest.R` (new)
- **Details**:
  - Store each scope under
    `<manifest-root>/dependency-manifest/<scope_id>/` as immutable files named
    `manifest-v1-<20-digit-generation>-<uuid>.rds`. Parse generations
    numerically and allocate `max()` across every parseable final filename,
    including invalid/corrupt generations, plus one. Do not reuse a generation
    after fallback or use a mutable current pointer.
  - Wrap the payload with a SHA-256 checksum. Write to a same-directory temp
    path, close it, read it back, validate schema/invariants/checksum, then
    rename to a unique nonexistent final path. Never overwrite or pre-delete a
    generation during publication.
  - Readers sort numeric generations newest-first and select the first valid matching
    scope/schema. Ignore temp files. Distinguish absent state (bootstrap
    required) from corrupt state (fall back to a prior valid generation; hard
    stop when none remain).
  - Retain at least three valid generations. Cleanup is best effort after
    publication and must never affect the just-published or last two valid
    predecessors.
  - Record each generation's complete parent identity: parent filename, UUID,
    payload checksum, and generation. Under the lease, re-read the latest valid
    generation and compare this full identity before merge.
  - Use an atomically created lease directory for one writer per scope. Record
    host, PID, run ID, heartbeat, and a random fencing token. Verify token
    ownership immediately before every stamp artifact write, release/master
    write, and manifest publication, not only at checkpoints.
  - Do not auto-break a stale-looking lease. An explicit override must
    atomically quarantine the prior lease and establish a new fencing token;
    the old writer stops at its next ownership check. If quarantine/fencing
    cannot be demonstrated on the target filesystem, support offline manual
    recovery only and do not expose programmatic override.
  - Publish verified successful records after every 25 work units or 60
    seconds, plus a mandatory final checkpoint. On handled failure/interrupt,
    best-effort checkpoint only fully verified and reconciled units. A failed
    checkpoint stops further artifact writes.
- **Test Scenarios**:
  - valid round-trip and monotonic generation merge;
  - truncated temp/final, checksum mismatch, wrong scope, unsupported schema,
    and newest-generation corruption fall back safely;
  - corrupt generation 10 -> fallback to 9 -> publication allocates 11 ->
    reader selects 11; adversarial equal-generation files abort;
  - publication failure leaves prior generation readable;
  - lock contention, live-writer override, lease loss mid-unit, stale-lock, and
    two-contender cases never permit an unfenced write;
  - simulated parent-generation race aborts rather than dropping records;
  - test paths use `withr::local_tempdir()` only.
- **Tests**: `devtools::test(filter = "dependency-manifest")`
- **Acceptance criteria**: no failure mode can create a false-current record,
  destroy the last valid generation, reuse a corrupt generation, or permit an
  unfenced writer; no new hard persistence dependency is introduced.

### 4. Implement metadata fact resolution and the dependency planner

- **Requirements**: R1, R5, R6, R8, R9, R10, R14, R15, R19, R20, R21
- **Files**: `R/dependency_inputs.R` (new), `R/dependency_plan.R` (new),
  `R/pd_aux_attr.R`, `R/recode_spec.R`, `R/valid_dlw_load.R`,
  `R/pd_process_data.R`, `R/pd_deflate_pipeline.R`,
  `tests/testthat/test-dependency-inputs.R` (new),
  `tests/testthat/test-dependency-plan.R` (new)
- **Details**:
  - Extract shared pure selectors for PFW, CPI, PPP, pop, GDP, and PCE. Define a
    canonical projection as the complete named vector supplied to the entity's
    metadata/deflation step, not the unknowable subset touched by household
    rows. Sort declared keys, columns, and names; preserve explicit types and NA
    representation; reject duplicate semantic keys; and give missing required
    CPI/PPP/pop projections typed failed/stale reasons.
  - Preserve CPI country/year/acronym, PPP country/reporting-level, and
    pop/GDP/PCE country/year/reporting-level semantics. Include all applicable
    national/area reporting-level values so planning remains metadata-only.
  - Query each aux artifact once for exact path, `version_id`, and
    `content_hash`; load with `load_aux_data(measure, version=version_id)`;
    verify `stamp::st_hash_obj()` against the captured hash; and freeze those
    objects/projections in the planning snapshot. A changed latest version
    between query and load must not change execution inputs.
  - Build per-entity semantic input maps from the supplied/current DLW
    inventory, master inventory, stamp catalogs, auxiliary data, package recode
    YAML/catalog version, and current code fingerprints. Dependency facts may
    read catalogs and aux metadata but must never call `pip_read(alias="pip")`.
  - Inspect recode package content and existing stamp catalog read-only; never
    call `sync_recode_spec()` during planning.
  - Reconcile manifest output references against stamp catalogs. Missing
    versions, hash mismatches, or unexpected output movement become
    `output_drift`; the manifest never overrides stamp facts.
  - Add one pure `expected_pip_ids(inv_row, pfw_projection)` projector shared
    by planner and worker. No matching/valid PFW output is a typed clean failure,
    not a zero-output success. Welfare/module additions and removals produce an
    explicit expected output set and manifest tombstones; tombstoned outputs are
    removed from current master/release views but remain historical in stamp.
  - Generate clean actions by `survey_id`; propagate intended clean actions to
    downstream selections using only expected IDs. Actual same-run downstream
    attempts are bound later to verified clean results, never placeholder
    versions/hashes.
    Generate metadata and deflate actions independently for aux/code/output
    changes.
  - Compute a semantic deflation-input hash from exact cleaned-data hash,
    selected CPI/PPP/pop values, and deflation-code hash. Record full metadata
    version/hash for audit, but do not use GDP/PCE-only differences to trigger
    deflation.
  - Integrate `force_surveys` as additive `forced` reasons and retain module
    filtering/deduplication. Keep `valid_dlw_load()` as a compatibility wrapper
    selecting clean-stage actions; do not remove its public contract in this
    phase.
  - Return deterministically ordered `pip_dependency_plan` actions/reasons and
    an immutable planning snapshot of exact versions/hashes used for execution.
  - Install the bootstrap safety contract now, before Phase 3: append
    `bootstrap = FALSE` to all affected top-level executors and reject any
    selected `unknown_provenance` action unless bootstrap is explicitly true.
    The guard runs before force versioning, writer lease acquisition, recode
    synchronization, or any artifact/inventory write. Step 9 adds bootstrap
    scheduling/resume, not this fundamental guard.
  - Metadata `output_missing`, corruption, base-schema incompleteness, or a
    base-metadata fingerprint change escalates the entity to clean. Only a
    valid compact `pip_meta` with current base schema can use aux-only refresh.
- **Test Scenarios**:
  - complete invalidation matrix: none, new DLW, DLW hash, PFW, recode content,
    three code stages, CPI/PPP/pop, GDP/PCE, upstream output, output drift,
    missing/unknown manifest, force, and overlapping reasons;
  - aux row change in another country/year/acronym/reporting level does not
    affect the entity;
  - row/column reordering is hash-stable; duplicate semantic keys, missing
    required rows, NA values, empty selections, and national/area projections
    have explicit deterministic behavior;
  - latest aux changes between catalog query and load; planner either uses the
    pinned version or aborts on hash mismatch;
  - base-metadata code mutation and missing/corrupt/incomplete `pip_meta`
    escalate to clean; aux-only refresh remains metadata-only;
  - no-PFW, invalid welfare type, module change, welfare output addition/removal,
    and tombstone projection use the same expected-ID helper;
  - one survey producing multiple `pip_id`s yields one clean action and the
    correct downstream actions without duplicates;
  - absent manifest plus `bootstrap = FALSE` aborts before every write-capable
    call, even though the planner can report all unknown actions;
  - planner performs no household reads or writes and does not mutate supplied
    data.tables by reference.
- **Tests**: `devtools::test(filter = "dependency-(inputs|plan)")`
- **Acceptance criteria**: plan output is deterministic, reason-complete, and
  uses pinned aux versions and the same selectors/output-ID projector as
  execution; GDP/PCE-only changes never plan deflation; no later phase can
  execute unknown legacy actions without explicit bootstrap.

### 5. Export the read-only change report and compact logging contract

- **Requirements**: R6, R7, R16, R18
- **Files**: `R/pd_change_report.R` (new), `R/aaa.R`, `NAMESPACE`/`man/`
  through roxygen, `tests/testthat/test-pd-change-report.R` (new)
- **Details**:
  - Export `pd_change_report()` with injectable inventory/aux/manifest inputs
    for tests and normal defaults that load metadata facts only.
  - Render stage totals, entity IDs, reasons, and old/new fingerprint/version
    summaries. Return the underlying `pip_dependency_plan` invisibly so an
    eventual orchestrator can consume the stable object without parsing text.
  - Keep rendering separate from planning. Add `print.pip_dependency_plan()`
    only if it delegates to a side-effect-free formatter.
  - A report must not create manifest directories, initialize state, sync the
    recode spec, repair files, update timestamps, call `pip_write`, or load
    household artifacts.
  - Include the scope/context resolver in the read-only boundary: call any
    exported pipfun getter inside an isolated local environment, use public
    alias-list state only, and assert the caller/global environment is unchanged.
  - Define execution logmeta fields from the same stage/reason constants. Log
    only compact IDs/counts/reasons at orchestration boundaries; never call
    typed logging wrappers from workers whose formals contain survey objects.
- **Test Scenarios**:
  - no-change, mixed-reason, all-stage-code-change, and bootstrap-required
    reports;
  - stable ordering and formatting;
  - write spies for `pip_write`, stamp save, manifest publication, filesystem
    mtimes, and recode sync remain untouched;
  - `pip_read(alias="pip")` call count remains zero.
- **Tests**: `devtools::test(filter = "pd-change-report")`
- **Acceptance criteria**: report and execution consume the same plan schema;
  reporting is demonstrably read-only and metadata-only.

## Phase 3: Verified Clean and Metadata Execution

### 6. Normalize verified stamp receipts and stage-aware inventory reconciliation

- **Requirements**: R4, R12, R13, R14, R17, R19, R22
- **Files**: `R/save_pip.R`, `R/build_pip_inventory.R`,
  `R/reconcile_pip_inventory.R` (new), `R/pd_process_data.R`, `R/aaa.R`,
  `tests/testthat/test-save_pip.R`,
  `tests/testthat/test-reconcile-pip-inventory.R` (new),
  `tests/testthat/test-build_pip_inventory.R`
- **Details**:
  - Add an internal typed save/receipt helper returning `alias`, exact artifact
    path/ID, `version_id`, `content_hash`, `skipped`, `success`, and failure
    details. Keep exported `save_pip_data()`'s signature, success shape, and
    `NULL`-on-failure behavior by projecting internal results through a
    compatibility wrapper.
  - For a new write, require the exact artifact path, nonempty returned
    `version_id`, returned `metadata$content_hash`, and a matching exact row
    from `stamp::st_versions(artifact, alias)`.
  - For a content-skipped write, hash the attempted object using the verified
    public `stamp::st_hash_obj()` contract; query `st_versions()` for that exact
    artifact; and accept only one exact current version whose content hash
    equals the attempted object. Never use alias-wide latest or an unrelated
    historical row. Revalidate the exact row under the fencing lease immediately
    before checkpoint publication.
  - Separate failed-attempt logging from last-success provenance. Internal typed
    failure results cannot enter inventory/manifest updates; public wrappers
    retain their documented failure return.
  - Replace survey-wide generic upsert behavior with stage-aware reconciliation:
    - clean replaces a survey's old output set only after every expected `pip`
      output verifies; removed prior outputs become explicit tombstones;
    - metadata updates only the matching `pip_id` metadata columns and never
      deletes sibling rows;
    - deflation updates only matching deflation columns.
  - Pass the exact `sync_recode_spec()` result/version into reconciliation;
    remove the independent “latest recode catalog row” lookup for current work.
  - Implement one fenced checkpoint/finalizer state machine:
    1. verify artifact receipts and lease token;
    2. reconcile a candidate master in memory;
    3. write and verify the release-inventory receipt;
    4. apply that exact release version ID to the candidate master;
    5. write and verify the master-inventory receipt;
    6. revalidate lease token and every batch receipt against exact stamp rows;
    7. publish the manifest generation.
    No stage record advances before both derived inventories verify.
  - A failure at release, master, lease, or manifest publication leaves the old
    manifest authoritative for planning and causes conservative retry. This
    removes the prior unreachable “finalize on a zero-action resume” assumption.
    Test crash/restart at every boundary, including a release failure followed
    by a normal run that still schedules the uncommitted work.
- **Test Scenarios**:
  - real-stamp and mocked receipt success, content-skip, missing version,
    missing hash, thrown error, ambiguous exact rows, object-hash mismatch, and
    historical artifact collision;
  - multi-output clean with one failed save does not replace the prior clean set;
  - metadata-only update preserves sibling rows and clean versions;
  - release/master persistence, lease loss, and manifest publication failures
    prevent manifest advancement and remain scheduled on restart;
  - exact recode version passed through despite a newer concurrent catalog row.
- **Tests**: `devtools::test(filter = "(save-pip|reconcile-pip|build-pip-inventory)")`
- **Acceptance criteria**: only exact verified current-attempt outputs can be
  represented as successful; exported API behavior remains compatible; a batch
  is current only after release, master, fencing, and manifest publication all
  verify; no stage update corrupts unrelated inventory rows.

### 7. Split metadata refresh from cleaning and execute clean/metadata plans

- **Requirements**: R4, R7, R8, R9, R12, R13, R14, R16, R19, R20, R21, R22
- **Files**: `R/pd_process_data.R`, `R/valid_dlw_load.R`, `R/pd_aux_attr.R`,
  `R/pd_metadata_refresh.R` (new), `R/build_pip_inventory.R`,
  `tests/testthat/test-pd_process_data.R`,
  `tests/testthat/test-valid_dlw_load.R`,
  `tests/testthat/test-pd-metadata-refresh.R` (new)
- **Details**:
  - Compute/revalidate one authoritative plan at `pd_process_data()` entry and
    pass immutable action rows to workers. A prior report is advisory; execution
    always revalidates metadata facts at run start.
  - Split the current combined worker into internal clean and metadata units.
    Retain exported `process_data()` unconditionally as a compatibility wrapper
    with its existing formals, positional behavior, return shape, and side
    effects.
  - Clean only planned `survey_id`s. Save and verify the complete expected
    `pip_id` set before marking clean success. A clean failure suppresses new
    downstream metadata/deflation actions for that attempt and leaves prior
    provenance stale.
  - Preserve the pre-run plan as immutable selection evidence, then create an
    internal stage-attempt/result object after each verified clean. This result
    binds actual output ID, `version_id`, content hash, base metadata, frozen aux
    snapshot, code hash, and fencing token. Same-run metadata attempts use these
    facts, never intended placeholders or prior clean versions.
  - For newly cleaned output, build base metadata from the in-memory clean
    object before freeing it. For aux-only refresh, require a valid exact compact
    `pip_meta` with the current base schema, preserve non-aux base attributes,
    and replace only declared aux fields. Missing/corrupt/incomplete base
    metadata was already escalated to clean by Step 4 and must never trigger a
    household read inside the metadata worker.
  - Use the shared selectors from Step 4 for metadata construction and semantic
    input hashes. PFW remains a clean input; CPI/PPP/pop/GDP/PCE remain metadata
    inputs.
  - Use exact frozen aux versions/projections from the planning snapshot; do not
    reload latest aux data inside workers. Checkpoint verified/reconciled success
    batches through the fenced state machine from Step 6.
    Failed actions retain prior/unknown last-success records; they are still
    planned on the next run.
  - Preserve `force = TRUE` timestamp behavior and `force_surveys` additive
    content-version behavior. Keep all new optional arguments appended. The
    Step-4 bootstrap guard remains in force; bootstrap uses neither global force
    nor timestamp versioning.
  - Emit compact plan/execution summaries only after survey objects have been
    released; retain explicit `rm()`/conditional `gc()` behavior.
- **Test Scenarios**:
  - Colombia-only DLW/PFW changes clean only Colombia and plan downstream work;
  - CPI-only change performs metadata refresh with zero `pip` household reads;
  - GDP/PCE-only refresh does not create a deflate action;
  - clean-code change plans all clean entities; metadata-code change refreshes
    metadata only;
  - partial survey/pip_id failures, interrupts between checkpoints, and resume;
  - stamp skip, unexpected version, or one failed output in a multi-output clean
    cannot create a downstream metadata attempt with placeholder provenance;
  - missing/corrupt/base-schema-incomplete metadata escalates to clean, while a
    valid aux-only refresh performs zero `pip` reads;
  - all existing force/module/positional/no-change tests remain green;
  - logging wrappers never capture `inv`, `df`, `ls_clean`, or metadata objects.
- **Tests**: `devtools::test(filter = "(pd-process-data|valid-dlw-load|metadata-refresh)")`
- **Acceptance criteria**: clean and metadata actions are independently
  executable and recoverable; metadata-only runs avoid household I/O; existing
  public controls remain compatible.

## Phase 4: Exact Deflation, Bootstrap, and Final Verification

### 8. Pin exact deflation inputs and execute semantic deflation actions

- **Requirements**: R4, R10, R11, R12, R13, R14, R16, R19, R20, R22
- **Files**: `R/pd_deflation.R`, `R/pd_deflate_pipeline.R`,
  `R/reconcile_pip_inventory.R`, `R/build_pip_inventory.R`, `R/aaa.R`,
  `tests/testthat/test-pd-deflation.R`,
  `tests/testthat/test-pd-deflate-pipeline.R`
- **Details**:
  - Add an internal exact deflation helper/plan-row interface with distinct
    `data_version_id` and `metadata_version_id`. Preserve exported
    `pd_deflation()`'s existing formals, positional order, and documented
    interactive behavior; do not condition compatibility on discovering callers.
    Exact pipeline mode is internal, explicit, and fail closed.
  - Remove latest fallback from exact mode. If either pinned artifact cannot be
    loaded or its content hash does not match the planning snapshot, fail the
    action and replan; never mix a planned data version with unrelated latest
    metadata.
  - Pass complete plan/attempt rows to `deflate_one()` rather than only `pip_id`.
    Deflation receives exact data/meta IDs and semantic CPI/PPP/pop input hash.
  - Select candidates from planner actions, not only `deflated`. A deflate-code
    change plans all deflate entities; cleaned-output or semantic metadata
    changes plan only matching `pip_id`s.
  - Verify lease fencing and each exact `pip_deflated` receipt before setting
    current output fields.
    Add `version_id_deflated` alongside `content_hash_deflated`; missing
    provenance is a failed action, not a warning followed by `deflated=TRUE`.
  - On failure under new inputs, clear current deflation status/pointers in the
    reconciled master while retaining historical versions in stamp and the
    previous last-success record in manifest for audit. The input mismatch keeps
    the action stale.
  - Persist exact clean/meta versions and semantic input components in the
    manifest record. Full metadata version/hash is audit provenance; only the
    consumed CPI/PPP/pop projection participates in currentness.
- **Test Scenarios**:
  - exact data/meta happy path and mismatched/missing versions;
  - stale metadata version cannot fall back to latest;
  - clean output, CPI/PPP/pop, deflate code, GDP/PCE-only, already-deflated, and
    force paths;
  - verified and failed saves, missing catalog receipt, partial batch failure,
    and resume;
  - caller-supplied inventory remains supported without an extra reload;
  - API snapshot and positional tests prove `pd_deflation()` and
    `pd_deflate_pipeline()` remain backward compatible with appended options.
- **Tests**: `devtools::test(filter = "(pd-deflation|pd-deflate-pipeline)")`
- **Acceptance criteria**: every current deflated record is reproducible from
  exact recorded inputs; GDP/PCE-only changes never trigger deflation; latest
  fallback is unreachable from pipeline execution.

### 9. Implement explicit resumable legacy bootstrap and completeness audit

- **Requirements**: R2, R4, R15, R17, R18, R19, R22
- **Files**: `R/dependency_bootstrap.R` (new), `R/pd_process_data.R`,
  `R/pd_deflate_pipeline.R`, `R/pd_change_report.R`,
  `tests/testthat/test-dependency-bootstrap.R` (new)
- **Details**:
  - Treat absent manifest state as `unknown_provenance` for every applicable
    current artifact. Reporting remains read-only and presents counts, stages,
    catalog sizes, and the required rebuild scope.
  - Reuse the `bootstrap = FALSE` execution guard installed in Step 4; Step 9
    must not defer or weaken it. Append a dedicated restrictive
    `bootstrap_entities` selector for explicit bootstrap calls.
  - Keep corruption distinct from first bootstrap. Readers may fall back to an
    older valid generation; if no valid generation remains for an initialized
    scope, stop for operator recovery rather than silently rebuilding.
  - Bootstrap uses normal content versioning and planner-generated actions,
    never `force = TRUE`. `bootstrap_entities` intersects the unknown action
    table across clean, metadata, and deflate stages; it never unions candidates.
    Accept exact `survey_id`/`pip_id` identifiers with explicit resolution and
    leave every unselected entity unknown. Do not overload additive
    `force_surveys` for canaries.
  - Produce a completeness audit joining manifest records to exact stamp
    catalog versions. Completion requires every applicable clean, metadata, and
    deflate entity to have matching successful provenance and no unknown or
    output-drift actions.
  - Document an operator runbook: dry report -> canary subset -> inspect logs
    and storage -> resume batches -> final completeness audit. Executing the
    real production baseline is not part of `/cg-work` for this plan.
- **Test Scenarios**:
  - absent state, corrupted initialized state, fallback generation, explicit
    bootstrap required, subset resume, partial failure, and final completeness;
  - bootstrap never requests timestamp versioning;
  - with every entity unknown, a one-survey `bootstrap_entities` canary executes
    only that survey and its selected downstream `pip_id`s; `force_surveys`
    remains additive and unchanged;
  - interrupted bootstrap replays at most the configured checkpoint interval;
  - another release/scope remains untouched.
- **Tests**: `devtools::test(filter = "dependency-bootstrap")`
- **Acceptance criteria**: legacy provenance can be established safely and
  resumably, but no normal/report call can accidentally launch the rebuild.

### 10. Complete cross-stage tests, documentation, and performance verification

- **Requirements**: R6, R7, R14, R16, R18, R19, R22
- **Files**: all affected test files, `R/pipdata-options.R`, roxygen blocks,
  generated `man/` and `NAMESPACE`, `README.md`, `NEWS.md`, relevant vignettes
- **Details**:
  - Add end-to-end mocked integration scenarios for Colombia-only DLW, PFW,
    CPI, GDP/PCE, clean-code, metadata-code, and deflate-code changes, including
    mixed reasons and partial failures.
  - Add fault injection after artifact save, receipt verification, master write,
    release write, lease loss, manifest temp write, generation publication, and
    checkpoint interrupt.
  - Add a 2,500-entity synthetic planner benchmark/call-count test proving no
    per-entity external I/O and no `pip` household reads. Record elapsed time and
    object size as non-brittle evidence; fail on I/O/count regressions rather
    than a machine-specific wall-clock threshold.
  - Test manifest unique-name rename, fencing, and lock semantics on local
    Windows. Add a signed deployment smoke-test procedure for the configured
    production/SMB manifest path. This plan may complete implementation without
    claiming production activation; production activation remains blocked until
    an operator records successful target-filesystem fencing/rename evidence.
  - Document public report/bootstrap APIs, manifest path/scope options, stage
    semantics, reason codes, corruption recovery, production shared-path
    requirements, and the operator baseline runbook.
  - Update README/NEWS and pipeline vignettes to distinguish clean, metadata,
    and deflate stages. Explain that stamp stores versions while pipdata stores
    dependency provenance.
  - Run roxygen, targeted tests, full tests, and package check.
- **Test Scenarios**: full invalidation and failure matrix; clean R session;
  generated docs; no stale snapshots or undeclared dependencies.
- **Tests**: `devtools::document()`, `devtools::test()`, `devtools::check()`
- **Acceptance criteria**: complete contract evidence passes; package docs state
  operational boundaries and recovery behavior; planner scales without
  household I/O or per-entity catalog calls.

## Testing Strategy

- Use testthat edition 3 and `testthat::local_mocked_bindings()` for pipload,
  stamp, pipfun, clock, and filesystem boundaries.
- Use `withr::local_tempdir()`, `local_tempfile()`, and `local_options()` for all
  manifest and option tests. Never write to the live user cache or production
  aliases from tests.
- Build small data.table fixtures for actions, reasons, records, inputs, and
  stamp receipts. Keep each test self-contained.
- Test pure contracts/fingerprints before persistence, persistence before
  planning, and planning before pipeline mutation.
- Test worker failures at each commit boundary and assert both manifest and
  release/master state, fencing ownership, and manifest state, not only returned
  values.
- Include real-stamp integration tests for new and content-skipped exact artifact
  receipt verification; mocks alone cannot prove the public receipt contract.
- Add API snapshot tests for exported formals, positional calls, success/failure
  return shapes, and documented side effects.
- Add pinned-aux race tests, canonical projection ordering/type/NA/duplicate
  tests, expected-output/tombstone tests, and base-metadata escalation tests.
- Preserve and run existing tests for `valid_dlw_load()`,
  `build_pip_inventory()`, `pd_process_data()`, `pd_deflation()`, and
  `pd_deflate_pipeline()` after each relevant phase.
- Use call-count/write-spy assertions to prove read-only and metadata-only
  behavior. Do not rely on manual inspection for these invariants.
- Final commands:
  - `devtools::document()`
  - `devtools::test()`
  - `devtools::check()`

## Documentation Checklist

- Document `pd_change_report()` return object, side-effect-free behavior, and
  reason/action tables.
- Document explicit bootstrap behavior and the resumable baseline runbook.
- Document restrictive `bootstrap_entities` separately from additive
  `force_surveys`; include a one-survey canary example.
- Document `pipdata.dependency_manifest_path`, `pipdata.dependency_scope`, and
  checkpoint options in `R/pipdata-options.R`.
- Document exact-version pipeline mode in `pd_deflation()` and the distinction
  from any retained interactive fallback.
- Document base-metadata (clean-coupled) versus aux-only metadata semantics,
  canonical aux projections, expected output IDs, and tombstones.
- Document lease fencing/override restrictions, release/master/manifest
  checkpoint order, and the separate signed production-filesystem activation
  smoke test.
- Update `pd_process_data()`, `valid_dlw_load()`, `pd_deflate_pipeline()`, and
  `build_pip_inventory()` stage/provenance details.
- Regenerate `NAMESPACE` and affected `.Rd` files through roxygen.
- Add NEWS and README/vignette coverage for the staged dependency architecture.
- Recommend updating `compound-gpid.md` Current Focus separately; it is a
  protected workflow artifact and not an implementation file in this plan.

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Stamp/pipload receipt lacks exact public version/hash facts | Medium | Critical | Normalize and verify receipts first; blocked-stop and separate upstream work rather than infer from latest |
| Per-user manifest collides across release/identity or is unavailable to another operator | High without scoping | Critical | Context-derived `scope_id`; production shared-path override; local cache documented as development default |
| Manifest publication is atomic but concurrent writers lose updates | Medium | High | One-writer lease, fencing token before every write, full parent identity, immutable generation names, override/lease-loss tests |
| Windows/SMB rename or lingering handle prevents publication | Medium | High | Same-directory unique final rename, no overwrite/delete, closed/read-verified temp, deployment smoke test |
| CPI/PPP selector in planner differs from metadata execution | Medium | High | One shared pure selector per measure used by both hashing and construction; canonical projection and pinned-version race tests |
| Whole metadata hash causes GDP/PCE over-deflation | High without semantic hash | Medium | Deflation semantic fingerprint contains only consumed CPI/PPP/pop plus exact clean/code inputs |
| Multi-output survey partially saves and old catalog rows masquerade as success | Medium | Critical | Verified current-attempt receipts; clean output-set commit is all-or-nothing; explicit tombstones |
| Release/master and manifest diverge after a crash | Medium | High | Verified output -> release receipt -> master receipt -> fencing revalidation -> manifest state machine; conservative replay at every boundary |
| Metadata-only refresh cannot reconstruct base metadata | Medium | High | Base extraction belongs to clean; missing/corrupt/incomplete base metadata escalates to clean; aux-only refresh reuses exact compact `pip_meta` with zero `pip` reads |
| Logging retains survey objects and recreates OOM behavior | Medium | High | Compact boundary logging only; no typed logs from workers with large formals; regression tests |
| Legacy deployment automatically rewrites thousands of artifacts | Medium | Critical | Phase-2 bootstrap guard, restrictive `bootstrap_entities`, dry report, canary/resume runbook, content versioning, no automatic execution |
| Exported helper behavior breaks downstream callers | Medium | High | Internal typed adapters; append arguments; API snapshot/positional/return-shape tests |
| Public API exists only in newer stamp/pipload than DESCRIPTION permits | Medium | High | Phase-1 compatibility spike and tested dependency floors; blocked stop before implementation |
| Plan size produces an unreviewable implementation change | Medium | Medium | Four independently verified phases; stop at each phase evidence gate; run `/cg-plan-review` before implementation |

## Out of Scope

- Selecting or implementing an external orchestrator (`targets` or otherwise).
- Estimation-stage artifacts or dependencies.
- A generic DAG framework or distributed/multi-writer scheduler.
- Changes inside stamp, pipload, pipfun, their catalog schemas, or their locking.
- Executing the real production full baseline rebuild; this plan ships and
  tests the capability and runbook only.
- Retrofitting provenance into arbitrary historical stamp sidecars without
  reprocessing.
- Supporting concurrent writers to the same release scope.
- Claiming production/SMB activation before the signed target-filesystem fencing
  and immutable-rename smoke test is completed.

## Completion Contract

### Outcome

Pipdata can compute a metadata-only, explainable dependency plan across clean,
metadata, and deflate stages; execute only stale work; and persist successful
per-artifact provenance in a release-scoped manifest without modifying stamp.
Partial failures remain stale, exact upstream versions are pinned, and legacy
artifacts can be rebuilt through an explicit resumable baseline process.

### Verification Surface

| ID | Phase | Evidence Required | Command/Artifact | Required |
|----|-------|-------------------|------------------|----------|
| V1 | 1 | Clean, metadata, and deflate fingerprints are deterministic and change only for their declared value-affecting closures | `tests/testthat/test-code-fingerprint.R` | yes |
| V2 | 2 | Context-scoped immutable RDS generations survive malformed/truncated files, lock contention, and failed publication without losing the last valid generation | `tests/testthat/test-dependency-manifest.R` | yes |
| V3 | 2 | `pd_change_report()` performs no stamp/filesystem writes and never loads `pip` household artifacts | write-spy and `pip_read` call-count tests in `test-pd-change-report.R` | yes |
| V4 | 2 | Planner returns deterministic actions/reasons for no-change, DLW, PFW, recode, stage-code, CPI/PPP/pop, GDP/PCE, output-drift, and force cases | `tests/testthat/test-dependency-plan.R` | yes |
| V5 | 3 | Missing or unverifiable stamp receipts cannot be recorded as successful or resolved from an unrelated historical catalog row | `tests/testthat/test-save_pip.R` and integration tests | yes |
| V6 | 3 | Multi-`pip_id` clean outputs, metadata-only refreshes, stage-aware inventory upserts, and partial failures preserve correct sibling rows and last-success provenance | cleaning/metadata integration tests | yes |
| V7 | 3 | Existing `force`, `force_surveys`, module filtering, positional arguments, content versioning, and memory-bounded per-survey behavior remain intact | existing and expanded pipeline regression tests | yes |
| V8 | 4 | Deflation loads exact planned `pip` and `pip_meta` versions and fails closed; the pipeline path never falls back to latest | `tests/testthat/test-pd-deflation.R` and `test-pd-deflate-pipeline.R` | yes |
| V9 | 4 | CPI/PPP/pop or semantic metadata changes re-deflate affected `pip_id`s, while GDP/PCE-only changes refresh metadata without deflation | invalidation matrix tests | yes |
| V10 | 4 | Missing legacy provenance requires explicit bootstrap, supports resumable subsets, uses content versioning, and ends with zero unknown current artifacts | bootstrap integration test and completeness audit | yes |
| V11 | final | Targeted and full test suites pass | `devtools::test()` | yes |
| V12 | final | Generated docs and package checks pass | `devtools::document()` and `devtools::check()` | yes |
| V13 | 1 | Declared minimum stamp/pipload versions expose exact artifact history/hash, alias context, pinned aux load, and exact read contracts; DESCRIPTION floors match tested APIs | `tests/testthat/test-dependency-api-contract.R` and `DESCRIPTION` | yes |
| V14 | 2 | Lease fencing stops an old/live writer after override or lease loss before any subsequent artifact, release, master, or manifest write | fencing fault tests in `test-dependency-manifest.R` | yes |
| V15 | 2 | Corrupt highest generation falls back, next publication allocates above every observed generation, and the new generation is selected deterministically | fallback-publication tests in `test-dependency-manifest.R` | yes |
| V16 | 2 | Exact aux versions are frozen and verified; canonical projections are stable under ordering and reject duplicate/missing required semantic inputs | `test-dependency-inputs.R` including latest-version race | yes |
| V17 | 3 | New and content-skipped writes are verified against exact artifact history with a real stamp alias; ambiguous/historical rows cannot prove success | real-stamp integration test in `test-save_pip.R` | yes |
| V18 | 3 | Release-write or master-write failure publishes no stage record and a subsequent normal run still schedules the uncommitted entity | checkpoint/finalizer restart test | yes |
| V19 | 3 | Exported `save_pip_data()`, `process_data()`, `build_pip_inventory()`, and pipeline wrappers preserve formal order, positional behavior, return shapes, and side effects | API snapshot and compatibility tests | yes |
| V20 | 4 | With all legacy entities unknown, `bootstrap_entities` executes only the requested canary while unselected entities remain unknown; `force_surveys` stays additive | bootstrap restriction test | yes |
| V21 | final | A 2,500-entity plan completes with zero household reads and no per-entity external I/O; elapsed time and object size are recorded | synthetic benchmark/call-count artifact | yes |
| V22 | final | Production activation documentation explicitly requires a signed target Windows/SMB fencing and immutable-rename smoke test and does not claim activation without it | activation checklist/runbook review | yes |

### Constraints

| ID | Phase | Constraint | Check |
|----|-------|------------|-------|
| C1 | all | `stamp` remains authoritative for artifact versions; no stamp internals or schemas are modified | diff review and boundary tests |
| C2 | 2-4 | A manifest record advances only after exact output receipt verification and required inventory reconciliation; failed work retains old/unknown provenance | failure-injection tests |
| C3 | 2 | Planning/reporting loads metadata only, never household microdata | mocked call-count tests |
| C4 | 2-4 | Deflation invalidation uses semantic CPI/PPP/pop inputs, not whole `pip_meta` content, so GDP/PCE do not over-invalidate | matrix tests |
| C5 | 3 | `force = TRUE` and `force_surveys` behavior remains backward compatible; bootstrap never uses timestamp force mode | regression tests |
| C6 | all | Logs contain compact IDs/counts/reasons only and never retain survey or inventory objects | logging argument tests and memory review |
| C7 | 2 | Manifest scope includes release, identity, repository/alias context; TEST/INT/PROD cannot share state accidentally | context-isolation tests |
| C8 | 2-4 | Corrupt/missing state never silently launches a full rebuild or marks artifacts current | recovery/bootstrap tests |
| C9 | 4 | Production deflation never resolves planned inputs using “latest” fallback | exact-version tests |
| C10 | all | No external orchestrator, estimation stage, or generic DAG framework is introduced | scope review |
| C11 | all | Existing exported signatures, positional ordering, return shapes, and documented side effects remain compatible; new options are appended | API snapshot/regression tests |
| C12 | 2-4 | The active lease fencing token is verified immediately before every write; parent-generation checking alone is insufficient | lease-loss/override fault tests |
| C13 | 2-4 | `bootstrap_entities` intersects unknown actions; `force_surveys` remains additive and is never used to bound a bootstrap | bootstrap and force regression tests |
| C14 | 1-3 | Base-metadata changes or missing/corrupt base metadata escalate to clean; aux-only metadata refresh never certifies base metadata | fingerprint/planner/metadata tests |
| C15 | 3-4 | Manifest publication follows verified release and master receipts; no zero-action resume can strand pending release finalization | checkpoint restart tests |
| C16 | 2-4 | Execution uses catalog-pinned, hash-verified aux versions and canonical projections; latest aux races cannot alter recorded inputs | pinned-aux race tests |

### Boundaries

- **Allowed**: new pipdata dependency-contract, fingerprint, manifest, planner,
  report, bootstrap, metadata-refresh, and reconciliation modules; changes to
  current pipeline wrappers, options, inventories, roxygen, tests, and generated
  package documentation.
- **Allowed**: a `codetools` test-only closure audit if required, plus existing
  `digest`, `fs`, `data.table`, stamp, and pipload public APIs.
- **Out of scope**: changes inside stamp, pipload, or pipfun; selection of an
  orchestrator; estimation-stage dependencies; automatic production baseline
  execution; multi-writer distributed scheduling; claiming production/SMB
  activation before the signed target-filesystem smoke test.

### Iteration Policy

1. Freeze schemas, scope identity, commit order, and stage semantics before
   pipeline mutation.
2. Implement deterministic fingerprints and their tests first.
3. Implement immutable-generation manifest persistence, fenced writes, the
   strictly read-only planner/report, and the unknown-provenance execution guard.
4. Normalize verified stamp receipts and stage-aware inventory reconciliation
   before advancing manifest records.
5. Integrate clean and metadata actions, preserving all existing force and
   memory behavior.
6. Integrate exact-version deflation and semantic metadata invalidation.
7. Ship explicit bootstrap/report/resume capability; production baseline
   execution remains a separate operator-approved action.
8. Run targeted tests after each phase, then full tests, documentation, and
   package checks.
9. Under deviation policy `ask`, pause before changing schemas, adding hard
   dependencies, modifying another package, or weakening a required invariant.

### Blocked-Stop Conditions

- Public pipload/stamp receipts cannot expose or verify exact `version_id` and
  `content_hash` without changing another package.
- Declared minimum stamp/pipload releases cannot provide exact artifact history,
  object hashing, alias context, pinned aux loads, or exact version reads through
  public APIs.
- Release, identity, and repository context cannot be derived through public
  APIs or an explicit configuration contract.
- Safe immutable publication, fencing, or lease-loss detection cannot be
  demonstrated on local Windows; production activation separately remains
  blocked until target-filesystem smoke evidence exists.
- Base metadata cannot be classified as clean-coupled or missing/corrupt base
  metadata cannot be escalated safely to clean.
- Exact `pip`/`pip_meta` loading requires fallback to latest.
- A manifest or inventory failure could mark failed work current.
- Release/master receipts cannot be verified before manifest publication, or a
  release failure can become stranded behind a zero-action plan.
- Required phase or final verification remains failing after permitted recovery
  attempts.
