---
date: 2026-08-24
title: "Pipdata Staged Dependency Manifest"
status: decided
scope: "Deep"
artifact-schema-version: 1
chosen-approach: "Staged provenance planner with a pipdata-owned dependency manifest"
tags: [pipeline, invalidation, provenance, dependency-graph, caching, stamp, correctness]
---

# Pipdata Staged Dependency Manifest

## Context

V2 replaces the V1 pipeline, whose `targets`-based orchestration is too slow,
opaque, difficult to debug, and difficult to maintain. V2 must make pipdata
itself a transparent, self-contained dependency-tracking package before any
future orchestrator is selected.

The current pipeline tracks DLW source hashes and some auxiliary-data hashes,
but it does not record code provenance, treats all auxiliary changes as a
cleaning trigger, and lacks incremental deflation invalidation. Consequently,
a change to cleaning or deflation code can silently leave artifacts stale, and
a CPI-only change can unnecessarily re-clean household microdata.

The historical brainstorm
`2026-08-14-orchestration-cache-invalidation-audit.md` identified a broad
dependency-DAG idea. This decision refines its pipdata-internal portion only.
It explicitly does not select a future orchestrator or design the estimation
stage.

## Requirements

- Detect changes without loading household-survey data.
- Explain which input changed, which artifact is stale, and which action will
  run for each `survey_id` or `pip_id`.
- Preserve `stamp` as the authoritative version store for `pip`, `pip_meta`,
  and `pip_deflated` artifacts. Pipdata must not modify stamp internals.
- Create new stamp versions whenever a planned action changes its output.
- Support partial successes: a failed survey must never be marked current
  merely because other surveys completed under the same code version.
- Track the transitive, value-affecting function closure for each pipeline
  stage, including formal arguments/defaults and relevant external package
  versions.
- Recompute current code fingerprints once at each top-level pipeline entry,
  so installed releases, `load_all()`, and in-session namespace changes are
  detected.
- Provide a read-only change report and have execution use the same planning
  logic so reports and actions cannot drift.
- Treat legacy artifacts with unknown code provenance as stale and establish a
  trustworthy baseline through one full rebuild.
- Do not choose or require an external orchestrator, `targets`, or future
  estimation logic.

## Approaches Considered

### Approach 1: Minimal Code-Hash Patch

Add global cleaning and deflation code hashes plus a small report around the
existing invalidation gates.

**Pros:** Smallest implementation and catches global code changes quickly.

**Cons:** Retains the current coarse rule that re-cleans data for CPI/PPP
changes, does not provide precise deflation invalidation, and cannot safely
represent partial successes with a global hash alone.

**Effort:** Medium.

### Approach 2: Staged Provenance Planner with Dependency Manifest

Model clean, metadata, and deflate as explicit artifact stages. A pipdata-owned
dependency manifest stores per-artifact input/output provenance and code
fingerprints; a single planner derives targeted actions and their reasons.

**Pros:** Matches the required per-survey behavior, supports partial failures,
keeps planning fast and inspectable, and preserves stamp version history.

**Cons:** Requires refactoring the current coarse auxiliary invalidation,
pinning exact input versions, manifest lifecycle handling, and broad tests.

**Effort:** Large.

### Approach 3: Generic DAG/Orchestration Engine

Build a declarative graph framework intended to schedule pipdata and future
estimation work.

**Pros:** Maximum future flexibility.

**Cons:** Prematurely recreates an orchestration system, exceeds pipdata's
current boundary, and introduces substantial design risk.

**Effort:** Very large.

## Decision

Choose **Approach 2: staged provenance planner with a pipdata-owned dependency
manifest**.

### Stage Contract

```text
DLW + PFW + recode spec + cleaning code
  -> pip (cleaned data)

pip + CPI/PPP/pop/GDP/PCE + metadata code
  -> pip_meta

pip + pip_meta + deflation code
  -> pip_deflated
```

Future estimation artifacts are deliberately out of scope, but the manifest
and planner must expose stable provenance suitable for a later orchestrator to
consume.

### Invalidation Rules

- A DLW or PFW change for a survey triggers clean, metadata refresh, and
  deflation for that survey's resulting `pip_id`s.
- A recode-spec or cleaning-code change triggers clean, metadata refresh, and
  deflation for all affected surveys. Initially, code changes are global and
  affect all surveys in the relevant stage.
- A CPI, PPP, or population change triggers metadata refresh and deflation for
  affected surveys, without re-cleaning household microdata.
- A GDP or PCE change triggers metadata refresh only unless a declared
  downstream dependency requires more work.
- A deflation-code change triggers deflation only; it does not re-clean data.
- A changed cleaned-data or metadata artifact triggers deflation of matching
  `pip_id`s.

### Code Fingerprints

Use three curated, value-affecting function groups declared in `aaa.R`:

- `clean`: PFW merge/split, DLW cleaning methods, recode handlers, and
  cleaning helpers.
- `metadata`: `pd_aux_attr()` and helpers that change `pip_meta`.
- `deflate`: deflation methods and helpers that change `pip_deflated`.

Each function fingerprint hashes its `formals()` and `body()`. Stage hashes are
deterministic composites of sorted per-function fingerprints. The curated
groups must include relevant S3 methods and internal handlers, not only
exported wrappers. Relevant external implementation versions (notably `wbpip`)
are recorded for audit and considered when defining the stage fingerprint.

The current fingerprints are recomputed once at the start of
`pd_process_data()` and `pd_deflate_pipeline()`. The package version is stored
as audit metadata, not used as a blanket invalidation trigger.

### Dependency Manifest

Pipdata owns a compact, atomic dependency manifest at
`tools::R_user_dir("pipdata", which = "cache")`, overridable through
`pipdata.dependency_manifest_path`.

The manifest contains a global header (schema version, stage fingerprints,
function-level fingerprints, package/external version metadata, and timestamps)
and records keyed by stage plus `survey_id` or `pip_id`. Each record captures
the exact successful input fingerprints, output artifact version/content hash,
stage code fingerprint, and completion state.

The manifest is updated only for successfully written artifacts and uses atomic
replacement. It can therefore represent partial runs: successful records advance
while failed records retain their prior/unknown provenance. It is an index owned
by pipdata; stamp remains authoritative for stored artifact versions.

### Planning and Visibility

An internal read-only planner, tentatively `pd_dependency_plan()`, compares
current inventories, auxiliary state, code fingerprints, and manifest records.
It returns a deterministic action table containing entity identifiers, stages,
reasons, and old/new provenance values.

An exported `pd_change_report()` presents and returns that plan without writing
artifacts. `pd_process_data()` and `pd_deflate_pipeline()` consume the same
planner and emit equivalent structured logging during execution.

### Bootstrap

Existing artifacts lack reliable code-stage provenance. They are treated as
unknown rather than assumed current. The first deployment performs one full
baseline rebuild of clean, metadata, and deflated artifacts to establish a
trustworthy manifest.

## Next Steps

1. Define the stage/action/reason contract and dependency-manifest schema.
2. Map and test the curated function closures for clean, metadata, and deflate
   stages, including S3 methods and external implementation dependencies.
3. Implement atomic manifest read/write, corruption handling, and the full
   baseline-rebuild migration path.
4. Implement the read-only dependency planner and `pd_change_report()` before
   mutating existing pipeline selection behavior.
5. Refactor auxiliary handling so metadata refresh is independent of data
   cleaning, then wire planner actions into `pd_process_data()`.
6. Pin the exact recode-spec, cleaned-data, and metadata versions consumed by
   each downstream artifact; wire incremental deflation actions into
   `pd_deflate_pipeline()`.
7. Add unit and integration coverage for no-change, per-survey DLW/PFW/CPI
   changes, global code changes, partial failures, malformed manifests,
   deterministic plans, and read-only reporting.
8. Update `compound-gpid.md` Current Focus to describe artifact-level
   dependency planning rather than only a content-hash re-clean trigger.
