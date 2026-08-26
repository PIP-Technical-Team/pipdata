---
date: 2026-08-25
title: "Rewrite the DLW wrapper around explicit acquisition and validation stages"
status: completed
completed-date: 2026-08-26
completed-phases: [1, 2, 3, 4, 5]
execution-report: ".cg-docs/work-reports/2026-08-26-dlw-wrapper-rewrite.md"
scope: "Deep"
brainstorm: ".cg-docs/brainstorms/2026-08-24-dlw-wrapper-rewrite.md"
language: "R"
estimated-effort: "large"
deviation-policy: "ask"
artifact-schema-version: 1
phases: 5
roadmap-feature: "dlw-wrapper-rewrite"
tags: [dlw, pipeline, architecture, acquisition, validation, logging, retry, orchestration]
---

# Plan: Rewrite the DLW Wrapper Around Explicit Acquisition and Validation Stages

## Objective

Refactor `pipdata_get_gmd()`, `pipdata_validate_gmd()`, and
`pipdata_dlw_process()` into explicit stage-owned orchestrators that map compact
one-survey workers, isolate survey execution failures, persist trustworthy
inventories, emit the existing B2 logging contracts, and invisibly return
inspectable DLW-specific results.

The implementation must preserve public signatures, current artifact and log
identifiers, existing operator-side quiet behavior, and independent DLW-stage
execution. It must prepare an adapter-friendly handoff for Stream C without
adding a generic result class, shared pipeline context, safe-step framework,
run ID, resume token, or top-level orchestrator.

## Context

The source brainstorm selected **Stage-owned delegates with survey workers**.
The current branch was fetched and verified against `origin/PROD` before
planning. It contains the required upstream merges:

| Dependency | Merge | Planning consequence |
|---|---|---|
| B1 data-driven validation | `4bd90fe` | `dlw_validation_engine()` remains the only validation-rule engine. |
| B2 unified logging/reporting | `5370bf6` | Existing discriminators, phases, report sections, and checkpoint alias remain stable. |
| C1 targeted `force_surveys` | `f6d213f` | Reference pattern for explicit candidate ownership and compatibility. |
| A1 batch deflation | `3faf8f0` | Reference pattern for stage-owned map/worker outcomes. |
| A2 data-level semantics | `c06f772` | Current production baseline; must not regress. |

### Current Source Facts

- `pipdata_get_gmd()` returns invisible `NULL`, catches individual download
  failures, and currently persists the acquisition inventory twice because
  `dlw_gmd_new(update_inventory = TRUE)` writes before the download loop and
  `pipdata_get_gmd()` writes again afterwards.
- `pipdata_validate_gmd()` returns invisible `NULL`; data-load errors are
  isolated, but `stamp::st_info()` and validation-engine errors can abort the
  complete map.
- `dlw_gmd_unvalidated(check_missing = TRUE)` currently retains prior
  `data_available = "No"` rows on the anti-join right-hand side. Failed
  validation executions are therefore normally suppressed rather than retried.
- Validation inventory is currently persisted before validation report. A
  report-write failure can leave inventory state that prevents automatic
  report repair on rerun.
- A completed engine result containing `type == "error"` is an invalid-data
  classification, but current `build_dlw_validation_summary()` counts its
  `phase = "validation"` error log as failed execution.
- `inv_gmd_list` is documented as a configurable acquisition-inventory ID, but
  the active compare/load/existence paths are hardcoded to `"dlw_gmd_inv"`.
- Catalog helpers consider seven modules, while acquisition and downstream
  cleaning actively process five: `ALL`, `GROUP`, `HIST`, `GPWG`, and `BIN`.
- The validation report is accumulated in package-global
  `.pipdataenv$validation_report`; B3 may reset it before one validation map but
  must not redesign B1's accumulation mechanism.
- Current cleaning planning consumes rows present in `gmd_valid_inv` without a
  dedicated validation-retry filter. B3 therefore cannot use that artifact as
  both a completed-data inventory and a retry-control ledger.
- The installed planning environment exposes `pipfun 1.0.1`, `pipload 1.0.0`,
  `dlw 0.1.2`, and `testthat 3.3.2`. `DESCRIPTION` permits different compatible
  versions, so coding must verify behavior rather than depend only on the local
  version numbers.

### Implementation Base Gate

At final planning review, branch `HEAD` was `c06f772` while current
`origin/PROD` was `a55a089`, six commits ahead. `/cg-work` must not begin
production edits from that stale base.

Before Phase 1:

1. Run `git fetch --prune origin`.
2. Require `git merge-base --is-ancestor origin/PROD HEAD` to succeed. If it
   fails, stop for user-approved merge/rebase; do not auto-resolve conflicts.
3. Re-read the five planned production source files and protected files changed
   upstream. If source behavior materially invalidates a pinned contract, stop
   under `deviation-policy: ask` and revise the plan.
4. Record `BASE_SHA = git rev-parse HEAD` in the execution report only after
   synchronization and drift audit.
5. Run and record the test/check baseline from that exact SHA before edits.

All final path-boundary evidence compares against recorded `BASE_SHA`, not the
historical brainstorm commit.

### Brain and Prior-Work Findings

- B2 intentionally standardized typed logging and deferred the structural DLW
  rewrite to this roadmap feature.
- Existing no-work calls are successful stage outcomes and still produce a DLW
  stage marker/checkpoint.
- Inventory-driven candidate selection should remain owned by the stage that
  decides work; wrapper-level injection or log parsing would fragment policy.
- The deflation plan's later correction demonstrates that current source and
  installed API behavior must override optimization assumptions in historical
  brainstorms.
- Typed logging must remain at orchestration boundaries because caller-formal
  capture can retain large objects in persistent log state.

## Requirements

| ID | Requirement | Source |
|---|---|---|
| R1 | Keep acquisition and validation as explicit, ordered, independently callable stages. | Brainstorm |
| R2 | Preserve exact public signatures, defaults, parameter order, and exports for the three DLW public functions. | Compatibility policy |
| R3 | Invisibly return documented plain lists while normal unassigned calls remain non-printing. | User decision |
| R4 | Return stage identity, outcome, trustworthy inventory, stage summary, compact failures, and artifact write facts without raw R conditions. | Brainstorm |
| R5 | Use one-survey acquisition and validation workers that return compact facts and do not own discovery, persistence, or typed stage logging. | Chosen architecture |
| R6 | Isolate survey download, load, artifact-info, engine, and inventory-row failures without stopping sibling surveys. | User decision |
| R7 | Treat an engine-completed invalid survey as successful validation execution (`n_invalid`), not failed execution (`n_failed`). | User decision + source audit |
| R8 | Preserve acquisition inventory ID/alias/PK and make non-default `inv_gmd_list` control existence, load, comparison, and write consistently. | Public parameter contract |
| R9 | Preserve the current five active acquisition modules; correct documentation that claims ASPIRE/L downloads. | Current source |
| R10 | Persist the acquisition inventory once per completed attempt and make unresolved `"No"` rows retryable under `check_missing = TRUE`. | Resume decision + source audit |
| R11 | Preserve validation inventory schema as completed-data state, omit execution-failure rows, and upsert exactly one completed row per active `survey_id`. | Resume + cleaning handoff |
| R12 | Set `pipeline_version` to `1L` or `max(prior pipeline_version) + 1L` for the next completed validation; failed attempts do not consume persisted versions. | Source audit decision |
| R13 | Reset the B1 report accumulator immediately before a non-empty validation map and retain current-run report state after the call. | B1 compatibility |
| R14 | Replace report rows for every attempted survey, append new engine output only for completed executions, then write report before inventory commit. | Retry consistency decision |
| R15 | Distinguish first-run missing artifacts from unreadable existing artifacts; never overwrite trustworthy history after a read failure. | Data integrity |
| R16 | Convert runtime stage errors after setup into failed results while preserving caller/precondition aborts, user cancellation, and interrupts. | User decision |
| R17 | Run validation whenever a trustworthy persisted acquisition inventory exists, even if acquisition failed to add new surveys; block only when validation prerequisites are unavailable. | User continuation decision |
| R18 | Derive aggregate outcome and B2 summary logs from returned stage facts, never by parsing `piplog`. | Brainstorm |
| R19 | Preserve B2 discriminators and existing phases; add a validation completion entry and legacy report fallback that separate valid, invalid, and execution-failed counts. | Logging compatibility |
| R20 | Preserve the DLW checkpoint name/stage/alias; record checkpoint failure separately without changing the completed business outcome. | Brainstorm + plan decision |
| R21 | In noninteractive execution, missing acquisition inventory returns a failed stage rather than invoking `utils::menu()`; interactive cancellation still propagates. | Orchestrator readiness |
| R22 | Use inventory-driven at-least-once reruns; do not add run IDs, resume tokens, exact-once checkpoints, or per-survey durable writes. | User decision |
| R23 | Copy loaded/caller-visible `data.table` objects before reference mutation. | R/data.table safety |
| R24 | Keep `pipdata_dlw_process()` public as an expert stage after a future top-level orchestrator exists. | User decision |
| R25 | Update tests, roxygen, NEWS, generated `.Rd`, and pipeline vignettes for the new return, error, retry, and validation semantics. | Migration requirement |
| R26 | Do not add generic stage infrastructure, new dependencies, exports, or external package changes. | Stream boundary |
| R27 | Validate every public argument before entering a runtime error-conversion boundary. | Plan review P2.3 |
| R28 | Reconcile every thrown or invalid persistence result against reloaded durable state; never assume stamp rollback. | Plan review P1.1 |
| R29 | Pin acquisition to the candidate `FileName` and reject ambiguous `dlw_call_list` returns as download failures. | Plan review P1.3 |
| R30 | Scope acquisition and validation report counts, phase details, and failure lists to their respective latest log-attempt segments. | Plan review P2.2 + fresh review P2.2 |
| R31 | Use a canonical phase/event/error vocabulary that distinguishes invalid classification, execution failure, workflow failure, and logger failure. | Plan review P2.5 |
| R32 | Apply a server-authoritative acquisition merge matrix for current, stale, inactive, and ASPIRE/L rows. | Plan review P2.6 |
| R33 | Enforce a pinned validation-report schema drift policy before either validation artifact is persisted. | Plan review P2.7 |
| R34 | Route the wrapper's custom acquisition inventory ID through an unexported validation core while the public validation signature remains unchanged. | Plan review P1.2 |
| R35 | Return wrapper-level post-setup bootstrap failures in a top-level compact failures table. | Verification review P2.3 |
| R36 | Apply and reconcile the server-authoritative full-inventory merge even when zero survey workers are selected. | Verification review P1.1 |
| R37 | Pin wrapper-only `not_run` result shape and every validate-only/dependency-failed aggregate path. | Verification review P1.2 |
| R38 | Reconcile retained public utility/bootstrap writes and abort direct utility calls when intended durable state is not verified. | Verification review P2.2 |
| R39 | Emit an unconditional validation attempt boundary before fallible discovery and segment reports from the latest boundary first. | Verification review P2.1 |
| R40 | Represent confirmed absence, permitted absence, not-reached writes, and unreadable durable state explicitly in artifact/checkpoint facts. | Final review P1.1 |
| R41 | Emit and validate one exact authoritative validation completion-log schema. | Final review P2.1 |
| R42 | Pin folder resolution/directory-check ownership and phases for wrapper, standalone acquisition, and standalone/validate-only validation modes. | Final review P2.2 |
| R43 | Inspect prior validation-report state before every no-work return and fail on legacy inventory/report inconsistency. | Handoff review P2.1 |
| R44 | Synchronize with current `origin/PROD`, re-audit source drift, and record the exact implementation base SHA before edits. | Seal review P2.3 |
| R45 | Use complete executable focused/full/vignette/boundary evidence commands with an explicit changed-path allowlist. | Seal review P2.3 |
| R46 | Update and allow regenerated documentation for all exported `dlw_gmd_*` utilities whose retry/persistence semantics change. | Definitive review P2.1 |
| R47 | Force network replacement for every selected acquisition candidate and reject cached/ambiguous returns as success. | Fresh review P1.1 |
| R48 | Keep execution failures out of `gmd_valid_inv`; store only completed `valid`/`invalid` rows so cleaning never consumes retry-control state. | Fresh review P1.2 |
| R49 | Reconcile validation inventory and report to the authoritative current acquisition keys, including zero-worker pruning. | Fresh review P1.3 |
| R50 | Enforce an all-path report/inventory key invariant before mapping and before persistence. | Fresh review P1.4 |
| R51 | Canonicalize DLW artifacts deterministically before durable-content comparison and use one narrow DLW persistence reconciler. | Fresh review P2.1 |
| R52 | Add acquisition attempt boundaries and latest-attempt report segmentation equivalent to validation. | Fresh review P2.2 |
| R53 | Pin validation acquisition-schema, inventory-schema, and report-consistency phases plus deterministic legacy duplicate resolution. | Fresh review P2.3 |
| R54 | Emit and validate exact acquisition completion and wrapper summary logmeta schemas. | Fresh review P2.4 |
| R55 | Filter legacy validation inventories to completed `Yes` plus `valid`/`invalid` rows at cleaning and change-report entry points. | Verification P1.1 |
| R56 | Require one authoritative current checksum per normalized filename/survey ID before acquisition mapping. | Verification P1.2 |
| R57 | Enforce explicit acquisition and completed-validation inventory schemas with safe coercion and fatal malformed completed rows. | Verification P2.1 |
| R58 | Compute next validation pipeline version from pre-pruning completed per-survey history so checksum changes do not reset versions. | Verification P2.2 |
| R59 | Prevent stale DLW rows from appearing in generic report sections; dedicated latest-attempt DLW sections own those entries. | Verification P2.3 |
| R60 | Deduplicate exact normalized validation-report rows deterministically on no-work and worker paths. | Verification P3.1 |

## Pinned Contracts

### Stage Result Shape

Acquisition and validation return the following names in this order:

```r
c("stage", "outcome", "inventory", "summary", "failures", "artifacts")
```

`outcome` uses:

| Value | Meaning |
|---|---|
| `success` | At least one candidate completed, every required commit is verified, and no execution/workflow failure occurred. |
| `partial` | Every required commit is verified, at least one candidate completed, and at least one worker or non-commit workflow failure occurred. |
| `failed` | A required commit is unverified/failed, no candidate completed while failures occurred, or discovery/workflow failed before useful completion. |
| `no_work` | Discovery completed against trustworthy state and selected zero candidates. |
| `not_run` | Nested wrapper-only stage result; `summary$reason` is `disabled` or `dependency_failed`. |

Invalid validation classifications count as completed candidates. They do not
cause `partial` or `failed` unless execution or workflow failures also occur.

Stage outcomes use this precedence, evaluated top to bottom:

1. Trustworthy zero-candidate discovery with no workflow/logging failure is
   `no_work`.
2. Any required artifact commit that is not verified after reconciliation is
   `failed`, even when workers completed.
3. A pre-map discovery/workflow failure is `failed`.
4. Zero completed candidates plus one or more worker/workflow failures is
   `failed`, even when another durable inventory state was committed.
5. One or more completed candidates plus worker or non-commit workflow
   failures is `partial`.
6. All candidates completed with verified commits and no workflow failure is
   `success`.

### Failure Table

Every stage result contains a `data.table`, including the zero-row case, with:

| Column | Type | Contract |
|---|---|---|
| `survey_id` | character | Survey ID or `NA_character_` for workflow failures. |
| `phase` | character | Stable operation boundary. |
| `error_type` | character | Most specific non-generic condition class, falling back to `unknown_error`; synthetic workflow failures use a pinned phase-specific value. |
| `condition_msg` | character | `conditionMessage()` text or a deterministic synthetic message. |

Condition objects, calls, traces, and survey data must not be retained in the
result.

### Canonical Failure and Logging Vocabulary

Existing B2 phases are preserved. New phases are additive and must use the
following meanings:

| Stage | Event | Phase | Meaning |
|---|---|---|---|
| acquisition | info | `attempt_start` | Unconditional positional boundary before fallible discovery. |
| acquisition | info | `no_new_data`, `start`, `complete` | Existing stage lifecycle entries. |
| acquisition | error | `catalog_load` | Local inventory or server catalog could not be loaded. |
| acquisition | error | `catalog_schema` | Required catalog columns/types are unavailable. |
| acquisition | error | `inventory_missing` | Requested acquisition inventory is absent and cannot be bootstrapped in the current mode. |
| acquisition | error | `folder_resolve`, `directory_check` | Standalone/acquisition-stage folder resolution or required directory check failed. |
| acquisition | error | `download` | One candidate download failed or returned an ambiguous `dlw_call_list`. |
| acquisition | error | `inventory_match` | Candidate/full-inventory merge or uniqueness validation failed. |
| acquisition | error | `inventory_save` | Write and reconciliation did not verify the intended inventory. |
| acquisition | none | `log_emit` | Logger failure recorded only in the result; do not recursively log it. |
| validation | info | `attempt_start` | Unconditional positional boundary emitted before any fallible discovery work. |
| validation | info | `no_new_data`, `start`, `inventory_save`, `report_save`, `complete` | Existing phases plus the new completion summary. |
| validation | error | `catalog_load`, `inv_load_fail`, `load`, `artifact_info_fail` | Existing load/workflow failures. |
| validation | error | `inventory_missing` | Requested acquisition inventory is absent, including validate-only execution. |
| validation | error | `folder_resolve`, `directory_check` | Validation-stage folder resolution or required directory check failed. |
| validation | error | `catalog_schema` | Current acquisition inventory lacks required columns/types. |
| validation | error | `inventory_schema` | Existing validation inventory is malformed or has unresolved conflicting active duplicates. |
| validation | error | `validation` | Engine completed and classified data invalid; this is not a failure-table row. |
| validation | error | `validation_engine` | Engine raised before classification completed. |
| validation | error | `inventory_row` | Retry inventory row could not be constructed. |
| validation | error | `report_unavailable`, `report_load_fail`, `report_save`, `inventory_fail`, `inventory_save` | Existing report/inventory workflow failures. |
| validation | error | `report_schema` | Required columns/types or compatible additive drift checks failed. |
| validation | error | `report_consistency` | Report IDs do not satisfy the completed-validation inventory key invariant. |
| validation | none | `log_emit` | Logger failure recorded only in the result; do not recursively log it. |
| wrapper | none | `alias_init` | Post-setup stamp alias initialization failed. |
| wrapper | none | `folder_resolve` | Working folder resolution failed. |
| wrapper | none | `directory_check` | A required directory check failed. |
| wrapper | none | `bootstrap_inventory` | Interactive/noninteractive inventory bootstrap did not verify intended durable state. |

For B2 log entries, `logmeta$error` remains the stable DLW discriminator and
the operation stays in `phase`. Failure-table `error_type` uses the normalized
condition class or a phase-specific synthetic value such as
`catalog_schema_error` or `inventory_missing_error`. A logger failure appends `phase = "log_emit"` directly
to the stage failures table, does not call another logger, and affects outcome
under the stage precedence above.

If acquisition or validation cannot emit `attempt_start`, that call suppresses
all later typed entries for the same stage attempt so they cannot contaminate
the preceding segment. The returned stage result records `log_emit`; business
work may continue, but no unbounded start/failure/completion entries are added.

### Acquisition Completion Log Schema

Every runtime acquisition attempt that emitted `attempt_start` attempts one
completion entry with exactly these fields and no extras:

```r
list(
  info = .logtype_dlw_acquisition,
  phase = "complete",
  outcome = "partial",
  n_total = 2L,
  n_success = 1L,
  n_failed = 1L,
  surveys_success = "A",
  surveys_failed = "B"
)
```

`info` and `phase` equal the constants shown; `outcome` is `success`,
`partial`, `failed`, or `no_work`; counts are length-one nonnegative whole
integers; IDs are character, non-`NA`, nonempty, unique, and disjoint; vector
lengths equal counts; and `n_total = n_success + n_failed`. `n_total` counts
terminal worker outcomes, so catalog/schema/pre-map failures use zero survey
counts plus their workflow phase. Completion emission/omission and malformed
fallback rules match validation.

### DLW Wrapper Summary Log Schema

`dlw_summary_inf` preserves its four existing fields verbatim and appends only
the following scalar fields, in this exact schema:

```r
list(
  info = .logtype_dlw_summary,
  phase = "complete",
  get_dlw_data = TRUE,
  validate_dlw_data = TRUE,
  outcome = "partial",
  acquisition_outcome = "success",
  validation_outcome = "partial",
  acquisition_n_total = 2L,
  acquisition_n_success = 2L,
  acquisition_n_failed = 0L,
  validation_n_total = 2L,
  validation_n_valid = 1L,
  validation_n_invalid = 0L,
  validation_n_failed = 1L
)
```

Flags are scalar non-missing logicals. Outcomes are scalar allowed stage or
aggregate values; nested outcomes may also be `not_run`. Counts are scalar
nonnegative whole integers and use zero for disabled/dependency-not-run stages.
Acquisition and validation arithmetic follows their completion schemas. No
nested lists or survey-ID vectors are added, preserving existing
`build_stage_warning()` compatibility.

### Validation Completion Log Schema

Every runtime validation attempt that emitted `attempt_start` must attempt one
authoritative completion entry after its final result is known:

```r
list(
  info = .logtype_dlw_validation,
  phase = "complete",
  outcome = "partial",
  n_total = 3L,
  n_valid = 1L,
  n_invalid = 1L,
  n_failed = 1L,
  surveys_valid = "A",
  surveys_invalid = "B",
  surveys_failed = "C"
)
```

A valid completion has exactly the fields shown above with no extras;
`info == .logtype_dlw_validation`; `phase == "complete"`; scalar character
`outcome`;
`outcome` is one of `success`, `partial`, `failed`, or `no_work`; all four
counts are length-one, nonnegative, whole-number integers; all three survey
fields are character vectors whose IDs are non-`NA`, nonempty, unique, and
pairwise-disjoint; vector lengths
equal their counts; and `n_total = n_valid + n_invalid + n_failed`. Workflow
failures before candidate selection may validly have zero counts and
`outcome = "failed"`; the workflow failure is rendered from its phase entry.

For validation, `n_total` means the number of terminal worker outcomes, not the
number of selected candidates. Candidates selected but never mapped because a
report-load, report-schema, accumulator-reset, or other pre-map workflow
failure occurs are not placed in `surveys_failed`; completion uses zero survey
counts and the workflow phase conveys the failure. Once mapping starts, every
worker must terminate as valid, invalid, or execution-failed, so the arithmetic
still holds.

Completion is attempted for success, partial, failed, and no-work runtime
results. It is absent only when preconditions/cancellation/interrupt prevent a
runtime attempt, `attempt_start` itself cannot be emitted, or completion logging
fails. A completion logger failure becomes `log_emit` in the returned result.
The reporter selects a completion only after isolating the latest attempt
segment and uses fallback only when that segment contains no valid completion.

### Acquisition Summary and Artifacts

Acquisition summary names are:

```r
c("n_total", "n_success", "n_failed", "surveys_success", "surveys_failed")
```

The artifact shape is:

```r
list(
  inventory = list(
    id = inv_gmd_list,
    alias = "dlw_inv",
    attempted = TRUE,
    success = TRUE,
    trustworthy = TRUE,
    version_id = "...",
    skipped = FALSE,
    reconciled = FALSE
  )
)
```

`trustworthy = TRUE` means `inventory` represents durable state: either a
successfully loaded prior inventory on a no-work/failure path or a successfully
committed updated inventory. On a failed write, the returned inventory is the
prior durable inventory unless reconciliation proves that the intended table
became active despite a reported write failure.

### Acquisition Inventory Schema

Before selection or merge, normalize and validate these required columns:

| Column | Type/rule |
|---|---|
| `Country` | Nonempty character. |
| `Year` | Nonmissing whole-number integer after safe numeric coercion. |
| `Survey_acronym` | Nonempty character. |
| `Vermast`, `Veralt` | Nonempty character; factors safely become character. |
| `Module` | Character in `GPWG`, `GROUP`, `BIN`, `HIST`, `ALL`, `ASPIRE`, `L`. |
| `Collection` | Nonempty character. |
| `FileName` | Nonempty character `.dta` filename. |
| `Checksum` | Nonempty character. |
| `Ext` | Character equal to `dta` after case normalization for server rows. |
| `data_available` | Local inventory only; character `Yes` or `No`. Server rows receive `No` before merge. |

Additional server columns are allowed and canonicalized lexicographically.
Unsafe type conversion, missing keys, unknown availability/status values, or
malformed required fields fail `catalog_schema`; they are never silently
pruned. Define normalized filename as uppercase basename of `FileName`. After
catalog module/extension filtering, exactly one distinct `Checksum` may exist
per normalized filename/derived survey ID. Multiple checksums fail
`catalog_schema` before any worker unless Step 1 verifies a documented server
authoritative-current field and the user approves that deviation.

Every artifact/checkpoint write fact follows this truth table:

| Durable state | `attempted` | `success` | `trustworthy` | `version_id` | `skipped` | `reconciled` |
|---|---:|---:|---:|---|---:|---:|
| No write needed; prior state loaded | `FALSE` | `NA` | `TRUE` | prior ID or `NA` | `NA` | `FALSE` |
| Confirmed absent and absence is permitted | `FALSE` | `NA` | `TRUE` | `NA` | `NA` | `FALSE` |
| Required write not reached; prior/absence is known | `FALSE` | `FALSE` | `TRUE` | prior ID or `NA` | `NA` | `FALSE` |
| Required write not reached; prior state unreadable/unknown | `FALSE` | `FALSE` | `FALSE` | `NA` | `NA` | `FALSE` |
| Write returned a version | `TRUE` | `TRUE` | `TRUE` | returned ID | `FALSE` | `FALSE` |
| Identical-content write skipped | `TRUE` | `TRUE` | `TRUE` | `NA` unless supplied | `TRUE` | `FALSE` |
| Reported failure; reload proves intended content active | `TRUE` | `TRUE` | `TRUE` | recovered ID or `NA` | `FALSE` | `TRUE` |
| Reported failure; reload proves prior content active | `TRUE` | `FALSE` | `TRUE` | prior ID or `NA` | `FALSE` | `TRUE` |
| Reported failure; active content is unreadable/ambiguous | `TRUE` | `FALSE` | `FALSE` | `NA` | `FALSE` | `TRUE` |

Persistence helpers must treat a thrown error, `version_id = NULL` without
`skipped = TRUE`, and any malformed return as an uncertain write. They reload
the artifact, compare canonical intended/prior content, and normalize the fact
using this table. They must not infer rollback from the return value.

The value accompanying a known prior row is a copied prior table. Confirmed
absence and unknown/unreadable states return `inventory = NULL`. Permitted
absence applies only where the contract allows no artifact, such as a
first-run all-execution-failed validation report. A required missing
acquisition inventory uses the known-absence row (`success = FALSE`,
`trustworthy = TRUE`) and still blocks continuation because no non-NULL
inventory exists. Continuation therefore requires both a trustworthy fact and
a non-NULL durable inventory.

### Durable Content Equality

Use one narrow unexported DLW persistence reconciler parameterized by an
artifact-specific canonicalizer/comparator. It is shared only by acquisition,
validation report/inventory, retained DLW utility writes, and checkpoint facts;
it is not a stage-result constructor or generic pipeline abstraction.

Canonicalization rules are:

| Artifact | Rows | Columns and attributes |
|---|---|---|
| Acquisition inventory | Stable sort by `FileName`, `Checksum`. | Pinned required catalog columns first, remaining catalog columns lexicographically, `data_available` last; remove transient keys/indices/row names; apply the verified `stamp_pk = c("Checksum", "FileName")` semantics before hash/equality comparison. |
| Validation inventory | Stable sort by `survey_id`. | Core persisted columns in pinned order, parsed fields lexicographically afterwards; remove transient keys/indices/row names; apply verified `stamp_pk = "survey_id"`. |
| Validation report | Stable sort by `table_name`, `type`, `message`, `description`, then remaining stable columns. | Required fields first, additive fields lexicographically; normalize required character types; remove transient data-table attributes. |
| Checkpoint | Not table-canonicalized. | Reconcile by deterministic ID, readable artifact, and verified version advancement/skipped state relative to the pre-attempt catalog snapshot. |

Both intended and reloaded objects are copied and canonicalized before
comparison. Shuffled server rows, raw versus reloaded stamp-PK attributes, and
equivalent data-table internal attributes must compare equal and must not create
new no-work versions. Step 1 verifies the exact installed stamp PK attribute
name/shape before implementation.

### Validation Summary and Artifacts

Validation summary names are:

```r
c(
  "n_total", "n_valid", "n_invalid", "n_failed",
  "surveys_valid", "surveys_invalid", "surveys_failed"
)
```

Validation artifacts contain `report` and `inventory` write facts. Each uses
the acquisition write-fact fields. `validation_report` remains represented by
artifact facts only; its full table is not returned.

The validation inventory preserves these core columns and current sentinels:

```text
survey_id, pipeline_version, latest_version_id, content_hash, file_path,
status, data_available, date_validated, Checksum
```

Completed validation rows require:

| Column | Type/rule |
|---|---|
| `survey_id` | Nonempty character, one completed row per active ID. |
| `pipeline_version` | Nonmissing positive integer. |
| `latest_version_id`, `content_hash`, `file_path`, `Checksum` | Nonempty character. |
| `status` | Character `valid` or `invalid`. |
| `data_available` | Character `Yes`. |
| `date_validated` | Nonmissing `POSIXct` after safe Date/POSIX normalization; arbitrary character dates are not silently coerced. |
| `country_code`, `survey_acronym`, `vermast`, `veralt`, `module` | Nonempty character fields produced/validated by `survey_id_to_vars()`. |
| `surveyid_year` | Nonmissing whole-number integer. |

Recognized legacy retry rows are exactly `status == ""` and
`data_available == "No"` with empty/absent artifact metadata; they are pruned
as control state. Any other malformed completed/unknown row fails
`inventory_schema`. Safe coercion is limited to factor-to-character and
whole-number numeric-to-integer conversions. Required parsed downstream fields
must survive canonical ordering and are not optional.

`gmd_valid_inv` is a completed-data inventory, not a retry-control ledger. It
contains only workers whose engine completed with `status = "valid"` or
`"invalid"` and `data_available = "Yes"`. Load, artifact-info, engine, and
inventory-row execution failures remain in the returned result and typed log
but contribute no persisted validation-inventory row. They retry because the
authoritative acquisition `survey_id`/`Checksum` key remains absent from the
completed validation inventory.

Before checksum pruning, compute a validated historical maximum
`pipeline_version` per `survey_id` across loaded completed rows. The next
completed version is `1L` when no history exists or historical max plus `1L`,
regardless of checksum change. Persist it only when validation completes. A
prior checksum at version 5 followed by a successful new checksum is version 6;
failed retries do not consume version 6. Attempt history belongs to
logs/results, not the cleaning inventory.

### Cleaning Eligibility Handoff

Because only completed `valid`/`invalid` rows are written to `gmd_valid_inv`,
new B3 artifacts contain no retry-control rows. A narrow consumer guard is still
required for legacy durable artifacts that may be loaded before revised
validation migrates them. Existing invalid-row consumption behavior is
preserved; changing whether invalid completed data may clean is a separate
policy decision outside B3. Tests must prove execution-failed IDs are absent
from new artifacts and excluded from every legacy cleaning/planning entry.

### Validation Report Schema and Commit Rule

The current report must contain character-compatible required fields
`table_name`, `message`, `type`, `description`, `module_type`, `vermast`,
`veralt`, `country_code`, and `rf_year`. Required fields are normalized to
character before merging. A column present on only one side is compatible
additive drift and is retained with typed missing values. Same-name optional
columns must have identical compatible classes; missing required columns,
incompatible required types, or incompatible same-name optional types produce
`phase = "report_schema"` and block both writes. The old warn-and-fill behavior
is not sufficient for incompatible drift.

The validation stage first derives authoritative completed state:

1. Restrict current acquisition to `data_available == "Yes"` and its active
   `survey_id`/`Checksum` keys.
2. Retain prior completed validation rows only when both key fields still match
   current acquisition.
3. Drop catalog-deleted, superseded-checksum, acquisition-failed, and recognized
   legacy retry-control rows; fail malformed completed rows as
   `inventory_schema`.
4. Resolve legacy duplicates by the unique active acquisition checksum and
   highest `pipeline_version`; identical tied rows deduplicate, while
   conflicting tied active rows fail `inventory_schema`.
5. Remove report rows for every pruned validation ID.

This authoritative pruning runs even with zero workers. A deletion-only,
failed-replacement, or stale-state reconciliation may write report and inventory
while retaining stage `outcome = "no_work"` when no worker ran and commits are
verified.

The all-path report/inventory key invariant is:

- the set of `validation_report$table_name` values exactly equals the set of
  completed `gmd_valid_inv$survey_id` values;
- every completed ID has one or more report rows;
- no report row exists for pruned, absent, or execution-failed IDs; and
- report rows for one ID are distinct only when at least one persisted column
  differs after schema/type normalization.

Deterministically remove exact duplicate rows across the complete persisted
column set after normalization and canonical column ordering. Because
`assertion.id` is not persisted, it is not a usable identity. Different
normalized rows for the same completed ID remain distinct validation checks;
exact duplicates never survive no-work repair or worker merge. If future
conflicting duplicates require identity beyond persisted fields, fail
`report_consistency` rather than inventing one.

Validate this invariant after loading/pruning prior state and again after adding
worker outcomes. Readable extra/orphan report IDs are repairable by removal.
Missing report IDs for completed inventory are `report_consistency` and block
mapping/writes because diagnostics cannot be reconstructed. Report-only state
left by prior report-success/inventory-failure is repaired against committed
inventory, never accepted as trustworthy completed state.

The commit sequence remains:

1. Build authoritative prior state and repairable report removals.
2. Build all worker results in memory.
3. Append inventory/report rows only for workers whose engine completed.
4. Revalidate the exact key invariant.
5. Persist `validation_report` first when durable content changes.
6. Persist `gmd_valid_inv` last.

Every reported write failure is reconciled by reloading durable content. If the
report write is not verified as intended, inventory is not written. If report
is verified and inventory is not, the returned inventory/facts reflect the
reloaded durable state. If the prior inventory remains active, the next rerun
reselects surveys and idempotently replaces report rows. If intended inventory
is active despite the reported failure, normalize the commit as recovered
success. Ambiguous/unreadable durable state is untrustworthy and blocks
validation continuation.

When every worker fails before engine execution, the absent current-run report
accumulator is expected. A first-run all-failed stage with empty authoritative
prior state writes neither report nor validation inventory. If authoritative
pruning removed stale prior state, persist the repaired empty/nonempty report
first and pruned completed inventory last despite zero completed workers.

Before any validation `no_work` return, inspect the prior report artifact:

| Authoritative completed inventory state | Report state | Stage behavior |
|---|---|---|
| Inventory absent/empty | Report confirmed absent | `no_work`; report fact is permitted confirmed absence. |
| Inventory absent/empty | Readable report has only extra/orphan IDs | Repair report to empty; verified state-only write remains `no_work`. |
| Inventory has completed rows | Report exactly covers completed IDs | `no_work`; prior state is consistent. |
| Inventory has completed rows | Report confirmed absent or misses any completed ID | `failed` with `report_consistency`; no writes. |
| Any inventory state | Readable report has extra/orphan IDs only | Remove extras and persist repaired report before any inventory write. |
| Any inventory state | Report or inventory unreadable/incompatible | `failed` with load/schema/consistency phase; no unverified writes. |
| Any inventory state | Report exists but is unreadable/incompatible | `failed` with `report_load_fail` or `report_schema`; no writes. |

This invariant is checked on no-work and worker paths. A no-work result never
uses an uninspected report fact or accepts report-only uncommitted state.

### Aggregate Result and Continuation

The wrapper returns:

```r
list(
  stage = "dlw",
  outcome = "partial",
  acquisition = acquisition_result,
  validation = validation_result,
  failures = wrapper_failure_dt,
  checkpoint = list(
    summary_logged = TRUE,
    summary_condition_msg = NA_character_,
    attempted = TRUE,
    success = TRUE,
    trustworthy = TRUE,
    alias = "dlw_meta",
    stage = "dlw",
    version_id = "...",
    skipped = FALSE,
    reconciled = FALSE,
    condition_msg = NA_character_
  )
)
```

Wrapper `failures` uses the same compact failure-table columns. It owns
post-setup failures that precede or sit outside a requested stage, with phases
`alias_init`, `folder_resolve`, `directory_check`, and `bootstrap_inventory`.
Any critical wrapper failure prevents requested stages, sets them to
`not_run: dependency_failed`, and forces aggregate `failed`. `setup_working_release()`
failure remains an escaping precondition error. Summary/checkpoint failures
remain in `checkpoint` and do not duplicate into wrapper failures.

Wrapper-only `not_run` results retain the six-field stage shape but are an
explicit exception to acquisition/validation summary schemas:

```r
list(
  stage = "acquisition", # or validation
  outcome = "not_run",
  inventory = NULL,
  summary = list(reason = "disabled"), # or dependency_failed
  failures = typed_empty_failure_dt,
  artifacts = list()
)
```

A requested validation stage never returns `not_run` for its own missing
inventory/folder prerequisite. It returns `failed` with the validation
`inventory_missing` or relevant workflow phase. `not_run: dependency_failed`
is reserved for validation blocked by an earlier acquisition or wrapper
failure before validation can own execution.

Validation runs when requested and a trustworthy persisted acquisition
inventory exists. This includes a no-work result, prior durable state returned
after a catalog/write failure, and an updated inventory containing partial or
all-failed download outcomes. Validation is `not_run` with
`summary$reason = "dependency_failed"` only when no trustworthy acquisition
inventory or validation prerequisite exists.

Aggregate outcomes use this exhaustive requested-stage matrix:

| Acquisition | Validation | Aggregate |
|---|---|---|
| `success` | `success` or `no_work` | `success` |
| `success` | `partial` or `failed` | `partial` |
| `no_work` | `success` | `success` |
| `no_work` | `no_work` | `no_work` |
| `no_work` | `partial` or `failed` | `partial` |
| `partial` | `success`, `no_work`, `partial`, or `failed` | `partial` |
| `failed` with trustworthy state | `success`, `no_work`, or `partial` | `partial` |
| `failed` with trustworthy state | `failed` | `failed` |
| `failed` without trustworthy state | `not_run: dependency_failed` | `failed` |
| `not_run: disabled` | requested validation outcome | Validation outcome. |
| requested acquisition outcome | `not_run: disabled` | Acquisition outcome. |
| `not_run: disabled` | `not_run: disabled` | `no_work` |

Additional legality rules make the matrix exhaustive:

- Acquisition disabled plus validate-only missing inventory is acquisition
  `not_run: disabled`, validation `failed: inventory_missing`, aggregate
  `failed`.
- Acquisition success/no-work/partial plus a validation-owned prerequisite
  failure is validation `failed`, producing aggregate `partial` except when a
  critical wrapper failure already forces aggregate `failed`.
- Validation `not_run: dependency_failed` is legal only after acquisition or
  wrapper failure prevented validation from owning execution.
- Both disabled produces `no_work` only when wrapper failures are empty;
  post-setup alias/folder/bootstrap failure produces aggregate `failed`.

Checkpoint failure remains explicit in `checkpoint$success` and
`condition_msg`, but does not rewrite the business `outcome`. Summary-log
failure sets `checkpoint$summary_logged = FALSE` and records only in
`summary_condition_msg`, so simultaneous summary/checkpoint failures retain
both diagnostics. Checkpoint success requires a valid version/skipped return or
post-failure reconciliation against the deterministic checkpoint artifact.

### Public Argument Validation

Validation occurs before setup or runtime error conversion:

| Function | Arguments validated |
|---|---|
| `pipdata_dlw_process()` | Nonempty scalar character `inv_gmd_list`, `release`, and `identity`; identity in `PROD`, `INT`, `TEST`; scalar non-missing logical `get_dlw_data`, `validate_dlw_data`, `check_missing`, and `verbose`. |
| `pipdata_get_gmd()` | Nonempty scalar character `inv_gmd_list`; scalar non-missing logical `check_missing` and `verbose`. |
| `pipdata_validate_gmd()` | Scalar non-missing logical `verbose`. |
| Internal validation core | Nonempty scalar character acquisition inventory ID plus validated `verbose`. |

Wrong types, `NA`, zero-length values, empty strings, and length greater than
one abort as caller errors and never become failed stage results.

### Custom Inventory Validation Route

Add an unexported validation core that accepts
`acquisition_inventory_id = "dlw_gmd_inv"`. The exported
`pipdata_validate_gmd(verbose)` validates its public arguments and calls the
core with the default ID. `pipdata_dlw_process(inv_gmd_list = ...)` calls the
same core with its requested ID. Public formals for validation and comparison
utilities remain unchanged. An end-to-end test must run with only a custom
acquisition artifact present.

### Server-Authoritative Acquisition Merge

The current seven-module server catalog is authoritative for retained keys:

| Row state | Full-inventory result |
|---|---|
| Current catalog key matches prior `Yes` | Retain current catalog row with `data_available = "Yes"`. |
| Current five-module candidate was attempted | Retain current catalog row with worker `Yes`/`No`. |
| Current checksum changed for an existing filename | Drop the old checksum row; retain the current candidate result. |
| Prior key is absent from current catalog | Drop it from the current inventory. Stamp history preserves old versions. |
| Prior successful ASPIRE/L key remains current | Retain it as `Yes` for compatibility. |
| New/unresolved ASPIRE/L key | Omit it because active acquisition does not download those modules. |
| Current five-module key is prior `No` and `check_missing = FALSE` | Retain the current catalog row as `No` and do not attempt it. |
| Prior unresolved row is no longer current | Drop it. |

The merge must produce exactly one row per normalized filename/survey ID (and
therefore one `FileName`/`Checksum` key), preserve current server columns, and
never retain stale catalog-deleted checksums. The full
intended inventory is always built after discovery, even when zero download
candidates are selected. If it differs from prior durable state, persist and
reconcile that catalog-only change while retaining `outcome = "no_work"` when
the write is verified and no worker ran.

### Preconditions, Runtime Failures, and Cancellation

The following remain escaping caller/precondition conditions:

- invalid public arguments;
- missing `release` or `identity` in `pipdata_dlw_process()`;
- absent working release for standalone delegates;
- `setup_working_release()` failure;
- explicit interactive user cancellation; and
- R interrupts.

After setup and preconditions succeed, folder, artifact, catalog, schema,
worker, persistence, logging, and checkpoint errors are converted to compact
result facts. `tryCatch(error = ...)` and `piperr` handlers must not use a
general `condition` handler, so interrupt conditions continue to propagate.

When an acquisition inventory is missing and `get_dlw_data = TRUE`:

- interactive wrapper calls preserve the Download/Abort menu;
- noninteractive wrapper calls return a failed acquisition result with
  `phase = "inventory_missing"` and do not call `menu()`.

Standalone acquisition and validation delegates never invoke `menu()`; after
confirming a working release, they own and return their respective failed
`inventory_missing` result.

Validate-only wrapper calls (`get_dlw_data = FALSE`,
`validate_dlw_data = TRUE`) never invoke `menu()`, regardless of
`interactive()`. Acquisition is `not_run: disabled`; validation owns the
missing inventory as `failed: inventory_missing`; aggregate outcome is
`failed`.

### Folder and Bootstrap Failure Ownership

| Call mode | Owner and behavior |
|---|---|
| Wrapper, both stages disabled | Skip `get_pip_folders()` and directory/bootstrap checks. Alias initialization, summary, and checkpoint still run; alias failure is wrapper-owned. |
| Wrapper with acquisition requested | Wrapper owns only folder resolution and `dlw_inventory` directory work needed for interactive bootstrap. After bootstrap, acquisition owns its `dlw_data` and `dlw_inventory` checks. |
| Wrapper validate-only | Wrapper skips acquisition bootstrap folder checks. The validation core owns `dlw_data`, `dlw_inventory`, `dlw_metadata`, and missing acquisition inventory failures. |
| Standalone acquisition | Acquisition owns folder resolution plus `dlw_data` and `dlw_inventory` checks and returns failed `folder_resolve`/`directory_check`. |
| Standalone validation | Validation owns folder resolution plus `dlw_data`, `dlw_inventory`, and `dlw_metadata` checks and returns failed `folder_resolve`/`directory_check`. |

Wrapper-owned folder/bootstrap failure populates the top-level failures table
and dependency-blocks requested stages. Stage-owned folder failure returns that
requested stage as `failed`, not `not_run`. Tests must cover every listed
folder in standalone acquisition, standalone validation, validate-only wrapper,
and acquisition-requested wrapper modes.

## Implementation Steps

## Phase 1: Contract Scaffolding and Dependency Preflight

### 1. Verify Runtime Dependency Contracts and Record the Baseline

- **Requirements**: R2, R8, R15, R16, R20, R21, R26, R27, R28, R29, R34, R40, R42, R44
- **Files**: `tests/testthat/test-pipdata_dlw_compare.R`, `tests/testthat/test-dlw-unified-logging.R`; read-only verification of installed `pipfun`, `pipload`, `dlw`, and `stamp` APIs
- **Details**: Before edits, fetch origin, require current origin/PROD to be an ancestor, obtain user-approved merge/rebase if needed, re-audit upstream-changed planned/protected files, record synchronized BASE_SHA, and run baseline tests/check. Add executable characterization tests for formals, IDs/PKs, modules, stage marker/checkpoint, and compare utilities. Verify checkpoint/pip_write returns, missing artifacts, arbitrary IDs, st_info metadata, DLW terminal/ambiguous returns, and menu behavior. On temporary aliases, fault-inject payload/sidecar/catalog failures and reload prior/intended/ambiguous content. Stop before edits if synchronization, source drift, or durable state cannot be resolved.
- **Test Scenarios**: Default and custom inventory IDs; current five-module set; removed `log`/`save_log`; checkpoint completion; null-version and skipped returns; payload/sidecar/catalog fault injection; prior/intended/ambiguous post-failure reload; missing versus corrupt artifacts; single and ambiguous DLW matches; invalid public argument shapes.
- **Tests**: `testthat::test_file("tests/testthat/test-pipdata_dlw_compare.R")`; `testthat::test_file("tests/testthat/test-dlw-unified-logging.R")`; baseline `devtools::check(error_on = "never")` recorded in the work report
- **Acceptance criteria**: Branch contains current origin/PROD; synchronized BASE_SHA and baseline are recorded; source drift is reconciled with the plan; contracts are executable; rollback is never assumed; every external behavior is verified or blocks before edits.

### 2. Add Stage-Specific Result and Failure Constructors

- **Requirements**: R3, R4, R16, R23, R26, R28, R31, R40, R51
- **Files**: `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`, `tests/testthat/test-pipdata_get_gmd.R`, `tests/testthat/test-pipdata_validate_gmd.R`
- **Details**: Add stage-specific result/failure helpers plus one narrow DLW persistence reconciler that accepts artifact-specific canonicalization/comparison. Normalize write facts, copy inventories, and return plain lists. Canonicalize row/column order, transient data-table internals, and verified stamp PK semantics before comparing intended/prior/reloaded content; checkpoint uses version advancement/readability. Distinguish every truth-table state. Do not add a stage class/shared result constructor/registry.
- **Test Scenarios**: Zero/workflow/logger failures; invalid summary; every write fact; NULL absent/unknown; prior/intended/ambiguous; shuffled rows/columns; raw versus reloaded PK-bearing table; equivalent data-table internals; checkpoint version reconciliation; raw-condition rejection; non-mutation.
- **Tests**: Focused constructor tests in the two new stage test files
- **Acceptance criteria**: Both stage constructors produce the exact documented names/types and reject malformed internal inputs; no generic result abstraction or raw condition is introduced.

## Phase 2: Acquisition Stage Redesign

### 3. Separate Acquisition Discovery, Selection, and Inventory Merge

- **Requirements**: R5, R8, R9, R10, R15, R23, R28, R32, R36, R38, R51, R56, R57
- **Files**: `R/pipdata_dlw_compare.R`, `tests/testthat/test-pipdata_dlw_compare.R`
- **Details**: Extract pure helpers that load/copy prior inventory, enforce the explicit catalog schema and one checksum per normalized filename, select/filter candidates, and always build server-authoritative intended inventory. Multiple checksums for one normalized filename fail `catalog_schema` before mapping unless an approved documented server-current field exists. Canonical output uses stable row/column order. Keep utility signatures; reconcile direct writes; no hidden stage write.
- **Test Scenarios**: New/changed checksum; existing success; unresolved retry on/off over two calls; deletion-only no-candidate write; server-column-refresh-only write; retained successful/omitted unresolved ASPIRE/L; default/custom utility write; valid/skipped/null/throw write with prior/intended/ambiguous reload; duplicate keys; no `.joyn`; non-mutation.
- **Tests**: `testthat::test_file("tests/testthat/test-pipdata_dlw_compare.R")`
- **Acceptance criteria**: Discovery can run without persistence; custom stage IDs are honored consistently; exported utility behavior remains compatible; candidate and merge helpers are deterministic and side-effect free.

### 4. Implement the One-Survey Acquisition Worker

- **Requirements**: R5, R6, R9, R16, R23, R29, R47
- **Files**: `R/pipdata_get_gmd.R`, `tests/testthat/test-pipdata_get_gmd.R`
- **Details**: Add a worker that calls `dlw::dlw_get_gmd()` with existing mappings plus exact `filename = candidate$FileName` and `local_overwrite = TRUE` for every selected candidate. A normal terminal non-`dlw_call_list` return is success; ambiguous/cached-only results are download failures and never mark `Yes`. The worker does not discover/persist/log/retain conditions/catch interrupts.
- **Test Scenarios**: Successful forced download; existing local pin with changed checksum proves overwrite/download path; cached read cannot be success; ambiguous multi-match; typed/generic error; exact filename/overwrite/other arguments; interrupt; no raw conditions.
- **Tests**: Worker tests in `tests/testthat/test-pipdata_get_gmd.R`
- **Acceptance criteria**: One failed download cannot stop sibling workers, and worker outputs contain only compact scalar facts.

### 5. Rebuild `pipdata_get_gmd()` as the Acquisition Orchestrator

- **Requirements**: R1, R2, R3, R4, R5, R6, R8, R9, R10, R15, R16, R22, R23, R27, R28, R31, R36, R42, R47, R52, R54
- **Files**: `R/pipdata_get_gmd.R`, `tests/testthat/test-pipdata_get_gmd.R`, `tests/testthat/test-dlw-unified-logging.R`
- **Details**: Validate arguments, confirm release, then emit acquisition `attempt_start` before fallible folders/discovery. If boundary logging fails, suppress later acquisition logs for that call. Build/reconcile the authoritative intended inventory even with zero workers. Worker paths write once and reconcile. Emit exact acquisition completion logmeta from terminal outcomes; logger failure is `log_emit`. All-download-failed with verified state is failed but durable.
- **Test Scenarios**: Invalid arguments; attempt boundary success/failure suppression; exact acquisition completion valid/malformed schema; folders; unchanged/catalog-only no-work; all outcomes; forced overwrite; workflow/write/log failure; reconciliation; custom ID; one write; `verbose`; result/log equality; interrupt.
- **Tests**: `testthat::test_file("tests/testthat/test-pipdata_get_gmd.R")`; acquisition cases retained in `test-dlw-unified-logging.R`
- **Acceptance criteria**: Acquisition returns invisibly with the pinned contract, persists once, isolates survey failures, preserves logs/artifacts, and retries unresolved rows on the next call.

## Phase 3: Validation Stage Redesign

### 6. Replace Validation Candidate and Inventory Merge Semantics

- **Requirements**: R5, R11, R12, R15, R22, R23, R34, R48, R49, R53, R56, R57, R58
- **Files**: `R/pipdata_dlw_compare.R`, `R/pipdata_validate_gmd.R`, `tests/testthat/test-pipdata_dlw_compare.R`, `tests/testthat/test-pipdata_validate_gmd.R`
- **Details**: Add pure helpers parameterized by acquisition ID that enforce explicit schemas and acquisition checksum uniqueness, capture completed historical max version per survey before pruning, derive active available keys, prune nonmatching/legacy control rows, and select missing completed keys. Resolve legacy duplicates by active checksum then highest version; identical ties deduplicate, conflicting active ties fail inventory_schema. Upsert only completed workers. New checksum uses pre-pruning historical max plus one; failures consume no version.
- **Test Scenarios**: Default/custom ID; same filename multiple checksums fails catalog_schema; new/changed/deleted; prior checksum version 5 plus successful new checksum version 6; failed retries retain next 6; failed replacement prunes old; ASPIRE/L; zero-worker; legacy retry prune; duplicate tie policies; schema coercion/failure; uniqueness/non-mutation.
- **Tests**: Compare and validation test files
- **Acceptance criteria**: Candidate selection and inventory assembly are deterministic, retryable, and produce exactly one row per `survey_id` without resetting later pipeline versions.

### 7. Implement the One-Survey Validation Worker

- **Requirements**: R5, R6, R7, R11, R12, R13, R16, R23, R31, R48, R57, R58
- **Files**: `R/pipdata_validate_gmd.R`, `tests/testthat/test-pipdata_validate_gmd.R`
- **Details**: Add a worker around load, st_info, B1 mapping/engine, classification, and completed inventory-row construction. Keep seven mappings plus skip. Only valid/invalid return a persistable inventory row and report rows. Execution failures return compact failure facts with no inventory/report row, so they remain absent and retry automatically. Reserve `validation` for completed invalid classification; use exact execution phases. Do not change B1 source/spec.
- **Test Scenarios**: Valid/invalid persistable rows; load/artifact/engine/inventory-row failures return no inventory/report row and remain selectable next call; unknown module; blank data; stamp mapping; interrupt; no survey data/conditions retained.
- **Tests**: Unit cases in `test-pipdata_validate_gmd.R`; one smoke case with the real B1 engine and existing synthetic fixtures
- **Acceptance criteria**: Every survey attempt produces one compact worker outcome; only completed valid/invalid outcomes produce inventory/report rows; execution failures remain absent and retryable; sibling workers continue.

### 8. Make Validation Report Assembly Run-Scoped and Retry-Safe

- **Requirements**: R7, R13, R14, R15, R23, R33, R43, R49, R50, R53, R60
- **Files**: `R/pipdata_validate_gmd.R`, `tests/testthat/test-pipdata_validate_gmd.R`
- **Details**: Reset accumulator before a non-empty map. Load/validate all prior state, build authoritative pruned validation state, and enforce exact report coverage. Repair extra/orphan rows, fail missing completed IDs, fail malformed inventories, normalize compatible additive fields, and deterministically remove exact duplicate report rows across all persisted columns. Worker paths append only completed normalized outcomes, deduplicate again, and recheck invariant before writes.
- **Test Scenarios**: Empty/absent; report-only orphan; missing completed row; extra row; exact duplicate versus distinct normalized check rows on no-work and worker paths; legacy retry pruning; new-candidate inconsistencies; deletion/failed replacement/zero-worker; accumulator/drift/exact phases/idempotence.
- **Tests**: Report-workflow cases in `test-pipdata_validate_gmd.R`
- **Acceptance criteria**: Current-run report content cannot be contaminated by prior in-memory runs, stale attempted-survey rows are not presented as current, and unreadable history is never overwritten.

### 9. Rebuild `pipdata_validate_gmd()` and Commit Report Before Inventory

- **Requirements**: R1, R2, R3, R4, R5, R6, R7, R11, R12, R13, R14, R15, R16, R19, R22, R23, R27, R28, R31, R33, R34, R39, R41, R42, R43, R48, R49, R50, R51, R53, R58, R60
- **Files**: `R/pipdata_validate_gmd.R`, `tests/testthat/test-pipdata_validate_gmd.R`, `tests/testthat/test-dlw-unified-logging.R`
- **Details**: Implement validation core with attempt boundary. Load/canonicalize all prior state, derive authoritative pruning and report repairs, enforce invariant, then select. With zero workers, persist verified state-only repairs report-first/inventory-last and retain no_work. Otherwise reset/map, add only completed rows, recheck invariant, canonicalize, persist/reconcile. Terminal-worker counts exclude unattempted candidates. Attempt exact completion logmeta. Execution failures never enter trustworthy inventory.
- **Test Scenarios**: Public/default/custom ID; folders/boundary; zero-worker deletion/report repair; nonempty candidates with pre-map failures zero counts; all outcomes; all-failed no inventory row; failed replacement removes old validation/report; report-success/inventory-failure then acquisition disappearance repair; canonical shuffled/hash equality; reconciliation/order/IDs/PKs; `verbose`; log equality; interrupt.
- **Tests**: `testthat::test_file("tests/testthat/test-pipdata_validate_gmd.R")`; validation integration cases in `test-dlw-unified-logging.R`
- **Acceptance criteria**: Validation returns invisibly, isolates all survey execution failures, commits retry-safe durable state, distinguishes invalid from failed, and preserves B1/B2 contracts.

### 10. Guard Cleaning and Change-Report Entry Points

- **Requirements**: R48, R55, R57
- **Files**: `R/dependency_execution.R`, `R/pd_process_data.R`, `R/pd_change_report.R`, `tests/testthat/test-dependency-execution.R`, `tests/testthat/test-pd_process_data.R`, `tests/testthat/test-pd-change-report.R`
- **Details**: Add one narrow unexported completed-validation filter requiring `data_available == "Yes"` and `status %in% c("valid", "invalid")`, with the pinned validation schema checks. Apply it immediately after `pd_process_data()` loads/receives `inv`, at the start of `pd_prepare_execution()` as defense in depth for internal callers, and before `pd_change_report()` calls `pd_dependency_plan()`. Preserve current invalid-data policy and every public signature. Legacy blank/`No` rows are excluded from snapshots, plans, force mappings, and row lookup before any clean action is created.
- **Test Scenarios**: Direct caller inventory and loaded durable inventory containing valid, invalid, canonical legacy retry, and malformed rows; only valid/invalid create snapshot/plan actions; retry IDs absent from `pd_process_data()` row lookup and `pd_change_report()` output; malformed completed rows fail the pinned schema phase; empty deflation inventory remains compatible.
- **Tests**: Focused dependency-execution, process-data, and change-report test files plus E3
- **Acceptance criteria**: No current or legacy execution-failure control row can enter cleaning/dependency planning before or after validation migration; no cleaning or change-report public contract changes.

## Phase 4: Wrapper Orchestration and Reporting

### 11. Implement Explicit DLW Stage Continuation and Aggregate Outcomes

- **Requirements**: R1, R2, R3, R4, R16, R17, R18, R20, R21, R22, R23, R24, R27, R28, R34, R35, R37, R38, R42
- **Files**: `R/pipdata_dlw_process.R`, `tests/testthat/test-pipdata_dlw_process.R`
- **Details**: Validate arguments before setup. Add helpers for wrapper failures, exact not-run, continuation, and exhaustive matrix. After setup, own alias failures. Skip folder/bootstrap resolution when both stages disabled. With acquisition requested, wrapper owns only folder/directory work required for inventory bootstrap; delegates own later stage checks. Validate-only delegates all folder/missing-inventory checks to validation. Critical wrapper failures dependency-block stages and force failed aggregate. Honor/reconcile custom bootstrap and internal validation core. Preserve cancellation/interrupts.
- **Test Scenarios**: Invalid arguments; four flags/matrix; legal dependency-not-run pairs; interactive and noninteractive validate-only missing default/custom inventory never call menu and return validation failed; acquisition-requested menu choices; acquisition-requested wrapper bootstrap folder failure versus acquisition stage folder failure; alias failure including both disabled; custom artifact; utility reconciliation; delegate error; cancellation/interrupt.
- **Tests**: `testthat::test_file("tests/testthat/test-pipdata_dlw_process.R")`
- **Acceptance criteria**: Every continuation and aggregate outcome row is executable and deterministic; the wrapper remains public, quiet, and independent.

### 12. Derive Summary Logging and Checkpoint Facts From the Aggregate

- **Requirements**: R18, R19, R20, R28, R31, R40, R54
- **Files**: `R/pipdata_dlw_process.R`, `tests/testthat/test-pipdata_dlw_process.R`, `tests/testthat/test-dlw-unified-logging.R`
- **Details**: Emit the exact pinned `dlw_summary_inf` schema, preserving the four existing fields and adding only scalar outcomes/counts. Validate names/types/arithmetic before logging and prove no log parsing. Attempt checkpoint after summary, validate/reconcile every return, and populate all write facts. Keep separate summary/checkpoint messages, no recursive logging, and unchanged business outcome.
- **Test Scenarios**: Exact valid summary for all flag/outcome combinations; missing/extra/wrong-type/arithmetic/nested payload rejection; preservation of four existing fields; summary throw; checkpoint version/skipped/null/throw/reconciliation; simultaneous failures; no parsing; exact arguments/visibility.
- **Tests**: Wrapper and unified-logging test files
- **Acceptance criteria**: Observability failures are explicit, existing checkpoint identity remains stable, and business outcome is independent of checkpoint success.

### 13. Update DLW Attempt-Scoped Reporting

- **Requirements**: R7, R19, R30, R31, R39, R41, R52, R54, R59
- **Files**: `R/log_report.R`, `tests/testthat/test-log_report.R`
- **Details**: Segment acquisition and validation independently from each latest attempt_start, then validate exact completion. Infer only inside current segment; preserve legacy boundaries. Dedicated DLW sections exclusively own all `.logtype_dlw_acquisition`, `.logtype_dlw_validation`, and `.logtype_dlw_summary` rows. Exclude those discriminators from generic country/type/detail builders so stale or even current DLW failures cannot be double-counted outside the segmented sections. Suppress later logs when boundary emission fails.
- **Test Scenarios**: Both exact/malformed completions; success/failure ordering; no-work/pre-map; validation invalid/execution/workflow; boundary failure; legacy/mixed; full rendered report proves obsolete and current DLW IDs appear only in dedicated latest-attempt sections and never generic country/type sections.
- **Tests**: `testthat::test_file("tests/testthat/test-log_report.R")`
- **Acceptance criteria**: Report counts agree with stage results, invalid data is not labeled failed execution, and historical logs remain renderable.

## Phase 5: Migration, Documentation, and Final Verification

### 14. Update Public Documentation, Vignettes, and Release Notes

- **Requirements**: R2, R3, R4, R7, R8, R9, R10, R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R27, R28, R29, R30, R31, R32, R33, R34, R46, R47, R48, R49, R50, R51, R52, R53, R54, R55, R56, R57, R58, R59, R60
- **Files**: `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`, `R/pipdata_dlw_process.R`, `R/pipdata_dlw_compare.R`, `R/log_report.R`, `R/dependency_execution.R`, `R/pd_process_data.R`, `R/pd_change_report.R`, `NEWS.md`, three pipeline vignettes, generated wrapper/utility/report manuals plus `man/pd_process_data.Rd` and `man/pd_change_report.Rd`
- **Details**: Document results/outcomes, canonical reconciliation, explicit schemas, forced replacement, acquisition uniqueness, authoritative completed validation state, retry by absence, report coverage/dedup, cleaning/change-report legacy guard, custom IDs, and exact latest-attempt logs. Document utility semantics and generic-report DLW exclusion. Retain wrapper as current DLW entry point. Knit articles before roxygen; regenerate manuals only through roxygen.
- **Test Scenarios**: Roxygen renders list structures and value vocabulary; vignette examples use current signatures; no deprecated validator list remains; no claim of exact-once resume or generic stage class; no stale claim that all seven modules download.
- **Tests**: `devtools::document()` followed by documentation/vignette checks and final package check
- **Acceptance criteria**: Reference and narrative documentation agree with current source and migration risks are prominent in `NEWS.md`.

### 15. Run Final Regression, Boundary, and Built-Package Verification

- **Requirements**: R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31, R32, R33, R34, R35, R36, R37, R38, R39, R40, R41, R42, R43, R44, R45, R46, R47, R48, R49, R50, R51, R52, R53, R54, R55, R56, R57, R58, R59, R60
- **Files**: All allowed files; no additional scope
- **Details**: Run every command in Final Evidence Commands with synchronized BASE_SHA. Render all three excluded Rmd articles directly to a temporary directory. Compare final package check with synchronized baseline and permit no new ERROR/WARNING. Execute tracked-plus-untracked allowlist gate against BASE_SHA, including Compound workflow artifacts. Inspect final diff for generated debris, raw conditions, formal drift, duplicate logs, reference mutation, or generic infrastructure.
- **Test Scenarios**: Clean full run; shuffled/order-sensitive validation state where supported; built package exports/docs; final path boundary; baseline check comparison.
- **Tests**: Commands in the Completion Contract Verification Surface
- **Acceptance criteria**: All required evidence passes, no protected boundary changed, and the plan is ready for `/cg-work` completion/roadmap handoff.

## Testing Strategy

### Test Organization

| File | Primary responsibility |
|---|---|
| `tests/testthat/test-pipdata_dlw_compare.R` | Pure candidate/retry/module/server-authoritative merge, no-worker catalog changes, custom-ID, duplicate, and reconciled compatibility-writer behavior. |
| `tests/testthat/test-pipdata_get_gmd.R` | Acquisition result, worker, persistence, error, interrupt, logging, and non-mutation contracts. |
| `tests/testthat/test-pipdata_validate_gmd.R` | Validation result, worker, retry, version, upsert, report, commit-order, error, interrupt, and non-mutation contracts. |
| `tests/testthat/test-pipdata_dlw_process.R` | Setup, top-level bootstrap failures, exact not-run shapes, custom-ID core routing, continuation, aggregate, summary, checkpoint, and cancellation contracts. |
| `tests/testthat/test-dlw-unified-logging.R` | Cross-stage B2 discriminator/phase/checkpoint compatibility. |
| `tests/testthat/test-log_report.R` | Latest-attempt-first segmented completion rendering and historical fallback. |
| `tests/testthat/test-dependency-execution.R` | Defense-in-depth completed-validation filtering before snapshots/plans. |
| `tests/testthat/test-pd_process_data.R` | Public cleaning entry excludes legacy execution-failure rows before action lookup. |
| `tests/testthat/test-pd-change-report.R` | Change-report entry excludes legacy execution-failure rows. |

### Mocking Rules

- Mock external calls in their owner namespaces: `pipfun`, `pipload`, `stamp`,
  and `dlw`.
- Mock internal helpers with `local_mocked_bindings(..., .package = "pipdata")`.
- Use `withr` temporary directories and cleanup for options, stamp builders,
  `.pipdataenv$validation_report`, and log state.
- Do not use real DLW network or production repository I/O.
- Assert observed call order with recorded event vectors; do not use source-text
  position tests for write/checkpoint ordering.
- Test interrupts with an interrupt-class condition and an outer interrupt
  handler; do not rely on `expect_error()` for cancellation semantics.
- Recursively assert that no returned object retains a condition.
- Include at least one B1 engine smoke test; keep golden fixture tests unchanged.

### Test Sequence

1. Run the focused test file for the active step.
2. Run the related integration test file before completing each phase.
3. Run unchanged B1 tests after validation changes.
4. Run the full suite after Phase 4 and after documentation regeneration.
5. Run the package check only after targeted/full tests pass.

## Documentation Checklist

- [ ] `pipdata_get_gmd()` documents five active modules and acquisition result.
- [ ] `pipdata_validate_gmd()` documents valid/invalid/failed semantics and report artifact reference.
- [ ] `pipdata_dlw_process()` documents aggregate, continuation, noninteractive behavior, and public lifecycle.
- [ ] `log_report()` documents completion-entry preference and legacy fallback.
- [ ] `dlw_gmd_match()`, `dlw_gmd_new()`, `dlw_gmd_unvalidated()`, and `dlw_gmd_list()` document server-authoritative selection and reconciled direct writes.
- [ ] `NEWS.md` calls out assigned-return and runtime-error behavior changes.
- [ ] `Validating-Data.Rmd` describes `dlw_validation_engine()` rather than deprecated per-module validators.
- [ ] `PIP-data-pipeline.Rmd` shows optional result capture and partial outcomes.
- [ ] `Processing-Data.Rmd` keeps DLW result/report wording consistent.
- [ ] `pd_process_data()` and `pd_change_report()` document completed-validation input filtering without changing signatures.
- [ ] Documentation names `pipdata_dlw_process()` as the current supported DLW entry point and describes `run_pipeline()` only as future direction.
- [ ] Generated `.Rd` files match roxygen source.
- [ ] No exact-once, generic-class, seven-module-download, or automatic top-level orchestration claim appears.

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| Runtime failures no longer abort and an unassigned script continues | High | High | Emit conspicuous typed error/CLI output, document prominently in NEWS, and test failed outcomes; top-level orchestrator later consumes results. |
| Stamp reports failure after activating new content | Medium | High | Fault-inject in Phase 1; reconcile every invalid/thrown write by reloading prior/intended content; block on ambiguous state. |
| Absent or not-reached artifacts are mislabeled trustworthy/successful | Medium | High | Use explicit known-absence/unknown-state write facts and require non-NULL inventory for continuation. |
| Report succeeds but inventory commit reports failure | Medium | High | Write report first, inventory last; reconcile active inventory; replace report rows idempotently on rerun; retain stamp history. |
| Report failure after workers could otherwise strand inventory | Medium | High | Reconcile report state and never attempt inventory unless intended report content is verified active. |
| Execution failure control rows leak into cleaning planning | High | High | Never persist failure rows in `gmd_valid_inv`; retry by absence and assert failed IDs are absent. |
| Legacy control rows are consumed before revised validation migrates inventory | High | High | Filter completed rows at `pd_process_data`, `pd_prepare_execution`, and `pd_change_report` entry points. |
| Changed checksum creates duplicate/stale `survey_id` rows or resets version | Medium | High | Reconcile to active acquisition key, upsert completed rows, compute next completed version, and assert uniqueness. |
| Invalid data remains counted as execution failure | High | Medium | Add completion summary and update log-report fallback semantics. |
| Package-global validation report leaks rows across calls | High | High | Reset before non-empty map; isolate tests; leave parallel/run-scoped redesign out of scope. |
| Custom `inv_gmd_list` remains partially hardcoded | Medium | High | Route wrapper through an internal validation core accepting the ID and test with only custom artifact present. |
| Acquisition module scope accidentally expands to ASPIRE/L | Medium | Medium | Pin five modules in tests and correct roxygen. |
| Server exposes multiple checksums for one active filename | Medium | High | Require one normalized filename/checksum before mapping and fail catalog schema absent an approved authoritative marker. |
| Cached or ambiguous DLW return is marked as a changed download | Medium | High | Pass exact `filename`, force `local_overwrite = TRUE`, reject `dlw_call_list`/cached-only paths, and test existing-pin replacement. |
| Zero-worker catalog changes are mislabeled trustworthy without persistence | Medium | High | Always build full intended inventory and reconcile deletion/refresh-only writes while retaining no-work only after verification. |
| Reference mutation changes loaded or caller-owned inventories | Medium | High | Copy before `:=`, ordering, or merge; add non-mutation tests. |
| External package API differs from installed planning environment | Medium | High | Phase 1 executable preflight and blocked-stop before production edits. |
| Branch starts from stale production and protected files drifted upstream | High | High | Require origin ancestry, user-approved synchronization, source re-audit, recorded BASE_SHA, and post-sync baseline before edits. |
| Checkpoint/logging failure obscures successful business output | Low | Medium | Record separate observability facts and do not rewrite business outcome. |
| Historical log failures leak into current validation report section | High | Medium | Segment from the latest attempt boundary first, then validate completion only inside that segment. |
| Acquisition history still leaks while validation is segmented | High | Medium | Emit acquisition attempt boundaries and segment both report sections independently. |
| Completion emitter and report parser disagree on valid metadata | Medium | High | Share the pinned schema/invariants in tests for both producer and consumer without creating a generic stage class. |
| Validation report schema drift corrupts history | Medium | High | Pin required character fields, allow compatible additive drift only, and fail before writes on incompatible drift. |
| No-work validation hides missing/corrupt legacy report state | Medium | High | Inspect prior report before no-work and fail explicitly when inventory rows lack a trustworthy report. |
| Validation/report retain catalog-deleted or superseded surveys | High | High | Reconcile both artifacts to current available acquisition keys on every run, including zero-worker paths. |
| Raw/reloaded PK or shuffled row differences cause false write ambiguity | Medium | High | Canonicalize artifact-specific rows, columns, attributes, and stamp PK semantics before equality. |
| Malformed legacy completed rows are silently pruned as retry state | Medium | High | Pin explicit schemas; prune only exact blank/No legacy rows and fail all other malformed completed rows. |
| Exact duplicate report rows inflate diagnostics | Medium | Medium | Deduplicate across all normalized persisted columns on every reconciliation path. |
| Missing inventory invokes interactive menu under automation | High | High | Pin noninteractive failed-result behavior and test that `menu()` is never called. |
| Validate-only or bootstrap failures have no nested/top-level owner | Medium | High | Pin requested-stage failure versus dependency-not-run and add wrapper failures table with exhaustive tests. |
| Folder checks are duplicated with conflicting owners | Medium | Medium | Follow the call-mode ownership matrix and test each folder under standalone and wrapper modes. |
| Result helpers evolve into premature generic infrastructure | Medium | Medium | Keep stage-specific, unexported, colocated helpers; prohibit class/context/registry changes. |
| Expanded test files rely on newer testthat than DESCRIPTION minimum | Low | Medium | Reuse patterns already present; if a genuinely new minimum is required, stop for approval rather than silently changing DESCRIPTION. |
| `.Rbuildignore` excludes narrative articles and Pandoc is unavailable | Certain | Medium | Knit all three articles with installed `knitr` to a temporary directory before roxygen in E6; do not require Pandoc. |

## Out of Scope

- `pipdata_stage_result`, any other S3 result class, or a shared generic result
  constructor.
- `pipeline_context`, `safe_pipeline_step`, `run_pipeline()`, run manifests,
  run IDs, resume tokens, or exact-once semantics.
- Per-survey durable writes, transaction infrastructure, or parallel execution.
- Changes to `dlw_validation_engine()`, `validation_spec.yml`, B1 fixtures, or
  report accumulator architecture.
- Typed validation-report fields or removal of `table_name` parsing.
- Log alias/folder consolidation or generic log-entry snapshots.
- Changes to `pd_process_data()`, `pd_change_report()`, dependency planning, or
  `pd_deflate_pipeline()` beyond the narrow completed-validation input guard.
- Changes to `pipfun`, `pipload`, `dlw`, `stamp`, DESCRIPTION dependencies, or
  package exports without an approved deviation.
- Expanding acquisition/cleaning to ASPIRE or L.
- Unrelated cleanup in legacy helpers, vignettes, tests, or roadmap entries.

## Plan Review Resolution

The first `/cg-plan-review` returned 4 P1, 7 P2, and 1 P3 findings. All are
accepted and incorporated:

| Finding | Resolution in this plan |
|---|---|
| P1.1 stamp rollback assumption | Added temporary-alias fault injection, post-failure reload/content reconciliation, write-fact states, and ambiguous-state blocked stop. |
| P1.2 custom ID cannot reach validation | Added an internal validation core accepting acquisition inventory ID; public validation remains default-signature compatible. |
| P1.3 normal DLW return can be ambiguous | Worker passes exact `filename` and treats `dlw_call_list` as download failure. |
| P1.4 overlapping/incomplete outcomes | Added stage precedence and exhaustive aggregate cross-product matrix. |
| P2.1 checkpoint failures under-specified | Added return validation/reconciliation, skipped/null-version cases, and separate summary/checkpoint messages. |
| P2.2 historical log failures leak | Added positional latest-run segmentation for all counts/details/failure lists. |
| P2.3 public input errors can be swallowed | Added exact pre-catch validation matrix and edge-case tests. |
| P2.4 write facts incomplete | Added six-state write-fact truth table including not-attempted, skipped, reconciled success/failure, and ambiguity. |
| P2.5 phases/logger behavior unpinned | Added canonical phase/event/error vocabulary and no-recursion logger-failure semantics. |
| P2.6 acquisition stale-row policy absent | Added server-authoritative merge matrix for stale/current/five-module/ASPIRE-L rows. |
| P2.7 report drift policy absent | Added required fields/types, compatible additive drift, and fatal incompatible drift before writes. |
| P3.1 docs recommend nonexistent API | Documentation now presents top-level orchestration as future direction and the DLW wrapper as the current supported entry point. |

The independent verification review returned 3 P1 and 4 P2 residual findings.
All are also incorporated:

| Verification finding | Resolution in this plan |
|---|---|
| P1.1 no-candidate merge bypass | Full server-authoritative intended inventory is built and reconciled even with zero workers; catalog-only writes remain `no_work` only when verified. |
| P1.2 reachable `not_run` ambiguity | Added exact wrapper-only shape, validate-only missing-inventory failure, legal pairing rules, and exhaustive aggregate handling. |
| P1.3 `inventory_missing` absent from vocabulary | Added acquisition and validation phases plus `inventory_missing_error`. |
| P2.1 stale latest completion selection | Added unconditional `attempt_start`; reporting chooses latest segment first and completion only within it. |
| P2.2 retained utility writes unreconciled | `dlw_gmd_new(update_inventory=TRUE)` and `dlw_gmd_list()` now require reconciliation; direct calls abort when intended state is unverified. |
| P2.3 post-setup wrapper failure owner absent | Added top-level wrapper failure table and critical bootstrap outcome rules. |
| P2.4 retry-disabled unresolved row absent | Added explicit merge-matrix retention of current five-module `No` rows without attempt. |

The final verification review returned 1 P1 and 2 P2 contract ambiguities. All
are incorporated:

| Final verification finding | Resolution in this plan |
|---|---|
| P1.1 absent/not-reached write facts incomplete | Added permitted/required absence, known/unknown not-reached states, returned inventory semantics, and checkpoint `trustworthy`. |
| P2.1 completion logmeta undefined | Added exact completion fields, types, vector uniqueness/disjointness, arithmetic invariants, emission/omission rules, and fallback criteria. |
| P2.2 folder failure ownership conflicted | Added call-mode ownership matrix, stage folder phases, wrapper skip behavior for both-disabled, and per-folder tests. |

The handoff-gate review returned one final P2 and one P3. Both are incorporated:

| Handoff finding | Resolution in this plan |
|---|---|
| P2.1 no-work report fact unpinned | Validation always inspects prior report before no-work; added full inventory/report state matrix and legacy inconsistency failure. |
| P3.1 malformed completion IDs/extra fields | Completion now requires exact fields/constants and non-missing, nonempty, unique, disjoint survey IDs; producer/parser rejection tests added. |

The seal review returned one P1 and three P2 findings. All are incorporated:

| Seal finding | Resolution in this plan |
|---|---|
| P1.1 validate-only menu ownership conflict | Menu is scoped to acquisition-requested wrapper calls; validate-only never invokes it and validation owns missing inventory. |
| P2.1 retry-only absent report rejected | Initially permitted canonical retry-only state; the later fresh review superseded this by removing execution-failure rows from `gmd_valid_inv` and pruning legacy retry rows. |
| P2.2 completion counts for unattempted candidates | Validation totals now mean terminal worker outcomes; pre-map failures use zero survey counts plus workflow phase. |
| P2.3 evidence/base gate incomplete | Added current-origin synchronization, recorded BASE_SHA, direct Rmd rendering, executable focused commands, and tracked/untracked allowlist gate. |

The definitive review returned one P1 and one P2. Both are incorporated:

| Definitive finding | Resolution in this plan |
|---|---|
| P1.1 Pandoc unavailable for required evidence | E6 now preflights and directly knits all excluded articles with installed `knitr` before roxygen; no Pandoc dependency remains. |
| P2.1 utility manuals omitted | Step 13, checklist, boundaries, and E9 now include all four exported `dlw_gmd_*` `.Rd` files. |

A later fresh review against the synchronized dependency-planning source found
4 P1 and 4 P2 issues. All are incorporated:

| Fresh-review finding | Resolution in this plan |
|---|---|
| P1.1 changed checksum can read stale local pin | Acquisition passes exact filename plus `local_overwrite = TRUE` and rejects cached/ambiguous success. |
| P1.2 retry rows enter cleaning | Execution failures are no longer persisted in `gmd_valid_inv`; completed valid/invalid rows remain the only cleaning handoff. |
| P1.3 validation state not acquisition-authoritative | Validation/report are pruned to current available acquisition keys on every run, including zero-worker paths. |
| P1.4 report/inventory mismatch accepted | Added exact all-path key invariant, repairable orphan removal, and blocking missing-completed diagnostics. |
| P2.1 durable equality unspecified | Added artifact-specific canonicalization and one narrow DLW persistence reconciler. |
| P2.2 acquisition logs remain historical | Added acquisition attempt boundaries, exact completion schema, and independent latest-attempt segmentation. |
| P2.3 schema/consistency phases missing | Added `catalog_schema`, `inventory_schema`, `report_consistency`, and deterministic legacy duplicate rules. |
| P2.4 acquisition/wrapper logmeta undefined | Added exact acquisition completion and scalar wrapper summary schemas with malformed-entry tests. |

The subsequent verification returned 2 P1, 3 P2, and 1 P3 findings. All are
incorporated:

| Verification finding | Resolution in this plan |
|---|---|
| P1.1 legacy failure rows can clean before migration | Added one completed-validation guard at process, internal execution preparation, and change-report entry points. |
| P1.2 active checksum uniqueness assumed | Added normalized filename uniqueness and fatal multiple-checksum catalog schema policy. |
| P2.1 inventory schemas incomplete | Added explicit acquisition/completed-validation columns, types, key/null/status rules, safe coercion, and legacy-row distinction. |
| P2.2 changed-checksum version ambiguous | Historical per-survey max is captured before pruning; next successful checksum increments it and failures consume nothing. |
| P2.3 generic report sections retain stale DLW | Dedicated segmented DLW sections own all DLW discriminators; generic country/type sections exclude them. |
| P3.1 report duplicate identity undefined | Exact normalized full-row duplicates are deterministically removed; distinct persisted rows remain separate checks. |

The verification review after these fresh fixes returned **No significant
issues found**.

## Final Evidence Commands

`/cg-work` records all command output in its execution report. Commands use the
synchronized `BASE_SHA` recorded in Phase 1.

**E0 - synchronization and base gate**

```powershell
git fetch --prune origin
git merge-base --is-ancestor origin/PROD HEAD
git rev-parse HEAD
```

The second command must exit zero before edits. The third command becomes the
recorded `BASE_SHA` after the source-drift audit.

**E1 - contract/preflight tests**

```powershell
Rscript -e "devtools::test(filter = 'pipdata_dlw_compare|dlw-unified-logging')"
```

**E2 - acquisition tests**

```powershell
Rscript -e "devtools::test(filter = 'pipdata_(get_gmd|dlw_compare)|dlw-unified-logging')"
```

**E3 - validation tests**

```powershell
Rscript -e "devtools::test(filter = 'pipdata_validate_gmd|pipdata_dlw_compare|dlw-unified-logging|dependency-execution|pd_process_data|pd-change-report')"
```

**E4 - wrapper and report tests**

```powershell
Rscript -e "devtools::test(filter = 'pipdata_dlw_process|dlw-unified-logging|log_report')"
```

**E5 - unchanged B1 validation tests**

```powershell
Rscript -e "devtools::test(filter = 'dlw_validation_(engine|spec)')"
```

**E6 - directly excluded vignette articles, then roxygen**

```powershell
Rscript -e "stopifnot(requireNamespace('knitr', quietly = TRUE)); local({ out <- tempfile('pipdata-vignettes-'); dir.create(out); on.exit(unlink(out, recursive = TRUE), add = TRUE); inputs <- file.path('vignettes', 'articles', c('Validating-Data.Rmd', 'PIP-data-pipeline.Rmd', 'Processing-Data.Rmd')); outputs <- vapply(inputs, function(x) knitr::knit(x, output = file.path(out, paste0(basename(x), '.md')), quiet = TRUE, envir = new.env(parent = globalenv())), character(1)); stopifnot(all(file.exists(outputs))) })"
Rscript -e "devtools::document()"
```

**E7 - full test suite**

```powershell
Rscript -e "devtools::test()"
```

**E8 - package check**

```powershell
Rscript -e "devtools::check(error_on = 'never')"
```

Compare E8 ERROR/WARNING counts and messages with the E8 baseline recorded at
the synchronized `BASE_SHA`; no new ERROR or WARNING is allowed.

**E9 - tracked and untracked path allowlist**

Replace `<BASE_SHA>` with the recorded exact hash:

```powershell
Rscript -e "base <- commandArgs(TRUE)[1]; if (is.na(base) || !nzchar(base)) stop('BASE_SHA is required'); tracked <- system2('git', c('diff', '--name-only', base, '--'), stdout = TRUE); untracked <- system2('git', c('ls-files', '--others', '--exclude-standard'), stdout = TRUE); paths <- unique(c(tracked, untracked)); allowed <- c('^R/(pipdata_(dlw_process|get_gmd|validate_gmd|dlw_compare)|log_report|dependency_execution|pd_process_data|pd_change_report)\\.R$', '^tests/testthat/test-(pipdata_(dlw_compare|get_gmd|validate_gmd|dlw_process)|dlw-unified-logging|log_report|dependency-execution|pd_process_data|pd-change-report)\\.R$', '^NEWS\\.md$', '^vignettes/articles/(Validating-Data|PIP-data-pipeline|Processing-Data)\\.Rmd$', '^man/(pipdata_get_gmd|pipdata_validate_gmd|pipdata_dlw_process|dlw_gmd_(match|new|unvalidated|list)|log_report|pd_process_data|pd_change_report)\\.Rd$', '^\\.cg-docs/brainstorms/2026-08-24-dlw-wrapper-rewrite\\.md$', '^\\.cg-docs/plans/2026-08-25-dlw-wrapper-rewrite\\.md$', '^\\.cg-docs/work-reports/[0-9]{4}-[0-9]{2}-[0-9]{2}-dlw-wrapper-rewrite(-[0-9]+)?\\.md$', '^\\.cg-docs/active-state/current\\.json$', '^roadmap\\.json$'); bad <- paths[!vapply(paths, function(x) any(vapply(allowed, grepl, logical(1), x = x)), logical(1))]; if (length(bad)) stop(paste('Paths outside allowlist:', paste(bad, collapse = ', ')))" <BASE_SHA>
```

## Completion Contract

### Outcome

The three exported DLW functions preserve their public signatures and persisted
artifact/log contracts while invisibly returning explicit stage results.
Acquisition and validation isolate survey failures, retry unresolved inventory
rows, distinguish invalid data from execution failure, and expose deterministic
outcomes that Stream C can later adapt without a generic B3 result class.

### Verification Surface

| ID | Phase | Evidence Required | Command/Artifact | Required |
|---|---:|---|---|---|
| V1 | 1 | Current origin is contained; synchronized BASE_SHA/baseline are recorded; result/failure/write-fact/API contracts and fault-injected durable states are executable | E0 and E1 plus temporary-alias fault-injection evidence in work report | yes |
| V2 | 2 | Acquisition covers one checksum per normalized filename, exact filename plus forced overwrite, cached/ambiguous rejection, catalog-only writes, canonical merge/equality, exact attempt/completion logs, interrupts, and reconciliation | E2 | yes |
| V3 | 3 | Validation covers explicit schemas, pre-pruning historical versions, authoritative key pruning, completed-only inventory, legacy cleaning guards, exact report coverage/dedup, schema phases, zero-worker repair, reconciliation, and interrupts | E3 | yes |
| V4 | 4 | Wrapper exact summary/checkpoint contracts and whole rendered report contain only latest-attempt DLW details, with DLW excluded from generic sections | E4 | yes |
| V5 | 4 | B1 engine/spec behavior and golden fixtures remain unchanged | E5 | yes |
| V6 | 5 | All three `.Rbuildignore`-excluded articles knit successfully before roxygen, and generated wrapper/utility manuals match accurate contracts | E6 | yes |
| V7 | final | Entire test suite passes without introduced order dependence | E7 | yes |
| V8 | final | Package check has no new ERROR or WARNING relative to synchronized baseline | E8 and baseline comparison in work report | yes |
| V9 | final | Tracked plus untracked paths relative to BASE_SHA match the explicit allowlist, including required workflow artifacts | E9 | yes |

### Constraints

| ID | Phase | Constraint | Check |
|---|---:|---|---|
| C1 | all | Public signatures, defaults, order, and exports remain unchanged | Formals/export tests |
| C2 | all | Existing aliases, artifact IDs, PKs, checkpoint alias, log discriminators, and report-consumed phases remain stable | Contract tests |
| C3 | 2 | Acquisition downloads five modules; exact filename and forced overwrite are used; full catalog merge runs on no-work; custom ID reaches validation | Candidate, worker, wrapper, and artifact tests |
| C4 | 3 | Invalid data is not failed execution; execution failures create no validation inventory/report row and retry by absence | Validation, persistence, and cleaning-handoff tests |
| C5 | 3 | Report exactly covers completed validation inventory, both are acquisition-authoritative, report is verified before inventory, and every uncertain write is reconciled | Key-invariant, pruning, fault-injection, equality, and write-order tests |
| C6 | all | No returned object contains an R condition or survey-sized data object beyond documented inventory | Recursive contract tests |
| C7 | all | User cancellation and interrupts propagate; runtime errors alone are converted | Condition-class tests |
| C8 | all | No generic stage class, shared constructor, context, run ID, resume token, or top-level orchestrator is introduced | Boundary diff review |
| C9 | all | B1 engine/spec, `aaa.R`, `DESCRIPTION`, and `NAMESPACE` remain unchanged unless deviation is approved | Executed path-specific diff checks |
| C10 | all | Input inventories are copied before `data.table` reference mutation | Non-mutation tests |
| C11 | all | Public input errors abort before runtime catches; cancellation and interrupts propagate | Argument and condition-class tests |
| C12 | 3 | Validation report allows compatible additive drift only and blocks all incompatible drift before writes | Report schema tests |
| C13 | 4 | Current acquisition and validation report details/counts each come from one latest positional attempt segment | Both-stage failed-first/success-second and reverse-order tests |
| C14 | 4 | Wrapper-only `not_run` results contain `summary$reason`; requested validation owns its own missing prerequisite as failed | Exact shape and validate-only tests |
| C15 | 4 | Post-setup alias/folder/directory/bootstrap errors have one top-level wrapper failure owner | Wrapper infrastructure-failure tests |
| C16 | 2 | Retained public acquisition utility writes reconcile uncertain durable state and never silently succeed | Direct utility fault/invalid-return tests |
| C17 | all | Confirmed absence, required-not-reached, and unreadable states use exact write facts and NULL/prior values | Constructor and failure-path tests |
| C18 | 3 | Validation completion entries satisfy exact field/type/vector/arithmetic invariants | Emitter and parser validity tests |
| C19 | all | Folder failures have one owner and a canonical phase for every call mode/folder | Standalone/wrapper per-folder tests |
| C20 | 3 | Validation no-work never returns an uninspected/fabricated report fact and fails on legacy inventory/report inconsistency | No-work state-matrix tests |
| C21 | 3 | Completion metadata has exact fields/constants and only nonmissing, nonempty, unique, disjoint survey IDs | Producer/parser malformed-schema tests |
| C22 | 1 | Implementation begins only after current origin/PROD is contained and exact BASE_SHA/baseline are recorded | E0 and work-report evidence |
| C23 | 5 | All three excluded vignette articles knit directly in a clean temporary output directory before documentation mutation | E6 |
| C24 | 3 | Validation totals count terminal worker outcomes only; pre-map failures use zero survey counts plus workflow phase | Pre-map failure completion tests |
| C25 | 4 | Validate-only missing inventory never invokes menu in interactive or noninteractive mode | Wrapper menu-guard tests |
| C26 | 3 | Execution-failed IDs are absent from trustworthy `gmd_valid_inv`, and legacy control rows are filtered at process/change-report/planning entries | Validation inventory and direct consumer-handoff assertions |
| C27 | 3 | Validation/report state contains only current available acquisition keys; deleted/superseded/failed-replacement keys are pruned even with zero workers | Authoritative state reconciliation tests |
| C28 | all | Durable equality uses artifact-specific canonical row/column/attribute/PK semantics | Shuffled catalog and raw/reloaded-PK tests |
| C29 | 3 | Validation schema/consistency failures and legacy duplicate conflicts use exact canonical phases/error types | Exact error contract tests |
| C30 | 2 | Acquisition completion uses the exact schema and malformed payloads are rejected | Acquisition producer/consumer contract tests |
| C31 | 4 | Wrapper summary preserves four existing fields and uses the exact appended scalar schema | Wrapper producer/consumer contract tests |
| C32 | 2 | Current acquisition has exactly one checksum per normalized filename/survey ID before mapping | Same-filename/different-checksum catalog-schema test |
| C33 | 3 | Acquisition and validation inventories satisfy explicit type/key/status/parsed-field schemas | Safe-coercion and fatal malformed-row tests |
| C34 | 3 | New checksum versions use pre-pruning completed per-survey historical maximum plus one | Version-5 to version-6 and failed-retry tests |
| C35 | 4 | DLW discriminators appear only in dedicated segmented report sections, never generic country/type sections | Full rendered report stale-ID assertions |
| C36 | 3 | Exact normalized validation-report duplicates are removed on no-work and worker paths | Report deduplication tests |

### Boundaries

- **Allowed**: `R/pipdata_dlw_process.R`, `R/pipdata_get_gmd.R`,
  `R/pipdata_validate_gmd.R`, `R/pipdata_dlw_compare.R`, `R/log_report.R`,
  `R/dependency_execution.R`, `R/pd_process_data.R`, and
  `R/pd_change_report.R` for the narrow completed-validation guard only.
- **Allowed**: focused new tests, existing unified logging/report tests,
  `NEWS.md`, three pipeline vignettes, generated wrapper/report `.Rd` files,
  and generated `man/dlw_gmd_match.Rd`, `man/dlw_gmd_new.Rd`,
  `man/dlw_gmd_unvalidated.Rd`, `man/dlw_gmd_list.Rd`,
  `man/pd_process_data.Rd`, and `man/pd_change_report.Rd`.
- **Allowed**: unexported stage-specific helpers colocated with their owning
  source files.
- **Out of scope**: B1 validation rules/spec changes, generic stage
  infrastructure, Stream C context/orchestration, exact-once resume,
  parallelism, typed validation-report fields, log-folder changes, external
  package changes, cleaning/deflation result redesign, or dependency-planning
  changes beyond completed-validation input filtering.

### Iteration Policy

1. Synchronize with current origin/PROD, re-audit source drift, record BASE_SHA,
   then verify dependencies and baseline before production edits.
2. Establish executable compatibility contracts before replacing active paths.
3. Complete and verify acquisition before starting validation.
4. Reconcile validation/report state to current acquisition keys, enforce exact
   coverage, then persist report first and completed inventory last; never
   assume rollback.
5. Preserve B2 discriminators and use a completion summary plus legacy fallback
   for valid/invalid/execution-failed reporting; segment from the latest attempt
   boundary before selecting a completion.
6. Treat checkpoint failure as a separate observability fact; do not rewrite
   completed business outcome.
7. Under `deviation-policy: ask`, pause before external package changes, new
   exports/dependencies, or changes to protected B1/C3 boundaries.

### Blocked-Stop Conditions

- Required missing-versus-corrupt artifact states cannot be distinguished with
  current APIs without changing `pipload`.
- Current origin/PROD is not an ancestor of HEAD, synchronization is not
  approved/completed, or upstream source drift invalidates pinned contracts.
- Custom `inv_gmd_list` cannot be honored consistently without an external API
  change.
- Retained direct utility/bootstrap writes cannot be reconciled against durable
  prior/intended state with existing APIs.
- Post-failure durable content cannot be reloaded and distinguished as prior,
  intended, or ambiguous with current stamp/pipload semantics.
- Report-first/inventory-last safety cannot be implemented with reconciliation
  against current stamp semantics.
- The B1 report accumulator cannot be safely reset without modifying B1
  behavior.
- Preventing execution-failure control rows from entering `gmd_valid_inv`
  requires a cleaning consumer change rather than completed-only persistence.
- Runtime error conversion would require catching user interrupts or explicit
  cancellations.
- Wrapper post-setup alias/folder/bootstrap failures cannot be represented
  without violating the pinned aggregate shape.
- A required change crosses into `pipfun`, `pipload`, `dlw`, `stamp`, B1
  validation rules, or Stream C's generic interface.
- Required phase or final evidence fails after allowed recovery attempts.
- Worktree drift conflicts with the planned public or persistence contracts.
- `/cg-work` cannot durably create/update its execution report or satisfy the
  default blocked-stop rules in `.kilo/shared/goal-execution.contract.md`.
