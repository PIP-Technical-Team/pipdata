---
date: 2026-10-27
title: "M2 Pipdata Stamp Authority: Bounded Engine Follow-up"
status: completed
scope: "Deep"
brainstorm: ".cg-docs/brainstorms/2026-08-27-executable-staged-invalidation-dag.md"
language: "R"
estimated-effort: "large"
deviation-policy: "ask"
artifact-schema-version: 1
phases: 2
execution-report: ".cg-docs/work-reports/2026-10-27-m2-pipdata-authority.md"
failing-steps: []
completed-phases: [1, 2]
tags: [m2, pipdata, stamp, authority, recipes, fingerprints, receipts, test-safety]
---

# Plan: M2 Pipdata Stamp Authority

## Objective

Prepare a bounded opt-in engine route in pipdata that supplies recipes, exact
inputs and automatic fingerprints while stamp alone selects and revalidates
run/skip/blocked work. Establish the pipdata contribution to umbrella Step 3
and L-authority/V6 using synthetic producers and actual stamp/pipload storage,
without requiring a survey, mean or TEST manifest.

Originally task `m2-engine-local-plan`, revised by `m2-engine-work-readiness`;
milestone `m2-walking-skeleton`, feature
`create-engine-skeleton`, pipdata contribution only. Status active describes
the saved Plan, not an active implementation phase. This is a concrete work
specification. A direct `/cg-work` request selects its scope and decisions; do
not require a second approval bundle or another session. This document-only
revision itself does not start work or authorize Git mutations.

## Context

### Approval And Local Bearings

- Direct user choices: `Authorize report exception`,
  `Follow-up (Recommended)`, then `Approve save and validation`. The last choice
  originally approved saving/validation only. Subsequent requests authorized all
  Plan-review corrections, then on 2026-10-27T18:34:48Z explicitly required the
  outer caller to settle work decisions, scope and test strategy here instead
  of deferring generic approvals. Historical review/checkpoint evidence remains
  below and in the original reports. No package work runs during this revision.
- Actual approval/save day is October 27. The October 26 filename is retained
  because it is the exact assigned output, not a claim of October 26 creation.
- Root: `E:/PovcalNet/01.personal/wb384996/PIP/pipdata`; verified clean `PROD`
  at `84442e979c98d33fa5565ab9e56d3179cc7d5278` before planning. Default ref
  `origin/PROD`. No branch mutation is authorized now. Future work needs a
  named E work branch and normal ownership check, not a new package. Creating
  `feat/m2-pipdata-authority` remains a separately named Git operation in the
  eventual work instruction; `/cg-work` alone is not branch-creation permission.
- Charter objective is PIP validation/cleaning and R package improvement;
  bounded engine alignment supports it. Root `compound-gpid.local.md` is
  missing; no local syntax/review configuration is inferred. R/data.table
  follows current code and the controlling system design.
- Existing C2/C3/C4 Plans are completed historical inputs, not overwritten:
  `2026-08-24-pipdata-staged-dependency-manifest.md`,
  `2026-08-25-pipeline-stage-interface.md`, and
  `2026-08-27-executable-staged-invalidation-dag.md`. The last has four completed
  phases. Its linked Deep brainstorm provides history, not authority to retain
  pipdata currentness on M2.
- Pipdata worktree listing also contains two prunable historical detached C:
  entries with nonexistent gitdir targets. Do not access, prune or clean them.
  Worker liveness/concurrent ownership remain UNKNOWN.

### Controlling Inputs And Evidence

Use `H = E:/PovcalNet/01.personal/wb384996/PIP/pipsystem` below.

| Input | Relevant authority/evidence |
|---|---|
| `H/SYSTEM_DESIGN.md:184-223,258-311` | Stamp sole authority; automatic calculation fingerprints; feedback data.table; failure/review rules; Force interaction and M1 R1 remain open; one pipdata entry point |
| `H/.cg-docs/plans/2026-10-10-m2-walking-skeleton.md:207-219,278-288,415-429,453,555` | Manifest reconciliation; local ownership/runtime/safe-suite gates; L-authority with neutral producers; umbrella is NOT a /cg-work target |
| `H/.cg-docs/orchestration/outputs/stamp/current-work-checkpoint/20261022T163746Z-stamp-session/{handoff,evidence}.md` | Producer identity distinctions; evidence.md E6 contains public request/receipt/metadata/failure contract |
| `H/.cg-docs/orchestration/outputs/pipload/m2-adapter-final-handoff/20261023T012858Z-pipload-code/{handoff,evidence}.md` | Prior adapter subset, actual contracts, source manifest and runtime/exclusion limits |
| `H/.cg-docs/orchestration/outputs/pipload/m2-adapter-commit-push/20261023T212536Z-pipload-code/{handoff,evidence}.md` | Delivered commit and exact CRLF-only content binding; supersedes earlier no-publication checkpoint, not earlier test limits |

Original reporting owner: `pipdata-engine-planning Code session`. Historical
reports, read back during planning and readiness correction, are under
`H/.cg-docs/orchestration/outputs/pipdata/m2-engine-local-plan/20261026T220603Z-code-plan/`:
`handoff.md` and `evidence.md`. They contain sanitized self-contained research;
they do not certify runtime behavior. Native/request/coordinator IDs UNKNOWN.

Readiness reporting owner is the existing pipdata Code session. This revision's
only reporting outputs are
`H/.cg-docs/orchestration/outputs/pipdata/m2-engine-work-readiness/20261027T180338Z-code-plan-fix/{handoff,evidence}.md`.
The starting Plan no-filter blob was independently reverified as
`c473b2219b6f0f4ecb77388a0d7e99e64cf60fc1`, not assumed equivalent. The outer
caller alone edits this Plan/reports; installed review and critic are read-only.

### Distinct Pins

| Role | Identity | Qualification |
|---|---|---|
| Pipdata observed baseline | `84442e979c98d33fa5565ab9e56d3179cc7d5278` | Fresh local HEAD; no package test run |
| Stamp producer content | `5d1ddebdc926dad9c6ec99c08a65e74baee0da92` | Fixed prospective producer source; historical tested-content evidence, not clean execution-time tested commit |
| Stamp current observed HEAD | `235a18a58eabe15d5c98e7e4ee0e582eac60b4f5` | Fresh local rev-parse; do not substitute for producer content |
| Pipload delivered commit | `4ef0d90a974cb6ff0725da69373bde421dd04ad5` | Fresh local delivered-worktree HEAD; no new tests/PROD integration/CI claim |
| Pipload prior tested manifest | Git blob `7f2edb317a260fa2a7331646a3e49fc81892e51c`; SHA-256 `96a2c872ddde2ed7f2c144749529d51dbeb14024ce745b99f28204b948412d1b` | Manifest identity, NOT tested commit; supplied delivery evidence binds 91 raw files and 16 committed blobs through CRLF-only normalization |

Fresh source status: stamp has modified `.gitignore`, `compound-gpid.local.md`
and untracked `.compound-gpid/active-manifest.json`; delivered pipload has only
untracked `.cg-docs/active-state/current.json`. Do not modify these. Source/help
inspection does not prove an installed library matches the pin.

### Verified Source Seams

| Concern | Current source | Required treatment |
|---|---|---|
| Public route | `R/pd_run_pipeline.R:342-428` | Existing route calls C2 selector and returns classed pipeline result; use explicit opt-in, not silent replacement |
| Duplicate selection | `R/dependency_execution.R:559-727`; `R/dependency_plan.R:76-155,212-228` | Neither hash comparisons, force actions nor manifest absence can select work on M2 |
| Fingerprints | `R/code_fingerprint.R:1-106`; `R/dependency_contract.R:257-259` | Reuse canonical-function/curated-closure patterns; old xxhash64 serialized-v3 hashes are NOT stamp siphash13 descriptors |
| Save seam | `R/save_pip.R:91-148` | Existing local artifact receipts are not stamp execution receipts; do not forge or reconstruct modern success from catalog rows |
| Persistent safeguards | `R/dependency_manifest.R:22-46,53-89,142-180`; `R/dependency_execution.R:875-967` | Preserve immutable/checksummed publication, lease ownership and context/code/parent fences where applicable; no second scheduler |
| Return/schema | `R/pipeline_context.R:23-95`; `R/pipeline_run_result.R:1-66`; `R/dependency_contract.R:1-59` | Fixed clean/metadata/deflate schemas cannot silently accept generic M2 nodes or new returns |
| Existing Force | `R/pd_run_pipeline.R:458-473` | Timestamp versioning is not a stamp-approved Force execution mechanism |
| Hooks | `R/zzz.R:2-17`; pipfun `R/zzz.R:9-33` | User-cache default/log initialization and dependency auto-logging require pre-load E controls |
| Compatibility | `tests/testthat/test-dependency-api-contract.R:1-109` | Exact formals/export assertions need an explicit additive change, with old positions/defaults preserved |

Brain findings retained: result-bound provenance/checkpoints
(`.cg-docs/solutions/data-quality/2026-08-25-authoritative-staged-provenance-checkpoints.md`),
fail-closed complete accounting
(`.cg-docs/solutions/bugs/2026-08-26-fail-closed-typed-stage-orchestration.md`),
and built-package dependency evidence
(`.cg-docs/solutions/testing-patterns/2026-08-21-built-package-and-checkpoint-verification.md`).
The September 1 durable-invalidation lesson warns against resurrecting old
artifacts after failure; transfer that invariant to stamp outcomes, not its
historical manifest-as-selector implementation. Manual Brain query used because
helpers are prohibited in this planning task.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Stamp alone selects/revalidates all M2 work; no legacy selector override | System design sections 6/10; umbrella reconciliation |
| R2 | Supply complete exact requests, automatic truthful component hashes, typed semantic metadata and explicit formats | Producer evidence E6; stamp public request contract |
| R3 | Preserve real execution receipts, retained-output convergence, governing failures and immutable exact-pin history | Producer rebuild/execution-info contract |
| R4 | Preserve old argument positions/defaults and legacy returns; append only the specified opt-in recipes argument; no new package/circular hard dependency | Invocation; umbrella Step 3; decision E1 |
| R5 | Return the specified compact data.table feedback with attempted failures and blocked work; logs never select work | System design section 7; decision E1 |
| R6 | Preserve legacy safeguards unchanged; implement explicit modern lease/context/proof guards and serial validation; reject invalid results without claiming global write rollback/cancellation | Existing safeguards; system design section 7; decision E3 |
| R7 | Establish L-authority with synthetic producers and real stamp/pipload, independent of later survey/mean/release stages | Umbrella Step 3/V6 |
| R8 | Execute the exact source/test/docs/lifecycle/report/runtime scope and phase-specific safe substitute; verify branch, ownership and runtime as normal preflight | Current readiness instruction; umbrella D8; decisions E4-E6 |
| R9 | Keep Force support, M1 R1, production/SMB activation and broader pipeline claims unresolved/out of scope | Invocation; producer explicitly lacks Force API |

## Execution Decisions

These are settled choices for the bounded contribution, not a pre-start approval
checklist. An explicit `/cg-work` request authorizes this specified work under
its permissions; no duplicate API/mode/scope/test/pin acceptance is required.
Normal preflight verifies facts and stops on actual failure, denied action or
scope conflict. Material deviations still use deviation-policy ask. Branch
creation must additionally be explicitly named, as shown in Work Handoff.

| ID | Decision | Execution consequence |
|---|---|---|
| E1 | Append only `recipes = NULL` to pd_run_pipeline; NULL preserves the old route/return; non-NULL uses the complete envelope/feedback contract below. No new export. | Update the two exact-formals assertions to append recipes while asserting all eight old positions/defaults unchanged; behavior-test both routes. |
| E2 | This local slice is incremental only. Reject force/force_surveys and incompatible legacy controls before loads/leases/writes. | No Force emulation or implicit failed-work override. Full umbrella Force remains incomplete and outside this contribution; M1 R1 is not decided. |
| E3 | Use the fixed producer's serial, node-local failure/receipt policy plus pipdata integrity guards and final proof validation, exactly as detailed below. | Failed nodes/descendants are not successful; independent work may continue. No global no-later-write/rollback claim. Integrity failure raises a typed run error with actual feedback, never a normal success table or pipeline checkpoint. |
| E4 | W is the existing E pipdata checkout on named future branch feat/m2-pipdata-authority; owner is this pipdata Code session; exact file/lifecycle/central/runtime allowlists below. No worktree or parallel worker. | Direct work instruction includes branch creation if needed; ownership/collision checks are facts, not another general approval bundle. |
| E5 | Use the specified Phase-1 two-file baseline/storage smoke and Phase-2 three-file candidate/engine smoke as this bounded task's selected full-suite substitute, with all 41 excluded files visible. | The direct work request adopts this task-specific workflow exception; record its rationale/exclusions in the execution report. Never call it a full package/commit gate, waive a failed selected check or infer production coverage. |
| E6 | Export stamp at fixed content pin and pipload at delivered commit into S, install those plus the candidate into S/library; use the exact installed dependency versions listed below as read-only inputs. | Source/archive/install/loaded-content checks are ordinary preflight. The global stamp/pipload installations are not substitutes. A mismatch/missing dependency stops; no automatic update/download. |

### Envelope And Feedback

`recipes` is a plain named list with exactly `context` and `nodes`.
`context` has exactly `release`, `identity`, `root`: nonempty scalar character
release; identity exactly `TEST` for this bounded slice; root a resolved absolute
E path for one initialized isolated stamp repository. Root/alias selection is
explicit setup, not inferred from production options. Do not initialize storage
inside a calculation or fall back to a main_dir-derived production alias.

`nodes` is an unnamed list. Each record has exactly `path`, `parents`,
`calculation`, `helpers`, `constants`, `semantic_schema`, `semantic`, `format`,
and optional `audit`, `review`. Paths identify unique outputs under that root;
parents use producer path/version records. Calculation is a function with
`(path, parents)` formals returning the stamp builder contract; helpers are
uniquely named actual functions (including active S3 methods/external functions
when supported by the binding rules below), constants are uniquely named actual
rule/configuration values. Empty helpers/constants are ordinary list() values;
nonempty lists require unique nonempty names, disjoint across helper/constant
bindings and from callback formal names. Required component
IDs are generated as `calculation`, `helper:<name>` and `constant:<name>`.
Hashes are computed automatically, never supplied as manual source/version
labels. Builders consume exact supplied parents through public pinned reads,
return values and metadata, and do NOT save their own output.

The bounded slice accepts trusted, reviewed ordinary R closures, not arbitrary
user code. Reject primitive calculations/helpers before planning: distinct
primitives can have identical NULL formals/body. Use an ordinary R wrapper whose
body names the operation instead. Base operators/functions reached through the
base environment are covered by the fixed R runtime identity, not mislabeled
as distinct declared primitive components.

Create private copies of calculation/helpers with source references removed,
rebinding their environments to one fresh proof environment whose parent is
baseenv(). Bind only the declared helper copies and deep-copied immutable
constant values, then lock its bindings/environment. Hash exactly these copies
and values. Callbacks receive only explicit path/resolved parents; no original
mutable capture environment is used. Every non-base free value/function must
resolve to that declared environment or an explicitly reviewed qualified public
read API. Reject undeclared free names, mutable/opaque values, arbitrary namespace
closures requiring hidden state, dynamic get/eval/parse/reflection/native code,
global assignments and run-state option/environment dependencies. Do not
silently rebind an incompatible existing processing function: stop for a later
approved adapter/closure contract. No general dependency discovery is promised.

Permitted adapter reads are explicit-version pipload data/sidecar and public
stamp proof reads; `rlang::abort` with the two documented stamp failure classes
is also allowed for classified calculation failures (not namespace mutation).
Apart from those reads/condition signaling, calculations/helpers are pure: no
writing their output or other artifacts, modifying parents, aliases/options,
leases, proof bindings, other nodes or storage configuration. This is a trusted
callback contract and reviewed declaration check, not an R security sandbox.
Recompute declared proof and check alias/root/context/options invariants before
and after each callback. A violation latches the guard described below; do not
return that callback's x to stamp. If an invariant changes outside the declared
surface, reject the unsupported dependency rather than claim detection coverage.

For non-NULL recipes reject nondefault legacy controls (`inv`, forcing,
bootstrap/selectors or nondefault checkpoint settings) before loads/leases/writes;
`verbose` still controls diagnostics. The modern route branches before any
legacy inventory/auxiliary load or C2 preparation. Empty nodes return a typed
zero-row feedback table with no planner/storage mutation. Invalid configuration
or cancellation/interrupt propagates; selected node failures follow stamp.

Feedback columns in this order: `release_id`, `path`,
`preview_decision`, `status`, `reason`, `version_id`, `request_id`, `receipt_id`,
`outcome_id`, `failure_class`, `governing_failure_id`, `message` (character),
`attempted` (logical). Use NA_character_
for absent IDs, never fabricate usable pins. Preserve actual stamp statuses;
preview_decision is explicitly the preview, not a claim that revalidation cannot
change work. Derive attempted from actual calculation callback entry after its
entry guard, including calculation failure, not dispatcher entry denied by a
guard, preview run or guessed status. One row per selected node,
including skips/blocked work; no payloads or closures in the returned table.
Totals are derivable; persistent records remain stamp-owned, logging compact
and separate. Do not retrofit this table onto legacy return objects.

After rebuild, resolve each nonempty returned outcome_id with exact public
st_execution_info lookup. Preserve structured failure.class, not parsed message
text. For a blocked outcome, follow its public previous_id links for that same
artifact until the governing non-blocked actual execution is found; expose its
ID in governing_failure_id only when it is a validated actual failure. Use a
visited-ID set and maximum 1000 exact lookups per artifact, aborting
on cycle, malformed envelope, cross-artifact link or missing exact record. Do not
substitute latest or an earlier success when proof is invalid. No failure means
NA governing ID. Keep the latest observation outcome_id separate: the review
request references governing_failure_id, which can survive multiple later
blocked observations and a new process. Reaching the bound fails clearly;
it never truncates into a guessed governing failure.
An unrecordable blocked/invalid template may legitimately have no outcome_id;
keep it NA with stamp's actual reason and no invented failure class/history.
A built/unchanged/skipped row without required output/receipt proof is instead
an integrity error, not a usable pin. Missing an explicitly returned ID is never
treated as the same as no ID being returned.

### Modern Safety Ownership

For this one-root TEST slice define a plain internal context containing
schema=1L, explicit release/identity, canonical root and recipe proof digest.
The lease scope excludes changing recipe proof: compute scope_id as SHA-256 of
serialized-v3 list(schema=1L, release, identity, canonical_root), so competing
recipes for the same release/root cannot obtain different leases. Use exact
lease base `context$root/.pipdata-engine-state` (under S/stores in tests), with
the unchanged pd_lease_acquire(context, root=lease_base) primitive producing
`dependency-manifest/<scope_id>/writer.lease/owner.rds`. This directory is only
ownership state, not a modern calculation manifest. The existing helper's
directory name does not grant its old planner any authority.

Acquire before calling stamp planning/rebuild; assert token/root/context and
frozen proof before planning, immediately before rebuild, at callback entry and
exit, and after rebuild. Release through ownership-checked on.exit cleanup.
Never recover a held/lost lease automatically. Persist no modern C2 records,
inventories or checkpoint state. Implement small internal context/guard logic
in allowed R/dependency_execution.R; do NOT call legacy pd_dependency_context,
pd_assert_execution_fence, pd_assert_execution_refresh_fence,
pd_revalidate_snapshot or manifest publication. Legacy immutable/checksummed
manifest publication remains unchanged and is inapplicable to modern results
because no pipdata calculation manifest is published. Stamp owns exact parent
revalidation, serial save/proof validation and receipt/outcome publication.

A pre-call ownership/configuration/proof failure stops before stamp invocation.
During rebuild, latch any observed guard/integrity failure in private run state:
that callback returns no x, and later callback entries perform no value
calculation. Stamp's fixed public API catches ordinary callback/publication
errors and can continue graph processing, recording failed/blocked outcomes or
attempting unrelated publication; pipdata cannot promise to prevent those
writes or interrupt stamp's internal save boundary. Do not fake cancellation,
split/reorder the complete plan, or classify this as an ordinary retryable
external failure. Verify every returned success/skip with its exact receipt and
same-pin metadata. If the guard latched, a publication_failed/invalid_state
result appears, or required exact proof is unreadable/inconsistent, raise
`pipdata_engine_integrity_error` after unwind with a `feedback` field containing
actual attempted/blocked/status facts, and a structured cause. Do not return a
normal success table or publish any pipdata checkpoint. Previously valid stamp
records remain actual historical facts, not a whole-run success. Preflight for
a later analyst-started call must re-establish integrity; no retry loop or
automatic repair. Never forge a stamp outcome on an invalid effective request.

This choice follows SYSTEM_DESIGN.md:209-215 (boundary checks and continuation
after individual failures) and umbrella Plan:205 (serial rejection of invalid
pinned state, no transactional/multiwriter claim). The original stronger
global no-later-write promise was introduced by the earlier local Plan, not
established by those controlling inputs, and is explicitly removed rather than
silently represented as implemented. Stamp may record outcomes or save
independent nodes before a mid-call failure is observable to pipdata. This
slice does not claim run-atomic rollback, immediate cancellation inside stamp,
or safety against arbitrary concurrent writers. Independent node-local
calculation failures use stamp's actual deterministic/external/unknown rules;
they do not by themselves turn the whole run into an infrastructure failure.

Postguards run in an unwind-safe finally path on both successful and exceptional
calculation exits, including a callback that mutates an invariant and then
throws a typed node failure. Capture calculation condition, postguard condition
and outer lease-release condition separately before propagating. A failed
postguard latches integrity even when the calculation already failed; do not
return x or allow another value calculation. Preserve the original classified
calculation condition as a structured cause, not a text-derived replacement.

Cleanup precedence is explicit: an original cancellation/interrupt remains
that original cancellation after ownership-checked cleanup is attempted; retain
cleanup/integrity conditions as structured secondary causes without masking it.
Otherwise any postguard/release/proof integrity failure yields
pipdata_engine_integrity_error with actual feedback and primary/secondary
conditions attached. A secondary release failure must not replace an existing
typed integrity condition or erase its feedback. If no primary condition exists,
release failure becomes the integrity cause and prevents normal result return.
Do not suppress cleanup errors or delete another owner's lease. This uses the
same allowed execution code/test files, not a new recovery/transaction layer.

### Exact Producer Boundary

- Set targets to ALL normalized node paths, once each; this slice has no separate
  scoped/terminal-target selector. Include all recipes, including prerequisite
  roots, as candidates. Normalization checks containment/identity only; stamp
  retains topology/currentness authority. First-build chain and multi-root
  diamond tests assert every supplied node is selected exactly once.
  `stamp::st_plan_rebuild(targets, include_targets = TRUE, requests = requests)`
  receives the unnamed request list. Keep the complete returned object, including
  request list-column/attributes and preview skips, into
  `stamp::st_rebuild(plan, rebuild_fun = callback)`. Pipdata dispatches the
  callback stamp invokes; it does not order/filter/force candidate execution.
- Each request has required fields `schema = 1L`, `path`, logical `root`,
  unnamed `parents`, `required_components`, `components`, `semantic_schema`,
  `semantic`, `format`; optional audit/review only. Root TRUE iff parents empty.
  Parent records contain exactly path/version_id, scalar character exact pin.
  NULL pins are preview-only for selected upstream recipes; stamp resolves and
  validates effective pins. Never send NA/numeric/latest selectors as exact pins.
- New descriptors use algorithm `sha256`, lowerhex64, canonicalization schema
  `pipdata-r-function-v1` for source-stripped formals/body serialized-v3 and
  `pipdata-r-value-v1` for declared immutable values serialized-v3. Compute the
  actual digest with SHA-256; do not relabel xxhash64 as siphash13. Include known
  closure helpers/constants/rules; codetools is supplementary, not discovery of
  hidden dependencies. Reject missing, NA, environment/external-pointer or
  unreviewed opaque rule values. Preserve legacy fingerprint output unchanged.
- No semantic fields means NULL schema and list() values. Otherwise preserve
  declared logical/integer/double/character types and scalar/vector shapes,
  absent/NULL/NA/empty distinctions and reserved-key validation in stamp.
  Request format is an exact case-sensitive registered identifier. No extension
  guessing, alias coercion or pipload qs2 default substitution for explicit RDS.
  This bounded route accepts literal `rds`, `qs2`, `fst` only (the actual registry
  keys verified in stamp/R/format_registry.R:124-138); uppercase aliases and other
  registered formats are rejected before writes. RDS/QS2/FST below name those
  format families, not alternative accepted string spellings. Other producer
  formats are described for contrast, not claimed engine coverage.
- Callback returns one named x, including actual NULL for RDS/QS2, plus allowed
  metadata/format/code/code_label. Optional format must equal requested format;
  semantic metadata must match. FST/CSV/Parquet NULL is an error, not a skip.
- Public execution results carry built/unchanged/skipped/blocked/failed/dry_run
  and version/request/receipt/outcome IDs. Verify exact receipt with
  `stamp::st_execution_info(path, receipt_id = id, alias = alias)`; explicit bad
  ID never falls back. Raw pip_write/pd_save_receipt results are not execution
  receipts. A successful changed-parent callback can retain an output version
  while adding a new receipt; a satisfied repeat adds neither.
- Metadata/data reads use the same explicit character version with
  `pipload::pip_read(id, format, version, alias, verbose = FALSE)` and
  `metadata_only = TRUE`. Format conflicts/missing exact metadata fail without
  latest fallback. Separate metadata-artifact payloads or warning/NA enrichment
  are not data-free proof. Raw forwarding tests separately exercise
  `pipload::pip_write(..., parents=, request=, semantic_schema=)` and its invisible
  unmodified producer return; do not double-save inside rebuild callbacks.
- Deterministic/external callback failures use the respective
  stamp_deterministic_failure/stamp_external_failure condition classes. Unknown
  actual failure blocks. Deterministic recovery requires changed proof AND
  request$review$outcome_id naming the governing failure. External failure is
  eligible only on the next analyst-started call; never schedule retries.
  `st_record_outcome` permits failed/blocked only, with exact effective request
  and failure list(class,message); blocked class is unknown. Do not duplicate
  an outcome already recorded by st_rebuild or infer class from message text.
- Modern path/parent proof is constrained by the selected alias root. Prove one
  registered-root graph first. Do not infer cross-root compatibility from five
  legacy aliases or privately rewrite paths; a needed cross-root graph stops
  for an approved producer-contract extension/separate task.

## Implementation Steps

## Phase 1: Execution Preflight And Bound Verification Environment

### 1. Verify Execution Identity And Ownership

- **Requirements**: R4, R5, R8, R9
- **Files**: read this Plan, installed `.kilo/commands/cg-work.md` and shared
  goal/active-state/artifact contracts; specified lifecycle/reporting paths below.
- **Details**: treat decisions E1-E6 as settled by this work specification, not
  questions to re-ask. Reverify W/branch/base/dirty changes, dependency source
  pins, E containment, absent-or-owned S, write-set collisions and active-state
  ownership. Run structural validation before lifecycle writes. Record the
  explicit work instruction and its named branch operation, plus the E5
  workflow substitute rationale. Preserve unrelated changes. Never execute the
  umbrella Plan or use PROD silently. cg-work may update only allowed progress
  metadata; a needed material body/scope change still requires revision.
- **Test Scenarios**: denied branch/path, conflicting writer/active task,
  mismatched source pin, unexpected runtime owner, Force requested; actual
  failures stop before implementation, not another generic approval bundle.
- **Tests**: read-only Git/path/ownership checks and document validation; then
  initialize only the specified report/state records under normal workflow.
- **Acceptance criteria**: V1; actual identity/ownership/containment checks
  succeed and exact write/report paths are recorded. A failed check or denied
  action blocks; a successful preflight proceeds without duplicate consent.

### 2. Prepare And Bind Disposable Runtime

- **Requirements**: R2, R3, R6, R8
- **Files**: only specified runtime S paths below; no shipping hook/config edit.
- **Details**: inspect installed dependency hooks and selected/new tests before
  first package load; implement guarded E runner, export fixed producer/adapter
  content and candidate pipdata, install only explicitly permitted packages in
  S/library. Bind archive/source/install/loaded namespaces and all import
  versions. Disable auto logging before load and set explicit cache/store roots.
  Do not copy ignored profiles, .stamp stores, credentials or session logs.
- **Test Scenarios**: source drift, wrong global stamp/pipload, missing import,
  unexpected hook/warning, reparse escape, setup failure, native timeout.
- **Tests**: future `run.ps1 -Stage preflight`; public API/formals, save/read and
  exact metadata smoke on simple RDS/qs2/fst fixtures. Record actual native exits.
- **Acceptance criteria**: V2; loaded producer/adapter match approved manifests,
  every write path contained on E, no production aliases/network access, all
  required dependencies available without download/upgrade. Phase-1
  completion uses only baseline tests and producer/adapter/public legacy smoke;
  it requires no new recipe argument, engine test or modern behavior.

## Phase 2: Thin Authority Route And Local Evidence

### 3. Build Complete Requests And Automatic Component Proof

- **Requirements**: R1, R2, R4
- **Files**: `R/code_fingerprint.R`, `R/dependency_plan.R`;
  `tests/testthat/test-code-fingerprint.R`,
  `tests/testthat/test-m2-engine-authority.R`.
- **Details**: add small internal request construction for the specified recipe
  schema; reuse pd_canonical_function, use truthful SHA-256 descriptors for new
  path only. Preserve old component/summary/audit output. Metadata/input facts
  become request proof, not pipdata predicates. Use source identities only as
  audit. Reuse curated closure patterns when binding real processing functions;
  do not claim neutral synthetic closures prove full production coverage.
- **Test Scenarios**: order invariance, changed formals/helper/constant/rule,
  missing/NA proof, changing audit only, invalid parent/schema/format, root/NULL
  distinction, undeclared semantic metadata; changed captured helper/constant
  versus locked declared copy; two distinct primitive helpers both rejected;
  undeclared/dynamic dependency and mutation attempts rejected.
- **Tests**: add failing cases first; run the Phase-2 gate after correction.
- **Acceptance criteria**: V3; stamp receives complete truthful requests before
  execution, unrelated audit changes do not invalidate; legacy hashes unchanged.

### 4. Delegate Execution Without A Shadow Manifest

- **Requirements**: R1, R3, R4, R5, R6, R9
- **Files**: `R/pd_run_pipeline.R`, `R/dependency_execution.R`, `R/save_pip.R`;
  `tests/testthat/test-m2-engine-authority.R`,
  `tests/testthat/test-dependency-api-contract.R`.
- **Details**: append only recipes=NULL as specified. Branch before legacy reads;
  reject unsupported legacy controls/Force before side effects. Pass complete
  stamp plan unchanged to rebuild and dispatch only requested callback. Keep
  compact feedback projection separate from stamp execution object. Modern
  request execution must not call pd_snapshot_facts, pd_dependency_plan,
  pd_assert_bootstrap, timestamp forcing or catalog-hash receipt recovery.
  Apply the exact modern lease/context/guard mapping above; do not use legacy
  execution fences or publish synthetic stage records. A latched observed
  integrity failure prevents subsequent value calculations, not stamp's own
  possible later outcome/publication writes. Preserve the complete plan and
  E3 limits. Stamp owns serial parent/save/receipt validation. Resolve
  structured failure_class/governing_failure_id from public exact outcome chains.
- **Test Scenarios**: old manifest says stale but stamp skips; old manifest says
  current but stamp runs/blocks; absent legacy manifest has no authority; preview
  skip becomes runnable after upstream; no household/aux loads; legacy no-recipes
  formals/return path unchanged; Force rejected with zero writes; all recipe
  paths selected on first-build chain/multi-root diamond; callback mutation
  latches guard and prevents later calculations while actual outcome writes
  remain accurately recorded, not falsely claimed absent; mutation followed
  by typed callback failure still runs/latches postguard; lost lease plus
  failing release retains typed error/feedback/secondary cause and no later
  value calculations; original cancellation is not replaced by cleanup error.
- **Tests**: actual engine/real stamp, with legacy selector calls replaced by
  fail-if-called sentinels only; never mock stamp decisions or successful receipts.
- **Acceptance criteria**: V4 and applicable V6; only stamp decides/executes
  selected work, unsupported modes cannot enter legacy behavior accidentally.

### 5. Prove L-Authority, Pins, Failure Rules And Restart

- **Requirements**: R2, R3, R5, R6, R7, R9
- **Files**: `tests/testthat/test-m2-engine-authority.R`; specified S fixtures and
  evidence only. Keep self-contained fixture constructors in this test file.
- **Details**: use small root A, child B and descendant C functions; chain,
  diamond/multi-target and cycle cases call real stamp graph APIs. Validate
  source-to-request-to-callback-to-receipt trace, single root and exact loaded
  versions. Distinguish callback success from durable receipt and any local
  checkpoint; never mark success from old catalog presence. Compare immutable
  old data/metadata after later writes. Restart in a new process with the same
  saved E fixture and observe governing stamp outcomes, not in-memory flags.
- **Test Scenarios**: first build; satisfied repeat with zero callbacks/new
  versions/receipts; helper-only change; A changes but B output retained with new
  receipt and C cutoff; missing parent, failed parent, invalid receipt, invalid
  metadata pin, explicit format mismatch, semantic-only version change, NULL
  payload versus omitted x; deterministic review/change matrix, external-next-call,
  unknown-error block; callback/fence/save failure and cancellation accounting;
  deterministic failed -> blocked -> blocked -> new-process reviewed-change
  using only feedback/public history for the governing ID; missing/cyclic
  outcome links fail without latest fallback. Distinguish supported node-local
  failures from global no-write cancellation, which is not provided here.
  Use public APIs to construct state, not forged receipts. Corruption/removal
  fixtures may mutate only captured paths created by that test under its unique
  S/stores/<phase>/<test-id> root. The test records each artifact/receipt/sidecar
  path after public creation and verifies containment before corrupting it;
  no production aliases, arbitrary catalog forging or unbounded glob deletion.
- **Tests**: real RDS/qs2 storage plus fst value round-trip/null rejection; exact
  pipload data/sidecar forwarding. Preserve original pins and stamp plan
  attributes. Required cases cannot skip or be static source assertions only.
- **Acceptance criteria**: V5/V6; actual adapter trace supports umbrella
  L-authority/V6 contribution, not full T6/T8/T11/T12 or complete Step 3/whole M2.

### 6. Validate Compatibility, Documentation And Scoped Closeout

- **Requirements**: R4, R5, R7, R8, R9
- **Files**: `man/pd_run_pipeline.Rd`,
  `vignettes/articles/PIP-data-pipeline.Rmd`, specified tests/report/state and Plan
  progress fields only. Roxygen source is `R/pd_run_pipeline.R`.
- **Details**: document opt-in envelope/table, legacy path, no Force, exact
  metadata/receipt semantics and scope limits. Manually edit/check the single
  scoped Rd in S/docs alongside matching roxygen source, without running a
  mismatched generator; transfer only the allowed Rd delta. Refuse NAMESPACE/other help/site
  churn. No new export/dependency expected. Retain Phase-1 baseline evidence,
  run Phase-2 current-tree gate and installed engine smoke plus scoped docs checks. Record
  warnings/skips/exclusions/identities and unresolved integration obligations.
- **Test Scenarios**: positional/default legacy calls, no-recipes result, empty
  M2 feedback, blocked feedback, installed help/API availability and wrong library.
- **Tests**: exact E5 commands below, native exits and scoped Rd/source checks.
- **Acceptance criteria**: V7 and all earlier required evidence; no unexecuted
  check relabeled passed, no roadmap parent-feature completion.

## Dependency And Ownership Graph

Fixed producer source + delivered adapter + explicit work instruction -> bound
runtime (Phase 1) -> complete requests/fingerprints -> stamp-owned execution ->
real-storage failure/pin evidence -> scoped compatibility/docs closeout.

Hard package graph remains pipdata -> pipload -> stamp and pipdata -> stamp.
Pipload Suggests pipdata for its load-then-deflate convenience; do not use that
callback inside the engine or promote it to Imports. No new dependency needed.

Shared future ownership: dependency_execution, dependency_plan, code_fingerprint,
save_pip, public entry-point/tests/docs overlap later aux/deflation/engine work.
Sequence ownership until a fresh disjoint write-set check exists. No parallel
worker, worktree/session or helper is authorized by this Plan's existence.

## Work Scope And Lifecycle Paths

W is exactly `E:/PovcalNet/01.personal/wb384996/PIP/pipdata`; no new session,
checkout or worktree is required. The future implementation branch is
`feat/m2-pipdata-authority`. The eventual work instruction must explicitly name
`git checkout -b feat/m2-pipdata-authority` if it is to be created; this revision
does not execute it. If already on that branch, verify it; if it already exists
elsewhere or branch creation is denied, stop with that specific conflict. Do
not switch to an unknown branch, stash, commit, merge or push.

Package allowlist, W-relative: `R/pd_run_pipeline.R`, `R/dependency_plan.R`,
`R/dependency_execution.R`, `R/code_fingerprint.R`, `R/save_pip.R`,
`tests/testthat/test-m2-engine-authority.R`,
`tests/testthat/test-code-fingerprint.R`,
`tests/testthat/test-dependency-api-contract.R`, `man/pd_run_pipeline.Rd`,
`vignettes/articles/PIP-data-pipeline.Rmd`. Existing manifest/contract/input/stage
implementation, DESCRIPTION, NAMESPACE and hook files remain read-only; stop
for approval if any becomes necessary. No global cleanup of old selectors.

Lifecycle scope, W-relative: the canonical Plan
`.cg-docs/plans/2026-10-26-m2-pipdata-authority.md` (only status, completed-date,
failing-steps, completed-phases, current-phase, execution-report),
`.cg-docs/work-reports/2026-10-27-m2-pipdata-authority.md` (the frontmatter pointer,
date identifies this work order; actual execution times are recorded in each
run section), `.cg-docs/active-state/current.json`, and
`.cg-docs/active-state/2026-10-27-m2-pipdata-authority-previous.json` as the exact
one-time backup of the prior completed state before replacing the pointer.
Current state was read: completed September 1 for the older DAG Plan, not a live
ownership claim. Recheck it before mutation; changed/active unrelated ownership
blocks. Existing report for this Plan is append-only; same-path unrelated
report or backup content blocks rather than silently replacing it. Historical
reports/Plans/roadmap/configuration remain read-only. No review artifact is
required; final self-review findings/results go in the execution report. Only
the later explicit review command authorizes its read-only critic, no workers.

Future central execution reporting is exactly
`H/.cg-docs/orchestration/outputs/pipdata/m2-engine-work/20261027-plan-authority-work/handoff.md`
and its sibling `evidence.md`, owner existing pipdata Code session. Use
pip-handoff-v1 and all ten headings; real UTC/identity/checkpoints, update at
start/boundaries/blockers/completion and read back. Append run/resume history;
an unrelated existing owner/content blocks. Original planning/readiness reports
are read-only historical inputs, not execution reports. No new reporting path
must be negotiated after the direct work request.

S is exactly `E:/PovcalNet/01.personal/wb384996/PIP/pipdata-m2-engine-runtime/`.
Only `runner/run.ps1`, `runner/gate.R`, and descendants of `source/`, `library/`,
`tests/`, `tmp/`, `home/`, `cache/`, `stores/`, `evidence/`, `build/`, `docs/`.
No S creation now. Source exports/install/build copies/logs/native captures stay
there. Runtime fixtures are S/stores/phase1 or S/stores/phase2 with unique test
subdirectories; proof-corruption tests may change only their recorded descendant
paths. Retain immutable Phase-1 source at S/source/phase1/pipdata; candidate
copies go to S/source/phase2/pipdata. Use separate S/library/phase1 and phase2
libraries/fresh processes. Installed/loaded identities, test cases, native
exits/timeouts/warnings and source manifests go under S/evidence. Do not reuse
an unowned existing S. These exact paths and their stated bounded descendants
are part of the eventual work request, not present implementation permission.

## Testing Strategy

### Current Planning Checks

Source/document/metadata reads, read-only Git checks, document validation-only
and one explicitly requested read-only critic with closing recheck ran. No R
process, package load/install/build/test, production data, remote query or CI
check. Actual validation/review evidence is in the central reports/final handoff;
neither is a package-test pass.

### Selected E5 Phase Gates

A safe ordinary full-suite gate has NOT been established. Tests/testthat.R loads
the package; helpers are automatic and helper-dlw-wrapper-fixtures.R parses nine
test files and evaluates assignments. Existing fixture code creates aliases and
deletes directories; current suite-wide hook/resource audit is incomplete.
Select the phase-specific substitute below for this bounded contribution, not
the unsafe ordinary suite. The user's explicit work request for this revised
Plan adopts E5; record that instruction as the task-specific workflow exception
to the generic full-suite boundary, with this rationale and exact exclusions.
Do not seek duplicate test-strategy approval or use it to waive selected tests.
No full-package regression or commit/merge certification follows from E5.

Future runner canonical invocation, PowerShell 5.1:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "E:/PovcalNet/01.personal/wb384996/PIP/pipdata-m2-engine-runtime/runner/run.ps1" -Stage preflight
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "E:/PovcalNet/01.personal/wb384996/PIP/pipdata-m2-engine-runtime/runner/run.ps1" -Stage gate -Phase 1
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "E:/PovcalNet/01.personal/wb384996/PIP/pipdata-m2-engine-runtime/runner/run.ps1" -Stage gate -Phase 2
```

These are future scripts inside the specified S scope, not existing or
executed helpers. The runner must enforce the following exact R gate, using
`C:/Program Files/R/R-4.5.2/bin/Rscript.exe --vanilla S/runner/gate.R gate <phase>`
after executable availability/version is verified. Executable reads on C are
permitted; no created files there. Source is S/source/phase1/pipdata for baseline
or S/source/phase2/pipdata for candidate; phase is integer 1 or 2, never inferred
from a missing test. The overlay library is S/library/phaseN and the only
additional read-only library input is C:/Program Files/R/R-4.5.2/library. Each selected
S/tests/phaseN directory contains exactly its approved files below, no helpers,
setup files or fixture regeneration scripts. Validate existence and exact set
before loading R tests; a missing required file is failure, not a zero-test pass.

```r
options(
  pipfun.log.auto = FALSE,
  pipfun.main_dir = file.path(S, "stores", "pip-root"),
  pipdata.dependency_manifest_path = file.path(S, "stores", "manifest")
)
phase_library <- file.path(S, "library", paste0("phase", phase))
.libPaths(c(phase_library, "C:/Program Files/R/R-4.5.2/library"))
pkgload::load_all(file.path(S, "source", paste0("phase", phase), "pipdata"),
                  helpers = FALSE, quiet = TRUE)
test_env <- new.env(parent = asNamespace("pipdata"))
for (name in getNamespaceExports("testthat")) {
  assign(name, getExportedValue("testthat", name), envir = test_env)
}
stopifnot(
  identical(get("test_that", envir = test_env), testthat::test_that),
  identical(get("expect_identical", envir = test_env),
            testthat::expect_identical),
  identical(get("pd_hash_object", envir = test_env),
            getFromNamespace("pd_hash_object", "pipdata"))
)
testthat::test_dir(file.path(S, "tests", paste0("phase", phase)),
                   load_package = "none",
                   env = test_env,
                   reporter = "summary", stop_on_failure = TRUE,
                   stop_on_warning = TRUE)
```

The private test environment deliberately exposes testthat's public exports
while inheriting package internals from the loaded pipdata namespace; it is not
a package NAMESPACE edit or automatic helper sourcing. Validate representative
framework and package symbols before test-file evaluation. An unexpected
binding/collision that changes a selected test's intended lookup is a failed
runner preflight, not a reason to rewrite unchanged baseline tests.

Phase-1 baseline gate: exactly unchanged `test-code-fingerprint.R` and
`test-dependency-api-contract.R` from the pinned baseline, plus a separate
runner public storage/API smoke for fixed stamp/pipload and unchanged pipdata.
It does NOT include test-m2-engine-authority.R or expect recipes/modern feedback.
The baseline API test must retain original formals until Phase 2 changes them.
Phase 1 can complete using V1/V2 and these executed baseline checks alone.

Phase-2 candidate gate: exactly candidate `test-code-fingerprint.R`, updated
`test-dependency-api-contract.R` and new self-contained
`test-m2-engine-authority.R`, plus installed public engine smoke. Phase-2 tests
are written/verified red in Steps 3-4, never pulled into Phase 1 to make its
boundary pass. At final closeout retain Phase-1 evidence with its baseline
identity and rerun Phase-2 candidate checks at the current identity.

If any selected file requires an unselected helper/test, stop with the exact
missing dependency instead of automatically sourcing helpers. Record all
skipped/warned tests; any required
case skipped is a failed completion gate. Do not globally suppress warnings.
Build-version warnings have the exact load-only rule below; no test assertion,
selected failure or other warning is waived.

Installed smoke stage must build source in S/build/phaseN using
`R.exe CMD build --no-build-vignettes --no-manual <S/source/phaseN/pipdata>`, then
`R.exe CMD INSTALL --no-test-load --no-multiarch --no-html --library=<S/library/phaseN> <exact-archive>`,
then run phase-specific public smoke in a fresh Rscript process with
library(pipdata, lib.loc=phase_library): Phase 1 verifies baseline exports/formals and
real producer/adapter pins/receipts without modern pipdata behavior; Phase 2
exercises the new public recipe/feedback/receipt/pin cases. Capture archive name/hash and
actual installed namespaces; use only exported/public behavior in that smoke.
Preflight explicitly installs fixed stamp/pipload sources into each overlay
with the same no-test-load/no-multiarch/no-html flags, under the E child controls,
then verifies in a fresh configured Rscript child. No dependency resolver,
unbounded install.packages, source rebuild of ordinary imports or network
fetch. No R CMD check claim. Each child captures timeout (120 seconds preflight,
600 seconds gate/build/install), stdout/stderr separately, native exit code and
source/library manifests; timeout is failure and terminates that owned child
tree, never a hanging progress poll or implicit pass. No background loop.

### Dependency Identities And Source Binding

Critical identities are fixed in Distinct Pins. Export only
DESCRIPTION/NAMESPACE/LICENSE/R/man from the stamp pin and delivered pipload
commit; retain exact archive member list and SHA-256 plus raw/export-normalized
source manifests. Stamp tree at the supplied pin is
12ea22c4b2eaa0b2e1d93ea79d1cd20ca60cb4cb. For pipdata export baseline package
files from its fixed HEAD for Phase 1; Phase 2 copies only candidate package
DESCRIPTION/NAMESPACE/LICENSE/R/man/inst plus the three selected tests, excluding
.git, profiles, configuration, sessions and existing stores. Include inst recode
assets for legacy fingerprint tests. Snapshot every source byte used before
the child, then verify unchanged after the child. No ignored-profile copy.

Fresh read-only DESCRIPTION queries on 2026-10-27 established the following
read-only baseline under C:/Program Files/R/R-4.5.2/library. These are exact
selected package identities, not a claim that they have just loaded/passed.
Record full raw installed-file manifests/loaded paths as normal Phase-1
preflight, including transitive namespace closure; do not invent source SHAs
where installed metadata has none. Those packages are identified by version,
Built record and captured installed bytes, not falsely by a repository HEAD.

| Package | Selected version | Source identity when installed metadata supplies it |
|---|---|---|
| cli | 3.6.5 | installed bytes, no RemoteSha |
| collapse | 2.1.6 | installed bytes, no RemoteSha |
| data.table | 1.18.2.1 | installed bytes, no RemoteSha |
| digest | 0.6.39 | installed bytes, no RemoteSha |
| yaml | 2.3.12 | installed bytes, no RemoteSha |
| fs | 1.6.6 | installed bytes, no RemoteSha |
| glue | 1.8.0 | installed bytes, no RemoteSha |
| haven | 2.5.5 | installed bytes, no RemoteSha |
| joyn | 0.3.0 | installed bytes, no RemoteSha |
| kit | 0.0.21 | installed bytes, no RemoteSha |
| pipaux | 0.3.0 | 44fc4bcc6f253420e0d31edc5ac45af6c0d729bb |
| pipfun | 1.0.1 | installed bytes, no RemoteSha |
| purrr | 1.2.1 | installed bytes, no RemoteSha |
| rlang | 1.1.7 | installed bytes, no RemoteSha |
| wbpip | 0.1.4 | ae0d3937bc61b27fa4566e72a75c56e562ac7f88 |
| data.validator | 0.2.1 | installed bytes, no RemoteSha |
| assertr | 3.0.1 | installed bytes, no RemoteSha |
| labelled | 2.16.0 | installed bytes, no RemoteSha |
| DT | 0.34.0 | installed bytes, no RemoteSha |
| dlw | 0.1.1 | 0ee8e31dd23d2f5bbb9d094790396aacbb5fd433 |
| testthat | 3.3.0 | installed bytes, no RemoteSha |
| withr | 3.0.2 | installed bytes, no RemoteSha |
| codetools | 0.2-20 | base runtime library input |
| pkgload | 1.4.1 | installed bytes, no RemoteSha |
| qs2 | 0.1.7 | installed bytes, no RemoteSha |
| fst | 0.9.8 | installed bytes, no RemoteSha |

R executable is C:/Program Files/R/R-4.5.2/bin/Rscript.exe (R.exe sibling);
verify actual R 4.5.2 UCRT x64 and executable bytes/version before use. Global
stamp is actually 0.0.10 at RemoteSha 39b4bf3ce580012bf0d3e43f67bdc87ff8114b40,
and global pipload 1.0.0 is RemoteSha 347a3af6290ac3acfce964e9c9b90a14fc5f5298:
NEITHER is the selected producer/adapter. Overlay stamp must be 0.0.11 from the
fixed content pin, pipload 1.0.0 from delivered 4ef0d90, and loaded paths must be
inside the correct S/library/phaseN. No same-version global fallback.

Installed roxygen2 is 7.3.3, whereas DESCRIPTION records generation version
8.0.0. Do not download or run a mismatched generator across this package.
Update the single permitted Rd argument/details manually alongside matching
roxygen source, parse/check that Rd and compare formals in the installed smoke;
leave DESCRIPTION's generation field/NAMESPACE/other help/site unchanged.
This is a selected docs strategy, not an unresolved dependency decision.

The following selected installed packages have Built R 4.5.3 while runtime is
4.5.2: assertr, digest, data.validator, data.table, collapse, yaml, labelled,
joyn, qs2, purrr, rlang, DT. They are already installed binary inputs; no ABI
success is presumed. During namespace load only, a handler may record and
muffle exactly `package '<name>' was built under R version 4.5.3` (or the same
R quotation style), only for those names with that exact rechecked Built
record. Record every occurrence. No broad suppressWarnings, no handler around
test execution, no waiver of DLL/ABI/load errors or any other warning. Selected
test/build/install failure still blocks. This explicit startup compatibility
notice policy replaces the prior blanket stop-on-any-load-warning dead end;
actual runtime compatibility is proved only by the executed selected gate.
Every changed/missing version/byte identity after baseline capture is a failed
preflight or evidence drift, not an automatic dependency choice or reapproval.

### Excluded Current Test Files

All 41 other current test files are excluded from the selected substitute:

```text
test-adjust-population.R
test-build_pip_inventory.R
test-dependency-bootstrap.R
test-dependency-contract.R
test-dependency-execution.R
test-dependency-inputs.R
test-dependency-manifest.R
test-dependency-performance.R
test-dependency-plan.R
test-dlw_store_release.R
test-dlw_validation_engine.R
test-dlw_validation_spec.R
test-dlw-unified-logging.R
test-get-aux-hashes.R
test-get-country-pfw.R
test-log_report.R
test-logging-integration.R
test-pd_dlw_clean.R
test-pd_dlw_gd_clean.R
test-pd_process_data.R
test-pd-change-report.R
test-pd-deflate-pipeline.R
test-pd-deflation.R
test-pd-env.R
test-pd-metadata-refresh.R
test-pd-run-pipeline.R
test-pipdata_dlw_compare.R
test-pipdata_dlw_process.R
test-pipdata_get_gmd.R
test-pipdata_validate_gmd.R
test-pipdata_validation_report.R
test-pipeline-context.R
test-pipeline-run-result.R
test-pipeline-stage-result.R
test-recode-spec.R
test-reconcile-pip-inventory.R
test-release_mgmt.R
test-save_pip.R
test-utils.R
test-valid_aux_load.R
test-valid_dlw_load.R
```

Lost coverage includes numerical processing, full legacy wrappers, inventory
reconciliation, C2 manifest/lease behavior, logging, acquisition and validation.
The new tests must explicitly exercise reused safety primitives on the modern
route; that does not certify excluded suites. Two existing historical files
contain disabled if(FALSE) tests; absence of execution is not coverage.
Full package check/examples/vignettes, producer full check, production data,
SMB/multiwriter, target-scale performance and umbrella T1-T12 remain excluded.

### E-Only Controls And Cleanup

Before EVERY child, including build/install, set TEMP/TMP/TMPDIR, HOME,
USERPROFILE/R_USER, XDG_DATA_HOME/XDG_STATE_HOME/XDG_CACHE_HOME/XDG_CONFIG_HOME,
R_USER_CACHE_DIR/R_USER_DATA_DIR/R_USER_CONFIG_DIR, PIP_ROOT_DIR, libraries and
tool-specific caches/logs/output paths under S. Neutralize R user/site profiles
and environ files; set pipfun.log.auto=FALSE plus explicit main/manifest roots
before namespace load. Keep required Windows SystemRoot/WINDIR/COMSPEC and
System32 execution paths; do not repeat stripped-environment shutdown stalls.
Inspect every loaded import's write-capable hooks and block unbounded behavior.

Verify existing E parents, resolved root containment and no reparse escape
before creating/copying/deleting. Fixture aliases unique per process. Failure-
safe teardown restores options/aliases/leases without deleting unowned files.
Retain evidence/manifests/failed logs; clean only approved generated S descendants
after readback, never entire repositories, historical C paths or live leases.
This is process hygiene, not OS-enforced filesystem/network containment.

## Documentation Checklist

- Document exact opt-in recipe and feedback schema, rejected legacy controls,
  stamp-only selection and incremental-only limitation in roxygen/Rd/article.
- Explain artifact receipt versus execution receipt, same-pin metadata, formats,
  retained outputs, failure review and no retry loop.
- Keep legacy API/defaults/returns and production activation warning explicit.
- Manually edit/check only the scoped Rd in a disposable docs copy, keeping
  roxygen source parity; no generator run, docs/ site, NAMESPACE, DESCRIPTION or
  unrelated generated-doc changes.
- Save actual source/library/native-exit/exclusion evidence and nonempty gaps;
  do not mark complete engine, Step 3 or whole M2 from this contribution alone.

## Risks & Mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| Old manifest/force route still selects M2 work | Two authorities, false reuse/rebuild | Early explicit route; fail-if-called selector tests; no merged plans |
| xxhash64 mislabeled as accepted stamp algorithm | Invalid/incomparable proof or misleading equality | Compute actual SHA-256 descriptors with canonicalization schema; preserve legacy hashes |
| Raw save called an execution receipt | False success/incorrect retained-output lineage | Real rebuild receipts and exact public execution-info checks |
| Missing effective pin/metadata or changed graph after preview | Wrong input, false currentness | Preserve plan attributes/skips; stamp revalidates; exact reads and failure tests |
| Hooks/helpers/caches escape E or reach data | Unauthorized writes/access | Inspect/preconfigure before every child; no automatic helpers; stop on unsafe dependency |
| Synthetic single-root slice mistaken for production migration | Unsupported cross-alias/Force/SMB claims | Decision E2 and one-root boundary; separate future stage/production gates |
| Partial substitute hides legacy regressions | Unverified backward behavior | API smoke/new branch tests; enumerate exclusions; require separate full-suite work before broader certification |
| Shared engine/deflation files have concurrent owners | Conflicting changes/evidence drift | Fresh exact ownership/pin checks and sequenced work; preserve unrelated edits |

## Out of Scope

No implementation in this planning session; no source/test/docs/configuration,
roadmap, registry, active-state or execution-report edits now. No Git mutations,
general helpers/delegation, package execution/install, production/network data
access. The user's later explicit Plan-review request authorized only its one
read-only critic/recheck; no implementation worker or general helper permission.
Future slice excludes new orchestrator package, pipload/stamp repairs, all
legacy currentness removal, survey selection/acquisition, real aux/deflation,
mean/indicator computations, TEST manifest, M1 R1, Force implementation, full
analyst/scoped/plan-only interface, production activation, release/CI/PROD merge
or whole-M2 completion. Full-suite exclusion is not a permanent waiver.

## Completion Contract

### Outcome

A bounded, opt-in M2 route in pipdata supplies explicit recipes, exact inputs
and automatic fingerprints to stamp. Stamp alone selects/revalidates work;
synthetic producers and real stamp/pipload storage demonstrate the boundary
without requiring a survey, mean or TEST manifest.

### Verification Surface

| ID | Phase | Evidence Required | Command/Artifact | Required |
|----|-------|------------------|------------------|----------|
| V1 | 1 | Explicit work instruction and named branch operation, successful baseline/ownership/path/source-pin checks, correct lifecycle/central-report initialization and recorded E1-E6 decisions/E5 rationale | Read-only preflight ledger, execution report and central handoff | yes |
| V2 | 1 | Bound source/archive/install/library/runtime, E-only preflight, two-file baseline gate and unchanged public/producer/adapter smoke; no Phase-2 artifacts required | S runner preflight and gate -Phase 1, baseline manifests/public smoke | yes |
| V3 | 2 | Truthful automatic component descriptors, changed helper/rule proof, missing-proof block and audit-only stability | Fingerprint and new engine-authority tests | yes |
| V4 | 2 | Stamp decisions respected despite contradictory legacy facts; request-bearing plan/attributes preserved | Real-storage L-authority decision/call trace | yes |
| V5 | 2 | Genuine built/unchanged/skipped receipts, immutable history, same-pin metadata/data and explicit formats | Real stamp/pipload receipt/round-trip cases | yes |
| V6 | final | Failed-parent block, governing failure/review, no retry, complete feedback and no false success after integrity failure | Synthetic failure/restart tests, saved outcomes | yes |
| V7 | final | Retained Phase-1 baseline evidence plus current Phase-2 three-file candidate gate, installed engine/docs smoke and exact exclusions | gate -Phase 2 native exits/results/manifests, scoped Rd checks | yes |

These are local evidence IDs; local V6 is NOT umbrella V6 by numbering.
Only actual L-authority execution supports the umbrella V6 contribution.

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | No independent pipdata/pipload currentness authority on M2 | Selector reachability and contradictory-fact tests |
| C2 | Exact pins/history; modern lease/proof guards and producer-owned serial validation under E3; integrity failures yield typed run error, not success or unavailable global no-write cancellation | Fault injection, calculation-entry trace, typed error/feedback and exact outcome/immutable checks |
| C3 | Legacy callers/returns and dependency direction preserved | API/compatibility assertions; no new package/export/reverse Imports |
| C4 | No inferred Force support or M1 R1 choice | Reject forcing before writes; retain explicit deferred decisions |
| C5 | All generated data/cache/log/build/test files on E | Resolved-path guards and cleanup/evidence inventory |

### Boundaries

- Allowed when explicitly requested through cg-work: exact opt-in route, request/hash
  construction, stamp-owned execution/receipt handling, compact feedback,
  synthetic real-storage tests and scoped docs/lifecycle paths listed above.
- Out of scope: producer/adapter changes, broad migration, survey/mean/release,
  Force/R1, production/SMB, full analyst interface, roadmap/Git publication and
  complete Step 3/whole M2 certification.

### Iteration Policy

1. After the explicit work request, perform normal preflight with settled E1-E6;
   proceed without another approval bundle when facts/permissions pass. Branch
   creation still requires the separately named instruction.
2. Bind specified source/runtime/library identities before behavior changes.
3. Execute Phase-1 baseline/preflight gate before its completion. In Phase 2 add
   failing behavior tests, implement minimally, then execute the candidate gate
   and installed engine smoke; never require Phase-2 artifacts to close Phase 1.
4. At most two corrective attempts per step without weakening required tests
   or counting skips/static inspection as passes; otherwise blocked-stop.
5. Under deviation-policy ask, obtain approval before deviations/additional
   files; preserve failures, prior history and remaining gaps.

### Blocked-Stop Conditions

- No explicit work request, denied branch/path/action, or actual failing
  ownership/containment/source/runtime/verification check. Saving/revising this
  Plan is not implementation, but a later cg-work request does not need a
  duplicate consent bundle for its already specified scope.
- Producer mismatch, missing dependency, unbounded hook/path, potential C write,
  production/network access, or unsafe/unavailable verification.
- Work needs Force, unsupported cross-root proof, M1 R1, or changes outside
  specified package/runtime/lifecycle boundaries.
- Observed integrity failure stops new calculations/pipdata calls under the
  latched-guard contract; stamp may finish its current call and publish outcomes.
  A new requirement for stronger global no-write fail-stop is a material scope
  change needing producer capability, not an unresolved current choice.
  Missing exact pin/metadata/receipt, required
  skip, persistent verification failure, cancellation or protected-boundary
  crossing block local completion.
- Mandatory reports cannot be durably updated or Plan validation fails; do not
  claim a pass or complete state from static inspection alone.

## Gaps

No implementation phase has run. API, incremental mode, feedback, serial
failure/integrity behavior, exact write/report/runtime scope, dependency inputs
and task-specific test strategy are selected above; no material user choice
is intentionally left as a generic pre-start gate. Actual runtime namespace/
transitive-byte binding and hook containment remain execution preflight facts,
not presumed passes. The branch operation is explicitly named in Work Handoff
but not authorized/executed by this document-only revision. A real conflict,
missing input, denied operation or failed check still blocks work.

Full Force semantics, M1 R1, cross-root behavior, safe ordinary full-suite
execution, production/SMB and full umbrella integration remain outside this
contribution, openly unverified. E5 is partial coverage, not full-suite success.
No package test/load/install, integrated survey, mean, release or CI ran during
planning/revision. Historical results retain their content-identity limits.

## Review Corrections

Direct user follow-up on 2026-10-27 requested `/cg-plan-review`, correction of
all findings in this Plan and handoff to another session, not implementation.
The installed command's read-only critic was used; no separate review file was
created. The independently authorized outer caller applied corrections to the
Plan; the installed command and its critic never obtained write permission.

Initial critic result: 2 P1, 5 P2, 0 P3. All seven selected for correction by
the direct user request; none accepted as risk or deferred. Correction ledger:

| Finding | Problem | Correction |
|---|---|---|
| P1.1 | Phase 1 required Phase-2 test/API artifacts | Explicit two-file baseline/storage smoke versus three-file candidate/engine smoke; V2/V7 and boundary commands separated |
| P1.2 | Fixed stamp API cannot stop all later writes after mid-call fault | First revision exposed D-SAFETY instead of claiming impossible behavior. Readiness revision replaces that deferred gate with concrete E3, grounded in controlling serial/node-failure policy, typed integrity-error result and explicit continuation limits. |
| P2.1 | Declared proof did not bind callback's actual lexical values | Private rebound/locked proof environment, immutable values, trusted pure-callback restrictions and mutation/captured-value behavioral cases |
| P2.2 | Distinct primitives have equal NULL formals/body representation | Reject declared primitive calculation/helpers; ordinary wrapper required; two-primitive regression |
| P2.3 | Modern lease/context mapping unspecified or reused legacy fences | Explicit context/scope hash/lease location/boundaries; legacy fences/manifests excluded; stamp-owned checks distinguished |
| P2.4 | targets was undefined and could omit prerequisite recipes | All normalized node paths are targets; first-build chain/multi-root/diamond assertions |
| P2.5 | Latest blocked outcome ID is not governing failure review ID | Structured failure_class and governing_failure_id via exact public previous_id traversal, guarded/capped; fresh-process repeated-block recovery |

Historical initial parser validation passed after correcting punctuation in
six Requirements mappings; validation is document structure only. Closing
critic re-read all 866 corrected lines and checked the original seven findings
against relevant source/workflow: all seven closed, no new significant P1/P2/P3
issues. No accepted-risk/deferred finding. This is structural/source review,
not a runtime check or implementation result. Corrected Markdown validation
passed; final readback/identity and post-log validation results are recorded
from actual checks in the assigned central handoff/evidence.

## Readiness Corrections

On 2026-10-27 the user explicitly requested continuing this same session,
document-only correction of this canonical Plan plus the two new readiness
reports, followed by read-only cg-plan-review and outer-caller fixes of ALL
findings including P3. Earlier reports remain untouched. This revision does not
expand command/critic permissions and does not start implementation.

| ID | Readiness finding | Concrete correction |
|---|---|---|
| U1 | Generic deferred approval bundle instead of work decisions | Settled E1-E6; normal failed-check/denied-action stops, no duplicate consent |
| U2 | API/mode/failure/test choices left for next session | Exact recipes argument/schema/table, incremental-only rejection, E3 typed integrity result and E5 phase-specific substitute; limitations stated |
| U3 | Incomplete future scope/identities/commands | Fixed W/branch, report pointer/state backup/central execution paths, S subtrees, overlay libraries, complete dependency versions/RemoteShas, safe commands/exclusions |
| U4 | Pin/path/ownership/runtime checks treated as user decisions | Ordinary preflight verifies actual facts, stops on conflict/mismatch/missing resources |
| U5 | Required session restart and repeated approval after cg-work | Same-session direct Work Handoff; branch creation separately named, no commits/pushes |
| U6 | Critic/schema pass treated as sufficient correction evidence | Source/metadata/workflow comparisons, exact finding ledger, closing critic, document validation and final material-diff/readback evidence |
| U7 | Re-review and all P3 not guaranteed | Requested installed critic is read-only; all returned priorities get explicit outer-caller correction/evidence/closure, no accepted/deferred findings |

The attributed readiness critic findings, their exact fixes and closing results
are recorded in the new central evidence and added to this ledger as returned;
no pass is presumed before that review actually runs.

Readiness critic returned 0 P1, 1 P2, 1 P3 after reading all 1060 lines of the
pre-correction revision. All findings selected for correction; none accepted
as risk or deferred. No new user decision was required by those findings.

| ID | Attribution and finding | Fix and substantive verification |
|---|---|---|
| W-P2.1 | cg-plan-critic: private namespace-parented test env did not explicitly bind unqualified testthat symbols | Runner binds public testthat exports and verifies test_that/expect_identical/pd_hash_object identity before sourcing; original tests and installed testthat/pipdata NAMESPACE declarations independently read |
| W-P3.1 | cg-plan-critic: exceptional callback exits/lease cleanup could bypass postguard or mask primary error/feedback | Mandatory finally postguards; separately retained callback/guard/release conditions, cancellation and typed-integrity precedence, ownership-only cleanup; two behavioral fault cases plus cancellation case added; existing lease release and legacy cleanup source independently read |
| O-P3.1 | outer-caller material recheck: Step 6/checklist retained stale generate-help wording despite selected manual Rd route | Replaced both operative references with manual scoped Rd/source parity and no generator; matches discovered installed7.3.3 versus recorded8.0.0 without changing dependency/scope decisions |

Closing critic read all 1113 corrected lines and confirmed W-P2.1 and W-P3.1
closed, no unresolved/new P1/P2/P3, no additional user decision. E1-E6 and direct
Work Handoff remain coherent. This is source/Plan review, not runtime
certification. Actual final no-HTML validation/material readback/revision
identity are recorded in the assigned readiness handoff/evidence.
Outer-caller O-P3.1 was corrected during independent material readback after
that critic pass; its exact text consistency and closing check are likewise
recorded in readiness evidence. No runtime check is implied.

## Work Handoff

Continue in the existing pipdata Code session. On the future explicit work
request, perform normal read-only preflight and no-HTML validation; initialize
only the exact scoped report/state records before implementation. Verify actual
ownership/pins/paths; preserve unrelated changes. No another-session requirement
or D-* approval checklist remains. Missing permissions or actual failed checks
still stop with the specific reason.

Exact eventual user instruction, not executed by this revision:

```text
Create branch feat/m2-pipdata-authority in the existing pipdata checkout
using git checkout -b feat/m2-pipdata-authority if it does not already exist;
if it exists or conflicts, report that specific condition before switching.
Then execute:
/cg-work .cg-docs/plans/2026-10-26-m2-pipdata-authority.md phase1 deviate:ask
Use the Plan's settled E1-E6 decisions, exact work/report/runtime scope and
selected E5 workflow substitute. No duplicate scope approval, new session,
worktree, extra helper/delegation, roadmap edit, commit or push. Actual failed
preflight/checks or denied operations still block. All created files/caches on E:.
```

Phase 1 is baseline/runtime preflight only; it has no Phase-2 test/API dependency.
The normal cg-work phase-boundary offer governs continuing to Phase 2; the user
may select phase2 after Phase 1 is complete without renegotiating this scope.
Full-plan completion requires all selected V/C evidence, not merely the critic
or structural validator. This document-only task has not run any package work.
