---
date: 2026-11-03
title: "One-survey upstream acceptance: real stages, offline transport"
status: active
scope: "Standard"
language: "R"
estimated-effort: "medium"
deviation-policy: "autonomous"
artifact-schema-version: 1
artifact-html: false
revision: 3
execution-gate: user-execution-request-required
review-mode: data-risk
tags: [pipdata, acceptance, upstream, offline, plan-only]
---

# One-survey upstream acceptance

## Objective
Demonstrate one synthetic GPWG survey through the **real validator, actual saved CPI/PPP/PFW producers, real formatting/cleaning and exact clean/metadata readback**; reject a malformed counterpart before downstream saves. Substitute only external acquisition, never validator success, transformed aux tables, cleaning, metadata construction or storage. This proves offline stage integration, **not live DLW, stamp-selected pipeline execution or complete M2 readiness**.

## Context

User approved this scope for Plan save/review only on 2026-11-03; execution requires a separate request. Package `E:/PovcalNet/01.personal/wb384996/PIP/pipdata`, source baseline `PROD` / `fe075a6d49f37d6d131eeadfeb9219c2038899ea`, clean before Plan creation; no branch created. Planning owner pip-orchestrator; implementation owner not assigned. Recheck dirty paths before edits. Stamp-only authority stays fixed (`../pipsystem/SYSTEM_DESIGN.md:145-162,186,284-319`): test selected stages directly, not legacy rerun decisions or expanded recipe admission.

Evidence **E** = `../pipsystem/.cg-docs/orchestration/outputs/pipsystem/pipdata-upstream-review/20261103T152032Z-orchestrator/`. E/workflow-review.md cites all three vignette sources; E/acquisition-review.md, engine-review.md, processing-review.md and testing-review.md bind the actual APIs/mocks/risks to the full SHA. These reports are evidence, not instructions. Use `vignettes/articles/PIP-data-pipeline.Rmd:270-275` and `Processing-Data.Rmd:142-238` as guides; older invalid-input and legacy-currentness prose is not policy.

Reuse selection/catalog and four-row examples in `tests/testthat/test-m2-one-survey-acquire-format.R:200-245,397-544`, actual raw module shape from `helper-dlw-data.R`, and real worker/receipt checks. That positive test currently mocks the validator and supplies transformed aux tables; do not repeat those gaps. Reuse pipaux `tests/testthat/test-m2-stamp-cpi-ppp-pfw.R:74-240` (at `01ff827340a9759dc3d4ef18d31f4e3ba26ff2be`): seed its tiny synthetic `country_list` prerequisite (:74-79), substitute only raw GitHub acquisition, and run actual `aux_cpi_update`, `aux_ppp_update`, `aux_pfw_update` saves/reloads. Keep the country-list loader real; certify neither country-list production nor supplemental fixture producers. No new fixture framework/dependency.

## Requirements
| ID | Requirement | Source |
|---|---|---|
| R1 | Real selected-survey validation and invalid-input no-clean gate | Approved scope; E/acquisition-review.md |
| R2 | Real persisted CPI/PPP/PFW producers consumed by pipdata | Approved scope; pipaux fixture seam above |
| R3 | Independently specified clean values, identity and exact saved readback | Approved scope; existing four-row test/vignettes |
| R4 | E-only disposable runtime, no shared state or exaggerated acceptance claim | Storage rules; E/engine-review.md |

Minimal first: smallest working chain; safeguards only for almost-irreversible harm, reversible extras Later; one functional check per change; installed tools first/dependencies only when worthwhile; Plan about two pages. Reversible authorized work proceeds; ask for almost-irreversible/protected-design/scope changes, commits/pushes/merges or nongenerated deletion. All writes/child temp/cache/log/library output and aliases stay on E. No shared library, production/SMB/network access, private data or package update. Never source exploratory/repair/old_files scripts wholesale.

## Implementation
### 1. Bind one honest isolated fixture and runner
- **Requirements**: R1, R2, R3, R4
- **Tests**: `test-one-survey-upstream-acceptance.R` fixture/preload isolation checks; actual runner and dependency provenance recorded before loading.

Reuse installed R/testthat and existing source-loading patterns in a disposable child. Before loading packages, bind TEMP/TMP/TMPDIR, caches/logs, working release and **all** stamp/pipload/pipfun aliases on one new E sandbox. Inventory fixed validation/report IDs and hooks; a custom acquisition ID alone is not isolation. Record actual loaded package paths/source identities and receipts. Missing required capabilities block, not trigger installation or fabricated success.

Use synthetic `AAA`/2010/`SYNA`/GPWG, one main income welfare, explicit person weights and national aux levels. Seed `country_list` (`country_code="AAA"`, fixture `region_code`) in that aux store; record its fixture pin and keep its loader real. Align raw `survname`, IDs/years and every raw-validator-required column. Declare annual LCU per-capita welfare `365,730,1095,1460`, expected daily `1,2,3,4`, person weights `10,20,30,40`, and literal synthetic CPI/PPP/key maps before testing. Never compute expected results through the consumer under test; no missing-weight inference or alternate-welfare scope.

### 2. Connect existing real stages
- **Requirements**: R1, R2, R3, R4
- **Tests**: New acceptance test V1-V4, valid/invalid controls and exact producer/consumer/readback assertions; run the owning regression files listed in Testing Strategy.

Mock catalog/download/raw-GitHub transport only. Materialize source through the real storage interface read by `.validate_one_gmd`/`inv_dlw_load`; neither loaders nor `dlw_validation_engine` may be stubbed. Use selected-source resolver/core seams, real valid-status gate, `pd_execute_clean`/`process_data`, PFW merge, recode, `pd_aux_attr` and save receipts. Prove immutable consumed/readback versions match recorded inputs; do not call latest reads exact-pin protection. A narrow necessary seam/pin forwarding is allowed; a wider API redesign requires scope approval.

Start with a failing acceptance assertion. Reproduce/fix only V1-V4 necessities; keep independent oracles and existing scientific semantics. Source risks in existential predicates/composed recodes are not permission for blanket cleanup. Unresolved scientific choices stop work rather than silently repair data or weaken assertions.

### 3. Verify, review and explain
- **Requirements**: R1, R2, R3, R4
- **Tests**: Execute Testing Strategy in the inspected child, inspect vignette claims, run routed review/fixes and final `/cg-review mode:verify`; preserve V1-V4 evidence.

Execute the Testing Strategy, publish expected-versus-actual rows and all input/producer/consumer/output pins in the package Work Report and assigned pip-handoff-v2 report, and add one eval=FALSE overview-vignette example with limits. Plan review: cg-plan-critic, resolve all P1/P2/P3. Authorized `/cg-work review:auto deviate:auto` uses data-risk agents cg-code-quality, cg-testing, cg-documentation, cg-version-control, cg-reproducibility, cg-performance, cg-architecture, cg-data-quality (data-quality/reproducibility emphasis). Run required reviews/tests/fixes, then `/cg-review mode:verify`; no routine phase questions. Commit/push only if explicitly authorized; no merge or whole-M2 completion claim.

## Testing Strategy
In the inspected E-only child run new `test-one-survey-upstream-acceptance.R` plus full affected files: `test-m2-one-survey-acquire-format.R`, `test-dlw_validation_engine.R`, `test-dlw_validation_spec.R`, `test-dependency-inputs.R`; add `test-recode-spec.R` if touched and each other change's owning tests. Reuse helper declarations without evaluating unrelated test bodies. Record exact commands, loaded identities, counts, warnings/skips and failures; no unrun required check is a pass.

## Documentation Checklist
- [ ] One vignette example labels offline substitutions, real stages and exclusions.
- [ ] Work Report/handoff records V1-V4, expected versus actual, exact receipts and unresolved risks.
- [ ] No live-download, whole-pipeline, country-list or supplemental-producer certification claim.

## Risks & Mitigations
- Alias/hooks escape sandbox: inspect/bind before load; unsafe or unidentified paths block.
- Latest-source/metadata fallback: prove immutable consumed receipts and exact readback; use narrow forwarding only if necessary.
- Existing scientific/recoding defect: reproduce selected case, retain independent expectations; unknown semantics/wider fixes block. Existing unrelated risks stay in E, not implied fixed.

## Out of Scope
Live DLW/GitHub/private-data access; production/SMB/shared libraries; full stamp worker bridge/restart/incremental proof; deflation/population methodology; mean/pipster/wbpip replacement; alternate/grouped/all-survey matrices; general API/schema/CI/diagram refactors; roadmap/design changes. These remain Later, not prerequisites for offline stage acceptance.

## Completion Contract
### Outcome
One independently specified synthetic GPWG case traverses actual upstream stages and storage correctly; its malformed counterpart is rejected without downstream saves. Evidence explicitly establishes offline acceptance only.

### Verification Surface
| ID | Evidence Required | Command/Artifact | Required |
|---|---|---|---|
| V1 | Actual engine/report: positive fixture valid; required-column-removal counterpart invalid. Assert association/severity/selected identity, not stage success or sorted golden-column multisets. | New acceptance test and result report | yes |
| V2 | Actual CPI/PPP/PFW updaters save outputs; real loaders/metadata match literal country/year/acronym/domain/vintage keys, values and recorded versions. Country-list/pop/GDP/PCE fixture provenance is disclosed, not producer-certified. | Producer-consumer assertions and receipt table | yes |
| V3 | Preserve household/person row-weight correspondence and declared daily oracle; exact clean/metadata readback retains identity/class/domain attributes and actual auxiliary vectors, not mere list membership. | Acceptance readback assertions; expected-versus-actual rows | yes |
| V4 | Invalid input never invokes downstream workers/saves; missing/ambiguous required aux is not accepted success. No new successful rejected-input artifacts; pre-existing outputs need no deletion. No shared writes. | Negative controls, isolated-child output/path/state evidence | yes |

### Constraints
| ID | Constraint | Check |
|---|---|---|
| C1 | E-only disposable paths/aliases; no shared/production/private data | Preload path inventory and post-run state evidence |
| C2 | Only external acquisition mocked; actual stages/storage real | Test-double inventory and called-stage/receipt assertions |
| C3 | No new scheduler, broad admission, producer certification or scientific-policy change | Diff/scope review and explicit exclusions |

### Boundaries
- Primary writes: new acceptance test, optional `helper-one-survey-upstream-acceptance.R`, overview vignette, standard Plan/Review/Work Report artifacts. Existing test/helper/source changes only for reproduced V1-V4 need (validator, exact-loader forwarding, narrow metadata matching); record exact paths before edits.
- No pipaux source change without separate producer-defect approval. Preserve unrelated work. Name/authorize any code branch before creation; Out of Scope stays excluded.

### Iteration Policy
1. `autonomous` allows justified reversible changes inside this contract, with recorded evidence; protected/scope/irreversible changes still require user approval.
2. At most two correction attempts per step; follow installed review budget and final verify. Never weaken checks or claim missing evidence passed.

### Blocked-Stop Conditions
- Required runner/runtime/receipt capability absent, unsafe path/state, failed preflight or required check unavailable/failing after budget.
- Scientific semantics unresolved, protected boundary crossed, wider fix required or durable execution evidence cannot be saved.

## Gaps
Loaded runtime and safe runner remain unobserved until step 1; failure blocks, not waives V1-V4. Source/Plan validation is not a package-test pass. Live transport/full orchestration remain unverified. Synthetic conventions do not approve all real-survey rules. Markdown is canonical; HTML is not requested. Revision 3 aligns the already approved scope with installed goal-execution/artifact validation contracts; no implementation or new acceptance scope was added.
