---
date: 2026-11-02
title: "M2 Phase 3: Independently Save the Mean"
status: active
scope: "Standard"
brainstorm: null
language: "R"
estimated-effort: "medium"
deviation-policy: "autonomous"
artifact-schema-version: 1
artifact-html: false
revision: 2
execution-gate: blocked
tags: [m2, pipdata, mean, recovery, plan-only]
---

# Plan: M2 Independent Mean Save

## Objective
Deliver the coordination/storage part of umbrella Step 8 / E8 / L-calc / V10: supply approved deflated welfare and final weights to pipster, independently save each successful mean, then recover after an injected downstream assembly failure without recomputation when stamp says the saved result is current. Plan-only approval is not execution or contract approval.

## Context
Root: `E:/PovcalNet/01.personal/wb384996/PIP/pipdata`; Plan-save branch/base: `PROD` / `7f4a98471d9c2350c3537fcae8448856896cd9f9`. Planning owner: pip-orchestrator; implementation owner UNKNOWN. User approved the three Plan-only saves on 2026-11-02. Preserve the pre-existing October 30 Plan and `.tmp/`. No branch operation is approved.

This unit follows `2026-11-02-m2-deflate-one-survey.md` in this directory and `../pipster/.cg-docs/plans/2026-11-02-m2-pure-weighted-mean.md` relative to the package root. One pipdata owner sequences shared entry-point/fingerprint/test edits; do not run overlapping workers. Canonical PROD is behind local `origin/PROD` `f06acb9c557aa1866bab965ffdfe86af22a636fe`, which contains Phase 2 `57006c2f39fc6f3e03413515ee941d122e44c6dd` and a receipt fix. The old Phase 2 worktree is absent. D8 must name a feature-containing execution base; do not silently checkout, merge or assume canonical source has the prerequisites.

Authority: `../pipsystem/SYSTEM_DESIGN.md:166,186-205,290-309`; umbrella `../pipsystem/.cg-docs/plans/2026-10-10-m2-walking-skeleton.md:174,231-236,342-352,415-429`. Evidence **E**: `../pipsystem/.cg-docs/orchestration/outputs/pipsystem/m2-phase-3-readiness/20261102T135331Z-orchestrator/`. Source audit E/`pipdata-readiness.md:76-94,114,158-191` records the real recipe interface and its calculation-call restriction at `57006c2`; relevant engine/input/fingerprint sources have no delta to the observed newer ref. E/`pipster-storage-readiness.md:100-118` records public storage APIs, not an execution pass.

Observed candidates: stamp `4a1fa5d92313aa223296fae0e29cf3446a314aea`; pipload adapter `4ef0d90a974cb6ff0725da69373bde421dd04ad5`; pipaux now `4b59a60d846a91c513d17f59629f8dc509867e78`. Pipster baseline is `828064e406e07e711c34d2ba725746e367d35f9b`; a tested mean producer pin does **not** yet exist. Required incoming evidence: completed local deflation and pure-mean handoffs, exact tested commits/source deltas and output/metadata pins. Default installed metadata and the old overlay are not this evidence; reconcile the earlier stamp identity conflict before use.

## Required decisions before execution
| Gate | Missing approval / proposed bounded answer, not a decision |
|---|---|
| D1-D3 | Approved validated input/selection, vintage/unit/consumed-input evidence and aligned final weights, including deflation's mandatory synthetic population-on control even if the primary fixture is unadjusted. Population policy/pins come from that producer, not a new policy here. |
| D4 | Proposed pure result `list(mean=<double>)`; independently stored result is one data.table row per module-bearing artifact / PPP year / approved reporting level / statistic `mean`, with scalar double `value` and explicit unit. Propose RDS and tolerance 1e-12. Exact column names/types/nulls, artifact-path mapping, unit tag, failure schema and semantic metadata still need approval. No global mandatory long table. Preserve existing incremental-only feedback; do not add Force behavior. |
| D8 | Tested producer pins, sequential owner/base, exact edit/lifecycle sets, E-only one-store layout and safe full suite/substitute; exact narrow pipster/collapse admitted-call and fingerprint coverage after deflation's adapter review. |

## Minimal first and autonomy
Build the smallest end-to-end save/recovery path. Add safeguards only for almost-irreversible loss of data/work/history, wrong externally released numbers, private-data/credential exposure, or production/shared-branch changes; defer recoverable extras under Later. Each change gets a functional check. Use installed tools; add a dependency only when it saves more work than it costs. Keep this Plan about two pages. Reversible work proceeds; protected design/scope, near-irreversible actions, commits/pushes/merges and nongenerated-file deletion require approval. No new scheduling authority follows from autonomy.

## Requirements
| ID | Requirement | Source |
|---|---|---|
| R1 | Supply aligned approved vectors for dynamically discovered PPP years; pure producer owns arithmetic | E8; design package boundary |
| R2 | Each successful mean is saved independently before downstream assembly; exact pin/receipt reads | Design independent results; umbrella V10 |
| R3 | Actual storage/recovery, failure/no-save, fingerprint and API compatibility evidence | Umbrella L-calc; D8 |

## Implementation Steps
### 1. Compose approved mean recipes through existing authority
- **Requirements**: R1, R2
- **Files**: `DESCRIPTION`, new internal `R/pd_mean_result.R`, `R/code_fingerprint.R`, `R/pd_run_pipeline.R`; `tests/testthat/test-code-fingerprint.R`, `test-dependency-api-contract.R`.
- **Tests**: `tests/testthat/test-code-fingerprint.R`, `test-dependency-api-contract.R` and Step 2's new real-producer/storage test, only under the D8-approved runner.
- After producer evidence and D4/D8 approvals, add the actual pipster dependency and confirm no hard Imports cycle. Pipload's existing suggested reverse deflation convenience path is not a reason to call it from this route. Select only `^welfare_[0-9]{4}$`, explicitly group/filter approved reporting levels, preserve final weight/welfare row alignment and call the approved pure producer once per independent result. No second estimator in pipdata.
- Extend only the already reviewed calculation adapter for the exact producer/collapse implementation and known helpers/constants. Source SHAs are audit provenance, not blanket package invalidation. Submit input pins/fingerprints to existing stamp requests before numerical execution. Use `st_plan_rebuild`/`st_rebuild` and their genuine execution receipts; do not fabricate success via raw saves or catalog-only receipts. One builder returns each mean to the independent save layer before downstream work. Use public `pip_read(..., version=..., metadata_only=TRUE)` for exact matching sidecars and version-pinned payload reads; no private snapshot paths or latest fallback.
- **Checks/acceptance:** proposed 2.5 oracle and declared stored identity/unit/type, multiple discovered years, distinct results and usable pins, relevant helper change and unrelated-source no-change. Independently of the primary fixture's adjustment choice, reuse the D3-approved population-on control from deflation: change one area's pinned population, retain identical welfare, and prove changed aligned weights alter the national mean and its saved provenance. Candidate two-area oracle: welfare `c(1,3)`, final weights `c(1,3)` -> mean 2.5; population-only change to final weights `c(3,3)` -> mean 2.0. Keep this mutation case separate from unchanged-input assembly recovery. Invalid producer result creates no successful saved result; preserve existing entry-point/defaults and feedback.

### 2. Prove independent save and assembly-failure recovery
- **Requirements**: R1, R2, R3
- **Files**: new `tests/testthat/test-m2-mean-save.R`, `tests/testthat/test-m2-engine-authority.R`; generated `man/pd_run_pipeline.Rd` only if changed documented contracts require it.
- **Tests**: The two files above plus the D8-approved suite; mandatory real save/recovery cases cannot be waived by targeted passes.
- Use actual approved deflation, actual pure mean and real stamp/pipload adapters in an isolated initialized E store. Count producer calls/save attempts and retain exact payload/metadata/receipt pins. After successful independent mean save, inject a failure in a small downstream assembly callback; no real release/assembly framework is needed.
- **Checks/acceptance:** next analyst-started execution receives a stamp-current mean decision, verifies/loads the retained saved result and recovers downstream work with zero additional mean calls or mean-version writes. A failed save, missing/corrupt payload, absent snapshot/metadata or unavailable parent is not reusable; follow stamp's actual run/blocked outcome. Test invalid-result zero-save and unrelated successful-result retention. Then run the approved suite and review/fix/verify loop. Keep deterministic versus external failure handling within existing approved semantics; do not add retries or Force overrides.

## Testing Strategy
Proposed root: `E:/PovcalNet/01.personal/wb384996/PIP/pipdata/.tmp/m2-phase3-mean-save`, separate `source`, `library`, `temp`, `home`, `cache`, `store`, `logs`; never share a live alias/store or an analyst R process. All recipe parent/output paths must fit the initialized fixture root; acquire new synthetic pins there or approve transfer semantics, not outside-root bypasses. Before package loading inspect current hooks/helpers/teardown and redirect TEMP/TMP/TMPDIR, HOME/R_USER, user/config/cache/log/PIP outputs and explicit libraries to resolved E paths. Bind actual source and dependency builds, not default installed packages. Use installed tools and fresh child R processes; no shared-library upgrades.

No safe full-suite command or substitute is approved. D8 must record the exact environment, executable, command and exclusions before implementation. Mandatory local files include new `test-m2-mean-save.R` and the three existing test paths in Steps 1-2, plus approved deflation/Phase 2 regressions. The conditional commands in E/`pipdata-readiness.md:195-247` are leads, not a gate or permission to run; the historical canonical-copy/build runner is not suitable unchanged. Record exact commands/loaded identities/results/skips; targeted passes do not waive required local recovery or final T1-T12 evidence.

## Completion Contract
### Outcome
Independently pinned mean results survive injected downstream assembly failure and are reused without recomputation when stamp certifies currentness. Only Step 8's local evidence is complete; no TEST-release registration or full M2 success is implied.
### Verification Surface
| ID | Evidence Required | Command/Artifact | Required |
|---|---|---|---|
| V1 | Scientific/result/D8 approvals, tested producer/input pins, owner/base and safe gate | Execution preflight; currently missing | yes |
| V2 | Real pure result/independent save, exact identity/units/metadata/fingerprints and mandatory population-adjusted national-mean control | Step 1 oracle and adapter tests with actual producer/storage | yes |
| V3 | Assembly failure then stamp-current recovery: zero extra mean calls/versions; missing/corrupt/failed outputs not reused | Step 2 real-storage counters, decisions, pins and reads | yes |
| V4 | Approved suite, API compatibility, all P0-P3 findings verified closed | Execution/review/verify reports | yes |
### Constraints
| ID | Constraint | Check |
|---|---|---|
| C1 | Stamp alone selects work; pipster has no I/O; no shadow mean/scheduler | Decision traces, producer/source review |
| C2 | Current API/LCU behavior and one-root E-only isolated storage remain intact | Compatibility checks and runner/path evidence |
### Boundaries
Allowed **now**: this Plan and Plan-review evidence only. Future candidate files are exactly Steps 1-2; no default edits to `R/save_pip.R` or other repositories. Proposed lifecycle, awaiting D8: `.cg-docs/work-reports/2026-11-02-m2-independent-mean-save.md`, matching review/verify reports under `.cg-docs/reviews/`, `.cg-docs/active-state/current.json`, command-permitted Plan progress fields. Check collisions/current edits first; re-review declared additions rather than silently expanding the set.
### Iteration Policy
1. Resolve approvals and producer gates before `/cg-work`; after explicit execution authorization run sequentially after deflation's pipdata unit. Allow two correction attempts per step; resolve all findings including P3 without weakening or dropping required checks.
2. Use review:auto; result-schema/adapter changes route to full: code-quality, testing, documentation, version-control, reproducibility, performance, architecture, data-quality, learnings-researcher, adversarial. Finish `/cg-review mode:verify`; no commit/push/merge or production install inferred.
### Blocked-Stop Conditions
Missing producer evidence/result/D8 approval; unreviewed calculation capability; unsafe suite/runtime; unavailable pin; denied path/dirty target conflict; extra scope, protected-design change or exhausted correction budget. D7/M1 R1 is required before any new selective-invalidation mechanism or final T3 claim, not replaced by a whole-file-parent test here.

## Documentation Checklist
- Synchronize only changed contracts; retain exact input/output/receipt pins, command results and review closure.
## Risks & Mitigations
Result saved only during assembly: independent builder and failure injection. Stale/corrupt reuse: exact receipt/payload/metadata checks. Omitted population/helper dependency: pins and fingerprint mutations. Package cycle/shared-file conflict: dependency inspection and sequential owner.
## Out of Scope
Real assembly/release manifest, M1 R1 selection, more statistics, retry/Force policy, full API ingestion and production/SMB certification.
## Later
Broader estimate assembly and release lifecycle; no extra framework for local recovery.
## Gaps
Tested deflation/mean producer pins, D1-D4/D8, stored contract/adapter specification and safe suite/owner remain unresolved; no execution, numerical recovery or implementation evidence exists for this Plan.
