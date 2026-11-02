---
date: 2026-11-02
title: "M2 Phase 3: Deflate One Survey"
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
tags: [m2, pipdata, deflation, plan-only]
---

# Plan: M2 Phase 3 Deflation

## Objective
Deliver umbrella Step 7 / E7 / L-calc / V9: deflate one validated, pinned synthetic survey into dynamic `welfare_YYYY`, retain LCU/day welfare, and save/read the actual output with exact consumed-input and final-weight provenance. This Plan is saved and reviewed only; no implementation is approved.

## Context
Root: `E:/PovcalNet/01.personal/wb384996/PIP/pipdata`. Plan-save branch/base: `PROD` / `7f4a98471d9c2350c3537fcae8448856896cd9f9`; owner: pip-orchestrator (planning), implementation owner UNKNOWN. User approved the three Plan-only saves on 2026-11-02. Existing October 30 untracked Plan and `.tmp/` are unrelated and preserved. No branch operation is authorized.

Authority: `../pipsystem/SYSTEM_DESIGN.md:162-168,186-205,290-319` and `../pipsystem/.cg-docs/plans/2026-10-10-m2-walking-skeleton.md:173,229-236,330-340,415-429` (umbrella, not an execution target). Evidence below is under `../pipsystem/.cg-docs/orchestration/outputs/pipsystem/m2-phase-3-readiness/20261102T135331Z-orchestrator/` (**E**).

Source inspected at `57006c2f39fc6f3e03413515ee941d122e44c6dd`: `R/pd_deflation.R:338-363,674-928,1012-1078`, `R/pd_aux_attr.R:129-210`, `R/code_fingerprint.R:22-37,189-419`; audit E/`pipdata-readiness.md:76-114`. Its former worktree is now absent. Local `origin/PROD` is `f06acb9c557aa1866bab965ffdfe86af22a636fe`, containing that feature and a receipt fix; the cited deflation/input/fingerprint/entry-point files have no delta between these commits. Canonical PROD is behind; no fetch or live merge verification is claimed. D8 must select a feature-containing execution base and explicitly authorize any branch operation before mutation.

Observed dependency candidates, **not Phase 3 tested pins**: stamp `4a1fa5d92313aa223296fae0e29cf3446a314aea`; pipload `4ef0d90a974cb6ff0725da69373bde421dd04ad5`; pipaux feature now `4b59a60d846a91c513d17f59629f8dc509867e78` (supersedes original `798242c91f661d259bc2c45cce032b6a93bb1d6b`). Verify producer evidence and loaded bytes before use. Historical overlay stamp `0.0.11` / `5d1ddebdc926dad9c6ec99c08a65e74baee0da92` conflicts with the October 30 Plan's label; E/`pipdata-readiness.md:116-136` is historical, not a current runtime finding.

## Required decisions before execution
| Gate | Missing approval / proposed bounded answer, not a decision |
|---|---|
| D1 | Explicit synthetic selection, coverage, annual-LCU source and validated-clean artifact/metadata pins; no inferred real survey or live download. |
| D2 | One explicit PPP release/adaptation per approved year; exact CPI/PPP/PFW keys and independent levels; required-year/missing/ambiguous-match policy. Propose rejecting missing/ambiguous coverage and finite nonpositive divisors. Establish currency/spatial adjustment evidence; never infer `ccf` multiplication. |
| D3 | Primary input needs population adjustment or not; welfare/weight admissibility, NA/zero/normalization, survey-ID versus observed year, area/tie matching and population pins. Also approve the mandatory separate synthetic population-on control if the primary fixture is unadjusted. National output is not evidence of no population adjustment. |
| D8 | Owner, execution base, exact edit/lifecycle sets, loaded dependencies, E-only runner and full-suite gate or directly approved compatible substitute. Also approve the narrow admitted calculation-call/fingerprint contract and one-store input layout; current recipe proof rejects real data.table/S3/external calls. |

## Minimal first and autonomy
Build the smallest end-to-end version. Add safeguards only for almost-irreversible loss of data/work/history, wrong externally released numbers, exposed private data/credentials, or production/shared-branch changes; put recoverable extras under Later. Give each change a functional check, use installed tools, add dependencies only when they save more work than they cost, and keep this Plan about two pages. Reversible work proceeds without asking; protected design/scope, the listed near-irreversible actions, commits/pushes/merges and nongenerated-file deletion need approval. Plan-only approval grants none of those actions.

## Requirements
| ID | Requirement | Source |
|---|---|---|
| R1 | Exact approved consumed inputs, dynamic years, aligned final weights and retained LCU/day. | E7 / V9; design Stage 4 |
| R2 | Actual transformation/storage through the existing stamp-authoritative engine; complete known fingerprints. | Design sections 6/10; E/`pipdata-readiness.md:90-114` |
| R3 | Numerical, provenance, compatibility and safe-suite evidence; no inferred selectivity. | Umbrella L-calc / D8; M1 R1 remains Open |

## Implementation Steps
### 1. Resolve inputs and deflate on the bounded M2 route
- **Requirements**: R1, R3
- **Files**: `R/pd_deflation.R`, `R/pd_aux_attr.R`, `R/dependency_inputs.R`; `tests/testthat/test-pd-deflation.R`, `test-adjust-population.R`, `test-dependency-inputs.R`.
- **Tests**: The three test files above under the D8-approved runner; exact full-suite gate remains blocked.
- Reuse validated clean/domain/core interfaces; clean welfare is already LCU/day, so do not divide by 365 again. Audit actual row/year/level/vintage keys and values, including an explicit adjusted/not-adjusted currency/spatial statement. Keep full-source pins separate from consumption evidence; do not add a selective cache/projection artifact or scheduler.
- On the M2 internal route only, emit one `welfare_YYYY` per approved vintage/year, keep aligned LCU `welfare` and existing `welfare_lcu` where required; do not break legacy public naming/signatures. Preserve row alignment through sorting. D3 approves complete population year/area matches and final weights; population is not a welfare divisor. Regardless of the primary fixture's adjustment choice, retain one approved synthetic population-on control: change only a pinned area population, execute actual deflation/storage, and assert unchanged LCU/PPP welfare, exact changed aligned weights and population/rule provenance on readback.
- **Checks/acceptance:** declared oracle LCU/day `c(8,24)`, CPI 2, PPP 4 yields `c(1,3)`; added year, independent national/area mappings, duplicate/missing vintage/year/level, zero/nonfinite divisor and approved invalid weight/population cases. A consumed CPI change alters arithmetic; an unused area/year CPI change leaves values unchanged, even if audit/source pins change (no selective-cache claim). Reject invalid cases before successful save; assert exact LCU retention and rows, not table-or-NA success.

### 2. Admit the actual calculation and prove pinned storage
- **Requirements**: R1, R2, R3
- **Files**: `R/code_fingerprint.R`, `R/pd_run_pipeline.R`; `tests/testthat/test-code-fingerprint.R`, `test-m2-engine-authority.R`, `test-dependency-api-contract.R`. Conditional generated docs: `man/pd_deflation.Rd`, `man/pd_run_pipeline.Rd` only for changed documented contracts.
- **Tests**: The three adapter/fingerprint/API files above plus the D8-approved suite; no package test is approved now.
- First record the approved narrow adapter surface and known dependency proof; do not broadly whitelist qualified/S3 calls. Cover `get_welfare_ppp`, reused attribute/domain helpers/constants and external arithmetic implementation. Use existing request-aware stamp selection/rebuild/save receipts and public pipload exact-version reads. All recipe parents/outputs must fit one initialized isolated E store; unrelated existing stores require an approved transfer contract, not path bypasses.
- **Checks/acceptance:** actual numerical worker and real pinned payload/same-version metadata round trip; relevant callback/helper/rule mutation changes requested fingerprint, unrelated source edit does not; invalid input has zero successful artifacts; public defaults/legacy API remain intact. Then run the approved suite and review/fix/verify loop. No mean, assembly or release is needed for this local gate.

## Testing Strategy
No package test command is approved yet: D8 blocks implementation, not just completion. Proposed isolated root: `E:/PovcalNet/01.personal/wb384996/PIP/pipdata/.tmp/m2-phase3-deflate` with separate `source`, `library`, `temp`, `home`, `cache`, `store`, `logs`. Inspect hooks, helpers and teardown first; before loading anything, bind TEMP/TMP/TMPDIR, HOME/R_USER, user/config/cache/log/PIP paths and explicit R libraries to resolved E paths. Use fresh child R processes and installed tools; no shared-library update or C fallback. Record exact executable, commands, loaded paths/versions/source identities, outcomes and every skip. E/`pipdata-readiness.md:195-247` has conditional focused recipes, **not a safe full-suite approval**; do not reuse the old canonical-copy/build runner. The D8 record must replace this pending gate with an exact safe full-suite command or approved substitute/exclusion list that includes every required local case.

## Completion Contract
### Outcome
Approved fixture arithmetic, dynamic welfare columns and provenance survive real stamp/pipload save/readback; this completes only Step 7's local evidence, not M2 or T3.
### Verification Surface
| ID | Evidence Required | Command/Artifact | Required |
|---|---|---|---|
| V1 | D1-D3/D8 approvals, owner/base, tested producer identities and safe gate | Execution preflight record; none established by saving this Plan | yes |
| V2 | Exact arithmetic, dynamic years, mapping/CPI mutation cases, LCU/row alignment and mandatory population-only control with pinned final-weight provenance | Step 1 strict numerical tests and actual saved control/readback | yes |
| V3 | Actual save/readback, invalid no-save, complete curated fingerprints and API compatibility | Step 2 adapter/receipt tests | yes |
| V4 | Approved suite plus route-aware review and all findings verified closed | Execution report, review and verify reports | yes |
### Constraints
| ID | Constraint | Check |
|---|---|---|
| C1 | Stamp alone selects work; no R1 mechanism or new scheduler | Decision traces, adapter/source review |
| C2 | No PPP-as-LCU relabeling, legacy API break or non-E/shared access | Compatibility tests, loaded paths and runner logs |
### Boundaries
Allowed **now**: this Plan and its Plan-review evidence only. Future candidate edits are exactly the two step sets; other files require a declared D8 revision. Proposed lifecycle: `.cg-docs/work-reports/2026-11-02-m2-deflate-one-survey.md`, matching `-review.md`/`-verify-review.md` under `.cg-docs/reviews/`, `.cg-docs/active-state/current.json`, and only command-permitted Plan progress fields, all awaiting D8. Check collisions and current edits before any future write. One pipdata owner completes this unit before the independent-mean-save Plan edits shared files.
### Iteration Policy
1. Do not start `/cg-work` until every applicable gate is explicitly resolved. After execution authorization, run both steps sequentially; allow two correction attempts per step and resolve all review findings including P3 without weakening required checks.
2. Use review:auto; schema/adapter changes route to full: code-quality, testing, documentation, version-control, reproducibility, performance, architecture, data-quality, learnings-researcher, adversarial. Finish with `/cg-review mode:verify`; no commits or publication inferred.
### Blocked-Stop Conditions
Missing scientific/adapter/D8 decision, unsafe suite, denied path, dirty target conflict, absent tested pins, extra write-set requirement, protected-design change or exhausted correction budget stops affected work. No waiver of required numerical/storage evidence or T1-T12 follows from local completion.

## Documentation Checklist
- Synchronize only changed documented API contracts; save exact approvals, loaded pins, commands/results and review closure.
## Risks & Mitigations
Vintage collisions/incorrect units: explicit D2 and oracle. Hidden weight dependence: D3 and population-only case. False engine reuse: narrow proof and real storage. Stale runtime: bind/test exact loaded builds.
## Out of Scope
M1 R1 selection, full survey selection, population producer, mean/release/assembly, production API conversion and SMB/parallel certification.
## Later
Broader selection, numerical equivalence and runtime generalization; no extra framework for this fixture.
## Gaps
D1-D3/D8, actual input pins, adapter specification, safe suite and execution ownership remain unresolved; no R or Phase 3 implementation evidence exists for this Plan.
