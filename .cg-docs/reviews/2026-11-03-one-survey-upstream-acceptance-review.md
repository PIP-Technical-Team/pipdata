---
date: 2026-11-03
depth: data-risk
type: standard
plan: .cg-docs/plans/2026-11-03-one-survey-upstream-acceptance.md
findings:
  P2.1: fixed
  P3.1: fixed
---

# One-Survey Upstream Acceptance Review

## Scope and Route
`review:auto` resolved to `data-risk`: scientific consumer outputs, exact
producer/consumer identities, validation and missing-data behavior changed.
Eight route-appropriate reviewers ran once, read-only. Actual pipdata target:
`feat/one-survey-upstream-acceptance` in the main checkout, not the session
worktree. Separately approved stamp scope: `feat/one-survey-class-readback`,
only `R/hashing.R` and its new exact-class regression. Other stamp dirty files
were excluded. Protected artifacts were not destructively changed.

## Findings

### P2.1: Stamp Regression Hardcodes an E-Only Test Host
Sources: cg-code-quality and cg-version-control (deduplicated).
Original location: stamp `tests/testthat/test-one-survey-class-readback.R:25-32`.
The ordinary regression aborted when `tempdir()` was not on E, breaking generic
Linux/macOS/default Windows package checks before roundtrip assertions.

Fixed: use standard `withr::local_tempdir()` in the portable regression. Remove
the redundant environment rewriting/host guard, not any class/hash/key
expectation. The inspected parent runner still binds and verifies E-only paths
for this agent's execution before package loads. No CI/config change.
Executed post-fix owning selection:
`E:/temp/pdu-20261103T1846104771888Z-ca64ebe9/evidence/`: 32 blocks,
544 expectations, zero failed/error/warning/skipped.

### P3.1: Vignette Does Not Identify Its Work Report
Source: cg-documentation.
Original location: `vignettes/articles/PIP-data-pipeline.Rmd:310-314`.
The offline example said "linked Work Report" without a link or path.

Fixed: explicitly identify
`.cg-docs/work-reports/2026-11-03-one-survey-upstream-acceptance.md` beside the
example, making its runner, actual pins and gaps discoverable. The chunk remains
`eval=FALSE` and is not itself runtime evidence.

## Reviewer Results
| Reviewer | Result | Session |
|---|---|---|
| cg-code-quality | P2.1 above; no production-code bug | ses_e5d46d7c2ffe2uLtJsbRrWbqj9 |
| cg-testing | No actionable P0-P3 | ses_e5d46d7b2ffeYoRY3Rhbf2xsgW |
| cg-documentation | P3.1 above; no P0-P2 | ses_e5d46d7abffeBkV9NaRoupflvw |
| cg-version-control | P2.1 above; intended scope/staging boundaries otherwise match | ses_e5d46d7a9ffejoPmY8svBUl8FT |
| cg-reproducibility | No P0-P3 within local offline-stage scope | ses_e5d46d7afffe3josphE16BFRRN |
| cg-performance | No material P0-P3 performance regression | ses_e5d46d7adffepIGuDzJnn7dQ3t |
| cg-architecture | No meaningful P0-P3 within approved boundary | ses_e5d46d7acffewRCyWYtK1StP5s |
| cg-data-quality | No P0-P3 data-quality finding | ses_e5d46d7b0ffelKhI2ZNMDRSd1z |

Reviewers inspected source and retained CSV evidence; they did not independently
run R/tests, deserialize RDS or recompute its numerical/hash results. Coordinator
execution evidence is in the Work Report: acceptance 174, pipdata owning
regressions 411, stamp owning regressions 544 expectations passed.

## Boundaries and Next Check
No P0/P1, no skipped findings, and both actionable findings fixed. This review
does not certify live transport, full orchestration, incremental/restart behavior,
supplemental producers, deflation/mean or full M2 readiness. Local rerunnability
depends on the retained inspected E runner/snapshot/sources; no portable archive
claim is made. Finish with staged `/cg-review mode:verify` (light: code-quality
and testing only), retaining P0/P1 and cross-file-breakage reporting strength.
