---
date: 2026-10-28
depth: light
parent-review: .cg-docs/reviews/2026-10-27-m2-pipdata-authority-review.md
type: verification
findings:
  P0.2: open
  P1.4: open
  P2.14: open
---

# M2 Merge-Readiness Verification

## Verdict

NOT CONVERGED. Candidate is not established merge-ready. Three deduplicated
findings remain: P0:1, P1:1, P2:1, P3:0. Source corrections and new runtime
verification are outside this task's authorization and were not performed.
The earlier review-verify.md remains unchanged as historical evidence; its clean
claim does not override this genuine native-selected verification result.

## Native Selection And Dispatch

Outer caller repaired only standard-report tracking/schema/crosswalk under direct
authorization, preserving its original A1-A17 descriptions and attribution.
Confirmed parent exists. Applied installed cg-review Step1.7 fixed-map selection,
descending date then filename, excluding verify reviews. Selected this exact
parent from18 eligible reports, initially17 fixed IDs. Fallback=false. Forced
light, no risk-class broad dispatch. No staging required or performed.

Only cg-code-quality (ses_e7d616f86ffeJ10SamErJxq1Rn) and cg-testing
(ses_e7d616f83ffe3M92YwNqyJueOg) dispatched. Both outputs usable; no retries,
additional helpers, model overrides, writes or package processes by reviewers.
Suppression explicitly included: always report P0/P1 and cross-file breakage;
suppress P2/P3 only within the prior map's explicit fixed scope; doubt=>report.
R package/infrastructure/testing skills required. .Rbuildignore:10 excludes
.cg-docs. Local config absent. Nine changed candidate source/test/doc files
reviewed, with relevant dependency/schema/runtime evidence as read-only context.

## P0: Blocking

### P0.2 / A2 — Nested Implicit Dispatch Escapes Frozen Proof

Attribution: cg-testing. R/code_fingerprint.R:340 guards only top-level
is.object(args). lengths remains permitted at line180, and
`lengths(list(value))` can invoke a registered length.Date method for a Date
constant accepted by pd_engine_value. The top-level operand is an unclassed list.
Changing that ambient method can change the calculation without changing any
declared body/constant/policy descriptors; stamp may skip the stale result.
The direct Date-operator test does not exercise this nested dispatch.

Static source-derived reproduction, NOT executed here:

```r
function(path, parents) list(x = lengths(list(value)))
# constants = list(value = as.Date("2026-01-01"))
# Change registered length.Date between identical requests.
```

Required next action: separately authorize nested dispatch rejection/binding
for retained recursive operations, plus method-mutation regression with cleanup
proving rejection or truthful invalidation. Prior P0.2 tracking reopened; other
16 prior entries retain their saved fixed state. This is provenance correctness,
not a demand to support arbitrary untrusted R code or a sandbox.

## P1: Critical

### P1.4 — Admitted Pipload API Can Fail After Publication

Merged at highest severity: cg-code-quality local P1.1 and cg-testing local
P2.13 (missing compatibility assertion). R/dependency_execution.R:177-178 always
requires pip_read(metadata_only=TRUE), but DESCRIPTION:25 accepts
pipload>=0.2.0.9005. Sibling ../pipload is1.0.0 (DESCRIPTION:2) and defines
pip_read at R/pip_read-write.R:48-54 without metadata_only or ... . The tested
adapter is also1.0.0 but has the capability. Version1.0.0 alone cannot distinguish.
No capability rejection precedes st_rebuild at dependency_execution.R:148, so
durable publication can occur before the unsupported validation call fails.
Integrity handler prevents normal success, not the avoidable prior write.

Required next action: separately authorize capability validation before lease/
planning/publication or a distinguishable dependency release/contract, plus
consumed pipload/stamp API assertions and negative regression requiring zero
callback/publication on unsupported capability. Do not just raise minimum1.0.0.
Sibling checkout is compatibility counterexample, never a substitute tested pin.

## P2: Important

### P2.14 — Nonempty M2 Route Requires Suggested Codetools

Attribution: cg-code-quality local P2.1, NOT prior P2.1/A6. The new route always
calls codetools::findGlobals (R/code_fingerprint.R:274); DESCRIPTION:35-38 lists
codetools under Suggests only. Required-dependency-only installations may load
pipdata yet cannot prepare nonempty M2 recipes. The saved runtime includes
codetools0.2-20 and cannot prove missing-package behavior.

Required next action: separately authorize declaring the runtime dependency or
an explicit actionable optional-feature dependency gate/documentation; never
bypass fingerprint proof. Verify selected behavior in an isolated environment.

## Prior Dispositions And ID Reconciliation

16 prior entries have present corrections with no newly reported defect in
their fixed scopes. P0.2/A2 is partially unresolved and now open in the parent.
P1.4 merges two critics' same underlying API-contract issue; its missing-test
aspect is included, not dropped. P2.14 avoids collision with prior P2.1/A6 and
testing-local P2.13. Parsed3 unique canonical open IDs, matching total3 findings.
Nothing fixed or skipped by this task; no autofix in mode:verify.

## Current Candidate And Historical Test Binding

Outer caller independently hashed all182 package source-manifest rows against
both current W and retained S/source/phase2/pipdata:0 missing/mismatched. Saved
phase2-before-gate.json SHA256
0A8A19B2B8C2390EEF5B97D91039B46ED32A37859FF87B8AAC1D548550166B68.
Eight changed runtime source/test/Rd paths match. Article is not in this manifest;
it is documentation-only review, not executed/rendered test proof. Full per-file
hashes and self-contained dependency/results evidence saved centrally below.

Stamp fixed content5d1ddebdc926dad9c6ec99c08a65e74baee0da92:100 source/100 normalized
blob bindings/21 installed,0 mismatches. Pipload commit
4ef0d90a974cb6ff0725da69373bde421dd04ad5:87 source/87 blob bindings/31 installed,
0 mismatches. Candidate installed30 files and archive
93137883660F3846707CA6611B9953BDDFDA13FC0FAD6E32E3409A12434B68FE match saved records.

Saved historical gate20261027T2136114632065Z: exit0,41tests286expectations.
Saved build2137167145213Z/install2137185099372Z and installed smoke2137270168486Z:
exit0, no timeout; engine built/skipped/empty and rds/qs2/fst exact checks.
Records/logs read, not rerun. This task executed no R/test/build/install process.
Current HEAD/base84442e979c98d33fa5565ab9e56d3179cc7d5278 is not newly tested;
binding is to dirty candidate bytes. No merge SHA, stage, commit or push.

## Saved Attributed Outputs

Central root:
`E:/PovcalNet/01.personal/wb384996/PIP/pipsystem/.cg-docs/orchestration/outputs/pipdata/merge-readiness/20261027T220915Z-code-review-repair/`.
Outer caller saved handoff.md, evidence.md, cg-code-quality.md and cg-testing.md.
Evidence includes actual hashes/results and all41 E5 excluded filenames, not
merely links. Report repair and verification outputs are the only owned writes.
Earlier verification SHA256 unchanged:
DF8AE57CA11D9A8F4C5F1EE0C4FD499E60DD266466DE9D219A4F37804A5A4638.

## Gaps And Merge Risk

No full-suite/package-check/production/SMB/multiwriter/scale/umbrella claim. E5
excludes41 existing test files covering full legacy/numerical/inventory/C2/
logging/acquisition/validation; full examples/vignettes and producer checks also
excluded. Pinned successful execution cannot establish arbitrary dependency
compatibility. Article runtime binding, absence-of-codetools behavior and nested
method-mutation reproduction remain unexecuted. No independent old-log attestation
or current entire transitive environment verification. Source fixes need approval.

## Model Advisory: Review To Fix Triage

Capability profile: independent provenance/correctness reasoning and targeted
dependency/API diagnosis. Strong option: a high-capability reasoning reviewer
at high effort; economical option: a capable coding reviewer at medium effort
for bounded metadata/capability work, not as substitute for the P0 analysis.
Rationale: method dispatch can silently falsify currentness; API mismatches cross
durable write boundaries. Capability-only suggestions, no execution settings;
availability differs by platform/date and the user chooses model and effort.
