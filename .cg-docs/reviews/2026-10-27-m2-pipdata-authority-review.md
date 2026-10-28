---
date: 2026-10-27
type: standard
depth: full
status: open
review-mode: full
plan: .cg-docs/plans/2026-10-26-m2-pipdata-authority.md
scope: uncommitted
findings:
  P0.1: fixed
  P0.2: open
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: fixed
  P2.6: fixed
  P2.7: fixed
  P2.8: fixed
  P2.9: fixed
  P2.10: fixed
  P2.11: fixed
  P2.12: fixed
---

# M2 Pipdata Authority Automatic Review

User requested `review:auto` and final `cg-review mode:verify`; this report is
the fixed-findings input for that explicit verification command. Scope remains
the canonical Plan E1-E6. No commits/pushes or production activation.

## Dispatch And Evidence

Full route selected for proof-schema/lease/API boundaries. Ten read-only critics
completed: code-quality, testing, documentation, version-control, reproducibility,
performance, architecture, data-quality, learnings-researcher, adversarial.
Version-control and performance found no actionable issues. Other findings below
are deduplicated, not discarded. Completed results recovered through local recall
after callback delivery did not surface all results in the parent transcript;
board snapshot confirmed all ten complete. No reviewer implemented changes.

Baseline:13tests62expectations; original candidate31tests190expectations. After
boundary/test corrections40tests257expectations and built/installed public smoke
passed: S/evidence/gate-phase2-20261027T2125412139225Z-native.json and
installed-phase2-20261027T2126521087814Z-native.json. Later metadata-presence,
published-legacy-fixture and explicit-read-format additions await the final gate.
S is E:/PovcalNet/01.personal/wb384996/PIP/pipdata-m2-engine-runtime.

## Findings And Disposition

| ID | Priority | Critics / original location | Fix / regression | Status |
|---|---|---|---|---|
| A1 | P0 | data-quality/reproducibility; code_fingerprint.R qualified reads | Guard qualified reads against effective declared parent path/exact pin, explicit format; reject omitted/NULL/selectors/undeclared artifacts before I/O; real exact-parent positive and negative cases | fixed |
| A2 | P0 | adversarial/code-quality/learnings; ambient timezone/locale/implicit S3 | Reject ambiguous conversion/generic surface; base-operation boundary refuses classed operands; remove implicit replacement dispatch; hash read/base/closure policy code; timezone/S3-method regressions | fixed |
| A3 | P1 | architecture/data-quality/adversarial; dependency_execution.R lease destination | Resolve full lease destination including owner path before acquisition; guard resolved lease path; real pre-existing E-to-E escaping junction test proves no outside write | fixed |
| A4 | P1 | adversarial; explicit cancellation | Use existing cancellation predicate, latch original condition, prevent later calculations, propagate original after cleanup with feedback/secondary causes; explicit cancellation and original interrupt tests | fixed |
| A5 | P1 | reproducibility; run.ps1 gate identity | Before/after every native child assert retained producer source/install SHA256 manifests; missing or drift fails, never recapture gate identities | fixed |
| A6 | P2 | reproducibility; ordinary overlay shadowing | Reject unexpected overlay packages; assert loaded ordinary namespace paths/versions against checked global inputs in fresh processes | fixed |
| A7 | P2 | code-quality; missing argument AST traversal | Index-based argument walk skips missing arguments, including qualified calls; matrix omitted-subscript execution test | fixed |
| A8 | P2 | learnings; exceptional attempt accounting | Synchronize attempted facts after outer catch regardless of returned result; cancellation feedback asserts completed entry even without rebuild return | fixed |
| A9 | P2 | testing; helper-only invalidation | Assert exact helper descriptor change and real recalculated independently expected3L | fixed |
| A10 | P2 | testing; fallback and retained failed recalculation | Two real successful receipts before removal, old success retained; successful output then changed-proof failure/repeated blocks never becomes success | fixed |
| A11 | P2 | testing; restarted governing-ID discovery | Fresh R process first calls unreviewed recipe, recovers public blocked governing ID, then changes proof/reviews using child feedback and succeeds | fixed |
| A12 | P2 | testing; semantic distinctions | Same-pin logical/integer/double/character, NA/NULL/empty/vector metadata, absence/presence, wrong type, contradictory callback semantic metadata | fixed |
| A13 | P2 | testing; cancellation assertion weakness | Original marker preserved, exact secondary cleanup condition retained, ownership-checked cleanup removes lease; actual attempt facts retained | fixed |
| A14 | P2 | testing; contradictory legacy state | Supported legacy empty/current-claim manifests published in isolated fixture roots, modern skip/run/failure/block unchanged with fail-if-called legacy sentinels | fixed |
| A15 | P2 | testing; callback-success/save-failure | Real fst NULL publication fault (no mocked producer) requires typed integrity, publication_failed reason, attempted TRUE, no successful pin | fixed |
| A16 | P2 | data-quality; checksum-only malformed-link tests | Update captured COMMITTED SipHash marker consistently, assert producer chain/envelope rejection; separately exercise traversal bound on genuine blocked history via internal max_records seam | fixed |
| A17 | P2 | documentation; failure classes unnamed | Explicit stamp_deterministic_failure/stamp_external_failure names and unknown default added to roxygen/article, owned Rd regenerated | fixed |

All fixes are within the already authorized exact source/test/runtime/doc scope;
manual tags required implementation reasoning, not new external policy or scope.
User explicitly requested fixing pending issues. No declined/advisory findings.
Failed correction/fixture gate outputs retained. Own setup/fixture corrections
follow the user's automatic-repair authorization, not verification waivers.

## Limits

E5 partial verification only;41 excluded files listed in Plan/work report. No
full-suite/check, commit/merge, production stage, SMB, full concurrency or umbrella
claim. Legacy fixture records are deliberately contradictory claims, not a
certification of their currentness. Malformed committed-link cases are rejected
by stamp before pipdata traversal; the separate bound test uses genuine records.
Trusted declarative callback protocol, not arbitrary R closure support/sandbox.
Final light verification review and latest candidate gate remain pending here.

## Final Verification

The final light review is saved in
`2026-10-27-m2-pipdata-authority-review-verify.md`. First pass found remaining A2
character-collation behavior and weak A1/A13 assertions. All corrected; second
pass FIXED from both required critics. Final gate41tests286expectations with no
failures/warnings/skips and built/installed smoke exit0. No pending findings.

## Tracking Repair: 2026-10-28

Authorized outer-caller repair for merge-readiness-review-repair only. Original
A1-A17 descriptions, priorities, attributions, chronology and historical caveats
above are retained unchanged. Original frontmatter type was code-review; depth
was expressed as review-mode:full. Native standard type/depth/findings fields
now enable the installed selector. This is not a new implementation or test run.

All fixed statuses above describe saved correction dispositions, supported by
the preserved earlier review-verify.md Final Verification/Executed Evidence,
final native gate2136114632065Z and installed2137270168486Z records, plus the
saved execution report final closeout. They do not preempt the fresh read-only
verification, establish merge approval, or infer any new fixed results.

### Canonical ID Crosswalk And Suppression Anchors

| Canonical ID | Original ID | Explicit fixed scope anchor | Saved disposition |
|---|---|---|---|
| P0.1 | A1 | R/code_fingerprint.R pd_engine_read_boundary; exact-parent negative and positive test blocks | fixed |
| P0.2 | A2 | R/code_fingerprint.R pd_engine_proof/pd_engine_base_boundary/pd_engine_components; ambient S3/TZ/collation test blocks | fixed |
| P1.1 | A3 | R/dependency_execution.R pd_engine_run lease acquisition and pd_engine_guard; escaping-junction test | fixed |
| P1.2 | A4 | R/dependency_execution.R callback/check/cancellation propagation; explicit cancellation test | fixed |
| P1.3 | A5 | S/runner/run.ps1 Assert-Producers/Invoke-Captured before-and-after byte checks | fixed |
| P2.1 | A6 | S/runner/run.ps1 overlay rejection; S/runner/gate.R ordinary loaded namespace checks | fixed |
| P2.2 | A7 | R/code_fingerprint.R visit_arguments/inspect_call; omitted matrix subscript test | fixed |
| P2.3 | A8 | R/dependency_execution.R post-catch attempted synchronization; cancellation exceptional-attempt assertions | fixed |
| P2.4 | A9 | tests/testthat/test-m2-engine-authority.R helper-only invalidation and test-code-fingerprint.R descriptors | fixed |
| P2.5 | A10 | test-m2-engine-authority.R two-success receipt removal and retained-success/failure history tests | fixed |
| P2.6 | A11 | test-m2-engine-authority.R fresh-process unreviewed governing-ID recovery then reviewed success | fixed |
| P2.7 | A12 | test-m2-engine-authority.R typed same-pin semantic presence/type/shape/contradiction tests | fixed |
| P2.8 | A13 | test-m2-engine-authority.R original cancellation marker, exact cleanup condition and hidden owner removal | fixed |
| P2.9 | A14 | test-m2-engine-authority.R published contradictory legacy manifests and fail-if-called authority sentinels | fixed |
| P2.10 | A15 | test-m2-engine-authority.R actual fst NULL publication failure feedback | fixed |
| P2.11 | A16 | test-m2-engine-authority.R checksum-consistent malformed-link and genuine-history bound tests | fixed |
| P2.12 | A17 | R/pd_run_pipeline.R roxygen, man/pd_run_pipeline.Rd and article classified failure names | fixed |

Parsed17 canonical finding IDs (P0:2, P1:3, P2:12, P3:0), matching17 original
deduplicated findings. Suppression is restricted to these explicit fixed scopes;
P0/P1 and cross-file breakage remain reportable, as does uncertainty. S denotes
the exact E runtime root already defined above. Earlier verification preserved.

### Current Tracking Disposition After Genuine Verification

The map's P0.2 is reopened by cg-testing session ses_e7d616f83ffe3M92YwNqyJueOg:
nested implicit dispatch remains unresolved. Historical A2 table/crosswalk fixed
dispositions above are preserved as past evidence, not current closure. Other16
prior entries remain fixed; new cross-file dependency findings and required
actions are recorded in the new canonical
`2026-10-27-m2-pipdata-authority-verify-review.md`. No source correction authorized
or performed. The selector initially recognized17 fixed entries before this
fresh result; it was not rerouted or silently replaced by a broad review.
