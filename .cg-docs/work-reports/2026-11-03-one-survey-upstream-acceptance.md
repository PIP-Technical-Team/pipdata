# One-survey upstream acceptance execution

## Plan Reference
`.cg-docs/plans/2026-11-03-one-survey-upstream-acceptance.md`, revision 3.

## Run 2026-11-03
Status: completed. Active deviation policy: autonomous (stored autonomous; runtime `deviate:auto`). Review: auto, resolved data-risk, followed by light mode:verify.

## Completed Steps
All four steps implemented and verified; review converged and code checkpoints pushed.
Canonical artifact validation passed before mutation. Repository main checkout initially clean on PROD at `59415667df3051a5d2acc14a57db803fbcacbd21`. Created `feat/one-survey-upstream-acceptance` there with `git switch -c feat/one-survey-upstream-acceptance 59415667df3051a5d2acc14a57db803fbcacbd21`.

## Deviations
Session initially opened in existing `.kilo/worktrees/feat-one-survey-upstream-acceptance` on hyphenated branch at the same SHA. Left it unchanged and used the explicitly requested main checkout. No new worktree created. `compound-gpid.local.md` absent; language comes from Plan/DESCRIPTION (R). No matching roadmap feature found; no roadmap writes.

Potential narrow source paths recorded before edits: `R/inv_dlw_load.R`
(forward inventory version to the existing version-capable real loader) and
`R/pd_aux_attr.R` (reject missing/duplicate exact matches consistently with
`pd_aux_component_projection()`); matching Rd files and the existing acquisition
test loader mock signature if necessary. These are hypotheses from source
inspection, not reproduced defects yet; require executed red baseline before
implementation. No recode/scientific rule change is planned.

First correction round also requires `R/dependency_execution.R`:
`pd_freeze_aux_snapshot()` must request `ppp_defaults = FALSE` from the real
exact-version loader. The existing default filters the stored nine-vintage
PPP artifact before comparing it with the full-artifact catalog hash. Keep
the hash assertion and pin unchanged. Acceptance exact readback explicitly
requests the same unfiltered artifact. This is within the Plan's narrow
loader-forwarding boundary, not pipaux/pipload modification or a new PPP policy.

## Accepted Exceptions
None.

## Approved Dependency Scope Extension
User explicitly approved a narrow stamp class-restoration fix and required
verification against most recent origin. Live `git ls-remote --symref origin
HEAD refs/heads/ready_cran` returned ready_cran
`4a1fa5d92313aa223296fae0e29cf3446a314aea`, equal to inspected local HEAD.
Live master is `b6e5e2c5519a7c00dbb8f815a59aeac2daf9592e`; `git merge-base`
confirmed it and prior M2 branch `5d1ddebdc926dad9c6ec99c08a65e74baee0da92`
are ancestors of ready_cran. Created the approved separate feature branch
`feat/one-survey-class-readback` at that ready_cran SHA, without merge.
Preserved stamp's unrelated dirty `.gitignore`, `compound-gpid.local.md`, and
untracked `.compound-gpid/`; exclude them from delivery.
Only stamp `R/hashing.R` and new class-readback test are intended changes.
Restoring the original class after data.table reconstruction fixes exact
readback/hash equality without changing storage or scientific calculations.

## Runtime Decision and Test Index
The known E runtime libraries were inspected: pipdata-m2-engine-runtime
library/phase1 and library/phase2 each contain only pipdata/pipload/stamp;
pipload-m2-stamp-adapter-runtime/library/retry-1 contains only stamp. Their
old runners fall back to C libraries and are not reused wholesale.
User explicitly selected `Allow E snapshot (Recommended)` in this run:
read-copy installed third-party dependencies into a new disposable E library,
then load only that snapshot and inspected PIP sources; no shared-library
modification, package installation or network. This resolves the runtime
permission blocker, not runtime verification. pipaux source HEAD observed
`01ff827340a9759dc3d4ef18d31f4e3ba26ff2be`, matching the reviewed producer seam.

One test index scan identified the required files: new
`test-one-survey-upstream-acceptance.R`, existing
`test-m2-one-survey-acquire-format.R`, `test-dlw_validation_engine.R`,
`test-dlw_validation_spec.R`, `test-dependency-inputs.R`; add
`test-recode-spec.R` if recode changes. Existing source owning tests include
`test-pd_process_data.R`, `test-get-country-pfw.R` and
`test-pd-metadata-refresh.R` if their code changes.

Relevant Brain: `.cg-docs/solutions/data-quality/2026-08-17-yaml-driven-validation-engine-pattern.md`
documents literal-prefix versus regex selection and helper-fixed severities;
retain those semantics and test actual association/severity rather than sorted
golden-column multisets. Existing revision-3 plan critique at
`../pipsystem/.cg-docs/orchestration/outputs/pipsystem/git-delivery/20261103T162831Z-orchestrator/plan-critic-r3.md`
was read: zero open P1/P2/P3; this is historical review, not current runtime proof.

## Evidence
| ID | Status | Evidence |
|---|---|---|
| V1 | passed | Real selected GPWG engine/report and missing-weight invalid/no-save counterpart; V1-validation-gate.rds |
| V2 | passed | Real CPI/PPP/PFW producers, exact unfiltered consumer pins, literal keys/classes/values; V2-producer-consumer.rds |
| V3 | passed | Old validated source consumed despite newer decoy; real clean/recode/metadata and exact readback; V3-clean-readback.rds |
| V4 | passed | Real malformed eligibility gate plus missing/ambiguous PFW/CPI/PPP: zero saves, unchanged catalogs; V4-aux-negative-controls.rds |

All four artifacts above are in
`E:/temp/pdu-20261103T1851184462870Z-1ca4e9b8/evidence/`.
Its `tests.csv`: 4 blocks, 174 expectations, no failures/errors/warnings/skips.

### Immutable Numerical-Chain Pins
These are from the V3 block, not mixed with independent V2 fixture stores.

| Artifact | Actual immutable version | Actual content hash |
|---|---|---|
| validated raw | 063817fba1dd3d77 | 484e3547f5ce1bf4 |
| CPI producer = consumer | 819a7a167a0836c8 | ac7acb9c9dc98475 |
| PPP producer = consumer | aff8c7abe3f13a19 | fb07faf47cb42d3d |
| PFW producer = consumer | bc7f1260a09df0ca | 1f687e6b93f71bb3 |
| supplemental population fixture | bf1ea71bce0d5b25 | e9ee2b95e5894e8c |
| supplemental GDP fixture | e03704f49fb995f8 | a5f1b6e304b6fef1 |
| supplemental PCE fixture | 467c1a19825ec680 | 50efb5d94b2e7265 |
| clean output | 652f216767807d17 | 5e785953720f4d2e |
| metadata output | 55f300af12e7f47e | fd6c38dbd49d7ff3 |

Recode version `240076d6e122a07a`; newer raw decoy
`56d4aa3d81804805` was not consumed. Saved class is exactly
`pipmd`, `data.table`, `data.frame`; row-aligned daily welfare `1,2,3,4`
and weights `10,20,30,40` match literal expectations, not a consumer-generated
golden. V3 RDS retains hhid/pid/urban/gender/age, domain/identity and named
CPI/PPP/pop/GDP/PCE metadata checks. Supplemental producers are not certified.

## Constraints Check
| ID | Status | Evidence |
|---|---|---|
| C1 | passed | Actual E aliases/roots/stamp paths/options/env/loaded identities in each inventory and fixture RDS; 119 third-party snapshot packages, read-only base R runtime on C, user-approved copy only |
| C2 | passed | Only transport mocked; worker/storage/log state is real; no live acquisition, production access, merge, default-branch push or installation |
| C3 | passed so far | New fixture/runner, narrow reproduced V1-V4 fixes, docs, allowed evidence/frontmatter; separately approved stamp scope above |

## Correction Budget by Plan Step
- Step 1: two fixture corrections (receipt API/selector/state/tracer plumbing,
  then the remaining trace-observer binding). Assertions and real workers
  preserved; final V1-V4 passed.
- Step 2: one functional correction round: exact-zero missingness predicate
  and unfiltered PPP snapshot. Required validator/input/execution regressions
  passed before Step 3.
- Step 3: two functional correction rounds: raw pin forwarding and missing/
  duplicate metadata rejection, then duplicate detection using emitted metadata
  names rather than renamed survey match keys. Distinct vintages retained.
- Approved stamp extension: one source correction after red baseline;
  original class restoration, not an analytical rule change.
- Harness adjustments (short E paths, explicit declaration-only cross-file
  helper, unique run directory and diagnostic output) did not weaken functional
  assertions or substitute any real pipeline/storage function.

## Verification Commands and Final Selections
All commands below are Windows PowerShell using
`& 'E:/PovcalNet/01.personal/wb384996/PIP/pipdata/.tmp/one-survey-upstream-acceptance/run.ps1'`.

| Arguments | Evidence directory | Observed result |
|---|---|---|
| `-Stage tests -TestFiles @('test-one-survey-upstream-acceptance.R')` | E:/temp/pdu-20261103T1851184462870Z-1ca4e9b8/evidence | 4 blocks / 174 expectations passed |
| `-Stage tests -TestFiles @('test-m2-one-survey-acquire-format.R','test-dlw_validation_engine.R','test-dlw_validation_spec.R','test-dependency-inputs.R','test-dependency-execution.R','test-pd-metadata-refresh.R')` | E:/temp/pdu-20261103T1851430977060Z-391258ab/evidence | 83 blocks / 411 expectations passed |
| `-Stage tests -TestPackage stamp -TestFiles @('test-one-survey-class-readback.R','test-data-table-keys.R','test-hash-sanitize.R','test-hash-attribute-order.R','test-audit-hashing.R')` | E:/temp/pdu-20261103T1852093440919Z-0fc38e90/evidence | 32 blocks / 544 expectations passed |

Final selections ran after committing both source checkpoints. The acceptance
`configuration.txt` records loaded source heads pipdata
`6e8a351b279b6c474bb1e341e620a8c3ce7e75e1` and stamp
`95890411e8bf9db842593c2b0ab68ec4f2a66b25`, actual source paths and MD5s.
Prior precommit green runs (183700/183815/183900) remain retained and were the
evidence inspected by the eight reviewers; post-review stamp 184610 also passed.

These are the complete Plan-required and affected-owner selections, not a
claim that every package test or R CMD check ran. Each final selection has
zero test failures/errors/warnings/skips. No PowerShell/Pester code changed.
The supplied tool interface has no `get_errors`; no VS Code diagnostic-clean
claim is made. Executed R tests, later parse/Rd checks and diff checks provide
the available diagnostics surface.

Earlier runs retained for traceability: acceptance baseline
`20261103T1731302323314Z` and correction run `20261103T1734133562865Z` under
`.tmp/one-survey-upstream-acceptance/runs/`; E:/temp runs
`pdu-20261103T1741315150964Z`, `pdu-20261103T1742314897275Z`,
`pdu-20261103T1834358267512Z` failed as described above. Stamp red baseline
`E:/temp/pdu-20261103T1834358177496Z/evidence` had 12 class/hash failures
across RDS/QS2 and keyed/unkeyed cases. A concurrent run directory collision
at `E:/temp/pdu-20261103T1835456549431Z` is explicitly excluded from final
evidence; run names now include a unique suffix and final selections ran
sequentially. Legacy acquisition fixtures also leave mappings to deleted
temporary stores, so acceptance and legacy selections use fresh children;
no missing alias or warning was ignored in acceptance.

## Commands and Observations
- Runtime inventory smoke passed, latest evidence `.tmp/one-survey-upstream-acceptance/runs/20261103T1732091597728Z/evidence/`; 119 E third-party snapshots, no startup warnings. Source SHAs: stamp `4a1fa5d92313aa223296fae0e29cf3446a314aea`, pipfun `0c6a78d9884ed0965a524311537eec2a5095d60b`, pipload `ff9a81e386a09fb2531c154f63fd2b91521313c6`, pipaux `01ff827340a9759dc3d4ef18d31f4e3ba26ff2be`, pipdata baseline `59415667df3051a5d2acc14a57db803fbcacbd21`. Actual source hashes/configuration/loaded paths saved in that evidence directory.
- Baseline command: `& 'E:/PovcalNet/01.personal/wb384996/PIP/pipdata/.tmp/one-survey-upstream-acceptance/run.ps1' -Stage tests -TestFiles @('test-one-survey-upstream-acceptance.R')`. Failed, evidence `.tmp/one-survey-upstream-acceptance/runs/20261103T1731302323314Z/evidence/`; 4 test blocks, 8 reported failures/errors. V1 real acquisition succeeded but validation failed in the engine: `lower bound must be strictly lower than upper bound`. Required zero-missing threshold rounded to zero for 4 rows; `within_bounds(0, 0)` throws instead of testing exact zero. First targeted correction to `R/pipdata_dlw_validation.R`: use `in_set(0)` only when existing rounded threshold is zero, retain other intervals/severities. No threshold/scientific policy changed. V2-V4 failed early on pinned auxiliary hash verification; diagnosed separately, not claimed as version-forwarding or negative-control proof.
- `git status --short --branch` in both checkouts: clean, required baseline.
- `git worktree list --porcelain`: main PROD and existing hyphenated worktree; both at full baseline SHA above.
- `cg-render-artifact --validate-only E:/PovcalNet/01.personal/wb384996/PIP/pipdata/.cg-docs/plans/2026-11-03-one-survey-upstream-acceptance.md`: passed with TEMP/TMP/TMPDIR=E:/temp and PYTHONDONTWRITEBYTECODE=1.
- `Rscript --vanilla -e "cat('R_LIBS_USER=', Sys.getenv('R_LIBS_USER'), '\\nR_LIBS_SITE=', Sys.getenv('R_LIBS_SITE'), '\\n'); cat(.libPaths(), sep='\\n')"`: R 4.5.2 executable found; default user library on C, base library on C; no package loaded by this inventory command.
- `Rscript --vanilla E:/PovcalNet/01.personal/wb384996/PIP/pipsystem/tools/pipsys/pipsys.R check-path E:/PovcalNet/01.personal/wb384996/PIP/pipsystem/.cg-docs/orchestration/outputs/pipdata/one-survey-upstream-acceptance/20261103T162831Z-code/handoff.md`: passed, inside checkout. TEMP/TMP/TMPDIR, XDG cache/data/state and R user cache/config/data redirected to E:/temp; this containment tool loads data.table only, not PIP packages.

## Remaining Uncertainty
Route-aware data-risk review completed with eight agents. Deduplicated P2.1
(portable stamp test temp root) and P3.1 (explicit vignette Work Report path)
were fixed; no P0/P1 or skipped findings. Review report:
`.cg-docs/reviews/2026-11-03-one-survey-upstream-acceptance-review.md`.
Stamp owning selection rerun after review fix passed 32/544 at
`E:/temp/pdu-20261103T1846104771888Z-ca64ebe9/evidence/`.
Final staged `/cg-review mode:verify` completed with cg-code-quality and
cg-testing: both findings confirmed fixed, no new P0-P3/cross-file breakage.
Verification report:
`.cg-docs/reviews/2026-11-03-one-survey-upstream-acceptance-verify-review.md`.
Exact committed-identity reruns passed as recorded above. Code checkpoints
were pushed successfully; final evidence-checkpoint SHA is recorded in the
external handoff after its successful push (avoids a report self-hash cycle).

## Mechanical Self-Review
Mechanical self-review complete: no added debug/import/TODO/secret issue found.
Nine changed R source/test files and both changed Rd files parsed successfully;
staged diffs passed whitespace checks. Relevant package imports were available
in the inspected snapshot. `rg` is not installed in the shell; dedicated Grep
was used instead. Existing unrelated print/token references were not changed.
Statistical and logical correctness are not checked by this mechanical pass;
the executed independent-oracle checks and data-risk review above are separate.
Only offline stage acceptance is established: live DLW, stamp-selected
orchestration, incremental/restart/scope orchestration, supplemental producer
certification, deflation, mean and complete M2 readiness remain outside scope.
The runner and dependency snapshot are local E artifacts, not installed/shared
libraries or a portable archive. Final committed source identities and MD5s
are recorded in the executed evidence; unrelated stamp and pipload metadata
dirty files are disclosed in that inventory, not included or modified.

## Git Delivery
- pipdata code/review checkpoint `6e8a351b279b6c474bb1e341e620a8c3ce7e75e1`:
  `test(integration): verify one-survey upstream acceptance`.
- stamp dependency checkpoint `95890411e8bf9db842593c2b0ab68ec4f2a66b25`:
  `fix(storage): preserve data.table subclasses on exact readback`.
- Both `git push --set-upstream origin HEAD:refs/heads/<authorized-feature>`
  commands succeeded and set tracking. HTTPS commands used a per-command gh
  credential helper with GCM disabled, E temp/cache settings and no persistent
  git-config change. No merge/default-branch push, force push or PR creation.
- This final evidence checkpoint contains only Plan execution frontmatter,
  Work Report and active state. Its actual final delivery SHA and remote
  verification are in the assigned external pip-handoff-v2 report.
- Initial pipdata dirty exclusions: none. Stamp pre-existing `.gitignore`,
  `compound-gpid.local.md`, `.compound-gpid/` preserved and excluded. Session
  hyphenated worktree untouched. No roadmap plan/title match on completion;
  no roadmap writes or feature/milestone completion claim.

## Model Advisory
Implementation to review: reliable code/tool use and independent evidence
checking, high effort recommended. Strong option: retain a capable coding/
review model with high effort; economical option only for bounded mechanical
checks at medium effort. Model and effort remain user-controlled; no override
or switch was made for delegation.

## Final Status
Completed within the explicitly bounded offline acceptance scope. V1-V4 and
required affected-owner regressions passed; both review findings fixed and
mode:verify converged. No accepted evidence exceptions and no unresolved science.
