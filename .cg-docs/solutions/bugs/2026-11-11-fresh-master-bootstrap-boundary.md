---
date: 2026-11-11
title: "Fresh-master bootstrap failed before preparation"
category: "bugs"
type: "bug"
language: "R"
tags: [bootstrap, master-inventory, pd_process_data, stamp, missing-history, integration-gap]
root-cause: "The wrapper read master inventory before bootstrap preparation could handle absent history."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
red-phase-confirmed: "yes"
expected-behavior-source: "user-requirement"
test-gap: "integration-gap"
---

# Fresh-master bootstrap failed before preparation

## Symptom

With a completed-valid inventory row and an empty TEST store,
`pd_process_data(inv = inv, bootstrap = TRUE)` failed with
`No version files found in 'pip_master_inventory.qs2'` before entering
bootstrap preparation. The retained actual validated row reproduced the same
error; this was not an inventory-validation or native QS2 loading failure.

## Expected Behavior Source

**User requirement (`user-requirement`):** explicit bootstrap must cross the
fresh-master boundary when history is genuinely absent. Preparation must receive
an empty in-memory master with zero rows and `bootstrap = TRUE`; absence alone
must not stop the public wrapper before preparation.

Existing history must still use the normal reader. Read/listing errors,
corruption warnings and surviving master storage without trustworthy catalog
history must not be interpreted as first-run absence. This requirement does not
promise successful downstream processing without auxiliary data and other
pipeline prerequisites.

## Root Cause

The wrapper unconditionally called
`pipload::load_pip_master_inventory()` before
`pd_prepare_execution(..., bootstrap = bootstrap)`. The reader correctly rejects
an artifact with no versions, so preparation never received the explicit
bootstrap permission. The fix belongs at the wrapper boundary, not in the
general-purpose reader.

The source baseline was existing branch `PROD` at
`13a38280c1eafe7e03a97837cdb57833fbe9d926`. The verified correction remains an
uncommitted working-tree change, not a published immutable pipdata revision.

## Reproduction Test

File: [tests/testthat/test-pd-process-fresh-bootstrap.R](../../../tests/testthat/test-pd-process-fresh-bootstrap.R).
The original case is
`fresh bootstrap reaches preparation with genuinely empty master`.
It initializes a real empty stamp repository, supplies a completed-valid
inventory, calls the real public wrapper and reader, and passively observes
preparation entry. No processing worker is replaced and no populated master is
written or substituted into this public regression.

The assertions use the requirement's zero-row expectation, not an expected
value copied from implementation output. Excerpt, with setup and cleanup in
the linked test file:

```r
expect_true(observed$entered, info = diagnostic)
if (isTRUE(observed$entered)) {
  expect_true(is.data.frame(observed$master))
  expect_equal(nrow(observed$master), 0L)
  expect_identical(observed$bootstrap, TRUE)
}
expect_false(grepl(
  "No version files found in 'pip_master_inventory.qs2'",
  diagnostic, fixed = TRUE
), info = diagnostic)
```

Before source edits, this test had two failed assertions carrying the reported
missing-version error and never entered preparation. The empty catalog checks
passed before and after the call. A frozen boundary test was subsequently run
unchanged against the installed old baseline and corrected candidate, yielding
the same red symptom and a green result respectively. Its SHA256 is
`385ab06c66fd6c3b557a6e4b51045c6b18c464a3a04c15ff374bdf248af6d772`.

## Test Gap

**`integration-gap`:** existing wrapper tests supplied in-memory masters and
covered other inputs, rather than the real reader interacting with absent
history. The existing wrapper file passed 31 assertions on the broken baseline.
Those assertions were not wrong; the missing interaction was the gap. No old
expected values were weakened or repaired. The new file adds seven cases,
including existing-reader delegation, error propagation, corrupt-history
warnings, missing aliases and surviving storage with a missing catalog.

## Fix

The wrapper now calls `pd_load_process_master(bootstrap, verbose)` in
[R/pd_process_data.R](../../../R/pd_process_data.R). The helper permits empty
state only for explicit bootstrap, one matching configured alias and a
warning-free zero-version listing consistent with absence of master storage.
Existing history and non-bootstrap calls retain the normal reader. Listing
warnings become `pipdata_master_history_error`, preserving the warning as the
parent condition; listing/read errors propagate.

A review-found edge case required an additional check: stamp can return an
empty catalog without warning when the catalog file is missing but artifact
storage survives. The helper therefore checks the configured root before
returning empty state:

```r
if (nrow(versions) == 0L) {
  entries <- fs::dir_ls(root, all = TRUE)
  if ("pip_master_inventory.qs2" %in% tolower(fs::path_file(entries))) {
    rlang::abort(
      "Master inventory storage exists without trustworthy version history.",
      class = "pipdata_master_history_error"
    )
  }
  return(data.table::data.table())
}
```

A real saved EMPTY master with its catalog moved aside exposed this edge case
before the guard was added. The corrected guard and regression then passed;
the independent data-quality reviewer closed the finding. No history is
reconstructed or repaired. Corresponding bootstrap help in
`man/pd_process_data.Rd` describes this boundary.

Verification evidence is retained in the
[handoff](../../../../pipsystem/.cg-docs/orchestration/outputs/pipdata/practical-integration-tests/20261110T230827Z-bootstrap-fix/handoff.md).
Paths below are relative to that handoff's run folder:

- Original red: `runtime/logs/red.R.log`; exact frozen pair:
  `b/logs/frozen-red.R.log` and `runtime/logs/final-green.R.log`.
- Real-storage controls: `runtime/logs/real-storage-final.R.log`; fresh public
  bootstrap reaches the missing-auxiliary-data boundary, an existing empty
  master reads normally, and damaged/unreadable catalogs are rejected.
- Approved verification: `v/logs/check-affected.R.log` records **109 test cases,
  539 passing assertions, zero failures/errors/skips**. Existing: 102 cases /
  520 assertions; new: seven cases / 19 assertions. Full independent review
  and final scoped verification have no open findings.

The all-green context uses explicitly approved test-only pipload commit
`4ef0d90a974cb6ff0725da69373bde421dd04ad5` and temporary files under
`E:/PovcalNet/01.personal/wb384996/PIP/pipsystem/.tmp/bf-20261110/tmp`.
The original survey pin `ff9a81e386a09fb2531c154f63fd2b91521313c6` lacks the
required metadata-only API; its broader-suite failure evidence was retained,
not reclassified as green. Original Plan/pins and installations were unchanged.
Other copied package versions/R-code hashes and exported immutable source
identities are recorded under `v/logs/`.

Scope limits remain: this is not full survey-processing acceptance or production
deployment. No full survey rerun, commit or push occurred. A nonblocking
installation help-CSS copy warning is retained; help styling is not verified.
The user replied `confirmed fixed` on November 11, 2026 before this document
and its lessons were written.

## Lessons Learned

- For an `integration-gap`, add a public-boundary regression using real storage
  and the real reader; mocked master fixtures alone cannot detect initialization
  ordering failures. Preserve valid existing assertions.
- Declare independent expected behavior before writing the regression. Here,
  the correct zero-row master comes from the bootstrap requirement, not from
  rerunning the implementation and accepting its output.
- Distinguish absence from untrustworthy state. A warning-free empty catalog
  can still contradict surviving artifact storage; never catch every read
  failure and substitute an empty master.
- Preserve exact red test source and dependency identities. An approved
  alternate verification dependency does not retroactively make the original
  survey-pin context compatible or constitute a full survey rerun.

## Related

None.
