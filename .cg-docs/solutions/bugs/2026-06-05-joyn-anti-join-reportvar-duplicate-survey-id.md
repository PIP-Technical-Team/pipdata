---
date: 2026-06-05
title: "joyn::anti_join missing reportvar=FALSE leaks .joyn column causing duplicate survey_id in inv_to_clean"
category: "bugs"
type: "bug"
language: "R"
tags: [joyn, anti_join, reportvar, survey_id, duplicate, inv_to_process, valid_dlw_load, build_pip_inventory]
root-cause: "joyn::anti_join adds a .joyn factor column by default; without reportvar=FALSE the column leaks into inv_to_clean, causing unique() to treat rows from inv_svy and inv_aux as distinct even when survey_id is identical"
severity: "P1"
test-written: "yes"
fix-confirmed: "yes"
red-phase-confirmed: "yes"
expected-behavior-source: "user-requirement"
test-gap: "missing-test"
---

# joyn::anti_join missing `reportvar=FALSE` leaks `.joyn` column causing duplicate `survey_id` in `inv_to_clean`

## Symptom

`pd_process_data()` aborted with:

```
Error in `build_pip_inventory()` at pipdata/R/pd_process_data.R:133:3:
! anyDuplicated(inv_to_clean$survey_id) == 0L is not TRUE
```

Traceback:
```
pd_process_data(inv = inv, verbose = FALSE)
  └─ build_pip_inventory(...)
       └─ stopifnot(anyDuplicated(inv_to_clean$survey_id) == 0L)
```

Triggered consistently when running without `force = TRUE`, whenever at least one survey was simultaneously:
- new (not yet in the master inventory → kept by `inv_to_process()`), and
- affected by an auxiliary file change (CPI/PPP/etc. → kept by `filter_aux_inv()`).

## Expected Behavior Source

User requirement (enforced by the downstream contract in `build_pip_inventory()`):
`valid_dlw_load()` must return a `data.table` with no duplicate `survey_id` values —
one row per survey to process. The assertion at `build_pip_inventory.R:59` is the
explicit contract enforcement point.

## Root Cause

`inv_to_process()` called `joyn::anti_join(dt_master, by = key_inventory, verbose = FALSE)`
**without** `reportvar = FALSE`. `joyn::anti_join` always appends a `.joyn` factor
column (value `"x"`) to its result unless `reportvar = FALSE` is passed.

Back in `valid_dlw_load()`, the two sub-inventories are assembled as:

```r
inv_to_clean <- rbind(inv_svy, inv_aux, fill = TRUE)
inv_to_clean <- unique(inv_to_clean)
```

`inv_svy` (from `inv_to_process()`) carries `.joyn = "x"`.  
`inv_aux` (from `filter_aux_inv()`, which already uses `reportvar = FALSE`) has no
`.joyn` column → `fill = TRUE` sets it to `NA` for those rows.

For any survey present in **both** sets, `unique()` sees two structurally distinct rows
(`.joyn = "x"` vs `.joyn = NA`) and retains both, producing a duplicate `survey_id`.
The duplicate then triggers the `stopifnot` in `build_pip_inventory()`.

## Reproduction Test

File: `tests/testthat/test-valid_dlw_load.R`

**Test 1** — direct probe of the root cause:
```r
test_that("inv_to_process does not add .joyn column to result", { ... })
```
Asserts `".joyn" %nin% names(result)` after calling `inv_to_process()`.

**Test 2** — end-to-end regression:
```r
test_that("valid_dlw_load returns no duplicate survey_ids when survey appears in new and aux-changed sets", { ... })
```
Constructs a scenario where a single survey is both new and aux-changed; asserts
`anyDuplicated(result$survey_id) == 0L`.

Both tests failed on the unfixed code and pass after the fix.

## Test Gap

**missing-test** — No test file existed for `valid_dlw_load.R` or its helpers
(`inv_to_process()`, `filter_aux_inv()`). The duplicate-survey-id path only manifests
when the same survey simultaneously qualifies as "new" *and* "aux-changed", a
two-condition conjunction that never appeared in the automated test suite and was only
triggered by a real run with actual auxiliary file changes. The joyn `.joyn` column
side-effect is subtle and not obvious from reading `anti_join` call sites in isolation.

## Fix

Single-line change in `R/valid_dlw_load.R`, `inv_to_process()`:

```r
# Before
inv_svy <- inv |>
  joyn::anti_join(dt_master, by = key_inventory, verbose = FALSE)

# After
inv_svy <- inv |>
  joyn::anti_join(dt_master, by = key_inventory, verbose = FALSE, reportvar = FALSE)
```

Adding `reportvar = FALSE` suppresses the `.joyn` column, making `inv_svy`
structurally identical to `inv_aux`. `unique()` now correctly deduplicates on
`survey_id` and the downstream `stopifnot` passes.

## Lessons Learned

**Every `joyn::` join call that feeds a result used as production data must include
`reportvar = FALSE`**, unless the `.joyn` column is explicitly consumed downstream.
The default `reportvar = TRUE` is designed for diagnostics; leaving it on in data
pipelines silently contaminates the schema.

Pattern to follow: audit all `joyn::anti_join`, `joyn::inner_join`, and
`joyn::left_join` call sites in production code paths for the presence of
`reportvar = FALSE`. Joins used purely for diagnostics or already dropping `.joyn`
are fine; joins whose output feeds `rbind`, `unique`, or downstream schema checks
must have it.

The missing-test gap: helper functions like `inv_to_process()` that implement
non-trivial filtering logic should have unit tests that verify their output schema
(column names) in addition to their row-filtering correctness.

## Related

- `.cg-docs/solutions/data-quality/2026-06-05-joyn-diagnostic-column-discipline.md` — team-wide pattern document covering both joyn diagnostic column classes and the full audit/prevention checklist.
- `.cg-docs/solutions/bugs/2026-05-20-joyn-suffix-collision-persisted-to-inventory.md` — Class 2: `.x`/`.y` suffix columns persisted to master inventory; same root pattern.
