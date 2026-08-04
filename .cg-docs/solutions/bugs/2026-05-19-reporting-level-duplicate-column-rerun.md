---
date: 2026-05-19
title: "reporting_level duplicate column on re-run of pd_process_data"
category: "bugs"
type: "bug"
language: "R"
tags: [update_pip_inventory, reporting_level, joyn, collapse, ftransform, duplicate-column, re-run]
root-cause: "old_pip_inv loaded from disk already has reporting_level; rowbind into new_pip_inv carries it forward; subsequent joyn::left_join with pfw_rl_unq (which also has reporting_level as a non-key column) creates a duplicate column that collapse::ftransform_core() rejects"
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

> **UPDATED 2026-08-04**: `update_pip_inventory()` has been replaced by
> `build_pip_inventory()` (`R/build_pip_inventory.R`), which changes this
> further than a simple rename: `build_pip_inventory()` no longer computes
> `reporting_level` at all — it is treated as a legacy column and explicitly
> **dropped** from `old_inv` before the upsert (see the `legacy_cols` list in
> `build_pip_inventory()`). Enrichment with `reporting_level` is now opt-in,
> done by callers via `pipload::pip_inv_enrich(inv, fields = "reporting_level")`
> in the `pipload` package. The specific duplicate-column collision described
> below therefore cannot recur inside `pipdata` today, but the general
> "guard before joining a column the target may already carry" pattern (see
> `.cg-docs/solutions/data-quality/2026-05-19-guard-before-join-reentrant-column.md`)
> remains valid and worth checking if `pipload::pip_inv_enrich()` performs a
> similar join.

# reporting_level duplicate column on re-run of pd_process_data

## Symptom

`pd_process_data()` (and by extension `update_pip_inventory()`) errors on a
second (or any subsequent) run with:

```
Error in `ftransform_core()`:
! All columns of .data have to be uniquely named
```

Traceback leads through `joyn::left_join` → `joyn:::joyn_workhorse` →
`collapse::ftransform(dt_result, .joyn1 = as.numeric(.joyn1))`.

The first run succeeds; only re-runs trigger the error.

## Root Cause

Inside `update_pip_inventory()`, `new_pip_inv` is assembled by row-binding the
fresh `pip_inv` (just processed) with `old_pip_inv` loaded from the master
inventory on disk:

```r
new_pip_inv <- pip_inv |>
  collapse::rowbind(old_pip_inv, fill = TRUE) |>
  ...
```

`old_pip_inv` was written by the previous run and already carries a
`reporting_level` column. `new_pip_inv` therefore inherits it. Later, the code
computes `pfw_rl_unq` which also has a `reporting_level` column, and joins:

```r
new_pip_inv <- joyn::left_join(
  new_pip_inv,      # has reporting_level
  pfw_rl_unq,       # also has reporting_level (non-key)
  by = c("country_code", "surveyid_year", "survey_acronym"),
  ...
)
```

joyn builds an intermediate `dt_result` carrying both copies of
`reporting_level`, then calls `collapse::ftransform(dt_result, .joyn1 = ...)`.
`collapse::ftransform_core()` rejects tables with non-unique column names.

## Reproduction Test

Added to `tests/testthat/test-update_pip_inventory.R`:

```r
test_that("reporting_level join succeeds when new_pip_inv already has reporting_level", {
  new_pip_inv <- data.table::data.table(
    survey_id      = "ABX_2020_HBS",
    pip_id         = "ABX_2020_HBS_INC_ALL",
    country_code   = "ABX",
    surveyid_year  = 2020L,
    survey_acronym = "HBS",
    reporting_level = "1"   # already present from previous run
  )
  ...
  if ("reporting_level" %in% names(new_pip_inv)) {
    new_pip_inv[, reporting_level := NULL]
  }
  expect_no_error({ result <- joyn::left_join(...) })
  expect_equal(result$reporting_level, "1")
})
```

## Fix

In `R/update_pip_inventory.R`, drop `reporting_level` from `new_pip_inv`
immediately before the `joyn::left_join` with `pfw_rl_unq`:

```r
# Drop any existing reporting_level before joining pfw_rl_unq — on a re-run
# old_pip_inv already carries this column, which would create a duplicate
# and cause collapse::ftransform_core() to error.
if ("reporting_level" %in% names(new_pip_inv)) {
  new_pip_inv[, reporting_level := NULL]
}
new_pip_inv <- joyn::left_join(
  new_pip_inv,
  pfw_rl_unq,
  by = c("country_code", "surveyid_year", "survey_acronym"),
  relationship = "many-to-one",
  reportvar = FALSE,
  verbose = FALSE
)
```

The value is always re-derived fresh from the current PFW, so silently dropping
the cached column is safe and correct.

## Lessons Learned

When a function reads existing state from disk and merges it with fresh results,
any columns that are _computed and appended_ later in that same function will
already be present in the loaded state on re-runs. Any subsequent join that
produces those same columns as non-key output will create duplicates.

**Pattern to follow:** Before joining a table that produces column `X` as a
non-key output, always guard with:

```r
if ("X" %in% names(target)) target[, X := NULL]
```

**Anti-pattern that caused it:** Assuming join inputs are always "clean" (i.e.
don't already contain the output column), which is only true on the first run.

## Related

- `.cg-docs/solutions/data-quality/2026-05-19-guard-before-join-reentrant-column.md` — generalised pattern and prevention guide for load-then-recompute duplicate column bugs
- `.cg-docs/solutions/bugs/2026-04-29-conditional-column-init-inconsistent-schema.md` — related: conditional column initialisation causing schema drift across runs
