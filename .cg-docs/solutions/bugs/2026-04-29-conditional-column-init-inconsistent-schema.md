---
date: 2026-04-29
title: "Conditional column initialization produces inconsistent data.table schema"
category: "bugs"
language: "R"
tags: [data.table, schema, column-init, data-quality, master-inventory, release-version, guard]
root-cause: "Columns were only added to a data.table inside a guard block (if !is.na(release_vid)), so when the guard never fired the table was saved without those columns, producing a varying schema across versions"
severity: "P2"
---

> **UPDATED 2026-08-04**: `update_pip_inventory()` has been renamed/rewritten
> as `build_pip_inventory()` (`R/build_pip_inventory.R`). The unconditional-init
> pattern described below is preserved there — see lines ~368-372
> (`first_release_version_id`/`latest_release_version_id` initialised outside
> the `is.na(release_vid)` guard). References to `update_pip_inventory()`
> below are historical; read as `build_pip_inventory()` for current code.

# Conditional column initialization produces inconsistent data.table schema

## Problem

`update_pip_inventory()` (now `build_pip_inventory()`) populates two new columns on the master inventory
(`first_release_version_id`, `latest_release_version_id`). The column
initialization (`dt[, col := NA_character_]`) and the population logic were
both inside an `if (!is.na(release_vid))` guard.

When the release inventory write failed (`release_vid = NA_character_`), the
guard never executed — the columns were never added. The master inventory was
then saved via `pip_write()` without those columns, producing an inconsistent
schema: some versions of the master inventory had both columns, others had
neither.

Downstream consumers seen with `collapse::rowbind(..., fill = TRUE)` would
handle missing columns gracefully on load, but the columns were permanently
absent from affected versions until the next successful release write.

## Root Cause

Mixing "schema setup" (unconditional) with "value population" (conditional)
inside a single guard. The schema always needs to exist; only the population
is conditional.

## Solution

Separate initialization (unconditional) from population (conditional):

```r
# BEFORE (broken): both init and population inside guard
if (!is.na(release_vid)) {
  if (!"first_release_version_id" %in% names(new_pip_inv))
    new_pip_inv[, first_release_version_id := NA_character_]
  if (!"latest_release_version_id" %in% names(new_pip_inv))
    new_pip_inv[, latest_release_version_id := NA_character_]
  new_pip_inv[survey_id %in% release_ids & is.na(first_release_version_id),
              first_release_version_id := release_vid]
  new_pip_inv[survey_id %in% release_ids,
              latest_release_version_id := release_vid]
}

# AFTER (correct): init unconditional, population inside guard
if (!"first_release_version_id" %in% names(new_pip_inv)) {
  new_pip_inv[, first_release_version_id := NA_character_]
}
if (!"latest_release_version_id" %in% names(new_pip_inv)) {
  new_pip_inv[, latest_release_version_id := NA_character_]
}
if (!is.na(release_vid)) {
  new_pip_inv[survey_id %in% release_ids & is.na(first_release_version_id),
              first_release_version_id := release_vid]
  new_pip_inv[survey_id %in% release_ids,
              latest_release_version_id := release_vid]
}
```

Regression test added to lock in the schema-consistency guarantee:

```r
test_that("release_vid = NA leaves release version columns as NA", {
  inv <- data.table::data.table(
    survey_id = c("CHN_2022_A", "IND_2019_B"),
    pip_id    = c("CHN_2022_A_INC_ALL", "IND_2019_B_INC_ALL")
  )
  release_vid <- NA_character_
  dt <- data.table::as.data.table(inv)
  if (!"first_release_version_id" %in% names(dt))
    dt[, first_release_version_id := NA_character_]
  if (!"latest_release_version_id" %in% names(dt))
    dt[, latest_release_version_id := NA_character_]
  if (!is.na(release_vid)) {
    dt[, first_release_version_id  := release_vid]
    dt[, latest_release_version_id := release_vid]
  }
  expect_true("first_release_version_id"  %in% names(dt))
  expect_true("latest_release_version_id" %in% names(dt))
  expect_true(all(is.na(dt$first_release_version_id)))
  expect_true(all(is.na(dt$latest_release_version_id)))
})
```

## Prevention

- **Schema setup is never conditional.** Any column that must always be present
  in a saved artifact should be initialized unconditionally (even as
  `NA_character_`) before any guard that might skip population.
- Check: "If the guard never fires, does the table still have all expected
  columns?" If no, move the `dt[, col := NA_character_]` lines above the guard.
- When adding columns to a data.table that will be persisted, add a test for
  the failure path (guard not taken) to verify the columns exist and are the
  correct type.

## Related

- [2026-04-29-duplicate-logmeta-discriminator-key.md](./2026-04-29-duplicate-logmeta-discriminator-key.md) — companion bug from the same review session
- [2026-04-27-contract-testing-for-logging-side-effects.md](../testing-patterns/2026-04-27-contract-testing-for-logging-side-effects.md) — contract testing pattern for side-effect validation
