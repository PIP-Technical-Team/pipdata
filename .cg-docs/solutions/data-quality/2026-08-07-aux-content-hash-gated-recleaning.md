---
date: 2026-08-07
title: "Gate auxiliary re-cleaning with run-level content hashes and row-level changes"
category: "data-quality"
language: "R"
tags: [auxiliary-data, content_hash, stamp, valid_dlw_load, valid_aux_load, master-inventory, pip_id, survey_id, joyn, incremental-processing]
root-cause: "Auxiliary row-change comparison ran on every pipeline invocation without first checking whether the current aux artifact had changed since the survey was cleaned"
severity: "P1"
---

# Gate auxiliary re-cleaning with run-level content hashes and row-level changes

## Problem

`valid_dlw_load()` always invoked `valid_aux_load()`, which compares current
auxiliary releases/vintages and returns changed country/year keys. This caused
surveys to be considered for re-cleaning even when the pipeline had already
processed them with the same auxiliary artifact.

A global auxiliary table hash alone is insufficient: an updated CPI or PPP
table may contain changes for countries that are not among the surveys requested
in the current DLW inventory. The pipeline needs both a global artifact check
and a row-level affected-survey check.

The master inventory also has an important identity rule: its current snapshot
contains one row per `(survey_id, pip_id)`, while stamp preserves previous
inventory snapshots as artifact versions. When a survey is reprocessed, stale
`pip_id` rows from the previous snapshot must be removed if the current
reprocess no longer produces them.

## Root Cause

The previous logic had no record of which auxiliary artifact content hash was
used for each successfully cleaned survey. It also joined survey-level master
rows only by `survey_id`, which is ambiguous when a survey has multiple
historical DLW `content_hash_dlw` values.

The inventory upsert retained old rows by `pip_id`. If a reprocessed survey's
new result had fewer welfare-type rows, such as losing a `CON` or `INC` split,
the obsolete row could remain in the current master snapshot.

## Solution

### 1. Resolve current auxiliary hashes once

Before loading auxiliary data, query the `aux` stamp catalog once and match
requested measures to exact `<measure>.qs2` artifacts:

```r
aux_hashes <- get_aux_hashes(aux_measures, verbose = verbose)
aux_list <- lapply(aux_measures, pipload::load_aux_data, verbose = verbose)
```

`get_aux_hashes()` uses the catalog `content_hash`, includes PFW, fails loudly
on missing or ambiguous artifacts, and does not call `st_latest()` or hash the
loaded data tables.

The hash map is passed explicitly through `pd_process_data()` to
`valid_dlw_load()` and `build_pip_inventory()`.

### 2. Use a two-stage aux gate

`valid_dlw_load()` performs:

1. **Artifact gate** — compare the current run's aux hash with the stored
   `aux_<measure>_hash` for the matching survey and DLW content version.
2. **Affected-row gate** — call `valid_aux_load()` only for measures whose
   artifact hash changed, then intersect its changed country/year keys with the
   candidate surveys requested in `inv`.

This prevents a CPI change for USA/GER from re-cleaning already-cleaned COL/ARG
surveys when COL/ARG rows were not changed.

The master inventory is loaded once within `valid_dlw_load()` and shared with
DLW and aux comparisons. Direct callers that omit `aux_hashes` have hashes
resolved internally; `force = TRUE` bypasses all comparisons.

### 3. Match the current DLW version explicitly

For both DLW and aux comparisons, rename the current inventory's
`content_hash` to `content_hash_dlw` for the join and use joyn's explicit
same-name key:

```r
inv_join <- data.table::copy(inv)
data.table::setnames(inv_join, "content_hash", "content_hash_dlw")

inv_compare <- joyn::left_join(
  inv_join,
  dt_master_hash,
  by = c("survey_id", "content_hash_dlw"),
  relationship = "many-to-one",
  reportvar = ".joyn",
  verbose = FALSE
)
```

The `.joyn` report value identifies matched versus unmatched current DLW
versions. It is removed before returning production data.

### 4. Replace all rows for a reprocessed survey

`build_pip_inventory()` retains old master rows by `survey_id`, not by
`pip_id`:

```r
reprocessed_surveys <- unique(new_versions$survey_id)
old_retained <- old_inv[!old_inv$survey_id %in% reprocessed_surveys]
run_inv <- collapse::rowbind(new_versions, old_retained, fill = TRUE)
```

Thus the current master snapshot's `pip_id` set exactly matches the latest
successful reprocess. Previous snapshots remain recoverable through stamp
version history.

### 5. Validate and test the contracts

The implementation validates supplied `aux_hashes` as a named, unique,
non-empty character vector with non-missing values. Tests cover:

- PFW and all requested aux measures;
- historical DLW versions;
- conflicting aux hashes within the same survey/content version;
- stale welfare-type `pip_id` removal;
- direct callers and force mode;
- deterministic catalog tie-breaking;
- stateful master persistence/reload;
- requested-country intersection;
- release/vintage merge semantics in `valid_aux_load(compare = "all")`.

## Prevention

- Treat stamp `content_hash` as the artifact-level change gate; do not use
  `version_id` as the content-change signal.
- Keep the artifact-level hash gate and the row-level `compare_aux_*` filter;
  either one alone is insufficient.
- Always match survey-level master facts by both `survey_id` and the current
  DLW `content_hash_dlw`.
- Replace the full current row set for a reprocessed survey; do not retain
  stale welfare-type rows from an older snapshot.
- Use explicit `reportvar` settings on joyn joins. If `.joyn` is needed for
  match classification, remove it before returning production data.
- Preserve stamp artifact versions for historical recovery, but do not confuse
  artifact history with rows in the current loaded master snapshot.
- Run the full test suite after changes to inventory keys, stamp joins, or
  aux-change selection logic.

## Related

- `.cg-docs/brainstorms/2026-08-05-aux-version-gate-valid-dlw-load.md`
- `.cg-docs/plans/2026-08-06-aux-version-gate-valid-dlw-load-revised.md`
- `.cg-docs/reviews/2026-08-06-aux-version-gate-valid-dlw-load-revised-review.md`
- `.cg-docs/reviews/2026-08-06-aux-version-gate-valid-dlw-load-revised-verify-review.md`
- `.cg-docs/solutions/data-quality/2026-06-05-joyn-diagnostic-column-discipline.md`
- `.cg-docs/solutions/data-quality/2026-05-27-legacy-column-persistence-in-on-disk-inventory.md`
- `.cg-docs/solutions/data-quality/2026-05-27-catalog-validation-before-filter-spurious-warnings.md`
- `.cg-docs/solutions/bugs/2026-05-05-stamp-version-id-vs-content-hash.md`
