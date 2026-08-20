---
date: 2026-08-17
title: "pip_id to survey_id reverse-map requires dedup and uniqueness assertion"
category: "data-quality"
language: "R"
tags: [data-quality, pip_id, survey_id, reverse-map, dedup, master-inventory, force_surveys, build_pip_inventory]
root-cause: "The master inventory retains multiple rows per survey (historical content_hash_dlw), so a naive pip_id→survey_id reverse-map built from the raw master is ambiguous and silently resolves to an arbitrary row."
severity: "P2"
---

# pip_id → survey_id reverse-map requires dedup and uniqueness assertion

## Problem

When resolving a user-supplied `pip_id` back to its `survey_id` (e.g. the
`force_surveys` surgical re-processing feature), building a reverse-map from the
raw master inventory and indexing it picks one row per duplicate silently:

```r
# BAD: dt_master has multiple rows per survey (historical content hashes);
# duplicate pip_id names -> `[[` returns the first, name-index-dependent.
master_pip_key <- stats::setNames(dt_master$survey_id, toupper(dt_master$pip_id))
svy <- master_pip_key[[toupper(id)]]
```

A `pip_id` can legitimately repeat across historical `content_hash_dlw` rows of
the *same* survey. Without dedup, the reverse-map can resolve to a stale/old-row
`survey_id` — silently misclassifying a valid current `pip_id` as unknown (R9
out-of-filter) or forcing the wrong survey.

## Root Cause

`build_pip_inventory()` (via `inv_to_process()`/`aux_hash_candidates()`) retains
multiple `content_hash_dlw` rows per `survey_id` — one per DLW source-version.
The previously-clean assumption "one row per `(survey_id, pip_id)`" is violated
by this multi-row history, so a reverse map keyed on `pip_id` alone is not a
function.

## Solution

Before building the reverse-map, reduce the master to one row per
`(pip_id, survey_id)` (this collapses legitimate historical repeats), then
**assert** no `pip_id` maps to more than one *distinct* `survey_id`; abort loudly
if so rather than silently picking:

```r
pip_map <- collapse::funique(dt_master[, .(pip_id, survey_id)])
n_distinct <- collapse::fndistinct(pip_map$pip_id)
if (n_distinct != nrow(pip_map)) {
  ambiguous <- pip_map[duplicated(pip_map$pip_id), pip_id][1L]
  cli::cli_abort(
    "pip_id '{ambiguous}' maps to multiple distinct survey_ids; cannot resolve force_surveys.",
    class = "piperr"
  )
}
master_pip_key <- stats::setNames(pip_map$survey_id, toupper(pip_map$pip_id))
```

Key points:
- `funique` on `(pip_id, survey_id)` removes rows where the same `pip_id` and
  `survey_id` repeat (historical content hashes) — these are NOT ambiguity.
- The `fndistinct(pip_id) != nrow` check catches a `pip_id` that genuinely maps
  to two **different** `survey_id`s — a true blocked-stop (matches the
  plan's blocked-stop condition).
- `toupper()` makes the lookup case-insensitive (mirrors how `pip_id_map` is
  built in `pd_process_data.R`).

## Prevention

- Never build a `pip_id`→`survey_id` `setNames` reverse-map directly from raw
  master rows; always dedup on the composite key first.
- Treat "unique or abort" as the default for any lookup intended as a function
  when the input table is not guaranteed key-unique.
- Cover with a regression test where the same `pip_id` appears on two master
  rows with distinct `survey_id`s (must abort with `piperr`) and where it
  repeats with the *same* `survey_id` (must resolve, not abort).

## Related

- `.cg-docs/solutions/data-quality/2026-08-07-aux-content-hash-gated-recleaning.md` — the master one-row-per-survey content-hash semantics this builds on
- `.cg-docs/solutions/data-quality/2026-06-05-joyn-diagnostic-column-discipline.md` — dplyr/data.table column discipline for joins
- `.cg-docs/plans/2026-08-17-force-surveys-surgical-reprocessing.md` — the feature that surfaced this