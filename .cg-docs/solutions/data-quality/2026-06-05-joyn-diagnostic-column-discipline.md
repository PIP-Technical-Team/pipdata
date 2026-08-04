---
date: 2026-06-05
title: "joyn diagnostic columns in production data paths — reportvar and suffix discipline"
category: "data-quality"
language: "R"
tags: [joyn, reportvar, suffix, anti_join, left_join, inner_join, .joyn, schema, production-data, data-quality]
root-cause: "joyn appends diagnostic columns (.joyn factor, .x/.y suffixes) by default; when join output feeds production data paths, these columns contaminate schema checks, unique() deduplication, and persisted artifacts"
severity: "P1"
---

# joyn diagnostic columns in production data paths — `reportvar` and suffix discipline

## Problem

Two distinct classes of bug in this codebase were caused by the same underlying
pattern: `joyn::` join diagnostic columns surviving into production data.

**Class 1 — `.joyn` report column** (2026-06-05):  
`joyn::anti_join(..., verbose = FALSE)` without `reportvar = FALSE` appended a
`.joyn` factor column to `inv_svy`. When `rbind`-ed with `inv_aux` (no `.joyn`),
`fill = TRUE` set `.joyn = NA` for aux rows. `unique()` treated `.joyn = "x"` and
`.joyn = NA` as distinct rows for the same `survey_id`, bypassing deduplication.
`build_pip_inventory()` then aborted on `anyDuplicated(inv_to_clean$survey_id) == 0L`.

**Class 2 — `.x`/`.y` suffix columns** (2026-05-20):  
`joyn::left_join()` suffixed a shared column (`reporting_level`) as
`reporting_level.x` / `reporting_level.y` when the column was present in both
tables but not listed in `by`. The suffixed result was persisted to the master
inventory artifact. On reload + `rowbind`, all three column variants survived,
creating a self-perpetuating three-column schema artifact.

## Root Cause

`joyn::` functions are designed for interactive exploratory use where diagnostic
output is valuable. Their defaults prioritise visibility:
- `reportvar = TRUE` (default) → always appends `.joyn` factor column
- suffix behaviour → appends `.x`/`.y` when a non-key column is shared

In a production data pipeline where joins feed `rbind`, `unique()`, schema
assertions, or persisted artifacts, these diagnostic columns silently corrupt the
data schema.

## Solution

### Rule 1: Always pass `reportvar = FALSE` on production `joyn::` calls

Every `joyn::anti_join`, `joyn::inner_join`, and `joyn::left_join` call whose
result is stored in a variable or passed to downstream pipeline steps must
include `reportvar = FALSE`:

```r
# BAD — .joyn leaks into production data
inv_svy <- joyn::anti_join(inv, dt_master, by = key_vars, verbose = FALSE)

# GOOD — .joyn suppressed
inv_svy <- joyn::anti_join(inv, dt_master, by = key_vars, verbose = FALSE,
                           reportvar = FALSE)
```

### Rule 2: Ensure all non-key shared columns are listed in `by` or renamed before joining

When two tables share a column name that is NOT a join key, joyn renames it with
`.x`/`.y` suffixes. The fix is either:
- include the column in `by` if the semantics match, or
- rename/drop one copy before the join.

```r
# BAD — reporting_level in both tables → .x/.y suffixes
result <- joyn::left_join(new_inv, old_inv, by = "pip_id", verbose = FALSE)

# GOOD — either include in by, or drop before joining
old_inv[, reporting_level := NULL]
result <- joyn::left_join(new_inv, old_inv, by = "pip_id", verbose = FALSE,
                          reportvar = FALSE)
```

### Rule 3: Never persist raw `joyn::` output without a schema audit

Before passing `joyn::` output to `pip_write()` / `st_save()` or returning it
from a function, verify `names(result)` contains no `.joyn`, no `.x`, no `.y`
columns. A one-liner guard:

```r
stopifnot(!any(grepl("^\\.joyn$|\\.(x|y)$", names(result))))
```

## Prevention

**Audit checklist** — run this grep across `R/` when writing or reviewing a join:

```r
# Find all joyn call sites missing reportvar = FALSE
grep_search(pattern = "joyn::(anti|inner|left|right)_join", isRegexp = TRUE)
# For each hit: confirm reportvar = FALSE is present
```

**Pattern to follow**: treat `joyn::` like `data.table::merge()` in production —
always explicit, always schema-checked. Use `joyn::` diagnostics (default
`reportvar = TRUE`) only in exploratory/REPL work.

**Anti-pattern to avoid**: copying a join call from an exploratory script into
production code without auditing for diagnostic column side-effects.

## Related

- `.cg-docs/solutions/bugs/2026-06-05-joyn-anti-join-reportvar-duplicate-survey-id.md` — the Class 1 bug fixed by adding `reportvar = FALSE` to `inv_to_process()`
- `.cg-docs/solutions/bugs/2026-05-20-joyn-suffix-collision-persisted-to-inventory.md` — the Class 2 bug where `.x`/`.y` suffixes were persisted to the master inventory
