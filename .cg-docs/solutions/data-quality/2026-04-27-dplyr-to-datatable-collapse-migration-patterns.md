---
date: 2026-04-27
title: "dplyr/tidyr/tibble → data.table/collapse migration patterns"
category: "data-quality"
language: "R"
tags: [dplyr, data.table, collapse, fcase, rbindlist, migration, refactoring]
root-cause: "dplyr/tidyr/tibble used via :: without being declared in DESCRIPTION Imports — phantom dependencies"
severity: "P2"
---

# dplyr/tidyr/tibble → data.table/collapse Migration Patterns

## Problem

Functions used `dplyr::`, `tidyr::`, and `tibble::` without declaring these
packages in `DESCRIPTION Imports`. This creates phantom dependencies — the
package works only if those packages happen to be loaded in the session,
not because they are formally required.

## Root Cause

Incremental development: authors familiar with tidyverse used `pkg::fn()`
shorthand without adding the package to Imports, which is otherwise caught
by `devtools::check()` only if called in a top-level context.

## Solution

### `dplyr::case_when` → `data.table::fcase()`

```r
# Before
collapse::ftransform(
  educy = dplyr::case_when(
    educy < 0 ~ NA_real_,
    educy >= 0 & educy <= 50 ~ educy,
    educy > 50 ~ NA_real_,
    .default = NA_real_
  )
)

# After — note commas instead of ~, and default= not .default=
collapse::ftransform(
  educy = fcase(
    educy < 0, NA_real_,
    educy >= 0 & educy <= 50, educy,
    educy > 50, NA_real_,
    default = NA_real_
  )
)
```

Key differences:
- `case_when`: `condition ~ value`, `.default = NA`
- `fcase`: `condition, value`, `default = NA` (no dot prefix)
- `fcase` works inside `collapse::ftransform()` and `collapse::fmutate()`
  just like inside `data.table[, := ]`

### `dplyr::bind_rows(list)` → `data.table::rbindlist(list, fill = TRUE)`

```r
# Before
final_inv <- dplyr::bind_rows(new_inv) |>
  pipload::survey_id_to_vars() |>
  tidyr::as_tibble() |>
  data.table::as.data.table()

# After — rbindlist already returns data.table; as_tibble round-trip is removed
final_inv <- data.table::rbindlist(new_inv, fill = TRUE) |>
  pipload::survey_id_to_vars()
```

Notes:
- `fill = TRUE` matches `bind_rows` behaviour for lists with mismatched columns
- `rbindlist` silently skips NULL entries in the list (same as `bind_rows`)
- `survey_id_to_vars()` calls `data.table::as.data.table()` internally, so the
  `tidyr::as_tibble() |> data.table::as.data.table()` round-trip was redundant

### `dplyr::count(col)` → `[, .(n = .N), keyby = col]`

```r
# Before — returns a tibble, sorted alphabetically
valid_data |> dplyr::count(data_status)

# After — returns a data.table; use keyby (not by) to preserve sort order
valid_data[, .(n = .N), keyby = data_status]
```

Important: `dplyr::count()` sorts the output. `[, .N, by = col]` preserves
insertion order. Use `keyby` when the output must be sorted, **or** when the
grouping variable is a factor with defined levels (factor ordering is already
deterministic with `by`).

### `tidyr::as_tibble()` → remove

If data already flows through `data.table`-returning functions, `as_tibble()`
followed immediately by `as.data.table()` is a no-op. Remove both.

## Prevention

- Run `grep -r "dplyr::\|tidyr::\|tibble::" R/` periodically to catch phantom
  dependencies before they accumulate.
- Any `pkg::fn()` call must have `pkg` in `DESCRIPTION Imports` or `Suggests`.
- `devtools::check()` will flag missing Imports as a NOTE.

## Related

- [build-errors/2026-04-16-r-cmd-check-no-visible-binding-datatable-nse.md](../build-errors/2026-04-16-r-cmd-check-no-visible-binding-datatable-nse.md) — related data.table NSE patterns
