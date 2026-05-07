---
date: 2026-05-07
title: "Deflation output contract: welfare_lcu, welfare_vars, adj_pop"
category: "data-quality"
language: "R"
tags: [deflation, output-contract, welfare_vars, adj_pop, welfare_lcu, attributes, pd_deflation]
root-cause: "No documented contract on what pd_deflation() returns — redundant welfare column, no metadata attributes on the result"
severity: "P2"
---

# Deflation Output Contract: welfare_lcu, welfare_vars, adj_pop

## Problem

`pd_deflation()` returned both a `welfare` column and a `welfare_lcu` column,
where `welfare_lcu` was created as a direct copy of `welfare` before the
`deflate_wlf()` step. This caused two issues:

1. **Redundant column**: `welfare` and `welfare_lcu` were always identical in
   value. Downstream code had to know to use `welfare_lcu` (the pre-deflation
   LCU value) and ignore `welfare`.
2. **No discovery metadata**: callers had no structured way to find which
   welfare columns were created or whether population adjustment was applied.
   Typically required `grep("^welfare_", names(result))` or manual inspection.

## Root Cause

The `welfare_lcu()` helper copies `welfare` into `welfare_lcu` but the
original `welfare` column was never dropped. `finalize_deflation_output()`
placed `welfare` first in column order, reinforcing its presence. No attributes
were set on the output to advertise the structure of the result.

## Solution

### 1. Remove `welfare` after deflation

In both `.deflation_pipmd_core()` and `.deflation_pipgd_core()`, drop the
`welfare` column immediately before calling `finalize_deflation_output()`:

```r
# Remove welfare column (welfare_lcu is the canonical version after deflation)
dt_c[, welfare := NULL]
result <- finalize_deflation_output(char_to_fct(dt_c))
```

`welfare_lcu` is the canonical pre-deflation LCU welfare variable.
`welfare_ppp_*` columns are the deflated outputs.

### 2. Set `welfare_vars` attribute

After finalizing the output, set an attribute listing all welfare columns:

```r
welfare_cols <- grep("^welfare_", names(result), value = TRUE)
data.table::setattr(result, "welfare_vars", welfare_cols)
```

Callers can then use `attr(result, "welfare_vars")` instead of grep.

### 3. Set `adj_pop` attribute

Track whether population adjustment was applied and attach it as a logical
attribute:

```r
# In .deflation_pipmd_core():
adj_pop <- identical(attr(dt_c, "pop_data_level"), "area")
if (adj_pop) dt_c <- adjust_population(dt_c, pop)
...
data.table::setattr(result, "adj_pop", adj_pop)

# In .deflation_pipgd_core(): always FALSE
data.table::setattr(result, "adj_pop", FALSE)
```

### 4. Update column ordering

`finalize_deflation_output()` now anchors on `welfare_lcu` and `weight`
(not `welfare` and `weight`):

```r
anchor    <- intersect(c("welfare_lcu", "weight"), nms)
new_block <- intersect(c(wlf_ppp, "area", ppp_cols, cpi_cols), nms)
rest      <- setdiff(nms, c(anchor, new_block))
data.table::setcolorder(dt, c(anchor, new_block, rest))
```

## Canonical Output Structure

After a successful `pd_deflation()` call the result is a `pipmd`/`pipgd`
`data.table` with the following guaranteed structure:

| Position | Column(s) | Description |
|----------|-----------|-------------|
| 1 | `welfare_lcu` | Welfare in local currency (before deflation) |
| 2 | `weight` | Survey weight (may be population-adjusted) |
| 3+ | `welfare_ppp_YYYY_RR_AA` | Deflated welfare, newest base year first |
| next | `area` | Area category (only for subnational surveys) |
| next | `ppp_YYYY_RR`, `cpiYYYY` | Raw PPP/CPI columns used for deflation |
| rest | all other columns | Original survey columns |

**Attributes**:
- `welfare_vars`: `character` vector of welfare column names (e.g., `c("welfare_lcu", "welfare_ppp_2017_01_01")`)
- `adj_pop`: `logical`; `TRUE` if `adjust_population()` was applied (subnational `pipmd` surveys only)

## Prevention

- **Never return a column that is a plain copy of another column.** When
  `welfare_lcu := welfare` is created for the deflation chain, drop `welfare`
  immediately before returning.
- **Always set discovery attributes on complex outputs.** Callers shouldn't
  have to grep column names to find welfare variables — expose them via
  `welfare_vars` attribute.
- **`adj_pop`** lets batch orchestrators (e.g., `pd_deflate_pipeline()`)
  filter or log which surveys had population adjustment without re-inspecting
  the data.

## Related

- [2026-05-06-subnational-deflation-area-attribute-not-resolved.md](../bugs/2026-05-06-subnational-deflation-area-attribute-not-resolved.md) — the fix that
  established `pop_data_level == "area"` as the guard condition for
  `adjust_population()`, which `adj_pop` now reflects
- [2026-05-05-data-level-columns-stripped-on-stamp-round-trip.md](../bugs/2026-05-05-data-level-columns-stripped-on-stamp-round-trip.md) — related precedent
  that `*_data_level` values are attributes, never columns; same principle
  applied here to `welfare_vars`/`adj_pop`
