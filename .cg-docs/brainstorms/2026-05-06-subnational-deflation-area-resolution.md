---
date: 2026-05-06
title: "Fix subnational deflation: resolve area attribute to column values"
status: decided
scope: "Standard"
chosen-approach: "Hybrid — fast fix add_rep_lvl/add_ppp/add_cpi + roadmap item"
tags: [deflation, subnational, area, ppp_data_level, cpi_data_level, pipgd, pipmd]
---

# Fix subnational deflation: resolve area attribute to column values

## Context

The new pipeline stores `ppp_data_level` and `cpi_data_level` as scalar
attributes on survey data.tables. `add_dom_vars()` sets these to `"area"`
when the PFW indicates subnational domains (reporting_level == 2,
cpi_domain_var == "urban").

The deflation code (`add_rep_lvl()`, `add_ppp()`, `add_cpi()`) then uses
these attribute values as lookup keys into named CPI/PPP vectors. But the
named vectors use actual level names (`"rural"`, `"urban"`, `"national"`) —
never the literal `"area"`. This causes all subnational surveys (e.g., CHN
group data) to get `NA` deflation values.

The old pipeline (`process_svy_data_to_cache.R`) handled this correctly:
`ppp_data_level` and `cpi_data_level` were per-row columns with actual
level values, and PPP/CPI were merged by those columns directly.

## Requirements

1. When `ppp_data_level == "area"` or `cpi_data_level == "area"`, deflation
   must resolve to the per-row values of the `area` column in the data.table.
2. When the attribute is `"national"`, behaviour stays unchanged (scalar
   broadcast to all rows).
3. Both `pipmd` and `pipgd` must be supported — any survey with a subnational
   domain in the PFW should deflate correctly regardless of distribution type.
4. Mixed cases (e.g., `ppp_data_level = "area"`, `gdp_data_level = "national"`)
   must still work.
5. The `"national"` entries in the named PPP/CPI vectors are unused for surveys
   deflated at the subnational level (urban/rural PPP used per-row).

## Approaches Considered

### Approach 1: Fix add_rep_lvl() to resolve "area" → column values (fast fix)

Modify `add_rep_lvl()` so that when `ppp_data_level == "area"`, it assigns
`reporting_level := dt$area` per-row. Similarly modify `add_ppp()` and
`add_cpi()` to use `dt$area` as per-row lookup keys when the attribute is "area".

- **Pros**: Minimal change (~30 lines), keeps add_dom_vars() unchanged, works
  for both pipmd and pipgd
- **Cons**: The "area" = pointer-to-column convention remains implicit
- **Effort**: Small

### Approach 2: Store resolved level values in attributes (architectural fix)

Change `add_dom_vars()` to store actual domain values (e.g., `c("rural", "urban")`)
instead of `"area"`. Downstream code uses these directly.

- **Pros**: Explicit semantics
- **Cons**: Breaks scalar-attribute contract, requires changes across many files
- **Effort**: Medium

### Approach 3: Hybrid — fast fix now + roadmap item for cleaner semantics

Implement Approach 1 now. Add a roadmap item to revisit the attribute semantics
in a future milestone.

- **Pros**: Ship the fix, track the debt
- **Effort**: Small now + future item

## Decision

Approach 3 chosen. The fast fix in `add_rep_lvl()`, `add_ppp()`, and `add_cpi()`
will resolve the `"area"` attribute to per-row `dt$area` column values. A roadmap
item will be created for a future architectural improvement to make the
pointer-to-column semantics more explicit.

## Next Steps

1. Modify `add_rep_lvl()` in `pd_deflation.R`: when `rep_lvl == "area"`,
   assign `dt[, reporting_level := area]` instead of the literal scalar.
2. Modify `add_ppp()`: when `attr(dt, "ppp_data_level") == "area"`, use
   `dt$area` per-row as the lookup key into the named PPP vector.
3. Modify `add_cpi()`: same logic for `cpi_data_level`.
4. Update/add tests for the subnational case (CHN-like fixture with
   urban/rural rows and area-keyed named vectors).
5. Add roadmap item for future semantic cleanup of `*_data_level` attributes.
