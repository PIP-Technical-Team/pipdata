---
date: 2026-05-06
title: "Subnational deflation produces NA: ppp_data_level='area' not resolved to per-row column values"
category: "bugs"
type: "bug"
language: "R"
tags: [deflation, subnational, area, ppp_data_level, cpi_data_level, add-rep-lvl, add-ppp, add-cpi, pipgd, pipmd, urban, rural, national]
root-cause: "add_dom_vars() stores 'area' as a column-pointer in ppp/cpi_data_level attributes, but add_rep_lvl/add_ppp/add_cpi treat the attribute as a literal level name — so named-vector lookup fails with NA for all urban/rural surveys."
severity: "P1"
test-written: "no"
fix-confirmed: "brainstorm-decided"
---

# Subnational deflation produces NA: `ppp_data_level = "area"` not resolved to per-row column values

## Symptom

All surveys with subnational domain (urban/rural in PFW) are silently
deflated to `NA`. This affects group data (e.g., all CHN surveys) and any
microdata survey where `cpi_domain_var == "urban"` in the PFW.

The CPI/PPP named vectors have valid entries keyed by `"rural"` and `"urban"`:

```r
# CPI vector for a subnational survey
c("2011_rural" = 1.0, "2011_urban" = 1.0,
  "2017_rural" = 0.848, "2017_urban" = 0.882)

# PPP vector
c("ppp_2011_02_02_rural" = 3.039, "ppp_2011_02_02_urban" = 3.905,
  "ppp_2011_02_02_national" = 3.698)
```

But `add_ppp()` and `add_cpi()` look up `lev_map["area"]` → `NA`.

## Root Cause

`add_dom_vars()` in `pd_cpfw_merge.R` sets `*_data_level` attributes to the
string `"area"` in all cases where the PFW domain is subnational
(reporting_level == 2, cpi_domain_var == "urban"):

```r
setattr(dt, "ppp_data_level", "area")  # pointer to a column name
setattr(dt, "cpi_data_level", "area")
```

The intent is: `"area"` means "look at the `area` column in the data for the
per-row level value." But the downstream deflation helpers treat the attribute
value as a **literal level name** to look up in named vectors:

```r
# add_rep_lvl(): assigns literal "area" to all rows
ppp_lvl <- attr(dt, "ppp_data_level")  # = "area"
dt[, reporting_level := ppp_lvl]        # every row gets "area" — wrong

# add_ppp(): looks up "area" in the named PPP vector → NA
lev_map <- setNames(ppp[idx], report_levels[idx])  # keys: "national","rural","urban"
dt[, (v) := lev_map[ppp_lvl]]                       # lev_map["area"] = NA
```

This is a **semantic mismatch**: `"area"` is used as a column-name pointer
upstream, but as a level-name token downstream. The old pipeline
(`process_svy_data_to_cache.R`) avoided this by using `ppp_data_level` and
`cpi_data_level` as actual per-row columns (containing `"urban"`, `"rural"`,
`"national"`), which were merged against PPP/CPI tables directly.

## Solution

**Phase 1 (fast fix) — modify `add_rep_lvl()`, `add_ppp()`, `add_cpi()` in
`pd_deflation.R`**: when the attribute is `"area"`, resolve it to the per-row
values of `dt$area` instead of using the scalar literally.

### `add_rep_lvl()` fix

```r
add_rep_lvl <- function(dt) {
  ppp_lvl <- attr(dt, "ppp_data_level")
  rep_lvl <- if (!is.null(ppp_lvl)) ppp_lvl else attr(dt, "cpi_data_level")

  if (is.null(rep_lvl)) {
    cli::cli_abort(
      "Cannot determine reporting level: no {.field ppp_data_level} or {.field cpi_data_level} attribute.",
      class = c("add_rep_lvl", "piperr")
    )
  }

  if (rep_lvl == "area") {
    if (!"area" %in% names(dt)) {
      cli::cli_abort(
        "{.field ppp_data_level} is 'area' but no {.field area} column found in {.arg dt}.",
        class = c("add_rep_lvl", "piperr")
      )
    }
    dt[, reporting_level := area]
  } else {
    dt[, reporting_level := rep_lvl]
  }

  setorder(dt, reporting_level)
  dt
}
```

### `add_ppp()` fix (named-vector path)

```r
# replace scalar lookup:
#   dt[, (v) := lev_map[ppp_lvl]]
# with per-row lookup when ppp_lvl == "area":
ppp_lvl <- attr(dt, "ppp_data_level")
if (ppp_lvl == "area") {
  dt[, (v) := lev_map[area]]     # per-row lookup via the area column
} else {
  dt[, (v) := lev_map[ppp_lvl]]  # scalar broadcast (national case)
}
```

### `add_cpi()` fix (named-vector path)

Same pattern replacing `cpi_lvl`/scalar broadcast with `dt$area` per-row
lookup when `cpi_data_level == "area"`.

**Phase 2 (future refactor)** — tracked as roadmap item
`explicit-data-level-semantics`: make the pointer convention explicit instead
of implicit, e.g., store `list(column = "area", values = c("rural", "urban"))`
or always store resolved level values.

## Prevention

- When any `*_data_level` attribute equals a **column name** rather than a
  level value (`"national"`, `"urban"`, `"rural"`), all downstream helpers
  that use that attribute as a lookup key must resolve it to per-row column
  values first.
- The canonical recognized `*_data_level` values are: `"national"`, `"area"`
  (pointer → area column). Any new value should be documented explicitly.
- Tests for subnational deflation should use a fixture with an `area` column
  containing `c("rural", "urban")` rows and named-vector CPI/PPP with
  `"{year}_rural"` and `"{year}_urban"` keys.

## Related

- `.cg-docs/solutions/bugs/2026-05-05-data-level-columns-stripped-on-stamp-round-trip.md`
  — earlier related fix that introduced the current attribute-only approach in
  `add_rep_lvl()`. The fix there introduced the attribute path but did not
  handle the `"area"` pointer case.
- `.cg-docs/brainstorms/2026-05-06-subnational-deflation-area-resolution.md`
  — brainstorm that diagnosed this issue and decided the approach.
- `roadmap.json` → `explicit-data-level-semantics` — future Phase 2 cleanup.
- `R/pd_deflation.R`: `add_rep_lvl()`, `add_ppp()`, `add_cpi()`
- `R/pd_cpfw_merge.R`: `add_dom_vars()` (source of the `"area"` pointer)
- `pip_ingestion_pipeline/R/pipdm/R/process_svy_data_to_cache.R`: old
  pipeline reference — used per-row column values directly, no pointer.
