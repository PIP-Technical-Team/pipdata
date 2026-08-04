---
date: 2026-05-06
title: "Subnational deflation produces NA: ppp_data_level='area' not resolved to per-row column values"
category: "bugs"
type: "bug"
language: "R"
tags: [deflation, subnational, area, ppp_data_level, cpi_data_level, pop_data_level, add-ppp, add-cpi, adjust-population, pipgd, pipmd, urban, rural, national, mixed-domain]
root-cause: "add_dom_vars() stores 'area' as a column-pointer in *_data_level attributes, but add_rep_lvl/add_ppp/add_cpi treated the attribute as a literal level name — so named-vector lookup fails with NA for all urban/rural surveys. Compounded by add_rep_lvl() being an unnecessary intermediary that translates per-function semantics into a shared column."
severity: "P1"
test-written: "no"
fix-confirmed: "yes"
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

**Revised approach (2026-05-07)**: Remove `add_rep_lvl()` entirely and have
each deflation function branch directly on its **own** `*_data_level` attribute.
This correctly handles the **mixed-domain case** where `reporting_level == 2`
but one domain (e.g., `ppp_data_level`) is `"national"` because `ppp_domain` 
== 1 in the PFW — using a shared integer discriminator would incorrectly
trigger per-row lookup in `add_ppp()` even for a national PPP vector.

**Key design rule**: each function checks its own attr:

```r
# add_ppp() — branches on ppp_data_level, not reporting_level integer
ppp_lvl <- attr(dt, "ppp_data_level")
for (v in unique_versions) {
  idx <- ppp_versions == v
  lev_map <- stats::setNames(ppp[idx], report_levels[idx])
  if (identical(ppp_lvl, "area")) {
    dt[, (v) := lev_map[as.character(area)]]  # per-row lookup
  } else {
    dt[, (v) := lev_map[ppp_lvl]]            # scalar broadcast ("national")
  }
}
```

```r
# add_cpi() — same, branches on cpi_data_level
cpi_lvl <- attr(dt, "cpi_data_level")
for (yr in unique_years) {
  col <- paste0("cpi", yr)
  idx <- cpi_years == yr
  lev_map <- stats::setNames(cpi[idx], report_levels[idx])
  if (identical(cpi_lvl, "area")) {
    dt[, (col) := lev_map[as.character(area)]]  # per-row lookup
  } else {
    dt[, (col) := lev_map[cpi_lvl]]             # scalar broadcast
  }
}
```

```r
# adjust_population() guard — branches on pop_data_level, not reporting_level integer
if (identical(attr(dt_c, "pop_data_level"), "area")) {
  dt_c <- adjust_population(dt_c, pop)
}
```

`adjust_population()` itself uses the `area` column for grouping/joining 
instead of the old `reporting_level` column:

```r
# Before:
spop <- df[, .(weight = sum(weight)), by = "reporting_level"]
# After:
spop <- df[, .(weight = sum(weight)), by = "area"]
```

**Why integer `reporting_level` attr is not sufficient**: `add_dom_vars()` in
`pd_cpfw_merge.R` handles a mixed-domain case (`any(same_rep_lvl == FALSE)`)
where each `*_data_level` is set independently. A survey can have
`reporting_level == 2` but `ppp_data_level == "national"` (ppp_domain == 1).
Branching on the integer would incorrectly apply per-row lookup to a national
PPP vector, producing NA. Branching on each function's own attr is exact.

**Plan reference**: `.cg-docs/plans/2026-05-06-subnational-deflation-fast-fix.md`

---

**~~Previously documented approach (superseded)~~**

~~Phase 1 fast fix: modify `add_rep_lvl()` to resolve `"area"` to `dt$area`
before broadcasting to the `reporting_level` column. Superseded because
`add_rep_lvl()` is now removed entirely — the function was both the source of
the intermediate column and the place where per-function semantics were
incorrectly merged into a single discriminator.~~

## Prevention

- **Each deflation function must branch on its own `*_data_level` attr**, not
  a shared integer discriminator. `ppp_data_level`, `cpi_data_level`, and
  `pop_data_level` are set independently in the mixed-domain path of
  `add_dom_vars()` and can differ for the same survey.
- When any `*_data_level` attribute equals `"area"`, resolve it to the per-row
  values of the `area` column in `dt` — never use `"area"` as a literal lookup
  key in named PPP/CPI/pop vectors.
- Do **not** introduce an intermediate `reporting_level` column to translate
  per-function attribute semantics into a shared token. Each function has the
  precise answer in its own attribute.
- Assert `"area" %in% names(dt)` before per-row lookup instead of silently
  producing `NA`.
- The canonical recognized `*_data_level` values are: `"national"` (literal),
  `"area"` (pointer to `area` column).
- `adjust_population()` guard must check `pop_data_level == "area"`, not 
  `reporting_level integer == 2`: a subnational survey can have population data
  at the national level (`pop_domain == 1` in PFW).
- Tests for subnational deflation must include an `area` column with
  `c("rural", "urban")` rows and named-vector CPI/PPP/pop with level-suffixed
  keys (`"YEAR_rural"`, `"YEAR_urban"`).

## Related

- `.cg-docs/solutions/bugs/2026-05-05-data-level-columns-stripped-on-stamp-round-trip.md`
  — earlier related fix that introduced the attribute-only approach. The fix
  there introduced `add_rep_lvl()` attribute fallback but did not handle the
  `"area"` pointer case or the mixed-domain problem.
- `.cg-docs/brainstorms/2026-05-06-subnational-deflation-area-resolution.md`
  — brainstorm that diagnosed the issue and chose the hybrid approach.
- `.cg-docs/plans/2026-05-06-subnational-deflation-fast-fix.md` — full
  implementation plan with per-step code patterns.
- `roadmap.json` → `explicit-data-level-semantics` — future cleanup: make the
  pointer convention explicit rather than implicit.
- `R/pd_deflation.R`: `add_ppp()`, `add_cpi()`, `adjust_population()`,
  `.deflation_pipmd_core()`, `.deflation_pipgd_core()`
- `R/pd_cpfw_merge.R`: `add_dom_vars()` (source of the `"area"` pointer),
  `add_main_att()` (source of integer `reporting_level` attribute)
