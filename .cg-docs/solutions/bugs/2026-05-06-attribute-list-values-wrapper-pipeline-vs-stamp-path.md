---
date: 2026-05-06
updated: 2026-05-06
title: "RETRACTED: list(values=...) was a fixture bug, not a real pipeline pattern"
category: "bugs"
type: "retraction"
language: "R"
tags: [attributes, stamp, round-trip, pipmd, pipgd, retraction, fixture-bug]
root-cause: "The list(values=X) attribute wrapper was introduced only by an incorrect make_pipmd() test fixture. It never existed in real pipeline objects."
severity: "P1"
test-written: "yes"
fix-confirmed: "yes"
---

# RETRACTED — `list(values=...)` attribute wrapper

## Retraction Notice

An earlier version of this document (and several `is.list()` guards in
`pd_deflation.R`) was based on the incorrect belief that survey attributes are
stored as `list(values = X)` on the live pipeline path.

**This is false.** The `list(values = X)` structure was introduced solely by
the `make_pipmd()` test fixture; it was never emitted by the real pipeline code
(`vars_to_attr()` or any other function).

All `is.list()` unwrapper guards have been removed from `pd_deflation.R` and
the `make_pipmd()` fixture has been corrected to use plain scalars.

---

## Real Attribute Structure

Survey attributes on real `pipmd`/`pipgd` objects (both in-memory pipeline path
and after stamp round-trip) are **always plain scalars or character vectors**:

```r
attr(dt, "survey_id")       # "BOL_2022_EH_V01_M_V02_A_GMD_ALL"   chr(1)
attr(dt, "country_code")    # "BOL"                                chr(1)
attr(dt, "surveyid_year")   # 2022                                  dbl or chr
attr(dt, "survey_acronym")  # "EH"                                 chr(1)
attr(dt, "welfare_type")    # "income"                             chr(1)
attr(dt, "ppp_data_level")  # "national"                           chr(1)
attr(dt, "cpi_data_level")  # "national"                           chr(1)
attr(dt, "ppp_versions")    # c("2017_01_01", "2021_01_01")        chr vector
attr(dt, "cpi_years")       # c("2017", "2021")                    chr vector
```

**There is no `list(values=...)` wrapping on any path.** Data-level metadata
(`ppp_data_level`, `cpi_data_level`, `pop_data_level`) is **always stored as
attributes only**. These values are never materialised as columns in the
survey data.table — `add_ppp()`, `add_cpi()`, and `add_rep_lvl()` read them
directly from `attr(dt, ...)`.  The `restore_data_level_cols()` helper that
previously materialised them as columns has been removed.

## Correct Pattern

Use `attr()` directly — no unwrapping needed:

```r
survey_id <- attr(dt, "survey_id")        # always a plain character scalar
ppp_level <- attr(dt, "ppp_data_level")   # plain scalar; may also be a column
```

Any code using `is.list()` guards, `$values` accessors, or a `get_attr_val()`
helper is incorrect and should be removed.

## What to Watch For

Data-level attributes are scalars. `add_ppp()` and `add_cpi()` use them to
look up the correct value from a named vector:

```r
ppp_lvl <- attr(dt, "ppp_data_level")   # e.g. "national"
dt[, (v) := lev_map[ppp_lvl]]           # scalar lookup, not per-row column
```

This means **a single survey object must have one `ppp_data_level`** (surveys
with multiple reporting levels are handled upstream by splitting before
deflation, not by mixing levels within a single `dt`).

## Related

- `.cg-docs/solutions/bugs/2026-05-05-data-level-columns-stripped-on-stamp-round-trip.md` — the real stamp issue: columns stripped, not attrs wrapped
- `.cg-docs/solutions/testing-patterns/2026-05-06-stamp-vs-pipeline-path-test-fixtures.md` — updated fixture docs
