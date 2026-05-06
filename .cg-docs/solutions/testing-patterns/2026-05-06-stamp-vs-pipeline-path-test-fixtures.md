---
date: 2026-05-06
updated: 2026-05-06
title: "Dual fixture pattern: stamp-path vs pipeline-path for pipmd/pipgd objects"
category: "testing-patterns"
language: "R"
tags: [fixtures, stamp, round-trip, pipmd, pipgd, make_pipmd_stamp, make_pipmd, test-fixtures, attributes, data-level]
root-cause: "A single make_pipmd() fixture that stores data_level only as columns never exercises the stamp round-trip code path (columns absent), leaving restore_data_level_cols() untested."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

# Dual fixture pattern: stamp-path vs pipeline-path for `pipmd`/`pipgd` objects

## Problem

`make_pipmd()` stores `ppp_data_level`/`cpi_data_level` only as table columns.
Code like `restore_data_level_cols()` that checks `if (!col %in% names(dt))`
is always a no-op against this fixture; the stamp round-trip path (columns
absent, attrs only) is never tested. Bugs that manifest only on stamp-loaded
objects are invisible in the test suite.

> **Note**: An earlier version of this document claimed `make_pipmd()` stored
> attrs as `list(values = X)`. That was a fixture bug — since corrected. All
> survey attributes are always plain scalars. See the retracted bug doc for full
> details.

## Solution

Maintain **two complementary fixture helpers**:

### `make_pipmd()` — pipeline-path representation

- Level info stored as **columns** in the data.table.
- Attrs stored as **plain scalars** (same as stamp-path; no `list(values=X)`).
- Use for: testing pipeline-path logic, input validation, named-vector path helpers.

```r
make_pipmd <- function(..., welfare_type = "income", module = "D1") {
  dt <- data.table::data.table(
    welfare = ..., weight = ...,
    ppp_data_level = "national",   # ← present as column
    cpi_data_level = "national"
  )
  data.table::setattr(dt, "class", c("pipmd", "data.table", "data.frame"))
  data.table::setattr(dt, "ppp_data_level", "national")   # ← plain scalar attr
  data.table::setattr(dt, "welfare_type", welfare_type)
  data.table::setattr(dt, "module", module)
  ...
}
```

### `make_pipmd_stamp()` — stamp round-trip representation

- **No** level columns in the data.table (stripped by `vars_to_attr()`).
- Attrs stored as **plain scalar** strings (as returned by `pip_read()`).
- Use for: testing `restore_data_level_cols()`, Mode B `pd_deflation()`,
  and any code that must handle stamp-loaded objects.

```r
make_pipmd_stamp <- function(..., welfare_type = "income", module = "D1") {
  dt <- data.table::data.table(
    welfare = ..., weight = ...
    # no ppp_data_level / cpi_data_level columns
  )
  data.table::setattr(dt, "class", c("pipmd", "data.table", "data.frame"))
  data.table::setattr(dt, "ppp_data_level", "national")   # ← plain scalar
  data.table::setattr(dt, "cpi_data_level", "national")
  data.table::setattr(dt, "welfare_type", welfare_type)
  data.table::setattr(dt, "module", module)
  ...
}
```

**The two fixtures are identical in attr structure** — the only meaningful
difference is whether `ppp_data_level`/`cpi_data_level`/`pop_data_level`
exist as columns.

## Usage Guidelines

| Scenario | Use |
|----------|-----|
| Testing input validation (`validate_deflation_input`) | `make_pipmd()` |
| Testing `add_ppp()`, `add_cpi()` named-vector path | `make_pipmd()` |
| Testing `restore_data_level_cols()` | `make_pipmd_stamp()` |
| Testing `pd_deflation()` Mode B (loaded via `pip_id`) | `make_pipmd_stamp()` |
| Testing `pd_deflation()` Mode A (dt passed directly, pipeline context) | `make_pipmd()` |

## Required attrs for `pip_id` construction

Both fixtures must include `welfare_type` and `module` attributes (in addition
to `country_code`, `surveyid_year`, `survey_acronym`) because `pd_deflation()`
builds `pip_id` via the same logic as `cache_id()`:

```r
# pip_id = "{country_code}_{surveyid_year}_{survey_acronym}_{INC|CON}_{module}"
wt_map <- c(income = "INC", consumption = "CON")
pip_id <- paste(attr(dt, "country_code"), attr(dt, "surveyid_year"),
                attr(dt, "survey_acronym"), wt_map[attr(dt, "welfare_type")],
                attr(dt, "module"), sep = "_")
```

Missing `welfare_type` or `module` causes `pd_deflation()` to abort with
"Cannot construct pip_id: missing attributes".

## Related

- `.cg-docs/solutions/bugs/2026-05-06-attribute-list-values-wrapper-pipeline-vs-stamp-path.md` — retracted: `list(values=X)` never existed
- `.cg-docs/solutions/bugs/2026-05-05-data-level-columns-stripped-on-stamp-round-trip.md`
- `tests/testthat/test-pd-deflation.R` — canonical example of both fixtures in use
