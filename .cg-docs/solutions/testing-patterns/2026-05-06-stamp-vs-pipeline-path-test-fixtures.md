---
date: 2026-05-06
updated: 2026-05-06
title: "Dual fixture pattern: stamp-path vs pipeline-path for pipmd/pipgd objects"
category: "testing-patterns"
language: "R"
tags: [fixtures, stamp, round-trip, pipmd, pipgd, make_pipmd_stamp, make_pipmd, test-fixtures, attributes, data-level]
root-cause: "A single make_pipmd() fixture must not include data_level columns. Those are attrs-only by design; tests that add them as columns were testing the wrong invariant."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

# Dual fixture pattern: stamp-path vs pipeline-path for `pipmd`/`pipgd` objects

## Canonical Design

`ppp_data_level`, `cpi_data_level`, and `pop_data_level` are **attributes
only** — they are never columns in a `pipmd`/`pipgd` data.table. `add_ppp()`,
`add_cpi()`, and `add_rep_lvl()` read them directly from `attr(dt, ...)`.

There is therefore **only one fixture**, `make_pipmd()`, which reflects this:

```r
make_pipmd <- function(..., ppp_data_level = "national", ...) {
  dt <- data.table::data.table(
    welfare = ..., weight = ...
    # no ppp_data_level / cpi_data_level columns
  )
  data.table::setattr(dt, "ppp_data_level", ppp_data_level)  # attr only
  data.table::setattr(dt, "cpi_data_level", cpi_data_level)
  ...
}
```

`make_pipmd_stamp()` has been removed — it is identical to `make_pipmd()`
because there is no "pipeline path with columns" variant.

## Usage Guidelines

| Scenario | Use |
|----------|-----|
| All deflation tests | `make_pipmd()` |
| Testing `add_ppp()`, `add_cpi()`, `add_rep_lvl()` | `make_pipmd()` |
| Testing `pd_deflation()` Mode A or B | `make_pipmd()` |

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
