---
date: 2026-05-05
title: "survey_year vs surveyid_year attribute name mismatch between pipeline and stamp"
category: "bugs"
type: "bug"
language: "R"
tags: [deflation, attributes, survey-year, surveyid-year, stamp, round-trip, pd-deflation]
root-cause: "The pipeline path attaches survey year as the 'survey_year' attribute, but stamp stores and restores it as 'surveyid_year'. Validation and attribute-reading code that expects 'survey_year' fails on stamp-loaded surveys."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

# survey_year vs surveyid_year attribute name mismatch

## Symptom

`.validate_deflation_input()` aborted with:

```
Error: Input is missing required attributes: `survey_year`.
```

when called on a survey loaded from stamp via `pip_read()`, even though the
survey was a fully valid cleaned `pipmd` object.

Separately, the legacy `add_cpi()` path read `attributes(dt)$survey_year$value`
and got `NULL` for stamp-loaded surveys.

## Root Cause

Two incompatible attribute naming conventions exist in the codebase:

| Source | Attribute name | Structure |
|--------|---------------|-----------|
| Pipeline (`survey_id_to_attr`) | `survey_year` | `list(values = 2022L)` |
| Stamp round-trip (`pip_read`) | `surveyid_year` | plain scalar `2022L` |

The stamp convention mirrors the inventory column name (`surveyid_year`).
The pipeline convention pre-dates stamp integration. Tests that call
`make_pipmd()` used `survey_year`, masking the mismatch in CI.

## Fix

1. **Normalised on `surveyid_year`** as the canonical attribute name throughout
   `pd_deflation.R` — this is what stamp restores and what `pipload` attributes
   map to.

2. **Updated `.validate_deflation_input()`** to require `surveyid_year`.

3. **Updated `add_cpi()` legacy path** to read the year attribute defensively,
   handling both `list(values=X)` and plain scalar forms:

```r
get_attr_val <- function(dt, nm) {
  v <- attr(dt, nm)
  if (is.list(v)) v[["values"]] else v
}
svy_year <- get_attr_val(dt, "surveyid_year")
```

4. **Updated `make_pipmd()` test fixture** to set `surveyid_year` instead of
   `survey_year`.

## Lessons Learned

- **Attribute names are part of the data contract.** When stamp stores a
  `data.table`, scalar attributes use the column/inventory convention
  (`surveyid_year`). When the pipeline builds attributes inline, it used the
  human-readable name (`survey_year`). These must be reconciled at one
  canonical name — `surveyid_year` — since that is what survives round-trips.
- **Test fixtures must mirror real data sources.** `make_pipmd()` using
  `survey_year` hid this bug. Fixtures for stamp-loaded objects should be
  derived from an actual `pip_read()` call or explicitly document the
  attribute form they represent.
- Any function that reads year from `dt` attributes must handle both the
  `list(values=X)` form (pipeline) and plain scalar (stamp). Use
  `get_attr_val()` helper pattern above.

## Related

- `.cg-docs/solutions/bugs/2026-05-05-pip-class-stripped-on-stamp-round-trip.md`
- `.cg-docs/solutions/data-quality/2026-05-04-master-inventory-real-column-names.md`
