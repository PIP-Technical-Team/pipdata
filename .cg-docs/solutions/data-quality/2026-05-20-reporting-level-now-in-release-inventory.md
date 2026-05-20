---
date: 2026-05-20
title: "reporting_level is present in the release inventory — remove placeholder"
category: "data-quality"
language: "R"
tags: [release-inventory, manifest-generation, reporting-level, placeholder, pipload]
root-cause: "manifest_generation.R used a hardcoded 'national' placeholder for reporting_level while awaiting the column in the release inventory; the column is now present"
severity: "P2"
---

# reporting_level is present in the release inventory — remove placeholder

## Problem

`generate_release_manifest()` in `manifest_generation.R` was writing
`reporting_level = "national"` for **every** survey entry in the manifest JSON,
regardless of the survey's actual reporting domain. Three TODO comments flagged
this as a placeholder pending an inventory column:

```r
# TODO: add "reporting_level" once inventory column is available
# TODO: replace placeholder with row_i$reporting_level once inventory column is available
# TODO: remove default once inventory column is available
```

This meant `{piptm}` would receive `"national"` for all surveys — including
subnational surveys with `"urban"` or `"rural"` domains — causing downstream
breakage in any loading logic that branches on reporting level.

## Root Cause

The `reporting_level` column was expected to be added to the release inventory
at a later pipeline stage. The column has since been added; the placeholders
were never removed.

Confirmed present via:

```r
inv <- pipload::load_pip_release_inventory()
"reporting_level" %in% names(inv)
# [1] TRUE
```

The inventory `data.table` (4 146 rows × 40 columns as of release `20260401_TEST`)
includes `reporting_level` as column 40.

## Solution

Four targeted changes to `R/manifest_generation.R`:

1. **`required_inv_cols`** — added `"reporting_level"` to the required columns
   vector. This makes the function fail loudly if the column is ever missing
   from a future inventory version.

2. **Manifest loop** — replaced the `"national"` literal with
   `row_i$reporting_level` (the actual per-survey value from the inventory row).

3. **`build_manifest_entry()` signature** — removed the TODO comment from the
   `reporting_level = "national"` default parameter. The default is kept as a
   safe fallback for direct callers that do not pass this argument.

4. **`@param reporting_level` documentation** — updated to state the value
   comes from `release_inventory$reporting_level`.

```r
# required_inv_cols (was missing "reporting_level")
required_inv_cols <- c(
  "survey_id", "pip_id", "country_code", "surveyid_year",
  "welfare_type", "survey_acronym", "vermast", "veralt", "module",
  "reporting_level"   # <-- added
)

# loop: was "national", now live from inventory
survey_entries[[i]] <- build_manifest_entry(
  ...
  reporting_level  = row_i$reporting_level,   # <-- was hardcoded "national"
  ...
)
```

## Prevention

- When adding inventory columns, immediately search the codebase for
  placeholder TODO comments referencing that column name and remove them.
- Including the column in `required_inv_cols` is the correct guard: the
  function will error at entry rather than silently emit wrong values.
- Never store a literal domain value (`"national"`) as a constant in a
  per-row loop — always source it from the data row.

## Related

- `R/manifest_generation.R` — `generate_release_manifest()`, `build_manifest_entry()`
- `.cg-docs/solutions/data-quality/2026-05-04-master-inventory-real-column-names.md`
  — similar pattern: inventory column names assumed rather than verified
- `docs/project-context.md` §Release Manifest — manifest schema reference
