---
date: 2026-05-20
title: "Read welfare_type from inventory column, not from pip_id string parsing"
category: "data-quality"
language: "R"
tags: [generate-arrow-dataset, welfare-type, inventory, pip-id, arrow-generation]
root-cause: "generate_arrow_dataset() was deriving welfare_type by parsing the pip_id string instead of reading the column that both master and release inventories already carry"
severity: "P2"
---

# Read welfare_type from inventory column, not from pip_id string parsing

## Problem

`generate_arrow_dataset()` in `arrow_generation.R` was computing `welfare_type`
for each pip_id by splitting the pip_id string and extracting the
second-to-last segment:

```r
pip_rows[, welfare_type := vapply(pip_id, .extract_welfare_from_pip_id,
                                  character(1L))]
```

The code comment even stated:

```
# NOTE: the inventory does not carry a welfare_type column. It is derived
# from pip_id (second-to-last "_" segment, e.g. "..._INC_ALL" -> "INC")
```

This was always inefficient and fragile — string parsing of a structured ID
is brittle compared to reading a typed column. More importantly, the comment
was simply wrong: both `pipload::load_pip_master_inventory()` and
`pipload::load_pip_release_inventory()` carry a `welfare_type` column.

## Root Cause

The original implementation was written with a (false) assumption that the
inventory lacked `welfare_type`. Confirmed false:

```r
inv <- pipload::load_pip_master_inventory()
"welfare_type" %in% names(inv)   # TRUE

inv2 <- pipload::load_pip_release_inventory()
"welfare_type" %in% names(inv2)  # TRUE
```

## Solution

Three changes in `R/arrow_generation.R`:

1. **Guard**: abort loudly if a caller passes an inventory without
   `welfare_type` (future-proof, fail-fast):

```r
if (!"welfare_type" %in% names(inventory)) {
  cli::cli_abort(
    c(
      "{.arg inventory} is missing required column {.field welfare_type}.",
      "i" = "Both {.fn pipload::load_pip_master_inventory} and
             {.fn pipload::load_pip_release_inventory} provide this column."
    )
  )
}
```

2. **Column selection**: include `welfare_type` in the `.SD` projection
   alongside the other inventory fields — no derivation needed:

```r
pip_rows <- inventory[
  !is.na(pip_id) & survey_id %in% survey_ids,
  .(survey_id, pip_id, country_code, surveyid_year,
    welfare_type, survey_acronym, vermast, veralt, collection, module)
]
```

3. **`.extract_welfare_from_pip_id()` doc**: demoted from "canonical way to
   derive welfare_type" to a retained utility for ad-hoc use when only a
   pip_id string is available.

Three tests updated / added in `tests/testthat/test-arrow-generation.R`:

- `"generate_arrow_dataset reads welfare_type directly from inventory column"` — verifies `INC`/`CON` values pass through correctly
- `"generate_arrow_dataset aborts when inventory lacks welfare_type column"` — verifies the new guard fires
- Mocked `generate_arrow_dataset` fixture updated to include `welfare_type = "INC"`

## Prevention

- **Never parse a structured ID string to recover a value that is already
  present as a typed column in the inventory.** String parsing is fragile
  and obscures intent.
- When writing batch functions that accept an inventory, declare required
  columns explicitly at the top of the function and fail loudly if any are
  missing — the same pattern used in `generate_release_manifest()` via
  `required_inv_cols`.
- Verify inventory column availability in the R session before writing
  "inventory does not carry X" comments:
  ```r
  "welfare_type" %in% names(pipload::load_pip_master_inventory())
  ```

## Related

- `R/arrow_generation.R` — `generate_arrow_dataset()`, `.extract_welfare_from_pip_id()`
- `R/manifest_generation.R` — `generate_release_manifest()`: same pattern
  of declaring `required_inv_cols` and failing loudly
- `.cg-docs/solutions/data-quality/2026-05-20-reporting-level-now-in-release-inventory.md`
  — parallel fix: another column assumed absent that was already present
- `.cg-docs/solutions/data-quality/2026-05-04-master-inventory-real-column-names.md`
  — original inventory column audit
