---
date: 2026-04-03
title: "welfare_type NA from inventory causes load_pip_data() to find 0 files"
category: "bugs"
type: "bug"
language: "R"
tags: [arrow, parquet, welfare_type, pip_id, inventory, generate_arrow_dataset, pipload, data.table]
root-cause: "generate_arrow_dataset() selected welfare_type from the inventory in a data.table .() expression; the column does not exist in the inventory, so data.table silently returned NA, which was passed to load_pip_data() causing it to match 0 files."
severity: "P1"
test-written: "yes"
fix-confirmed: "yes"
---

# welfare_type NA from inventory causes load_pip_data() to find 0 files

## Symptom

`generate_arrow_dataset(inv, overwrite = TRUE)` wrote only 1 of 2 surveys when
passed a 2-row inventory (`inv2`). The second survey (ARG) produced a silent
error row in the results with message:

> "Wrong number of data to load. It should be only 1. You attempt to load 0"

The function did not abort — the error was caught by the `tryCatch` handler and
recorded as `status = "error"` in the output. The first survey (BOL) succeeded
because it had already been written and the path was resolved correctly in an
earlier debug session. In a cold run, both surveys would fail.

## Root Cause

`generate_arrow_dataset()` resolved `pip_rows` from the inventory with a
`data.table` `.()` column selection that included `welfare_type`:

```r
pip_rows <- inventory[
  !is.na(pip_id) & survey_id %in% survey_ids,
  .(survey_id, pip_id, country_code, surveyid_year,
    survey_acronym, vermast, veralt, collection, module,
    welfare_type)   # ← this column does not exist in the inventory
]
```

The release inventory (`pipload::load_pip_release_inventory()`) does **not**
contain a `welfare_type` column. When a non-existent column is named inside
`data.table`'s `.()` selection, `data.table` silently returns `NA` for every
row — no error, no warning.

The resulting `pip_rows$welfare_type` was `NA` for every survey. This `NA` was
then passed directly to `pipload::load_pip_data(welfare_type = NA)`, which
found 0 matching `.qs2` files and threw the "attempt to load 0" error.

The welfare type **is** reliably available — it is structurally encoded in
`pip_id` as the second-to-last underscore-delimited token:
`COUNTRY_YEAR_ACRONYM_(INC|CON)_(ALL|GPWG)`.

This is a **P1** because it caused incorrect (silent) results: the batch
function appeared to succeed (returned a results table) but only wrote a
subset of the requested surveys, with no indication to the caller that the
remaining surveys were silently skipped due to `NA` welfare type.

## Reproduction Test

Added to `pipdata/tests/testthat/test-arrow-generation.R`
(section `# welfare_type derivation from pip_id`):

```r
test_that(".extract_welfare_from_pip_id correctly parses INC and CON pip_ids", {
  expect_identical(pipdata:::.extract_welfare_from_pip_id("ARG_2003_EPHC-S2_INC_ALL"), "INC")
  expect_identical(pipdata:::.extract_welfare_from_pip_id("BOL_2020_EH_INC_ALL"),       "INC")
  expect_identical(pipdata:::.extract_welfare_from_pip_id("IDN_1990_SUSENAS_CON_GROUP"), "CON")
  expect_identical(pipdata:::.extract_welfare_from_pip_id("COL_2010_ECH_INC_ALL"),       "INC")
})

test_that("generate_arrow_dataset pip_rows welfare_type is never NA when inventory lacks the column", {
  inv_no_wt <- data.table::data.table(
    survey_id      = c("ARG_2003_EPHC-S2_V01_M_V09_A_GMD_ALL", "BOL_2020_EH_V01_M_V04_A_GMD_ALL"),
    pip_id         = c("ARG_2003_EPHC-S2_INC_ALL", "BOL_2020_EH_INC_ALL"),
    country_code   = c("ARG", "BOL"),
    surveyid_year  = c(2003L, 2020L),
    survey_acronym = c("EPHC-S2", "EH"),
    vermast        = c("v01", "v01"),
    veralt         = c("v09", "v04"),
    collection     = c("GMD", "GMD"),
    module         = c("ALL", "ALL")
    # NOTE: no welfare_type column — matches real inventory structure
  )
  pip_rows <- inv_no_wt[
    !is.na(pip_id),
    .(survey_id, pip_id, country_code, surveyid_year,
      survey_acronym, vermast, veralt, collection, module)
  ]
  pip_rows[, welfare_type := vapply(pip_id, pipdata:::.extract_welfare_from_pip_id,
                                    character(1L))]

  expect_false(any(is.na(pip_rows$welfare_type)))
  expect_identical(pip_rows$welfare_type, c("INC", "INC"))
})
```

Both tests failed before the fix and pass after.

## Fix

**`pipdata/R/arrow_generation.R`**

1. **Added internal helper `.extract_welfare_from_pip_id(pip_id)`** — extracts
   the welfare token (second-to-last `_`-delimited segment) from a `pip_id`
   string. Errors explicitly if `pip_id` has fewer than 2 segments.

   ```r
   .extract_welfare_from_pip_id <- function(pip_id) {
     parts <- strsplit(pip_id, "_", fixed = TRUE)[[1L]]
     if (length(parts) < 2L) {
       cli::cli_abort(
         "Cannot extract welfare type from pip_id {.val {pip_id}}: too few segments."
       )
     }
     parts[[length(parts) - 1L]]
   }
   ```

2. **Fixed `generate_arrow_dataset()` `pip_rows` resolution** — removed
   `welfare_type` from the `.()` column selection (where it was silently `NA`)
   and derived it from `pip_id` using the new helper immediately after the join:

   ```r
   pip_rows <- inventory[
     !is.na(pip_id) & survey_id %in% survey_ids,
     .(survey_id, pip_id, country_code, surveyid_year,
       survey_acronym, vermast, veralt, collection, module)
     # welfare_type intentionally omitted — not in inventory
   ]
   pip_rows[, welfare_type := vapply(pip_id, .extract_welfare_from_pip_id,
                                     character(1L))]
   ```

3. **Added explanatory comment** at the derivation site warning future readers
   never to select `welfare_type` from the inventory.

## Lessons Learned

**`data.table` silently returns `NA` for non-existent columns in `.()` selections.**
This is a well-known `data.table` behaviour but easy to miss, especially when
the column was expected to exist and the resulting `NA` is consumed downstream
without type-checking.

The pattern to avoid:

```r
# DANGEROUS: if welfare_type is not in inventory, every row is silently NA
dt[condition, .(col1, col2, welfare_type)]
```

Safe alternatives:

```r
# Option A: derive from a column that IS reliably present
dt[condition, .(col1, col2)]
dt[, welfare_type := vapply(pip_id, .extract_welfare_from_pip_id, character(1L))]

# Option B: guard explicitly before selecting
stopifnot("welfare_type" %in% names(inventory))
dt[condition, .(col1, col2, welfare_type)]
```

More broadly: **any identifier that encodes structured information (like
`pip_id`) should have an explicit extractor function** rather than relying on
a parallel column that may or may not be present. The `pip_id` column is
always present; a `welfare_type` column derived from it in a join is fragile.

## Related

- `.cg-docs/solutions/bugs/2026-04-03-pip-id-survey-id-column-naming.md` —
  previous bug in the same file; also involved a column naming assumption about
  what fields are present in the inventory/data.table.
- `.cg-docs/solutions/data-quality/2026-04-03-data-table-silent-na-column-selection.md` —
  team-wide pattern document capturing the `data.table` silent-NA anti-pattern
  and the "derive from a reliable column" prevention rule.
