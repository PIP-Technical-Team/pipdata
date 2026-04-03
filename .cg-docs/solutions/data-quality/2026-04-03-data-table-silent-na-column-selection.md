---
date: 2026-04-03
title: "data.table silently returns NA for non-existent columns in .() selections"
category: "data-quality"
language: "R"
tags: [data.table, silent-NA, column-selection, inventory, schema, pip_id, welfare_type, arrow]
root-cause: "data.table .() column selection returns NA (not an error) when a named column does not exist in the source table — the NA propagates silently into downstream function calls."
severity: "P1"
---

# data.table silently returns NA for non-existent columns in .() selections

## Problem

`generate_arrow_dataset()` passed a 2-row inventory to `load_pip_data()`.
One survey loaded correctly; the other produced:

> "Wrong number of data to load. It should be only 1. You attempt to load 0"

The function returned a mixed results table with one `"written"` row and one
`"error"` row — appearing to succeed while silently dropping half the work.

Root symptom: `pip_rows$welfare_type` was `NA` for every row despite the
column appearing in the `.()` selection expression.

## Root Cause

`data.table`'s `.()` (alias for `list()`) column selection **silently returns
`NA` for any column name that does not exist in the source table**. No error
is thrown, no warning is emitted.

```r
dt <- data.table::data.table(a = 1:3, b = letters[1:3])

# Column 'c' does not exist — data.table returns NA silently:
dt[, .(a, b, c)]
#    a b     c
# 1: 1 a <NA>
# 2: 2 b <NA>
# 3: 3 c <NA>
```

In `generate_arrow_dataset()`, the release inventory from
`pipload::load_pip_release_inventory()` does **not** carry a `welfare_type`
column. The `.()` selection included `welfare_type` under the assumption it
would be present. Every resulting row had `welfare_type = NA`, which was
passed directly to `load_pip_data(welfare_type = NA)` — zero files matched.

This was compounded by the `tryCatch` error handler catching the downstream
error per-survey, so the batch loop continued and returned a plausible-looking
results table.

## Solution

**Never select a column from a `data.table` unless you have verified it
exists.** When a value is reliably derivable from another column that *is*
always present, compute it explicitly instead.

In this case, `welfare_type` is structurally encoded in `pip_id`
(`COUNTRY_YEAR_ACRONYM_(INC|CON)_(ALL|GPWG)`). The fix was:

1. **Add a dedicated extractor** that makes the derivation explicit and testable:

```r
#' @keywords internal
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

2. **Remove the non-existent column from the `.()` selection** and derive it
   immediately after the join:

```r
# BEFORE (broken — welfare_type not in inventory, returns NA silently):
pip_rows <- inventory[
  !is.na(pip_id) & survey_id %in% survey_ids,
  .(survey_id, pip_id, country_code, surveyid_year,
    survey_acronym, vermast, veralt, collection, module,
    welfare_type)   # ← NA for every row
]

# AFTER (correct — derive from pip_id which is always present):
pip_rows <- inventory[
  !is.na(pip_id) & survey_id %in% survey_ids,
  .(survey_id, pip_id, country_code, surveyid_year,
    survey_acronym, vermast, veralt, collection, module)
]
pip_rows[, welfare_type := vapply(pip_id, .extract_welfare_from_pip_id,
                                  character(1L))]
```

3. **Add a comment at the derivation site** warning future readers:

```r
# NOTE: the inventory does not carry a welfare_type column. It is derived
# from pip_id using .extract_welfare_from_pip_id(). Never select welfare_type
# from the inventory — data.table would silently return NA for a missing
# column, which would cause load_pip_data() to find 0 matching files.
```

## Prevention

### Rule: always guard before selecting, or derive from a reliable source

```r
# SAFE option A: guard explicitly
if (!"welfare_type" %in% names(inventory)) {
  cli::cli_abort("inventory must contain a {.field welfare_type} column.")
}
inventory[cond, .(welfare_type)]

# SAFE option B: derive from a column that IS always present
pip_rows[, welfare_type := vapply(pip_id, .extract_welfare_from_pip_id,
                                  character(1L))]
```

### Rule: encode structured identifiers with explicit extractor functions

When an identifier column (`pip_id`, `survey_id`, `file_path`, etc.) encodes
multiple pieces of information, create a named, documented, tested internal
helper to extract each piece:

```r
.extract_welfare_from_pip_id()   # pip_id → "INC" | "CON"
```

This makes the extraction:
- **Testable** in isolation
- **Visible** to future readers (self-documenting)
- **Centralised** — one place to fix if the format ever changes

### Rule: treat error handlers as part of the schema contract

The `tryCatch` error fallback in `generate_arrow_dataset()` masked the `NA`
propagation entirely — the batch appeared to succeed with an `"error"` row
rather than aborting. Always ask: *if the error handler fires, does the
caller know the full scope of what was lost?*

Consider emitting a `cli::cli_warn()` at the end of the batch loop if any
rows have `status == "error"`:

```r
n_errors <- sum(results$status == "error", na.rm = TRUE)
if (n_errors > 0L) {
  cli::cli_warn("{n_errors} survey(s) failed. Check the {.field message} column.")
}
```

## Related

- `.cg-docs/solutions/bugs/2026-04-03-welfare-type-na-from-inventory.md` —
  the specific bug report with full reproduction test and fix details.
- `.cg-docs/solutions/bugs/2026-04-03-pip-id-survey-id-column-naming.md` —
  companion bug from the same session: a schema column rename (`survey_id` →
  `pip_id`) was not propagated to the test fixture and error handler, causing
  `rbindlist(fill = TRUE)` to silently produce `NA` in the `pip_id` column of
  error rows. Same root pattern: a column assumed to exist that did not.
