---
date: 2026-04-03
title: "pip_id/survey_id column naming inconsistency in arrow_generation.R"
category: "bugs"
type: "bug"
language: "R"
tags: [arrow, parquet, pip_id, survey_id, schema, column-naming, pipdata]
root-cause: "The data column holding the file-level survey identifier was renamed from survey_id to pip_id in the schema, but the test fixture, error handler, and documentation strings in arrow_generation.R were not updated to match."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

# pip_id/survey_id column naming inconsistency in arrow_generation.R

## Symptom

Four distinct inconsistencies caused by a stale `survey_id` naming artifact:

1. **Test fixture `make_arrow_dt()` built a `survey_id` column** — not a schema column.
   This caused all tests calling `write_survey_parquet()` to fail at `.validate_for_write()`
   because `pip_id` was missing (required) and `survey_id` was an extra disallowed column.

2. **`generate_arrow_dataset()` error handler returned `survey_id` as the first column**,
   while the success path (from `write_survey_parquet()`) returns `pip_id`. After
   `rbindlist(fill = TRUE)`, error rows had `pip_id = NA` and a stray `survey_id` column;
   success rows had no `survey_id`. The combined batch-results table was structurally broken
   whenever any individual survey failed.

3. **`.build_parquet_filename()` roxygen** used `@param survey_id` and described the
   function as deriving a filename "from a `survey_id`", but the function was always
   called with a `pip_id` value.

4. **`write_survey_parquet()` partition structure comment** showed `<survey_id>-0.parquet`
   as the filename pattern, and the file-level header described `pip_id` as "the value
   stored in the data column `survey_id`".

## Root Cause

The data column holding the file-level survey identifier was renamed from `survey_id`
to `pip_id` when the schema (`piptm::pip_arrow_schema()`) was updated. The schema
defines `pip_id` as a required `utf8` column, `survey_id` is not a schema column at
all. The rename was applied to `arrow_prep.R` (`inject_metadata_cols()`) and to
`schema.R`, but was not propagated to:

- The `make_arrow_dt()` test fixture in `test-arrow-generation.R`
- The error-fallback `data.table` inside `generate_arrow_dataset()`
- The roxygen for `.build_parquet_filename()`
- The partition structure comment in `write_survey_parquet()` and the file header

## Reproduction Test

Added to `pipdata/tests/testthat/test-arrow-generation.R` (section
`# pip_id vs survey_id consistency`):

```r
test_that("make_arrow_dt helper uses pip_id column, not survey_id", {
  dt <- make_arrow_dt()
  expect_true("pip_id"    %in% names(dt))
  expect_false("survey_id" %in% names(dt))
})

test_that("write_survey_parquet returns a result with pip_id column, not survey_id", {
  tmp    <- withr::local_tempdir()
  dt     <- make_arrow_dt()
  result <- write_survey_parquet(dt, arrow_repo_path = tmp)
  expect_true("pip_id"    %in% names(result))
  expect_false("survey_id" %in% names(result))
})

test_that(".validate_for_write rejects data with survey_id column instead of pip_id", {
  dt <- make_arrow_dt()
  data.table::setnames(dt, "pip_id", "survey_id")
  expect_error(pipdata:::.validate_for_write(dt), regexp = "pip_id")
})
```

All three tests failed before the fix and pass after.

## Fix

**`pipdata/tests/testthat/test-arrow-generation.R`**

- Renamed the `survey_id` parameter of `make_arrow_dt()` to `pip_id`.
- Changed the column created inside the helper from `survey_id` to `pip_id`.
- Updated the two callers that passed `survey_id = ...` to use `pip_id = ...`.

**`pipdata/R/arrow_generation.R`**

1. **File header comment**: changed "the value stored in the data column `survey_id`"
   → "the value stored in the data column `pip_id`". Removed the parenthetical that
   described the old rename.

2. **`.build_parquet_filename()` roxygen**: renamed title to "Derive the Parquet filename
   from a `pip_id`", renamed `@param survey_id` → `@param pip_id`, updated the parameter
   name in the function signature.

3. **`write_survey_parquet()` partition structure comment**: changed
   `<survey_id>-0.parquet` → `<pip_id>-0.parquet`.

4. **`generate_arrow_dataset()` error handler**: changed `survey_id = survey_id_i`
   → `pip_id = pip_id_i` so the error-row schema matches the success-row schema from
   `write_survey_parquet()`.

5. **`generate_arrow_dataset()` `@return` docs**: updated to list `pip_id` as the
   first column instead of `survey_id`.

## Lessons Learned

**When renaming a data column that is part of a schema contract, update all four
touch-points together:**

1. The schema definition (source of truth)
2. The write path that injects/creates the column
3. The test fixture(s) that build synthetic data conforming to the schema
4. Any error-path or fallback `data.table` that mirrors the success-path structure

The most dangerous consequence here was #3 (the error handler): the `rbindlist(fill = TRUE)`
silently accepted mismatched column names, producing `pip_id = NA` on all error rows
with no runtime warning. Schema-mismatch bugs in error handlers are easy to miss in
testing because tests typically only exercise the happy path.

**Anti-pattern**: building error-fallback rows with hardcoded column names rather than
deriving the structure from a shared skeleton. A safer pattern is to define the skeleton
once and fill it for all branches:

```r
# Preferred: define skeleton once
error_row <- data.table::data.table(
  pip_id               = pip_id_i,
  country_code         = row_i$country_code,
  surveyid_year        = row_i$surveyid_year,
  welfare_type         = row_i$welfare_type,
  file_path            = NA_character_,
  n_rows               = NA_integer_,
  available_dimensions = NA_character_,
  status               = "error",
  message              = NA_character_
)
```

## Related

None.

<!-- See also: .cg-docs/solutions/data-quality/2026-04-03-data-table-silent-na-column-selection.md
     for the broader team-wide pattern: data.table silently returns NA for
     non-existent columns, and the "error handler schema must mirror success
     handler schema" rule. -->
