# Build the DLW validation summary section

Workflow phase markers are reported separately from per-survey failures
so inventory and report persistence are not counted as survey
validations.

## Usage

``` r
build_dlw_validation_summary(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines, or an empty vector.
