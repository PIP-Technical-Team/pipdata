# Build the DLW acquisition summary section

Successful acquisitions are inferred from the start-entry denominator
minus per-survey download failures. Phase markers are not outcomes.

## Usage

``` r
build_dlw_acquisition_summary(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines, or an empty vector.
