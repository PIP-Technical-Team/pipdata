# Build the latest top-level pipeline run summary

Build the latest top-level pipeline run summary

## Usage

``` r
build_pipeline_run_summary(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines, or an empty vector.
