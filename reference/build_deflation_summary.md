# Build the deflation summary section

Renders counts from the `deflate_summary_inf` log entry written by
[`pd_deflate_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflate_pipeline.md).
Returns an empty character vector when the entry is absent.

## Usage

``` r
build_deflation_summary(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines.
