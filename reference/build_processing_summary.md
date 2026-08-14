# Build the processing summary section

Renders counts from the `process_summary_inf` log entry written by
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md).
Returns an empty character vector when the entry is absent.

## Usage

``` r
build_processing_summary(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines.
