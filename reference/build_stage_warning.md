# Build a stage-awareness warning for a parsed pipeline log

Build a stage-awareness warning for a parsed pipeline log

## Usage

``` r
build_stage_warning(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines, or an empty vector.
