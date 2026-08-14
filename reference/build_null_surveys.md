# Build the null-surveys section

Extracts the `null_svys_inf` entry (if present) which lists all surveys
that were not cleaned.

## Usage

``` r
build_null_surveys(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines.
