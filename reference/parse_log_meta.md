# Parse logmeta into a flat data.table

Extracts `error_type`, `survey`, and `country` from the nested `logmeta`
list-column of a `piplog` object.

## Usage

``` r
parse_log_meta(log)
```

## Arguments

- log:

  A `piplog` / `data.table`.

## Value

A `data.table` with columns from `log` plus `error_type`, `survey`, and
`country`.
