# Build the auxiliary file changes section

Renders changed measures and affected survey counts from the
`aux_changes_inf` log entry written by
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md).
Returns an empty character vector when the entry is absent (no aux
changes).

## Usage

``` r
build_aux_changes(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines.
