# Build the skipped-surveys section

Reads `skipped_svys_data` and `skipped_svys_metadata` log entries
written by
[`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md)
and renders each group with its skip reasons. Returns an empty character
vector when no skipped-survey entries exist.

## Usage

``` r
build_skipped_surveys(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines.
