# Build the inventory verification section

Renders the cross-check between successfully cleaned surveys and the
master inventory, from the `inv_update_inf` log entry written by
[`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md).
Lists any surveys confirmed missing. Returns an empty character vector
when the entry is absent.

## Usage

``` r
build_inventory_additions(dt)
```

## Arguments

- dt:

  Parsed log `data.table` (output of
  [`parse_log_meta()`](https://pip-technical-team.github.io/pipdata/reference/parse_log_meta.md)).

## Value

Character vector of markdown lines.
