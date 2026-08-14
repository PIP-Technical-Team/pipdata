# Reorder columns and rows in deflation output

Columns: `welfare_lcu`, `weight` first; then `welfare_ppp_*` (newest
base year first); then all remaining columns. Rows: sorted ascending by
the newest `welfare_ppp_*` column, then `weight`.

## Usage

``` r
finalize_deflation_output(dt)
```

## Arguments

- dt:

  Deflated `data.table` (after `char_to_fct()`).

## Value

`dt` with columns and rows reordered (mutates by reference). Sets
attribute `ppp_sort` (integer) to the base year of the `welfare_ppp_*`
column used for sorting (e.g. `2017L`), or `NULL` when no
`welfare_ppp_*` columns are present.
