# Add auxiliary data for deflation

Add auxiliary data for deflation

## Usage

``` r
add_aux(dt, ppp, cpi)
```

## Arguments

- dt:

  A single cleaned survey `data.table` (class `pipmd` or `pipgd`), or
  `NULL` when `pip_id` is given instead.

- ppp:

  Named numeric vector of PPP values (as returned by
  [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)),
  or a `data.table` from `pipload::pip_load_aux("ppp")` for the legacy
  interface. `NULL` triggers inventory-based loading.

- cpi:

  Named numeric vector of CPI values (as returned by
  [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)),
  or a `data.table` from `pipload::pip_load_aux("cpi")` for the legacy
  interface. `NULL` triggers inventory-based loading.

## Value

data.table
