# Deflation of welfare for micro data

Deflation of welfare for micro data

## Usage

``` r
# S3 method for class 'pipmd'
deflation(dt, cpi, ppp, pop, ...)
```

## Arguments

- dt:

  data.table of cleaned DLW survey from `wbpip_clean`

- cpi:

  Named numeric vector of CPI values (as returned by
  [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)),
  or a `data.table` from `pipload::pip_load_aux("cpi")` for the legacy
  interface. `NULL` triggers inventory-based loading.

- ppp:

  Named numeric vector of PPP values (as returned by
  [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)),
  or a `data.table` from `pipload::pip_load_aux("ppp")` for the legacy
  interface. `NULL` triggers inventory-based loading.

- pop:

  Named numeric vector of population values (as returned by
  [`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)),
  or a `data.table` from `pipload::pip_load_aux("pop")` for the legacy
  interface. `NULL` triggers inventory-based loading.

- ...:

  extra arguments

## Value

data.table
