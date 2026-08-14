# Identify base years for deflation

Compares available CPI and PPP years (from `dt` attributes set by
[`add_cpi()`](https://pip-technical-team.github.io/pipdata/reference/add_cpi.md)
and
[`add_ppp()`](https://pip-technical-team.github.io/pipdata/reference/add_ppp.md))
and sets a `base_years` attribute on `dt`. When `ppp` is a named numeric
vector the PPP versions are read from the `ppp_versions` attribute of
`dt` (set by
[`add_ppp()`](https://pip-technical-team.github.io/pipdata/reference/add_ppp.md));
when `ppp` is a `data.table` the versions come from its own
`ppp_versions` attribute.

## Usage

``` r
cpi_ppp_years(dt, ppp)
```

## Arguments

- dt:

  A `data.table` that has already been processed by
  [`add_cpi()`](https://pip-technical-team.github.io/pipdata/reference/add_cpi.md)
  and
  [`add_ppp()`](https://pip-technical-team.github.io/pipdata/reference/add_ppp.md).

- ppp:

  Named numeric vector or wide PPP `data.table` (used only to locate the
  `ppp_versions` attribute).

## Value

`dt` with a `base_years` attribute.
