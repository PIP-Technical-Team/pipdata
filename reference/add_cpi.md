# Merge survey with CPI

Accepts either a named numeric vector (format produced by
[`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md))
or a `data.table` (legacy format from `pipload::pip_load_aux("cpi")`).

## Usage

``` r
add_cpi(dt, cpi)
```

## Arguments

- dt:

  A cleaned survey `data.table` with a `cpi_data_level` attribute.

- cpi:

  Named numeric vector or `data.table`.

## Value

`dt` augmented with one `cpiYYYY` column per base year and a `cpi_years`
attribute listing the year strings.

## Details

Named vector names follow the pattern `{cpi_year}_{reporting_level}`,
e.g. `"2017_national"`. Each unique year becomes a `cpiYYYY` column in
`dt` with the matching value looked up via `cpi_data_level`.
