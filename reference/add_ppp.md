# Merge survey with PPP

Accepts either a named numeric vector (format produced by
[`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md))
or a wide `data.table` (legacy format from
[`ppp_to_wide()`](https://pip-technical-team.github.io/pipdata/reference/ppp_to_wide.md)).

## Usage

``` r
add_ppp(dt, ppp)
```

## Arguments

- dt:

  A cleaned survey `data.table` with a `ppp_data_level` attribute.

- ppp:

  Named numeric vector or wide `data.table`.

## Value

`dt` augmented with one column per PPP version and a `ppp_versions`
attribute listing the version names.

## Details

Named vector names follow the pattern
`ppp_{ppp_year}_{release_version}_{adaptation_version}_{reporting_level}`,
e.g. `"ppp_2017_01_01_national"`. Each unique version becomes a column
in `dt` with the matching value looked up via `ppp_data_level`.
