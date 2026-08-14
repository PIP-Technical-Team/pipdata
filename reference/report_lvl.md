# Add reporting level variable to country PFW

Computes `reporting_level` as the per-row maximum across the five domain
columns (CPI, PPP, GDP, PCE, pop). Values are stored as character: `"1"`
= national (all domains are national); `"2"` = subnational (at least one
domain, e.g. `cpi_domain`, is 2 meaning urban/rural-specific data are
available). This value is later read by
[`add_main_att()`](https://pip-technical-team.github.io/pipdata/reference/add_main_att.md)
as the integer `reporting_level` attribute on the survey `data.table`.

## Usage

``` r
report_lvl(cpfw)
```

## Arguments

- cpfw:

  data.table with country Price Framework containing the five `*_domain`
  columns (see `.DOMAIN_COLS`) and an `inpovcal` indicator.

## Value

`cpfw` with a new `reporting_level` character column (`"1"` or `"2"`)
added by reference. Rows with `inpovcal != 1` are dropped.
