# Recode urban to area for micro data

**Deprecated.** Superseded by
[`apply_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/apply_recode_spec.md),
which recodes `urban` -\> `area` (and normalises `subnatid` via
[`shift_subnatid()`](https://pip-technical-team.github.io/pipdata/reference/shift_subnatid.md))
from the YAML recode specification (`inst/extdata/recode_spec.yml`).
Kept only for backward compatibility; calling this method now emits a
deprecation warning via
[`base::.Deprecated()`](https://rdrr.io/r/base/Deprecated.html).

## Usage

``` r
# S3 method for class 'pipmd'
add_area(dt)
```

## Arguments

- dt:

  data.table

## Value

data.table
