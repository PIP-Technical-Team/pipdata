# Recoding gender variable

**Deprecated.** Superseded by
[`apply_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/apply_recode_spec.md),
which applies the declarative YAML recode specification
(`inst/extdata/recode_spec.yml`). Kept only for backward compatibility;
calling this function now emits a deprecation warning via
[`base::.Deprecated()`](https://rdrr.io/r/base/Deprecated.html).

## Usage

``` r
recode_gndr(dt)
```

## Value

data.table
