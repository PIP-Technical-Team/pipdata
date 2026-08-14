# Normalise subnatid column hierarchy

Shifts existing `subnatidN` columns up by one (`subnatid1` →
`subnatid2`, etc.) then renames `subnatid` → `subnatid1`. No-op if no
plain `subnatid` column exists. Called explicitly in
[`dlw_clean.pipmd()`](https://pip-technical-team.github.io/pipdata/reference/dlw_clean.pipmd.md)
before
[`apply_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/apply_recode_spec.md)
— structural renames that are not variable-level recodes live here, not
in the YAML spec.

## Usage

``` r
shift_subnatid(dt)
```

## Arguments

- dt:

  data.table

## Value

`dt` modified by reference via `setnames()`
