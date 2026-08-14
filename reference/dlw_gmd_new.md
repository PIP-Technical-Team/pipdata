# Compare the local GMD dataset list with the server version to identify new entries.

Compare the local GMD dataset list with the server version to identify
new entries.

## Usage

``` r
dlw_gmd_new(check_missing = TRUE, update_inventory = FALSE)
```

## Arguments

- check_missing:

  Logical. If TRUE, includes missing datasets from either side.

- update_inventory:

  Logical. If TRUE, updates the local inventory with new entries.
  Default is FALSE.

## Value

A data.table with new or unmatched GMD datasets.

## Note

This function expects a working release to be configured via
[`pipfun::setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.html).
When called from
[`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md),
the release is already set. When called standalone, ensure
`setup_working_release()` has been invoked first.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- dlw_gmd_new()
head(df)
} # }
```
