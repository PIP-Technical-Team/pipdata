# Compare the local GMD inventory with the current server catalog

Candidate comparison recognizes all seven catalog/validation modules.
New current rows are returned, and current rows recorded as
`data_available = "No"` are also returned when `check_missing = TRUE`.
[`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md)
applies the narrower five-module download policy.

## Usage

``` r
dlw_gmd_new(check_missing = TRUE, update_inventory = FALSE)
```

## Arguments

- check_missing:

  Logical scalar. Include current unresolved inventory rows. Default
  `TRUE`.

- update_inventory:

  Logical scalar. Reconcile and write the default local inventory.
  Default `FALSE`.

## Value

A `data.table` with new or unresolved current GMD datasets.

## Details

When `update_inventory = TRUE`, the default inventory is reconciled to
the authoritative catalog: active five-module rows are retained,
obsolete rows are removed, and `"ASPIRE"`/`"L"` rows remain only when
already available. The direct utility write is reloaded after any
uncertain return and aborts unless intended durable content is verified.

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
pipfun::setup_working_release("20260206", "TEST")
df <- dlw_gmd_new()
head(df)
} # }
```
