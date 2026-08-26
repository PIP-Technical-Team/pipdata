# Match available local GMD rows to the current server catalog

Reconciles the default local acquisition inventory in memory against the
authoritative current seven-module server catalog, then returns rows
whose durable status is `data_available = "Yes"`. Catalog-deleted and
superseded rows are excluded; current successful `"ASPIRE"` and `"L"`
rows are retained for compatibility even though active acquisition
downloads only five modules. This function does not write the reconciled
inventory.

## Usage

``` r
dlw_gmd_match()
```

## Value

A `data.table` of current matched available datasets, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
pipfun::setup_working_release("20260206", "TEST")
df <- dlw_gmd_match()
head(df)
} # }
```
