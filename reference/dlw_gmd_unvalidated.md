# Get un-validated datasets list

Get un-validated datasets list

## Usage

``` r
dlw_gmd_unvalidated(check_missing = TRUE)
```

## Arguments

- check_missing:

  Logical. If TRUE, includes missing datasets from validation inventory
  list.

## Value

A data.table with new or unmatched local GMD datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- dlw_gmd_unvalidated()
head(df)
} # }
```
