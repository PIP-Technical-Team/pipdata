# Return Validated GMD Records

This function filters the GMD dataset to return only the records that
match entries in the validated inventory.

## Usage

``` r
gmd_validated(gmd_new, inv_validated)
```

## Arguments

- gmd_new:

  A data.table containing new GMD records. Must include columns
  `FileName` and `Checksum`.

- inv_validated:

  A data.table of validated inventory records with `survey_id` and
  `Checksum` columns.

## Value

A data.table with only validated GMD records that exist in both
`gmd_new` and `inv_validated`. Returns `NULL` if `inv_validated` is NULL
or empty. Result is returned invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
validated_gmd <- gmd_validated(gmd_new, inv_validated)
} # }
```
