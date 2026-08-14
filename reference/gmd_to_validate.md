# Get datasets list that needs to be validated

This function filters and returns the subset of new GMD records that
match the validated inventory.

## Usage

``` r
gmd_to_validate(gmd_new, inv_validated)
```

## Arguments

- gmd_new:

  A data.table containing the new GMD records. Must include columns
  `FileName` and `Checksum`.

- inv_validated:

  A data.table of validated inventory records with `survey_id` and
  `Checksum` columns.

## Value

A data.table containing only GMD records that match the validated
inventory. Returns all of `gmd_new` if `inv_validated` is NULL or empty.
Result is returned invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
validated_records <- gmd_to_validate(gmd_new, inv_validated)
} # }
```
