# Get current available GMD datasets without completed validation

Reconciles the default completed validation inventory to current
available acquisition `survey_id`/`Checksum` keys and returns keys
absent from completed state. Completed state means
`data_available = "Yes"` with status `"valid"` or `"invalid"`;
recognized legacy blank/`"No"` retry rows are not completed.
Consequently execution failures are returned again because they have no
completed inventory row. All seven validation module mappings may be
present.

## Usage

``` r
dlw_gmd_unvalidated(check_missing = TRUE)
```

## Arguments

- check_missing:

  Logical scalar retained for API compatibility. It is validated, while
  retry selection is now determined by absence from the completed
  validation inventory.

## Value

Invisibly, a `data.table` of current available acquisition rows that
lack a completed validation row.

## Examples

``` r
if (FALSE) { # \dontrun{
pipfun::setup_working_release("20260206", "TEST")
df <- dlw_gmd_unvalidated()
head(df)
} # }
```
