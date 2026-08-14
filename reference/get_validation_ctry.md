# List of validation result by country and module type

List of validation result by country and module type

## Usage

``` r
get_validation_ctry(
  e_type = c("error", "warning", "success"),
  ctry = NULL,
  by_year = FALSE,
  r_year = NULL
)
```

## Arguments

- e_type:

  Character. Validation result type, error/warning, defulat is `error`

- ctry:

  Character. Country 3-digits ISO code, defualt is `NULL`

- by_year:

  Logical. Defualt is `FALSE`. If `TRUE`, generates wide table by module
  types

- r_year:

  Character. Ref years, defualt is `NULL`. If value(s) is provided,
  generates wide table for specified ref years by module types. Note: If
  r_year has value(s), `by_year` argument should be `FALSE`

## Value

data in DT format

## Examples

``` r
if (FALSE) { # \dontrun{
get_validation_ctry(report_data,
  e_type = "warning",
  ctry = c("ARG", "CHL", "HRV"),
  by_year = TRUE
)
} # }
```
