# List of validation result by survey ID and module type

List of validation result by survey ID and module type

## Usage

``` r
get_validation_list(e_type = c("error", "warning", "success"), m_type = NULL)
```

## Arguments

- e_type:

  Character. Validation result type, error/warning, defulat is `error`

- m_type:

  Character. Module type, GPWG/GROUP/BIN/HIST/OTHER

## Value

data in DT format

## Examples

``` r
if (FALSE) { # \dontrun{
get_validation_list(report_data,
  e_type = "warning",
  m_type = "GPWG"
)
} # }
```
