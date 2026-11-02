# Find unique values in PFW according to some key variables

Find unique values in PFW according to some key variables

## Usage

``` r
unq_obs_dt(dt, keyVar)
```

## Arguments

- dt:

  data.table or data.frame

- keyVar:

  character vector with variables to determine unique observations

## Value

data.table or data.frame

## Examples

``` r
pfw <- data.table::data.table(
  country_code = c("COL", "PER"),
  survey_year = 2020,
  survey_acronym = c("GEIH", "ENAHO"),
  welfare_type = "income"
)
keyVar <- c("country_code", "survey_year", "survey_acronym", "welfare_type")
unq_obs_dt(pfw, keyVar)
#>    country_code survey_year survey_acronym welfare_type
#>          <char>       <num>         <char>       <char>
#> 1:          COL        2020           GEIH       income
#> 2:          PER        2020          ENAHO       income
```
