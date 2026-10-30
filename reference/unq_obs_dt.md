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
release <- "20260401"
pipfun::setup_working_release(release)
#> Git credentials are missing or invalid in non-interactive mode.
#> Git credentials are missing or invalid in non-interactive mode.
#> Git credentials are missing or invalid in non-interactive mode.
#> ⠙ 10 items, page 1 | 2ms
#> Git credentials are missing or invalid in non-interactive mode.
#> ⠙ 0 items, page 1 | 1ms
#> Error in value[[3L]](cond): ✖ Error downloading file from github
#> ℹ check file ppp_vintage.csv exists
#> ✖ `password` must be a single string, not a character `NA`.

pfw <- pipload::load_aux_data("pfw")
#> Error in pipfun::get_wrk_release(verbose = FALSE): ✖ Working release has not been set up
#> ℹ You need to set a working release with `pipfun::setup_working_release()`
keyVar <- c("country_code", "survey_year", "survey_acronym", "welfare_type")
unq_obs_dt(pfw, keyVar)
#> Error: object 'pfw' not found
```
