# Get Country Price framework data based on PFW and DLW data info

Get Country Price framework data based on PFW and DLW data info

## Usage

``` r
get_country_pfw(dt, pfw)
```

## Arguments

- dt:

  data frame with micro data, loaded with
  [`pipload::pip_load_dlw()`](https://pip-technical-team.github.io/pipload/reference/pip_load_dlw.html)

- pfw:

  data frame with Price framework data, loaded with
  `pipload::pip_load_aux("pfw")`

## Value

list of data.tables

## Examples

``` r
if (FALSE) { # \dontrun{
release <- "20250203"
pipfun::setup_working_release(release)

pfw <- pipload::pip_load_aux("pfw")
gd   <- pipload::pip_load_dlw("PHL", 2012)
gd  <- survey_id_to_attr(gd, unique(gd$survey_id))
cpfw <- get_country_pfw(gd, pfw)
} # }
```
