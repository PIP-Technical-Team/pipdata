# Split data based on alternative welfare

Split data into two dataframes when there is alternative welfare in the
same survey

## Usage

``` r
pd_split_alt_welfare(dt, cpfw)
```

## Arguments

- dt:

  data table loaded with
  [`pipload::pip_load_dlw()`](https://pip-technical-team.github.io/pipload/reference/pip_load_dlw.html)

- cpfw:

  data frame with Price framework data for country/survey in `df`. It is
  loaded with `get_country_pfw(df, pfw)`. `pfw` is loaded in
  `pipload::pip_load_aux("pfw")`

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{
release <- "20250203"
pipfun::setup_working_release(release)

md   <- pipload::pip_load_dlw(country = "PHL", 2012)
md  <- survey_id_to_attr(md, unique(md$survey_id))
pfw  <- pipload::pip_load_aux("pfw")
cpfw <- get_country_pfw(md, pfw)
df   <- pd_split_alt_welfare(md, cpfw)
} # }
```
