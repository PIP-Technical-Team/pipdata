# Merge country/survey PFW info with dataliweb survey data

Merge country/survey PFW info with dataliweb survey data

## Usage

``` r
pd_cpfw_merge(dt, pfw)
```

## Arguments

- dt:

  DLW country/survey data

- pfw:

  PFW

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{
release <- "20250203"
pipfun::setup_working_release(release)

pfw  <- pipload::pip_load_aux("pfw")
md   <- pipload::pip_load_dlw(country = "PHL", 2012)
md  <- survey_id_to_attr(md, unique(md$survey_id))
l    <- pd_cpfw_merge(md, pfw)

gd   <- pipload::pip_load_dlw("CHN", 2015)
gd  <- survey_id_to_attr(gd, unique(gd$survey_id))
l    <- pd_cpfw_merge(gd, pfw)
} # }
```
