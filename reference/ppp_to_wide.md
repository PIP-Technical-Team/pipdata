# Convert PPP data from `pipload` to wide format

Convert PPP data from `pipload` to wide format

## Usage

``` r
ppp_to_wide(ppp)
```

## Arguments

- ppp:

  data frame with ppp data from `pipload::pip_load_aux("ppp")`

## Value

data.table with PPP values to wide format based on versioning

## Examples

``` r
if (FALSE) { # \dontrun{
release <- "20250203"
pipfun::setup_working_release(release)

ppp <-  pipload::pip_load_aux("ppp")
x   <-  ppp_to_wide(ppp)
names(x)
} # }
```
