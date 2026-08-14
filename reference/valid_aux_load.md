# Retrieve the inventory of aux files that changed from previous release or vintage

Retrieve the inventory of aux files that changed from previous release
or vintage

## Usage

``` r
valid_aux_load(
  measure = c("cpi", "ppp", "pfw", "pop"),
  compare = "all",
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- measure:

  measure of auxiliary files to compare

- compare:

  either `release`, `vintage` or `all`

- verbose:

  Logical. Print progress messages. Default:
  `getOption("pipdata.verbose", default = TRUE)`.

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{
valid_aux_load()
} # }
```
