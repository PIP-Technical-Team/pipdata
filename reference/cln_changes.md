# Clean output from compare_aux_releases and compare_aux_vintages

Clean output from compare_aux_releases and compare_aux_vintages

## Usage

``` r
cln_changes(changes)
```

## Arguments

- changes:

  output from
  [`pipaux::compare_aux_releases`](https://rdrr.io/pkg/pipaux/man/compare_aux_releases.html)
  or
  [`pipaux::compare_aux_vintages`](https://rdrr.io/pkg/pipaux/man/compare_aux_vintages.html)

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{
changes_vintage <- pipaux::compare_aux_vintages(measure = "pfw", verbose = FALSE)
cln_chngs <- cln_changes(changes_vintage)
} # }
```
