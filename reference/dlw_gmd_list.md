# Retrieve a List of GMD datasets from the Server and save it in the local dlw inventory folder.

This function fetches a list of GMD datasets from the server, filters
them based on Module and file extension.

## Usage

``` r
dlw_gmd_list(inv_gmd_list = "dlw_gmd_inv")
```

## Arguments

- inv_gmd_list:

  Character. The name of the inventory file containing the list of GMD
  datasets.

## Value

A data table containing the list of GMD datasets.

## Note

This function expects a working release to be configured via
[`pipfun::setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.html).
When called from
[`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md),
the release is already set. When called standalone, ensure
`setup_working_release()` has been invoked first.

## Examples

``` r
if (FALSE) { # \dontrun{
gmd_list <- dlw_gmd_list()
head(gmd_list)
} # }
```
