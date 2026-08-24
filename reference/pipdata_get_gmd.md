# Retrieve and Save GMD Catalog Datasets to a Local Directory

This wrapper function automates the process of managing GMD catalog
datasets by performing the following tasks:

1.  Checks for new datasets in the GMD catalog using the inventory file
    (`dlw_gmd_inv`), which contains the current list of available GMD
    datasets.

2.  If new datasets are found (specifically `"GPWG"`, `"GROUP"`,
    `"BIN"`, `"HIST"`, `"ALL"`, `"ASPIRE"`, and `"L"`), downloads them
    using
    [`dlw::dlw_get_gmd`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.html)
    and save them to the local directory.

3.  Updates the inventory file (`dlw_gmd_inv`) with information about
    the newly downloaded datasets.

Logging is unconditional. The function writes `dlw_acquisition_inf`
entries for start, no-new-data, per-survey download failures, and
completion. Error conditions are represented by `condition_msg` in
`logmeta`; the discriminator in `logmeta$error` is always a string.

## Usage

``` r
pipdata_get_gmd(
  inv_gmd_list = "dlw_gmd_inv",
  check_missing = TRUE,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- inv_gmd_list:

  Character. The name of the inventory file containing the list of GMD
  datasets.

- check_missing:

  Logical. Whether to check for and retrieve missing data. Default is
  `TRUE`.

- verbose:

  Logical. Controls verbosity of downstream
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)
  calls. Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

Invisibly returns `NULL`; the acquisition inventory is persisted as a
side effect.

## Note

This function expects a working release to be configured via
[`pipfun::setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.html).
When called from
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md),
the release is already set. When called standalone, ensure
`setup_working_release()` has been invoked first.

## Examples

``` r
if (FALSE) { # \dontrun{
pipdata_get_gmd(
  inv_gmd_list = "dlw_gmd_inv",
  check_missing = TRUE
)
} # }
```
