# Process DLW Data

This wrapper function automates key steps in processing DLW data by
performing the following tasks:

1.  Checks if the list of GMD DLW datasets is available, if not, get the
    list

2.  Checks for new datasets in the GMD catalog, downloads them using
    [`dlw::dlw_get_gmd`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.html),
    and save them to the local directory.

3.  Validates the downloaded datasets using
    [`pipdata::pipdata_validate_gmd`](https://pip-technical-team.github.io/pipdata/reference/pipdata_validate_gmd.md)
    and updates the validation inventory (`"gmd_valid_inv"`).

4.  Writes a `dlw_summary_inf` stage marker and persists a DLW logging
    checkpoint after the delegates complete.

## Usage

``` r
pipdata_dlw_process(
  inv_gmd_list = "dlw_gmd_inv",
  get_dlw_data = TRUE,
  validate_dlw_data = TRUE,
  check_missing = TRUE,
  release = NULL,
  identity = NULL,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- inv_gmd_list:

  Character. The name of the inventory file containing the list of GMD
  datasets.

- get_dlw_data:

  Logical. Whether to check for and download new DLW data. Default is
  `TRUE`.

- validate_dlw_data:

  Logical. Whether to validate newly downloaded datasets. Default is
  `TRUE`.

- check_missing:

  Logical. Whether to check for and retrieve missing data. Default is
  `TRUE`.

- release:

  Character. The data release identifier or date, used to configure the
  working environment.

- identity:

  Character. One of `"PROD"`, `"INT"`, or `"TEST"`.

- verbose:

  Logical. Controls verbosity of downstream
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)
  calls. Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

Invisibly returns `NULL`. Output files are written to disk.

## Examples

``` r
if (FALSE) { # \dontrun{
pipdata_dlw_process(inv_gmd_list = "dlw_gmd_inv",
            get_dlw_data = TRUE,
            validate_dlw_data = TRUE,
            check_missing   = TRUE,
            release         = "20260206",
            identity        = "TEST"
            )
} # }
```
