# Validate GMD data and generate inventory report data

Validate GMD data and generate inventory report data

## Usage

``` r
pipdata_validate_gmd(
  log = TRUE,
  save_log = TRUE,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- log:

  Logical. Keep logging file, TRUE/FALSE default value is `TRUE`

- save_log:

  Logical. Save logging file, TRUE/FALSE default value is `TRUE`

- verbose:

  Logical. Controls verbosity of downstream I/O calls (including
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)).
  Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

data.table, inventory report

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
pipdata_validate_gmd(
  log = FALSE,
  save_log = FLASE
)
} # }
```
