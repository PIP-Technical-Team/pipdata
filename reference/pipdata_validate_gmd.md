# Validate GMD data and generate inventory report data

Logging is unconditional. The function writes `dlw_validation_inf`
entries for validation start, no-new-data, load/validation failures,
inventory and report workflow phases. Error conditions are stored as
`condition_msg` and the discriminator in `logmeta$error` is always a
string.

## Usage

``` r
pipdata_validate_gmd(verbose = getOption("pipdata.verbose", default = TRUE))
```

## Arguments

- verbose:

  Logical. Controls verbosity of downstream I/O calls (including
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)).
  Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

Invisibly returns `NULL`; validation inventory and report artifacts are
persisted as side effects.

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
pipdata_validate_gmd()
} # }
```
