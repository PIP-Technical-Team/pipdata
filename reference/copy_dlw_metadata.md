# Copy DLW Metadata Between Release Folders

Copies the DLW metadata (inventory, validation report, and log) from one
release folder to another.

## Usage

``` r
copy_dlw_metadata(
  from_release = NULL,
  from_identity = NULL,
  to_release = NULL,
  to_identity = NULL,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- from_release:

  Character. Release period **to copy metadata from**.

- from_identity:

  Character. Identity type **to copy metadata from**.

- to_release:

  Character. Release period **to copy metadata to**.

- to_identity:

  Character. Identity type **to copy metadata to**.

- verbose:

  Logical. Controls verbosity of downstream
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)
  and
  [`pipfun::get_wrk_release()`](https://pip-technical-team.github.io/pipfun/reference/get_wrk_release.html)
  calls. Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

Invisibly returns `TRUE` if the operation completes successfully.

## Details

The function:

1.  Sets up a working environment for the source release folder to load,
    the metadata (`gmd_valid_inv`, `gmd_valid_report`, and
    `gmd_valid_log`).

2.  Sets up a working environment for the destination release folder to
    copy the metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
copy_dlw_metadata(
  from_release = "20250203",
  from_identity = "TEST",
  to_release = "20250811",
  to_identity = "TEST"
)
} # }
```
