# Retrieve the GMD server catalog and initialize an acquisition inventory

Loads the authoritative server catalog for the seven recognized modules
and `.dta` files, then builds the local acquisition inventory. On a
first run the inventory contains the five active download modules as
unavailable; current `"ASPIRE"` and `"L"` rows are retained only when a
prior inventory already records them as available. Obsolete rows and
superseded checksums are removed.

## Usage

``` r
dlw_gmd_list(inv_gmd_list = "dlw_gmd_inv")
```

## Arguments

- inv_gmd_list:

  Character scalar. Acquisition inventory artifact ID. This ID controls
  loading, comparison, and persistence.

## Value

Invisibly, a copy of the verified durable acquisition `data.table`.

## Details

The write uses `inv_gmd_list` consistently. Any thrown, null-version, or
malformed write result is treated as uncertain and reconciled by
reloading durable state; the function aborts unless the intended
inventory is verified.

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
pipfun::setup_working_release("20260206", "TEST")
gmd_list <- dlw_gmd_list()
head(gmd_list)
} # }
```
