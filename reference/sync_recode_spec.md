# Sync recode spec from package to stamp

Compares the package YAML (`inst/extdata/recode_spec.yml`) to the latest
stamp version. If different (or no stamp version exists), saves a new
version. Returns the active spec and its stamp version_id.

## Usage

``` r
sync_recode_spec(alias = "pip_inv", verbose = TRUE)
```

## Arguments

- alias:

  Stamp alias. Default: `"pip_inv"`.

- verbose:

  Logical; show sync messages?

## Value

Named list: `spec` (full recode spec list), `version_id` (character).
