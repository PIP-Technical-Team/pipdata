# Report staged dependency changes without writing artifacts

Report staged dependency changes without writing artifacts

## Usage

``` r
pd_change_report(
  inv = pipload::load_gmd_valid_inv(verbose = FALSE),
  master = pipload::load_pip_master_inventory(verbose = FALSE),
  manifest = NULL,
  context = pd_dependency_context()
)
```

## Arguments

- inv:

  Completed DLW validation inventory metadata. Before dependency
  planning, only `data_available = "Yes"` rows with status `"valid"` or
  `"invalid"` are retained. Recognized legacy blank/`"No"` retry rows
  are excluded, while malformed completed rows abort.

- master:

  PIP master inventory metadata.

- manifest:

  Optional dependency manifest.

- context:

  Optional resolved dependency context.

## Value

A `pip_dependency_plan`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
pipfun::setup_working_release("20260206", "TEST")
plan <- pd_change_report(
  inv = pipload::load_gmd_valid_inv(),
  master = pipload::load_pip_master_inventory()
)
} # }
```
