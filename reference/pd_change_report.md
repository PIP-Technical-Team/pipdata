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

  DLW inventory metadata.

- master:

  PIP master inventory metadata.

- manifest:

  Optional dependency manifest.

- context:

  Optional resolved dependency context.

## Value

A `pip_dependency_plan`, invisibly.
