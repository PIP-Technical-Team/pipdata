# Apply recode specification to a data.table

Reads the recode spec from stamp (synced once upstream by
[`sync_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/sync_recode_spec.md))
and applies all matching rules to `dt`.

## Usage

``` r
apply_recode_spec(dt, alias = "pip_inv", verbose = TRUE, recode_spec = NULL)
```

## Arguments

- dt:

  data.table with DLW survey data.

- alias:

  Stamp alias. Default: `"pip_inv"`.

- verbose:

  Logical. Default: `TRUE`.

- recode_spec:

  Optional pre-resolved spec as returned by
  [`sync_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/sync_recode_spec.md)
  — a named list with `spec` (the full recode spec) and `version_id`.
  When supplied, the spec and version are taken from it and **no** stamp
  I/O is performed. When `NULL` (the default), the spec is read from
  stamp on each call. In the per-survey pipeline the spec is synced once
  upstream and threaded in via this argument to avoid thousands of
  redundant catalog reads.

## Value

`dt` (modified by reference) with attribute `recode_spec_version_id`.

## Details

**Replace-type recodes** (`range_clamp`, `binary_map`, `haven_labels`):
if `source_column` differs from `var_name`, the source column is
**renamed** to `var_name` after the recode (dropping the source).
Example: `urban → area`, `male → gender`.

**Derive-type recodes** (`binned_from_continuous`,
`quantile_from_continuous`): the source column is preserved and
`var_name` is added as a new column. Example: `age` stays, `age_group`
is added.

The stamp `version_id` of the active spec is attached as attribute
`"recode_spec_version_id"` on the returned `dt`.
