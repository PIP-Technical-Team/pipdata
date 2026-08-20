# Resolve `force_surveys` identifiers to survey_id values

Maps a character vector of `survey_id` and/or `pip_id` identifiers to
the subset present in the module-filtered, latest-version inventory
(`inv_svy_full`). Lookup is first-by-`survey_id` membership, then by
`pip_id` reverse-map through the already-loaded master inventory.
Unknown identifiers are collected, not aborted.

## Usage

``` r
resolve_force_surveys(force_surveys, inv_svy_full, dt_master, verbose = TRUE)
```

## Arguments

- force_surveys:

  Character vector of `survey_id` and/or `pip_id` identifiers, or
  `NULL`.

- inv_svy_full:

  A `data.table` of the module-filtered, latest-version DLW inventory
  (already computed by
  [`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)).

- dt_master:

  A `data.table` of the PIP master inventory, already loaded by the
  caller, or `NULL` when unavailable.

- verbose:

  Logical. Print progress messages.

## Value

A named list with character vectors: `survey_ids` (resolved survey_ids
present in `inv_svy_full`), `resolved_from_survey_id`,
`resolved_from_pip_id`, and `unknown`.

## See also

Other pd_process_data pipeline:
[`add_attr()`](https://pip-technical-team.github.io/pipdata/reference/add_attr.md),
[`aux_hash_candidates()`](https://pip-technical-team.github.io/pipdata/reference/aux_hash_candidates.md),
[`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md),
[`create_attr()`](https://pip-technical-team.github.io/pipdata/reference/create_attr.md),
[`data_to_dt()`](https://pip-technical-team.github.io/pipdata/reference/data_to_dt.md),
[`filter_aux_data()`](https://pip-technical-team.github.io/pipdata/reference/filter_aux_data.md),
[`filter_aux_inv()`](https://pip-technical-team.github.io/pipdata/reference/filter_aux_inv.md),
[`fix_year_var()`](https://pip-technical-team.github.io/pipdata/reference/fix_year_var.md),
[`get_aux_hashes()`](https://pip-technical-team.github.io/pipdata/reference/get_aux_hashes.md),
[`inv_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/inv_dlw_load.md),
[`inv_to_process()`](https://pip-technical-team.github.io/pipdata/reference/inv_to_process.md),
[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md),
[`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md),
[`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md),
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md),
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)
