# Identify surveys whose stored aux hash differs from the current aux hash

Stage 1 of the two-stage aux-change gate. For each previously-cleaned
survey (present in the master inventory), compares its stored per-survey
aux hash (from the master's `aux_<measure>_hash` columns) against the
current aux `content_hash` for each requested measure. A survey is a
candidate when any requested measure's stored hash differs from the
current hash, or when the stored hash is missing (survey cleaned before
this feature — treated as changed).

## Usage

``` r
aux_hash_candidates(inv, dt_master, aux_hashes, verbose = TRUE)
```

## Arguments

- inv:

  A `data.table` of DLW surveys (latest versions).

- dt_master:

  A `data.table` of the PIP master inventory.

- aux_hashes:

  A named character vector of current aux `content_hash` values, one per
  requested measure.

- verbose:

  Logical. Print progress messages.

## Value

A `data.table` of candidate surveys (subset of `inv`), with an attribute
`changed_measures` holding the measures whose hash changed. Returns
`NULL` when no survey is a candidate.

## Details

The master inventory is reduced to one row per `survey_id` for the same
`content_hash_dlw`. All rows in that group must have identical aux
hashes; a conflict aborts loudly (this protects the invariant that split
`pip_id`s for one survey/content version use the same aux versions).

The current DLW inventory is joined to the master on both `survey_id`
and the DLW content hash (`inv$content_hash` matched to
`master$content_hash_dlw`), so a survey with multiple historical DLW
versions is compared against the aux hashes of its current version only.

## See also

Other pd_process_data pipeline:
[`add_attr()`](https://pip-technical-team.github.io/pipdata/reference/add_attr.md),
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
