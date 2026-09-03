# Resolve current content hashes for auxiliary measures from the aux catalog

Queries the `"aux"` stamp catalog once and returns the current
`content_hash` for each requested auxiliary measure. Each measure is
matched to exactly one catalog artifact whose path basename is
`<measure>.qs2` (e.g. `cpi.qs2`, `ppp.qs2`, `pfw.qs2`).

## Usage

``` r
get_aux_hashes(
  aux_measures,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- aux_measures:

  Character vector of auxiliary measures to resolve.

- verbose:

  Logical. Retained for consistency with pipeline helpers; catalog
  lookup itself emits no progress messages. Default:
  `getOption("pipdata.verbose", default = TRUE)`.

## Value

A named character vector of `content_hash` values, one per requested
measure. Names are the measure names.

## Details

This is the single source of the current aux hashes used to gate
aux-change detection in
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md).
It must be called once per pipeline run, before aux data is loaded, and
the result passed through the run so that the hashes recorded in the
master inventory match the aux data actually used.

The function aborts loudly when the `"aux"` alias is unavailable, a
requested artifact is missing, or multiple catalog rows match a measure.
It never falls back to
[`stamp::st_latest()`](https://randrescastaneda.github.io/stamp/reference/st_latest.html)
or to hashing loaded aux tables.

**Precondition**: the `"aux"` catalog and
[`pipload::load_aux_data()`](https://pip-technical-team.github.io/pipload/reference/load_aux_data.html)
must resolve through the same configured working release and storage
root. The hashes returned here are only meaningful if the aux data
subsequently loaded for processing comes from the same artifacts.
Callers must ensure the working release is set up consistently before
calling this function.

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
[`inv_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/inv_dlw_load.md),
[`inv_to_process()`](https://pip-technical-team.github.io/pipdata/reference/inv_to_process.md),
[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md),
[`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md),
[`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md),
[`pd_run_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_run_pipeline.md),
[`resolve_force_surveys()`](https://pip-technical-team.github.io/pipdata/reference/resolve_force_surveys.md),
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md),
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)
