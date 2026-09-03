# Save cleaned PIP data or metadata to versioned storage

Iterates over a named list of cleaned `data.table` objects and writes
each one to the PIP storage backend via
[`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html).
Errors during individual saves are caught, logged, and returned as
`NULL` so that remaining surveys can continue.

## Usage

``` r
save_pip_data(
  data,
  alias,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- data:

  A named list of `data.table` objects to save. Names are used as the
  `id` argument to
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html).

- alias:

  Character scalar. The storage alias passed to
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)
  (e.g., `"pip"` for survey data, `"pip_meta"` for metadata).

- verbose:

  Logical. Controls verbosity of downstream
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)
  calls. Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

A named list with one entry per artifact: `list(pip_id, success = TRUE)`
on success or `NULL` on failure. Version metadata is persisted to the
stamp catalog and read back by
[`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md)
— it is not returned here.

## Details

Artifacts are written largest-first (by
[`object.size()`](https://rdrr.io/r/utils/object.size.html)) so that the
largest serialisation buffers are allocated while the heap is cleanest.
Before writing any artifact whose in-memory size exceeds
`getOption("pipdata.gc_threshold_bytes", default = 100e6)` (default 100
MB), a [`gc()`](https://rdrr.io/r/base/gc.html) cycle is triggered to
reclaim fragmented memory and reduce the risk of
`cannot allocate buffer` errors from `qs2`.

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
[`pd_run_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_run_pipeline.md),
[`resolve_force_surveys()`](https://pip-technical-team.github.io/pipdata/reference/resolve_force_surveys.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md),
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)
