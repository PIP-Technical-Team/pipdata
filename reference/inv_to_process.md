# Remove surveys already cleaned from the processing inventory

Compares the current DLW inventory against the PIP master inventory by
joining on `survey_id` and comparing `content_hash` (DLW) against
`content_hash_dlw` (master). Surveys are kept when they are new to the
master (no `content_hash_dlw`) or when their DLW content hash differs
from the previously cleaned value. If the master inventory cannot be
loaded, all surveys are returned.

## Usage

``` r
inv_to_process(inv, verbose = TRUE, dt_master = NULL, master_available = NULL)
```

## Arguments

- inv:

  A `data.table` of DLW surveys (latest versions).

- verbose:

  Logical. Print progress messages.

- dt_master:

  A `data.table` of the PIP master inventory, already loaded by the
  caller
  ([`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md))
  and shared with the aux-hash comparison. Default `NULL`, in which case
  the master is loaded here.

- master_available:

  Logical. Whether the caller already attempted to load the master. When
  `TRUE`, `dt_master` is used as-is. When `FALSE`, the master was
  attempted but unavailable, so all surveys are returned without
  re-loading. Default `NULL` (unknown — load here if `dt_master` is
  `NULL`).

## Value

A `data.table` of surveys still needing processing, or `NULL` if all
surveys have already been cleaned.

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
[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md),
[`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md),
[`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md),
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md),
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)
