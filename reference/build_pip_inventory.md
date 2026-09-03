# Build the PIP master and release inventories from stamp catalogs

Delta/update assembler replacing `update_pip_inventory()`. Reads version
facts for **current-run surveys only** from stamp's persisted catalogs
(`"pip"` and `"pip_meta"` aliases), upserts them into the prior master
inventory, and saves the result. Old surveys not reprocessed this run
are retained unchanged from the prior master.

## Usage

``` r
build_pip_inventory(
  inv_to_clean,
  pip_id_map,
  verbose = getOption("pipdata.verbose", default = TRUE),
  aux_hashes = NULL
)
```

## Arguments

- inv_to_clean:

  A `data.table` of DLW surveys sent for processing (as returned by
  [`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)).
  Must have unique `survey_id` rows.

- pip_id_map:

  A `data.table` with exactly two columns: `survey_id` (DLW survey
  identifier) and `pip_id` (PIP identifier, uppercase). Built from
  successful
  [`process_data()`](https://pip-technical-team.github.io/pipdata/reference/process_data.md)
  calls in
  [`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md).

- verbose:

  Logical. Controls verbosity of downstream
  [`pipload::load_pip_master_inventory()`](https://pip-technical-team.github.io/pipload/reference/load_pip_data.html)
  and
  [`pipload::load_aux_data()`](https://pip-technical-team.github.io/pipload/reference/load_aux_data.html)
  calls. Default: `getOption("pipdata.verbose", default = TRUE)`.

- aux_hashes:

  A named character vector of current aux `content_hash` values, one per
  requested auxiliary measure (e.g. `cpi`, `ppp`, `pfw`). Resolved once
  per run by
  [`get_aux_hashes()`](https://pip-technical-team.github.io/pipdata/reference/get_aux_hashes.md)
  and recorded on the master-inventory rows produced for successfully
  processed surveys. Default `NULL` (no aux hashes recorded).

## Value

A `data.table`: the updated PIP master inventory. Does **not** include
`reporting_level` — enrich after load via
`pipload::pip_inv_enrich(inv, fields = "reporting_level")`.

## Details

Compared to `update_pip_inventory()`, this function:

- Queries catalogs then immediately filters to the current run's
  pip_ids, avoiding all catalog-wide validation issues.

- Does not require in-memory version metadata (crash-safe for current
  run).

- Does not compute `reporting_level` — enrichment is handled by
  [`pipload::pip_inv_enrich()`](https://pip-technical-team.github.io/pipload/reference/pip_inv_enrich.html)
  when needed by consumers.

- Upserts by `pip_id`: reprocessed surveys replace their old row; all
  other surveys are retained from the prior master. One row per
  `pip_id`, always.

**Logging**: writes the following entries to `"pipdata_log"`:

- `inv_update_inf`: verification that expected surveys appear in master
  (info-level if all confirmed, error-level if any missing).

- `release_write_err`: tryCatch-caught release inventory write failure.

**Column provenance**:

- `version_id_data`, `content_hash_data`, `size_bytes_data`,
  `created_at_data`, `path_data` â€” from the `"pip"` catalog.

- `version_id_metadata`, `content_hash_metadata`, `size_bytes_metadata`,
  `created_at_metadata`, `path_metadata` â€” from the `"pip_meta"`
  catalog.

- `pipeline_version_dlw`, `latest_version_id_dlw`, `content_hash_dlw`,
  `Checksum_dlw`, `path_dlw` â€” renamed from DLW inventory columns.

- `welfare_type` â€” derived from the 4th `_`-delimited segment of
  `pip_id`.

- `aux_<measure>_hash` (e.g. `aux_cpi_hash`, `aux_ppp_hash`,
  `aux_pfw_hash`) â€” current aux `content_hash` for each requested
  measure, from the run-level `aux_hashes` map passed by
  [`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md).
  Only populated for surveys successfully processed in the current run.

- `first_release_version_id`, `latest_release_version_id` â€” stamp
  version IDs of the release inventory (first appearance and most
  recent).

## See also

Other pd_process_data pipeline:
[`add_attr()`](https://pip-technical-team.github.io/pipdata/reference/add_attr.md),
[`aux_hash_candidates()`](https://pip-technical-team.github.io/pipdata/reference/aux_hash_candidates.md),
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
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md),
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)
