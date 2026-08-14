# Generate a markdown report from a pipeline log

Parses a `piplog` object produced by
[`pipfun::log_filter()`](https://pip-technical-team.github.io/pipfun/reference/log_filter.html)
and writes a structured markdown document summarising errors,
informational messages, and affected surveys.

## Usage

``` r
log_report(
  log = NULL,
  path = NULL,
  title = "Pipeline Log Report",
  overwrite = FALSE
)
```

## Arguments

- log:

  A `piplog` object (inherits from `data.table`). Default `NULL`, in
  which case it is loaded internally via
  `pipfun::log_filter(name = "pipdata_log")`.

- path:

  Character scalar. File path for the output `.md` file. If `NULL`
  (default), the report is returned as a character vector and not
  written to disk.

- title:

  Character scalar. Title for the report (default:
  `"Pipeline Log Report"`).

- overwrite:

  Logical. Overwrite `path` if it already exists (default: `FALSE`).

## Value

Invisibly, the report as a character vector (one element per line). If
`path` is non-`NULL`, the file is written as a side-effect.

## Details

The report contains:

- Run metadata (time window, total entries, success/fail counts).

- Processing summary: total, cleaned, and failed counts (from
  `process_summary_inf` log entry).

- Auxiliary file changes: which measures changed and how many surveys
  were affected (from `aux_changes_inf` log entry).

- Summary table by error / info type.

- Country-level breakdown of errors.

- Inventory verification: confirmed vs missing surveys (from
  `inv_update_inf` log entry).

- Surveys skipped during data processing or metadata creation
  (`skipped_svys_data` / `skipped_svys_metadata` entries), with reasons.

- List of surveys that failed processing (`null_svys_inf` entry).

Sections that rely on a specific logmeta entry are silently omitted when
that entry is absent from the log.

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
[`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md),
[`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md),
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md),
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Return as character vector (log defaults to the "pipdata_log")
report <- log_report()
# Write to file
log_report(path = "log_report.md", overwrite = TRUE)
} # }
```
