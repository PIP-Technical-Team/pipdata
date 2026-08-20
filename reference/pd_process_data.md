# Process DLW inventory and create cleaned pip data

Iterate over the datalibweb (DLW) inventory, process each survey by
merging auxiliary data (PFW, CPI, PPP, population, GDP, PCE), cleaning
main variables, creating metadata, and saving new versions of the
cleaned data and metadata into the pip storage. The function returns an
updated pip inventory with the new versions recorded.

## Usage

``` r
pd_process_data(
  inv = NULL,
  aux_measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE),
  force_surveys = NULL
)
```

## Arguments

- inv:

  A data.frame or tibble containing the DLW inventory. Default `NULL`,
  in which case it is loaded internally via
  [`pipload::load_gmd_valid_inv()`](https://pip-technical-team.github.io/pipload/reference/load_dlw_data.html).

- aux_measures:

  A character vector of auxiliary measures to load and merge with the
  DLW data. The default is
  `c("pfw", "cpi", "ppp", "pop", "gdp", "pce")`.

- force:

  Logical. If `TRUE`, forces reprocessing of all surveys by switching
  stamp versioning to `"timestamp"` and bypassing the master inventory
  comparison. Default `FALSE`. For surgical re-processing without the
  global versioning side effect, see `force_surveys`.

- verbose:

  Logical. Print progress messages. Default:
  `getOption("pipdata.verbose", default = TRUE)`.

- force_surveys:

  Character vector of `survey_id` and/or `pip_id` values to re-process
  surgically, alongside the normal invalidation candidates. Mutually
  exclusive with `force = TRUE`. Preserves content-based stamp
  versioning (unlike `force = TRUE`, which switches to timestamp
  versioning for the entire run). Unknown identifiers are warned about
  and skipped. Default `NULL`.

## Value

A data.frame: updated pip inventory (`new_pip_inv`) with new versions
for cleaned data and metadata.

## Details

**Logging**: This function writes `process_summary_inf` and
`null_svys_inf` entries to the `"pipdata_log"`, summarizing totals and
failed surveys. Additional entries for auxiliary file changes and
inventory verification are emitted by
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)
and
[`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md)
respectively.

**Aux hashes**: the current `content_hash` for every requested auxiliary
measure is resolved once from the `"aux"` stamp catalog via
[`get_aux_hashes()`](https://pip-technical-team.github.io/pipdata/reference/get_aux_hashes.md)
before aux data is loaded. The run-level hash map is passed to
[`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md)
and recorded in the master inventory so that
[`valid_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_dlw_load.md)
can gate aux-change detection against the aux data actually used in this
run.

**Recode spec**: the recode specification is synced to stamp once via
[`sync_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/sync_recode_spec.md)
before the per-survey loop and the resolved spec is threaded into each
[`process_data()`](https://pip-technical-team.github.io/pipdata/reference/process_data.md)
call, so
[`apply_recode_spec()`](https://pip-technical-team.github.io/pipdata/reference/apply_recode_spec.md)
performs no stamp I/O per survey.

**Memory management**: surveys are processed one at a time. After each
survey is saved, the large intermediates (`df`, `ls_cpfw`, `ls_clean`,
`metadata`) are explicitly removed and
[`gc()`](https://rdrr.io/r/base/gc.html) is called inside
[`process_data()`](https://pip-technical-team.github.io/pipdata/reference/process_data.md)
before the next survey is loaded, keeping peak heap bounded on
full-inventory runs.

## Examples

``` r
if (FALSE) { # \dontrun{
release <- "20250203"
pipfun::setup_working_release(release)
pd_process_data()
} # }
```
