# Batch-deflate every survey in the PIP master inventory

Builds a fresh dependency plan, loads exact planned data and metadata
versions through the internal strict exact-deflation path, saves
verified receipts to the `"pip_deflated"` alias, and publishes inventory
and manifest checkpoints.

## Usage

``` r
pd_deflate_pipeline(
  inv = NULL,
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE),
  bootstrap = FALSE,
  bootstrap_entities = NULL,
  dependency_plan = NULL
)
```

## Arguments

- inv:

  A master inventory `data.table` (as returned by
  [`pipload::load_pip_master_inventory()`](https://pip-technical-team.github.io/pipload/reference/load_pip_data.html)).
  Default `NULL`, in which case the master inventory is loaded
  internally.

- force:

  Logical. If `TRUE`, re-deflate every row regardless of the `deflated`
  column. Default `FALSE`.

- verbose:

  Logical. Controls verbosity of downstream
  [`pipload::load_pip_master_inventory()`](https://pip-technical-team.github.io/pipload/reference/load_pip_data.html)
  and
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)
  calls. Default: `getOption("pipdata.verbose", default = TRUE)`.

- bootstrap:

  Logical. Explicitly permit unknown-provenance work.

- bootstrap_entities:

  Optional restrictive bootstrap identifiers.

- dependency_plan:

  Optional advisory plan, revalidated before writes.

## Value

The updated master inventory `data.table` (rows deflated in this run
have `deflated = TRUE`, `content_hash_deflated` and
`aux_*_hash_at_deflation` populated). Side effects: when at least one
survey is processed, writes deflated artifacts to the `"pip_deflated"`
alias, writes the updated master to `"pip_master"`, and logs a
`deflate_summary_inf` entry.

## Details

This function is an independent pipeline stage: it is **not** called by
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md).
Run it after cleaning (and after the `"pip_deflated"` alias is
registered, e.g. via
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
or an explicit
[`stamp::st_init()`](https://randrescastaneda.github.io/stamp/reference/st_init.html)).

First version policy (R7): everything not yet deflated is deflated.
There is no incremental aux-hash gating yet – pass `force = TRUE` to
re-deflate surveys whose `deflated` column is already `TRUE`.

**Logging**: writes a `deflate_summary_inf` info entry to
`"pipdata_log"` with pinned keys `n_total`, `n_success`, `n_failed`,
`surveys_success`, and `surveys_failed` whenever at least one survey was
processed. Per-survey failures are logged as `error` entries
(`deflation_na`, `deflate_save_error`, or the underlying condition
class) with the survey id. A missing `content_hash_deflated` for a
deflated survey is logged (`deflate_provenance_missing`), never silent.

**Column provenance**:

- `deflated` – logical; `TRUE` for surveys successfully deflated.

- `content_hash_deflated` – `content_hash` of the `"pip_deflated"`
  artifact (queried from stamp after the run).

- `aux_cpi_hash_at_deflation`, `aux_ppp_hash_at_deflation`,
  `aux_pop_hash_at_deflation` – aux `content_hash` resolved once per run
  by
  [`get_aux_hashes()`](https://pip-technical-team.github.io/pipdata/reference/get_aux_hashes.md),
  snapshot on the deflated rows. These describe the aux catalog state
  when the pipeline ran; the exact aux vintage consumed is embedded in
  the pinned `pip_meta` artifact (pinned by `version_id_metadata`), not
  this snapshot.

## See also

[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
for the cleaning stage,
[`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md)
for single-survey deflation,
[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
for the report that renders the `deflate_summary_inf` entry.

## Examples

``` r
if (FALSE) { # \dontrun{
release <- "20260401"
pipfun::setup_working_release(release, "TEST", verbose = FALSE)
stamp::st_init(
  root = fs::path(getOption("pipfun.main_dir"), "pip_repository", "pip_deflated"),
  alias = "pip_deflated"
)
new_pip_inv <- pd_deflate_pipeline(force = TRUE, verbose = TRUE)
} # }
```
