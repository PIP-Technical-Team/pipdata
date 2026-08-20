# Determine which DLW surveys need processing

Compares the current DLW inventory against previously cleaned data and
auxiliary-file changes to identify surveys that require (re-)processing.
Returns the filtered inventory of surveys to clean.

## Usage

``` r
valid_dlw_load(
  inv,
  aux_measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  modules = c("ALL", "GROUP", "HIST", "GPWG", "BIN"),
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE),
  aux_hashes = NULL,
  force_surveys = NULL
)
```

## Arguments

- inv:

  A `data.table` of the full DLW inventory.

- aux_measures:

  Character vector of auxiliary measures to check for changes. Default:
  `c("pfw", "cpi", "ppp", "pop", "gdp", "pce")`.

- modules:

  Character vector of survey modules to include. Default:
  `c("ALL", "GROUP", "HIST", "GPWG", "BIN")`.

- force:

  Logical. If `TRUE`, skip the comparison against the master inventory
  and process all surveys.

- verbose:

  Logical. Print progress messages. Default:
  `getOption("pipdata.verbose", default = TRUE)`.

- aux_hashes:

  A named character vector of current aux `content_hash` values, one per
  requested auxiliary measure. Resolved once per run by
  [`get_aux_hashes()`](https://pip-technical-team.github.io/pipdata/reference/get_aux_hashes.md)
  and used to gate aux-change detection. When `NULL` (the default) and
  `force = FALSE`, the hashes are resolved internally so that direct
  callers retain the previous behavior of always running aux-change
  detection.

- force_surveys:

  Character vector of `survey_id` and/or `pip_id` values to re-process
  surgically, alongside the normal invalidation candidates. Forced
  surveys bypass
  [`inv_to_process()`](https://pip-technical-team.github.io/pipdata/reference/inv_to_process.md)
  only and are unioned into the candidate set, deduplicated via
  [`unique()`](https://rdrr.io/r/base/unique.html). Mutually exclusive
  with `force = TRUE`. Preserves content-based stamp versioning. Unknown
  identifiers are warned about and skipped. Default `NULL`.

## Value

A `data.table` of surveys to process. If no surveys require processing,
the function aborts with class `piperr`.

## Details

The function:

1.  Detects changes in auxiliary files (PFW, CPI, PPP, etc.) and
    identifies affected surveys.

2.  Filters the inventory to requested modules.

3.  Selects the latest version of each survey via `last_ver_inv()`.

4.  Unless `force = TRUE`, removes surveys already cleaned in the master
    inventory via
    [`inv_to_process()`](https://pip-technical-team.github.io/pipdata/reference/inv_to_process.md).

5.  Combines DLW-new and aux-changed surveys into a single inventory.

**Force-survey path (`force_surveys`)**: forced surveys are resolved via
[`resolve_force_surveys()`](https://pip-technical-team.github.io/pipdata/reference/resolve_force_surveys.md)
(lookup-first: `survey_id` membership, then `pip_id` reverse-map through
the already-loaded master inventory) and unioned into the candidate set.
They bypass
[`inv_to_process()`](https://pip-technical-team.github.io/pipdata/reference/inv_to_process.md)
only; aux-change detection runs normally and overlaps are deduplicated
via [`unique()`](https://rdrr.io/r/base/unique.html). Emits
`force_surveys_inf` / `force_surveys_unknown_inf` log entries.

**Aux-change gating (two-stage)**: aux-change detection is gated on the
current aux `content_hash` values passed via `aux_hashes`.

- Stage 1 (cheap): for each filtered/latest survey, compare its stored
  per-survey aux hash (from the master inventory's `aux_<measure>_hash`
  columns) against the current hash for that measure. A mismatch or a
  missing historical hash makes the survey a candidate. New surveys and
  DLW-content-changed surveys are also retained through
  [`inv_to_process()`](https://pip-technical-team.github.io/pipdata/reference/inv_to_process.md)
  and are deduplicated with the aux candidates.

- Stage 2 (detailed): for the changed measures only,
  [`valid_aux_load()`](https://pip-technical-team.github.io/pipdata/reference/valid_aux_load.md)
  / `compare_aux_*` identifies which requested surveys actually have
  changed rows inside the aux file. The affected surveys are intersected
  with the candidate set, so a globally changed aux table that only
  affects non-requested countries does not re-clean requested surveys.

The master inventory is loaded at most once within this function and
shared between the DLW comparison and the aux-hash comparison. This
guarantee is scoped to `valid_dlw_load()`; downstream steps such as
[`build_pip_inventory()`](https://pip-technical-team.github.io/pipdata/reference/build_pip_inventory.md)
load the master again for their own assembly and verification. When
`force = TRUE`, no master or aux comparison runs and all filtered/latest
surveys are processed.

**Logging**: This function writes the following entries to the
`"pipdata_log"`:

- `aux_changes_inf` — changes were detected in any of the requested
  auxiliary measures and at least one survey is affected. Includes the
  measures that changed and the number/list of affected surveys.

- `aux_no_changes_inf` — no auxiliary file changes were detected at all.

- `aux_changes_no_surveys_inf` — auxiliary files changed but no surveys
  in the inventory were affected by those changes.

- `surveys_to_clean_inf` — emitted once after the DLW-new and
  aux-changed inventories are combined and deduplicated; includes counts
  of new, aux-changed, and total unique surveys, plus the aux measures
  that triggered re-cleaning.

When neither new DLW surveys nor auxiliary changes leave anything to
process, the function aborts with `cli::cli_abort(class = "piperr")`
rather than returning `NULL` silently.

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
[`resolve_force_surveys()`](https://pip-technical-team.github.io/pipdata/reference/resolve_force_surveys.md),
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md),
[`survey_id_to_attr()`](https://pip-technical-team.github.io/pipdata/reference/survey_id_to_attr.md)
