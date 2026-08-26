# Acquire and validate DLW data

`pipdata_dlw_process()` is the current supported DLW entry point. It
runs the explicit acquisition and validation stages in order, emits a
scalar `dlw_summary_inf` entry from their returned facts, and attempts
the existing `"dlw"` checkpoint. A broader `run_pipeline()` orchestrator
is future direction only and is not part of the current API.

## Usage

``` r
pipdata_dlw_process(
  inv_gmd_list = "dlw_gmd_inv",
  get_dlw_data = TRUE,
  validate_dlw_data = TRUE,
  check_missing = TRUE,
  release = NULL,
  identity = NULL,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- inv_gmd_list:

  Character scalar. Acquisition inventory artifact ID. This ID controls
  loading, comparison, and persistence.

- get_dlw_data:

  Logical scalar. Run acquisition. Default `TRUE`.

- validate_dlw_data:

  Logical scalar. Run validation. Default `TRUE`.

- check_missing:

  Logical scalar. Retry current unresolved five-module rows when `TRUE`.
  Default `TRUE`.

- release:

  Required nonempty character scalar. Data release identifier or date.
  The formal default `NULL` is a missing-required-value sentinel, not an
  operational default, and aborts before setup.

- identity:

  Required character scalar. One of `"PROD"`, `"INT"`, or `"TEST"`. The
  formal default `NULL` is a missing-required-value sentinel, not an
  operational default, and aborts before setup.

- verbose:

  Logical. Controls verbosity of downstream
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)
  calls. Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

Invisibly, a plain unclassed list with names `stage`, `outcome`,
`acquisition`, `validation`, `failures`, and `checkpoint`. `stage` is
`"dlw"`; `outcome` is the aggregate `"success"`, `"partial"`,
`"failed"`, or `"no_work"`. `acquisition` and `validation` are the
six-field stage results documented by
[`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md)
and
[`pipdata_validate_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_validate_gmd.md).
A wrapper-only not-run stage still has names `stage`, `outcome`,
`inventory`, `summary`, `failures`, and `artifacts`, with
`inventory = NULL`, `summary = list(reason = ...)`, an empty failure
table, and empty artifacts.

`failures` is the compact wrapper `data.table` with `survey_id`,
`phase`, `error_type`, and `condition_msg`. `checkpoint` records
`summary_logged`, `summary_condition_msg`, `attempted`, `success`,
`trustworthy`, `alias`, `stage`, `version_id`, `skipped`, `reconciled`,
and `condition_msg`.

## Details

Validation continues after acquisition `"no_work"`, `"partial"`, or even
`"failed"` when acquisition still returns a trustworthy non-`NULL`
durable inventory. It is dependency-blocked only when that prerequisite
is unavailable. Stage outcomes are aggregated as `"success"`,
`"partial"`, `"failed"`, or `"no_work"`; disabled or dependency-blocked
nested stages use `"not_run"` with `summary$reason` equal to
`"disabled"` or `"dependency_failed"`.

With both stages requested, acquisition `"success"` followed by
validation `"success"`/`"no_work"` is aggregate `"success"`; acquisition
`"no_work"` followed by validation `"success"` is also `"success"`,
while two no-work stages are `"no_work"`. Any acquisition `"partial"` is
aggregate `"partial"`. Acquisition `"success"`/`"no_work"` plus
validation `"partial"`/`"failed"` is `"partial"`. Acquisition `"failed"`
with trustworthy state plus validation
`"success"`/`"no_work"`/`"partial"` is `"partial"`, but two failed
stages are `"failed"`. An untrustworthy acquisition failure that
dependency-blocks validation is `"failed"`. When only one stage is
requested, its outcome is the aggregate; both disabled is `"no_work"`
unless a wrapper failure occurs. Checkpoint or summary-log failure is
reported in the checkpoint facts and does not rewrite the completed
business outcome.

The wrapper routes `inv_gmd_list` through acquisition, bootstrap, and
the internal validation path, so custom inventory IDs are honored end to
end. Validate-only calls never display a menu. A missing inventory in
validate-only or noninteractive execution is an inspectable
validation/acquisition failure, respectively. The Download/Abort menu is
reserved for interactive calls that request acquisition.

## Note

Invalid arguments, working-release setup failures, interactive user
cancellation, and interrupts escape. Runtime failures after setup are
converted to compact wrapper or stage facts so callers can inspect the
returned result. Normal unassigned calls remain quiet because the result
is invisible.

## Examples

``` r
if (FALSE) { # \dontrun{
pipdata_dlw_process(inv_gmd_list = "dlw_gmd_inv",
            get_dlw_data = TRUE,
            validate_dlw_data = TRUE,
            check_missing   = TRUE,
            release         = "20260206",
            identity        = "TEST"
            )
} # }
```
