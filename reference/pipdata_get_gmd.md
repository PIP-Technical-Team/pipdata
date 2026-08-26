# Acquire GMD catalog datasets and reconcile the local inventory

Compares the current server catalog with `inv_gmd_list`, downloads
selected files, and reconciles the complete local acquisition inventory
to the authoritative current catalog. Acquisition actively downloads
only the five modules `"ALL"`, `"GROUP"`, `"HIST"`, `"GPWG"`, and
`"BIN"`; the catalog and validation layer also recognize `"ASPIRE"` and
`"L"`.

## Usage

``` r
pipdata_get_gmd(
  inv_gmd_list = "dlw_gmd_inv",
  check_missing = TRUE,
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

## Arguments

- inv_gmd_list:

  Character scalar. Acquisition inventory artifact ID. This ID controls
  loading, comparison, and persistence.

- check_missing:

  Logical scalar. Retry current unresolved five-module rows when `TRUE`.
  Default `TRUE`.

- verbose:

  Logical. Controls verbosity of downstream
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)
  calls. Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

Invisibly, a plain unclassed list with names `stage`, `outcome`,
`inventory`, `summary`, `failures`, and `artifacts`. `stage` is
`"acquisition"`. `outcome` is `"success"` when one or more downloads
complete without failure and the intended write is verified; `"partial"`
when useful downloads complete but a worker or non-commit workflow
failure occurs; `"failed"` when a required commit is unverified or no
download completes while failures occur; or `"no_work"` when trustworthy
discovery selects no downloads.

`inventory` is a copy of the trustworthy durable `data.table`, or `NULL`
when durable state is absent or unknown. `summary` has exactly
`n_total`, `n_success`, `n_failed`, `surveys_success`, and
`surveys_failed`. `failures` is a `data.table` with `survey_id`,
`phase`, `error_type`, and `condition_msg`, and never contains condition
objects. `artifacts$inventory` records `id`, `alias`, `attempted`,
`success`, `trustworthy`, `version_id`, `skipped`, and `reconciled` for
the durable inventory write.

## Details

Every selected download is pinned to the catalog's exact `FileName` and
uses `local_overwrite = TRUE`. Cached or ambiguous multi-file DLW
responses are failures and cannot mark a row available. When
`check_missing = TRUE`, current five-module rows whose durable state is
`data_available = "No"` are selected again. Retry is inventory-driven
and at least once.

The intended inventory is assembled even when no download is selected.
It drops obsolete checksums and catalog-deleted rows, retains current
successful rows, and retains current `"ASPIRE"`/`"L"` rows only when
they were already available. A changed durable inventory is written once
per completed attempt. A thrown or malformed write result is uncertain:
active storage is reloaded and compared with canonical prior and
intended content. The result never assumes that a reported write failure
rolled back.

The persisted acquisition inventory has required columns `Country`
(nonempty character), `Year` (nonmissing whole-number integer),
`Survey_acronym`, `Vermast`, `Veralt`, `Collection`, `FileName`, and
`Checksum` (nonempty character), `Module` (one of the seven recognized
modules), `Ext = "dta"`, and `data_available` (`"Yes"` or `"No"`).
Server columns beyond this schema are retained in deterministic name
order. A normalized server catalog with zero rows is a load failure, not
authoritative evidence that all durable acquisition state should be
deleted.

Logging is unconditional. `dlw_acquisition_inf` entries include an
attempt boundary, lifecycle and failure entries, and an exact completion
summary.

## Note

This function expects a working release to be configured via
[`pipfun::setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.html).
When called from
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md),
the release is already set. When called standalone, configure it first.
Invalid arguments and a missing working release are caller/precondition
errors and escape. Runtime folder, catalog, download, logging, and
persistence failures are returned in an inspectable failed or partial
result; interrupts are not converted.

## Examples

``` r
if (FALSE) { # \dontrun{
pipfun::setup_working_release("20260206", "TEST")
pipdata_get_gmd(
  inv_gmd_list = "dlw_gmd_inv",
  check_missing = TRUE
)
} # }
```
