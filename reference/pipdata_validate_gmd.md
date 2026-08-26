# Validate available GMD data and commit completed validation state

Validates current available acquisition rows with
[`dlw_validation_engine()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation_engine.md).
The engine receives mappings for all seven recognized modules: `"ALL"`,
`"GROUP"`, `"HIST"`, `"GPWG"`, `"BIN"`, `"ASPIRE"`, and `"L"`. A
completed engine result is classified as `"valid"` or `"invalid"`;
invalid data is a completed validation, not an execution failure.

## Usage

``` r
pipdata_validate_gmd(verbose = getOption("pipdata.verbose", default = TRUE))
```

## Arguments

- verbose:

  Logical scalar. Controls verbosity of downstream I/O calls (including
  [`pipload::pip_write()`](https://pip-technical-team.github.io/pipload/reference/pip_write.html)).
  Default: `getOption("pipdata.verbose", default = TRUE)`.

## Value

Invisibly, a plain unclassed list with names `stage`, `outcome`,
`inventory`, `summary`, `failures`, and `artifacts`. `stage` is
`"validation"`. `outcome` is `"success"` when one or more validations
complete and required commits are verified; `"partial"` when a valid or
invalid result completes alongside an execution or non-commit workflow
failure; `"failed"` when a required commit is unverified or no
validation completes while failures occur; or `"no_work"` when
trustworthy discovery selects no validation workers. Invalid
classifications alone do not make an outcome partial or failed.

`inventory` is a copy of trustworthy durable completed state, or `NULL`
when that state is absent or unknown. `summary` has exactly `n_total`,
`n_valid`, `n_invalid`, `n_failed`, `surveys_valid`, `surveys_invalid`,
and `surveys_failed`; totals count terminal worker outcomes. `failures`
is a `data.table` with `survey_id`, `phase`, `error_type`, and
`condition_msg`. `artifacts` contains `report` and `inventory` facts,
each with `id`, `alias`, `attempted`, `success`, `trustworthy`,
`version_id`, `skipped`, and `reconciled`. The full validation report is
not returned.

## Details

`gmd_valid_inv` is authoritative completed-data state. It contains only
`data_available = "Yes"` rows with status `"valid"` or `"invalid"`.
Load, artifact-info, engine, and inventory-row failures produce no
inventory or report row and therefore retry because their current
acquisition key remains absent. Before pruning stale checksums,
historical inventory versions are scanned by `survey_id`; the next
completed `pipeline_version` is one plus the historical maximum, or `1L`
with no history. Failed attempts consume no version. Every
catalog-listed historical version must be readable and schema-valid;
otherwise validation blocks rather than understating history.

Completed inventory rows have exactly the core fields `survey_id`,
`pipeline_version`, `latest_version_id`, `content_hash`, `file_path`,
`status`, `data_available`, `date_validated`, and `Checksum`, plus
parsed identity fields `country_code`, `surveyid_year`,
`survey_acronym`, `vermast`, `veralt`, `collection`, `module`, and
`tool`. Character identity and artifact fields are nonmissing and
nonempty; `pipeline_version` is a positive integer; `surveyid_year` is a
nonnegative integer; `date_validated` is nonmissing Date/POSIX time;
`status` is `"valid"` or `"invalid"`; and `data_available = "Yes"`.

Validation inventory and report state are reconciled to current
available acquisition `survey_id`/`Checksum` keys on every call,
including no-work calls. The normalized report must exactly cover
completed inventory IDs and exact full-row duplicates are removed.
Completed worker rows are assembled in memory, then `validation_report`
is verified first and `gmd_valid_inv` is committed last. Every uncertain
write is reloaded and compared with canonical prior and intended
content; unreadable or ambiguous durable state is not overwritten as
though it were trustworthy.

Persisted reports require character-compatible `table_name`, `message`,
`type`, `description`, `module_type`, `vermast`, `veralt`,
`country_code`, and `rf_year`. Optional columns may be added only when
same-name columns have identical coercion-relevant attributes, including
class, factor levels and ordering, units, and time zone. Exact rows are
deduplicated and canonical row order uses every persisted column. Engine
`type` values must be nonempty character values in `success`, `warning`,
or `error` and agree with the report. If the engine completes but its
report rows are unavailable, the survey records
`phase = "report_unavailable"`, persists no completed row or report row,
and is selected again on the next call.

Logging is unconditional. `dlw_validation_inf` entries include an
attempt boundary, lifecycle and failure entries, and an exact completion
summary that separates valid, invalid, and execution-failed surveys.

## Note

This function expects a working release to be configured via
[`pipfun::setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.html).
When called from
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md),
the release is already set. When called standalone, configure it first.
This exported function reads the default `"dlw_gmd_inv"`;
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
routes its `inv_gmd_list` value to the same validation implementation.
Invalid arguments and a missing working release escape as
caller/precondition errors. Runtime folder, artifact, schema, worker,
logging, and persistence failures return inspectable results; interrupts
are not converted.

## Examples

``` r
if (FALSE) { # \dontrun{
pipfun::setup_working_release("20260206", "TEST")
pipdata_validate_gmd()
} # }
```
