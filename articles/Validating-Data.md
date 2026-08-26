# Validating Data: DLW Acquisition and Validation Internals

This article explains the current supported DLW entry point,
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md),
which acquires survey files from Datalibweb (DLW) and validates them
before survey cleaning. Code is illustrative and does not execute when
the article is built; running it requires a configured working release
and network and storage access.

For the end-to-end orchestration and how this wrapper fits with the
other two, see [PIP Data Pipeline: Orchestration
Overview](https://pip-technical-team.github.io/pipdata/articles/PIP-data-pipeline.md).
For what happens after validation (survey cleaning, deflation, logging),
see [Processing Data
functions](https://pip-technical-team.github.io/pipdata/articles/Processing-Data.md).

## What `pipdata_dlw_process()` does

[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
is an explicit orchestrator over two independently callable stages, run
in order:

| Order | Function | Purpose |
|:--:|----|----|
| 1 | [`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md) | Download new/updated GMD survey files from Datalibweb |
| 2 | [`pipdata_validate_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_validate_gmd.md) | Validate the downloaded datasets and update the validated inventory |

The same `inv_gmd_list` is routed through bootstrap, acquisition, and
validation, including when it is a custom artifact ID. The wrapper
configures the working release and records a summary and checkpoint
after the delegates. `release` and `identity` are required. Their formal
`NULL` defaults are explicit missing-value sentinels so an omitted value
aborts before release setup; `identity` must be `"PROD"`, `"INT"`, or
`"TEST"`.

``` r

dlw_result <- pipdata::pipdata_dlw_process(
  inv_gmd_list      = "dlw_gmd_inv",
  get_dlw_data      = TRUE,
  validate_dlw_data = TRUE,
  check_missing     = TRUE,
  release           = "20260401",
  identity          = "TEST"
)
```

Both stage flags can be set independently. A validate-only call never
displays a menu; a missing acquisition inventory is returned as a failed
validation result. A noninteractive call that requests acquisition also
returns a failed acquisition result rather than prompting. Only an
interactive acquisition call can offer the Download/Abort menu.

This wrapper remains the supported DLW lifecycle entry point. A broader
`run_pipeline()` orchestrator is future direction, not a current
function to call.

## Returned results and errors

Normal unassigned calls remain quiet, but all three DLW functions now
return plain, unclassed lists invisibly. Acquisition and validation
results have these names in this order:

``` r

c("stage", "outcome", "inventory", "summary", "failures", "artifacts")
```

Their outcomes have precise meanings:

| Outcome | Meaning |
|----|----|
| `success` | At least one worker completed, required commits were verified, and no execution/workflow failure occurred |
| `partial` | Useful work completed and required commits were verified, but a worker or non-commit workflow failure occurred |
| `failed` | A required commit was not verified, discovery failed, or no worker completed while failures occurred |
| `no_work` | Trustworthy discovery selected no workers |

The returned `inventory` is trustworthy durable state (or `NULL` when
absent or unknown). `failures` is a compact table with `survey_id`,
`phase`, `error_type`, and `condition_msg`; it does not retain R
conditions or survey data. Artifact facts expose whether each write was
attempted, successful, trustworthy, skipped, or reconciled and include
its version ID.

For artifact facts, `success = NA` means no write was needed or
permitted and is verified when `trustworthy = TRUE`. In contrast,
`success = FALSE` means a required write was not reached or was not
verified and cannot satisfy a successful stage outcome. This distinction
lets a retry succeed when its report is already durably current and only
the inventory commit needs recovery.

The wrapper result has `stage`, `outcome`, `acquisition`, `validation`,
`failures`, and `checkpoint`. A disabled or dependency-blocked nested
stage has `outcome = "not_run"` and `summary$reason` of `"disabled"` or
`"dependency_failed"`. Summary-log and checkpoint diagnostics are
retained in `checkpoint`; checkpoint failure does not change the
business outcome.

Invalid arguments, working-release setup/precondition errors, explicit
user cancellation, and interrupts still escape. Runtime folder, catalog,
artifact, worker, logging, and persistence errors after setup return
inspectable failed or partial results. Callers that need conditional
continuation should assign and inspect the result.

## Step 1: Acquiring new datasets — `pipdata_get_gmd()`

[`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md)
identifies current datasets that are new, changed, or eligible for retry
and downloads them:

1.  **Load and validate current state**: both the local inventory and
    server catalog must have one checksum for each normalized filename.
2.  **Select active download modules**: acquisition downloads only
    `"ALL"`, `"GROUP"`, `"HIST"`, `"GPWG"`, and `"BIN"`. `"ASPIRE"` and
    `"L"` remain recognized catalog/validation modules but are not newly
    downloaded here.
3.  **Force the selected file**: each worker passes the exact catalog
    `FileName` and `local_overwrite = TRUE` to
    [`dlw::dlw_get_gmd()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.html).
    Cached-only or ambiguous multi-file responses are failures.
4.  **Isolate failures**: one failed survey does not stop siblings.
    Successful rows become `data_available = "Yes"`; failures remain
    `"No"`.
5.  **Reconcile the full inventory**: current server rows replace stale
    rows and checksums, catalog-deleted rows are removed, and prior
    successful `"ASPIRE"`/`"L"` rows survive only while current. This
    happens even when no worker is selected.

With `check_missing = TRUE`, unresolved current five-module rows are
selected again because they remain `"No"`. The retry model is driven by
durable inventory state. A completed attempt writes the intended
inventory once. If a write throws or returns an invalid result, the code
reloads active storage and compares canonical intended and prior
content; it never assumes rollback.

The exact acquisition inventory schema is:

| Column | Persisted rule |
|----|----|
| `Country` | Nonempty character |
| `Year` | Nonmissing whole-number integer |
| `Survey_acronym`, `Vermast`, `Veralt`, `Collection` | Nonempty character |
| `Module` | `GPWG`, `GROUP`, `BIN`, `HIST`, `ALL`, `ASPIRE`, or `L` |
| `FileName` | Nonempty `.dta` filename, unique after basename/case normalization |
| `Checksum` | Nonempty character; exactly one per normalized filename |
| `Ext` | Character `dta` |
| `data_available` | Character `Yes` or `No` |

Additional server columns are retained in deterministic name order. A
normalized zero-row server catalog is a `catalog_load` failure and
cannot erase durable acquisition state.

``` r

acquisition_result <- pipdata::pipdata_get_gmd(
  inv_gmd_list  = "dlw_gmd_inv",
  check_missing = TRUE
)
```

## Step 2: Validating downloaded datasets — `pipdata_validate_gmd()`

[`pipdata_validate_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_validate_gmd.md)
validates current available acquisition keys that are absent from
completed validation state:

1.  **Reconcile authoritative keys**: current `data_available = "Yes"`
    acquisition `survey_id`/`Checksum` pairs determine retained
    validation rows. Deleted, superseded, and unavailable acquisition
    keys are pruned, including on a zero-worker call.
2.  **Map all validation modules**: the one-survey worker calls
    [`dlw_validation_engine()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation_engine.md)
    with mappings for `"ALL"`, `"GROUP"`, `"HIST"`, `"GPWG"`, `"BIN"`,
    `"ASPIRE"`, and `"L"` (or the skip mapping for an unrecognized
    module).
3.  **Separate classification from execution**: an engine-completed
    result is `valid` or `invalid`. Load, artifact-info, engine, or
    inventory-row errors are execution failures. Only valid/invalid rows
    enter `gmd_valid_inv`; failed IDs have no completed row and are
    selected again on the next call.
4.  **Preserve version history**: the next successful `pipeline_version`
    is `1L` or one plus that survey’s maximum across persisted inventory
    history, calculated before stale-checksum pruning. Failed retries
    consume no version.
5.  **Keep report and inventory consistent**:
    `validation_report$table_name` must exactly cover completed
    inventory IDs. Orphan rows are removed, missing diagnostics block
    the run, and exact normalized report rows are deduplicated.
6.  **Commit report first**: completed worker output is assembled in
    memory; `validation_report` is verified before `gmd_valid_inv` is
    written. Every uncertain write is reconciled against reloaded
    durable content.

Engine results must contain at least one row and nonmissing, nonempty
character `type` values in `success`, `warning`, or `error`; those
values must agree with the extracted report rows. A missing report after
engine completion is `report_unavailable`: no inventory or report row is
persisted for that survey, so the same acquisition key retries
automatically on the next call.

The exact completed validation inventory schema is:

| Column | Persisted rule |
|----|----|
| `survey_id` | Nonempty character; unique completed key |
| `pipeline_version` | Positive integer |
| `latest_version_id`, `content_hash`, `file_path`, `Checksum` | Nonempty character |
| `status` | Character `valid` or `invalid` |
| `data_available` | Character `Yes` |
| `date_validated` | Nonmissing Date/POSIX time normalized to UTC |
| `country_code`, `survey_acronym`, `vermast`, `veralt` | Nonempty parsed character identity |
| `surveyid_year` | Nonnegative integer |
| `collection`, `module`, `tool` | Nonempty parsed character identity |

The exact required report columns are `table_name`, `message`, `type`,
`description`, `module_type`, `vermast`, `veralt`, `country_code`, and
`rf_year`, normalized to character. Optional columns are additive only
when same-name columns have matching type and coercion-relevant
attributes, including class, factor levels/order, units, and time zone.
Additive raw columns are rejected because raw vectors cannot represent
typed missing values. Canonical row ordering compares the four primary
keys (`table_name`, `type`, `message`, `description`) and then every
remaining persisted column.

``` r

validation_result <- pipdata::pipdata_validate_gmd()
```

The exported validator uses `"dlw_gmd_inv"`. The wrapper uses the same
internal validation path with its requested custom inventory ID.

`gmd_valid_inv` is a completed-data inventory, not a retry ledger.
Downstream cleaning continues to admit completed `valid` and `invalid`
rows under existing policy, while entry guards remove recognized legacy
blank/`"No"` execution control rows before cleaning and dependency
planning.

## Logging scope

Both stages write unconditional typed entries to `"pipdata_log"`.
Acquisition and validation each emit an attempt boundary and exact
completion metadata; validation counts valid, invalid, and
execution-failed surveys separately. The wrapper derives its aggregate
summary from returned stage facts rather than parsing the log and
attempts the existing DLW checkpoint afterwards.

[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
independently selects each stage’s latest attempt segment and prefers
its exact completion entry. Older logs use fallback only within the
selected segment. DLW discriminators appear only in dedicated DLW
sections and are excluded from generic type and country tables,
preventing stale-attempt leakage and double counting.
