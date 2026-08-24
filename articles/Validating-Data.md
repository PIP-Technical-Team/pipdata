# Validating Data: DLW Acquisition and Validation Internals

This article explains the internal mechanics of
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
— the second of the three pipeline wrappers, responsible for downloading
new survey files from Datalibweb (DLW) and validating them before they
are handed off to survey cleaning. Code in this article is illustrative
and does not execute when the article is built — running it requires a
configured working release and network access to Datalibweb.

For the end-to-end orchestration and how this wrapper fits with the
other two, see [PIP Data Pipeline: Orchestration
Overview](https://pip-technical-team.github.io/pipdata/articles/PIP-data-pipeline.md).
For what happens after validation (survey cleaning, deflation, logging),
see [Processing Data
functions](https://pip-technical-team.github.io/pipdata/articles/Processing-Data.md).

## What `pipdata_dlw_process()` does

[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
is a thin orchestrator over two delegate functions, run in order:

| Order | Function | Purpose |
|:--:|----|----|
| 1 | [`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md) | Download new/updated GMD survey files from Datalibweb |
| 2 | [`pipdata_validate_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_validate_gmd.md) | Validate the downloaded datasets and update the validated inventory |

Before either delegate runs,
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
ensures a working release is configured and that a local GMD catalog
list exists (downloading it interactively via
[`dlw_gmd_list()`](https://pip-technical-team.github.io/pipdata/reference/dlw_gmd_list.md)
if missing).

``` r

pipdata::pipdata_dlw_process(
  inv_gmd_list      = "dlw_gmd_inv",
  get_dlw_data      = TRUE,
  validate_dlw_data = TRUE,
  check_missing     = TRUE,
  release           = "20260401",
  identity          = "TEST"
)
```

Both `get_dlw_data` and `validate_dlw_data` can be set to `FALSE`
independently — for example, to re-run only validation without
re-downloading data.

## Step 1: Acquiring new datasets — `pipdata_get_gmd()`

[`pipdata_get_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_get_gmd.md)
identifies datasets that are new or changed in the GMD catalog and
downloads them:

1.  **Identify new datasets** —
    `dlw_gmd_new(check_missing, update_inventory = TRUE)` compares the
    local inventory against the GMD catalog and returns the rows that
    need downloading. If nothing is new, the function records a typed
    no-new-data entry and returns invisibly.
2.  **Filter by module** — only `"ALL"`, `"GROUP"`, `"HIST"`, `"GPWG"`,
    and `"BIN"` modules are downloaded.
3.  **Download per survey** — for each row,
    [`dlw::dlw_get_gmd()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.html)
    downloads the file (identified by country, year, survey acronym,
    module, and `vermast`/`veralt` version markers) to the local DLW
    data folder. Each download is wrapped in
    [`tryCatch()`](https://rdrr.io/r/base/conditions.html): failures are
    logged automatically with typed `logmeta` and the row is marked
    `data_available = "No"` rather than aborting the whole run; a
    success is marked `"Yes"`.

``` r

pipdata::pipdata_get_gmd(
  inv_gmd_list  = "dlw_gmd_inv",
  check_missing = TRUE
)
```

## Step 2: Validating downloaded datasets — `pipdata_validate_gmd()`

[`pipdata_validate_gmd()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_validate_gmd.md)
runs after acquisition and validates the local datasets that have not
yet been validated:

1.  **Identify unvalidated local datasets** —
    [`dlw_gmd_unvalidated()`](https://pip-technical-team.github.io/pipdata/reference/dlw_gmd_unvalidated.md)
    scans the local DLW data folder for datasets not yet recorded in the
    validated inventory.
2.  **Load the existing validated inventory** — via
    [`pipload::load_gmd_valid_inv()`](https://pip-technical-team.github.io/pipload/reference/load_dlw_data.html)
    (`gmd_valid_inv`), if one already exists.
3.  **Diff against the existing inventory** —
    [`gmd_to_validate()`](https://pip-technical-team.github.io/pipdata/reference/gmd_to_validate.md)
    identifies datasets that still need validation;
    [`gmd_validated()`](https://pip-technical-team.github.io/pipdata/reference/gmd_validated.md)
    identifies datasets already validated in a prior run.
4.  **Dispatch validation by module** — each dataset is validated with a
    module-specific function:
    [`dlw_validation_gpwg()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation.md),
    [`dlw_validation_group()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation.md),
    [`dlw_validation_bin()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation.md),
    [`dlw_validation_hist()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation.md),
    [`dlw_validation_all()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation.md),
    [`dlw_validation_aspire()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation.md),
    [`dlw_validation_l()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation.md),
    or
    [`dlw_validation_skip()`](https://pip-technical-team.github.io/pipdata/reference/dlw_validation.md)
    as the default for unrecognized modules.
5.  **Write the updated validated inventory** — the result becomes the
    new `gmd_valid_inv`, which
    [`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
    consumes as its starting inventory (see [Processing Data
    functions](https://pip-technical-team.github.io/pipdata/articles/Processing-Data.md)).

``` r

pipdata::pipdata_validate_gmd()
```

## Logging scope

Both delegates write unconditional typed entries to the shared
`"pipdata_log"` log. Acquisition uses `dlw_acquisition_inf`; validation
uses `dlw_validation_inf`. The enclosing
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
wrapper writes a `dlw_summary_inf` stage marker and saves a DLW
checkpoint automatically.
[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
consumes these entries and presents DLW acquisition failures, validation
phases, and stage-aware warnings alongside the survey-cleaning sections.
