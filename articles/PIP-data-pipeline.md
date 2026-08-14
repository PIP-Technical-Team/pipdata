# PIP Data Pipeline: Orchestration Overview

This article explains how the PIP data pipeline is orchestrated end to
end: which functions run in which order, which package owns each step,
and how to run a full session. Code in this article is illustrative and
does not execute when the article is built — running the pipeline
requires a configured working release, network access to Datalibweb, and
write access to PIP storage.

For the internal mechanics of DLW acquisition and validation, see the
companion article [Validating
Data](https://pip-technical-team.github.io/pipdata/articles/Validating-Data.md);
for survey cleaning, deflation, and logging, see [Processing Data
functions](https://pip-technical-team.github.io/pipdata/articles/Processing-Data.md).

## The three wrapper functions

The pipeline runs as three sequential stages, each owned by a dedicated
wrapper function in a separate package and (typically) a different
developer:

| Order | Wrapper | Package | Developer scope | Purpose |
|:--:|----|----|----|----|
| 1 | `update_aux_measures()` | **pipaux** | Auxiliary data engineer | Refresh auxiliary data (CPI, PPP, population, GDP, PCE, PFW) |
| 2 | [`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md) | **pipdata** | Survey ingestion engineer | Download new Datalibweb (DLW) survey files, validate them, update the DLW inventory |
| 3 | [`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md) | **pipdata** | Survey cleaning engineer | Merge auxiliary data with each DLW survey, clean variables, attach metadata, save versioned outputs, update the PIP master inventory |

`update_aux_measures()` lives in the {pipaux} package and is not
detailed in this {pipdata} article beyond this table — it is a
prerequisite step that should run before Step 2 so that the latest
auxiliary data is available.

**Important**:
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
cleans and attaches metadata to survey data — it does **not** deflate
welfare. Deflation is a separate, currently manual step; see [Deflation
is a separate step](#deflation-is-a-separate-step) below.

## How to run: an end-to-end session

### 0. Configure the working release

Start by checking for package updates, then configure the working
release that all three wrappers, and the functions that consume their
output, read from:

``` r

# Check for updates to the PIP package ecosystem before running the pipeline
metapip::update_pip_packages()

release  <- "20260401"
identity <- "TEST"

pipfun::setup_working_release(release, identity, verbose = FALSE)
```

### 1. Refresh auxiliary data (pipaux)

``` r

# Owned by pipaux; see the pipaux package documentation for details.
pipaux::update_aux_measures()
```

### 2. Acquire and validate DLW data (pipdata)

``` r

pipdata::pipdata_dlw_process(
  inv_gmd_list      = "dlw_gmd_inv",
  get_dlw_data      = TRUE,
  validate_dlw_data = TRUE,
  log               = TRUE,
  save_log          = TRUE,
  check_missing     = TRUE,
  release           = release,
  identity          = identity
)
```

This downloads new/updated GMD survey files via
[`dlw::dlw_get_gmd()`](https://worldbank.github.io/dlw/reference/dlw_get_gmd.html)
and validates them, producing an updated validated inventory
(`gmd_valid_inv`) consumed by Step 3. See [Validating
Data](https://pip-technical-team.github.io/pipdata/articles/Validating-Data.md)
for the internal mechanics.

### 3. Clean surveys and build metadata (pipdata)

``` r

new_pip_inv <- pd_process_data()
```

[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
loads the validated inventory internally (via
[`pipload::load_gmd_valid_inv()`](https://pip-technical-team.github.io/pipload/reference/load_dlw_data.html))
when `inv` is not supplied, then iterates it, merges auxiliary measures,
cleans each survey, attaches metadata, saves versioned outputs, and
returns the updated PIP master inventory. See [Processing Data
functions](https://pip-technical-team.github.io/pipdata/articles/Processing-Data.md)
for the internal mechanics.

### 4. Build a consolidated log report

``` r

log_report(
  path      = file.path("log_reports", "log_report.md"),
  overwrite = TRUE
)
```

[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
loads the `"pipdata_log"` internally (via
`pipfun::log_filter(name = "pipdata_log")`) when `log` is not supplied,
and summarizes what
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
wrote to it — see [Logging and reporting
scope](#logging-and-reporting-scope) below for what it does and does not
cover.

## Architecture: why three wrappers?

Each stage is intentionally isolated because a different engineer
developed it independently, with its own auxiliary data, error handling,
and logging:

- **`update_aux_measures()`** (pipaux) manages the auxiliary-data
  lifecycle: dependency resolution, GitHub/Y-drive sync, and change
  detection.
- **[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)**
  (pipdata) is responsible only for getting raw survey data into a
  validated state — it does not clean or transform welfare data.
- **[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)**
  (pipdata) consumes the validated inventory and the refreshed auxiliary
  data, and is responsible for the cleaning/metadata transformation.
  Internally, it iterates one survey at a time, so that a per-survey
  failure is caught, logged, and skipped without aborting the rest of
  the run.

Each survey’s cleaning pass inside
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
follows:
[`inv_dlw_load()`](https://pip-technical-team.github.io/pipdata/reference/inv_dlw_load.md)
(load raw survey) -\>
[`pd_cpfw_merge()`](https://pip-technical-team.github.io/pipdata/reference/pd_cpfw_merge.md)
(merge Price Framework metadata) -\>
[`pd_dlw_clean()`](https://pip-technical-team.github.io/pipdata/reference/pd_dlw_clean.md)
(S3-dispatched cleaning) -\>
[`pd_aux_attr()`](https://pip-technical-team.github.io/pipdata/reference/pd_aux_attr.md)
(attach CPI/PPP/population/GDP/PCE metadata) -\>
[`save_pip_data()`](https://pip-technical-team.github.io/pipdata/reference/save_pip_data.md)
(write cleaned data and metadata to PIP storage). No deflation step is
part of this chain.

## Deflation is a separate step

[`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md)/[`deflation()`](https://pip-technical-team.github.io/pipdata/reference/deflation.md)
deflates a single cleaned survey’s welfare values using CPI, PPP, and
population auxiliary data. It is **not** called automatically by
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
— it must be run afterwards, per survey, once cleaned data exists in PIP
storage:

``` r

# Mode A: pass a cleaned survey directly (aux auto-loaded from the master
# inventory when cpi/ppp/pop are NULL)
dt <- pipload::pip_read(id = "BOL_2022_EH_INC_ALL", alias = "pip")
bol_deflated <- pd_deflation(dt)

# Mode B: load the survey by id and deflate in one call
bol_deflated <- pd_deflation(pip_id = "BOL_2022_EH_INC_ALL")
```

{pipload} also provides `load_pip_deflated_data()`, a convenience
wrapper around
[`pd_deflation()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflation.md)’s
Mode B: it locates a survey (by `id_name` or by filter arguments such as
`country_code`/`surveyid_year`/`module`), loads it, and deflates it in
one call — useful when you don’t already have the survey’s `pip_id` in
hand:

``` r

bol_deflated <- pipload::load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL")

# Or filter by country/year/module instead of a known id_name
bol_deflated <- pipload::load_pip_deflated_data(
  country_code  = "BOL",
  surveyid_year = 2022,
  module        = "ALL"
)
```

See [Processing Data
functions](https://pip-technical-team.github.io/pipdata/articles/Processing-Data.md)
for details on both modes.

## Logging and reporting scope

[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
only parses the `"pipdata_log"` entries written by
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)/[`process_data()`](https://pip-technical-team.github.io/pipdata/reference/process_data.md)
(processing summary, per-survey failures). It does **not** consume any
log produced by
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
(DLW acquisition/validation uses its own `log`/ `save_log` arguments and
does not currently feed into
[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)).
Keep this scope in mind when reading a generated report — a clean report
does not imply the DLW acquisition/validation step had no issues.

The goal going forward is for
[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
to summarize a single, harmonized log covering all three wrappers
(`update_aux_measures()`,
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md),
and
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)),
so one report reflects the health of the entire pipeline run rather than
just the cleaning step. This is tracked on the roadmap (see the
`unified-logging-report` idea).
