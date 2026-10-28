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

## Current pipeline entry points

The pipeline lifecycle uses three explicit entry points:

| Order | Wrapper | Package | Developer scope | Purpose |
|:--:|----|----|----|----|
| 1 | `update_aux_measures()` | **pipaux** | Auxiliary data engineer | Refresh auxiliary data (CPI, PPP, population, GDP, PCE, PFW) |
| 2 | [`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md) | **pipdata** | Survey ingestion engineer | Download new Datalibweb (DLW) survey files, validate them, update the DLW inventory |
| 3 | [`pd_run_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_run_pipeline.md) | **pipdata** | Survey processing engineer | Incrementally run clean, metadata, and deflate from completed validation state |

`update_aux_measures()` lives in the {pipaux} package and is not
detailed in this {pipdata} article beyond this table — it is a
prerequisite step that should run before Step 2 so that the latest
auxiliary data is available.

[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
is the supported DLW acquisition/validation entry point.
[`pd_run_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_run_pipeline.md)
is the top-level incremental entry point for the existing durable
processing stages. The older
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)
and
[`pd_deflate_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_deflate_pipeline.md)
wrappers remain independently callable and keep their master-inventory
returns.

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

dlw_result <- pipdata::pipdata_dlw_process(
  inv_gmd_list      = "dlw_gmd_inv",
  get_dlw_data      = TRUE,
  validate_dlw_data = TRUE,
  check_missing     = TRUE,
  release           = release,
  identity          = identity
)
```

The wrapper requires explicit nonempty `release` and `identity` values.
Their formal `NULL` defaults are missing-required-value sentinels, not
operational defaults; `identity` is restricted to `"PROD"`, `"INT"`, or
`"TEST"`.

This acquires five active modules and validates all seven recognized
module mappings, producing completed validation inventory
(`gmd_valid_inv`) for Step 3. The assigned result is a plain aggregate.
Inspect `dlw_result$outcome` (`success`, `partial`, `failed`, or
`no_work`) before deciding whether a script should continue. Disabled or
dependency-blocked nested stages use `not_run`.

Runtime stage failures after setup are represented in nested compact
failure tables and can yield partial or failed results. Invalid
arguments, release setup/precondition errors, explicit cancellation, and
interrupts still escape. Under automation a missing acquisition
inventory returns failure rather than opening a menu; validate-only
execution never prompts. See [Validating
Data](https://pip-technical-team.github.io/pipdata/articles/Validating-Data.md)
for the exact result and persistence contracts.

### 3. Run clean, metadata, and deflate incrementally (pipdata)

``` r

pipeline_result <- pd_run_pipeline(verbose = FALSE)
```

[`pd_run_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_run_pipeline.md)
loads the completed validation inventory internally (via
[`pipload::load_gmd_valid_inv()`](https://pip-technical-team.github.io/pipload/reference/load_dlw_data.html))
when `inv` is not supplied. It accepts current `valid` and `invalid`
available rows under existing cleaning policy, filters recognized legacy
blank/unavailable retry rows before planning, then runs only stale or
forced `clean:<survey_id>`, `metadata:<pip_id>`, and `deflate:<pip_id>`
nodes. It returns a compact `pipdata_pipeline_result`; it does not
return the master inventory. See [Processing Data
functions](https://pip-technical-team.github.io/pipdata/articles/Processing-Data.md)
for the internal mechanics.

Current nodes are cached. Stale or forced nodes are runnable. Failed
prerequisites make descendants blocked/skipped. `force_surveys` adds
selected survey or PIP chains without suppressing independent staleness.
Unknown legacy provenance requires explicit `bootstrap = TRUE`; use
`bootstrap_entities` for a restrictive canary.

The C2 manifest and exact Stamp receipts are the currentness authority.
A Colombia 2018 CPI change runs only matching Colombia 2018 metadata and
deflate nodes, while another Colombia year and unrelated country/year
remain cached. There is no persisted run cursor. After failure, call
[`pd_run_pipeline()`](https://pip-technical-team.github.io/pipdata/reference/pd_run_pipeline.md)
again; it replans from the last valid manifest and exact Stamp facts.

### 4. Build a consolidated log report

``` r

log_report(
  path      = file.path("log_reports", "log_report.md"),
  overwrite = TRUE
)
```

[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
loads `"pipdata_log"` internally and summarizes DLW acquisition, DLW
validation, survey cleaning, deflation, and structured failures. DLW
stage sections use each latest attempt and exact completion metadata,
with confined fallback for older logs. DLW discriminators are excluded
from generic type and country tables to avoid double counting.

## Architecture and compatibility

### Opt-in M2 recipe authority (TEST only)

`pd_run_pipeline(recipes = ...)` is a separate, bounded route. The
default `recipes = NULL` retains the staged pipeline above and its
existing return. The opt-in route does not call its selectors or C2
currentness manifest. It passes complete requests and all supplied
outputs to public stamp planning and rebuild APIs; stamp alone decides
run, skip and blocked work and revalidates at execution time. No
production stage adapters are activated.

``` r

# Illustrative only: choose an already approved, disposable E fixture root.
root <- "E:/approved-m2-fixture"
stamp::st_init(root, alias = "m2-fixture")
recipes <- list(
  context = list(release = "m2-example", identity = "TEST", root = root),
  nodes = list(list(
    path = "value.rds", parents = list(),
    calculation = function(path, parents) list(x = value),
    helpers = list(), constants = list(value = 42L),
    semantic_schema = NULL, semantic = list(), format = "rds"
  ))
)
feedback <- pd_run_pipeline(recipes = recipes, verbose = FALSE)
```

Each node requires exactly the fields shown; optional `audit` is
descriptive only and optional `review` names a governing failure
`outcome_id`. Parents are an unnamed list of
`list(path = ..., version_id = ...)`. Each pin is an exact nonempty
character ID, never a numeric position or `"latest"`/`"current"`
selector. Only a selected upstream recipe may use `version_id = NULL`;
stamp resolves it within the selected first-build graph. Multiple roots
and diamonds are supported; cycles are rejected. `nodes = list()`
returns the typed empty feedback without acquiring a lease. Nondefault
force/bootstrap/checkpoint controls and `inv` cannot be combined with
recipes. Formats are exactly `rds`, `qs2`, and `fst`; the callback
cannot override the requested format.

The callback has exactly `(path, parents)` formals, consumes supplied
exact pins, and returns exactly one named `x`, optionally `metadata`,
`format`, `code`, and `code_label`. A named NULL value is different from
omitting `x`; physical formats must support the value. Declared semantic
values are supplied as metadata and any callback-supplied semantic
fields must agree, preserving types and missing, NULL and NA
distinctions.

Pure calculation code, named ordinary-R helper closures and disjoint
named immutable constants are copied into a locked environment with a
base parent. SHA-256 component descriptors cover the actual copied
bodies/formals and values, not a manually supplied digest or
package-version label. Reordering declarations does not invalidate;
helper/constant/body edits do. An audit-only edit does not. This
deliberately narrow trusted callback contract rejects mutable captures,
missing constant values, primitive components, hidden dependencies,
dynamic lookup/dispatch and direct side effects. It is not a sandbox or
support for arbitrary legacy closures, and does not discover undeclared
dependencies. Guarded reads require the effective declared parent
path/pin pair and explicit format. Ambient locale/timezone conversions
and unsupported S3 generics are rejected; guarded base operations reject
classed operands rather than silently dispatching an unfingerprinted
method. Explicitly unclass reviewed inputs before operations. The
internal read/base/closure policy code is itself SHA-256 bound. Nested
classed list elements are rejected too, including implicit method
dispatch inside [`lengths()`](https://rdrr.io/r/base/lengths.html).
Consumed producer exports and explicit arguments are checked before
recipe preparation, lease or rebuild; a version label alone is
insufficient. Nonempty recipes require the existing suggested package
`codetools`. When it is unavailable, `pipdata_engine_dependency_error`
gives an actionable approved-library requirement before proof or
write-side work. No package is installed automatically and fingerprint
proof is never bypassed. Availability-seam tests do not certify a
dependency-minimal installation.

Feedback is one row per selected artifact: `release_id`, canonical
`path`, stamp `preview_decision`, actual `status`/`reason`,
`version_id`, `request_id`, `receipt_id`, `outcome_id`, `failure_class`,
`governing_failure_id`, `message`, and actual `attempted`. Missing proof
IDs are typed NA, not fabricated successes. Exact returned receipt IDs
and same-pin `pipload::pip_read(metadata_only = TRUE)` must validate
before successful feedback is returned. Retained-output work can return
`unchanged` with a new receipt and the same output pin; an unchanged
downstream artifact can consequently remain skipped.

Deterministic and unknown failures block descendants while independent
nodes can continue. External failures are retried only by another
explicit call. Signal classified calculation failures with
`rlang::abort(..., class = "stamp_deterministic_failure")` or
`rlang::abort(..., class = "stamp_external_failure")`; unclassified
errors are unknown failures. A review supplies
`list(outcome_id = feedback$governing_failure_id[[i]])` on the relevant
node after an actual parent/component change. The failure ID is
recovered from committed exact outcome history, not a process-local
cache or error text; it remains usable after repeated blocked calls and
a fresh process.

A dedicated lease under the selected root is scoped to
release/TEST/root, never to a code fingerprint, and does not publish a
legacy currentness manifest. Frozen proof, root, options and lease
ownership are checked on every callback entry and exit, including
errors. A failed integrity guard stops later callback attempts and
raises `pipdata_engine_integrity_error` with actual `feedback` and
original/secondary `causes`; cancellation still propagates as
cancellation.

Verification for this slice is deliberately partial: only three selected
test files and a built/installed public-API smoke run in an E-only
disposable runtime. The ordinary test suite, production
DLW/survey/deflation workflows, production SMB semantics, full
concurrency and umbrella milestone closure are not certified.

### Existing staged architecture

Each stage is intentionally isolated because a different engineer
developed it independently, with its own auxiliary data, error handling,
and logging:

- **`update_aux_measures()`** (pipaux) manages the auxiliary-data
  lifecycle: dependency resolution, GitHub/Y-drive sync, and change
  detection.
- **[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)**
  (pipdata) is responsible only for getting raw survey data into a
  validated state. It can continue validation after an acquisition
  failure when a trustworthy durable inventory remains, routes a custom
  inventory ID through both stages, and records summary/checkpoint
  facts. It does not clean or transform welfare data.
- **[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md)**
  (pipdata) consumes the validated inventory and the refreshed auxiliary
  data, and is responsible for the cleaning/metadata transformation.
  Internally, it iterates one survey at a time, so that a per-survey
  failure is caught, logged, and skipped without aborting the rest of
  the run.

The top-level executor composes these existing stage cores. Only clean,
metadata, and deflate are durable cached nodes. Load, PFW merge, recode,
auxiliary attachment, and save remain internal fingerprint components.

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

## Production activation block

Local tests do not activate production. Production remains blocked until
the signed target Windows/SMB fencing and immutable unique-rename
evidence are complete.

## Logging and reporting scope

[`log_report()`](https://pip-technical-team.github.io/pipdata/reference/log_report.md)
summarizes the shared `"pipdata_log"` entries from
[`pipdata_dlw_process()`](https://pip-technical-team.github.io/pipdata/reference/pipdata_dlw_process.md)
and
[`pd_process_data()`](https://pip-technical-team.github.io/pipdata/reference/pd_process_data.md).
It adds stage-aware warnings for DLW-only, pipeline-only, incomplete,
and DLW no-op runs. Acquisition and validation are segmented
independently at their latest attempt boundaries; dedicated DLW sections
own those records. Auxiliary refresh logging from `pipaux` remains a
separate follow-on scope.
