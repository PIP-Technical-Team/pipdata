# Project Context

Additional context for Copilot and the Compound GPID plugin. Edit freely —
this file is committed to git and shared with the team.

## Pipeline Architecture

The **PIP (Poverty and Inequality Platform)** data ingestion pipeline transforms raw survey microdata and grouped distributions from Datalibweb (DLW) into cleaned, versioned, analysis-ready datasets. The pipeline runs in three sequential stages:

1. **Auxiliary Data Refresh** (`update_aux_measures()` in pipaux) — Refresh CPI, PPP, population, GDP, PCE, PFW, etc. from GitHub and Y-drive
2. **DLW Data Acquisition & Validation** (`pipdata_dlw_process()` in pipdata) — Download GMD survey files and validate them
3. **Survey Cleaning & Metadata** (`pd_process_data()` in pipdata) — Merge auxiliary data with DLW surveys, clean variables, create metadata, save versioned outputs, update master inventory

For detailed technical walkthrough, see `docs/pipeline_overview.qmd`.

## Data Sources

- **Datalibweb (DLW)**: Source of raw household survey microdata and grouped distributions; accessed via `dlw` package
- **GitHub repositories**: Auxiliary data (CPI, PPP, population, GDP, PCE, PFW) sourced via pipaux dependency manifests
- **Y-drive storage**: Canonical locations for DLW inventory, PIP cleaned data, auxiliary metadata, and versioned artifacts (managed via pipload and stamp)
- **PIP Master Inventory**: Versioned catalogue of all cleaned surveys with deflation parameters and metadata attributes

## Domain Rules

- All processing is versioned via release dates (e.g., "20260401")
- Survey identity types (e.g., "TEST") control folder isolation during pipeline runs
- Auxiliary data refresh must complete before survey cleaning begins (strict ordering)
- All pipeline steps are logged via `pipfun::log_add()` and `pipfun::log_info()` into a unified `piplog` object
- Error handling uses custom `piperr` conditions for graceful recovery without silencing failures
- `dplyr`, `tidyr`, and `tibble` are **not** in `DESCRIPTION Imports` — use `data.table` (`:=`, `rbindlist`, `[, .N, by]`) and `collapse` (`fcase`, `ftransform`, `fmutate`) instead. Do not add new dplyr/tidyr/tibble calls anywhere. `dlw_scan_and_validate.R` still has ~19 legacy dplyr calls (Phase 2 migration, tracked in roadmap as `dplyr-to-collapse-phase2`).
- Always qualify `fcase()` and `fifelse()` with `data.table::` (i.e. `data.table::fcase(...)`) even when called inside `collapse::ftransform()` or `collapse::fmutate()`. This makes the dependency surface explicit and avoids ambiguity with similarly-named collapse functions.
- The pipeline emits four canonical logmeta entry types, parsed by `log_report()` to build report sections. Their `info`/`error` field values are:
  - `"process_summary_inf"` — emitted by `pd_process_data()`
  - `"aux_changes_inf"` — emitted by `valid_dlw_load()` when auxiliary files change
  - `"null_svys_inf"` — emitted by `update_pip_inventory()` when surveys fail (NULL)
  - `"inv_update_inf"` — emitted by `update_pip_inventory()` for inventory verification (info if all confirmed, error if any missing)
  - `"release_write_err"` — emitted by `update_pip_inventory()` when the release inventory write fails (error-level; includes `condition_msg`)
  These strings are used as string literals across multiple files; any typo silently breaks the corresponding report section. (Once `unified-logging-report` is implemented, three additional DLW types will be added: `"dlw_download_inf"`, `"dlw_validation_inf"`, `"dlw_summary_inf"`.)
- **`logmeta$error` and `logmeta$info` are always string type discriminators** — never R condition objects. Caught condition messages go in `logmeta$condition_msg = conditionMessage(e)`. The old pattern `logmeta = list(error = e)` (passing the condition object directly) is incorrect and will break `parse_log_meta()` which uses `vapply(..., character(1))`.
- **Logging inside `tryCatch` error handlers and `lapply` callbacks**: `capture_log_args()` in pipfun resolves to the anonymous handler's frame (`function(e)`), so `args` will contain `list(e = <condition>)` rather than the enclosing function's context. This is expected and harmless — put all structured context in `logmeta` instead of relying on `args` auto-capture. See `.cg-docs/solutions/testing-patterns/2026-04-29-logging-in-trycatch-handlers.md`.
- **Archiving R files to `old_files/`**: When moving files to `old_files/`, always: (1) check `R/` for active callers of functions defined in the archived file — if still needed, move them to the calling file as `@noRd` internal; (2) move the corresponding `tests/testthat/test-<stem>.R` alongside; (3) delete `man/<fun>.Rd` files manually (`devtools::document()` does not remove stale `.Rd` files); (4) wrap any `@examples` in other files that referenced the archived functions in `\dontrun{}` — bare calls to those symbols are now undefined and will fail R CMD check. Note: `:::` calls in examples suppress R CMD check static analysis, so replacing them with bare calls re-exposes the symbol to the checker. See `.cg-docs/solutions/build-errors/2026-04-30-r-package-file-archival-checklist.md`.
- **stamp versioning: `version_id` ≠ `content_hash`**: The master inventory stores `content_hash_metadata` (stamp's `content_hash` field). This is **not** the `version_id` that `pip_read(version=)` expects — passing `content_hash` directly always fails. Always resolve via `pipload::pip_read(id, alias="pip_meta", version="available")` and match `content_hash == content_hash_metadata` to obtain the correct `version_id`. When the match is empty (artifact replaced by a newer pipeline run), warn and fall back to `vintage == 0` (most recent). Never use `stamp::st_versions(raw_unc_path)` — it resolves against a different registry than `pip_read`. See `.cg-docs/solutions/bugs/2026-05-05-stamp-version-id-vs-content-hash.md`.
- **`*_data_level` values are attributes only — never columns**: `ppp_data_level`, `cpi_data_level`, and `pop_data_level` are scalar attributes on `pipmd`/`pipgd` objects. They are never materialised as columns in the data.table. `add_ppp()`, `add_cpi()`, and `add_rep_lvl()` all read them via `attr(dt, "ppp_data_level")` etc. — do not add these as columns to `dt` before calling deflation functions. `restore_data_level_cols()` has been removed. All survey attributes (`survey_id`, `country_code`, etc.) are always plain scalars — never wrapped as `list(values=X)`. See `.cg-docs/solutions/bugs/2026-05-06-attribute-list-values-wrapper-pipeline-vs-stamp-path.md`.

## Work in Progress

Currently tracked via `.cg-docs/plans/`:
- **Active**: Code Quality & Refactoring milestone — standardizing logging patterns, migrating dplyr to collapse/data.table, decomposing large functions, and expanding test coverage. See `roadmap.json` for individual feature tracking.

## Workspace Notes

- **pipaux**: Auxiliary data package (CPI, PPP, population refresh)
- **pipfun**: Core functions and utilities package (release management, folder paths, logging)
- **pipload**: Data loading and integration package (I/O, inventory management)
- **wbpip**: World Bank PIP package (microdata and grouped-data cleaning methods)
- **dlw**: DLW API client (authentication, survey download)
- **stamp**: Versioned artifact storage with metadata and pluggable serialisation

See `docs/pipeline_overview.qmd` for full technical documentation.
