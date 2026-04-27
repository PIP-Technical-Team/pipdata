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
  These strings are used as string literals across multiple files; any typo silently breaks the corresponding report section.

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
