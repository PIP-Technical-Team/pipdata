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

## Work in Progress

Currently tracked via `.cg-docs/plans/`:
- **Active**: Enrich log report with success metrics and auxiliary changes (2026-04-06, status: active)

## Workspace Notes

- **pipaux**: Auxiliary data package (CPI, PPP, population refresh)
- **pipfun**: Core functions and utilities package (release management, folder paths, logging)
- **pipload**: Data loading and integration package (I/O, inventory management)
- **wbpip**: World Bank PIP package (microdata and grouped-data cleaning methods)
- **dlw**: DLW API client (authentication, survey download)
- **stamp**: Versioned artifact storage with metadata and pluggable serialisation

See `docs/pipeline_overview.qmd` for full technical documentation.
