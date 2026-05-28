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
  - `"missing_metadata_err"` — emitted by `update_pip_inventory()` when pip_ids have no corresponding metadata entry after the version join; those pip_ids are excluded from the inventory (error-level; includes `pip_ids` and `surveys` arrays)
  These strings are used as string literals across multiple files; any typo silently breaks the corresponding report section. (Once `unified-logging-report` is implemented, three additional DLW types will be added: `"dlw_download_inf"`, `"dlw_validation_inf"`, `"dlw_summary_inf"`.)
- **`logmeta$error` and `logmeta$info` are always string type discriminators** — never R condition objects. Caught condition messages go in `logmeta$condition_msg = conditionMessage(e)`. The old pattern `logmeta = list(error = e)` (passing the condition object directly) is incorrect and will break `parse_log_meta()` which uses `vapply(..., character(1))`.
- **Logging inside `tryCatch` error handlers and `lapply` callbacks**: `capture_log_args()` in pipfun resolves to the anonymous handler's frame (`function(e)`), so `args` will contain `list(e = <condition>)` rather than the enclosing function's context. This is expected and harmless — put all structured context in `logmeta` instead of relying on `args` auto-capture. See `.cg-docs/solutions/testing-patterns/2026-04-29-logging-in-trycatch-handlers.md`.
- **Archiving R files to `old_files/`**: When moving files to `old_files/`, always: (1) check `R/` for active callers of functions defined in the archived file — if still needed, move them to the calling file as `@noRd` internal; (2) move the corresponding `tests/testthat/test-<stem>.R` alongside; (3) delete `man/<fun>.Rd` files manually (`devtools::document()` does not remove stale `.Rd` files); (4) wrap any `@examples` in other files that referenced the archived functions in `\dontrun{}` — bare calls to those symbols are now undefined and will fail R CMD check. Note: `:::` calls in examples suppress R CMD check static analysis, so replacing them with bare calls re-exposes the symbol to the checker. See `.cg-docs/solutions/build-errors/2026-04-30-r-package-file-archival-checklist.md`.
- **Legacy column migration in `build_pip_inventory()`**: When a column is removed from the inventory schema (e.g. `reporting_level` moved from persisted to on-the-fly enrichment), add it to the `legacy_cols` drop-list in Step 1 of `build_pip_inventory()`. The assembler strips these from `old_inv` before the upsert, migrating any on-disk master to the new schema on the next run. Pattern: `drop_cols <- intersect(legacy_cols, names(old_inv)); old_inv[, (drop_cols) := NULL]`. See `.cg-docs/solutions/data-quality/2026-05-27-legacy-column-persistence-in-on-disk-inventory.md`.
- **stamp versioning: `version_id` ≠ `content_hash`, and `ventry$version_id` ≠ `ventry$metadata$version_id`**: Two distinct traps. (1) `content_hash` and `version_id` are different stamp concepts — `pip_read(version=)` always expects a `version_id`; passing a `content_hash` directly always fails. (2) `pip_write()` / `stamp::st_save()` returns `list(version_id = <artifact_version>, metadata = list(version_id = <metadata_file_version>, ...), ...)`. The top-level `ventry$version_id` is the data artifact's version — this is what to store in the inventory and pass to `pip_read()`. The nested `ventry$metadata$version_id` is stamp's internal metadata-file version for the sidecar record — **not** the artifact version and not useful for loading. `format_vrs()` now captures `ventry$version_id` (top level) into `version_id_data` / `version_id_metadata` columns in the master inventory; `.load_deflation_aux()` reads these directly. The old hash-based `pip_read(version = "available")` lookup path has been removed. Never use `stamp::st_versions(raw_unc_path)` — it resolves against a different registry than `pip_read`. See `.cg-docs/solutions/bugs/2026-05-05-stamp-version-id-vs-content-hash.md` and `.cg-docs/solutions/bugs/2026-05-19-version-id-vs-metadata-version-id-in-format-vrs.md`.
- **`*_data_level` values are attributes only — never columns**: `ppp_data_level`, `cpi_data_level`, and `pop_data_level` are scalar attributes on `pipmd`/`pipgd` objects. They are never materialised as columns in the data.table. `add_ppp()`, `add_cpi()`, and `add_rep_lvl()` all read them via `attr(dt, "ppp_data_level")` etc. — do not add these as columns to `dt` before calling deflation functions. `restore_data_level_cols()` has been removed. All survey attributes (`survey_id`, `country_code`, etc.) are always plain scalars — never wrapped as `list(values=X)`. See `.cg-docs/solutions/bugs/2026-05-06-attribute-list-values-wrapper-pipeline-vs-stamp-path.md`.
- **`*_data_level = "area"` is a column pointer, not a level name**: When `ppp_data_level`, `cpi_data_level`, or any `*_data_level` attribute equals `"area"`, it means "resolve to the per-row values of the `area` column in the data.table." It is **not** a literal level name. Deflation helpers (`add_ppp()`, `add_cpi()`, `adjust_population()`) must check for this case (`if (identical(lvl, "area"))`) and use `dt$area` as per-row lookup keys into named CPI/PPP/pop vectors (whose keys are `"rural"`, `"urban"`, `"national"`). `"national"` is the only literal level value stored directly. Surveys with subnational PFW domains (reporting_level == 2, cpi_domain_var == "urban") always receive `*_data_level = "area"` from `add_dom_vars()`. See `.cg-docs/solutions/bugs/2026-05-06-subnational-deflation-area-attribute-not-resolved.md`.
- **Each deflation function branches on its own `*_data_level` attr — never use the integer `reporting_level` attribute as a shared branch discriminator**: `ppp_data_level`, `cpi_data_level`, and `pop_data_level` are set independently by `add_dom_vars()`. The mixed-domain case (`reporting_level == 2` for a subnational survey but e.g. `ppp_domain == 1` → `ppp_data_level = "national"`) means the same survey can have `ppp_data_level = "national"` and `cpi_data_level = "area"` simultaneously. `add_ppp()` must branch on `ppp_data_level`, `add_cpi()` on `cpi_data_level`, and `adjust_population()` guard must check `pop_data_level == "area"`. `add_rep_lvl()` was removed because it incorrectly conflated all `*_data_level` attrs into a single intermediary `reporting_level` column, which broadcast the literal `"area"` string causing NA lookups.
- **`pd_deflation()` output contract**: The deflated survey `data.table` always has `welfare_lcu` (LCU value) and `weight` as the first two columns, followed by `welfare_ppp_*` columns (newest base year first), then `area`, `ppp_*`, `cpi*`, and remaining survey columns. The original `welfare` column is dropped — `welfare_lcu` is the canonical pre-deflation variable. Three attributes are always set on the result: `welfare_vars` (character vector of all `welfare_*` column names, for discovery without grepping), `adj_pop` (logical; `TRUE` if `adjust_population()` was applied, always `FALSE` for `pipgd`), and `ppp_sort` (integer; the base year of the `welfare_ppp_*` column used for row sorting, e.g. `2017L`; `NULL` when no `welfare_ppp_*` columns are present). See `.cg-docs/solutions/data-quality/2026-05-07-deflation-output-contract.md`.
- **data.table `DT[i, ]` scoping: function arguments do NOT shadow columns**: Inside `DT[i, ]`, bare names resolve to **columns** before looking up in the parent frame. When a function argument shares a name with a column (e.g. `pip_id`), the expression `inv[inv$col == pip_id, ]` becomes a tautology — `pip_id` resolves to the column, so all rows match. Always break the name collision by renaming the local binding (`target_id <- pip_id`) or using the `..` prefix (`inv[col == ..pip_id, ]`). This is a silent bug — no error is thrown. See `.cg-docs/solutions/bugs/2026-05-19-datatable-scoping-column-vs-argument.md`.

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
