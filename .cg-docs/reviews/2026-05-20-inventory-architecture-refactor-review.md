---
plan: .cg-docs/plans/2026-05-20-inventory-architecture-refactor.md
date: 2026-05-27
depth: standard
findings:
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P3.1: skipped
  P3.2: fixed
---

## Review Report

**Review depth**: standard
**Files reviewed**: 6 (`R/build_pip_inventory.R`, `R/pd_process_data.R`, `R/pd_deflation.R`, `R/save_pip.R`, `R/aaa.R`, `tests/testthat/test-build_pip_inventory.R`)
**Findings**: 5 (P0: 0, P1: 0, P2: 3, P3: 2)

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-data-quality] `R/build_pip_inventory.R:~148` — `pip_id` format validation scans the **full catalog** before filtering to `target_ids`. Any historical non-standard artifact path in the `"pip"` or `"pip_meta"` alias (test artifacts, internal stamp housekeeping entries) triggers `cli_warn("build_pip_inventory_bad_pip_id_format")` on every run, even when those paths are unrelated to the current run's surveys.
  **Why**: The plan's stated "filter-first" design principle is violated here. The validation block runs on `nrow(cat_data)` rows (the entire alias catalog) but only `length(target_ids)` rows are relevant.
  **Fix**: Filter `cat_data` and `cat_meta` to `pip_id %in% target_ids` first, then run the format validation on the filtered sets only:
  ```r
  # Scope to current run first
  cat_data <- cat_data[pip_id %in% target_ids]
  cat_meta <- cat_meta[pip_id %in% target_ids]
  # P1.2: Then validate format only for the current-run set
  bad_data <- cat_data[!grepl(pip_id_pattern, pip_id), path]
  ...
  ```

- **[P2.2]** [cg-testing] `tests/testthat/test-build_pip_inventory.R` — No test for the `build_pip_inventory_bad_pip_id_format` warning path.
  **Why**: This branch filters non-standard artifacts from the catalog. Without a test it can silently regress.
  **Fix**: Add a test where `st_catalog_query` for `"pip"` returns one valid entry and one entry with a non-standard path (e.g., `path = "/admin/release_manifest.qs2"`) and verify `expect_warning(..., class = "build_pip_inventory_bad_pip_id_format")` and only the valid entry appears in the result.

- **[P2.3]** [cg-testing] `tests/testthat/test-build_pip_inventory.R` — No test for the `build_pip_inventory_bad_welfare_type` drop path (Step 8: pip_ids with `NA` welfare_type after `tstrsplit`).
  **Why**: The branch exists in production, emits a warning, and silently drops rows from the inventory. Untested row-drop paths are high risk.

  Note: if the 5-segment regex in Step 3.1 guarantees all pip_ids have exactly 5 segments, this branch may be dead code — if so, document that with a comment and remove the warning block.

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-architecture] `R/build_pip_inventory.R` — At 536 lines, substantially exceeds the ~80-line objective in the plan description. Steps 9–12 (upsert, release build/save, master save, verification logging) each represent a coherent unit that could be `@noRd` internal helpers.
  **Fix** (optional): Extract `build_release_inventory(run_inv, pfw)` and `verify_inventory(pip_id_map)` internal helpers. Not required before merge.

- **[P3.2]** [cg-testing] `tests/testthat/test-build_pip_inventory.R` — The `pip_write(...) returning skipped = TRUE → fall back to st_latest` code path (Step 10) is not exercised.
  **Fix**: Add a test variant in the second-run upsert test: set `pip_write` to return `list(version_id = NULL, skipped = TRUE)`, mock `st_latest = function(...) "from_latest_vid"`, and assert release version columns are populated from `st_latest`.

### ✅ Passed

- **cg-code-quality**: Naming, data.table NSE, `cli::cli_abort`/`cli_warn`, `|>` pipe, NAMESPACE clean (`update_pip_inventory`/`format_vrs` removed), `.cg-docs` in `.Rbuildignore`.
- **cg-documentation**: `build_pip_inventory()` roxygen comprehensive — `@return`, `@details` column provenance, `@param`, `@family` all present. Stale `format_vrs()` comment in `pd_deflation.R` correctly updated.
- **cg-version-control**: No secrets. Archival follows documented procedure (man pages deleted, test moved to `old_files/`).
- **cg-reproducibility**: No hardcoded paths. All I/O via stamp/pipload APIs.
- **cg-performance**: Catalog filtered to `target_ids` immediately. `collapse::rowbind` for upsert. `fs_bytes` class normalization prevents class-mismatch aborts.
- **cg-data-quality**: `stopifnot(anyDuplicated(...))` entry guard present. Duplicate pip_id post-upsert assertion prevents silent corruption.
