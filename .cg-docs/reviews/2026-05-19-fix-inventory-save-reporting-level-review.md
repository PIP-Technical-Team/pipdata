---
plan: .cg-docs/plans/2026-05-19-fix-inventory-save-reporting-level.md
findings:
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P3.1: fixed
  P3.2: fixed
---

## Review Report

**Review depth**: light
**Files reviewed**: 4
**Findings**: 6 (P0: 0, P1: 0, P2: 4, P3: 2)

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality] `R/update_pip_inventory.R` — `sentinel_col` is the first non-key column of `vrs_mdt` by position, not by name.
  **Why**: If `format_vrs()` output column order changes (e.g. a new column is prepended), a different column becomes the sentinel and the missing-metadata check silently targets the wrong field. `content_hash` is the canonical presence indicator.
  **Fix**: Replace the sentinel derivation block with a direct named check:
  ```r
  sentinel_col <- if ("content_hash" %in% names(vrs_mdt)) {
    "content_hash_metadata"
  } else {
    NULL
  }
  ```

- **[P2.2]** [cg-documentation] `R/update_pip_inventory.R` — `missing_metadata_err` is not listed in the `@details` logging section, even though it is now emitted by this function.
  **Why**: The `@details` block is the canonical inventory of log entry types for this function; omitting `missing_metadata_err` means callers and `log_report()` authors cannot discover it from the docs.
  **Fix**: Add to the `@details` logging bullet list:
  ```
  - `missing_metadata_err`: pip_ids excluded from inventory due to absent metadata entry.
  ```

- **[P2.3]** [cg-testing] `tests/testthat/test-update_pip_inventory.R` — No test for the missing-metadata exclusion path (the `sentinel_col` / `missing_meta` block).
  **Why**: This is the primary fix for R1 in the plan and has no direct coverage. The multi-pip_id test verifies the happy path only; a pip_id arriving in `vrs_dt` with no matching `vrs_mdt` row is untested.
  **Fix**: Add a test where `versions_metadata` for one of two pip_ids returns `NULL`, run `format_vrs()` for both, manually construct the join result with a NA metadata column, and verify the row is filtered and the log entry has `error = "missing_metadata_err"`. Since this logic is inside `update_pip_inventory()` rather than `format_vrs()`, a mock-based test similar to the pk-warning test is needed.

- **[P2.4]** [cg-testing] `tests/testthat/test-update_pip_inventory.R` — No test exercises the `reporting_level` join path.
  **Why**: The `pfw_rl_unq` construction and join are untested. If `.DOMAIN_COLS` changes or the `pmax` aggregation has an off-by-one (e.g. returns integer `2` instead of character `"2"`), there is no test to catch it at the inventory level.
  **Fix**: Add a unit test with a mock `new_pip_inv` (2 rows: one national, one subnational) and a mock `pfw` matching those rows, running only the `reporting_level` derivation logic. Verify the joined `reporting_level` column has `c("1", "2")` for the respective rows.

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `tests/testthat/test-update_pip_inventory.R:20` — `make_ventry()` uses `%||%` without an explicit import or package qualification.
  **Why**: `%||%` is available when `rlang` is loaded via `devtools::load_all()`, but testthat in isolation may not have it. Relying on side-effect loading is fragile.
  **Fix**: Replace `reason %||% "test reason"` with `if (is.null(reason)) "test reason" else reason`.

- **[P3.2]** [cg-code-quality] `R/update_pip_inventory.R` — ~~`check_vrs_unique()` is likely unreachable in practice.~~
  **Status**: Fixed — removed `check_vrs_unique()` and the `unique()` wrapper in `format_vrs()`. Confirmed via upstream analysis that `pip_names` cannot contain duplicates: `pd_split_alt_welfare()` produces at most 2 elements with distinct `cache_id` values (differentiated by welfare type `INC`/`CON`), and `report_lvl()` aborts if multiple PFW rows share the same welfare type. The `relationship = "one-to-one"` on the `joyn::left_join()` remains as an implicit guard.

### ✅ Passed

- **cg-code-quality**: `on.exit(..., add = TRUE)` in `load_pip_data()` is correctly ordered. `.Rbuildignore` already excludes `.cg-docs`.
- **cg-testing**: pk-suppression test correctly verifies both during-call state and post-call restoration. `format_vrs()` deduplication test is valid.
