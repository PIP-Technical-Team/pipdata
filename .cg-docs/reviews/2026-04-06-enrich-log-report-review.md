## Review Report

**Review depth**: standard
**Plan**: `.cg-docs/plans/2026-04-06-enrich-log-report.md`
**Files reviewed**: 5
**Findings**: 0 P1, 4 P2, 5 P3

### P1 — CRITICAL (must fix before merge)

_None._

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality] `R/log_report.R:build_type_summary` — Internal
  pipeline log entries (`process_summary_inf`, `aux_changes_inf`,
  `inv_update_inf`, `null_svys_inf`) now appear in the "Summary by Type" table
  mixed in with real errors (`gd_type_miss`, `yr_wrng`, etc.), making the table
  noisy and harder to read.
  **Why**: `parse_log_meta()` assigns `error_type` from any `$info` or `$error`
  field without distinguishing operational markers from genuine pipeline errors.
  `build_type_summary()` then shows every type.
  **Fix**: Define a character vector of internal types and exclude them from the
  summary table:
  ```r
  INTERNAL_TYPES <- c(
    "process_summary_inf", "aux_changes_inf",
    "inv_update_inf", "null_svys_inf"
  )
  tbl <- dt[!error_type %in% INTERNAL_TYPES, .N, by = .(event, error_type, message)][...]
  ```

- **[P2.2]** [cg-documentation] `R/valid_dlw_load.R`, `R/pd_process_data.R`,
  `R/update_pip_inventory.R` — Roxygen docs for `valid_dlw_load()`,
  `pd_process_data()`, and `update_pip_inventory()` do not mention the new
  logging side effects.
  **Why**: Callers inspecting the docs won't know these functions write to
  `"pipdata_log"` or which logmeta entry types they emit.
  **Fix**: Add a `@note` or `@details` paragraph to each:
  ```r
  #' @details
  #' Logging: writes an `aux_changes_inf` entry to `"pipdata_log"` when
  #' auxiliary file changes are detected.
  ```

- **[P2.3]** [cg-testing] `tests/testthat/test-log_report.R` — No test verifies
  that `build_type_summary()` correctly excludes (or at minimum doesn't crash
  on) the internal logmeta types.
  **Why**: If P2.1 is fixed with a filter, a regression test is needed. Even
  without that fix, a test documenting current behaviour is missing.
  **Fix**: Add a test building a log with both a real error and a
  `process_summary_inf` entry, and assert the internal type does or does not
  appear in the summary.

- **[P2.4]** [cg-testing] `tests/testthat/test-log_report.R` — The plan
  specified unit tests for the new logging calls in `valid_dlw_load()` (emit
  `aux_changes_inf` when changes present; omit when absent) and in
  `update_pip_inventory()` (info path vs error path). None were written.
  **Why**: The logging logic branches on `!is.null(all_changes_aux)` in
  `valid_dlw_load()` and on `length(missing_ids) == 0L` in
  `update_pip_inventory()`; untested branches.
  **Fix**: These functions have external dependencies (`pipload`, `pipaux`) so
  full integration tests are hard; at minimum add a test for the
  `build_inventory_additions()` round-trip using the error-path logmeta (already
  partially done) and document that pipeline-level tests are deferred to
  integration tests.

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/log_report.R:log_report` — When optional
  sections return `character(0)`, the `c(..., "", build_processing_summary(log),
  "", ...)` pattern inserts orphan blank lines in the output markdown, producing
  double blank lines between sections.
  **Fix**: Collect sections into a list, drop empty ones, then join with `""`
  separators:
  ```r
  sections <- Filter(length, list(
    build_header(dt, title),
    build_processing_summary(log),
    build_aux_changes(log),
    build_type_summary(dt),
    build_country_table(dt),
    build_inventory_additions(log),
    build_null_surveys(log)
  ))
  lines <- unlist(lapply(sections, \(s) c(s, "")))
  ```

- **[P3.2]** [cg-code-quality] `R/log_report.R:build_country_table` — The
  no-entries fallback returns `"## Errors by Country\n\nNo country-level entries
  found."` as a single string with embedded `\n`, inconsistent with every other
  builder which returns multi-element character vectors.
  **Fix**: `return(c("## Breakdown by Country", "", "No country-level entries found."))`

- **[P3.3]** [cg-performance] `R/log_report.R` — `data.table::as.data.table(log)`
  is called independently in `build_null_surveys()`, `build_processing_summary()`,
  `build_aux_changes()`, and `build_inventory_additions()` (4 redundant calls per
  `log_report()` invocation). Since `log` is already a `data.table` this is O(1),
  but it is inconsistent — `build_header()` and `build_type_summary()` receive
  the pre-parsed `dt`, while the other builders re-parse.
  **Fix**: Either pass `dt` to all builders, or add a note that this is
  intentional (builders usable standalone).

- **[P3.4]** [cg-data-quality] `R/log_report.R:build_inventory_additions` —
  `iv$n_expected`, `iv$n_confirmed`, `iv$n_missing` are accessed without NULL
  guards before being passed to `sprintf("%d", ...)`. A malformed logmeta entry
  would produce an uninformative error.
  **Fix**: Add `if (is.null(iv$n_expected)) return(character(0))` after
  retrieving `iv`.

- **[P3.5]** [cg-architecture] `R/log_report.R` — The logmeta type strings
  (`"process_summary_inf"`, `"aux_changes_inf"`, `"inv_update_inf"`,
  `"null_svys_inf"`) are repeated as string literals across `log_report.R`,
  `pd_process_data.R`, `valid_dlw_load.R`, and `update_pip_inventory.R`. A typo
  in any one of them silently breaks the report section.
  **Fix**: Define them as package-level constants in `aaa.R` or `zzz.R`:
  ```r
  .log_types <- list(
    process_summary = "process_summary_inf",
    aux_changes     = "aux_changes_inf",
    inv_update      = "inv_update_inf",
    null_surveys    = "null_svys_inf"
  )
  ```

---

### ✅ Passed

- **cg-version-control**: No hardcoded secrets, credentials, or absolute paths.
  Consistent use of `"pipdata_log"` as log name (matches existing pattern).
- **cg-reproducibility**: No hardcoded seeds. All file paths are relative or via
  `pipload` helpers.
- **cg-data-quality** (broadly): `!is.null()` guards before all `log_info()`
  calls. `character(0)` used correctly for empty survey lists. NULL handling in
  `build_aux_changes` and `build_null_surveys` is safe.
- **cg-architecture** (broadly): New builder functions follow the existing
  `build_*` pattern. Single-responsibility maintained. No new external
  dependencies introduced.
