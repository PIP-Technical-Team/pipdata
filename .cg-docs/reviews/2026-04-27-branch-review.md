---
plan: null
findings:
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P3.1: fixed
  P3.2: fixed
  P3.3: fixed
resolved_date: 2026-04-29
---

## Review Report

**Review depth**: standard  
**Scope**: Full branch diff `DEV_v2`→`to_dos`  
**Files reviewed**: 13 (`R/pd_dlw_clean.R`, `R/pd_process_data.R`, `R/pipdata_dlw_compare.R`, `R/pipdata_dlw_process.R`, `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`, `R/pipdata_validation_report.R`, `R/update_pip_inventory.R`, `R/utils.R`, `R/valid_dlw_load.R`, `tests/testthat/test-logging-integration.R`, `tests/testthat/test-pd_dlw_clean.R`, `tests/testthat/test-pipdata_validation_report.R`)  
**Findings**: 6 (P0: 0, P1: 0, P2: 3, P3: 3)

### Status: ✅ ALL RESOLVED (2026-04-29)

---

### P1 — CRITICAL (must fix before merge)

_None._

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-documentation] `R/pd_process_data.R:30-33` — `force` and `verbose` parameters are present in the `pd_process_data()` function signature but have no `@param` entries in the roxygen block.  
  **Why**: R CMD check raises a WARNING (`Undocumented arguments in documentation object 'pd_process_data': 'force' 'verbose'`), which blocks a clean package build.  
  **Fix**: Add to the roxygen block:
  ```r
  #' @param force Logical. If `TRUE`, forces reprocessing of all surveys by
  #'   switching stamp versioning to `"timestamp"` and bypassing the master
  #'   inventory comparison. Default `FALSE`.
  #' @param verbose Logical. Print progress messages. Default:
  #'   `getOption("pipdata.verbose", default = FALSE)`.
  ```

- **[P2.2]** [cg-documentation] `R/valid_dlw_load.R:11` — The inline link `[last_ver_inv()]` references an unexported internal function, causing an R CMD check WARNING (`Missing link or links in documentation object 'valid_dlw_load.Rd': 'last_ver_inv'`).  
  **Why**: Roxygen `[foo()]` syntax generates an Rd cross-reference that requires the target to be exported. Internal helpers must be referenced as plain code text instead.  
  **Fix**: Replace with backtick-code format:
  ```r
  #' 3. Selects the latest version of each survey via `last_ver_inv()`.
  ```

- **[P2.3]** [cg-code-quality] `R/pd_dlw_clean.R:recode_edu` — R CMD check emits a NOTE: `recode_edu: no visible binding for global variable 'school'`. The `school` variable inside `collapse::ftransform(school = fcase(school == ...))` is not visible to R's static analysis.  
  **Why**: R's static analyser cannot see column names passed to `collapse::ftransform()` as LHS symbols, so it flags them as undefined globals. The NOTE pollutes `R CMD check` output even though the code is correct at runtime.  
  **Fix**: Add `"school"` to the existing `utils::globalVariables()` declaration in `R/aaa.R` for centralized NSE symbol management.  
  **✅ RESOLVED**: Consolidated `"school"` into the existing `utils::globalVariables(c(...))` block in `R/aaa.R:29`, maintaining the package-wide NSE symbol registry in one location.

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-documentation] `R/pd_process_data.R:107` — `process_data()` roxygen documents `@param pfw PFW` but the function signature uses `aux_list` (a named list of all auxiliaries). This causes an R CMD check WARNING (`Documented arguments not in \usage: 'pfw'` / `Undocumented arguments: 'aux_list'`).  
  **Why**: The `pfw` → `aux_list` refactor updated the function signature but the documentation was not updated to match.  
  **Fix**: Replace `#' @param pfw PFW` with:
  ```r
  #' @param aux_list Named list of auxiliary data frames; expected keys:
  #'   `"pfw"`, `"cpi"`, `"ppp"`, `"pop"`, `"gdp"`, `"pce"`.
  ```

- **[P3.2]** [cg-data-quality] `R/pipdata_validate_gmd.R:339` — The schema drift check is one-directional: `setdiff(names(old_valid_report), names(valid_report))` catches columns present in the old report but absent from the new. It does not warn when the new report introduces columns not in the old.  
  **Why**: `rbindlist(fill = TRUE)` silently fills missing columns in both directions with `NA`. Schema growth (new report adds a column) is currently undetected.  
  **Fix**: Add a symmetric check immediately after the existing one:
  ```r
  cols_new <- setdiff(names(valid_report), names(old_valid_report))
  if (length(cols_new) > 0) {
    cli::cli_warn(c(
      "Schema drift detected in validation_report:",
      "i" = "New columns not in old report: {.val {cols_new}}"
    ))
  }
  ```

- **[P3.3]** [cg-documentation] `R/pipdata_validate_gmd.R:337-341` — The existing `cli::cli_warn()` call uses a multi-line string with literal indentation whitespace from code alignment. This indentation will appear verbatim in terminal output.  
  **Why**: `cli` does not strip leading whitespace from continuation lines inside a string literal, so the warning will show the code indentation as part of the message.  
  **Fix**: Use the structured cli multi-message format instead:
  ```r
  cli::cli_warn(c(
    "Schema drift detected in validation_report:",
    "i" = "Columns in old but missing from new: {.val {cols_old}}"
  ))
  ```

---

### ✅ Passed

- **cg-code-quality**: `fcase()` consistently qualified with `data.table::` throughout all recode functions. Dead code removal in `utils.R` clean. `school` global variable NOTE (P2.3) resolved by consolidation into aaa.R NSE symbol registry.
- **cg-testing**: 37+ new tests: 15 in `test-pd_dlw_clean.R` covering recode boundary/edge cases, 22 in `test-logging-integration.R` documenting logging contracts, 3+ in `test-pipdata_validation_report.R` covering the return-type contract of `get_data_status()`. All pass. `:::` triple-colon access for unexported functions is the correct testthat pattern.
- **cg-version-control**: No secrets or hardcoded paths. Conventional commit messages used throughout. `.cg-docs/` correctly excluded from package build via `.Rbuildignore`.
- **cg-reproducibility**: No statistical seeds required. All transformations are pure. Release management relies on `pipfun::setup_working_release()` — no hardcoded paths.
- **cg-performance**: `rbindlist(fill = TRUE)` over `bind_rows()` and `fcase()` over `case_when()` are correct performance improvements with no behavioural regressions.
- **cg-architecture**: `pipfun::log_info()` confirmed as a valid exported wrapper accepting `logmeta`. Redundant `get_wrk_release()` guards correctly removed from all 4 delegate functions. `dlw_scan_and_validate.R` correctly left untouched (Phase 2 scope).
- **cg-data-quality**: `rbindlist(fill = TRUE)` handles NULL entries gracefully (same as `bind_rows`). Schema drift guard for old→new direction added correctly in `pipdata_validate_gmd.R`; the reverse direction flagged as P3.2.
