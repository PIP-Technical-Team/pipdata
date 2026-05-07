---
plan: .cg-docs/plans/2026-04-29-remove-date-valid-filter.md
findings:
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P3.1: fixed
  P3.2: fixed
---

## Review Report

**Review depth**: light  
**Files reviewed**: 4 (`R/update_pip_inventory.R`, `R/valid_dlw_load.R`, `R/aaa.R`, `tests/testthat/test-logging-integration.R`)  
**Findings**: 5 (P0: 0, P1: 0, P2: 3, P3: 2)

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality] `R/update_pip_inventory.R:29-30` — Roxygen bullet formatting artifacts: `null_svys_inf` bullet has trailing ` *  ` and `release_write_err` bullet has trailing `#' `. These render as broken text in the HTML/Rd docs.  
  **Why**: The `multi_replace_string_in_file` edit left literal characters from the old `#'` comment prefix appended to the new lines.  
  **Fix**: Clean up the two lines to standard `#' - \`...\`: ...` format with no trailing junk.

- **[P2.2]** [cg-testing] `tests/testthat/test-logging-integration.R` — No contract test for `release_write_err` logmeta structure, despite all other logmeta types having one.  
  **Why**: If the logmeta fields for `release_write_err` silently change (e.g. `condition_msg` dropped), nothing catches it.  
  **Fix**: Add a `test_that("release_write_err logmeta structure", ...)` contract test asserting `error = "release_write_err"` and `is.character(condition_msg)`.

- **[P2.3]** [cg-testing] `tests/testthat/test-logging-integration.R` — No test for the `release_vid = NA_character_` branch (release write fails). The unconditional column-init fix ensures schema consistency in this case, but there's no regression test protecting it.  
  **Why**: Without a test, a future refactor could move the column init back inside the guard, silently reintroducing the schema inconsistency.  
  **Fix**: Add a test using `.apply_release_vid` with `release_vid = NA_character_` and assert both columns remain `NA` for all rows.

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/valid_dlw_load.R:87` — `inv_svy <- inv_svy` no-op when `force = TRUE`.  
  **Fix**: Remove the line; the `else` branch already handles the non-`force` path.

- **[P3.2]** [cg-code-quality] `R/valid_dlw_load.R:194` — Typo in warning: `"Could not load or master inventory"` → `"Could not load PIP master inventory"`.  
  **Fix**: Correct the string.

---

### ✅ Passed

- **cg-code-quality**: No style, naming, or DRY issues beyond the above; `.cg-docs` already in `.Rbuildignore`
- **cg-testing**: 30/30 tests passing; existing release-vid tests are well-structured
