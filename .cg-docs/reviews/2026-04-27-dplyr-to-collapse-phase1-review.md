---
plan: .cg-docs/plans/2026-04-27-dplyr-to-collapse-phase1.md
findings:
  P2.1: fixed
  P2.2: fixed
  P3.1: open
  P3.2: open
  P3.3: open
---

## Review Report

**Review depth**: standard
**Files reviewed**: 4 (`R/pd_dlw_clean.R`, `R/pipdata_validate_gmd.R`, `R/pipdata_validation_report.R`, `tests/testthat/test-pd_dlw_clean.R`)
**Findings**: 0 P1, 2 P2, 3 P3

### P1 — CRITICAL (must fix before merge)

_None._

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-testing] `R/pipdata_validation_report.R:get_data_status` — `get_data_status()` is an **exported** function whose return type silently changed from a tibble (`dplyr::count` returns a tibble) to a data.table (`[, .(n = .N), keyby]`). The column name `n` is the same, but callers using tibble-specific methods would break.
  **Why**: No test verifies the return type contract of this exported function.
  **Fix**: Add a test that mocks `.pipdata$validation_report` and asserts the result is a `data.table` with columns `data_status` and `n`.

- **[P2.2]** [cg-documentation] `R/pipdata_validate_gmd.R:252` — Commented-out line `# tidyr::unnest(pin_version, keep_empty = TRUE) |>` retains a trailing `|>` pipe operator — leftover from the original pipe chain. Creates visual confusion.
  **Why**: Migration broke the pipe chain structure, leaving the comment syntactically stranded.
  **Fix**: Remove the trailing `|>`: `# tidyr::unnest(pin_version, keep_empty = TRUE)`

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-architecture] `R/pd_dlw_clean.R` — `fcase()` and `fifelse()` called without `data.table::` namespace qualification. Consistent with existing style but explicit qualification would clarify the dependency surface.

- **[P3.2]** [cg-performance] `R/pipdata_validation_report.R:83` — `keyby = data_status` adds a sort step. Since `data_status` is a factor with defined levels, `by =` would produce the same order. Negligible for 2 levels — advisory only.

- **[P3.3]** [cg-data-quality] `R/pipdata_validate_gmd.R:344` — `rbindlist(list(old_valid_report, valid_report), fill = TRUE)` silently fills missing columns with `NA` if schemas diverge. Low risk today but silent on schema drift.
  **Fix**: Consider a `cli::cli_warn` if `setdiff(names(old_valid_report), names(valid_report))` is non-empty.

---

### ✅ Passed

- **cg-code-quality**: Clean substitutions. `fcase()` syntax correct. NULL-safety comment in `pipdata_validate_gmd.R` is helpful.
- **cg-testing**: 15 regression tests cover boundaries, all-NA, absent-column, and unexpected-value cases. Triple-colon access for internals is correct.
- **cg-version-control**: No secrets, no hardcoded paths. `roadmap.json` update appropriate.
- **cg-reproducibility**: No seeds needed. All transformations are pure. `data.table` and `collapse` in DESCRIPTION Imports.
- **cg-architecture**: Phantom dplyr/tidyr/tibble dependencies eliminated from 3 files. `dlw_scan_and_validate.R` correctly untouched (Phase 2).
