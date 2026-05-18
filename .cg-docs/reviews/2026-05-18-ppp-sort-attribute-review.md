---
plan: .cg-docs/plans/2026-05-18-ppp-sort-attribute.md
date: 2026-05-18
depth: standard
findings:
  P2.1: fixed
  P3.1: fixed
  P3.2: fixed
  P3.3: fixed
---

## Review Report

**Review depth**: standard
**Files reviewed**: 2 (`R/pd_deflation.R`, `tests/testthat/test-pd-deflation.R`)
**Findings**: 4 (P0: 0, P1: 0, P2: 1, P3: 3)

---

### P1 — CRITICAL (must fix before merge)

*None.*

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-testing] `tests/testthat/test-pd-deflation.R` — Plan requirement R2 (`ppp_sort = NULL` when no `welfare_ppp_*` columns exist) is not tested.
  **Why**: The `else` branch in `finalize_deflation_output()` that sets `ppp_sort` to `NULL` has no test. `finalize_deflation_output` is `@keywords internal` but is directly testable; the NULL path is reachable by calling it on a data.table that has no `welfare_ppp_*` columns. Untested branches in the output contract create silent regressions if the attribute is later removed.
  **Fix**:
  ```r
  test_that("ppp_sort is NULL when no welfare_ppp_* columns are present", {
    dt <- data.table::data.table(welfare_lcu = c(1, 2), weight = c(100, 200))
    result <- pipdata:::finalize_deflation_output(dt)
    expect_null(attr(result, "ppp_sort"))
  })
  ```

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-documentation] `R/pd_deflation.R` — exported `pd_deflation()` `@return` tag does not mention `ppp_sort` (or the other two output-contract attributes: `welfare_vars`, `adj_pop`).
  **Why**: Callers reading `?pd_deflation` see only "augmented with `welfare_lcu` and `welfare_ppp_*` columns" and cannot discover `attr(result, "ppp_sort")` without reading internal helper docs.
  **Fix**: Extend `pd_deflation()` `@return`:
  ```r
  #' @return The input survey `data.table` augmented with `welfare_lcu` and
  #'   `welfare_ppp_*` columns, and three attributes:
  #'   - `welfare_vars`: character vector of all `welfare_*` column names
  #'   - `adj_pop`: logical; TRUE if population weights were adjusted
  #'   - `ppp_sort`: integer base year used for row sorting (e.g. `2017L`), or
  #'     `NULL` when deflation produced no `welfare_ppp_*` columns
  #'   Returns `NA` when deflation fails (error logged via `log_failure()`).
  ```

- **[P3.2]** [cg-documentation] `compound-gpid.context.md` — the **`pd_deflation()` output contract** domain rule lists only two attributes (`welfare_vars`, `adj_pop`) and does not include `ppp_sort`.
  **Why**: Context rules are read by future AI sessions to understand invariants. A stale rule will cause the next session to infer the old contract.
  **Fix**: Append to the existing domain rule sentence:
  > "…and `ppp_sort` (integer; the base year of the `welfare_ppp_*` column used for row sorting, e.g. `2017L`; `NULL` when no `welfare_ppp_*` columns are present)."
  Also update `.cg-docs/solutions/data-quality/2026-05-07-deflation-output-contract.md` if it documents the attribute list.

- **[P3.3]** [cg-version-control] `tests/testthat/test-pd-deflation.R` — missing trailing newline at end of file.
  **Why**: POSIX convention; git diff shows `\ No newline at end of file`.
  **Fix**: Add a blank line after the last `})`.

---

### ✅ Passed

- **cg-code-quality**: Year extraction pattern correct; `setattr` convention followed; no DRY issues.
- **cg-performance**: All new operations are O(1).
- **cg-architecture**: Additive, backward-compatible output contract extension.
- **cg-reproducibility**: No seeds, hardcoded paths, or new dependencies.
- **cg-data-quality**: `regmatches` input is always a `welfare_ppp_YYYY_*` name — year extraction cannot produce `NA`.
- **cg-version-control**: `.Rbuildignore` already excludes `.cg-docs/`. No secrets.
