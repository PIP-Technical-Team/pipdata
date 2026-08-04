---
plan: .cg-docs/plans/2026-05-04-integrate-deflation.md
date: 2026-05-05
depth: standard
parent-review: .cg-docs/reviews/2026-05-04-integrate-deflation-review.md
findings:
  P1.1: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P3.1: fixed
  P3.2: fixed
---

## Review Report

**Review depth**: standard
**Scope**: commit `78fb46e` — stamp round-trip attribute/column bug fixes
**Files reviewed**: 2 (`R/pd_deflation.R`, `tests/testthat/test-pd-deflation.R`)
**Findings**: 6 (P0: 0, P1: 1, P2: 3, P3: 2)

---

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-code-quality] `R/pd_deflation.R` — `restore_data_level_cols()` and `add_rep_lvl()` attribute fallback do not unwrap `list(values = ...)` wrapper
  **Why**: On the pipeline path, `vars_to_attr()` stores level attrs as `list(values = "national")`. If called on a pipeline-path object (columns already stripped to attrs but in list form), `dt[, (col) := list(values = "national")]` creates a list column rather than `"national"`, silently corrupting the join key used by `add_ppp()` / `add_cpi()`. The identical unwrapper pattern is already in `add_cpi()` (`get_attr_val`).
  **Fix**:
  ```r
  # In restore_data_level_cols():
  val <- attr(dt, col)
  if (!is.null(val)) {
    val <- if (is.list(val)) val[["values"]] else val
    dt[, (col) := val]
  }

  # In add_rep_lvl() attribute fallback:
  rep_lvl <- dt_attrs$ppp_data_level %||% dt_attrs$cpi_data_level
  rep_lvl <- if (is.list(rep_lvl)) rep_lvl[["values"]] else rep_lvl
  ```
  Note: `%||%` is not in the package namespace — use the `if (is.null(...))` pattern instead (consistent with the rest of the file).

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-testing] `tests/testthat/test-pd-deflation.R` — no test exercises the real stamp round-trip path `restore_data_level_cols()` was written to fix
  **Why**: The Mode B test mocks `pip_read` to return `make_pipmd()`, which already has `ppp_data_level`/`cpi_data_level` as **columns** — `restore_data_level_cols()` is a no-op in all current tests. The actual bug (columns absent, only scalar attrs) is untested.
  **Fix**:
  ```r
  test_that("restore_data_level_cols materialises scalar attrs as columns", {
    dt <- make_pipmd()
    dt[, ppp_data_level := NULL]
    dt[, cpi_data_level := NULL]
    data.table::setattr(dt, "ppp_data_level", "national")
    data.table::setattr(dt, "cpi_data_level", "national")
    result <- pipdata:::restore_data_level_cols(dt)
    expect_true("ppp_data_level" %in% names(result))
    expect_equal(unique(result$ppp_data_level), "national")
  })
  ```

- **[P2.2]** [cg-testing] `tests/testthat/test-pd-deflation.R` — `make_pipmd()` stores level info as both columns and `list(values=...)` attrs, masking the stamp-path bug throughout the test suite
  **Why**: The fixture stores `ppp_data_level`/`cpi_data_level` as table columns *and* as `list(values=...)` attributes simultaneously. This means `restore_data_level_cols` is always skipped (column present check passes) and the plain-scalar-attr path is never exercised.
  **Fix**: Add a `make_pipmd_stamp()` helper that drops the level columns and sets plain scalar attrs (e.g. `setattr(dt, "ppp_data_level", "national")` not `list(values = ...)`). Use it in Mode B tests.

- **[P2.3]** [cg-code-quality] `R/pd_deflation.R` — `pd_deflation()` Mode A `pip_id` resolution uses `$values[[1L]]` accessor, breaks on stamp-path `dt`
  **Why**:
  ```r
  pip_id <- attributes(dt)$pip_names$values[[1L]]
  if (is.null(pip_id)) pip_id <- attributes(dt)$survey_id$values[[1L]]
  ```
  On a stamp-path `dt`, these attrs are plain character scalars — `"ABC..."$values` is `NULL`. Both branches resolve to `NULL`, causing `.load_deflation_aux()` to abort with "No inventory entry found for NULL" when Mode A is called with a stamp-loaded `dt` and no explicit `pip_id`.
  **Fix**:
  ```r
  get_first <- function(x) if (is.list(x)) x[["values"]][[1L]] else x[[1L]]
  pip_id <- get_first(attributes(dt)$pip_names)
  if (is.null(pip_id)) pip_id <- get_first(attributes(dt)$survey_id)
  ```

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/pd_deflation.R` — `safe_deflation()` skip message may print `list(values=...)` for pipeline-path objects
  **Why**: `pd_env_set("log_survey_id", attributes(dt)$survey_id)` stores the raw attribute. On the pipeline path this is `list(values = "ABC_2015...")`. The CLI skip message prints `list("ABC_2015...")`.
  **Fix**: `sv <- attr(dt, "survey_id"); pd_env_set("log_survey_id", if (is.list(sv)) sv[["values"]] else sv)`

- **[P3.2]** [cg-documentation] `R/pd_deflation.R` — `restore_data_level_cols()` docstring omits reference-semantics mutation note
  **Why**: The function calls `dt[, (col) := val]` which mutates `dt` in place. Callers passing a non-copy will have their object modified without warning.
  **Fix**: Add `@note Mutates \code{dt} by reference via \code{:=}. Pass \code{data.table::copy(dt)} if the original must be preserved.`

---

### ✅ Passed

- **cg-documentation**: All new functions have roxygen headers with `@keywords internal`.
- **cg-version-control**: No credentials or improperly-ignored files.
- **cg-reproducibility**: No hardcoded paths; stamp version resolution is deterministic.
- **cg-performance**: `restore_data_level_cols()` iterates 3 attrs — no concern.
- **cg-architecture**: New helper is self-contained; no new cross-package dependencies.
- **cg-data-quality**: `.validate_deflation_input()` validates both class and required attrs/columns before deflation runs.
- **cg-code-quality (.Rbuildignore)**: `^\.cg-docs$` already present. ✅
