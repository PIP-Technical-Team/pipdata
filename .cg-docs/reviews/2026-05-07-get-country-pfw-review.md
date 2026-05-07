---
date: 2026-05-07
plan: null
depth: standard
files-reviewed:
  - R/get_country_pfw.R
findings:
  P1.1: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: open
  P3.1: fixed
  P3.2: open
  P3.3: open
---

## Review Report

**Review depth**: standard
**Files reviewed**: 1 (`R/get_country_pfw.R`)
**Findings**: 7 (P0: 0, P1: 1, P2: 3, P3: 3)

---

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-performance] `R/get_country_pfw.R:67–79` — `apply(.SD, MARGIN = 1, ...)` in a `data.table` `j` expression converts `.SD` to a matrix row-by-row, defeating all data.table vectorisation.
  **Why**: `apply(..., MARGIN = 1)` forces a `matrix` coercion of `.SD` and calls the anonymous closure once per row — O(n) R-level function calls. The domain columns are integers so `pmax` works element-wise across columns without any loop.
  **Fix**:
  ```r
  cpfw[inpovcal == 1,
    reporting_level := as.character(do.call(pmax, .SD)),
    .SDcols = dcols
  ]
  ```
  `do.call(pmax, .SD)` returns an integer vector of per-row maxima across all domain columns in one vectorised C call, then `as.character()` converts once. Chain the `inpovcal == 1` filter into the `i` argument instead of `][`.

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality] `R/get_country_pfw.R:86, 92, 101` — Three `rlang::abort(message = ..., use_cli_format = TRUE)` calls. Project standard is `cli::cli_abort()`.
  **Why**: `rlang::abort(..., use_cli_format = TRUE)` is the legacy pattern. `cli::cli_abort()` is shorter and already used in all other `R/` files added this cycle.
  **Fix**:
  ```r
  cli::cli_abort(
    "PFW does not contain info for country, surveyid year, and survey_acronym",
    class = c("piperr", "info_pfw")
  )
  ```

- **[P2.2]** [cg-data-quality] `R/get_country_pfw.R:56` — `report_lvl()` does not validate that `dcols` columns exist in `cpfw` before passing them to `.SDcols`. If the PFW schema changes, the error is an opaque `object 'X' not found`.
  **Why**: Hard-coded column names in `.SDcols` with no up-front check produces cryptic data.table errors rather than an actionable abort.
  **Fix**:
  ```r
  missing_dcols <- setdiff(dcols, names(cpfw))
  if (length(missing_dcols) > 0L) {
    cli::cli_abort(
      "PFW is missing expected domain columns: {.field {missing_dcols}}.",
      class = c("report_lvl", "piperr")
    )
  }
  ```

- **[P2.3]** [cg-architecture] `R/get_country_pfw.R:54–58` — `dcols` is a hard-coded character vector inside `report_lvl()` with comment `# We need to include to sysdata`. Deferred in-place with no tracked action.
  **Why**: Hard-coded column lists are a maintenance hazard. No roadmap entry or TODO links this to planned work.
  **Fix**: Extract to a package-internal constant, e.g. in `R/sysdata.R`:
  ```r
  .DOMAIN_COLS <- c("cpi_domain", "ppp_domain", "gdp_domain", "pce_domain", "pop_domain")
  ```
  Or add a roadmap item for `sysdata-domain-cols`.

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/get_country_pfw.R:78` — Two-step anonymous function `function(x) { y <- max(x); as.character(y) }` is verbose; superseded by P1.1 fix. If kept for any reason, use `\(x) as.character(max(x))`.

- **[P3.2]** [cg-testing] `R/get_country_pfw.R` — No test file found for `get_country_pfw`, `report_lvl`, or `cache_id`. Three abort branches (`nrow == 0`, `nrow > 1 & n_cpfw_wt == 1`, `wt == ""`) are untested.
  **Fix**: Create `tests/testthat/test-get-country-pfw.R` covering at minimum: empty-PFW abort, non-unique-PFW abort, and a happy-path `reporting_level` value check.

- **[P3.3]** [cg-documentation] `R/get_country_pfw.R:49, 136` — `report_lvl()` and `cache_id()` have no `@return` descriptions and no explanation of what `reporting_level` values mean (`"1"` = national, `"2"` = subnational). The connection to `*_data_level` attrs set downstream in `add_dom_vars()` is implicit.

---

### ✅ Passed

- `@cg-version-control`: No secrets, credentials, or hard-coded paths outside the package namespace.
- `@cg-reproducibility`: No seeds needed; no external file paths; `split()` output is deterministic.
- `@cg-data-quality` (beyond P2.2): `inpovcal == 1` filter correctly applied before domain-max computation; `welfare_type` empty-string guard in `cache_id()` is present.
