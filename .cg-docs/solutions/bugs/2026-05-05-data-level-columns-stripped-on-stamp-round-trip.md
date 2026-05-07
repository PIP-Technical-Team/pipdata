---
date: 2026-05-05
title: "ppp_data_level / cpi_data_level columns stripped on stamp round-trip"
category: "bugs"
type: "bug"
language: "R"
tags: [deflation, stamp, round-trip, data-level, ppp-data-level, cpi-data-level, attributes, add-rep-lvl, add-ppp, add-cpi, pd-deflation]
root-cause: "stamp serialises data.table columns to attributes during pip_write(); ppp_data_level, cpi_data_level, and pop_data_level are stored as object attributes rather than columns after pip_read(), so add_rep_lvl / add_ppp / add_cpi fail when they look for those values as columns."
severity: "P1"
test-written: "no"
fix-confirmed: "yes"
---

# `ppp_data_level` / `cpi_data_level` columns stripped on stamp round-trip

## Symptom

`pd_deflation(pip_id = "BOL_2022_EH_INC_ALL")` silently returns `NA`. Calling
`.deflation_pipmd_core()` directly surfaces two successive errors:

**Error 1** — `add_rep_lvl()` tries to read `data_level` columns but finds
none:

```
Error in get():
! object 'NA' not found
```

`select_var` is empty so `select_var[1]` is `NA`, and `dt[, reporting_level := get("NA")]` fails.

**Error 2** (after patching `add_rep_lvl`) — `add_ppp()` references
`ppp_data_level` as a column inside a data.table `j` expression:

```
Error:
! object 'ppp_data_level' not found
```

## Root Cause

During the pipeline `pd_process_data()` calls `vars_to_attr()` which moves
level columns (`ppp_data_level`, `cpi_data_level`, `pop_data_level`, etc.) from
the `data.table` into named object attributes for storage efficiency. These
attributes are preserved across `pip_write()` / `pip_read()` (stamp round-trip)
**as attributes**, not re-materialised as columns.

The deflation helpers (`add_rep_lvl`, `add_ppp`, `add_cpi`) were written
assuming the columns would always be present in the table, not realising they
might be attribute-only on the stamp-load path.

An additional symptom: `attributes(dt)$survey_id` was accessed as
`attributes(dt)$survey_id$values` in `safe_deflation()`, but after a stamp
round-trip `survey_id` is a plain character scalar, not a `list(values = ...)`,
so `$values` returns `NULL` and `pd_env_set("log_survey_id", NULL)` silently
lost the survey identity used in the skip message.

## Fix

### 1. `safe_deflation()` — drop `$values` accessor

```r
# Before
pd_env_set("log_survey_id", attributes(dt)$survey_id$values)

# After
pd_env_set("log_survey_id", attributes(dt)$survey_id)
```

### 2. New helper `restore_data_level_cols()`

Added in `R/pd_deflation.R` — materialises any `*_data_level` attribute that
is missing as a column before the deflation pipeline runs:

```r
restore_data_level_cols <- function(dt) {
  level_attrs <- c("ppp_data_level", "cpi_data_level", "pop_data_level")
  for (col in level_attrs) {
    if (!col %in% names(dt)) {
      val <- attr(dt, col)
      if (!is.null(val)) {
        dt[, (col) := val]
      }
    }
  }
  dt
}
```

### 3. `add_rep_lvl()` — attribute fallback when no `data_level` columns exist

```r
dl_var <- grep("data_level", names(dt), value = TRUE)

if (length(dl_var) == 0L) {
  dt_attrs <- attributes(dt)
  rep_lvl  <- if (!is.null(dt_attrs$ppp_data_level)) {
    dt_attrs$ppp_data_level
  } else {
    dt_attrs$cpi_data_level
  }
  if (is.null(rep_lvl)) {
    cli::cli_abort(
      "Cannot determine reporting level: no data_level columns or attributes.",
      class = c("add_rep_lvl", "piperr")
    )
  }
  dt[, reporting_level := rep_lvl]
} else {
  ordered_level   <- purrr::map_dbl(dl_var, ~ get_ordered_level(dt, .x))
  report_lvl_cpfw <- as.numeric(attributes(dt)$reporting_level)
  select_var      <- dl_var[ordered_level == report_lvl_cpfw]
  dt[, reporting_level := get(select_var[1])]
}
```

### 4. Both core functions call `restore_data_level_cols()` first

```r
.deflation_pipmd_core <- function(dt, cpi, ppp, pop) {
  dt_c <- data.table::copy(dt)
  ...
  dt_c <- restore_data_level_cols(dt_c)   # ← new
  dt_c <- add_rep_lvl(dt_c)
  ...
}
```

## Prevention

- **Any** helper that joins on `ppp_data_level`, `cpi_data_level`, or
  `pop_data_level` must check for the attribute-only case or call
  `restore_data_level_cols()` first.
- The long-term fix (tracked in roadmap as `store-version-id-in-inventory`) is
  to materialise level columns before calling `pip_write()`, so the round-trip
  is lossless and no restoration is needed.
- When `safe_deflation()` returns `NA` with a "survey skipped" message, always
  reproduce the failure by calling `.deflation_pipmd_core()` directly to bypass
  the `tryCatch` and surface the real error.

## Related

- `.cg-docs/solutions/bugs/2026-05-06-subnational-deflation-area-attribute-not-resolved.md`
  — follow-on bug: the attribute-only `add_rep_lvl()` introduced here still
  does not handle `ppp_data_level == "area"` (column-pointer semantics) —
  results in NA deflation for all subnational surveys.
- `.cg-docs/solutions/bugs/2026-05-06-attribute-list-values-wrapper-pipeline-vs-stamp-path.md` — generalised pattern: any attr that may be `list(values=...)` or plain scalar needs the canonical unwrapper
- `.cg-docs/solutions/testing-patterns/2026-05-06-stamp-vs-pipeline-path-test-fixtures.md` — `make_pipmd_stamp()` fixture for testing stamp-path code
- `.cg-docs/solutions/bugs/2026-05-05-pip-class-stripped-on-stamp-round-trip.md` — same root cause: stamp round-trip strips object-level metadata (class and columns-as-attributes alike)
- `.cg-docs/solutions/bugs/2026-05-05-stamp-version-id-vs-content-hash.md`
- `R/pd_deflation.R`: `restore_data_level_cols()` (removed), `add_rep_lvl()` (removed — see subnational fix plan), `.deflation_pipmd_core()`, `.deflation_pipgd_core()`, `safe_deflation()`
