---
date: 2026-04-16
title: "R CMD check 'no visible binding' notes from data.table NSE column names"
category: "build-errors"
language: "R"
tags: [r-cmd-check, data.table, NSE, globalVariables, CRAN, package-check]
root-cause: "R's static analyser cannot resolve unquoted column names used in data.table's DT[i,j,by] syntax"
severity: "P3"
---

# R CMD check 'no visible binding' notes from data.table NSE column names

## Problem

Running `devtools::check()` produces a wall of NOTEs like:

```
checking R code for possible problems ... NOTE
build_country_table: no visible binding for global variable 'country'
parse_log_meta: no visible binding for global variable 'logmeta'
dlw_gmd_match: no visible binding for global variable 'FileName'
...
Undefined global functions or variables:
  Checksum Ext FileName Module N country data_available ...
```

`devtools::test()` passes with zero failures — the issue only appears in `check()`.

## Root Cause

`R CMD check` applies static analysis to every `.R` file. data.table's non-standard
evaluation (`:=`, `DT[i, j, by]`, `vapply` closures referencing column names as bare
symbols) looks like unbound global variables to the static checker because it cannot
follow the runtime semantics of data.table.

The same applies to:
- `dplyr`/`tidyverse` `.data$col` references
- Any symbol computed at runtime (`.joyn`, `..key`, `..selected_vars`, etc.)

## Solution

Add a `utils::globalVariables()` call in `R/aaa.R` (or a dedicated `R/globals.R`)
listing every symbol that triggers the note:

```r
utils::globalVariables(c(
  # data.table NSE column names
  "..key", "..selected_vars", ".data", ".joyn",
  "Checksum", "Ext", "FileName", "Module", "N",
  "age", "count_valid", "country", "data_available", "data_status",
  "date_validated", "description", "dlw_meta", "educat7", "educy",
  "error_type", "event", "ext", "hhid", "logmeta",
  "maxalt", "maxmast", "maxpip", "module_type", "pid", "pin_version",
  "pip_id", "rf_year", "status", "status_count", "survey",
  "table_name", "tool", "type", "version_dlw"
))
```

Also prefix any bare base R function call that triggers the note:
- `setNames(...)` → `stats::setNames(...)`
- `setNames` was used in `pd_aux_attr.R` and flagged as `add_attr: no visible global function`.

Obtain the full list from check output:
```r
res <- devtools::check(quiet = TRUE)
cat(res$notes, sep = "\n")
```

## Prevention

- Put `utils::globalVariables()` in `R/aaa.R` (runs first alphabetically) — **one declaration only**.
  Adding a second `utils::globalVariables()` call in another file (e.g., `R/utils.R`) creates
  a duplicate registry. All NSE symbols must be consolidated into the single block in `R/aaa.R`.
  When adding new symbols after refactoring, append to the existing list; never create a new call.
- After any new `.R` file is added with data.table NSE, re-run `check()` and
  add newly flagged names to the declaration.
- Always use `stats::setNames()` or `base::setNames()` — never bare `setNames()`.
- Prefer `importFrom("stats", "setNames")` in NAMESPACE if the function is used heavily.

## Related

- [R package development: Dealing with NSE and undefined globals](https://r-pkgs.org/r-cmd-check.html#check-r-code)
- See also `2026-04-16-non-ascii-characters-in-r-code.md` (companion check WARNING)
