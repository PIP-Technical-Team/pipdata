---
date: 2026-04-30
title: "Unified package environment: merging .pipdata / .pipdataenv / .logenv into one"
category: "environment-issues"
language: "R"
tags: [environment, package-state, accessor-helpers, refactoring, aaa.R, .pipdataenv]
root-cause: "Three separate package-level environments (.pipdata, .pipdataenv, .logenv) scattered mutable state with no encapsulation, raw assign()/rm()/$-access everywhere, and redundant dual-initialization in aaa.R + zzz.R"
severity: "P2"
---

# Unified Package Environment: Accessor-Helper Pattern

## Problem

`pipdata` had three separate package-level environments:

| Environment | Purpose | Defined in |
|-------------|---------|------------|
| `.pipdata`  | DLW validation accumulator (`validation_report`) | `aaa.R` |
| `.pipdataenv` | Save context (`save_id_name`) + pipeline context (`process_survey_id`) | `aaa.R` + re-init `zzz.R` |
| `.logenv`   | Deflation error log (`survey_id`, `piperr`, `unk_err`) | `aaa.R` + re-init `zzz.R` |

This caused:
- Raw `assign()`, `rm()`, `$`, `env_has()`, `env_poke()`, `get()` scattered
  across 7 source files — no encapsulation, no discoverability
- Dual-initialization: `.pipdataenv` and `.logenv` were both created in `aaa.R`
  **and** re-created in `.onLoad()` in `zzz.R` — order-dependent, fragile
- No consistent test pattern (direct `$` injection in some tests, `rlang::env_poke`
  in others)

## Root Cause

Incremental growth: each new piece of pipeline state got its own environment
because there was no shared accessor layer. The pattern was copy-pasted without
consolidation.

## Solution

### 1. Single environment + namespaced keys

Merge all three into `.pipdataenv` in `aaa.R`, with key prefixes indicating origin:

```r
# R/aaa.R
# Unified package-level environment. Stores mutable state for both pipeline
# wrappers using namespaced keys:
#   save_*      — save_pip.R / pd_process_data.R save context
#   process_*   — pd_process_data.R survey-loop context
#   validation_ — DLW validation accumulator (was .pipdata)
#   log_*       — deflation error log (was .logenv)
.pipdataenv <- new.env(parent = emptyenv())
```

### 2. Five accessor helpers (also in `aaa.R`)

```r
pd_env_set <- function(key, value) {
  assign(key, value, envir = .pipdataenv)
  invisible(value)
}

pd_env_get <- function(key, default = NULL) {
  if (rlang::env_has(.pipdataenv, key)) return(get(key, envir = .pipdataenv))
  default
}

pd_env_rm <- function(key) {
  if (rlang::env_has(.pipdataenv, key)) rm(list = key, envir = .pipdataenv)
  invisible(NULL)
}

pd_env_reset <- function() {
  rm(list = ls(.pipdataenv), envir = .pipdataenv)
  invisible(NULL)
}

pd_env_append <- function(key, new_rows) {
  existing <- pd_env_get(key)
  if (is.null(existing)) pd_env_set(key, new_rows)
  else pd_env_set(key, rbind(existing, new_rows, ignore.attr = TRUE))
}
```

### 3. Simplified `.onLoad()` in `zzz.R`

```r
.onLoad <- function(libname, pkgname) {
  pd_env_reset()   # wipe stale keys from .pipdataenv only
  pipfun::log_init("pipdata_log", overwrite = TRUE)
  # ... options
}
```

No re-creation of `.pipdataenv` — just reset. No `.logenv` or `.pipdata` blocks.

### 4. Key naming convention

| Old | New key | Accessor |
|-----|---------|----------|
| `.pipdata$validation_report` | `"validation_report"` | `pd_env_get("validation_report")` |
| `.pipdataenv$id_name` | `"save_id_name"` | `pd_env_get("save_id_name")` |
| `.pipdataenv$survey_id` | `"process_survey_id"` | `pd_env_get("process_survey_id")` |
| `.logenv$survey_id` | `"log_survey_id"` | `pd_env_get("log_survey_id")` |
| `get("piperr", envir = .logenv)` | `"log_piperr"` | `pd_env_get("log_piperr")` |
| `get("unk_err", envir = .logenv)` | `"log_unk_err"` | `pd_env_get("log_unk_err")` |

### 5. Accumulation pattern (`pd_env_append`)

The 8-repetition pattern in `pipdata_dlw_validation.R`:

```r
# BEFORE (7 lines × 8 functions = 56 lines)
if (!rlang::env_has(.pipdata, "validation_report")) {
  rlang::env_poke(.pipdata, "validation_report", validation_record)
} else {
  compiled_result <- rbind(.pipdata$validation_report, validation_record, ignore.attr = TRUE)
  rlang::env_poke(.pipdata, "validation_report", compiled_result)
  cli::cli_inform("Validation report has been added to .pipdata.")
}

# AFTER (1 line)
pd_env_append("validation_report", validation_record)
```

## Prevention

- **Never** add a new package-level environment. All new mutable state goes into
  `.pipdataenv` with a namespaced key.
- **Never** use raw `assign()`, `rm()`, or `$` on `.pipdataenv` outside of the
  5 accessor helpers in `aaa.R`.
- New keys should follow the prefix convention: choose the closest matching
  prefix (`save_`, `process_`, `validation_`, `log_`) or propose a new prefix
  in the `aaa.R` comment block.
- When testing functions that read from `.pipdataenv`, use `pd_env_set()` /
  `pd_env_rm()` via a scoped helper (see
  [testing-patterns/2026-04-27-testing-package-environment-state.md](../testing-patterns/2026-04-27-testing-package-environment-state.md)).

## Related

- [testing-patterns/2026-04-27-testing-package-environment-state.md](../testing-patterns/2026-04-27-testing-package-environment-state.md)
  — how to inject test state into `.pipdataenv` using accessor helpers
- [testing-patterns/2026-04-29-logging-in-trycatch-handlers.md](../testing-patterns/2026-04-29-logging-in-trycatch-handlers.md)
  — `capture_log_args()` resolves to handler frame; put structured context in
  `logmeta`, not in args auto-capture
