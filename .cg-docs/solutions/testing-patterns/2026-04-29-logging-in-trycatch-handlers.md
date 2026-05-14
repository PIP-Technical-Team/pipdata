---
date: 2026-04-29
title: "Logging inside tryCatch error handlers and lapply callbacks with pipfun typed wrappers"
category: "testing-patterns"
language: "R"
tags: [logging, pipfun, log_info, log_error, capture_log_args, tryCatch, lapply, logmeta, args]
root-cause: "capture_log_args() uses sys.function(-2) to find the calling function, which resolves to the anonymous error handler (function(e)) inside tryCatch, not the enclosing named function"
severity: "P1"
---

# Logging inside tryCatch error handlers and lapply callbacks with pipfun typed wrappers

## Problem

When `log_info()` or `log_error()` is called inside a `tryCatch` error handler,
the `args` column in the `piplog` row contains garbage instead of the enclosing
function's arguments:

```r
pipdata_get_gmd <- function(inv_gmd_list, check_missing) {
  for (i in seq_len(nrow(inv_gmd))) {
    country <- inv_gmd[["Country"]][i]
    year    <- inv_gmd[["Year"]][i]
    # ...
    tryCatch(
      { dlw::dlw_get_gmd(...) },
      error = function(e) {
        # PROBLEM: log_error() here captures list(e = <condition>)
        # NOT list(country = "CHN", year = 2015, ...)
        pipfun::log_error(msg, name = "pipdata_log", logmeta = list(...))
      }
    )
  }
}
```

The same issue occurs inside `lapply()` callbacks:

```r
new_inv <- lapply(seq_len(nrow(gmd_new)), function(i) {
  out <- tryCatch(
    { pipload::load_dlw_data(...) },
    error = function(e) {
      # PROBLEM: same issue — two levels deep (lapply + tryCatch)
      pipfun::log_error(msg, ...)
      NULL
    }
  )
})
```

## Root Cause

`pipfun` typed wrappers (`log_info`, `log_error`, `log_warn`) delegate to
`capture_log_args(helper_name, .env)`, which uses `sys.function(-2)` to
find "the function that called the helper" and captures its formal arguments.

Inside a `tryCatch` error handler `function(e)`, `sys.function(-2)` resolves
to that anonymous closure. Its only formal argument is `e` (the condition
object). So `args` gets `list(e = <simpleError>)` instead of the enclosing
named function's context.

The call stack when `capture_log_args` runs:

| Frame | Function |
|-------|----------|
| -0 | `capture_log_args()` |
| -1 | `log_error()` — the helper (skipped by `identical()` check) |
| -2 | `function(e)` — the anonymous error handler ← captured |
| -3 | (tryCatch machinery) |
| -4 | `pipdata_get_gmd()` — the function we actually want |

## Solution

**Put all structured data in `logmeta`. Accept that `args` will contain the
anonymous handler's `e` argument — it's diagnostic garbage, nobody reads it.**

This is the accepted design decision for pipdata. The `args` column is
auto-populated as a debugging aid; `logmeta` is the contract that
`log_report()` and all report builders read.

```r
# CORRECT — all meaningful context in logmeta
error = function(e) {
  pipfun::log_error(
    msg,
    name    = "pipdata_log",
    logmeta = list(
      error         = "dlw_download_inf",   # type discriminator (always a string)
      survey        = survey_id,            # what was being processed
      country       = country,
      year          = year,
      module        = md_type,
      condition_msg = conditionMessage(e)   # root cause from the dependency
    )
  )
}
```

Key rules:
1. `logmeta$error` / `logmeta$info` is **always a string** type discriminator —
   never a condition object. (Old pattern: `logmeta = list(error = e)` is wrong.)
2. Store the condition message in `logmeta$condition_msg = conditionMessage(e)`.
3. Store all survey/context identifiers that were in `args` into `logmeta`.

### Before vs After

```r
# BEFORE (old pattern) — relies on args auto-capture (broken in handlers)
pipfun::log_add(
  "error",
  msg,
  name    = "pipdata_log",
  args    = list(country = country, year = year, module = md_type),
  logmeta = list(error = e)                          # ← condition object!
)

# AFTER (new pattern) — everything in logmeta, typed wrapper
pipfun::log_error(
  msg,
  name    = "pipdata_log",
  logmeta = list(
    error         = "dlw_download_inf",              # ← always a string
    country       = country,
    year          = year,
    module        = md_type,
    condition_msg = conditionMessage(e)              # ← from the condition
  )
)
```

## Prevention

**Convention for all pipdata logging inside error handlers / callbacks:**

1. Always use `log_info()` / `log_error()` typed wrappers, never raw `log_add()`.
2. Never pass `args = list(...)` explicitly — either let auto-capture run
   (for top-level function calls) or put the data in `logmeta`.
3. `logmeta$error` / `logmeta$info` must be a string constant defined in `aaa.R`
   (e.g., `.logtype_dlw_download`). Never pass the caught condition object as
   this field.
4. Always include `condition_msg = conditionMessage(e)` in error handler
   `logmeta` so the root cause is preserved.
5. For top-level `log_info()` calls (not inside error handlers), auto-capture
   works correctly — no `args` parameter needed, no special handling.

### Signal to watch for

If a `piplog` entry's `args` column contains `list(e = <simpleError>)`, it was
logged inside a `tryCatch` error handler. This is expected and harmless —
check `logmeta` for the actual structured data.

### `parse_log_meta()` guard

`parse_log_meta()` in `log_report.R` uses `vapply(..., character(1))` to extract
`logmeta$error` and `logmeta$info`. This **requires** these fields to be
character strings, not condition objects. The NEW convention enforces this.

If you ever load logs from before this convention was established (unlikely —
this was implemented before the pipeline went to production), `vapply` would
error. In that case, add a guard:
```r
val <- x$error
if (!is.character(val) || length(val) != 1L) return(NA_character_)
return(val)
```

## Related

- [testing-patterns/2026-04-27-contract-testing-for-logging-side-effects.md](./2026-04-27-contract-testing-for-logging-side-effects.md) — how to write contract tests for functions with external I/O that log inside handlers
- [testing-patterns/2026-04-07-synthetic-piplog-testing-pattern.md](./2026-04-07-synthetic-piplog-testing-pattern.md) — how to build synthetic `piplog` objects for testing builders
- Plan: [.cg-docs/plans/2026-04-29-unified-logging-v2.md](../../plans/2026-04-29-unified-logging-v2.md) — canonical logmeta type definitions and DLW refactoring steps
