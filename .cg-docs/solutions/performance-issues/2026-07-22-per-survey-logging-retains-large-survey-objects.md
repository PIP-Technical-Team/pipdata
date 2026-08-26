---
date: 2026-07-22
title: "Per-survey typed logging can retain large survey objects and blow up RAM"
category: "performance-issues"
language: "R"
tags: [memory, performance, logging, pipfun, capture_log_args, .piplogenv, pd_process_data, apply_recode_spec, recode_spec, OOM]
root-cause: "pipfun typed log wrappers capture the caller's formals by reference into the persistent session log; when a hot per-survey function has a large data.table formal like dt, each log call can keep that survey alive and defeat garbage collection across the run"
severity: "P1"
---

# Per-survey typed logging can retain large survey objects and blow up RAM

## Problem

Full `pd_process_data()` runs on the `var_tabmaker2` branch showed runaway RAM
use and eventual OOM/session aborts even after the pipeline started cleaning up
survey intermediates more aggressively.

The symptom was misleading because the obvious suspects were already known:
- `process_data()` held several survey-sized objects (`df`, `ls_cpfw`,
  `ls_clean`, `metadata`)
- `apply_recode_spec()` had recently added YAML-driven recoding to every survey
- `save_pip_data()` failures surfaced as heap pressure / allocation errors

The decisive clue was that memory still grew across the run unless a planned
per-survey `log_info()` call inside `apply_recode_spec()` was avoided.

## Root Cause

`pipfun::log_info()`, `log_warn()`, and `log_error()` use
`capture_log_args()` to snapshot the calling function's formal arguments into
persistent session log state (`.piplogenv`).

That behavior is usually harmless for small scalar arguments, but it becomes a
memory trap in hot per-survey functions whose formals include a large mutable
object such as `dt`.

In this case:

```r
apply_recode_spec <- function(dt, alias = "pip_inv", verbose = TRUE,
                              recode_spec = NULL) {
  ...
}
```

If `apply_recode_spec()` emits a typed log event once per survey, the wrapper
captures a reference to `dt` into the persistent log. Across a 4000+ survey run,
that means the session log can retain thousands of cleaned survey objects.
Explicit `rm()` and `gc()` in the surrounding pipeline then become ineffective,
because the objects are still reachable through `.piplogenv`.

This was the real reason the remaining RAM growth persisted after the more
obvious per-survey cleanup work.

## Solution

Do **not** emit typed per-survey log events from hot functions whose formals
include large survey objects.

For `apply_recode_spec()`, the correct fix was to keep provenance out of the
session log and attach it directly to the data/inventory instead:

```r
# NOTE: do NOT emit a per-survey `log_info()` here. The pipfun log wrappers
# (log_info/log_warn/log_error) capture *all* of the calling function's
# formals by reference into the persistent session log (.piplogenv) via
# capture_log_args(). Inside this function that would stash a reference to
# `dt` (the cleaned survey, hundreds of MB) on every survey, defeating gc()
# and blowing up RAM across a full-inventory run. Spec provenance is already
# carried by the `recode_spec_version_id` attribute (below) and the
# `version_id_recode_spec` inventory column written by build_pip_inventory().

data.table::setattr(dt, "recode_spec_version_id", version_id)
```

Two supporting changes matter here:

1. `pd_process_data()` now resolves the recode spec once upstream and threads it
   through the call chain, removing redundant per-survey stamp/catalog I/O.
2. `process_data()` explicitly frees `df`, `ls_cpfw`, `ls_clean`, and
   `metadata`, then calls `gc(verbose = FALSE)` after saving.

Those help, but the critical lesson from this incident is that **logging itself
can be the retention root**.

## Prevention

Use this rule for any hot pipeline path that runs once per survey, per file, or
per row-group:

- Do not call `pipfun` typed log wrappers from a function whose formals include
  large `data.table`, list, or model objects unless you have verified that
  retaining those references is acceptable.
- If you need provenance for a large object, prefer object attributes,
  inventory columns, or a compact derived summary over a full typed log event.
- Keep typed logging at orchestration boundaries where arguments are small and
  stable, such as `pd_process_data()` summary logging after the run.
- When diagnosing unexplained memory growth, inspect whether persistent logging
  state is retaining objects that `rm()`/`gc()` appear to have freed.

Anti-pattern:

```r
# BAD in a hot per-survey function with dt as a formal
pipfun::log_info(
  "Applied recode spec.",
  name = "pipdata_log",
  logmeta = list(info = "recode_spec_applied", version_id = version_id)
)
```

Preferred pattern:

```r
# GOOD: compact provenance attached to the artifact, not the persistent log
data.table::setattr(dt, "recode_spec_version_id", version_id)
```

## Related

- `.cg-docs/plans/2026-06-24-pd-process-data-oom-crash.md` — the original OOM
  investigation; later refined when the logging retention root cause was
  confirmed
- `.cg-docs/plans/2026-06-28-pd-process-data-ram-regression.md` — the later RAM
  regression plan that connected spec threading, per-survey cleanup, and the
  `apply_recode_spec()` hot path
- `.cg-docs/solutions/testing-patterns/2026-04-29-logging-in-trycatch-handlers.md`
  — related `capture_log_args()` behavior in handler/callback frames
- `.cg-docs/solutions/environment-issues/2026-04-30-unified-package-environment-accessor-pattern.md`
  — background on shared package/session state and `.pipdataenv`
- `.cg-docs/solutions/data-quality/2026-08-26-durable-stage-reconciliation.md`
  — applies orchestration-boundary logging alongside fail-closed artifact and
  inventory reconciliation
