---
date: 2026-06-28
title: "pd_process_data: RAM regression fix (spec-once + per-survey gc)"
status: completed
scope: "Lightweight"
language: "R"
estimated-effort: "small"
deviation-policy: "ask"
tags: [pd_process_data, recode_spec, memory, gc, performance, regression]
---

# Plan: Fix the `pd_process_data()` RAM regression

## Context

Running `pd_process_data()` on the current `var_tabmaker2` branch makes RAM
explode, whereas commit `3e00987a` did not. The diff since that commit landed a
large inventory-architecture refactor that introduced a YAML-driven recode
engine (`recode_spec.R` / `inst/extdata/recode_spec.yml`) and moved version
facts into the stamp catalog. Three changes in that work regressed memory/time:

1. **Per-survey redundant stamp I/O.** `apply_recode_spec()` runs once per
   survey inside `dlw_clean.pipmd()` (`R/pd_dlw_clean.R:67`) and on *every* call
   does both `load_stamp_recode_spec()` (a `pipload::pip_read` that deserializes
   the whole `pip_inv` catalog + the spec file) **and**
   `stamp::st_catalog_query("pip_inv")` (deserializes the whole catalog again) —
   see `R/recode_spec.R:393-408`. The spec is *already* synced once at
   `R/pd_process_data.R:76` via `sync_recode_spec()`, whose return value is then
   discarded. For a 4000-survey run that is ~8,000 needless catalog
   reads/deserializations — the main *time* regression and a major source of
   heap churn/fragmentation. (`st_catalog_query` reads the entire catalog from
   disk every call — `stamp/R/version_store.R:1257`, `.st_catalog_read` at :156.)

2. **Deferred GC, now amplified.** `pd_process_data()` uses
   `lapply(inv_ls, process_data, …)` and `process_data()` never frees the four
   survey-sized intermediates (`df`, `ls_cpfw`, `ls_clean`, `metadata`). R's lazy
   GC lets that garbage pile up across surveys. The dedicated OOM-fix plan
   (`.cg-docs/plans/2026-06-24-pd-process-data-oom-crash.md`) was written but
   never implemented — only the `save_pip.R` gc-threshold landed. The new wider
   cleaned tables (the spec now defines 45 variables / ~20 new derived columns
   vs the old ~5 recodes) make this pile-up hit the limit far sooner than at
   baseline.

**Scope decision (confirmed with user):** safe, output-preserving performance
fixes only. The cleaned data keeps all 45 spec variables unchanged. We do **not**
trim derived columns, and we **keep `lapply`** (a `for`-loop rewrite is only a
fallback, see Fix 2).

## Approach

### Fix 1 — Resolve the recode spec once and thread it through the call chain

Eliminate all per-survey stamp I/O in `apply_recode_spec()` by passing the
already-synced spec down from `pd_process_data()`.

- **`R/pd_process_data.R`** (~line 76): capture the sync result:
  ```r
  recode_spec <- sync_recode_spec(alias = "pip_inv", verbose = verbose)
  ```
  Pass it into the per-survey call:
  `process_data(…, recode_spec = recode_spec, …)`.

- **`process_data()`** (same file): add `recode_spec = NULL` arg; forward to
  `pd_dlw_clean(ls_cpfw, recode_spec = recode_spec, verbose = verbose)`.

- **`R/pd_dlw_clean.R`**: add `recode_spec = NULL` to `pd_dlw_clean()`,
  `dlw_clean()` (generic), and `dlw_clean.pipmd()`. Forward through
  `purrr::map(.x = ls, .f = dlw_clean, recode_spec = recode_spec, verbose = verbose)`
  and into `apply_recode_spec(md, recode_spec = recode_spec, verbose = verbose)`.
  `dlw_clean.pipgd()` absorbs it via `...` (it doesn't recode).

- **`R/recode_spec.R` `apply_recode_spec()`**: add a `recode_spec = NULL`
  parameter. When supplied, read `spec <- recode_spec$spec$variables` and
  `version_id <- recode_spec$version_id` directly — **skip** both
  `load_stamp_recode_spec()` and `st_catalog_query()`. When `NULL`, fall back to
  the current stamp-loading path so existing direct callers and the tests in
  `tests/testthat/test-recode-spec.R` (which call `apply_recode_spec(dt,
  verbose = FALSE)` with no spec) keep working unchanged.

This removes ~8,000 catalog deserializations per full run with zero change to
output.

### Fix 2 — Free intermediates per survey (keep `lapply`)

The highest-leverage memory step from the existing OOM plan
(`.cg-docs/plans/2026-06-24-pd-process-data-oom-crash.md`, Step 1) is
**loop-agnostic**: freeing the survey-sized intermediates at the end of each
`process_data()` call works identically whether the outer loop is `lapply` or
`for`. So we **keep the existing `lapply`** and only change `process_data()`.

- **`process_data()` success branch** (after both `save_pip_data()` calls,
  before the return list): capture the result first, then free and collect:
  ```r
  result <- list(pip_names = names(ls_clean))
  rm(df, ls_cpfw, ls_clean, metadata)
  gc(verbose = FALSE)
  result
  ```
  (`result` must be built before `rm()` because it reads `names(ls_clean)`.)
  This drops the heap back to baseline after every survey, so garbage no longer
  accumulates across the `lapply` — without touching the loop itself.

- **Main loop:** unchanged. `lapply(inv_ls, process_data, recode_spec = …,
  aux_list = …, verbose = …)` stays. `for` vs `lapply` perform identically here
  (per-iteration cost is dominated by `process_data`'s I/O), so there is no
  reason to rewrite it.

The existing `save_pip.R` largest-first ordering + 100 MB gc threshold stays as
is — it complements this.

#### Fallback (only if memory still grows after Fixes 1–2)

If the per-survey `gc()` proves insufficient on the full 4000+ run, convert the
`lapply` to a `for` loop that adds a periodic `gc()` as a safety net — same
result list, identical behaviour, just an extra collection every N surveys:
```r
gc_interval <- getOption("pipdata.gc_interval", default = 50L)
results <- vector("list", length(inv_ls)); names(results) <- names(inv_ls)
for (i in seq_along(inv_ls)) {
  results[[i]] <- process_data(inv_ls[[i]], aux_list = aux_list,
                               recode_spec = recode_spec, verbose = verbose)
  if (i %% gc_interval == 0L) gc(verbose = FALSE)
}
```
This is a backup, not part of the initial change.

## Files to modify

- `R/pd_process_data.R` — capture sync result, thread `recode_spec`, add
  per-survey `rm`/`gc` in `process_data()` (keep `lapply`).
- `R/pd_dlw_clean.R` — thread `recode_spec` through `pd_dlw_clean` /
  `dlw_clean` / `dlw_clean.pipmd`.
- `R/recode_spec.R` — `apply_recode_spec()` accepts a pre-resolved
  `recode_spec`, with stamp fallback.
- `NEWS.md` — one bullet noting the spec-once + gc fix.
- Roxygen `@param`/`@details` updates on the touched functions (regenerate
  `man/` with `devtools::document()` — no hand edits).

## Out of scope

- Trimming/gating the 45 derived columns (user: keep all).
- Removing the defensive `copy(df)` in `dlw_clean.*` (correctness risk).
- Any change to `stamp` (catalog caching) — Fix 1 makes per-survey catalog reads
  disappear, so a stamp-level cache is unnecessary.
- Rewriting `lapply` to `for` as part of the main change (fallback only).

## Deferred / Not Implemented

- **`NEWS.md` bullet.** Originally planned under "Files to modify", but
  deliberately skipped: the package is still under active development with no
  external users yet, so a changelog entry for this internal fix was judged
  unnecessary (user decision, 2026-08-03).
- **Fallback `for`-loop with periodic `gc()`.** Documented in Fix 2 as a
  backup only if the per-survey `gc()` proved insufficient on the full
  4000+ run. The full run completed successfully with Fixes 1–2 alone (see
  `log_reports/log_report.md`), so the fallback was never needed and
  `lapply` was kept as-is.

## Verification

1. **Unit tests:** `devtools::test(filter = "recode-spec")` — the no-spec
   fallback path must keep all `tests/testthat/test-recode-spec.R` green.
   Run `devtools::test()` for the broader suite (`pd-deflation`, etc.).
2. **Equivalence:** on a ~30–50 survey subset, run `pd_process_data(inv, verbose
   = TRUE)` and confirm the returned `new_pip_inv` and the saved cleaned columns
   are identical to the current branch's output (spec threading must not alter
   results).
3. **Memory/time:** wrap the same subset run with `gc()`-delta probes (or
   `lobstr::mem_used()` before/after each survey); peak heap should no longer
   grow monotonically. Time per survey should drop noticeably now that the two
   per-survey catalog reads are gone.
4. **Full run:** execute the 4000+ inventory via `Pipdata_script.R`
   (`pd_process_data(inv = inv_3)`) and confirm it completes without R aborting.