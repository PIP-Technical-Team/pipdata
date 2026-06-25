---
date: 2026-06-24
title: "pd_process_data: OOM crash fix for 4000+ survey runs"
status: active
scope: "Lightweight"
brainstorm: null
language: "R"
estimated-effort: "small"
deviation-policy: "ask"
tags: [pd_process_data, memory, gc, oom, reliability, pipeline]
---

# Plan: pd_process_data — OOM Crash Fix for Large Inventory Runs

## Objective

Stop R session crashes (`cannot allocate vector of size X` / session abort)
when `pd_process_data()` is called with a full inventory of 4000+ surveys.

## Root Cause

### Primary: deferred GC accumulates survey-sized garbage

`pd_process_data.R:81–86` uses `lapply(inv_ls, process_data, ...)`.
Inside each `process_data()` call, four large objects are created and written:

| Object | Typical size | freed when |
|--------|-------------|------------|
| `df` (loaded survey) | 30–200 MB | never explicitly — waits for lazy GC |
| `ls_cpfw` (PFW-merged list) | 30–200 MB | same |
| `ls_clean` (cleaned list, 1–2 variants) | 60–400 MB | same |
| `metadata` (aux-attr list) | 10–50 MB | same |

R's garbage collector is lazy: it only runs when it needs memory.
With 4000 surveys processed sequentially inside a single `lapply()`, the heap
accumulates the garbage of all previous surveys (already saved to disk but not
yet freed) while allocating the next. At survey ~500–1000, `qs2::qs_save()`
requests a multi-threaded compression buffer and the request fails because the
heap is too fragmented even if total RSS is below the OS limit.

```
→ ERROR cannot allocate buffer of size X
  Metadata: list(error = "save_error", id_name = "ARG_2014_EPHC-S2_INC_ALL", ...)
```

### Secondary: no cross-survey GC in the main loop

`save_pip_data()` already calls `gc(verbose = FALSE)` per artifact above
100 MB (`save_pip.R:54–57`). But:
- Most surveys are 10–80 MB → threshold never triggered.
- `gc()` runs *inside* `save_pip_data()` while `ls_clean` and `metadata` are
  still alive in the calling frame; the collector cannot free them yet.

### Tertiary: logging accumulation in `.piplogenv`

Two issues in `pipfun` logging contribute secondarily:

**a) `capture_log_args` snapshots `inv` by reference**
`log_info()` (log_helpers.R:96) calls `capture_log_args(log_info, .env)` where
`.env = parent.frame()` is `pd_process_data()`'s environment.
`capture_log_args` derives the parent function as `pd_process_data` and calls
`mget(names(formals(pd_process_data)), envir = .env)` — capturing `inv`,
`aux_measures`, `force`, `verbose` into the log's `args` list column. `inv` is
a reference object (data.table), so this does **not** copy the rows, but the
reference in `.piplogenv` keeps `inv` alive in the session log until
`log_reset()` is called.

**b) O(N²) `rbindlist` growth pattern in `log_add`**
Every `log_add()` call (log.R:133) does
`log <- rbindlist(list(log, new_row), ...)` and pokes the result back into
`.piplogenv`. This allocates a brand-new data.table on each call. For N error
entries that is O(N²) total allocation. In the happy path (few errors) this is
negligible, but when `save_error` failures cascade — exactly the fragmented-heap
scenario — each new failure triggers another `rbindlist`, amplifying heap
fragmentation further. This is a **pipfun** issue and is tracked separately; it
is out of scope here but noted as a multiplier.

### Contributing: two `copy()` calls per survey

`pd_dlw_clean.R` calls `copy(df)` at the start of `dlw_clean.pipmd()` and
`dlw_clean.pipgd()`. Surveys with two welfare variants additionally copy in
`pd_split_alt_welfare.R`. A single survey briefly holds 3–4× its loaded size
simultaneously.

## Requirements

| ID | Requirement |
|----|-------------|
| R1 | Large locals inside `process_data()` must be explicitly freed after saves, before the function returns |
| R2 | A `gc()` must be called after those `rm()` calls so freed memory is reclaimed before the next survey loads |
| R3 | The main loop must call `gc()` periodically (every N surveys) as a belt-and-suspenders safety net |
| R4 | N must be configurable via `getOption("pipdata.gc_interval", 50L)` |
| R5 | No new package imports; base R only |
| R6 | The return contract of `process_data()` must not change: still returns `list(pip_names = ...)` on success, `NULL` on failure |
| R7 | Roxygen `@details` on `pd_process_data()` and `process_data()` updated |
| R8 | `NEWS.md` bullet added |

## Implementation Steps

### Step 1 — Explicit cleanup inside `process_data()` (highest leverage)

- **Requirements**: R1, R2, R6
- **File**: `R/pd_process_data.R`
- **Where**: inside the `tryCatch` `expr` block, after both `save_pip_data()`
  calls (currently lines 209–210), before `list(pip_names = ...)`.
- **Change**:

```r
# Save clean data and metadata (side effect; version facts read from stamp)
save_pip_data(ls_clean, alias = "pip", verbose = verbose)
save_pip_data(metadata, alias = "pip_meta", verbose = verbose)

# Capture result, then free large objects before returning so R can
# reclaim memory before the next survey is loaded by the caller.
result <- list(pip_names = names(ls_clean))
rm(df, ls_cpfw, ls_clean, metadata)
gc(verbose = FALSE)
result
```

- **Why `result` before `rm()`**: `names(ls_clean)` must be evaluated before
  `ls_clean` is removed.
- **Acceptance criteria**: at the end of each successful `process_data()` call
  (confirmed via `tracemem` / `lobstr::mem_used()` probe), heap does not
  retain the four objects.

---

### Step 2 — Periodic `gc()` in the main loop

- **Requirements**: R3, R4, R5
- **File**: `R/pd_process_data.R`
- **Where**: replace `lapply(inv_ls, process_data, ...)` (lines 81–86) with a
  `for` loop that calls `gc()` every `gc_interval` surveys.
- **Change**:

```r
gc_interval <- getOption("pipdata.gc_interval", default = 50L)
results <- vector("list", length(inv_ls))
names(results) <- names(inv_ls)

for (i in seq_along(inv_ls)) {
  results[[i]] <- process_data(inv_ls[[i]], aux_list = aux_list, verbose = verbose)
  if (i %% gc_interval == 0L) gc(verbose = FALSE)
}
```

- **Why `for` instead of `lapply`**: `lapply` does not allow inserting
  side-effects between iterations without `<<-` hacks. A `for` loop is
  idiomatic here and produces an identical result list.
- **Acceptance criteria**: `results` has the same names and values as the
  current `lapply` output; memory usage no longer grows monotonically.

---

### Step 3 — Update roxygen documentation

- **Requirements**: R7
- **File**: `R/pd_process_data.R` roxygen for `pd_process_data()` and
  `process_data()`
- **Change**: add a `@details` note to `pd_process_data()`:

```
#' @details
#' **Memory management**: surveys are processed one at a time.  After each
#' survey is saved, large intermediates (`df`, `ls_cpfw`, `ls_clean`,
#' `metadata`) are explicitly removed and `gc()` is called before the next
#' survey is loaded.  Additionally, `gc()` is called every
#' `getOption("pipdata.gc_interval", 50L)` surveys in the main loop.
```

---

### Step 4 — NEWS.md bullet

- **Requirements**: R8
- **File**: `NEWS.md`
- **Change**: add under `# pipdata (development version)`:

```
* fix: `pd_process_data()` now explicitly frees survey intermediates and
  calls `gc()` after each save, preventing OOM crashes on full-inventory runs
  of 4000+ surveys
```

## Testing Strategy

No new test file required. Manual verification:

1. Run `pd_process_data(inv = inv, verbose = TRUE)` on a 50-survey subset.
   Confirm identical output shape to current behaviour.
2. Monitor heap with `lobstr::mem_used()` or `gc()` return value (column
   `"used"`) before and after individual surveys — peak should not grow
   unboundedly.
3. Run the full 4000+ inventory. Session must complete without abort.

Optional regression test (deferred):
- Add `tests/testthat/test-pd_process_data.R` with a mocked `process_data()`
  that asserts `results` is a named list of the correct length.

## Documentation Checklist

- [ ] `@details` added to `pd_process_data()` describing GC strategy
- [ ] `NEWS.md` bullet added

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| `rm(df, ls_cpfw, ls_clean, metadata)` errors if a variable was never assigned (e.g. error path skipped assignment) | Low | Medium | Variables are only `rm()`'d inside the success branch of `tryCatch`; all four are guaranteed to exist there |
| `gc()` after every successful survey adds measurable wall-clock overhead | Low | Low | `gc()` is O(heap size), typically <50 ms; Step 2's periodic GC provides further control via `gc_interval` option |
| `for` loop vs `lapply` behaves differently on errors | None | None | `process_data()` already wraps all errors internally and returns `NULL`; the loop collects `NULL` identically |
| `gc_interval = 50` too aggressive for small dev runs | Low | Low | Option is user-configurable; default 50 is conservative |

## Out of Scope

- Removing `copy()` calls in `pd_dlw_clean.R` (correctness risk)
- Batching / chunked processing (deferred; Step 1+2 should suffice)
- `nthreads` tuning in `qs2`
- Retry-on-failure logic
- Parallelisation
- **pipfun** `log_add` O(N²) `rbindlist` pattern (tracked separately in pipfun)
- **pipfun** `capture_log_args` argument capture scope (tracked separately in pipfun)

---

## Completion Contract

### Outcome

`pd_process_data()` completes a full 4000+ survey run without R session abort.
Peak heap no longer grows unboundedly across surveys.

### Verification Surface

| ID | Evidence Required | Required |
|----|-------------------|----------|
| V1 | Full inventory run completes without crash | yes |
| V2 | Code review confirms `rm()` + `gc()` in `process_data()` success branch | yes |
| V3 | Code review confirms `for` loop with periodic `gc()` in main loop | yes |

### Constraints

| ID | Constraint |
|----|------------|
| C1 | No new imports |
| C2 | `process_data()` return contract unchanged |
| C3 | `gc_interval` configurable via option |

### Boundaries

- **Allowed**: `R/pd_process_data.R` (cleanup + loop + roxygen), `NEWS.md`
- **Out of scope**: `pd_dlw_clean.R`, `pd_cpfw_merge.R`, `save_pip.R`,
  `pipload`, `stamp`, `qs2`
