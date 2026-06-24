---
date: 2026-06-18
title: "save_pip_data: memory-resilient save order and GC guard"
status: done
scope: "Lightweight"
brainstorm: null
language: "R"
estimated-effort: "small"
deviation-policy: "ask"
tags: [save_pip, memory, gc, qs2, reliability, pipeline]
---

# Plan: save_pip_data — Memory-Resilient Save Order and GC Guard

## Objective

Prevent `cannot allocate buffer` failures in `save_pip_data()` when serializing
large survey artifacts (e.g. `ARG_2014_EPHC-S2_INC_ALL`) via `qs2::qs_save()`.
Two changes: sort the iteration order largest-first, and force a GC cycle before
serializing any artifact above a configurable size threshold.

## Context

`qs2::qs_save()` allocates multi-threaded compression buffers in-memory before
flushing to disk. When `save_pip_data()` processes many surveys in sequence, heap
fragmentation accumulates. If a large object arrives late in the loop (when
fragmentation is worst), `cannot allocate buffer` is thrown — the artifact is
logged as `save_error` and skipped silently.

Root cause confirmed via log entry:

```
→ [2026-06-18 08:54:22] ERROR - `cannot allocate buffer`
Metadata: list(error = "save_error", id_name = "ARG_2014_EPHC-S2_INC_ALL",
               status = "The cleaned survey was not saved")
```

The fix is purely additive inside `save_pip.R`:

1. Sort `names(data)` by descending `object.size()` so the largest artifacts are
   written when the heap is cleanest (immediately after the prior `lapply` that
   produced them).
2. Inside the `lapply`, call `gc(verbose = FALSE)` before `pip_write()` whenever
   the object exceeds `getOption("pipdata.gc_threshold_bytes", 100e6)` (100 MB).

No new imports — `object.size()` and `gc()` are base R.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Iteration order in `save_pip_data()` must be largest-first by `object.size()` | This plan |
| R2 | A `gc()` must fire before `pip_write()` when the object exceeds `getOption("pipdata.gc_threshold_bytes", 100e6)` | This plan |
| R3 | Threshold must be configurable; default 100 MB | This plan |
| R4 | No new package imports; base R only | Constraint C1 |
| R5 | Roxygen `@param` / `@details` updated to document the new behaviour | Documentation |

## Implementation Steps

### 1. Sort iteration order by descending object size

- **Requirements**: R1
- **Files**: `R/save_pip.R`
- **Details**:
  Before the `lapply`, compute sizes and sort:
  ```r
  survey_sizes <- vapply(names(data), \(y) as.numeric(object.size(data[[y]])), numeric(1))
  sorted_names <- names(sort(survey_sizes, decreasing = TRUE))
  ```
  Replace `lapply(names(data), ...)` with `lapply(sorted_names, ...)`.
  Keep `names(versions) <- sorted_names` (was `names(data)`) so the returned list
  remains named.
- **Test Scenarios**: happy path (list returned contains all names), edge case
  (single-element list), error path (save failure still returns `NULL` for that key)
- **Acceptance criteria**: `names(versions)` matches `sorted_names`; existing
  behaviour for the success/failure return contract is unchanged

### 2. Add GC guard inside the lapply body

- **Requirements**: R2, R3, R4
- **Files**: `R/save_pip.R`
- **Details**:
  Inside the `tryCatch` expr block, before `pip_write()`:
  ```r
  threshold <- getOption("pipdata.gc_threshold_bytes", default = 100e6)
  if (as.numeric(object.size(data[[y]])) > threshold) {
    gc(verbose = FALSE)
  }
  ```
- **Test Scenarios**: object above threshold triggers gc (can assert via mock or
  size check), object below threshold does not
- **Acceptance criteria**: no regression in success/failure return shape; `save_error`
  no longer fires for `ARG_2014_EPHC-S2_INC_ALL` on next full ARG run

### 3. Update roxygen documentation

- **Requirements**: R5
- **Files**: `R/save_pip.R` roxygen block
- **Details**: Add a `@details` note explaining sort order and the GC threshold
  option. Document `getOption("pipdata.gc_threshold_bytes")` inline.
- **Acceptance criteria**: `devtools::document()` runs cleanly; `.Rd` reflects new
  behaviour

## Testing Strategy

No new test file required for this Lightweight plan. Manual verification:

1. Re-run `pd_process_data(inv = inv_ARG, verbose = FALSE)` in the active session.
2. Inspect `pipfun::log_filter(name = "pipdata_log")` — `ARG_2014_EPHC-S2_INC_ALL`
   must not appear with `error = "save_error"`.
3. Confirm `new_pip_inv` is populated for the ARG 2014 survey.

Optional: a `testthat` unit test for sort order using a small named list with
known sizes can be added under `tests/testthat/test-save_pip.R` in a follow-up.

## Documentation Checklist

- [x] Roxygen `@details` added describing sort order and `pipdata.gc_threshold_bytes`
- [x] `NEWS.md` bullet under next release: "fix: `save_pip_data()` now processes
  largest surveys first and runs `gc()` above threshold to prevent
  `cannot allocate buffer` errors (#tbd)"

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Sort adds measurable overhead for large batches (>500 surveys) | Low | Low | `object.size()` is O(n) but called once per object outside the loop |
| GC call on every large survey slows throughput | Low | Medium | Default threshold (100 MB) exempts typical small surveys; raise if needed |
| `sorted_names` diverges from `names(data)` after failure | None | High | `names(versions) <- sorted_names` is set unconditionally after `lapply` |

## Out of Scope

- `nthreads` tuning in `qs2` / `stamp` / `pipload`
- Retry-on-failure logic for `save_error`
- New test file (deferred to follow-up)
- Changes to `pipload`, `stamp`, or `qs2`

---

## Completion Contract

### Outcome

`save_pip_data()` iterates over surveys largest-first and triggers a GC cycle
before writing any artifact above the configurable size threshold, eliminating
the `cannot allocate buffer` error observed for `ARG_2014_EPHC-S2_INC_ALL`.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required |
|----|-------------------|------------------|----------|
| V1 | `ARG_2014_EPHC-S2_INC_ALL` saves without `save_error` in log | Re-run `pd_process_data(inv = inv_ARG)`; inspect `pipfun::log_filter("pipdata_log")` | yes |
| V2 | Iteration order in `save_pip_data()` is largest-first by `object.size()` | Code review of `R/save_pip.R` | yes |
| V3 | GC guard fires only above the size threshold | Manual trace or optional unit test | no |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | No new imports — `object.size()` and `gc()` are base R | `DESCRIPTION` unchanged |
| C2 | GC threshold configurable via `getOption("pipdata.gc_threshold_bytes", 100e6)` | Option documented in roxygen |

### Boundaries

- **Allowed**: modifying `R/save_pip.R` (sort order + gc guard + roxygen)
- **Out of scope**: changes to `pipload`, `stamp`, `qs2` thread tuning, retry logic,
  new test file

### Iteration Policy

1. If sort-order alone resolves V1, the GC guard may be omitted
2. If 100 MB threshold causes GC on every survey in practice, raise it to 500 MB

### Blocked-Stop Conditions

- None — purely additive changes with no breakage risk
