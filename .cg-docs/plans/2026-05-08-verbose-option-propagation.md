---
date: 2026-05-08
title: "Propagate pipdata.verbose option to downstream package calls"
status: active
scope: "Lightweight"
brainstorm: null
language: R
estimated-effort: small
tags: [verbose, options, pipload, stamp, ux, code-quality]
completed-phases: [1, 2]
current-phase: 3
---

# Plan: Propagate `pipdata.verbose` Option to Downstream Package Calls

## Objective

Make `getOption("pipdata.verbose")` control verbosity of all downstream
`pipload`/`stamp` calls (e.g., `pip_read()`, `pip_write()`,
`load_pip_master_inventory()`, `load_aux_data()`). Currently most calls
hard-code `verbose = FALSE`, silencing all output even when the user has
set `options(pipdata.verbose = TRUE)`.

This is NOT about the structured pipeline logging (`piplog`/`logmeta`)
— it is about user-facing informational messages emitted by stamp and
pipload during I/O operations.

## Current State

| Function | Current Pattern | Lines |
|----------|----------------|-------|
| `pd_deflation()` / `.load_deflation_aux()` | No `verbose` arg; `pipload::pip_read()` called without `verbose` (defaults to stamp's own default) | ~62, 107, 140, 234 |
| `pd_process_data()` | Has `verbose` param → passes `verbose = FALSE` to `load_aux_data()` | ~54 |
| `valid_dlw_load()` | Has `verbose` param → passes `verbose = FALSE` to `load_pip_master_inventory()` | ~166, 255 |
| `update_pip_inventory()` | No `verbose` arg; passes `verbose = FALSE` to `load_aux_data()`, `pip_write()`, `load_pip_master_inventory()` | ~88, 141, 148, 183, 198, 260, 268 |
| `valid_aux_load()` | Passes `verbose = FALSE` to `pipaux::compare_aux_vintages()` | ~36, 69 |
| `save_pip()` | No `verbose` arg; `pip_write()` called without `verbose` | ~39 |
| `pipdata_dlw_process()` | Passes `verbose = FALSE` to delegate | ~56 |
| `pipdata_get_gmd()` | `pip_write()` called without `verbose` | ~156 |
| `pipdata_validate_gmd()` | `pip_write()` called without `verbose` | ~283, 363 |
| `pipdata_copy_dlw_meta()` | Passes `verbose = FALSE` to `get_wrk_release()`, `pip_write()` | ~49, 76, 82 |

## Design Decisions

1. **Single option**: `getOption("pipdata.verbose", default = TRUE)` (already
   defined in `zzz.R`).
2. **Default fallback alignment**: `zzz.R` sets `pipdata.verbose = TRUE`.
   All function formals must use `default = TRUE` to match. The existing
   functions `pd_process_data()` and `valid_dlw_load()` currently use
   `default = FALSE` — Phase 2 must fix this inconsistency.
3. **Internal helpers receive `verbose` as a parameter**: exported functions
   resolve the option and pass the value down to internal helpers as an
   explicit argument. Helpers do NOT read `getOption()` themselves. This
   ensures `pd_deflation(verbose = FALSE)` silences `.load_deflation_aux()`
   even when the global option is `TRUE`.
4. **Pattern**: `verbose = getOption("pipdata.verbose", default = TRUE)` in
   the function formals of exported functions. Internal helpers accept
   `verbose` as a parameter (no default — caller must supply it).
5. **Propagation**: pass the resolved `verbose` value to all `pipload`/`stamp`
   calls that accept a `verbose` argument.
6. **Default value**: keep `TRUE` (current default in `zzz.R`) so interactive
   users see I/O messages. Pipeline orchestration scripts set
   `options(pipdata.verbose = FALSE)` at session start.
7. **Batch-internal calls stay silent**: some downstream calls inside batch
   pipeline functions are high-frequency or low-value for the user. These
   remain `verbose = FALSE` unconditionally:
   - `pd_process_data()` → `lapply(aux_measures, load_aux_data)` (6 calls,
     once per pipeline run — not per survey; moderate volume).
   - `joyn::anti_join(..., verbose = FALSE)` in `valid_dlw_load()` — join
     diagnostics, not I/O messages; out of scope entirely.
   All other downstream calls propagate the resolved `verbose` value.

## Phase 1: Add `verbose` to `pd_deflation()` and propagate

### Tasks

1. Add `verbose = getOption("pipdata.verbose", default = TRUE)` to
   `pd_deflation()` formals.
2. Add `verbose` parameter to `.load_deflation_aux()` (no default —
   caller must supply). `pd_deflation()` passes its resolved `verbose`
   value to `.load_deflation_aux(verbose = verbose)`.
3. Pass `verbose` to all `pipload::pip_read()` and
   `pipload::load_pip_master_inventory()` calls inside
   `.load_deflation_aux()`.
4. Pass `verbose` to `pipload::pip_read()` in Mode B (survey load) inside
   `pd_deflation()` itself.
5. Update roxygen `@param verbose` documentation on `pd_deflation()`.
6. Add a test that `pd_deflation(verbose = FALSE)` suppresses messages from
   mocked `pip_read`/`load_pip_master_inventory`.

### Acceptance Criteria

- `pd_deflation(pip_id = "BOL_2022_EH_INC_ALL")` with
  `options(pipdata.verbose = TRUE)` prints stamp loading messages.
- Same call with `options(pipdata.verbose = FALSE)` is silent.
- `pd_deflation(verbose = FALSE)` silences downstream calls even when
  `getOption("pipdata.verbose")` is `TRUE` (per-call override works).

## Phase 2: Audit remaining exported functions

### Tasks

1. For each exported function in the table above that does NOT yet have
   `verbose`: add the parameter and propagate to downstream calls.
2. For functions that already have `verbose` but hard-code `FALSE` in
   downstream calls: replace `FALSE` with the resolved `verbose` value.
3. Update relevant tests (mock-based, no network).

### Specific changes

| Function | Change |
|----------|--------|
| `pd_process_data()` | Already has `verbose` → fix fallback from `default = FALSE` to `default = TRUE`; keep `load_aux_data(verbose = FALSE)` (batch-internal, stays silent per Design Decision 7); propagate `verbose` to `valid_dlw_load()` and `update_pip_inventory()` calls |
| `valid_dlw_load()` | Already has `verbose` → fix fallback from `default = FALSE` to `default = TRUE`; propagate to `load_pip_master_inventory()` calls (keep `joyn::anti_join(verbose = FALSE)` — out of scope) |
| `update_pip_inventory()` | Add `verbose` param; propagate to `pip_write()`, `load_pip_master_inventory()`, `load_aux_data()` |
| `save_pip()` | Add `verbose` param; propagate to `pip_write()` |
| `valid_aux_load()` | Add `verbose` param; propagate to `pipaux::compare_aux_vintages()` |
| `pipdata_dlw_process()` | Already has some verbose handling → audit delegate calls (`pipdata_get_gmd`, `pipdata_validate_gmd`) and pass `verbose` through |
| `pipdata_get_gmd()` | Add `verbose` param; propagate to `pip_write()` |
| `pipdata_validate_gmd()` | Add `verbose` param; propagate to `pip_write()` |
| `pipdata_copy_dlw_meta()` | Add `verbose` param; propagate to `pip_write()`, `get_wrk_release()` |

### Acceptance Criteria (Phase 2)

- All exported functions use `default = TRUE` fallback consistently.
- `pd_process_data(verbose = FALSE)` silences all propagated downstream
  calls (except batch-internal `load_aux_data` which stays silent always).
- No regressions in existing tests.

## Phase 3: Documentation and orchestration

### Tasks

1. Document `pipdata.verbose` in `?pipdata-package` or a dedicated
   `?pipdata-options` help page.
2. Update `Pipdata_script.R` (or its replacement) to show
   `options(pipdata.verbose = FALSE)` as the recommended pattern for
   batch runs.
3. Update `.cg-docs/compound-gpid.context.md` domain rules if needed.

## Out of Scope

- The structured logging system (`piplog`/`logmeta`, `log_add()`,
  `log_info()`) — this is tracked separately under `unified-logging-report`.
- Adding a `quiet` mode that suppresses CLI warnings/aborts — verbose only
  controls informational messages.
- Changes to downstream packages (pipload, stamp, pipaux) — we only
  control what we pass to them.
