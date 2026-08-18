---
date: 2026-08-17
title: "pd_deflate_pipeline() — Batch Deflation Orchestrator"
status: completed
completed-date: 2026-08-17
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-08-17-deflate-pipeline-wrapper.md"
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
execution-report: ".cg-docs/work-reports/2026-08-17-deflate-pipeline-wrapper.md"
tags: [deflation, pipeline, orchestration, batch-processing, stamp]
---

# Plan: pd_deflate_pipeline() — Batch Deflation Orchestrator

## Objective

Create `pd_deflate_pipeline()`, a batch orchestrator that iterates over a master inventory of cleaned surveys, deflates each via `pd_deflation()` (simple Mode B), correctly detects and skips failures (including `NA` returns), saves results to a new `"pip_deflated"` stamp alias, updates the master inventory with deflation columns, and logs a structured summary. The function operates as an independent pipeline stage — not called by `pd_process_data()`.

## Context

`pd_deflation()` works for single surveys (Mode A: pass `dt`, Mode B: pass `pip_id`). The `integrate-deflation` roadmap item is done. What's missing is the batch wrapper (`deflate-pipeline-wrapper` in roadmap.json, status: `planned`). The brainstorm at `.cg-docs/brainstorms/2026-08-17-deflate-pipeline-wrapper.md` documents the design decisions.

This plan was revised after `/cg-plan-review` (4 P1 + 4 P2 findings). Key corrections:

- **Dropped Approach 2 (version hints)**: The brainstorm's "hybrid" optimization assumed passing `version` to `pd_deflation()` skips the master-inventory load inside `.load_deflation_aux()`. This is false — `.load_deflation_aux()` (`R/pd_deflation.R:71`) *unconditionally* calls `pipload::load_pip_master_inventory()` regardless of `version`; `version` only changes which inventory row is selected (line 91, filtering on `content_hash_data`), while `pip_read()` at line 240 expects a stamp `version_id`. The `version` parameter is dual-purpose and cannot be used as a version hint safely. This plan uses simple Mode B (`pd_deflation(pip_id = id)` with no `version`).
- **Worker correctness fixes**: name the list before `save_pip_data()`, guard against `pd_deflation()` returning `NA`, and check the save result.
- **Column initialization**: `build_pip_inventory()` requires an explicit `:= NA` block to create deflation columns, not just adding to `ordered_cols`.

Architectural choices retained:
- **Mode B**: delegates loading to `pd_deflation()`
- **Separate stage**: called independently from `pd_process_data()`
- **`"pip_deflated"` alias**: separate stamp alias from cleaned data

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Iterate over an inventory of cleaned surveys and apply `pd_deflation()` to each | brainstorm |
| R2 | Save deflated outputs to a dedicated `"pip_deflated"` stamp alias | brainstorm |
| R3 | Log a structured summary entry to `pipdata_log` with pinned keys `n_total`, `n_success`, `n_failed`, `surveys_success`, `surveys_failed` | brainstorm + review P2.4 |
| R4 | Per-survey `tryCatch` with skip-and-log; detect `NA` returns from `pd_deflation()` as failures | brainstorm + review P1.2 |
| R5 | Support both internal inventory loading (`inv = NULL`) and caller-supplied inventory, normalizing a missing `deflated` column | brainstorm + review P2.3 |
| R6 | Use simple Mode B (`pip_id` only, no `version` hint) to delegate loading to `pd_deflation()` | review P1.4 |
| R7 | First version: deflate everything not yet deflated (no incremental aux-hash gating) | brainstorm |
| R8 | Standalone pipeline stage — invoked independently by script | brainstorm |
| R9 | Add and initialize deflation columns (`deflated`, `content_hash_deflated`, `aux_*_hash_at_deflation`) in the master inventory | brainstorm + review P1.3/P3.2 |
| R10 | Source aux hash snapshots via `get_aux_hashes(c("cpi", "ppp", "pop"))` | review P2.2 |
| R11 | Check `save_pip_data()` return value so save failures are not counted as success | review P2.1 |

## Implementation Steps

### 1. Register `"pip_deflated"` stamp alias
- **Requirements**: R2
- **Files**: `R/pipdata_dlw_process.R`
- **Details**: Add `"pip_deflated"` to the `pipfun::setup_working_release()` call at line 53. If `setup_working_release()` does not support additional aliases via its existing API, add a separate `stamp::st_init()` call after it (matching the `piplog` pattern at `Pipdata_script.R:61-64`), pointing at `pip_repository/pip_deflated/`.
- **Test Scenarios**: After `setup_working_release()`, `stamp::st_catalog_query(alias = "pip_deflated")` returns a valid catalog; `pip_write`/`pip_read` round-trip for a test id
- **Tests**: Manual verification in interactive session (infrastructure step)
- **Acceptance criteria**: `stamp::st_catalog_query(alias = "pip_deflated")` succeeds; `pipload::pip_write(x = dt, id = "test", alias = "pip_deflated")` then `pipload::pip_read(id = "test", alias = "pip_deflated")` round-trips

### 2. Initialize deflation columns in `build_pip_inventory()`
- **Requirements**: R9
- **Files**: `R/build_pip_inventory.R`
- **Details**: Add an explicit initialization block after the aux-hash init block (lines 405-415), mirroring the `first_release_version_id` pattern at lines 398-403:
  ```r
  # Initialise deflation columns so the master schema is always consistent.
  if (!"deflated" %in% names(run_inv)) {
    run_inv[, deflated := NA]
  }
  if (!"content_hash_deflated" %in% names(run_inv)) {
    run_inv[, content_hash_deflated := NA_character_]
  }
  for (col in c("aux_cpi_hash_at_deflation", "aux_ppp_hash_at_deflation",
                "aux_pop_hash_at_deflation")) {
    if (!col %in% names(run_inv)) {
      run_inv[, (col) := NA_character_]
    }
  }
  ```
  Then add `deflation_cols <- c("deflated", "content_hash_deflated", "aux_cpi_hash_at_deflation", "aux_ppp_hash_at_deflation", "aux_pop_hash_at_deflation")` and include it in `ordered_cols` (line 597) for column ordering. Note: `deflated` must be logical `NA`; the other four are character `NA_character_`.
- **Test Scenarios**: New inventory has all five deflation columns (all `NA`); existing inventory (without columns) loads and gains the columns via the init block
- **Tests**: Unit test in `test-pd-deflate-pipeline.R` or existing `test-build-pip-inventory.R` asserting column presence
- **Acceptance criteria**: `build_pip_inventory()` output contains all five deflation columns, initialized to `NA`/`NA_character_`

### 3. Create `R/pd_deflate_pipeline.R` — core function
- **Requirements**: R1, R3, R4, R5, R6, R7, R8, R10, R11
- **Files**: `R/pd_deflate_pipeline.R` (new), `R/pd_deflation.R` (update `@note` only)
- **Details**: Implement the main function and its internal `deflate_one()` worker.

  **Signature**:
  ```r
  pd_deflate_pipeline <- function(
    inv     = NULL,
    force   = FALSE,
    verbose = getOption("pipdata.verbose", default = TRUE)
  )
  ```

  **Internal flow**:
  1. Load master inventory if `inv` is NULL: `pipload::load_pip_master_inventory(verbose = verbose)`
  2. Normalize: `if (!"deflated" %in% names(inv)) inv[, deflated := NA]`
  3. Handle empty inventory: `if (nrow(inv) == 0L)` → log info, return `inv` unchanged
  4. Filter candidates: `inv[is.na(deflated) | deflated == FALSE]` (or all rows if `force = TRUE`). If none → log info, return `inv`.
  5. Resolve aux hashes once: `aux_hashes <- get_aux_hashes(c("cpi", "ppp", "pop"), verbose = verbose)`
  6. `inv_ls <- split(candidates, seq_len(nrow(candidates)))`; `names(inv_ls) <- candidates$pip_id`
  7. `results <- lapply(inv_ls, deflate_one, verbose = verbose)`
  8. Build summary from `results`; update master inventory for successful rows
  9. Save master: `pipload::pip_write(x = updated_inv, id = "pip_master_inventory", alias = "pip_master", pk = c("survey_id", "pip_id"), verbose = verbose)`
  10. Log summary with pinned keys; return updated inventory

  **`deflate_one()` worker** (internal, `@noRd`). No `pd_env_set`/`pd_env_rm` — `pip_id` is captured by closure in the handlers:
  ```r
  deflate_one <- function(inv_row, verbose) {
    pip_id <- inv_row$pip_id

    tryCatch(
      expr = {
        dt <- pd_deflation(pip_id = pip_id, verbose = FALSE)

        # pd_deflation() returns NA (not an error) on failure
        if (!data.table::is.data.table(dt)) {
          pipfun::log_add(event = "error",
            message = "Deflation returned a non-data.table result (deflation failed).",
            name = "pipdata_log",
            logmeta = list(error = "deflation_na", survey = pip_id,
                           status = "The survey was not deflated"))
          return(NULL)
        }

        # name the list so save_pip_data() iterates over it correctly
        dt_ls <- list(dt)
        names(dt_ls) <- pip_id
        sv <- save_pip_data(dt_ls, alias = "pip_deflated", verbose = verbose)
        saved <- !is.null(sv) && length(sv) > 0L && isTRUE(sv[[1L]]$success)
        rm(dt, dt_ls); gc(verbose = FALSE)

        if (!saved) {
          pipfun::log_add(event = "error",
            message = "Deflated survey could not be saved to pip_deflated.",
            name = "pipdata_log",
            logmeta = list(error = "deflate_save_error", survey = pip_id,
                           status = "The survey was not saved"))
          return(NULL)
        }

        list(pip_id = pip_id, success = TRUE)
      },
      piperr = function(cnd) {
        pipfun::log_add(event = "error", message = cnd$message, name = "pipdata_log",
          logmeta = list(error = class(cnd)[2L], survey = pip_id,
                         status = "The survey was not deflated"))
        NULL
      },
      error = function(cnd) {
        original_cnd <- cnd
        while (!is.null(original_cnd$parent)) original_cnd <- original_cnd$parent
        if (inherits(original_cnd, "piperr")) {
          pipfun::log_add(event = "error", message = original_cnd$message,
            name = "pipdata_log",
            logmeta = list(error = class(original_cnd)[2L], survey = pip_id,
                           status = "The survey was not deflated"))
        } else {
          pipfun::log_add(event = "error", message = cnd$message, name = "pipdata_log",
            logmeta = list(error = class(cnd)[1L], survey = pip_id,
                           status = "The survey was not deflated"))
        }
        NULL
      }
    )
  }
  ```

  **Inventory update** (flow item 8): for each successful `pip_id`, set:
  - `deflated = TRUE`
  - `content_hash_deflated` = content hash of the `"pip_deflated"` artifact (query `stamp::st_catalog_query(alias = "pip_deflated")` and match by id, or read from `sv` if `save_pip_data()`/`pip_write` exposes it)
  - `aux_cpi_hash_at_deflation` = `aux_hashes[["cpi"]]`
  - `aux_ppp_hash_at_deflation` = `aux_hashes[["ppp"]]`
  - `aux_pop_hash_at_deflation` = `aux_hashes[["pop"]]`

  **Roxygen**: `@export`, `@family pd_deflate_pipeline pipeline`, `@examples \dontrun{}`, `@return data.table`. Update `pd_deflation()` `@note` (line 157) to state the wrapper now exists.

- **Test Scenarios**: See Step 4
- **Tests**: See Step 4
- **Acceptance criteria**: `devtools::load_all()` loads `pd_deflate_pipeline`; function runs on mock inventory without crashing

### 4. Write tests for `pd_deflate_pipeline()`
- **Requirements**: R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11
- **Files**: `tests/testthat/test-pd-deflate-pipeline.R` (new)
- **Details**: Self-contained fixtures, no network/file I/O. Mock `pipload::load_pip_master_inventory()`, `pipload::pip_read()`, `pipload::pip_write()`, `pd_deflation()`, `save_pip_data()`, `get_aux_hashes()` via `testthat::local_mocked_bindings()`. Follow `test-pd-deflation.R` fixture style.

  **`deflate_one()` tests**:
  - **Happy path**: `pd_deflation()` returns data.table → `save_pip_data()` returns success → returns `list(pip_id, success = TRUE)`
  - **NA return**: `pd_deflation()` returns `NA` → logs `deflation_na`, returns `NULL` (does not call `save_pip_data`)
  - **Save failure**: `save_pip_data()` returns `NULL`/failed → returns `NULL`
  - **Piperr path**: `pd_deflation()` throws `piperr` → logged, returns `NULL`
  - **Generic error path**: throws plain error → logged, returns `NULL`
  - **Save called with named list**: assert `save_pip_data` receives a list whose `names()` equals `pip_id`

  **`pd_deflate_pipeline()` tests**:
  - **Empty inventory (0 rows)**: returns `inv` unchanged, logs info
  - **Missing `deflated` column**: inventory without `deflated` → normalized, no error
  - **All already deflated**: filter yields 0 candidates → early return
  - **Single survey success**: one candidate → deflated, inventory `deflated = TRUE`, `content_hash_deflated` set
  - **Partial failure**: 3 candidates, 2 succeed, 1 fails → inventory updated for 2, log `n_failed = 1`
  - **Force flag**: `force = TRUE` re-deflates already-deflated surveys
  - **Caller-supplied inventory**: `inv` parameter bypasses internal load
  - **Aux hash snapshots**: `aux_cpi_hash_at_deflation` etc. set from mocked `get_aux_hashes()`

- **Tests**: `testthat::test_file("tests/testthat/test-pd-deflate-pipeline.R")`
- **Acceptance criteria**: All tests pass; happy path, error paths, and boundary conditions covered

### 5. Add `deflate_summary_inf` to `log_report()` and internal types
- **Requirements**: R3
- **Files**: `R/log_report.R`, `R/aaa.R`
- **Details**: Two changes:
  1. Add `"deflate_summary_inf"` to `.log_internal_types` in `R/aaa.R` (lines 80-88) so it is excluded from the type-summary table.
  2. Add `build_deflation_summary(dt)` to the `sections <- Filter(length, list(...))` call (`R/log_report.R:78-90`), reading keys `n_total`, `n_success`, `n_failed`, `surveys_success`, `surveys_failed` from `deflate_summary_inf` entries. Mirror `build_processing_summary()`.
- **Test Scenarios**: Mock log with `deflate_summary_inf` entries → report renders a deflation summary section and does not list it as an error type
- **Tests**: Unit test in `test-pd-deflate-pipeline.R` or `test-log-report.R`
- **Acceptance criteria**: `log_report()` renders deflation summary; `deflate_summary_inf` absent from type-summary table

### 6. Update `Pipdata_script.R` and vignette
- **Requirements**: R8
- **Files**: `Pipdata_script.R`, `vignettes/articles/Processing-Data.Rmd`
- **Details**: In `Pipdata_script.R`, add after line 47 (`pd_process_data()`):
  ```r
  # ----- Deflate surveys -----
  new_pip_inv <- pd_deflate_pipeline(force = TRUE, verbose = TRUE)
  ```
  In `Processing-Data.Rmd`, add a section documenting `pd_deflate_pipeline()` as the second pipeline stage: signature, parameters (`inv`, `force`, `verbose`), and relationship to `pd_process_data()`.
- **Test Scenarios**: N/A (documentation)
- **Tests**: N/A
- **Acceptance criteria**: Script runs without error; vignette builds

## Testing Strategy

- **Unit tests**: Self-contained fixtures in `test-pd-deflate-pipeline.R`; mock all stamp/pipload I/O via `testthat::local_mocked_bindings()`
- **Pattern**: Follow `test-pd-deflation.R` fixture helpers
- **Regression**: `testthat::test_local()` after each step
- **Integration**: Manual verification via `Pipdata_script.R` with a small inventory subset

## Documentation Checklist

- [x] Roxygen2 on `pd_deflate_pipeline()` (`@export`, `@family`, `@return`, `@examples \dontrun{}`)
- [x] Roxygen2 on `deflate_one()` (`@noRd`)
- [ ] Update `pd_deflation()` `@note`
- [ ] Update `vignettes/articles/Processing-Data.Rmd`
- [ ] Update `compound-gpid.md` Current Focus section

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| `pipfun::setup_working_release()` doesn't support `"pip_deflated"` alias | Low | High | Fallback: `stamp::st_init()` directly (same pattern as `piplog`) |
| `content_hash_deflated` retrieval from stamp catalog is non-trivial | Medium | Medium | Query `stamp::st_catalog_query(alias = "pip_deflated")` and match by artifact id; if `pip_write`/`save_pip_data` return the version facts, use those instead |
| `get_aux_hashes()` fails because aux alias not configured in some environments | Low | Medium | Function is already used by `pd_process_data()`; environment is expected to be configured the same way |
| Inventory schema migration breaks existing inventory load | Low | Medium | Explicit `:= NA` init block guarantees column presence; `collapse::rowbind(fill = TRUE)` handles retained rows |
| `pd_deflation()` may load the master inventory per survey (redundant I/O) | High | Low | Accepted for first version; a future optimization can refactor `.load_deflation_aux()` to accept a pre-loaded inventory |

## Out of Scope

- Incremental aux-hash-gated re-deflation (future roadmap item)
- Downstream consumer migration from `"pip"` to `"pip_deflated"` alias
- `pd_run_pipeline()` orchestrator chaining `pd_process_data()` + `pd_deflate_pipeline()`
- Changes to `pipload` or `pipfun` packages
- Parallel execution (`future.apply`, `parallel::mclapply`)
- Refactoring `.load_deflation_aux()` to accept a pre-loaded inventory (the actual I/O optimization deferred)

## Completion Contract

### Outcome
`pd_deflate_pipeline()` is a working, exported R function that iterates over a master inventory of cleaned surveys, deflates each via `pd_deflation(pip_id = ...)` (simple Mode B), correctly detects and skips failures (including `NA` returns), saves results to a registered `"pip_deflated"` alias, updates the master inventory with deflation columns, and logs a structured summary.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required |
|----|---|---|---|
| V1 | `deflate_one()` unit tests: named-list save, `NA`-return guard, save-failure detection, piperr/error paths | `testthat::test_file("tests/testthat/test-pd-deflate-pipeline.R")` | yes |
| V2 | `pd_deflate_pipeline()` tests: empty/0-row inventory, missing-`deflated`-column normalization, single success, partial failure, force flag, caller-supplied inventory | same file | yes |
| V3 | `devtools::check()` no new ERROR/WARNING | `devtools::check()` | yes |
| V4 | `"pip_deflated"` alias registered | review `R/pipdata_dlw_process.R` | yes |
| V5 | `build_pip_inventory()` initializes deflation columns (not just reorders) | review `R/build_pip_inventory.R` | yes |
| V6 | `deflate_summary_inf` in `.log_internal_types` + sections list | review `R/aaa.R`, `R/log_report.R` | yes |
| V7 | Master write uses `pk = c("survey_id", "pip_id")` | review `R/pd_deflate_pipeline.R` | yes |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | `pd_deflation()` interface unchanged (no new params) | `diff R/pd_deflation.R` |
| C2 | `save_pip_data()` interface unchanged | `diff R/save_pip.R` |
| C3 | No regressions in existing tests | `testthat::test_local()` |
| C4 | Follows existing code style (roxygen2, `@family`, `@export`) | review |

### Boundaries
- **Allowed**: new `R/pd_deflate_pipeline.R`, new test file, `build_pip_inventory.R` (add column init), `pipdata_dlw_process.R` (register alias), `log_report.R` (add section), `aaa.R` (add internal type), `Pipdata_script.R`, `pd_deflation.R` (`@note` only), NAMESPACE
- **Out of scope**: incremental aux-hash-gated re-deflation, downstream consumer migration, `pd_run_pipeline()` orchestrator, `pipload`/`pipfun` changes, refactoring `.load_deflation_aux()`

### Iteration Policy
1. Infrastructure first (alias registration, column init), then core function, then tests, then integration
2. Run tests after each step
3. If a step reveals a dependency issue (e.g., `setup_working_release()` can't register the alias), stop and document the blocker

### Blocked-Stop Conditions
- `pipfun::setup_working_release()` cannot register `"pip_deflated"` without `pipfun` code changes
- `stamp::st_init()` requires a nonexistent root path
- `get_aux_hashes()` fails to resolve cpi/ppp/pop artifacts (aux alias not configured)
