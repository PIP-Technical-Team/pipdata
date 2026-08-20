---
date: 2026-08-17
title: "pd_deflate_pipeline() — Batch Deflation Orchestrator Design"
status: decided
chosen-approach: "Mode B batch with separate stamp alias and hybrid aux loading"
participants:
  - wb384996
tags: [deflation, pipeline, orchestration, batch-processing]
---

# pd_deflate_pipeline() — Batch Deflation Orchestrator Design

## Context

`pd_deflation()` works for single surveys (Mode A: pass `dt`, Mode B: pass `pip_id`). The `integrate-deflation` roadmap item is done. What's missing is the batch wrapper (`deflate-pipeline-wrapper` in roadmap.json, status: `idea`) that mirrors `pd_process_data()`'s iteration pattern: split inventory → lapply with tryCatch → log summary → build result.

`pd_deflation()` has a `@note` at line 157 referencing this future wrapper. Several plan and brainstorm docs also reference it as a deferred item.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Iterate over an inventory of cleaned surveys and apply `pd_deflation()` to each | user |
| R2 | Save deflated outputs to a dedicated `"pip_deflated"` stamp alias | user |
| R3 | Log a structured summary entry to `pipdata_log` (n_total, n_success, n_failed, failed_ids) | user |
| R4 | Per-survey `tryCatch` with skip-and-log on failure (no fail-fast) | existing pattern |
| R5 | Support both internal inventory loading (`inv = NULL`) and caller-supplied inventory | existing pattern |
| R6 | Use Mode B (`pip_id`) to delegate loading to `pd_deflation()` | design decision |
| R7 | First version: deflate everything not yet deflated (no incremental aux-hash gating) | user |
| R8 | Load master inventory once, pass `version` hints to avoid redundant inventory reads | design decision |
| R9 | Standalone pipeline stage — not called by `pd_process_data()`, invoked independently | user |
| R10 | Add deflation-specific columns to the master inventory for future incremental processing | design decision |

## Approaches Considered

### Approach 1: Simple Mode B (no aux optimization)

Call `pd_deflation(pip_id = id)` per survey. Each call internally loads the master inventory + data + metadata.

**Pros**: Zero changes to `pd_deflation()`. Simplest implementation.
**Cons**: Redundant inventory load per survey (N=500 → 500 × same inventory file read).
**Effort**: Small

### Approach 2: Hybrid — load inventory once, pass version hints (CHOSEN)

Load master inventory once in batch wrapper. Build `(pip_id → content_hash_data)` lookup. Pass `version` to `pd_deflation()` so `.load_deflation_aux()` skips its internal inventory load.

**Pros**: No interface changes to `pd_deflation()`. Uses existing `version` parameter. Meaningful I/O reduction (saves N inventory reads).
**Cons**: Slightly more complex batch wrapper (one extra lookup table).
**Effort**: Small-Medium

### Approach 3: Full pre-load — pass aux data directly

Load all CPI/PPP/pop once, pass to `pd_deflation()` via legacy path (`cpi`/`ppp`/`pop` args).

**Pros**: Fastest I/O (1 stamp read per survey instead of 2).
**Cons**: Couples batch wrapper to internal legacy path semantics. Loses per-survey version granularity. The legacy path expects `ppp` as a wide data.table with different processing.
**Effort**: Medium

### Pipeline Call Chain Options

| Option | Description | Pros | Cons |
|--------|-------------|------|------|
| Monolithic | `pd_process_data()` calls `pd_deflate_pipeline()` internally | Single entry point | Tight coupling, can't re-deflate independently |
| **Separate stages (CHOSEN)** | Script calls `pd_process_data()` then `pd_deflate_pipeline()` independently | Independent re-runs, matches existing codebase pattern | Two function calls in script |
| Orchestrator | New `pd_run_pipeline()` chains both | Best of both worlds | Another function to maintain |

## Decision

**Chosen approach**: Approach 2 (hybrid aux loading) with separate-stage pipeline architecture.

### Key decisions summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Inventory input | `inv = NULL` default, load internally or accept caller-supplied | Matches `pd_process_data()` pattern |
| Stamp alias | Create `"pip_deflated"` | Separate from cleaned data; requires `pipfun::setup_working_release()` registration |
| Master inventory | Add deflation columns (`deflated`, `deflation_version`, aux hash snapshots) | Enables incremental processing later |
| Error handling | Per-survey `tryCatch` + skip-and-log | Matches `pd_process_data()` — `process_data()` returns NULL on failure |
| Call mode | Mode B (`pip_id`) | Delegates loading to `pd_deflation()`; batch wrapper doesn't duplicate load logic |
| Filtering | "Deflate everything not yet deflated" | Simple first pass; incremental aux-hash gating deferred |
| Aux pre-loading | Hybrid — load inventory once, pass `version` hints | Eliminates redundant inventory reads without changing `pd_deflation()` interface |
| Pipeline chain | Separate stage, called independently by script | Independent re-runs, matches codebase pattern |
| Return value | Updated deflation inventory (data.table) | Consistent with `pd_process_data()` return |

### Proposed signature

```r
pd_deflate_pipeline <- function(
  inv     = NULL,                                          # master inventory; loaded internally when NULL
  force   = FALSE,                                         # re-deflate even if already deflated
  verbose = getOption("pipdata.verbose", default = TRUE)
)
```

### Proposed internal flow

```
1.  Load master inventory (if inv is NULL) via pipload::load_pip_master_inventory()
2.  Filter to deflation candidates:
      - deflated == FALSE | is.na(deflated)
      - OR force == TRUE (deflate everything)
3.  Early return if nothing to deflate (log info, return inv unchanged)
4.  Build version lookup: pip_id → content_hash_data (for passing to pd_deflation)
5.  Split inventory → lapply with deflate_one() per pip_id
6.  deflate_one() per survey:
      a. on.exit cleanup of .pipdataenv key
      b. tryCatch:
         - pd_deflation(pip_id = id, version = ver, verbose = FALSE)
         - save_pip_data(list(dt), alias = "pip_deflated", verbose = verbose)
         - return list(pip_id = id, success = TRUE)
      c. piperr handler: log via pipfun::log_add(), return NULL
      d. error handler: log via pipfun::log_add(), return NULL
7.  Collect results: build deflation_map from successful results
8.  Update master inventory with deflation columns:
      - deflated = TRUE
      - deflation_version = content_hash from pip_deflated stamp
      - aux_cpi_hash_at_deflation, aux_ppp_hash_at_deflation, aux_pop_hash_at_deflation
9.  Save updated master inventory to stamp (pip_master alias)
10. Log summary via pipfun::log_info():
      - info = "deflate_summary_inf"
      - n_total, n_success, n_failed, surveys_success, surveys_failed
11. Return updated master inventory
```

### Stamp alias registration

`"pip_deflated"` must be registered via `pipfun::setup_working_release()` before `pd_deflate_pipeline()` can write to it. This is an infrastructure prerequisite — either:
- Add it to the existing `setup_working_release()` call in `pipdata_dlw_process.R:53`, or
- Register it once manually via `stamp::st_init(root = ..., alias = "pip_deflated")`

### File locations

| What | Where |
|------|-------|
| Main function | `R/pd_deflate_pipeline.R` (new file) |
| Per-survey worker | `deflate_one()` — internal function in same file |
| Inventory update | Inline in the main function (or extracted to `update_deflation_inventory()` if >50 lines) |
| Tests | `tests/testthat/test-pd_deflate_pipeline.R` |
| Roxygen docs | Inline in `R/pd_deflate_pipeline.R` |

### Integration with master inventory

Add columns to `build_pip_inventory()`:

```r
# In build_pip_inventory.R — Step 7 or new step:
deflation_cols <- c("deflated", "deflation_version",
                    "aux_cpi_hash_at_deflation", "aux_ppp_hash_at_deflation",
                    "aux_pop_hash_at_deflation")
```

These columns are initialized as `NA` for new surveys, updated by `pd_deflate_pipeline()` after successful deflation.

## Consequences

1. **New stamp alias**: `"pip_deflated"` must be registered. Downstream consumers that read deflated data from `"pip"` will need to switch to `"pip_deflated"`, OR the final pipeline step copies from `"pip_deflated"` back to `"pip"` for downstream compatibility.

2. **Two-stage pipeline**: Users must call `pd_process_data()` then `pd_deflate_pipeline()` separately. This is a feature (independent re-runs) but requires updating `Pipdata_script.R` and the processing vignette.

3. **Inventory schema change**: Adding deflation columns to the master inventory is a schema migration. Existing inventories will have `NA` for these columns until re-deflated.

4. **Future incremental processing**: The deflation columns enable future aux-hash-gated re-deflation (matching `valid_dlw_load()`'s pattern). This is explicitly deferred to a follow-up.

5. **Downstream data source**: If `"pip_deflated"` becomes the source of deflated data, `pipload::pip_read(id, alias = "pip_deflated")` replaces `pipload::pip_read(id, alias = "pip")` for downstream consumers. This needs coordination with `pipload` and any Shiny/reporting code that reads pip data.

## Next Steps

1. Register `"pip_deflated"` stamp alias in `pipfun::setup_working_release()` / `stamp::st_init()`
2. Create `R/pd_deflate_pipeline.R` with `pd_deflate_pipeline()` and `deflate_one()`
3. Add deflation columns to `build_pip_inventory()` schema
4. Write tests in `tests/testthat/test-pd_deflate_pipeline.R`
5. Update `Pipdata_script.R` to call `pd_deflate_pipeline()` after `pd_process_data()`
6. Update the processing vignette (`vignettes/articles/Processing-Data.Rmd`)
7. Update `pd_deflation()` roxygen `@note` to reflect that the wrapper now exists
