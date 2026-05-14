---
date: 2026-04-28
title: "Unified logging and reporting across pipdata"
status: active
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-04-28-unified-logging.md"
language: "R"
estimated-effort: "medium"
tags: [logging, reporting, dlw, pipeline, harmonization, pipfun]
---

# Plan: Unified Logging and Reporting Across pipdata

## Objective

Unify the logging infrastructure so all pipdata functions (DLW acquisition,
validation, and survey cleaning) write to the same `piplog` format, use
canonical logmeta types, and produce a single consolidated report via
`log_report()`. Eliminate the `log`/`save_log` arguments and replace with
unconditional logging and automatic checkpoint saves.

## Context

The brainstorm (`.cg-docs/brainstorms/2026-04-28-unified-logging.md`) chose
**Approach 1 — Lift and Standardize**: migrate DLW delegate functions to the
pipeline's always-on, typed-logmeta pattern.

**Current state** (two incompatible conventions):

| Aspect | DLW wrapper | Pipeline wrapper |
|--------|-------------|------------------|
| Log argument | `log = TRUE` + `if (log)` guards | No argument — always logs |
| Persistence | `log_save()` via `save_log` arg | Log stays in memory |
| logmeta | Ad-hoc error strings | Canonical types (`process_summary_inf`, etc.) |
| Report | Not covered by `log_report()` | Fully covered |

**Active DLW surface**: `pipdata_dlw_process`, `pipdata_get_gmd`,
`pipdata_validate_gmd`. Legacy files (`dlw_dta_to_qs`, `dlw_get_dta`,
`dlw_scan_and_validate`) are excluded — they must be archived first
(prerequisite: `archive-legacy-dlw`).

**Existing pipfun API**: `log_add()`, `log_info()`, `log_error()`, `log_warn()`,
`log_save()`, `log_filter()`. `piplog` is a `data.table` with columns: `time`,
`package`, `fun`, `event`, `message`, `args`, `logmeta`, `output`, `trace`.
`logmeta` is a list-column of arbitrary named lists. `log_save()` persists via
`stamp::st_save()`.

**Testing patterns**: Synthetic `piplog` via `make_entry()`/`make_piplog()`
helpers; contract-based testing for logging side effects in functions with
heavy external dependencies.

## Prerequisites

| ID | Prerequisite | Status |
|----|-------------|--------|
| P0 | `archive-legacy-dlw` (Pipeline Alignment Audit) | not started |

**This plan should not begin until P0 is complete.** Archiving the legacy DLW
files removes 3 files with ~21 `log_add` calls and the old `if (log)` pattern,
reducing the surface and avoiding wasted refactoring.

## Requirements

| ID  | Requirement | Source |
|-----|-------------|--------|
| R1  | Eliminate `log` and `save_log` arguments from all DLW functions | brainstorm |
| R2  | All logging is unconditional — no `if (log)` guards | brainstorm |
| R3  | Define canonical logmeta types for DLW events: `dlw_download_inf`, `dlw_validation_inf`, `dlw_summary_inf` | brainstorm |
| R4  | Automatic checkpoint save after DLW step and after pipeline step | brainstorm |
| R5  | Log all DLW events (success + failure); `log_report()` surfaces mainly failures | brainstorm |
| R6  | `log_report()` is stage-aware: works after DLW-only runs with a warning that cleaning was not executed | brainstorm |
| R7  | Register new logmeta types in `.log_internal_types` in `aaa.R` | convention |
| R8  | Use `log_info()`/`log_error()` typed wrappers, not raw `log_add()` | standardization |
| R9  | Coordinate with pipfun for any API additions (checkpoint helper, type registry) | brainstorm |

## Implementation Steps

### Phase 1: pipfun Changes

#### Step 1. Add `log_save_checkpoint()` helper to pipfun

- **Requirements**: R4, R9
- **Files**: `pipfun/R/log_save.R` (or new file `pipfun/R/log_checkpoint.R`)
- **Details**: Create a thin wrapper around `log_save()` that:
  1. Accepts a `stage` argument (`"dlw"` or `"pipeline"`)
  2. Constructs an `id` like `"pipdata_log_checkpoint_{stage}"`
  3. Attaches `metadata = list(stage = stage, checkpoint_time = Sys.time())`
  4. Calls `log_save()` with a deterministic alias (e.g., `"log_checkpoint"`)
  ```r
  log_save_checkpoint <- function(
    name  = getOption("pipfun.log.default"),
    stage = c("dlw", "pipeline"),
    alias = "log_checkpoint",
    ...
  ) {
    stage <- match.arg(stage)
    id <- paste0(name, "_checkpoint_", stage)
    log_save(
      name     = name,
      id       = id,
      alias    = alias,
      metadata = list(stage = stage, checkpoint_time = Sys.time()),
      ...
    )
  }
  ```
- **Test Scenarios**:
  - ✅ Happy path: saves log with correct `id` and `metadata$stage`
  - 🛑 Edge case: empty log — should save an empty `piplog` without error
  - ❌ Error path: invalid `stage` value — `match.arg()` aborts
- **Tests**: Unit test in pipfun checking id construction and metadata attachment
- **Acceptance criteria**: `log_save_checkpoint(stage = "dlw")` persists the log; `stamp::st_info()` shows the expected id and metadata.

#### Step 2. Verify `log_info`/`log_error` cover DLW needs

- **Requirements**: R8, R9
- **Files**: `pipfun/R/log_helpers.R`
- **Details**: Audit the current DLW `log_add()` calls. All use `event = "info"` or `event = "error"` with `name`, `logmeta`, and optionally `args`. The typed wrappers `log_info()`/`log_error()` accept `message`, `name`, `logmeta`, and `output` — but **not `args`**. Two options:
  1. **(Preferred)** Drop explicit `args` from DLW calls — `log_info()`/`log_error()` auto-capture caller arguments. The existing auto-capture via `capture_log_args()` should suffice for DLW functions.
  2. If auto-capture is insufficient (e.g., need to log specific loop variables not in formals), add an `args` parameter to `log_info()`/`log_error()`.
  
  Research first (read pipfun source for `capture_log_args`), then decide. If Option 1 works, no pipfun change is needed for this step.
- **Test Scenarios**:
  - ✅ Happy path: `log_info()` called inside a loop captures the enclosing function's arguments
  - 🛑 Edge case: `log_info()` called inside `tryCatch` error handler — does auto-capture still work?
- **Acceptance criteria**: All DLW logging use cases can be expressed via `log_info()`/`log_error()` with `logmeta` carrying structured data.

### Phase 2: pipdata DLW Function Refactoring

#### Step 3. Refactor `pipdata_get_gmd()` — remove `log`/`save_log`, add typed logmeta

- **Requirements**: R1, R2, R3, R5, R8
- **Files**: `R/pipdata_get_gmd.R`
- **Details**:
  1. Remove `log` and `save_log` from function signature and roxygen.
  2. Remove all `if (log) { ... }` guards.
  3. Replace `log_add("info", ...)` with `log_info(...)` and `log_add("error", ...)` with `log_error(...)`.
  4. Replace ad-hoc logmeta with canonical types:
     - **Start entry**: `log_info("DLW download started.", logmeta = list(info = "dlw_download_inf", phase = "start", n_surveys = nrow(inv_gmd)))`
     - **Per-survey failure** (in tryCatch error handler): `log_error(msg, logmeta = list(error = "dlw_download_inf", survey = <survey_id>, country = country, year = year, module = md_type))`
     - **Inventory save**: `log_info("Inventory saved.", logmeta = list(info = "dlw_download_inf", phase = "complete", saved_at = pip_folders$dlw_inventory))`
  5. Remove the `log_save()` call at the end (checkpoint saves move to `pipdata_dlw_process`).
- **Test Scenarios**:
  - ✅ Happy path: 50 surveys download, all succeed → 1 start entry + 1 complete entry, no errors
  - 🛑 Edge case: 3 of 50 fail → 1 start entry + 3 `dlw_download_inf` error entries + 1 complete entry
  - ❌ Error path: all surveys fail → above pattern still holds
- **Tests**: Contract tests for logmeta structure of `dlw_download_inf`; verify entry counts match expected patterns using synthetic scenarios.
- **Acceptance criteria**: No `log` or `save_log` argument in signature. No `if (log)` guards. All log calls use typed wrappers with canonical logmeta.

#### Step 4. Refactor `pipdata_validate_gmd()` — same treatment

- **Requirements**: R1, R2, R3, R5, R8
- **Files**: `R/pipdata_validate_gmd.R`
- **Details**:
  1. Remove `log` and `save_log` from signature and roxygen.
  2. Remove all `if (log) { ... }` guards (~8 occurrences).
  3. Replace `log_add` calls with typed wrappers.
  4. Define canonical logmeta entries:
     - **Start**: `log_info("DLW validation started.", logmeta = list(info = "dlw_validation_inf", phase = "start", n_surveys = nrow(gmd_new)))`
     - **Per-survey load failure**: `log_error(msg, logmeta = list(error = "dlw_validation_inf", survey = file_id, phase = "load"))`
     - **Inventory save**: `log_info("Validation inventory saved.", logmeta = list(info = "dlw_validation_inf", phase = "inventory_save", saved_at = pip_folders$dlw_metadata))`
     - **Report save**: `log_info("Validation report saved.", logmeta = list(info = "dlw_validation_inf", phase = "report_save"))`
     - **Inventory not generated (error)**: `log_error("Inventory not generated.", logmeta = list(error = "dlw_validation_inf", phase = "inventory_fail"))`
     - **Old report load failure**: `log_error(msg, logmeta = list(error = "dlw_validation_inf", phase = "report_load_fail"))`
     - **Old inv load failure**: `log_error(msg, logmeta = list(error = "dlw_validation_inf", phase = "inv_load_fail"))`
  5. Remove the `log_save()` call at the end.
- **Test Scenarios**:
  - ✅ Happy path: all surveys valid → start + inventory_save + report_save entries
  - 🛑 Edge case: some surveys fail load → per-survey error entries with `phase = "load"`
  - ❌ Error path: inventory not generated → error entry with `phase = "inventory_fail"`
- **Tests**: Contract tests for `dlw_validation_inf` logmeta structure; condition tests for each logging branch.
- **Acceptance criteria**: Same as Step 3 — no `log`/`save_log`, no guards, typed wrappers, canonical logmeta.

#### Step 5. Refactor `pipdata_dlw_process()` — remove args, add summary + checkpoint

- **Requirements**: R1, R2, R3, R4
- **Files**: `R/pipdata_dlw_process.R`
- **Details**:
  1. Remove `log` and `save_log` from function signature, roxygen, and examples.
  2. Stop passing `log`/`save_log` to `pipdata_get_gmd()` and `pipdata_validate_gmd()`.
  3. Add a **DLW summary logmeta entry** after both delegates complete:
     ```r
     pipfun::log_info(
       "DLW processing complete.",
       name = "pipdata_log",
       logmeta = list(
         info = "dlw_summary_inf",
         get_dlw_data = get_dlw_data,
         validate_dlw_data = validate_dlw_data
       )
     )
     ```
  4. Add a **checkpoint save** after the summary entry:
     ```r
     pipfun::log_save_checkpoint(
       name  = "pipdata_log",
       stage = "dlw"
     )
     ```
- **Test Scenarios**:
  - ✅ Happy path: both steps run → `dlw_summary_inf` entry written, checkpoint saved
  - 🛑 Edge case: `get_dlw_data = FALSE` — only validation runs; summary still written
  - ❌ Error path: `pipdata_get_gmd()` aborts — no summary or checkpoint (expected)
- **Tests**: Contract test for `dlw_summary_inf` structure.
- **Acceptance criteria**: `pipdata_dlw_process()` signature has no `log`/`save_log`. A `dlw_summary_inf` entry and checkpoint file exist after a successful run.

#### Step 6. Add pipeline checkpoint save to `pd_process_data()`

- **Requirements**: R4
- **Files**: `R/pd_process_data.R`
- **Details**: After the existing `process_summary_inf` log entry (line ~88), add:
  ```r
  pipfun::log_save_checkpoint(
    name  = "pipdata_log",
    stage = "pipeline"
  )
  ```
  This creates symmetry: both wrappers produce a checkpoint.
- **Test Scenarios**:
  - ✅ Happy path: checkpoint file created after processing
  - 🛑 Edge case: no surveys to process (early return) — no checkpoint (acceptable; no processing occurred)
- **Tests**: Verify checkpoint is written when processing completes.
- **Acceptance criteria**: After `pd_process_data()` completes, a `"pipeline"` checkpoint exists.

### Phase 3: `log_report()` Extension

#### Step 7. Register new logmeta types in `aaa.R`

- **Requirements**: R7
- **Files**: `R/aaa.R`
- **Details**: Add the three new types to `.log_internal_types`:
  ```r
  .log_internal_types <- c(
    "process_summary_inf",
    "aux_changes_inf",
    "inv_update_inf",
    "null_svys_inf",
    "skipped_svys_data",
    "skipped_svys_metadata",
    "dlw_download_inf",
    "dlw_validation_inf",
    "dlw_summary_inf"
  )
  ```
  This prevents the new types from polluting the "Summary by Type" table.
- **Tests**: Verify `.log_internal_types` contains all 9 entries.
- **Acceptance criteria**: Report's type summary table excludes the new DLW types.

#### Step 8. Add `build_dlw_download_summary()` to `log_report.R`

- **Requirements**: R5, R6
- **Files**: `R/log_report.R`
- **Details**: New internal builder that:
  1. Finds all `dlw_download_inf` entries
  2. Counts total, success (info-level), failures (error-level)
  3. Renders a "DLW Download Summary" section with a count table
  4. Lists failed surveys with country/year/module details
  ```r
  build_dlw_download_summary <- function(dt) {
    dl_idx <- which(dt$error_type == "dlw_download_inf")
    if (length(dl_idx) == 0L) return(character(0))
    
    n_errors <- sum(dt$event[dl_idx] == "error")
    n_info   <- sum(dt$event[dl_idx] == "info")
    
    lines <- c(
      "## DLW Download Summary",
      "",
      sprintf("**Downloads attempted.** %d info entries, %d failures.", n_info, n_errors)
    )
    
    # List failures
    fail_idx <- dl_idx[dt$event[dl_idx] == "error"]
    if (length(fail_idx) > 0L) {
      lines <- c(lines, "", "**Failed downloads:**", "")
      for (i in fail_idx) {
        meta <- dt$logmeta[[i]]
        svy <- if (!is.null(meta$survey)) meta$survey else "unknown"
        lines <- c(lines, sprintf("- `%s`", svy))
      }
    }
    
    return(lines)
  }
  ```
- **Test Scenarios**:
  - ✅ Happy path: entries present → section rendered with correct counts
  - 🛑 Edge case: no `dlw_download_inf` entries → returns `character(0)`
  - ❌ Error path: all entries are errors → failure list shows all surveys
- **Tests**: Synthetic `piplog` with `dlw_download_inf` entries; verify markdown output.
- **Acceptance criteria**: Section appears when DLW download entries exist; omitted when absent.

#### Step 9. Add `build_dlw_validation_summary()` to `log_report.R`

- **Requirements**: R5, R6
- **Files**: `R/log_report.R`
- **Details**: Same pattern as Step 8 but for `dlw_validation_inf`. Group by `phase` to show inventory save, report save, and per-survey failures.
- **Test Scenarios**: Same pattern — present, absent, all-errors.
- **Tests**: Synthetic `piplog` tests.
- **Acceptance criteria**: Section appears when validation entries exist.

#### Step 10. Add stage-aware header logic to `log_report()`

- **Requirements**: R6
- **Files**: `R/log_report.R`
- **Details**: Modify `build_header()` (or add a new `build_stage_warning()` builder):
  1. Detect which stages ran by checking for marker entries:
     - `dlw_summary_inf` present → DLW stage ran
     - `process_summary_inf` present → Pipeline stage ran
  2. If only DLW ran:
     ```markdown
     > **⚠ Partial run:** Only DLW acquisition/validation completed.
     > Survey cleaning (`pd_process_data`) was not executed.
     ```
  3. If only pipeline ran (no DLW):
     ```markdown
     > **Note:** DLW acquisition was not part of this run.
     ```
  4. If both ran: no warning needed.
- **Test Scenarios**:
  - ✅ Both stages → no warning
  - 🛑 DLW only → warning about missing pipeline
  - 🛑 Pipeline only → note about missing DLW
  - ❌ Neither marker → generic "incomplete run" warning
- **Tests**: Synthetic `piplog` with each combination; verify warning text.
- **Acceptance criteria**: `log_report()` output includes correct stage warnings.

#### Step 11. Wire new builders into `log_report()` section list

- **Requirements**: R5, R6
- **Files**: `R/log_report.R`
- **Details**: Insert the new builders into the `sections` list in `log_report()`:
  ```r
  sections <- Filter(
    length,
    list(
      build_header(dt, title),
      build_stage_warning(dt),           # NEW
      build_dlw_download_summary(dt),    # NEW
      build_dlw_validation_summary(dt),  # NEW
      build_processing_summary(dt),
      build_aux_changes(dt),
      build_type_summary(dt),
      build_country_table(dt),
      build_inventory_additions(dt),
      build_skipped_surveys(dt),
      build_null_surveys(dt)
    )
  )
  ```
  DLW sections come before pipeline sections — matching execution order.
- **Tests**: Full integration test: build a `piplog` with entries from both stages; verify all sections appear in order.
- **Acceptance criteria**: `log_report()` produces a complete report with DLW + pipeline sections.

### Phase 4: Documentation & Cleanup

#### Step 12. Update roxygen and NAMESPACE

- **Requirements**: R1
- **Files**: `R/pipdata_dlw_process.R`, `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`, `R/log_report.R`, `R/aaa.R`, `NAMESPACE`
- **Details**:
  1. Remove `@param log` and `@param save_log` from all roxygen blocks.
  2. Update `@examples` to remove `log`/`save_log` arguments.
  3. Add `@details` to `pipdata_get_gmd()` and `pipdata_validate_gmd()` documenting which logmeta types they emit.
  4. Update `log_report()` `@details` to list the 3 new DLW sections.
  5. Document new internal builders with `@keywords internal`.
  6. Run `devtools::document()` to regenerate `.Rd` files and `NAMESPACE`.
- **Tests**: `R CMD check` passes without warnings about undocumented parameters.
- **Acceptance criteria**: All roxygen is consistent. No `log`/`save_log` references in documentation.

#### Step 13. Update `compound-gpid.context.md`

- **Files**: `compound-gpid.context.md`
- **Details**:
  1. Update the "Domain Rules" section: replace the 4 canonical logmeta types with the full list of 9.
  2. Add a note that `log`/`save_log` arguments were removed in this refactoring.
- **Acceptance criteria**: Context file reflects the new logging conventions.

## Testing Strategy

**Testing approaches** (using established patterns from `.cg-docs/solutions/`):

1. **Synthetic `piplog` tests** (for `log_report()` builders):
   Use `make_entry()`/`make_piplog()` helpers to construct minimal logs with
   the new DLW logmeta types. Test each builder independently.

2. **Contract tests** (for DLW logging side effects):
   Since DLW functions depend on external I/O (DLW API, stamp, pipload),
   use condition-based and structure-based contract tests:
   - Document expected logmeta structures for `dlw_download_inf`, `dlw_validation_inf`, `dlw_summary_inf`
   - Verify count arithmetic and field presence
   - Mirror source-code conditions in test assertions

3. **Integration test** (full report):
   Build a synthetic `piplog` with entries from all 9 logmeta types.
   Run `log_report()` and verify all sections appear with correct content.

4. **Stage-awareness tests**:
   Test `log_report()` with DLW-only logs, pipeline-only logs, and combined
   logs. Verify correct warning messages.

**Edge cases to cover**:
- Empty DLW stage (no surveys to download)
- All DLW surveys fail
- DLW-only run (no pipeline)
- Pipeline-only run (no DLW)
- Both stages with mixed success/failure

## Documentation Checklist

- [ ] Roxygen for `pipdata_dlw_process()` — remove `log`/`save_log` params
- [ ] Roxygen for `pipdata_get_gmd()` — remove params, add `@details` for logmeta
- [ ] Roxygen for `pipdata_validate_gmd()` — same
- [ ] Roxygen for `log_report()` — add DLW sections to `@details`
- [ ] `@keywords internal` for new builder functions
- [ ] Update `compound-gpid.context.md` Domain Rules section
- [ ] Inline comments for logmeta type strings (matching convention from existing code)

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Breaking callers of `pipdata_dlw_process(log=, save_log=)` | High | Medium | This is intentional. Announce breaking change in NEWS.md. The `Pipdata_script.R` master script is the primary caller — update it in the same PR. |
| `log_info()` auto-capture fails inside `tryCatch` error handler | Medium | Low | Test in Step 2. If auto-capture fails, pass `args` explicitly via `logmeta` or add `args` param to `log_info()`. |
| New logmeta type strings have typos → silently break report sections | Medium | High | Define type strings as constants (either in `aaa.R` or as function-level variables). Contract tests catch mismatches. |
| `log_save_checkpoint()` not yet available in pipfun when pipdata work begins | Low | High | Phase 1 (pipfun) must be merged and released before Phase 2 begins. Pin pipfun version in DESCRIPTION if needed. |

## Sequencing & Dependencies

```
P0: archive-legacy-dlw (prerequisite)
 │
 ├─► Phase 1: pipfun changes (Steps 1–2)
 │    │
 │    ▼
 ├─► Phase 2: pipdata DLW refactoring (Steps 3–6)
 │    │        [depends on Phase 1 for log_save_checkpoint]
 │    ▼
 ├─► Phase 3: log_report() extension (Steps 7–11)
 │    │        [depends on Phase 2 for new logmeta types being emitted]
 │    ▼
 └─► Phase 4: Documentation & cleanup (Steps 12–13)
```

Phases 2 and 3 could be parallelized (builder tests use synthetic logs, not
live DLW calls), but sequential execution is safer and easier to review.

## Out of Scope

- Legacy DLW file refactoring (`dlw_dta_to_qs`, `dlw_get_dta`, `dlw_scan_and_validate`) — archived separately
- `pipaux` logging — separate package, separate effort
- Approach 3 (DLW wrapper rewrite to mirror pipeline architecture) — future roadmap item `dlw-wrapper-rewrite`
- Changing the `piplog` data.table column schema — we add new logmeta types but don't alter the table structure
- HTML or interactive report formats
- Performance metrics (timing per survey)
- Consolidating the two wrappers into a single function
