---
date: 2026-04-28
title: "Unified logging and reporting across pipdata"
status: completed
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-04-28-unified-logging.md"
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
execution-report: ".cg-docs/work-reports/2026-08-21-unified-logging.md"
current-phase: 1
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

**Roadmap merge**: `logging-refactor` (standardize `log = TRUE` pattern)
and `unified-logging-report` (harmonize DLW + pipeline logging into one
report) share this plan and are treated as one effort. B2 implements
`logging-refactor` for the pipdata DLW surface (eliminating `log`/`save_log`
arguments). The pipaux-inclusion portion of `logging-refactor` remains a
follow-on after B2 — pipaux uses a separate log name (`"pipaux_update_log"`),
incompatible logmeta schema (`step`/`measure` vs `info`/`error`), and a
separate package, so it is out of scope here.

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
| R3  | Define canonical logmeta types for DLW events: `dlw_acquisition_inf`, `dlw_validation_inf`, `dlw_summary_inf` | brainstorm |
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
  1. Accepts `name` (the piplog name to filter/save, e.g.
     `"pipdata_log"`), `stage` (`"dlw"` or `"pipeline"`), and `alias`
  2. Constructs an `id` like `"pipdata_log_checkpoint_{stage}"`
  3. Attaches `metadata = list(stage = stage, checkpoint_time = Sys.time())`
  4. Calls `log_save()` with `name` flowing to `log_save()`'s name filter
  ```r
  log_save_checkpoint <- function(
    name  = "pipdata_log",
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
  Note: `name` is the piplog environment key (the same value passed to
  `log_add(name = ...)`). Steps 5 and 6 call
  `log_save_checkpoint(name = "pipdata_log", stage = "dlw")` — the
  `name` argument must be in the signature.
- **Test Scenarios**:
  - ✅ Happy path: saves log with correct `id` and `metadata$stage`
  - 🛑 Edge case: empty log — should save an empty `piplog` without error
  - 🛑 Alias reuse: saving `"dlw"` then `"pipeline"` checkpoint with
    the same alias `"log_checkpoint"` in one session — verify both
    checkpoints are retrievable (check `stamp::st_save()` keying
    semantics; if alias-only keying causes the second to shadow the
    first, switch to `alias = paste0("log_checkpoint_", stage)`)
  - ❌ Error path: invalid `stage` value — `match.arg()` aborts
- **Tests**: Unit test in pipfun checking id construction and metadata attachment
- **Acceptance criteria**: `log_save_checkpoint(stage = "dlw")` persists the log; `stamp::st_info()` shows the expected id and metadata.

#### Step 2. Spike: verify `log_info`/`log_error` auto-capture inside `tryCatch` handlers

- **Requirements**: R8, R9
- **Files**: `pipfun/R/log_helpers.R` (read only; modify only if needed)
- **Type**: **Prerequisite spike** — must be completed and documented
  **before** the plan is approved. Steps 3 and 4 assume Option 1
  (auto-capture works); if the spike disproves this, Steps 3-4 must
  be revised to pass `args` explicitly.
- **Details**: Audit the current DLW `log_add()` calls. All use
  `event = "info"` or `event = "error"` with `name`, `logmeta`, and
  optionally `args`. The typed wrappers `log_info()`/`log_error()`
  accept `message`, `name`, `logmeta`, and `output` — but **not `args`**.
  They auto-capture the caller's formals via `capture_log_args()`.

  **Known pitfall**: inside `tryCatch` error handlers and `lapply`
  callbacks, `capture_log_args()` resolves to the anonymous handler's
  frame (`function(e)`), so `args` becomes `list(e = <condition>)`
  rather than the enclosing function's context. This is documented in
  `compound-gpid.context.md:44`.

  **Assumption (to verify)**: Option 1 — auto-capture is sufficient
  for DLW functions because all structured context goes in `logmeta`,
  not `args`. The `args` auto-capture resolving to the handler frame
  is harmless (the context is in `logmeta`).

  **If the assumption holds**: no pipfun change needed. Steps 3-4
  proceed with `log_info(msg, logmeta = list(error = "...", ...))`.

  **If the assumption fails** (auto-capture causes an error or
  captures wrong context): Option 2 — add an `args` parameter to
  `log_info()`/`log_error()` and update Steps 3-4 to pass explicit
  `args` from DLW functions.
- **Test Scenarios**:
  - ✅ Happy path: `log_info()` called inside a loop captures the enclosing function's arguments
  - 🛑 Edge case: `log_info()` called inside `tryCatch` error handler — verify it does not error and `logmeta` is correctly preserved
- **Acceptance criteria**: Document the spike result (Option 1 or 2).
  If Option 1, no pipfun change. If Option 2, create a pipfun issue
  for the `args` parameter addition before proceeding to Phase 2.

### Phase 1.5: pipfun Release Coordination

#### Step 2.5. Release pipfun and pin version in pipdata

- **Requirements**: R9
- **Files**: pipfun `DESCRIPTION`, pipdata `DESCRIPTION`
- **Details**: pipfun is a separate package/repo. After Step 1 (and
  optionally Step 2 if Option 2 was chosen), pipfun must be released
  before pipdata Phase 2 can depend on `log_save_checkpoint()`:
  1. Bump pipfun's version in its `DESCRIPTION`
  2. Merge pipfun changes to main and tag/release
  3. Update pipdata's `DESCRIPTION` to pin the new pipfun version
     (e.g., `Imports: pipfun (>= X.Y.Z)`)
  4. Run `pkgload::load_all()` in pipdata to confirm
     `log_save_checkpoint` resolves
- **Acceptance criteria**: `pkgload::load_all()` in pipdata loads
  without error and `pipfun::log_save_checkpoint` is callable.

### Phase 2: pipdata DLW Function Refactoring

#### Step 3. Refactor `pipdata_get_gmd()` — remove `log`/`save_log`, add typed logmeta

- **Requirements**: R1, R2, R3, R5, R8
- **Files**: `R/pipdata_get_gmd.R`
- **Details**:
  1. Remove `log` and `save_log` from function signature and roxygen.
  2. Remove all `if (log) { ... }` guards.
  3. Replace `log_add("info", ...)` with `log_info(...)` and `log_add("error", ...)` with `log_error(...)`.
  4. Replace ad-hoc logmeta with canonical types:
     - **Start entry**: `log_info("DLW acquisition started.", logmeta = list(info = "dlw_acquisition_inf", phase = "start", n_surveys = nrow(inv_gmd)))`
     - **Per-survey failure** (in tryCatch error handler): `log_error(msg, logmeta = list(error = "dlw_acquisition_inf", survey = <survey_id>, country = country, year = year, module = md_type))`
     - **Inventory save**: `log_info("Inventory saved.", logmeta = list(info = "dlw_acquisition_inf", phase = "complete", saved_at = pip_folders$dlw_inventory))`
  5. Remove the `log_save()` call at the end (checkpoint saves move to `pipdata_dlw_process`).
- **Test Scenarios**:
  - ✅ Happy path: 50 surveys download, all succeed → 1 start entry + 1 complete entry, no errors
  - 🛑 Edge case: 3 of 50 fail → 1 start entry + 3 `dlw_acquisition_inf` error entries + 1 complete entry
  - ❌ Error path: all surveys fail → above pattern still holds
- **Tests**: Contract tests for logmeta structure of `dlw_acquisition_inf`; verify entry counts match expected patterns using synthetic scenarios.
- **Acceptance criteria**: No `log` or `save_log` argument in signature. No `if (log)` guards. All log calls use typed wrappers with canonical logmeta. **No `logmeta` entry contains an R condition object as `error`** — `error` is always a length-1 character string (this fixes the existing `logmeta = list(error = e)` anti-pattern at `pipdata_get_gmd.R:135`). Contract test asserts `is.character(dt$error_type)` after `parse_log_meta()` on logs produced by the refactored function.

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
- **Acceptance criteria**: Same as Step 3 — no `log`/`save_log`, no guards, typed wrappers, canonical logmeta. **No `logmeta` entry contains an R condition object as `error`** — `error` is always a length-1 character string (this fixes the existing `logmeta = list(error = e)` anti-pattern at `pipdata_validate_gmd.R:81, 160, 332`). Contract test asserts `is.character(dt$error_type)` after `parse_log_meta()`.

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
  - 🛑 Edge case: `get_dlw_data = FALSE` AND `validate_dlw_data = FALSE` —
    neither delegate runs, but `dlw_summary_inf` is **still written**
    (with both flags FALSE). This ensures the stage marker is present
    so `build_stage_warning()` (Step 10) can report "DLW stage ran but
    no acquisition or validation was performed" rather than falling
    through to "neither marker → incomplete run."
  - ❌ Error path: `pipdata_get_gmd()` aborts — no summary or checkpoint (expected)
- **Tests**: Contract test for `dlw_summary_inf` structure.
- **Acceptance criteria**: `pipdata_dlw_process()` signature has no `log`/`save_log`. A `dlw_summary_inf` entry and checkpoint file exist after a successful run.

#### Step 6. Add pipeline checkpoint save to `pd_process_data()`

- **Requirements**: R4
- **Files**: `R/pd_process_data.R`
- **Details**: After the existing `process_summary_inf` log_info() call
  (~line 145-155, after the `lapply` results are tallied), add:
  ```r
  pipfun::log_save_checkpoint(
    name  = "pipdata_log",
    stage = "pipeline"
  )
  ```
  This creates symmetry: both wrappers produce a checkpoint.

  **Important — early-return guard**: `pd_process_data()` has an early
  return at ~line 113-121 when `inv_to_clean` is NULL or has 0 rows.
  The checkpoint must be placed **after** this early-return guard, so
  it only fires when processing actually completed. Do not place it at
  line ~88 (that is inside the `force`/`stamp::st_opts` setup block).
- **Test Scenarios**:
  - ✅ Happy path: checkpoint file created after processing
  - 🛑 Edge case: no surveys to process (early return at ~line 113) — no checkpoint (acceptable; no processing occurred)
- **Tests**: Verify checkpoint is written when processing completes.
- **Acceptance criteria**: After `pd_process_data()` completes (past the early-return guard), a `"pipeline"` checkpoint exists.

### Phase 3: `log_report()` Extension

#### Step 7. Register new logmeta types in `aaa.R`

- **Requirements**: R7
- **Files**: `R/aaa.R`
- **Details**: Append the three new types to the **existing 8-entry**
  `.log_internal_types` vector in `R/aaa.R:80-89`. Do **not** rewrite the
  vector from scratch — the existing entries (`release_write_err`,
  `deflate_summary_inf`) must be preserved or the deflation summary section
  and release-write-error filtering will regress. The full 11-entry result:
  ```r
  .log_internal_types <- c(
    "process_summary_inf",
    "aux_changes_inf",
    "inv_update_inf",
    "null_svys_inf",
    "skipped_svys_data",
    "skipped_svys_metadata",
    "release_write_err",
    "deflate_summary_inf",
    "dlw_acquisition_inf",
    "dlw_validation_inf",
    "dlw_summary_inf"
  )
  ```
  This prevents the new types from polluting the "Summary by Type" table.
- **Tests**: Verify `.log_internal_types` contains all 11 entries (8 existing + 3 new).
- **Acceptance criteria**: Report's type summary table excludes the new DLW types. Existing `deflate_summary_inf` and `release_write_err` filtering still works.

#### Step 8. Add `build_dlw_acquisition_summary()` to `log_report.R`

- **Requirements**: R5, R6
- **Files**: `R/log_report.R`
- **Details**: New internal builder that:
  1. Finds all `dlw_acquisition_inf` entries
  2. **Distinguishes phase markers from per-survey outcomes**: entries
     with a `phase` field (`phase = "start"`, `phase = "complete"`)
     are phase markers, not per-survey outcomes. The plan emits
     per-survey errors but not one success row per successful survey,
     so use the `n_surveys` value from the start entry as the denominator:
     - `n_phase_markers` = count of entries where `phase %in% c("start", "complete")`
     - `n_errors` = count of error-level entries with a `survey` field
     - `n_surveys` = `n_surveys` from the start entry
     - `n_success` = `n_surveys` − `n_errors`
     Do **not** use raw entry totals or raw `n_info` as the success count —
     both would be wrong because phase markers are also info-level. If the
     start entry is absent, the builder returns `character(0)` and the
     contract test fails rather than presenting an inferred count.
  3. Renders a "DLW Acquisition Summary" section with a count table
  4. Lists failed surveys with country/year/module details
  ```r
  build_dlw_acquisition_summary <- function(dt) {
    dl_idx <- which(dt$error_type == "dlw_acquisition_inf")
    if (length(dl_idx) == 0L) return(character(0))

    # Separate phase markers from per-survey errors. Successful surveys are
    # represented by the n_surveys denominator in the start entry.
    metas <- dt$logmeta[dl_idx]
    has_phase <- vapply(metas, \(x) !is.null(x$phase), logical(1))
    n_phase_markers <- sum(has_phase)
    start_idx <- which(vapply(metas, \(x) identical(x$phase, "start"), logical(1)))
    if (length(start_idx) != 1L || is.null(metas[[start_idx]]$n_surveys)) {
      return(character(0))
    }
    n_surveys  <- metas[[start_idx]]$n_surveys
    has_survey <- vapply(metas, \(x) !is.null(x$survey), logical(1))
    n_errors   <- sum(dt$event[dl_idx] == "error" & has_survey)
    n_success  <- n_surveys - n_errors

    lines <- c(
      "## DLW Acquisition Summary",
      "",
      sprintf("**Surveys:** %d attempted, %d succeeded, %d failed.",
              n_surveys, n_success, n_errors)
    )

    # List failures
    fail_idx <- dl_idx[dt$event[dl_idx] == "error"]
    if (length(fail_idx) > 0L) {
      lines <- c(lines, "", "**Failed acquisitions:**", "")
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
  - ✅ Happy path: start entry reports 50 surveys and no errors → 50 attempted, 50 succeeded, 0 failed
  - 🛑 Mixed outcome: start entry reports 50 surveys and 3 per-survey errors → 50 attempted, 47 succeeded, 3 failed
  - 🛑 Edge case: no `dlw_acquisition_inf` entries → returns `character(0)`
  - ❌ Error path: all entries are errors → failure list shows all surveys
- **Tests**: Synthetic `piplog` with `dlw_acquisition_inf` entries; verify markdown output.
- **Acceptance criteria**: Section appears when DLW acquisition entries exist; omitted when absent.

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
  5. **DLW no-op case**: If `dlw_summary_inf` is present but its
     `get_dlw_data` and `validate_dlw_data` fields are both FALSE,
     emit a specific warning:
     ```markdown
     > **⚠ DLW no-op:** DLW stage ran but neither acquisition nor
     > validation was performed (both `get_dlw_data` and
     > `validate_dlw_data` were FALSE).
     ```
- **Test Scenarios**:
  - ✅ Both stages → no warning
  - 🛑 DLW only → warning about missing pipeline
  - 🛑 Pipeline only → note about missing DLW
  - 🛑 DLW no-op (both flags FALSE) → DLW no-op warning
  - ❌ Neither marker → generic "incomplete run" warning
- **Tests**: Synthetic `piplog` with each combination; verify warning text.
- **Acceptance criteria**: `log_report()` output includes correct stage warnings.

#### Step 11. Wire new builders into `log_report()` section list

- **Requirements**: R5, R6
- **Files**: `R/log_report.R`
- **Details**: Insert the new builders into the `sections` list in
  `log_report()`. The **existing** order in `R/log_report.R:80-93` is:
  header → processing_summary → deflation_summary → aux_changes →
  type_summary → country_table → inventory_additions → skipped_surveys →
  null_surveys. The DLW builders and stage warning are inserted **before**
  processing_summary (matching execution order: DLW runs first). The
  existing `build_deflation_summary` stays at its current position 3 —
  do not reorder it:
  ```r
  sections <- Filter(
    length,
    list(
      build_header(dt, title),
      build_stage_warning(dt),              # NEW
      build_dlw_acquisition_summary(dt),   # NEW
      build_dlw_validation_summary(dt),    # NEW
      build_processing_summary(dt),
      build_deflation_summary(dt),          # existing — unchanged position
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

#### Step 12. Update roxygen, NAMESPACE, and vignette code chunks

- **Requirements**: R1
- **Files**: `R/pipdata_dlw_process.R`, `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`, `R/log_report.R`, `R/aaa.R`, `NAMESPACE`, `vignettes/articles/Validating-Data.Rmd`, `vignettes/articles/PIP-data-pipeline.Rmd`
- **Details**:
  1. Remove `@param log` and `@param save_log` from all roxygen blocks.
  2. Update `@examples` to remove `log`/`save_log` arguments.
  3. Add `@details` to `pipdata_get_gmd()` and `pipdata_validate_gmd()` documenting which logmeta types they emit.
  4. Update `log_report()` `@details` to list the 3 new DLW sections.
  5. Document new internal builders with `@keywords internal`.
  6. Run `devtools::document()` to regenerate `.Rd` files and `NAMESPACE`.
  7. **Update vignette code chunks**: `Validating-Data.Rmd` has eval-able
     code chunks at lines 44-45, 77-78, 108-109 that pass
     `log = TRUE, save_log = TRUE` to `pipdata_dlw_process()` and
     `pipdata_get_gmd()`. Remove these arguments from all chunks.
     `PIP-data-pipeline.Rmd` lines 77-78 also pass these args. Update
     surrounding prose (e.g., `Validating-Data.Rmd:70` says "a failure
     is logged (if `log = TRUE`)" — this becomes stale; reword to
     "a failure is logged automatically").
  8. **Grep audit**: search all `.R` and `.Rmd` files outside `R/` and
     `vignettes/` for `save_log\s*=` or `log\s*=\s*TRUE` to find any
     other downstream callers that need updating. Note:
     `Pipdata_script.R` mentions `pipdata_dlw_process` only in a
     comment (line 35); it has no active call with these args — no
     update needed there.
- **Tests**: `R CMD check` passes without warnings about undocumented parameters. Vignettes build without error.
- **Acceptance criteria**: All roxygen is consistent. No `log`/`save_log` references in documentation or vignette code. `R CMD check` and vignette build both pass.

#### Step 13. Update `compound-gpid.context.md`

- **Files**: `compound-gpid.context.md`
- **Details**:
  1. Update the "Domain Rules" section with the three new DLW types. After
     this refactoring, document the 12 canonical discriminator types (the
     existing 9 plus `dlw_acquisition_inf`, `dlw_validation_inf`, and
     `dlw_summary_inf`) and separately note that `.log_internal_types`
     contains the 11 report-suppressed types.
  2. Add a note that `log`/`save_log` arguments were removed in this refactoring.
- **Acceptance criteria**: Context file reflects the new logging conventions.

#### Step 14. Announce breaking change in NEWS.md

- **Requirements**: R1
- **Files**: `NEWS.md` (or `inst/NEWS.md` depending on project convention)
- **Details**: Append a bullet under a new development version header
  documenting the removed arguments:
  ```
  * `pipdata_dlw_process()`, `pipdata_get_gmd()`, and
    `pipdata_validate_gmd()` no longer accept `log` or `save_log`
    arguments. All logging is now unconditional and writes to the
    `"pipdata_log"` log. Checkpoint saves happen automatically at
    stage boundaries. `log_report()` now covers DLW acquisition and
    validation in addition to survey cleaning (#unified-logging-report).
  ```
- **Acceptance criteria**: NEWS.md has a version header and bullet documenting the breaking change.

## Testing Strategy

**Testing approaches** (using established patterns from `.cg-docs/solutions/`):

1. **Synthetic `piplog` tests** (for `log_report()` builders):
   Use `make_entry()`/`make_piplog()` helpers to construct minimal logs with
   the new DLW logmeta types. Test each builder independently.

2. **Contract tests** (for DLW logging side effects):
   Since DLW functions depend on external I/O (DLW API, stamp, pipload),
   use condition-based and structure-based contract tests:
   - Document expected logmeta structures for `dlw_acquisition_inf`, `dlw_validation_inf`, `dlw_summary_inf`
   - Verify count arithmetic and field presence
   - Mirror source-code conditions in test assertions

3. **Integration test** (full report):
   Build a synthetic `piplog` with entries from all 12 canonical logmeta
   types (the existing 9 plus the 3 new DLW types).
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
- [ ] Update vignette `.Rmd` code chunks (`Validating-Data.Rmd`, `PIP-data-pipeline.Rmd`) — remove `log`/`save_log` args and update prose
- [ ] NEWS.md breaking-change bullet under new version header

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Breaking callers of `pipdata_dlw_process(log=, save_log=)` | High | Medium | This is intentional. Announce breaking change in NEWS.md (Step 14). The primary in-repo caller surface is the **vignettes** (`Validating-Data.Rmd` lines 44-45, 77-78, 108-109; `PIP-data-pipeline.Rmd` lines 77-78) — updated in Step 12. `Pipdata_script.R` mentions `pipdata_dlw_process` only in a comment (line 35) and needs no change. A grep audit (Step 12.8) catches any other callers. |
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
 ├─► Phase 1.5: pipfun release + pipdata pin (Step 2.5)
 │    │        [pipfun must be released before pipdata can use log_save_checkpoint]
 │    ▼
 ├─► Phase 2: pipdata DLW refactoring (Steps 3–6)
 │    │        [depends on Phase 1.5 for log_save_checkpoint availability]
 │    ▼
 ├─► Phase 3: log_report() extension (Steps 7–11)
 │    │        [depends on Phase 2 for new logmeta types being emitted]
 │    ▼
 └─► Phase 4: Documentation & cleanup (Steps 12–14)
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
- Fixing pre-existing type leakage: several discriminator types emitted by current code (`aux_no_changes_inf`, `aux_changes_no_surveys_inf`, `surveys_to_clean_inf`, `force_surveys_inf`, `force_surveys_unknown_inf`, `aux_na_hash_inf`, `deflate_provenance_missing`, `missing_metadata_err`) are not in `.log_internal_types` and leak into the type-summary table. This is a pre-existing inconsistency unrelated to the DLW unification and is deferred to a separate cleanup task.

## Completion Contract

### Outcome

All active DLW functions use unconditional typed logging with the canonical
DLW logmeta types, checkpoint saves are written at DLW and pipeline stage
boundaries, and `log_report()` produces a stage-aware combined report without
the removed `log` or `save_log` arguments.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required |
|----|-------------------|------------------|----------|
| V1 | The pipfun checkpoint helper passes its unit tests and persists both DLW and pipeline checkpoints with the expected stage metadata | pipfun test suite and `stamp::st_info()` inspection | yes |
| V2 | DLW functions emit only unconditional typed logging with canonical metadata and no `log`/`save_log` parameters or guards | targeted `testthat` contract tests for the DLW functions | yes |
| V3 | `log_report()` renders acquisition, validation, and stage-awareness sections for DLW-only, pipeline-only, combined, empty, and failure logs | targeted `testthat` report tests | yes |
| V4 | Documentation, vignettes, NEWS, NAMESPACE, and context updates are consistent with the breaking API change | `devtools::document()` and documentation grep audit | yes |
| V5 | The complete pipdata package has no new regressions | `devtools::test()` and `devtools::check()` | yes |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | Legacy DLW files remain excluded from this refactoring | inspect `old_files/` and active DLW call sites |
| C2 | The `piplog` data.table schema is unchanged | targeted schema test |
| C3 | Existing pipeline logmeta types and report sections continue to work | combined-log integration test |
| C4 | No new package dependency is introduced without explicit approval | DESCRIPTION diff and package check |
| C5 | Changes to pipfun are coordinated and released before pipdata depends on the checkpoint helper | pipfun release artifact and pipdata version requirement |

### Boundaries

- **Allowed**: pipfun checkpoint API changes and coordinated version pinning;
  active pipdata DLW and pipeline R files; report tests; roxygen, NAMESPACE,
  vignettes, NEWS, and `compound-gpid.context.md` updates required by the
  logging API change.
- **Out of scope**: legacy DLW refactoring, pipaux logging, changes to the
  `piplog` table schema, HTML or interactive reports, performance metrics, and
  consolidating the two wrappers into one function.

### Iteration Policy

1. Complete and verify the pipfun checkpoint prerequisite before changing
   pipdata callers.
2. Run targeted tests after each implementation step, then run the complete
   pipdata test and package-check gates before completion.
3. Under `deviation-policy: ask`, pause before any change to the allowed files,
   API scope, logging schema, dependency set, or sequencing; record the
   approved decision and impact in the execution report.
4. If a test or evidence check fails, make no more than the workflow-approved
   targeted recovery attempts and preserve the failure in the execution report
   when it remains unresolved.

### Blocked-Stop Conditions

- The pipfun checkpoint helper cannot be released or loaded by pipdata.
- A required test, package check, or documentation verification cannot be run.
- Required evidence fails after the allowed recovery attempts.
- A requested change crosses the stated boundaries and approval is unavailable.
- The `piplog` schema or existing pipeline report behavior would need to change.
- A protected artifact or roadmap file would need direct modification.
