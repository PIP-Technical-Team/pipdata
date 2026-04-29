---
date: 2026-04-29
title: "Unified logging and reporting across pipdata (revised)"
status: active
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-04-28-unified-logging.md"
language: "R"
estimated-effort: "medium"
tags: [logging, reporting, dlw, pipeline, harmonization, pipfun]
supersedes: ".cg-docs/plans/2026-04-28-unified-logging.md"
---

# Plan: Unified Logging and Reporting Across pipdata (Revised)

Supersedes: `.cg-docs/plans/2026-04-28-unified-logging.md`
Review findings incorporated from `/cg-plan-review` session (2026-04-29).

## Objective

Unify the logging infrastructure so all pipdata functions (DLW acquisition,
validation, and survey cleaning) write to the same `piplog` format, use
canonical logmeta types, and produce a single consolidated report via
`log_report()`. Eliminate the `log`/`save_log` arguments and replace with
unconditional logging and automatic checkpoint saves.

## Context

The brainstorm (`.cg-docs/brainstorms/2026-04-28-unified-logging.md`) chose
**Approach 1 — Lift and Standardize**: migrate DLW delegate functions to the
pipeline's always-on, typed-logmeta pattern. Long-term goal is Approach 3
(DLW wrapper rewrite), tracked as `dlw-wrapper-rewrite` in roadmap.

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

**Log initialization**: `log_init("pipdata_log", overwrite = TRUE)` is called
in `.onLoad()` (see `R/zzz.R`). No additional init is needed.

**Key design decisions** (from review):

1. **`args` column is auto-captured; ignore its content for error handlers.**
   Inside `tryCatch` error handlers and `lapply` callbacks, `capture_log_args()`
   captures the anonymous handler's `e` argument, not the enclosing function's
   context. This is acceptable — all structured data goes into `logmeta`.
   Nobody consumes `args` for report generation.

2. **`logmeta$error` is always a string type discriminator**, never a condition
   object. The caught condition's message is stored as `condition_msg`.

3. **Coarse-grained DLW logmeta types** with `phase` sub-field.
   DLW uses 3 coarse types (`dlw_download_inf`, `dlw_validation_inf`,
   `dlw_summary_inf`) excluded from the general type_summary table, with
   dedicated builder sections surfacing failures. This matches how pipeline
   types work (`process_summary_inf`, `null_svys_inf` etc. also have dedicated
   sections). Rationale: DLW errors are fewer than pipeline errors; granularity
   isn't needed; the dedicated sections provide full context; keeping them out
   of the general tables avoids diluting the pipeline diagnostic view.

4. **Checkpoint saves use existing stamp aliases** — `"dlw_meta"` for DLW
   checkpoint, `"piplog"` for pipeline checkpoint. Future consolidation into
   a single alias is tracked as `unified-log-folder` in roadmap.

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
| R9  | Coordinate with pipfun for any API additions (checkpoint helper) | brainstorm |
| R10 | Define logmeta type strings as constants in `aaa.R` to prevent typos | review P2.9 |
| R11 | DLW functions return early (not abort) when no work to do, so checkpoint always fires | review P2.6 |
| R12 | Create new orchestration script; deprecate `Pipdata_script.R` | review P2.4 |

## Implementation Steps

### Phase 1: pipfun Changes

#### Step 1. Add `log_save_checkpoint()` helper to pipfun

- **Requirements**: R4, R9
- **Files**: `pipfun/R/log_checkpoint.R` (new file)
- **Details**: Create a thin wrapper around `log_save()` that:
  1. Accepts a `stage` argument (`"dlw"` or `"pipeline"`)
  2. Accepts a required `alias` argument (no default — forces callers to be explicit about storage routing)
  3. Constructs an `id` like `"pipdata_log_checkpoint_{stage}"`
  4. Attaches `metadata = list(stage = stage, checkpoint_time = Sys.time())`
  ```r
  log_save_checkpoint <- function(
    name  = getOption("pipfun.log.default"),
    stage = c("dlw", "pipeline"),
    alias,
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
  - ❌ Error path: invalid `stage` value — `match.arg()` aborts; missing `alias` — standard R missing-arg error
- **Tests**: Unit test in pipfun checking id construction and metadata attachment.
- **Acceptance criteria**: `log_save_checkpoint(stage = "dlw", alias = "dlw_meta")` persists the log; `stamp::st_info()` shows the expected id and metadata.

#### Step 2. Verify `log_info`/`log_error` cover DLW needs

- **Requirements**: R8, R9
- **Files**: `pipfun/R/log_helpers.R` (read-only audit)
- **Details**: Confirm that `log_info()`/`log_error()` work for DLW contexts:
  - Inside `tryCatch` error handlers: `capture_log_args()` will capture the
    handler's `e` argument — this is acceptable. All meaningful data goes in
    `logmeta`.
  - Inside `lapply` callbacks: same behavior, same resolution.
  - **Decision**: No `args` parameter needed. Drop explicit `args` from all
    DLW calls. Rely on `logmeta` for structured data.
  - If any DLW call currently passes `args` without `logmeta`, merge the
    content into `logmeta` during refactoring (Steps 3–4).
- **Test Scenarios**:
  - ✅ `log_error()` inside `tryCatch` handler produces a valid `piplog` row
  - 🛑 `log_info()` inside `lapply` callback produces a valid row
- **Acceptance criteria**: Confirmed via manual test that all DLW logging use cases work with `log_info()`/`log_error()` + `logmeta`.

### Phase 2: pipdata DLW Function Refactoring

> **Note**: Phase 3 (report builders) can develop and test in parallel with
> Phase 2, since builders use synthetic `piplog` data. Only the final
> integration test at the end requires both phases complete.

#### Step 3. Define logmeta type constants in `aaa.R`

- **Requirements**: R3, R7, R10
- **Files**: `R/aaa.R`
- **Details**: Add named constants and register in `.log_internal_types`:
  ```r
  # Canonical logmeta type strings — DLW stage
  .logtype_dlw_download   <- "dlw_download_inf"
  .logtype_dlw_validation <- "dlw_validation_inf"
  .logtype_dlw_summary    <- "dlw_summary_inf"

  # Internal logmeta type markers -- excluded from the summary-by-type table
  .log_internal_types <- c(
    "process_summary_inf",
    "aux_changes_inf",
    "inv_update_inf",
    "null_svys_inf",
    "skipped_svys_data",
    "skipped_svys_metadata",
    .logtype_dlw_download,
    .logtype_dlw_validation,
    .logtype_dlw_summary
  )
  ```
  All subsequent steps reference the constants (e.g., `.logtype_dlw_download`)
  rather than inline string literals. Contract tests verify the emitted values
  match the constants.
- **Tests**: Verify `.log_internal_types` contains all 9 entries. Verify constants match expected strings.
- **Acceptance criteria**: Constants defined, referenced in all log calls and builders. R CMD check passes.

#### Step 4. Refactor `pipdata_get_gmd()` — remove `log`/`save_log`, add typed logmeta

- **Requirements**: R1, R2, R3, R5, R8, R10, R11
- **Files**: `R/pipdata_get_gmd.R`
- **Details**:
  1. Remove `log` and `save_log` from function signature and roxygen.
  2. Remove all `if (log) { ... }` guards.
  3. Replace `log_add("info", ...)` with `log_info(...)` and `log_add("error", ...)` with `log_error(...)`.
  4. Replace ad-hoc logmeta with canonical types using constants:
     - **Start entry**: `log_info("DLW download started.", name = "pipdata_log", logmeta = list(info = .logtype_dlw_download, phase = "start", n_surveys = nrow(inv_gmd)))`
     - **Per-survey failure** (in tryCatch error handler): `log_error(msg, name = "pipdata_log", logmeta = list(error = .logtype_dlw_download, survey = <survey_id>, country = country, year = year, module = md_type, condition_msg = conditionMessage(e)))`
     - **Inventory save**: `log_info("Inventory saved.", name = "pipdata_log", logmeta = list(info = .logtype_dlw_download, phase = "complete", saved_at = pip_folders$dlw_inventory))`
  5. Remove the `log_save()` call at the end (checkpoint saves move to `pipdata_dlw_process`).
  6. **Replace `cli_abort` when no new data with early-return + info log**:
     ```r
     if (is.null(inv_gmd) || nrow(inv_gmd) == 0) {
       pipfun::log_info(
         "No new GMD data found.",
         name = "pipdata_log",
         logmeta = list(info = .logtype_dlw_download, phase = "no_new_data")
       )
       return(invisible(NULL))
     }
     ```
     This ensures the checkpoint in `pipdata_dlw_process()` always fires.
- **Test Scenarios**:
  - ✅ Happy path: 50 surveys download, all succeed → start + complete entries, no errors
  - 🛑 Edge case: 3 of 50 fail → start + 3 error entries + complete entry
  - 🛑 Edge case: no new data → `no_new_data` info entry, early return
  - ❌ Error path: all surveys fail → start + N errors + complete entry
- **Tests**: Contract tests for `dlw_download_inf` logmeta structure; verify `condition_msg` field is present on error entries; verify `phase` values match expected set.
- **Acceptance criteria**: No `log`/`save_log` in signature. No `if (log)` guards. All calls use typed wrappers with constants. No abort on empty catalog.

#### Step 5. Refactor `pipdata_validate_gmd()` — same treatment

- **Requirements**: R1, R2, R3, R5, R8, R10, R11
- **Files**: `R/pipdata_validate_gmd.R`
- **Details**:
  1. Remove `log` and `save_log` from signature and roxygen.
  2. Remove all `if (log) { ... }` guards (~8 occurrences).
  3. Replace `log_add` calls with typed wrappers using constants.
  4. Define canonical logmeta entries:
     - **Start**: `log_info("DLW validation started.", logmeta = list(info = .logtype_dlw_validation, phase = "start", n_surveys = nrow(gmd_new)))`
     - **Per-survey load failure**: `log_error(msg, logmeta = list(error = .logtype_dlw_validation, survey = file_id, phase = "load", condition_msg = conditionMessage(e)))`
     - **Inventory save**: `log_info("Validation inventory saved.", logmeta = list(info = .logtype_dlw_validation, phase = "inventory_save", saved_at = pip_folders$dlw_metadata))`
     - **Report save**: `log_info("Validation report saved.", logmeta = list(info = .logtype_dlw_validation, phase = "report_save"))`
     - **Inventory not generated (error)**: `log_error("Inventory not generated.", logmeta = list(error = .logtype_dlw_validation, phase = "inventory_fail"))`
     - **Old report load failure**: `log_error(msg, logmeta = list(error = .logtype_dlw_validation, phase = "report_load_fail", condition_msg = conditionMessage(e)))`
     - **Old inv load failure**: `log_error(msg, logmeta = list(error = .logtype_dlw_validation, phase = "inv_load_fail", condition_msg = conditionMessage(e)))`
  5. Remove the `log_save()` call AND the trailing `log_add("info", "logging file is saved")` that follows it (orphaned meta-log entry).
  6. **Replace `cli_abort` when no surveys to validate** with early-return + info log (same pattern as Step 4).
- **Test Scenarios**:
  - ✅ Happy path: all surveys valid → start + inventory_save + report_save entries
  - 🛑 Edge case: some surveys fail load → per-survey error entries with `phase = "load"`
  - 🛑 Edge case: no surveys to validate → `no_new_data` info entry, early return
  - ❌ Error path: inventory not generated → error entry with `phase = "inventory_fail"`
- **Tests**: Contract tests for `dlw_validation_inf` logmeta structure; condition tests for each logging branch.
- **Acceptance criteria**: No `log`/`save_log`, no guards, typed wrappers, constants, no orphaned entries. No abort on empty input.

#### Step 6. Refactor `pipdata_dlw_process()` — remove args, add summary + checkpoint

- **Requirements**: R1, R2, R3, R4, R10
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
         info = .logtype_dlw_summary,
         get_dlw_data = get_dlw_data,
         validate_dlw_data = validate_dlw_data
       )
     )
     ```
  4. Add a **checkpoint save** after the summary entry:
     ```r
     pipfun::log_save_checkpoint(
       name  = "pipdata_log",
       stage = "dlw",
       alias = "dlw_meta"
     )
     ```
- **Test Scenarios**:
  - ✅ Happy path: both steps run → `dlw_summary_inf` entry written, checkpoint saved
  - 🛑 Edge case: `get_dlw_data = FALSE` — only validation runs; summary still written
  - 🛑 Edge case: delegates return early (no new data) — summary + checkpoint still fire
  - ❌ Error path: delegate throws unexpected error — handled by caller
- **Tests**: Contract test for `dlw_summary_inf` structure.
- **Acceptance criteria**: `pipdata_dlw_process()` signature has no `log`/`save_log`. Summary entry and checkpoint file exist after any successful run (including "no work" runs).

#### Step 7. Add pipeline checkpoint save to `pd_process_data()`

- **Requirements**: R4
- **Files**: `R/pd_process_data.R`
- **Details**: After the existing `process_summary_inf` log entry, add:
  ```r
  pipfun::log_save_checkpoint(
    name  = "pipdata_log",
    stage = "pipeline",
    alias = "piplog"
  )
  ```
  This creates symmetry: both wrappers produce a checkpoint.
- **Test Scenarios**:
  - ✅ Happy path: checkpoint file created after processing
  - 🛑 Edge case: no surveys to process (early return) — no checkpoint (acceptable; no processing occurred)
- **Tests**: Verify checkpoint is written when processing completes.
- **Acceptance criteria**: After `pd_process_data()` completes, a `"pipeline"` checkpoint exists.

### Phase 3: `log_report()` Extension

> **Parallelizable with Phase 2**: Steps 8–12 use synthetic `piplog` data
> via `make_entry()`/`make_piplog()` helpers. No dependency on the actual
> refactored DLW functions.

#### Step 8. Add `build_dlw_download_summary()` to `log_report.R`

- **Requirements**: R5, R6, R10
- **Files**: `R/log_report.R`
- **Details**: New internal builder that:
  1. Finds all entries where `error_type == .logtype_dlw_download`
  2. Counts info-level (success/progress) and error-level (failures)
  3. Renders a "DLW Download Summary" section with counts
  4. Lists failed surveys using `vapply` (consistent with existing builders):
  ```r
  build_dlw_download_summary <- function(dt) {
    dl_idx <- which(dt$error_type == .logtype_dlw_download)
    if (length(dl_idx) == 0L) return(character(0))

    n_errors <- sum(dt$event[dl_idx] == "error")
    n_info   <- sum(dt$event[dl_idx] == "info")

    lines <- c(
      "## DLW Download Summary",
      "",
      sprintf("**Downloads:** %d info entries, %d failures.", n_info, n_errors)
    )

    fail_idx <- dl_idx[dt$event[dl_idx] == "error"]
    if (length(fail_idx) > 0L) {
      fail_lines <- vapply(fail_idx, \(i) {
        meta <- dt$logmeta[[i]]
        svy <- meta$survey %||% "unknown"
        reason <- meta$condition_msg %||% ""
        if (nzchar(reason)) {
          sprintf("- `%s` \u2014 %s", svy, reason)
        } else {
          sprintf("- `%s`", svy)
        }
      }, character(1))
      lines <- c(lines, "", "**Failed downloads:**", "", fail_lines)
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

- **Requirements**: R5, R6, R10
- **Files**: `R/log_report.R`
- **Details**: Same pattern as Step 8 but for `.logtype_dlw_validation`. Group
  by `phase` to show inventory save, report save, and per-survey failures.
  Use `vapply` for failure line construction.
- **Test Scenarios**: Same pattern — present, absent, all-errors.
- **Tests**: Synthetic `piplog` tests.
- **Acceptance criteria**: Section appears when validation entries exist.

#### Step 10. Add stage-aware warning to `log_report()`

- **Requirements**: R6, R10
- **Files**: `R/log_report.R`
- **Details**: New `build_stage_warning()` builder:
  1. Detect which stages ran by checking for marker entries:
     - `.logtype_dlw_summary` present → DLW stage ran
     - `"process_summary_inf"` present → Pipeline stage ran
  2. If only DLW ran:
     ```markdown
     > **Partial run:** Only DLW acquisition/validation completed.
     > Survey cleaning (`pd_process_data`) was not executed.
     ```
  3. If only pipeline ran (no DLW):
     ```markdown
     > **Note:** DLW acquisition was not part of this run.
     ```
  4. If both ran: return `character(0)` (no warning).
  5. If neither marker: return generic "incomplete run" warning.
- **Test Scenarios**:
  - ✅ Both stages → no warning
  - 🛑 DLW only → warning about missing pipeline
  - 🛑 Pipeline only → note about missing DLW
  - ❌ Neither marker → generic warning
- **Tests**: Synthetic `piplog` with each combination; verify warning text.
- **Acceptance criteria**: Correct stage warnings in all scenarios.

#### Step 11. Wire new builders into `log_report()` section list

- **Requirements**: R5, R6
- **Files**: `R/log_report.R`
- **Details**: Insert the new builders into the `sections` list:
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
- **Tests**: Full integration test: build a `piplog` with entries from both
  stages; verify all sections appear in order.
- **Acceptance criteria**: `log_report()` produces a complete report with DLW + pipeline sections.

#### Step 12. Check `globalVariables()` for new symbols

- **Requirements**: (R CMD check compliance)
- **Files**: `R/aaa.R`
- **Details**: Check whether new builders introduce unquoted symbols in
  data.table `[i, j, by]` expressions that need registering in
  `utils::globalVariables()`. Add any that trigger R CMD check NOTEs.
- **Acceptance criteria**: `R CMD check` produces no NOTEs about undefined global variables.

### Phase 4: Documentation & Orchestration

#### Step 13. Update roxygen and NAMESPACE

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

#### Step 14. Create new orchestration script

- **Requirements**: R12
- **Files**: New file (e.g., `inst/scripts/run_pipeline.R` or top-level script)
- **Details**:
  1. Write a clean orchestration script using the new API (no `log`/`save_log`):
     ```r
     # 1. Setup
     pipfun::setup_working_release(release = "20260401", identity = "PROD")

     # 2. DLW acquisition & validation
     pipdata::pipdata_dlw_process(release = "20260401", identity = "PROD")

     # 3. Survey cleaning
     inv <- pipload::load_gmd_valid_inv()
     pip_inv <- pipdata::pd_process_data(inv = inv)

     # 4. Report
     log <- pipfun::log_filter(name = "pipdata_log")
     pipdata::log_report(log, path = "log_report.md", overwrite = TRUE)
     ```
  2. `Pipdata_script.R` remains as-is (deprecated, not modified in this plan).
     Ask user before removing it in a future cleanup.
- **Acceptance criteria**: New script runs the full pipeline with unified logging.

#### Step 15. Update `compound-gpid.context.md`

- **Files**: `compound-gpid.context.md`
- **Details**:
  1. Update the "Domain Rules" section: add the 3 new DLW logmeta types to
     the canonical list (now 9 total: 6 pipeline + 3 DLW).
  2. Add a note that `log`/`save_log` arguments were removed.
  3. Document the convention: `logmeta$error`/`logmeta$info` is always a
     string type discriminator; condition messages go in `condition_msg`.
- **Acceptance criteria**: Context file reflects the new logging conventions.

## Testing Strategy

**Testing approaches** (using established patterns from `.cg-docs/solutions/`):

1. **Synthetic `piplog` tests** (for `log_report()` builders):
   Use `make_entry()`/`make_piplog()` helpers to construct minimal logs with
   the new DLW logmeta types. Test each builder independently. Reference
   constants from `aaa.R` in test assertions to catch typo mismatches.

2. **Contract tests** (for DLW logging side effects):
   Since DLW functions depend on external I/O (DLW API, stamp, pipload),
   use condition-based and structure-based contract tests:
   - Document expected logmeta structures for all 3 DLW types
   - Verify `condition_msg` field is present on error entries
   - Verify `phase` values match expected set
   - Mirror source-code conditions in test assertions

3. **Integration test** (full report):
   Build a synthetic `piplog` with entries from all 9 logmeta types.
   Run `log_report()` and verify all sections appear with correct content
   and in execution order (DLW sections before pipeline sections).

4. **Stage-awareness tests**:
   Test `log_report()` with DLW-only logs, pipeline-only logs, and combined
   logs. Verify correct warning messages.

**Edge cases to cover**:
- Empty DLW stage (no surveys to download/validate) — early return path
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
- [ ] Inline comments for logmeta type constants in `aaa.R`
- [ ] New orchestration script with usage comments

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Breaking callers of `pipdata_dlw_process(log=, save_log=)` | High | Medium | New orchestration script replaces `Pipdata_script.R`. Ask before removing old script. Package is still in dev — no external consumers. |
| `log_info()` auto-capture produces garbage `args` in error handlers | Certain | None | Accepted by design. `args` column is diagnostic; all structured data in `logmeta`. Nobody reads `args` for reporting. |
| New logmeta type strings have typos → silently break report | Medium | High | Type strings defined as constants in `aaa.R` (`.logtype_dlw_*`). Contract tests verify emitted values match constants. |
| `log_save_checkpoint()` not yet available in pipfun when pipdata work begins | Low | High | Phase 1 (pipfun) must be merged first. Pin pipfun version in DESCRIPTION if needed. |

## Sequencing & Dependencies

```
P0: archive-legacy-dlw (prerequisite)
 │
 ├─► Phase 1: pipfun changes (Steps 1–2)
 │    │
 │    ▼
 ├─► Phase 2: pipdata DLW refactoring (Steps 3–7)
 │    │        [depends on Phase 1 for log_save_checkpoint]
 │    │
 │    │   Phase 3: log_report() extension (Steps 8–12)
 │    │        [can develop in PARALLEL with Phase 2]
 │    │        [uses synthetic piplog data, no dependency on refactored functions]
 │    │        [only final integration test needs both phases complete]
 │    │
 │    ▼
 └─► Phase 4: Documentation & orchestration (Steps 13–15)
              [depends on Phase 2 + 3 complete]
```

## Out of Scope

- Legacy DLW file refactoring — archived separately (prerequisite P0)
- `pipaux` logging — separate package, separate effort
- Approach 3 (DLW wrapper rewrite) — future roadmap item `dlw-wrapper-rewrite`
- Changing the `piplog` data.table column schema
- HTML or interactive report formats
- Performance metrics (timing per survey)
- Consolidating the two wrappers into a single function
- Consolidating checkpoint aliases into a single folder — tracked as `unified-log-folder`
