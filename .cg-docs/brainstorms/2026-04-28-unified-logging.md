---
date: 2026-04-28
title: "Unified logging and reporting across pipdata"
status: decided
scope: "Standard"
chosen-approach: "Lift and Standardize (Approach 1)"
tags: [logging, reporting, dlw, pipeline, harmonization, pipfun]
---
<!-- Valid status values: decided, in-progress, abandoned -->

# Unified Logging and Reporting Across pipdata

## Context

Two roadmap items — `logging-refactor` (standardize `log = TRUE` argument
pattern) and `unified-logging-report` (harmonize DLW and pipeline logging into
one piplog/log_report system) — address the same underlying problem: pipdata
has two incompatible logging conventions.

| Aspect | DLW wrapper (`pipdata_dlw_process`) | Pipeline wrapper (`pd_process_data`) |
|--------|-------------------------------------|--------------------------------------|
| Log argument | `log = TRUE` + `if (log)` guards | No argument — always logs |
| Log persistence | Delegates call `log_save()` via `save_log` arg | Log stays in memory |
| logmeta conventions | Ad-hoc error strings | Canonical types (`aux_changes_inf`, `null_svys_inf`, etc.) |
| Report coverage | Not covered by `log_report()` | Fully covered |

Active DLW surface: `pipdata_dlw_process`, `pipdata_get_gmd`,
`pipdata_validate_gmd`. Three legacy DLW files (`dlw_dta_to_qs`,
`dlw_get_dta`, `dlw_scan_and_validate`) are slated for archival and excluded
from this effort.

## Requirements

Gathered from Q&A:

1. **Sequential execution**: DLW acquisition runs first, then pipeline
   cleaning, in the same R session. A single `piplog` object accumulates
   entries from both stages.
2. **`log_report()` is the primary consumer**: The markdown report is the main
   output. Raw log checkpoint files are kept for safety/auditability but are
   secondary.
3. **Always log**: Eliminate `log = TRUE` and `save_log = TRUE` arguments
   entirely. All logging is unconditional.
4. **Checkpoint saves at stage boundaries**: Log is persisted to disk
   automatically after the DLW step and after the pipeline step — no opt-in
   flag.
5. **Log everything, report failures**: All DLW events (successes and
   failures) are captured in `piplog`. `log_report()` surfaces mainly
   failures; successes are summarised as counts.
6. **Stage-aware reporting**: `log_report()` works after DLW-only runs
   (warns that cleaning was not executed) and after full pipeline runs.
7. **`pipfun` changes are on the table**: New logmeta types, possible new
   helpers (e.g. `log_save_checkpoint()`), and any structural changes to the
   `piplog` data.table format can be proposed.
8. **Prerequisite**: Legacy DLW files must be archived first (Pipeline
   Alignment Audit milestone, `archive-legacy-dlw` item) to reduce the
   surface area before this work begins.

## Approaches Considered

### Approach 1: Lift and Standardize (chosen)

Migrate DLW delegate functions to use the same always-on, typed-logmeta
pattern as the pipeline side. Define new canonical logmeta types for DLW
events. Extend `log_report()` with DLW sections. Add unconditional checkpoint
saves at stage boundaries.

**Pros**:
- One logging convention across the entire package
- `log_report()` becomes the single source of truth
- Eliminates `log`/`save_log` from the API surface
- Minimal `pipfun` changes (new logmeta types + checkpoint helper)

**Cons**:
- Touches `pipdata_dlw_process`, `pipdata_get_gmd`, `pipdata_validate_gmd` signatures (breaking change)
- Requires defining logmeta types for DLW events upfront
- Need to write/update tests for new DLW log entries and report sections

**Effort**: Medium (3–5 days across pipdata + pipfun)

### Approach 2: Adapter Layer

Keep DLW functions logging as-is internally; add an adapter at the end of
`pipdata_dlw_process()` that translates DLW log entries into canonical
logmeta entries for `log_report()`.

**Pros**: Minimal changes to DLW internals.
**Cons**: Two styles persist; adapter can drift; doesn't clean up API.
**Effort**: Small (1–2 days).

### Approach 3: Pipeline-First Rewrite (long-term goal)

Rewrite `pipdata_dlw_process()` and delegates to structurally mirror
`pd_process_data()` — survey-level map loop with tryCatch/piperr,
unconditional logging, DLW summary logmeta entry.

**Pros**: Strongest architectural alignment; enables future single end-to-end function.
**Cons**: Largest scope; higher regression risk; premature before DLW architecture stabilizes.
**Effort**: Large (5–8 days).

## Decision

**Approach 1 — Lift and Standardize**, with Approach 3 as the declared
long-term direction. Approach 1 is the right increment: it fully unifies
logging conventions and makes `log_report()` the single endpoint without
over-engineering the DLW wrapper architecture.

**Sequencing**: The `archive-legacy-dlw` item from Pipeline Alignment Audit
must complete first to reduce surface area and avoid wasted work on files
being removed.

## pipfun Requirements Spec

Changes needed in `pipfun` before or alongside this work:

1. **New canonical logmeta types** (strings used in `info`/`error` fields):
   - `dlw_download_inf` — per-survey download outcome (success/failure)
   - `dlw_validation_inf` — per-survey validation outcome (pass/fail + reason)
   - `dlw_summary_inf` — aggregate DLW step summary (total/success/fail counts)
2. **`log_save_checkpoint(stage)`** — helper that persists current `piplog`
   to a timestamped file, tagged with stage name (`"dlw"` or `"pipeline"`).
   Replaces manual `log_save()` calls.
3. **Stage marker entries**: A mechanism (logmeta field or dedicated entry
   type) for `log_report()` to detect which stages have run. Could be as
   simple as `dlw_summary_inf` presence → DLW ran; `process_summary_inf`
   presence → pipeline ran.
4. **Verify `log_info`/`log_error` helpers** cover all use cases currently
   handled by raw `log_add("info", ...)` / `log_add("error", ...)` in DLW
   functions. If not, extend them.

## Next Steps

1. Complete `archive-legacy-dlw` (Pipeline Alignment Audit milestone)
2. Create implementation plan via `/cg-plan` covering:
   - pipfun changes (new logmeta types, checkpoint helper)
   - pipdata DLW function refactoring (remove `log`/`save_log`, add typed logmeta)
   - `log_report()` extension (DLW sections, stage-aware header/warning)
   - Test updates (DLW logging contracts, report section tests)
3. Coordinate branch strategy: pipfun changes merged first, then pipdata
