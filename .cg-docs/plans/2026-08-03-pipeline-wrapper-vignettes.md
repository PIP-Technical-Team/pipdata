---
date: 2026-08-03
title: "Vignettes for pipeline wrapper, deflation, and logging functions"
status: active
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-08-03-pipeline-wrapper-vignettes.md"
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
execution-report: ".cg-docs/work-reports/2026-08-03-pipeline-wrapper-vignettes.md"
current-phase: 1
tags: [documentation, vignettes, pkgdown, pipeline, deflation, logging]
---

# Plan: Vignettes for pipeline wrapper, deflation, and logging functions

## Objective

Replace outdated/empty vignette content with accurate, non-executing narrative
documentation covering the two pipeline wrapper functions
(`pipdata_dlw_process`, `pd_process_data`), `deflation`/`pd_deflation`, and
`log_report`, for both operators (how to run) and maintainers (architecture),
and wire the result into `_pkgdown.yml`.

## Context

Research findings that ground this plan (verified against current `R/`
source, `Pipdata_script.R`, and `docs/pipeline_overview.qmd`):

1. **Deflation is NOT yet wired into `pd_process_data()`.** `process_data()`'s
   per-survey pipeline is: `inv_dlw_load()` -> `pd_cpfw_merge()` ->
   `pd_dlw_clean()` -> `pd_aux_attr()` -> `save_pip_data()` x2.
   `deflation()`/`pd_deflation()` is never called there — confirmed by
   `Pipdata_script.R`'s own end-of-script test block, which calls
   `pd_deflation(pip_id = ...)` as a separate, manual, post-hoc step. The
   vignette must describe deflation as a **standalone step run after
   cleaning**, matching `pd_deflation()`'s own `@note`.
2. **`log_report()` only covers `pd_process_data()`'s `"pipdata_log"`.** It
   does not consume any log from `pipdata_dlw_process()` (DLW
   acquisition/validation). `docs/pipeline_overview.qmd` flags this
   explicitly as a known gap — the vignette should state this scope honestly.
3. **`docs/pipeline_overview.qmd`** is the canonical architecture reference
   (Pipeline Alignment Audit milestone), largely accurate with Mermaid
   diagrams, but is a `docs/` Quarto article, not a package vignette, and has
   **two known stalenesses** to correct when using it as a reference:
   (a) it says `process_data()` is "mapped via `purrr::map()`", but source
   now uses `lapply()` (per completed `purrr-to-lapply-audit`); and
   (b) it describes `pd_process_data()`'s output as a "cleaned,
   **deflated**, PIP-ready dataset" — this directly contradicts Context #1
   (deflation is NOT wired into `pd_process_data()`; the concept of
   "processing" has changed and no longer includes deflation) and must not
   be carried into the new vignettes. Treat the reference doc as
   historical/partially stale, not as ground truth to copy verbatim.
4. **Real call sequence** (from `Pipdata_script.R`):
   `pipfun::setup_working_release()` -> `pd_process_data(inv)` ->
   `pipfun::log_filter(name = "pipdata_log")` -> `log_report()` ->
   (separately) `pd_deflation()`.
5. The old `Processing-Data.Rmd` prose (`pd_split_alt_welfare`,
   `pd_dlw_clean`, `pd_wbpip_clean`, `get_country_pfw`, `pd_add_pip_vars`) is a
   mix of still-valid low-level functions and dead code — `pd_add_pip_vars`
   was archived (`archive-dead-pip-vars`, done). Confirms it must be replaced,
   not patched.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Verify every function name/signature/call sequence against current `R/` source before writing; treat old prose as unreliable | Brainstorm |
| R2 | Fill `PIP-data-pipeline.Rmd` with a non-executing orchestration/architecture overview (three-wrapper model + `pipdata_dlw_process`) | Brainstorm |
| R3 | Rewrite `Processing-Data.Rmd` around `pd_process_data`, `deflation`/`pd_deflation` (as a standalone post-hoc step), and `log_report` (scoped honestly to `pipdata_log`) | Brainstorm |
| R4 | Add `articles:`/`reference:` index to `_pkgdown.yml` | Brainstorm |
| R5 | Diagram refresh explicitly out of scope (deferred) | Brainstorm |
| R6 | Create `Validating-Data.Rmd` documenting the internal mechanics of `pipdata_dlw_process()` and its delegates (`pipdata_get_gmd()`, `pipdata_validate_gmd()`); update `PIP-data-pipeline.Rmd`'s companion-article cross-reference to point to it alongside `Processing-Data.Rmd` | Mid-execution deviation, 2026-08-04, user-confirmed |

## Implementation Steps

### Phase 1: Audit and overview vignette

### 1. Source-of-truth audit
- **Requirements**: R1
- **Files**: none changed (research only, output feeds steps 2-3)
- **Details**: Confirm current exported signatures for `pipdata_dlw_process`,
  `pd_process_data`, `pd_deflation`/`deflation`, `log_report`; confirm
  deflation's non-integration; note any further drift from
  `docs/pipeline_overview.qmd`.
- **Test Scenarios**: n/a (research step)
- **Tests**: n/a
- **Acceptance criteria**: a short verified fact list exists to drive steps 2-3.

### 2. Write `PIP-data-pipeline.Rmd` (orchestration overview)
- **Requirements**: R1, R2
- **Files**: `vignettes/articles/PIP-data-pipeline.Rmd`
- **Details**: Non-executing (`eval = FALSE`) narrative covering the
  three-wrapper model (`update_aux_measures` context mention only — owned by
  pipaux — then `pipdata_dlw_process()` and `pd_process_data()` in detail),
  an operator "how to run" section modeled on `Pipdata_script.R`, and a
  maintainer "why/architecture" section. Do **not** reference or embed
  `pipeline_flow.png` (diagram refresh deferred; no old `.png` files are
  reused in either rewritten vignette). Any summary of `pd_process_data()`'s
  output must NOT describe it as "deflated" (see Context #3b) — deflation is
  a separate downstream step.
- **Test Scenarios**: renders via `rmarkdown::render()`/
  `devtools::build_rmd()` without error; all referenced function names exist
  and are exported; no `eval = TRUE` code chunks that require live
  DLW/network access; no reference to `pipeline_flow.png`.
- **Tests**: `rmarkdown::render()` on the `.Rmd` directly (note:
  `devtools::check()` does NOT validate this file — `vignettes/articles` is
  excluded via `.Rbuildignore`); manual read-through against R1 audit.
- **Acceptance criteria**: `rmarkdown::render()` succeeds; every named
  function/argument matches current source; explicitly states deflation is a
  separate manual step; no image reference; no "deflated" mischaracterization
  of `pd_process_data()` output.

### Phase 2: Deep-dive rewrite, new validation vignette, pkgdown wiring, validation

### 3. Rewrite `Processing-Data.Rmd` (deep dive)
- **Requirements**: R1, R3
- **Files**: `vignettes/articles/Processing-Data.Rmd`
- **Details**: Replace outdated `pd_*` sub-chain prose. New structure:
  `pd_process_data(inv)` end-to-end (inputs, per-survey loop,
  outputs/inventory — described as cleaning only, NOT deflation, per Context
  #1/#3b), then `pd_deflation()`/`deflation()` (Mode A vs Mode B, and the
  "not yet automatic" caveat), then `log_report()` (scope limited to
  `pipdata_log`, example from `Pipdata_script.R`). `eval = FALSE` throughout.
  Do **not** reference or embed `images/pd_functions.png` (it diagrams the
  old `pd_*` sub-chain including archived `pd_add_pip_vars`; no old `.png`
  files are reused in either rewritten vignette).
- **Test Scenarios**: renders without error; code chunks use only current,
  exported functions; no reference to archived functions (`pd_add_pip_vars`,
  etc.); no reference to `pd_functions.png`.
- **Tests**: `rmarkdown::render()` on the `.Rmd` directly (note:
  `devtools::check()` does NOT validate this file — `vignettes/articles` is
  excluded via `.Rbuildignore`); manual cross-check against
  `R/pd_process_data.R`, `R/pd_deflation.R`, `R/log_report.R`.
- **Acceptance criteria**: `rmarkdown::render()` succeeds; accurately
  reflects deflation's standalone status and log_report's scope; no image
  reference.

### 4. Write `Validating-Data.Rmd` (DLW acquisition/validation deep dive)
- **Requirements**: R1, R6
- **Files**: `vignettes/articles/Validating-Data.Rmd` (new), `vignettes/articles/PIP-data-pipeline.Rmd` (cross-reference update)
- **Details**: New non-executing (`eval = FALSE`) vignette explaining the
  internal mechanics of `pipdata_dlw_process()`: its two delegates
  `pipdata_get_gmd()` (checks `dlw_gmd_new()` for new/updated datasets,
  downloads via `dlw::dlw_get_gmd()` per row, marks `data_available`) and
  `pipdata_validate_gmd()` (identifies unvalidated local datasets via
  `dlw_gmd_unvalidated()`, diffs against the prior validated inventory via
  `gmd_to_validate()`/`gmd_validated()`, dispatches to module-specific
  `dlw_validation_*()` functions, writes the updated `gmd_valid_inv`). Same
  audience split as the other vignettes (operator + maintainer sections). No
  image references (no old `.png` files reused). Update the "companion
  article" cross-reference in `PIP-data-pipeline.Rmd` (the line currently
  pointing only to `Processing-Data.html`) to reference both
  `Validating-Data.html` (DLW acquisition/validation mechanics) and
  `Processing-Data.html` (survey cleaning, deflation, logging).
- **Test Scenarios**: renders without error; all referenced functions
  (`pipdata_dlw_process`, `pipdata_get_gmd`, `pipdata_validate_gmd`,
  `dlw_gmd_new`, `dlw_gmd_unvalidated`) exist in the current source; no image
  references; `PIP-data-pipeline.Rmd`'s cross-reference correctly links both
  companion articles.
- **Tests**: `knitr::knit()`/`rmarkdown::render()` on the `.Rmd` directly
  (pandoc unavailable in this environment — `knitr::knit()` to an
  intermediate `.md` is an acceptable substitute since all chunks are
  `eval = FALSE`); manual cross-check against `R/pipdata_dlw_process.R`,
  `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R`.
- **Acceptance criteria**: knits/renders cleanly; every named
  function/argument matches current source; `PIP-data-pipeline.Rmd`
  cross-reference updated; no image reference.

### 5. Update `_pkgdown.yml`
- **Requirements**: R4
- **Files**: `_pkgdown.yml`
- **Details**: Add an `articles:` section listing all three vignettes
  (grouped, e.g., "Pipeline"), and a minimal `reference:` index grouping the
  functions in scope (the four original plus `pipdata_get_gmd`,
  `pipdata_validate_gmd`) so the site organizes them. First check whether
  `pkgdown` is installed locally before relying on it (it is not currently a
  declared dependency in `DESCRIPTION` Suggests).
- **Test Scenarios**: `pkgdown::build_site()` (or `check_pkgdown()`) doesn't
  error on the new YAML; no orphaned/undocumented `.Rd` warnings introduced.
- **Tests**: `pkgdown::build_site(preview = FALSE)` or
  `pkgdown::check_pkgdown()` if available locally; otherwise
  `yaml::read_yaml("_pkgdown.yml")` as a syntax-only fallback.
- **Acceptance criteria**: YAML parses; site build (or dry-run) succeeds, or
  the syntax-only fallback passes with the gap noted.

### 6. Final validation pass
- **Requirements**: R1-R4, R6
- **Files**: all three vignettes, `_pkgdown.yml`
- **Details**: Full re-read of all three vignettes side-by-side against
  source once more; confirm no stale references remain; run
  `devtools::document()`/`devtools::check()` to ensure nothing broke.
- **Test Scenarios**: full package check passes (or shows only pre-existing,
  unrelated notes/warnings).
- **Tests**: `devtools::check()`
- **Acceptance criteria**: no new check failures attributable to this change.

## Testing Strategy

No unit tests apply (documentation-only change). Verification is via
vignette build success + manual source cross-checks + `devtools::check()`.

## Documentation Checklist

- [ ] `PIP-data-pipeline.Rmd` filled with orchestration/architecture overview
- [ ] `Processing-Data.Rmd` rewritten around `pd_process_data` + deflation + `log_report`
- [ ] `Validating-Data.Rmd` created covering `pipdata_dlw_process` internals
- [ ] `PIP-data-pipeline.Rmd` cross-reference updated to link both companion articles
- [ ] `_pkgdown.yml` updated with `articles:`/`reference:` index (3 vignettes)
- [ ] `NEWS.md` bullet (optional — ask before adding)

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Vignette implies deflation runs automatically | Explicit "not yet integrated" callout, cross-checked against source |
| `docs/pipeline_overview.qmd`'s stale "deflated" description of `pd_process_data()` output is silently copied into the new vignettes | Explicit Context #3b correction; manual grep for "deflat" near `pd_process_data` prose before finishing |
| Existing `\dontrun{}` examples in `.Rd` files (e.g. `pd_deflation`, `pd_process_data`) diverge from the new vignette prose | Cross-check `@examples` blocks against vignette content during Step 5 final validation |
| `_pkgdown.yml` reference index omits functions, causing pkgdown warnings | Run `pkgdown::check_pkgdown()` before finishing, or note the gap if `pkgdown` is unavailable |

## Out of Scope

- New orchestrator script/function
- Diagram image regeneration (`pd_functions.png`, `pipeline_flow.png`) — deferred per roadmap
- Changes to `docs/pipeline_overview.qmd`
- Changes to `pipdata_dlw_process`/`pd_process_data`/`pipdata_get_gmd`/`pipdata_validate_gmd` source code
- New diagram/image for `Validating-Data.Rmd`

## Completion Contract

### Outcome
`PIP-data-pipeline.Rmd`, `Processing-Data.Rmd`, and `Validating-Data.Rmd`
accurately document the current three-wrapper pipeline architecture
(ingestion/validation, processing, deflation, logging) for operators and
maintainers — correctly describing deflation as a standalone step not part
of "processing" — wired into `_pkgdown.yml`, with no references to outdated
or archived functions, and no old `.png` diagram references
(`pipeline_flow.png`, `pd_functions.png`).

### Verification Surface
| ID | Evidence Required | Command/Artifact | Required |
|----|--------------------|-------------------|----------|
| V1 | All three vignettes render without error (sole evidence — `devtools::check()` does NOT cover these files; `vignettes/articles` is `.Rbuildignore`'d) | `rmarkdown::render()`/`knitr::knit()` per `.Rmd` | yes |
| V2 | No stale/archived function references remain, and no "deflated" mischaracterization of `pd_process_data()` output | Manual grep of vignette text vs. exported `R/` functions; grep for "deflat" near `pd_process_data` prose | yes |
| V3 | `_pkgdown.yml` parses and site builds | `pkgdown::build_site()`/`check_pkgdown()`, or `yaml::read_yaml()` fallback | yes |
| V4 | `devtools::check()` shows no new package-level NOTEs/WARNINGs (does NOT cover vignette content — see V1) | Check log | yes |
| V5 | No `pipeline_flow.png` reference remains | grep `vignettes/articles/PIP-data-pipeline.Rmd` | yes |
| V6 | No `pd_functions.png` reference remains | grep `vignettes/articles/Processing-Data.Rmd` | yes |
| V7 | `Validating-Data.Rmd` renders without error and references only current, exported functions | `knitr::knit()`/`rmarkdown::render()`; manual grep vs. `R/pipdata_get_gmd.R`, `R/pipdata_validate_gmd.R` | yes |
| V8 | `PIP-data-pipeline.Rmd` cross-reference links both `Validating-Data.html` and `Processing-Data.html` | grep the "companion article" paragraph | yes |

### Constraints
| ID | Constraint | Check |
|----|------------|-------|
| C1 | Non-executing vignettes (`eval = FALSE`) | grep chunk options |
| C2 | No new package dependencies | `DESCRIPTION` diff |
| C3 | No source (`R/`) changes | git diff scoped to `vignettes/` + `_pkgdown.yml` |

### Boundaries
- Allowed: edits to the three vignette `.Rmd` files (including creating
  `Validating-Data.Rmd`), `_pkgdown.yml`.
- Out of scope: orchestrator script, diagram regeneration, `R/` source
  changes, `docs/pipeline_overview.qmd`.

### Iteration Policy
1. If source audit reveals additional drift beyond what's noted here, surface
   it before writing prose.
2. If `pkgdown::check_pkgdown()` isn't available locally, fall back to YAML
   syntax validation only and note the gap.

### Blocked-Stop Conditions
- If a described function's signature can't be confirmed from source
  (ambiguous/conflicting), stop and ask rather than guess.
