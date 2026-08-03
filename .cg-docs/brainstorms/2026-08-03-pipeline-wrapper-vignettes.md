---
date: 2026-08-03
title: "Vignettes for pipeline wrapper, deflation, and logging functions"
status: decided
scope: "Standard"
chosen-approach: "Two-vignette split by architectural layer"
tags: [documentation, vignettes, pkgdown, pipeline, deflation, logging]
---
<!-- Valid status values: decided, in-progress, abandoned -->

# Vignettes for pipeline wrapper, deflation, and logging functions

## Context

The package's narrative documentation does not match the current three-wrapper
architecture. An audit of the existing vignettes found:

- `vignettes/articles/PIP-data-pipeline.Rmd` is **essentially empty** — a title
  plus a single flow image, no prose.
- `vignettes/articles/Processing-Data.Rmd` **exists but is outdated** — it
  documents the older `pd_*` sub-chain (`pd_split_alt_welfare`, `pd_dlw_clean`,
  `pd_wbpip_clean`, `pd_add_pip_vars`, `get_country_pfw`) and stops before the
  `pd_process_data` wrapper. It does not cover deflation or logging.
- Roxygen `.Rd` docs for `pipdata_dlw_process`, `pd_process_data`,
  `pd_deflation`/`deflation`, and `log_report` are **solid** (good
  `@param`/`@details`/`@examples`).
- `_pkgdown.yml` is **minimal** — no `articles:` or `reference:` index, so
  vignettes are not organized/discoverable on the pkgdown site.

The gap: the two top-level wrappers (`pipdata_dlw_process`, `pd_process_data`),
`deflation`, and `log_report` have good reference docs but **no narrative
vignette** tying them together for either operators or maintainers.

## Requirements

- **Audience**: internal GPID/PIP team members who *run* the pipeline
  (operational how-to) **and** maintainers who need to understand the
  **architecture** to modify it. Each vignette needs both a "how to run" and a
  "how it fits together / why" dimension.
- **Executability**: **non-executing** vignettes (`eval = FALSE`), consistent
  with the existing article style. The wrappers require a configured working
  release, datalibweb access, and write to PIP storage, so they cannot run
  during pkgdown build.
- **Structure**: fill the empty `PIP-data-pipeline.Rmd` as the high-level
  orchestration/architecture overview; rewrite the outdated
  `Processing-Data.Rmd` to explain `pd_process_data` + deflation + logging.
- **Accuracy**: treat the old prose as unreliable. **Verify every function name,
  signature, and call sequence against the current `R/` source** before writing.
  Reconcile content to the canonical three-wrapper architecture rather than
  re-describing possibly-superseded internals.
- **In scope**: update `_pkgdown.yml` with an `articles:`/`reference:` index so
  the vignettes are organized and discoverable.
- **Deferred (follow-up)**: refreshing/regenerating the pipeline diagram images
  (`images/pd_functions.png`, `images/pipeline_flow.png`).

## Approaches Considered

### Approach 1: Two-vignette split by architectural layer (CHOSEN)

Fill `PIP-data-pipeline.Rmd` as the high-level orchestration/architecture
overview (the three-wrapper model + `pipdata_dlw_process` ingestion/validation);
rewrite `Processing-Data.Rmd` as the processing deep-dive (`pd_process_data` +
`deflation`/`pd_deflation` + `log_report`).

- **Pros**: matches the chosen structure exactly; clean separation of "operate"
  (overview) vs "understand internals" (deep-dive); reuses existing files; each
  vignette serves both audiences via an overview section plus a mechanics
  section.
- **Cons**: two files to keep in sync as the API evolves; some conceptual
  overlap at the wrapper boundary.
- **Effort**: Medium.

### Approach 2: Single consolidated end-to-end vignette

One long article covering all four functions; leave the other stub minimal.

- **Pros**: single source of truth; no cross-file sync.
- **Cons**: long and hard to navigate; buries the architecture overview inside
  operational detail; conflicts with the decision to use both files.
- **Effort**: Medium.

### Approach 3: Four focused micro-vignettes (one per function)

Separate short articles for each wrapper/function.

- **Pros**: highly targeted; easy to link from roxygen.
- **Cons**: fragments the end-to-end narrative both audiences need; more
  `_pkgdown.yml` wiring; higher maintenance overhead.
- **Effort**: Large.

## Decision

**Approach 1 — Two-vignette split by architectural layer.**

It directly implements the chosen structure, reuses the existing files (filling
the empty stub and replacing outdated prose), and gives both operators and
maintainers what they need without fragmenting the end-to-end narrative.

Devil's-advocate outcomes:
- Vignettes are justified over reference-docs-only because reference docs do not
  convey architecture (maintainers) or an end-to-end runbook (operators). Keep
  vignettes lean; link to `.Rd` for parameter detail instead of duplicating it.
- Highest-value work: the orchestration overview (fills the empty stub) plus the
  function-accuracy reconciliation. Avoid exhaustive re-documentation of `pd_*`
  internals already covered by roxygen.
- Charter caveat: verify whether the old `pd_*` sub-chain is still part of the
  shipped architecture or absorbed into `pd_process_data`, so the rewrite
  reflects the canonical three-wrapper model.

## Next Steps

Handoff to `/cg-plan`. The plan should cover:

1. **Source audit** — inventory the current, shipped signatures and call order
   for `pipdata_dlw_process`, `pd_process_data`, `deflation`/`pd_deflation`,
   `log_report`, and confirm the status of the older `pd_*` sub-chain against
   the three-wrapper architecture. Record any renamed/removed functions.
2. **`PIP-data-pipeline.Rmd` (overview)** — write the high-level
   orchestration/architecture vignette: the three-wrapper model, where
   `pipdata_dlw_process` (ingestion + validation) fits, and how the pieces
   connect. `eval = FALSE`. Include an "operate" section and a "why/architecture"
   section.
3. **`Processing-Data.Rmd` (deep-dive)** — rewrite around `pd_process_data`, then
   `deflation`/`pd_deflation`, then `log_report`. `eval = FALSE`. Remove/replace
   outdated `pd_*` prose per the audit.
4. **`_pkgdown.yml`** — add `articles:` and `reference:` indexes to organize and
   surface the vignettes and function reference on the site.
5. **Deferred** — diagram image refresh (`pd_functions.png`, `pipeline_flow.png`)
   tracked as a follow-up, not part of this work.
