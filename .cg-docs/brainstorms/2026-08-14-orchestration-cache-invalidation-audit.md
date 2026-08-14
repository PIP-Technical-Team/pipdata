---
title: "End-to-End Orchestration & Smart Cache-Invalidation Audit"
date: 2026-08-14
status: ideation
tags: [orchestration, cache-invalidation, pipeline, architecture]
---

# End-to-End Orchestration & Smart Cache-Invalidation Audit

## Context

The `pipdata` package aims to serve as the end-to-end orchestration engine for PIP data production. The user's request centers on: (1) intelligent pipeline management that tracks dependencies, (2) smart caching/invalidation so that a single upstream component change (e.g., 2018 Colombia CPI) triggers recomputation of only directly downstream data, and (3) a production-ready architecture for full pipeline orchestration.

## Current Roadmap Summary (4 milestones, 33 ideas)

| Milestone | Total | Done | In-Progress | Planned/Idea |
|-----------|-------|------|-------------|--------------|
| Code Quality & Refactoring | 17 | 8 | 1 | 8 |
| Function Decomposition & Cleanup | 3 | 2 | 0 | 1 |
| Testing & Documentation | 3 | 3 | 0 | 0 |
| Pipeline Alignment Audit | 15 | 10 | 0 | 5 |

Key open roadmap items: `logging-refactor`, `unified-logging-report`, `loop-to-apply`, `purrr-to-lapply-audit` (in-progress), `subfunctions`, `audit-copy-dlw-meta`, `audit-dontrun-examples`, `run-manifest-audit`, `content-hash-reclean-trigger` (done), `deflate-pipeline-wrapper`, `explicit-data-level-semantics`.

## Analysis Passes

### Pass 1: Pain Points

1. **7 copy-pasted validation functions** (~900 duplicated lines in `R/pipdata_dlw_validation.R`): `dlw_validation_gpwg`, `dlw_validation_group`, `dlw_validation_bin`, `dlw_validation_hist`, `dlw_validation_all`, `dlw_validation_aspire`, `dlw_validation_l` follow identical patterns differing only in variable regexes.
2. **44% untested**: 12 of 27 R files have no test file, including `pd_process_data.R` (main orchestrator), `pipdata_dlw_validation.R` (all validation), `pd_cpfw_merge.R`, `pd_aux_attr.R`, `inv_dlw_load.R`.
3. **Dual legacy/modern code paths** in `pd_deflation.R` (1,020 lines): `add_ppp()`, `add_cpi()`, `adjust_population()` each branch on `data.table::is.data.table()` for legacy vs named-vector paths. `.deflation_pipmd_core` and `.deflation_pipgd_core` are near-identical.
4. **`cat()` debug statements** in `R/recode_spec.R:502,556,558` — uncontrolled stdout output in production pipeline runs.
5. **Dead code**: 4 deprecated functions still exported in `pd_dlw_clean.R` (~200 lines), extensive commented-out code blocks in 6 files.
6. **valid_dlw_load.R**: 658 lines with 6 levels of nesting, duplicated log entries at L234 and L267.
7. **25 tryCatch calls** with inconsistent patterns (silent swallow, custom piperr, generic error, flow control).

### Pass 2: Architecture

1. **No formal pipeline stage interface**: Acquisition returns invisible(NULL), validation accumulates in `.pipdataenv`, cleaning returns inventory, deflation returns data.table or NA. No shared result contract.
2. **Global `.pipdataenv` as hidden data bus** (R/aaa.R): validation writes via `pd_env_append()`, cleaning uses `process_survey_id`/`save_id_name`, deflation uses `log_survey_id`. Invisible coupling blocks parallelism.
3. **Duplicated tryCatch/log/return-NULL pattern** in 5+ locations with two parallel logging paths (`.pipdataenv` vs `pipfun::log_add()`).
4. **`_pkgdown.yml`**: Only 7 of 50+ exports get meaningful grouping; 45+ dumped in catch-all "Other".

### Pass 3: Quality

1. **4 error-throwing mechanisms**: `cli::cli_abort` (21 untyped calls), `rlang::abort`, `stop()`, `stopifnot()`. Inconsistent `class = "piperr"` tagging.
2. **Dead code in `pipdata_dlw_compare.R:211-217`**: `return()` before `cli_abort()` — dead code masks missing validation inventory.
3. **Hardcoded module list** `c("GPWG", "GROUP", "BIN", "HIST", "ALL", "ASPIRE", "L")` in 6+ locations. Magic numbers for age bounds, NA thresholds, welfare conversion.
4. **Missing input validation** in key exported functions: `get_country_pfw`, `pd_cpfw_merge`, `ppp_to_wide`, `save_pip_data`.
5. **No code-hash invalidation**: If R function logic changes, previously-cleaned data is silently stale. Only data file and aux file changes trigger re-processing.
6. **`force = TRUE` is all-or-nothing**: No way to force re-processing for specific surveys/countries/measures.
7. **No invalidation DAG**: Pipeline steps have implicit data dependencies but no formal dependency graph for step-level caching.

## Generated Ideas

| # | Idea | Category | Impact | Effort | Signal Source |
|---|------|----------|--------|--------|---------------|
| 1 | Consolidate 7 DLW validation functions into data-driven validator | code | high | medium | Pain Points #1, Architecture #1 |
| 2 | Add code-hash invalidation for pipeline function changes | perf | high | medium | Quality #5 |
| 3 | Add targeted `force_surveys` parameter to `pd_process_data()` | code | high | small | Quality #6 |
| 4 | Formalize pipeline stage interface with typed result objects | code | high | large | Architecture #1 |
| 5 | Standardize error handling: single `safe_pipeline_step()` wrapper | code | high | medium | Architecture #3, Quality #1 |
| 6 | Eliminate `.pipdataenv` as cross-stage side channel | code | medium | large | Architecture #2 |
| 7 | Remove `cat()` debug statements from `recode_spec.R` | code | high | small | Pain Points #4 |
| 8 | Remove dual legacy/modern code paths in `pd_deflation.R` | code | medium | medium | Pain Points #3 |
| 9 | Reorganize `_pkgdown.yml` by pipeline stage | docs | medium | small | Architecture #4 |
| 10 | Add test coverage for `pd_process_data.R` and validation subsystem | test | high | large | Pain Points #2 |
| 11 | Build dependency DAG for step-level cache invalidation | perf | high | large | Quality #7 |
| 12 | Add missing input validation to exported functions | code | medium | small | Quality #4 |

## Adversarial Filter

| # | Idea | Verdict | Reason |
|---|------|---------|--------|
| 1 | Data-driven validation engine | **KEEP** | Not on roadmap. `dlw-wrapper-rewrite` is about structural mirroring, not validation consolidation. Distinct. |
| 2 | Code-hash invalidation | **KEEP** | Not on roadmap. `content-hash-reclean-trigger` (done) covers DLW data changes only, not function code changes. |
| 3 | Targeted `force_surveys` | **KEEP** | Not on roadmap. Concrete, small effort, high impact. |
| 4 | Pipeline stage interface | **KEEP** | Not on roadmap. `dlw-wrapper-rewrite` is about DLW wrapper only; this is cross-stage. |
| 5 | `safe_pipeline_step()` | **KEEP** | Related to `unified-logging-report` but distinct. Logging harmonization is about log format; this is about error-handling pattern. |
| 6 | Eliminate `.pipdataenv` | **KEEP** | Not on roadmap. Distinct from logging refactor. |
| 7 | Remove `cat()` debug | **KEEP** | Not on roadmap. Trivial fix with high signal. |
| 8 | Remove legacy deflation paths | **KEEP** | Not on roadmap. `explicit-data-level-semantics` is about attribute encoding, not code-path removal. |
| 9 | Reorganize `_pkgdown.yml` | **KEEP** | Not on roadmap. Small effort, good discoverability. |
| 10 | Test coverage for orchestrator | **KEEP** | Not on roadmap (existing test items are for logging and specific functions, not the orchestrator). |
| 11 | Dependency DAG for invalidation | **KEEP** | Not on roadmap. Core to the user's cache-invalidation vision. |
| 12 | Input validation | **KEEP** | Not on roadmap. Complementary to code quality. |

No ideas were rejected. All 12 survive the filter.

## Ranked Ideas (Impact/Effort)

### Tier 1: Critical Path for Orchestration Engine

1. **Code-hash invalidation for pipeline function changes**
   - Category: perf | Impact: high | Effort: medium
   - Why: Without this, any R function logic change silently serves stale data. Core to the cache-invalidation vision.
   - Signal: Current invalidation only tracks DLW content hashes and aux file hashes, not function body hashes.

2. **Add targeted `force_surveys` parameter to `pd_process_data()`**
   - Category: code | Impact: high | Effort: small
   - Why: `force = TRUE` is destructive (timestamp versioning, bypasses all invalidation). Production needs surgical re-runs.
   - Signal: `pd_process_data.R:61-64` switches stamp to timestamp mode globally.

3. **Consolidate 7 DLW validation functions into data-driven validator**
   - Category: code | Impact: high | Effort: medium
   - Why: ~900 lines of duplicated validation logic. Bug fixes must be applied 7 times. A single parameterized engine with YAML/module-spec config eliminates this.
   - Signal: `R/pipdata_dlw_validation.R` (1,105 lines), 7 identical function structures.

4. **Formalize pipeline stage interface with typed result objects**
   - Category: code | Impact: high | Effort: large
   - Why: No shared result contract means stages can't be composed, resumed, or inspected. Blocks orchestration.
   - Signal: Acquisition returns NULL, validation accumulates in env, cleaning returns inventory, deflation returns DT/NA.

5. **Build dependency DAG for step-level cache invalidation**
   - Category: perf | Impact: high | Effort: large
   - Why: The user's core requirement: "if Colombia 2018 CPI updates, recompute only downstream." Currently only survey-level invalidation exists, not step-level.
   - Signal: `valid_dlw_load.R` tracks aux hashes at survey level but no formal DAG exists.

### Tier 2: Foundation for Clean Architecture

6. **Standardize error handling: single `safe_pipeline_step()` wrapper**
   - Category: code | Impact: high | Effort: medium
   - Why: 5+ duplicated tryCatch/log/return-NULL patterns + 21 untyped cli_abort calls. Silent error swallowing hides production failures.
   - Signal: `pd_process_data.R:221-303`, `save_pip.R:52-80`, `pipdata_get_gmd.R:100-141`.

7. **Eliminate `.pipdataenv` as cross-stage side channel**
   - Category: code | Impact: medium | Effort: large
   - Why: Global mutable state creates invisible coupling and blocks future parallel survey processing.
   - Signal: `R/aaa.R` defines the environment; validation, cleaning, and deflation all read/write it.

8. **Remove dual legacy/modern code paths in `pd_deflation.R`**
   - Category: code | Impact: medium | Effort: medium
   - Why: Each deflation helper has two code paths (data.table vs named-vector). Doubles bug surface.
   - Signal: `pd_deflation.R:600-657` (add_ppp), `673-732` (add_cpi), `902-1019` (adjust_population).

### Tier 3: Quick Wins and Quality

9. **Remove `cat()` debug statements from `recode_spec.R`**
   - Category: code | Impact: high | Effort: small
   - Why: `cat()` on stdout cannot be suppressed. Runs during every pipeline execution.
   - Signal: `R/recode_spec.R:502,556,558`.

10. **Add test coverage for `pd_process_data.R` and validation subsystem**
    - Category: test | Impact: high | Effort: large
    - Why: The main orchestrator and all validation logic have zero tests. Highest-risk components.
    - Signal: 12/27 R files untested, including the two most complex subsystems.

11. **Reorganize `_pkgdown.yml` by pipeline stage**
    - Category: docs | Impact: medium | Effort: small
    - Why: Current docs dump 45+ functions in catch-all. Pipeline stages should be the organizing principle.
    - Signal: `_pkgdown.yml` has 4 sections; only 7 functions get meaningful grouping.

12. **Add missing input validation to exported functions**
    - Category: code | Impact: medium | Effort: small
    - Why: Key functions like `get_country_pfw`, `pd_cpfw_merge`, `ppp_to_wide` lack defensive checks.
    - Signal: Contrast with `.validate_deflation_input()` (R/pd_deflation.R:6-51) which is exemplary.
