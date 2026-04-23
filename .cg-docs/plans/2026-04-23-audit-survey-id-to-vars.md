---
date: 2026-04-23
title: "Audit survey_id_to_vars usage across pipeline"
status: completed
scope: "Lightweight"
brainstorm: ~
language: R
estimated-effort: small
tags: [refactoring, survey-processing, pipload, audit]
---

# Plan: Audit survey_id_to_vars usage across pipeline

## Objective

Determine whether `pipload::survey_id_to_vars()` is called redundantly
across the pipdata pipeline and consolidate if so.

## Context

The function parses a `survey_id` string (e.g.,
`"BOL_1990_EPF_V01_M_V01_A_GMD_GROUP"`) into component columns
(`country_code`, `surveyid_year`, `survey_acronym`, `vermast`, `veralt`,
`collection`, `module`, `tool`). It includes an early return when the
target columns already exist, so redundant calls are harmless but noisy.

## Findings

### Call sites in pipdata (3)

| # | File | Line | Data source | Redundant? |
|---|------|------|-------------|:----------:|
| 1 | `R/update_dlw_inventory.R` | 99 | Raw file listing from PowerShell directory scan | No |
| 2 | `R/pipdata_validate_gmd.R` | 246 | Newly constructed inventory rows (`bind_rows(new_inv)`) | No |
| 3 | `R/dlw_scan_and_validate.R` | 293 | Newly constructed inventory rows (`bind_rows(new_inv)`) | No |

### Call site in pipload (1, upstream)

| # | File | Line | Data source | Redundant? |
|---|------|------|-------------|:----------:|
| 4 | `pipload::pip_load_dlw.R` | 246 | Loaded survey microdata (different data path) | No |

### Analysis

Each call operates on a **different data object** built from a different
source. No single data frame flows through multiple `survey_id_to_vars()`
calls in sequence. The pipload internal call (site 4) processes survey
microdata, while the pipdata calls (sites 1-3) process inventory rows —
completely separate data paths.

## Decision

**No consolidation needed.** All 3 pipdata calls are independent and
correctly placed. The roadmap item can be marked as done.

## Out of Scope

- Refactoring `survey_id_to_vars()` itself (lives in pipload).
- Performance of the parsing logic (already fast — simple `tstrsplit`).
