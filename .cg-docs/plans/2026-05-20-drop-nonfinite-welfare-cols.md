---
date: 2026-05-20
title: "Drop non-finite welfare columns in prepare_for_arrow()"
status: completed
completed-date: 2026-05-20
scope: Lightweight
estimated-effort: small
tags: [inline, arrow, welfare, bugfix]
---

# Plan: Drop non-finite welfare columns in prepare_for_arrow()

## Objective

Add symmetric handling in `prepare_for_arrow()` for welfare columns that are
entirely non-finite (all NA / Inf / NaN), mirroring the existing all-NA
dimension-column drop logic. Prevents hard aborts for surveys whose
`welfare_ppp_2005_01_01` column is empty (e.g. old KAZ surveys).

## Steps

### 1. Implement welfare column drop logic in `prepare_for_arrow()`

- **File**: `R/arrow_prep.R`
- **Details**: After column selection, detect any welfare column in `wv` where
  no row is finite. Drop it by reference, warn with column name + pip_id,
  update `wv`. Abort only if `wv` is empty after pruning.
- **Acceptance criteria**: Surveys with one invalid welfare column proceed;
  surveys with zero valid welfare columns abort with a clear message.

### 2. Add tests for the new behaviour

- **File**: `tests/testthat/test-arrow-prep.R` (new)
- **Acceptance criteria**:
  - All-NA welfare column dropped with warning; `welfare_vars` attr updated.
  - All-Inf welfare column dropped with warning.
  - All valid columns → no warning, no drop.
  - All columns non-finite → error.
