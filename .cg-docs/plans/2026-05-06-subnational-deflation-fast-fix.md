---
date: 2026-05-06
title: "Fix subnational deflation: resolve area attribute to column values"
status: planned
completed-date: null
scope: "Standard"
brainstorm: .cg-docs/brainstorms/2026-05-06-subnational-deflation-area-resolution.md
language: R
estimated-effort: small
tags: [deflation, subnational, area, ppp_data_level, cpi_data_level, fast-fix, prerequisite]
---

# Plan: Fix Subnational Deflation (area attribute resolution)

## Objective

Fix the broken subnational deflation pipeline: when `ppp_data_level` or 
`cpi_data_level` attributes are set to `"area"` (indicating per-row lookup), 
the deflation functions must resolve them to the per-row values of the `area` 
column. Currently, they treat `"area"` as a literal level name and produce `NA` 
for all subnational surveys (e.g., CHN grouped data with urban/rural domains).

**Scope**: Three-function quick fix in `R/pd_deflation.R`; update existing tests.

**Prerequisite for**: `integrate-deflation` plan — this fix must be completed 
before Step 3 (safe_deflation refactoring) and Step 4 (integration tests) to 
ensure the refactored S3 methods handle subnational data correctly.

## Context

The new pipeline stores `ppp_data_level` and `cpi_data_level` as scalar 
attributes on survey data.tables. When the PFW indicates subnational domains 
(reporting_level == 2, cpi_domain_var == "urban"), `add_dom_vars()` sets these 
to `"area"` — a **pointer to the per-row `area` column**, not a level name.

The deflation code (`add_rep_lvl()`, `add_ppp()`, `add_cpi()`) then uses these 
attribute values as lookup keys. But the named PPP/CPI vectors use actual level 
names (`"rural"`, `"urban"`, `"national"`) — never the literal `"area"`. This 
causes subnational surveys to get `NA` deflation values instead of the correct 
per-row PPP/CPI.

**Design decision**: The `"area"` pointer-to-column convention is intentional and 
will **not** change (that's a future architectural cleanup). Instead, deflation 
internals will be rewritten to recognize the `"area"` sentinel and resolve it to 
`dt$area` values at runtime.

## Requirements

| ID  | Requirement | Source |
|-----|-------------|--------|
| R1  | When `ppp_data_level == "area"`, `add_ppp()` must use per-row `dt$area` values as lookup keys into the named PPP vector | brainstorm |
| R2  | When `cpi_data_level == "area"`, `add_cpi()` must use per-row `dt$area` values as lookup keys into the named CPI vector | brainstorm |
| R3  | When the attribute equals `"national"` (literal level), behavior unchanged — scalar broadcast to all rows | brainstorm |
| R4  | Mixed cases work (e.g., `ppp_data_level = "area"`, `cpi_data_level = "national"`) | brainstorm |
| R5  | Both `pipmd` and `pipgd` S3 methods must deflate subnational data correctly | brainstorm |
| R6  | Existing tests for national-level deflation continue to pass | regression |
| R7  | New test fixtures for subnational case (CHN-like: urban/rural rows with area-keyed PPP/CPI) | test |

## Implementation Steps

### 1. Understand current behavior and validate the bug

- **Requirement**: R1, R2, R3
- **Files**: `R/pd_deflation.R` (inspect only)
- **Details**:
  - Examine `add_rep_lvl()`, `add_ppp()`, `add_cpi()` to confirm they treat 
    `ppp_data_level`/`cpi_data_level` as literal level names
  - Confirm that `dt$area` exists when these attributes are `"area"`
  - Review the named PPP/CPI vector structure produced by `pd_aux_attr()` 
    (keys: `"rural"`, `"urban"`, `"national"`)
- **Tests**: None (inspection only)
- **Acceptance criteria**: Bug location confirmed; code paths identified

### 2. Modify `add_rep_lvl()` to resolve "area" → `dt$area`

- **Requirement**: R1, R3, R4
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  - Locate the fallback assignment in `add_rep_lvl()` that sets `reporting_level` 
    when columns are missing (currently: 
    `dt[, reporting_level := ppp_data_level]`)
  - Add a conditional: if `ppp_data_level == "area"`, assign `reporting_level := dt$area` 
    (per-row lookup key); otherwise, assign the literal level value
  - Same logic for `cpi_data_level` fallback
  - **Code pattern**:
    ```r
    rep_lvl <- attr(dt, "ppp_data_level")
    if (!is.null(rep_lvl)) {
      if (rep_lvl == "area") {
        # "area" is a column pointer — resolve to per-row values
        dt[, reporting_level := area]
      } else {
        # "national" or other literal level
        dt[, reporting_level := rep_lvl]
      }
    }
    ```
- **Tests**: Existing unit tests should continue to pass (national-level case 
  with literal `reporting_level = "national"`)
- **Acceptance criteria**: Subnational fixture with `area = c("rural", "urban")` 
  produces columns `reporting_level = c("rural", "urban")`

### 3. Modify `add_ppp()` to resolve "area" → per-row lookup

- **Requirement**: R1, R2, R3, R4
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  - Locate the PPP merge/broadcast logic in `add_ppp()` (currently treats 
    `ppp_data_level` as a literal level name)
  - Add conditional: if `ppp_data_level == "area"`, use `dt$area` as per-row 
    lookup keys into the named PPP vector; otherwise, use the literal level 
    to broadcast a scalar value to all rows
  - **Code pattern** (simplified):
    ```r
    ppp_level <- attr(dt, "ppp_data_level")
    if (ppp_level == "area") {
      # Per-row lookup: use dt$area to index into ppp{...} named vector
      dt[, ppp := ppp[as.character(area)]]
    } else if (ppp_level == "national") {
      # Scalar broadcast: use the "national" value for all rows
      dt[, ppp := ppp["national"]]
    }
    ```
  - Named vectors have keys `c("rural", "urban", "national")` — ensure the 
    lookup doesn't produce `NA` for missing keys
- **Tests**: Existing unit tests for national-level should pass; new test for 
  subnational lookup added in Step 4
- **Acceptance criteria**: Subnational fixture with `area = c("rural", "urban")` 
  and `ppp = c(rural = 1.2, urban = 1.5, national = 1.3)` produces 
  `dt$ppp = c(1.2, 1.5)` (per-row lookup, not all `NA`)

### 4. Modify `add_cpi()` with same "area" → per-row lookup logic

- **Requirement**: R2, R3, R4
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  - Apply the same "area" conditional as Step 3, but for `cpi_data_level` 
    and the CPI merge
  - Same named-vector structure (`"rural"`, `"urban"`, `"national"`) assumed
  - **Code pattern**: Identical to Step 3 but with CPI variable names
- **Tests**: None new (covered by Step 4)
- **Acceptance criteria**: CPI lookup follows same per-row pattern as PPP

### 5. Add/update test fixtures for subnational case

- **Requirement**: R5, R7
- **Files**: `tests/testthat/test-pd-deflation.R` (update), or new fixture file
- **Details**:
  - Create a `make_pipmd_subnational()` helper (if not already present) that 
    returns a pipmd fixture with:
    - `area = c("rural", "urban", "rural")` (repeating pattern)
    - `ppp_data_level = "area"` (as attribute, scalar)
    - `cpi_data_level = "area"` (as attribute, scalar)
    - Other required columns/attributes (welfare, weight, survey_id, etc.)
  - Create named-vector fixtures for PPP and CPI:
    ```r
    ppp_subnational <- c(rural = 1.2, urban = 1.5, national = 1.3)
    cpi_subnational <- c(rural = 100, urban = 105, national = 102)
    ```
  - Write test:
    ```r
    test_that("add_ppp resolves area attribute to per-row lookup", {
      dt <- make_pipmd_subnational()
      result <- add_ppp(dt, ppp = ppp_subnational)
      expect_equal(result$ppp, c(1.2, 1.5, 1.2))  # per-row, not NA
    })
    ```
  - Same pattern for `add_cpi()` and the deflation S3 methods
- **Tests**: Three new unit tests (add_ppp, add_cpi, safe_deflation with mixed 
  levels), plus existing regression tests
- **Acceptance criteria**: All subnational tests pass; all national-level 
  regression tests still pass

### 6. Run full test suite and verify no regressions

- **Requirement**: R6
- **Files**: `tests/testthat/` (run all)
- **Details**:
  - Execute `devtools::test()` or `testthat::test_file()`
  - Verify no breakage to existing national-level deflation behavior
  - Verify subnational cases produce numeric (not `NA`) results
- **Tests**: Full suite
- **Acceptance criteria**: All tests pass; no regressions

## Testing Strategy

| Layer | What | How |
|-------|------|-----|
| Unit | `add_ppp()` with `ppp_data_level = "area"` | Direct call with fixture; verify per-row lookup not `NA` |
| Unit | `add_cpi()` with `cpi_data_level = "area"` | Direct call with fixture; verify per-row lookup not `NA` |
| Unit | Mixed case (one "area", one "national") | Mixed fixture; verify both paths work |
| Unit | Fallback (literal level without area column) | Fixture without `area` column; should use literal level value |
| Regression | `add_ppp()` with `ppp_data_level = "national"` | Existing tests must still pass |
| Regression | `add_cpi()` with `cpi_data_level = "national"` | Existing tests must still pass |
| Integration | `deflation.pipmd()` with subnational fixture | Full S3 method call; verify output has numeric welfare_ppp columns |
| Integration | `deflation.pipgd()` with subnational fixture | Full S3 method call; verify output has numeric welfare_ppp columns |

## Documentation Checklist

- [ ] Inline comments added to `add_rep_lvl()`, `add_ppp()`, `add_cpi()` explaining the `"area"` sentinel and per-row lookup logic
- [ ] Test fixtures documented with `@keywords internal` comments
- [ ] No roxygen changes needed (functions already documented)

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Named PPP/CPI vectors missing keys for `"rural"` or `"urban"` during lookup | Test explicitly for missing keys; abort with informative error rather than silent `NA` |
| Subnational data reaches deflation without `area` column | Assertion in `add_rep_lvl()`: `stopifnot(!is.null(dt$area))` when `ppp_data_level == "area"` |
| Regression: scalar broadcast broken for national-level | Regression tests still pass; existing behavior unchanged for non-"area" attributes |
| Confusion between attribute value and column name | Inline comment in each function: `# "area" is a pointer to the area column, not a literal level name` |

## Out of Scope

- Changing how `add_dom_vars()` stores level attributes (that's a future architectural 
  cleanup tracked in roadmap as `explicit-data-level-semantics`)
- Refactoring `add_rep_lvl()`, `add_ppp()`, or `add_cpi()` beyond the minimal "area" 
  resolution fix
- Full integration of subnational deflation into the pipeline orchestrator 
  (that's part of the main `integrate-deflation` plan)

## Integration with `integrate-deflation` Plan

This plan is a **prerequisite** for the active `integrate-deflation` plan:

- Step 1 of `integrate-deflation` validates input and loads metadata — assumes 
  deflation functions work correctly for all survey types
- Step 3 (refactoring `safe_deflation()` helper) modifies the S3 methods that 
  call `add_ppp()`/`add_cpi()` — this fix ensures they work correctly post-refactor
- Step 4 (integration tests) must cover subnational cases — this plan's test 
  fixtures support that

**Sequencing**: Complete this plan before starting Step 2 of `integrate-deflation`.

