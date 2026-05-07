---
date: 2026-05-06
title: "Fix subnational deflation: resolve area attribute to column values"
status: completed
completed-date: 2026-05-07
scope: "Standard"
brainstorm: .cg-docs/brainstorms/2026-05-06-subnational-deflation-area-resolution.md
language: R
estimated-effort: small
tags: [deflation, subnational, area, ppp_data_level, cpi_data_level, fast-fix, prerequisite]
---

# Plan: Fix Subnational Deflation (area attribute resolution)

## Objective

Fix subnational deflation by eliminating the `reporting_level` **column** and 
using the `area` column directly as the per-row lookup key in all deflation 
functions. Each deflation function branches on its **own** `*_data_level` 
attribute (`ppp_data_level`, `cpi_data_level`, `pop_data_level`) rather than 
a single integer discriminator — this correctly handles the mixed-domain case 
where `reporting_level == 2` but some domains are national.

Currently, `add_rep_lvl()` creates a `reporting_level` column that `add_ppp()`, 
`add_cpi()`, and `adjust_population()` use as a lookup key. For national 
surveys it holds `"national"` (scalar broadcast); for subnational surveys it 
should hold per-row values from `dt$area` but currently receives the literal 
`"area"` string, producing `NA` everywhere.

**Revised approach**: Remove `add_rep_lvl()` entirely. Each function checks its 
own `*_data_level` attr:
- `add_ppp()`: `ppp_data_level == "area"` → per-row `dt$area` lookup; else → 
  scalar broadcast using attr value (e.g., `"national"`)
- `add_cpi()`: `cpi_data_level == "area"` → per-row `dt$area` lookup; else → 
  scalar broadcast
- `adjust_population()` guard: `pop_data_level == "area"` → call; else → skip

This handles the mixed-domain case where `reporting_level == 2` but 
`ppp_data_level == "national"` (because `ppp_domain == 1` in the PFW).

**Scope**: Remove `add_rep_lvl()`, modify `add_ppp()`, `add_cpi()`, 
`adjust_population()`, and the core S3 methods in `R/pd_deflation.R`; archive 
dead `get_ordered_level()` in `R/utils.R`; update tests.

**Prerequisite for**: `integrate-deflation` plan — this fix must be completed 
before Step 3 (safe_deflation refactoring) and Step 4 (integration tests) to 
ensure the refactored S3 methods handle subnational data correctly.

## Context

The pipeline sets two pieces of metadata on each cleaned survey:

1. **`reporting_level` attribute** (integer 1 or 2): set by `add_main_att()` 
   from `cpfw$reporting_level`. This is the authoritative national/subnational 
   flag. It is **not** a column.

2. **`ppp_data_level` / `cpi_data_level` attributes** (string `"national"` or 
   `"area"`): set by `add_dom_vars()`. When `"area"`, it means "use per-row 
   `dt$area` values as lookup keys." But the literal string `"area"` is never a 
   valid key in the named PPP/CPI vectors (which have `"rural"`, `"urban"`, 
   `"national"`).

The current broken flow:
- `add_rep_lvl()` reads `ppp_data_level` attr → gets `"area"` → assigns 
  `dt[, reporting_level := "area"]`
- `add_ppp()` reads `ppp_data_level` attr → gets `"area"` → does 
  `lev_map["area"]` → returns `NA`
- `adjust_population()` groups by `reporting_level` column → gets one group 
  `"area"` → pop lookup fails

The fix:
- **Delete `add_rep_lvl()`** — it's the root cause
- **`add_ppp()`**: branch on `ppp_data_level` attr:
  - `== "area"`: per-row lookup using `dt$area` (contains `"rural"` / `"urban"`)
  - `== "national"` (or other literal): scalar broadcast
- **`add_cpi()`**: branch on `cpi_data_level` attr (same pattern)
- **`adjust_population()`**: use `area` column instead of `reporting_level` 
  column for grouping and joining
- **Guard in S3 core methods**: use `attr(dt_c, "pop_data_level") == "area"` 
  to decide whether to call `adjust_population()` — this correctly handles the 
  mixed-domain case where `reporting_level == 2` but `pop_data_level == "national"`

## Requirements

| ID  | Requirement | Source |
|-----|-------------|--------|
| R1  | When `ppp_data_level == "area"`, `add_ppp()` must use per-row `dt$area` values as lookup keys into the named PPP vector | brainstorm |
| R2  | When `cpi_data_level == "area"`, `add_cpi()` must use per-row `dt$area` values as lookup keys into the named CPI vector | brainstorm |
| R3  | When `*_data_level == "national"` (or other literal), behavior unchanged — scalar broadcast to all rows | brainstorm |
| R4  | Mixed cases work (e.g., `ppp_data_level = "area"`, `pop_data_level = "national"`) — each function checks its own attr independently | plan-review P1.1 |
| R5  | Both `pipmd` and `pipgd` S3 methods must deflate subnational data correctly | brainstorm |
| R6  | Existing tests for national-level deflation continue to pass | regression |
| R7  | New test fixtures for subnational case (CHN-like: urban/rural rows with area-keyed PPP/CPI) | test |
| R8  | `adjust_population()` uses `area` column (not `reporting_level` column) for subnational grouping | revised design |
| R9  | `add_rep_lvl()` is removed; no `reporting_level` column is created during deflation | revised design |
| R10 | `adjust_population()` guard checks `pop_data_level == "area"`, not integer `reporting_level` | plan-review P2.1 |
| R11 | Dead code `get_ordered_level()` in `R/utils.R` archived or removed | plan-review P3.1 |

## Implementation Steps

### 1. Remove `add_rep_lvl()` and update callers

- **Requirement**: R9
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  - Delete `add_rep_lvl()` function definition (lines ~505–527)
  - Remove `dt_c <- add_rep_lvl(dt_c)` from `.deflation_pipmd_core()` (line 380)
  - Remove `dt_c <- add_rep_lvl(dt_c)` from `.deflation_pipgd_core()` (line 403)
  - Update the guard for `adjust_population()` in `.deflation_pipmd_core()`:
    ```r
    # Before:
    if (length(dt_c[, unique(reporting_level)]) > 1L) {
      dt_c <- adjust_population(dt_c, pop)
    }
    # After: check pop_data_level attr (handles mixed-domain case correctly)
    if (identical(attr(dt_c, "pop_data_level"), "area")) {
      dt_c <- adjust_population(dt_c, pop)
    }
    ```
  - Remove `"reporting_level"` from `.validate_deflation_input()` required 
    attributes list (line 32) if present
  - Remove `"reporting_level"` from `finalize_deflation_output()` column 
    ordering (line 436) — replace with `"area"`
  - Update `setorder(dt, reporting_level)` (which was in `add_rep_lvl`) — 
    this ordering is no longer needed; if needed for output, sort by `area` 
    in `finalize_deflation_output()`
- **Tests**: Existing tests that check for `reporting_level` column must be 
  updated to check `area` instead
- **Acceptance criteria**: Package builds; `add_rep_lvl` no longer exists; no 
  references to `reporting_level` column creation remain

### 2. Modify `add_ppp()` to branch on `ppp_data_level` attr

- **Requirement**: R1, R3, R4
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  - In the named-vector path (loop over `unique_versions`), replace the scalar 
    broadcast `dt[, (v) := lev_map[ppp_lvl]]` with a conditional on the 
    function's **own** attr (`ppp_data_level`):
    ```r
    ppp_lvl <- attr(dt, "ppp_data_level")
    unique_versions <- unique(ppp_versions)
    for (v in unique_versions) {
      idx <- ppp_versions == v
      lev_map <- stats::setNames(ppp[idx], report_levels[idx])
      if (identical(ppp_lvl, "area")) {
        # Subnational: per-row lookup using area column
        dt[, (v) := lev_map[as.character(area)]]
      } else {
        # National (or other literal): scalar broadcast
        dt[, (v) := lev_map[ppp_lvl]]
      }
    }
    ```
  - Add assertion: if `ppp_lvl == "area"` and `!"area" %in% names(dt)`, 
    abort with informative error
  - Legacy `data.table` path (joyn merge by `ppp_data_level`): this path 
    merges by the `ppp_data_level` column, which doesn't exist on new-pipeline 
    data (attrs only). Flag with `# TODO: legacy path — candidate for removal` 
    comment. No functional change needed since it's unreachable on new data.
- **Tests**: Existing national-level tests pass; new subnational test in Step 5
- **Acceptance criteria**: Subnational fixture with `area = c("rural", "urban")` 
  and named PPP vector produces non-`NA` per-row values

### 3. Modify `add_cpi()` to branch on `cpi_data_level` attr

- **Requirement**: R2, R3, R4
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  - In the named-vector path (loop over `unique_years`), replace:
    ```r
    dt[, (col) := lev_map[cpi_lvl]]
    ```
    with:
    ```r
    cpi_lvl <- attr(dt, "cpi_data_level")
    for (yr in unique_years) {
      col <- paste0("cpi", yr)
      idx <- cpi_years == yr
      lev_map <- stats::setNames(cpi[idx], report_levels[idx])
      if (identical(cpi_lvl, "area")) {
        # Subnational: per-row lookup using area column
        dt[, (col) := lev_map[as.character(area)]]
      } else {
        # National (or other literal): scalar broadcast
        dt[, (col) := lev_map[cpi_lvl]]
      }
    }
    ```
  - Same assertion for `area` column existence
  - Legacy `data.table` path: flag with `# TODO: legacy path — candidate 
    for removal` (same as `add_ppp()`)
- **Tests**: Existing national-level tests pass; new subnational test in Step 5
- **Acceptance criteria**: CPI lookup produces non-`NA` per-row values for 
  subnational fixtures

### 4. Modify `adjust_population()` to use `area` instead of `reporting_level`

- **Requirement**: R8
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  - Replace all `by = "reporting_level"` with `by = "area"` in the 
    named-vector path of `adjust_population()`
  - In the legacy `data.table` path: rename `pop_data_level` → `area` 
    (instead of → `reporting_level`) for the join key, OR use `area` 
    directly if it already exists in `df`
  - Named pop vector keys are `"{year}_{level}"` (e.g., `"2015_rural"`) — 
    the `pop_levels` parsed from names already match `dt$area` values
  - Update the `spop` grouping:
    ```r
    # Before:
    spop <- df[, .(weight = sum(weight, na.rm = TRUE)), by = "reporting_level"]
    # After:
    spop <- df[, .(weight = sum(weight, na.rm = TRUE)), by = "area"]
    ```
  - Update the `fact_rows` lapply to iterate over `spop$area`
  - Update the final `joyn::left_join` to join `by = "area"`
- **Tests**: Update `tests/testthat/test-adjust-population.R` fixtures to use 
  `area` column instead of `reporting_level` column
- **Acceptance criteria**: Population adjustment works using `area` as the 
  grouping key; weights are correctly scaled per area

### 5. Add/update test fixtures for subnational case

- **Requirement**: R5, R7
- **Files**: `tests/testthat/test-pd-deflation.R` (update)
- **Details**:
  - Extend existing `make_pipmd()` helper to accept an `area` parameter:
    ```r
    make_pipmd <- function(
      welfare = c(5, 10, 15),
      weight = c(100, 200, 100),
      area = NULL,           # NEW: e.g., c("rural", "urban", "rural")
      ...
    ) {
      dt <- data.table::data.table(
        welfare = as.numeric(welfare),
        weight = as.numeric(weight)
      )
      if (!is.null(area)) dt[, area := area]
      ...
    }
    ```
  - Create subnational fixture helpers for named PPP/CPI/pop vectors:
    ```r
    make_ppp_vec_subnational <- function() {
      c(ppp_2017_01_01_rural = 3.0, ppp_2017_01_01_urban = 3.9,
        ppp_2017_01_01_national = 3.5)
    }
    make_cpi_vec_subnational <- function() {
      c("2017_rural" = 0.85, "2017_urban" = 0.88, "2017_national" = 0.87)
    }
    make_pop_vec_subnational <- function() {
      c("2015_rural" = 6e8, "2015_urban" = 7e8, "2015_national" = 1.3e9)
    }
    ```
  - Write tests:
    ```r
    test_that("add_ppp resolves subnational via area column", {
      dt <- make_pipmd(
        area = c("rural", "urban", "rural"),
        ppp_data_level = "area",
        cpi_data_level = "area",
        reporting_level = 2L
      )
      ppp <- make_ppp_vec_subnational()
      result <- pipdata:::add_ppp(dt, ppp)
      expect_true(all(!is.na(result$ppp_2017_01_01)))
      expect_equal(result$ppp_2017_01_01, c(3.0, 3.9, 3.0))
    })
    ```
  - Same pattern for `add_cpi()` and full S3 method integration tests
  - Update existing tests that referenced `reporting_level` column to use 
    `area` column
- **Tests**: Three+ new unit tests (add_ppp subnational, add_cpi subnational, 
  adjust_population subnational, full deflation integration), plus regression
- **Acceptance criteria**: All subnational tests pass; all national-level 
  regression tests still pass

### 6. Archive dead `get_ordered_level()` in `R/utils.R`

- **Requirement**: R11
- **Files**: `R/utils.R` (modify)
- **Details**:
  - `get_ordered_level()` (lines ~152–172) is defined in pipdata but never 
    called by any pipdata code. It is only used by the old 
    `pip_ingestion_pipeline` (which has its own copy).
  - Remove the function from `R/utils.R`
  - If it was exported, remove the NAMESPACE entry and delete any `.Rd` file
  - It is `@noRd` so no `.Rd` cleanup needed
- **Tests**: No tests reference this function in pipdata
- **Acceptance criteria**: Function removed; package builds cleanly

### 7. Run full test suite and verify no regressions

- **Requirement**: R6
- **Files**: `tests/testthat/` (run all)
- **Details**:
  - Execute `devtools::test()`
  - Verify no breakage to existing national-level deflation behavior
  - Verify subnational cases produce numeric (not `NA`) results
  - Verify `adjust_population()` correctly groups by `area`
  - Verify mixed-domain case: `ppp_data_level = "area"` + `pop_data_level = 
    "national"` → PPP uses per-row lookup, `adjust_population()` is skipped
- **Tests**: Full suite
- **Acceptance criteria**: All tests pass; no regressions

## Testing Strategy

| Layer | What | How |
|-------|------|-----|
| Unit | `add_ppp()` with `ppp_data_level = "area"`, `area` column | Direct call with subnational fixture; verify per-row PPP not `NA` |
| Unit | `add_cpi()` with `cpi_data_level = "area"`, `area` column | Direct call with subnational fixture; verify per-row CPI not `NA` |
| Unit | Mixed domain (ppp "area", pop "national") | Mixed fixture; verify `add_ppp()` does per-row lookup, `adjust_population()` is skipped |
| Unit | `adjust_population()` with `area` grouping | Subnational fixture with two areas; verify weight scaling per area |
| Regression | `add_ppp()` with `ppp_data_level = "national"` | Existing national tests must still pass |
| Regression | `add_cpi()` with `cpi_data_level = "national"` | Existing national tests must still pass |
| Regression | `adjust_population()` not called when `pop_data_level = "national"` | Verify guard prevents call |
| Integration | `deflation.pipmd()` with subnational fixture | Full S3 method; verify output has numeric welfare_ppp columns |
| Integration | `deflation.pipgd()` with subnational fixture | Full S3 method; verify output has numeric welfare_ppp columns |

## Documentation Checklist

- [ ] `add_rep_lvl()` roxygen/`@noRd` removed along with the function
- [ ] Inline comments in `add_ppp()` explaining `ppp_data_level == "area"` branching
- [ ] Inline comments in `add_cpi()` explaining `cpi_data_level == "area"` branching
- [ ] `adjust_population()` docstring updated: `@param df` now requires `area` column (not `reporting_level`)
- [ ] Legacy `data.table` paths in `add_ppp()` and `add_cpi()` flagged with `# TODO: legacy path — candidate for removal`
- [ ] `get_ordered_level()` removed from `R/utils.R`
- [ ] Test fixtures documented with comments explaining subnational structure

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Named PPP/CPI vectors missing keys for `"rural"` or `"urban"` during lookup | Assert non-`NA` after lookup; abort with informative error |
| Subnational data reaches deflation without `area` column | Assertion in `add_ppp()`/`add_cpi()`: abort if `*_data_level == "area"` and `"area"` not in `names(dt)` |
| `adjust_population()` legacy data.table path uses `pop_data_level` → `reporting_level` rename | Update to rename to `area` or join by matching strategy |
| National surveys have no `area` column | The `*_data_level == "national"` branch never touches `dt$area` — no issue |
| Mixed-domain: `reporting_level == 2` but `ppp_data_level == "national"` | Each function branches on its own `*_data_level` attr, not the integer `reporting_level`. `add_ppp()` would correctly scalar-broadcast `"national"` even for a subnational survey | 
| Downstream code outside deflation expects `reporting_level` column | Search codebase; `finalize_deflation_output()` is the only reference — update to `area` |

## Out of Scope

- Changing how `add_dom_vars()` stores level attributes (future architectural 
  cleanup tracked in roadmap as `explicit-data-level-semantics`)
- Full integration of subnational deflation into the pipeline orchestrator 
  (part of the main `integrate-deflation` plan)
- Eliminating the `"area"` string in `*_data_level` attributes — these attrs 
  are preserved as-is; each function checks its own attr directly
- Removing the legacy `data.table` path in `add_ppp()`/`add_cpi()`/ 
  `adjust_population()` — flagged with TODO but not removed in this plan

## Integration with `integrate-deflation` Plan

This plan is a **prerequisite** for the active `integrate-deflation` plan:

- Step 1 of `integrate-deflation` validates input and loads metadata — assumes 
  deflation functions work correctly for all survey types
- Step 3 (refactoring `safe_deflation()` helper) modifies the S3 methods that 
  call `add_ppp()`/`add_cpi()` — this fix ensures they work correctly post-refactor
- Step 4 (integration tests) must cover subnational cases — this plan's test 
  fixtures support that

**Sequencing**: Complete this plan before starting Step 2 of `integrate-deflation`.

