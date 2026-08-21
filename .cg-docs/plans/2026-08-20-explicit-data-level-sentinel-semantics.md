---
date: 2026-08-20
title: "Explicit data_level sentinel semantics (column-lookup registry)"
status: completed
completed-date: 2026-08-20
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-08-20-data-level-sentinel-semantics.md"
language: "R"
estimated-effort: "small"
deviation-policy: "ask"
artifact-schema-version: 1
phases: 2
execution-report: ".cg-docs/work-reports/2026-08-20-explicit-data-level-sentinel-semantics.md"
completed-phases: [1, 2]
tags: [deflation, data-level, attribute-semantics, subnational, ppp, cpi, population, refactor]
---

# Plan: Explicit `*_data_level` sentinel semantics (column-lookup registry)

## Objective

Replace the implicit `"area"` column-pointer sentinel with an explicit,
code-enforced column-lookup registry so the pointer-vs-literal distinction
is declared in one place and applied through one resolver, centralizing
the 3 `identical(*, "area")` dispatch branches in `R/pd_deflation.R`.
`adjust_population()` remains a separate hardcoded consumer of the
`"area"` column name (see Out of Scope); the registry centralizes the
*dispatch* but does not yet govern `adjust_population()`'s internal
column references.

## Context

The `ppp_data_level`, `cpi_data_level`, and `pop_data_level` attributes
carry dual semantics in a single string: `"national"` (or any non-`"area"`
string) is a literal level value broadcast across all rows; `"area"` is an
implicit pointer to the `area` *column* of `dt`, meaning "look up the
per-row level value (e.g. `rural`, `urban`) in the `area` column." The
convention is implicit and error-prone (the original `add_rep_lvl()` bug,
removed 2026-05-07, was caused by treating `"area"` as a literal).

Brainstorm `.cg-docs/brainstorms/2026-08-20-data-level-sentinel-semantics.md`
selected **Approach 4 (column-lookup registry)**: a package-level registry
mapping sentinel -> column name plus a `data_level_column()` resolver.
This minimizes blast radius (only the 3 consumer sites change) while
converting the convention from implicit-magic-string to explicit-and-enforced.

The registry lives in `R/aaa.R`, which already holds package constants
(`.log_internal_types`, `globalVariables`, `.pipdataenv`) and is loaded first.
No new `R/constants.R` is needed.

Producer (`add_dom_vars()` in `R/pd_cpfw_merge.R`) and auxiliary-vector builder
(`pd_aux_attr()`) are intentionally untouched — the attr value stays the
string `"area"`, so existing test fixtures and the `pipdata.R:40-61`
attribute whitelist pass unchanged.

## Requirements

| ID  | Requirement | Source |
|-----|-------------|--------|
| R1  | Add a `.data_level_columns` registry (sentinel -> column name) in `R/aaa.R` | Brainstorm Next Step 1 |
| R2  | Add a `data_level_column(lvl)` resolver that returns the column name for a registered sentinel or `NA_character_` for a literal level value; guard against `NULL`/`character(0)`/`NA` inputs so missing or empty `*_data_level` attrs safely fall through to scalar broadcast | Brainstorm Next Step 1 + Plan Review P1 |
| R3  | Rewrite the `adj_pop` guard at `R/pd_deflation.R:395` to use `data_level_column()` | Brainstorm Next Step 2 |
| R4  | Rewrite the pointer branch in `add_ppp()` at `R/pd_deflation.R:640` to use `data_level_column()`; keep the "area column absent" abort keyed off the resolved column name | Brainstorm Next Step 2 |
| R5  | Rewrite the pointer branch in `add_cpi()` at `R/pd_deflation.R:716` to use `data_level_column()`; keep the "area column absent" abort keyed off the resolved column name | Brainstorm Next Step 2 |
| R6  | Registry and resolver are internal-only (no `@export`); `NAMESPACE` unchanged | Contract C2 |
| R7  | `*_data_level` attr value remains the string `"area"` (no type change) | Contract C1 |
| R8  | Add a contract test covering `data_level_column("area") == "area"`, `is.na(data_level_column("national"))`, and degenerate inputs (`NULL`, `character(0)`, `NA_character_`) | Brainstorm Next Step 5 + Plan Review P1 |
| R9  | `add_dom_vars()`, `pd_aux_attr()`, and `adjust_population()` body remain unchanged | Contract / V5 |
| R10 | All existing tests pass with zero regressions | Contract V2 |

## Implementation Steps

## Phase 1: Core implementation

### 1. Add column-lookup registry and resolver to `R/aaa.R`

- **Requirements**: R1, R2, R6
- **Files**: `R/aaa.R`
- **Details**: Add after `.log_internal_types` (around line 89) a registry
  list and a resolver function:

  ```r
  # Column-pointer sentinel registry for *_data_level attributes.
  # Keys are sentinel strings stored in the attribute; values are the
  # column names they point to. Anything not in this registry is treated
  # as a literal level value (e.g. "national") and broadcast as a scalar.
  .data_level_columns <- list(area = "area")

  #' Resolve a data_level attribute to a column name
  #'
  #' Returns the column name when `lvl` is a registered column-pointer
  #' sentinel, or `NA_character_` when `lvl` is a literal level value
  #' (e.g. `"national"`) that should be broadcast as a scalar. Also
  #' returns `NA_character_` for degenerate inputs (`NULL`,
  #' `character(0)`, `NA_character_`) so that a missing or empty
  #' `*_data_level` attribute safely falls through to the scalar-broadcast
  #' branch rather than crashing.
  #'
  #' @param lvl Character scalar (or `NULL`/`character(0)`/`NA`).
  #'   Value of a `*_data_level` attribute.
  #' @return Character scalar column name, or `NA_character_`.
  #' @noRd
  data_level_column <- function(lvl) {
    if (is.null(lvl) || length(lvl) != 1L || is.na(lvl)) {
      return(NA_character_)
    }
    col <- .data_level_columns[[lvl]]
    if (is.null(col)) NA_character_ else col
  }
  ```

  The degenerate-input guard is required because `attr(dt, "*_data_level")`
  returns `NULL` when the attribute is absent, and `add_dom_vars()`
  (`R/pd_cpfw_merge.R:276,320`) emits `setattr(dt, dta_var,
  as.character())` which is `character(0)` (not a scalar) for reporting
  levels outside {1, 2}. The original `identical(attr(...), "area")`
  safely returned `FALSE` for both `NULL` and `character(0)`; the
  resolver must preserve that safe fall-through so the scalar-broadcast
  branch is reached instead of crashing with "attempt to select less
  than one element in get1index".

  No `@export`; `NAMESPACE` stays unchanged. `aaa.R` is loaded first by
  convention so the registry is available to `pd_deflation.R`.
- **Test Scenarios**: happy path (`"area"` -> `"area"`), literal (`"national"` -> `NA`), unregistered sentinel (`"region"` -> `NA`), missing attr (`NULL` -> `NA`), empty (`character(0)` -> `NA`), `NA_character_` -> `NA`
- **Tests**: standalone contract test in `tests/testthat/test-pd-deflation.R` (Step 5)
- **Acceptance criteria**: `data_level_column("area") == "area"`, `is.na(data_level_column("national"))`, `is.na(data_level_column(NULL))`, `is.na(data_level_column(character(0)))`, and `is.na(data_level_column(NA_character_))` all hold; package loads without error.

### 2. Rewrite the `adj_pop` guard in `.deflation_pipmd_core`

- **Requirements**: R3, R7
- **Files**: `R/pd_deflation.R` (around line 395)
- **Details**: Replace

  ```r
  adj_pop <- identical(attr(dt_c, "pop_data_level"), "area")
  ```

  with

  ```r
  adj_pop <- !is.na(data_level_column(attr(dt_c, "pop_data_level")))
  ```

  Semantics are preserved: `adj_pop` is `TRUE` iff `pop_data_level` is a
  registered column-pointer sentinel (currently only `"area"`). The
  `adjust_population()` body (called when `adj_pop` is `TRUE`) is
  unchanged — it already uses the `area` column directly and is
  pre-guarded by this check.
- **Test Scenarios**: subnational survey (`pop_data_level = "area"`) -> `adj_pop = TRUE`; national survey (`pop_data_level = "national"`) -> `adj_pop = FALSE`
- **Tests**: `test-pd-deflation.R:685` (`adj_pop = TRUE`), `test-pd-deflation.R:709` (`adj_pop = FALSE`)
- **Acceptance criteria**: both existing `adj_pop` tests pass unchanged.

### 3. Rewrite the pointer branch in `add_ppp()`

- **Requirements**: R4, R7, R9
- **Files**: `R/pd_deflation.R` (around lines 633-652)
- **No changes to `pd_aux_attr()`** (R9) — the attr value stays the string `"area"`.
- **Details**: Replace

  ```r
  ppp_lvl <- attr(dt, "ppp_data_level")
  ...
    if (identical(ppp_lvl, "area")) {
      if (!"area" %in% names(dt)) {
        cli::cli_abort(
          "ppp_data_level is \"area\" but {.arg dt} has no {.field area} column.",
          class = c("add_ppp", "piperr")
        )
      }
      dt[, (v) := lev_map[as.character(area)]]
    } else {
      dt[, (v) := lev_map[ppp_lvl]]
    }
  ```

  with

  ```r
  ppp_lvl <- attr(dt, "ppp_data_level")
  ppp_col <- data_level_column(ppp_lvl)
  ...
    if (is.na(ppp_col)) {
      dt[, (v) := lev_map[ppp_lvl]]
    } else {
      if (!ppp_col %in% names(dt)) {
        cli::cli_abort(
          "ppp_data_level is {.val {ppp_lvl}} but {.arg dt} has no {.field {ppp_col}} column.",
          class = c("add_ppp", "piperr")
        )
      }
      dt[, (v) := lev_map[as.character(dt[[ppp_col]])]]
    }
  ```

  Note the abort message now uses the resolved column name (`{ppp_col}`)
  instead of the hard-coded `"area"`, so the error stays correct if a
  second sentinel is ever registered. The `ppp_col` lookup is hoisted
  outside the `for (v in unique_versions)` loop since it does not depend
  on `v`.
- **Test Scenarios**: subnational (`ppp_data_level = "area"`, area column present) -> per-row rural/urban lookup; national (`ppp_data_level = "national"`) -> scalar broadcast; area sentinel but no area column -> abort with class `"add_ppp"`
- **Tests**: `test-pd-deflation.R:361` (subnational resolve), `test-pd-deflation.R:380` (abort), `test-pd-deflation.R:350` (national scalar)
- **Acceptance criteria**: all three `add_ppp` tests pass unchanged; the abort test still expects `class = "add_ppp"`.

### 4. Rewrite the pointer branch in `add_cpi()`

- **Requirements**: R5, R7
- **Files**: `R/pd_deflation.R` (around lines 706-728)
- **Details**: Mirror Step 3 for `add_cpi()`. Replace

  ```r
  cpi_lvl <- attr(dt, "cpi_data_level")
  ...
    if (identical(cpi_lvl, "area")) {
      if (!"area" %in% names(dt)) {
        cli::cli_abort(
          "cpi_data_level is \"area\" but {.arg dt} has no {.field area} column.",
          class = c("add_cpi", "piperr")
        )
      }
      dt[, (col) := lev_map[as.character(area)]]
    } else {
      dt[, (col) := lev_map[cpi_lvl]]
    }
  ```

  with

  ```r
  cpi_lvl <- attr(dt, "cpi_data_level")
  cpi_col <- data_level_column(cpi_lvl)
  ...
    if (is.na(cpi_col)) {
      dt[, (col) := lev_map[cpi_lvl]]
    } else {
      if (!cpi_col %in% names(dt)) {
        cli::cli_abort(
          "cpi_data_level is {.val {cpi_lvl}} but {.arg dt} has no {.field {cpi_col}} column.",
          class = c("add_cpi", "piperr")
        )
      }
      dt[, (col) := lev_map[as.character(dt[[cpi_col]])]]
    }
  ```

  Hoist `cpi_col` outside the `for (yr in unique_years)` loop. The
  `cpi_years` attribute is still set before the loop (unchanged).
- **Test Scenarios**: subnational (`cpi_data_level = "area"`, area column present) -> per-row lookup; national -> scalar broadcast; area sentinel but no area column -> abort with class `"add_cpi"`
- **Tests**: `test-pd-deflation.R:412` (subnational resolve), `test-pd-deflation.R:430` (abort), `test-pd-deflation.R:391` (national scalar)
- **Acceptance criteria**: all three `add_cpi` tests pass unchanged; the abort test still expects `class = "add_cpi"`.

## Phase 2: Tests and verification

### 5. Add resolver contract test

- **Requirements**: R8
- **Files**: `tests/testthat/test-pd-deflation.R`
- **Details**: Add a new `test_that` block near the top of the file
  (after the `make_pop_vec` helper, before the `add_ppp` section):

  ```r
  test_that("data_level_column() resolves the area sentinel and passes literals through", {
    expect_equal(pipdata:::data_level_column("area"), "area")
    expect_true(is.na(pipdata:::data_level_column("national")))
    expect_true(is.na(pipdata:::data_level_column("rural")))
    # Degenerate inputs from missing/empty *_data_level attrs — must not crash
    expect_true(is.na(pipdata:::data_level_column(NULL)))
    expect_true(is.na(pipdata:::data_level_column(character(0))))
    expect_true(is.na(pipdata:::data_level_column(NA_character_)))
  })
  ```

  This locks the registry contract: only `"area"` is a pointer; everything
  else (including `NULL`, `character(0)`, `NA`) is a literal/empty value
  that safely returns `NA_character_`. The degenerate-input assertions
  guard against the `[[` crash identified in the plan review (P1). Uses
  `:::` because the resolver is internal (`@noRd`, not exported).
- **Test Scenarios**: registered sentinel -> column name; literal -> `NA`; unregistered string -> `NA`; missing attr (`NULL`) -> `NA`; empty (`character(0)`) -> `NA`; `NA_character_` -> `NA`
- **Tests**: this block
- **Acceptance criteria**: the new test passes; `data_level_column` is found via `:::` (no load-order issue).

### 6. Full test suite verification

- **Requirements**: R10
- **Files**: (none — verification only)
- **Details**: Run `devtools::test_file("tests/testthat/test-pd-deflation.R")`
  and `devtools::test_file("tests/testthat/test-adjust-population.R")`
  first to confirm the deflation-family tests pass, then `devtools::test()`
  for the full suite. (testthat's `filter=` uses substring matching, not
  regex, so per-file `test_file()` is more reliable for this targeted run.)
  Confirm:
  - Zero failures, zero warnings in the deflation family.
  - `git diff R/pd_cpfw_merge.R R/pd_aux_attr.R` shows no changes to
    `add_dom_vars()` or `pd_aux_attr()` (V5).
  - `git diff R/pd_deflation.R` is limited to the 3 consumer-site regions
    (`:395`, `:640`, `:716`) plus the `aaa.R`-adjacent addition (none —
    `aaa.R` is same package, no import needed; `data_level_column()` is
    resolved lazily at call time from the package namespace).
- **Test Scenarios**: full suite green; no new warnings
- **Tests**: `devtools::test()`
- **Acceptance criteria**: `devtools::test()` exits 0 with zero failures; `git diff` confirms the change surface matches the boundaries.

## Testing Strategy

- **Unit**: Step 5 adds a contract test for `data_level_column()`.
- **Regression**: Steps 2-4 rely on the existing
  `test-pd-deflation.R` tests (`:350`, `:361`, `:380`, `:391`, `:412`,
  `:430`, `:685`, `:709`) and `test-adjust-population.R` to detect
  semantic drift. These tests are intentionally not modified — they are
  the regression guard. If any fails after a consumer rewrite, the
  resolver changed lookup semantics and must be reverted and re-examined.
- **Integration**: the `adj_pop = TRUE/FALSE` tests at `:685`/`:709`
  exercise the full `.deflation_pipmd_core` path including the guard.
- **No new test fixtures**: `make_pipmd()` and the `make_*_vec` helpers
  are unchanged because the attr value remains the string `"area"`.

## Documentation Checklist

- [x] Roxygen `@noRd` block on `data_level_column()` in `R/aaa.R` (Step 1)
- [x] Update the inline comment at each rewritten consumer site to reference `data_level_column()` instead of `identical(*, "area")` (Steps 2-4)
- [x] No vignette changes needed (deflation vignettes describe the pipeline, not the sentinel convention)
- [x] No `NEWS.md` bullet needed (internal refactor, no API change) — confirmed during `/cg-work`

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Resolver changes lookup semantics (e.g. returns a column for a literal) | Low | High | Contract test (Step 5) locks the mapping; existing subnational/national tests (Steps 2-4) catch drift immediately |
| Degenerate `*_data_level` attr (`NULL`, `character(0)`, `NA`) crashes the resolver via `[[` | Medium | High | Resolver guards `is.null(lvl) \|\| length(lvl) != 1L \|\| is.na(lvl)` before `[[` (Step 1); contract test (Step 5) covers `NULL`, `character(0)`, `NA_character_` |
| `adjust_population()` still hardcodes `"area"` (5 sites) — registry only governs the 3 dispatch branches | Medium | Medium | Documented in Objective/Outcome; a future non-`area` sentinel requires updating `adjust_population()` separately. Full `adjust_population()` routing is out of scope (R9). |
| Abort message change breaks a test that matches the old message text | Low | Medium | The abort tests (`:380`, `:430`) match on `class = "add_ppp"`/`"add_cpi"`, not on message text; Step 6 confirms |
| A future sentinel (e.g. `"region"`) is added to the attr without registering it | Medium | Medium | Unregistered sentinels silently fall through to the scalar-broadcast branch; documented in the `@noRd` block; adding a registry entry is necessary but not sufficient — `adjust_population()` also needs updating |
| `dt[[ppp_col]]` vs `area` NSE subtle difference | Low | Medium | `dt[[col]]` is base R standard extraction; `as.character()` wrapping is preserved; existing subnational tests (`:361`, `:412`) verify the values |

## Out of Scope

- Approach 1 (structural `list(column = "area")` encoding) — medium test/helper churn, deferred by the brainstorm.
- Approach 2 (always store resolved per-row values) — breaks the scalar fast path, rejected by the brainstorm.
- Approach 3 (bare package constant `.DATA_LEVEL_AREA`) — acceptable fallback but does not enforce the pointer convention in code.
- `add_dom_vars()` producer changes (`R/pd_cpfw_merge.R`).
- `pd_aux_attr()` changes (`R/pd_aux_attr.R`).
- `adjust_population()` body changes (`R/pd_deflation.R:902-1019`).
- Legacy `data.table` merge paths in `add_ppp()` (`:602-614`) and `add_cpi()` (`:675-697`) — these branch on `is.data.table(ppp/cpi)` and never inspect `*_data_level`.
- Test fixture helper changes (`make_pipmd`, `make_*_vec`).
- `NAMESPACE` / `@export` changes.

## Completion Contract

### Outcome

The `"area"` column-pointer sentinel is replaced by a code-enforced
column-lookup registry in `R/aaa.R`; the 3 `identical(*, "area")` dispatch
branches in `R/pd_deflation.R` (`:395`, `:640`, `:716`) call a single
`data_level_column()` resolver; all existing tests pass unchanged because
the `*_data_level` attribute value remains the string `"area"`.
`adjust_population()` still hardcodes the `"area"` column name internally
(out of scope for this plan); a future sentinel mapped to a different
column would require updating `adjust_population()` separately.

### Verification Surface

| ID  | Evidence Required | Command/Artifact | Required |
|-----|-------------------|------------------|----------|
| V1  | `data_level_column("area") == "area"`; `is.na(data_level_column("national"))`; `is.na(data_level_column(NULL))`; `is.na(data_level_column(character(0)))`; `is.na(data_level_column(NA_character_))` | `test-pd-deflation.R` contract test (Step 5) | yes |
| V2  | Full test suite passes with zero regressions | `devtools::test()` exit 0 | yes |
| V3  | Subnational `add_ppp`/`add_cpi` per-row lookup still produces correct values (rural/urban) | existing `test-pd-deflation.R:361,412` tests pass | yes |
| V4  | `adj_pop` guard still fires for subnational, not national | existing `test-pd-deflation.R:685,709` tests pass | yes |
| V5  | `add_dom_vars()`, `pd_aux_attr()`, `adjust_population()` body untouched | `git diff R/pd_cpfw_merge.R R/pd_aux_attr.R` shows no changes to those functions | yes |

### Constraints

| ID  | Constraint | Check |
|-----|------------|-------|
| C1  | `*_data_level` attr value stays the string `"area"` (no type change to list/NA) | existing test fixtures pass without modification |
| C2  | Registry and resolver are internal-only (no `@export`) | `NAMESPACE` unchanged |
| C3  | "area column absent" aborts still fire, keyed off the resolved column name | `test-pd-deflation.R:380,430` abort tests pass |
| C4  | Only 3 consumer sites change; no new branches added | `git diff R/pd_deflation.R` limited to `:395`, `:640`, `:716` regions |

### Boundaries

- **Allowed**: `R/aaa.R` (add registry + resolver), `R/pd_deflation.R` (3 consumer sites), `tests/testthat/test-pd-deflation.R` (add 1 contract test block)
- **Out of scope**: `R/pd_cpfw_merge.R` (`add_dom_vars`), `R/pd_aux_attr.R`, `adjust_population()` body, test fixture helpers (`make_pipmd`), Approach 1 structural `list(column=...)` encoding, legacy `data.table` merge paths in `add_ppp`/`add_cpi`

### Iteration Policy

1. Add registry + resolver to `R/aaa.R`; verify with the standalone contract test (Step 5) before touching any consumer.
2. Rewrite one consumer at a time — `adj_pop` guard (`:395`) -> `add_ppp` (`:640`) -> `add_cpi` (`:716`) — running the relevant existing test after each to catch semantic drift early.
3. Full `devtools::test()` at the end; if any pre-existing test fails, revert that consumer and re-examine the resolver.

### Blocked-Stop Conditions

- Any pre-existing test fails after a consumer rewrite (resolver changed lookup semantics).
- Contract test fails (registry logic wrong).
- Any degenerate-input assertion (`NULL`, `character(0)`, `NA_character_`) fails — resolver is unsafe for missing attrs.
- `devtools::test()` reports any new warning or failure traceable to this change.
