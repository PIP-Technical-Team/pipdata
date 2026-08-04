---
date: 2026-05-13
title: "Create load_pip_deflated_data() wrapper in pipload"
status: moved
moved-to: "pipload/.cg-docs/plans/2026-05-13-load-pip-deflated-data.md"
scope: "Lightweight"
brainstorm: null
language: R
estimated-effort: small
tags: [deflation, pipload, wrapper, convenience]
---

# Plan: Create load_pip_deflated_data() Wrapper

## Objective

Create a convenience function `load_pip_deflated_data()` in **pipload** that
chains `load_pip_data()` → `pipdata::pd_deflation()` in a single call. Users
pass the same filtering arguments as `load_pip_data()` (country_code,
surveyid_year, survey_acronym, welfare_type, module, id_name, etc.) and get
back the deflated survey data.table.

## Context

- `pd_deflation()` (pipdata) is complete and self-contained — it accepts a
  cleaned survey data.table and internally resolves CPI/PPP/pop from the
  master inventory.
- `load_pip_data()` (pipload) discovers and loads a single survey by
  country/year/acronym or by `id_name`.
- No function currently chains the two operations.
- **Dependency direction**: pipdata imports pipload, not the other way.
  Adding `pipdata` as a hard `Imports` to pipload would create a circular
  dependency. Solution: add `pipdata` to `Suggests` and guard with
  `rlang::check_installed("pipdata")`.

## Requirements

| ID  | Requirement                                                   | Source |
|-----|---------------------------------------------------------------|--------|
| R1  | Function lives in pipload at `R/load_pip_deflated_data.R`     | user   |
| R2  | Mirrors `load_pip_data()` filtering args (country_code, surveyid_year, survey_acronym, welfare_type, module, id_name, vermast, veralt, collection, latest_version, latest_year, where, version, format) | user |
| R3  | Calls `load_pip_data()` then `pipdata::pd_deflation(dt = ...)`| user   |
| R4  | `pipdata` is a soft dependency (Suggests), guarded at runtime | architecture |
| R5  | Returns the deflated data.table (same output contract as `pd_deflation()`) | architecture |

## Implementation Steps

### 1. Add pipdata to Suggests in pipload DESCRIPTION

- **Requirements**: R4
- **Files**: `pipload/DESCRIPTION`
- **Details**: Add `pipdata` to the `Suggests:` field.
- **Acceptance criteria**: `pipdata` appears in Suggests; no circular Imports.

### 2. Create `R/load_pip_deflated_data.R`

- **Requirements**: R1, R2, R3, R4, R5
- **Files**: `pipload/R/load_pip_deflated_data.R`
- **Details**:
  - Export `load_pip_deflated_data()`.
  - Forward all `load_pip_data()` args (except `metadata` which is
    irrelevant for deflation — always FALSE).
  - Guard with `rlang::check_installed("pipdata", reason = "...")`.
  - Call `load_pip_data(...)` to get the survey.
  - Pass result to `pipdata::pd_deflation(dt = survey)`.
  - Return the deflated data.table.
  - roxygen2 documentation with `@family load_pip_data`, `@export`,
    `@examples` (in `\dontrun{}`).
- **Test Scenarios**:
  - ✅ Happy path: id_name → loads and deflates
  - ✅ Happy path: country_code + year → discovers, loads, deflates
  - 🛑 Edge case: pipdata not installed → informative error
  - ❌ Error path: invalid country_code → propagates load_pip_data error
  - ❌ Error path: deflation fails (missing inventory entry) → propagates pd_deflation error
- **Acceptance criteria**: Function loads survey and deflates it in one call.

### 3. Write tests

- **Requirements**: R3, R4
- **Files**: `pipload/tests/testthat/test-load_pip_deflated_data.R`
- **Details**:
  - Test that `rlang::check_installed()` fires when pipdata missing
    (use `local_mocked_bindings()` or `testthat::skip_if_not_installed()`).
  - Integration test (may need `\dontrun{}` / `skip_on_ci()` since it
    requires a working release + stamp setup).
- **Acceptance criteria**: At least the guard-check test passes in CI.

## Testing Strategy

- Unit test the soft-dependency guard.
- Integration test (skippable) that verifies load → deflate round-trip
  against a known survey when a release is set up.

## Documentation Checklist

- [x] Function documentation (roxygen2 with `@inheritParams load_pip_data`)
- [ ] README mention (optional — low priority)
- [ ] `@examples` in `\dontrun{}`

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Circular dependency if someone adds `pipdata` to `Imports` later | Document in roxygen that it is Suggests-only; guard enforces it |
| `pd_deflation()` requires survey attributes set by pipdata's cleaning pipeline; raw `load_pip_data()` output may lack them | `load_pip_data()` reads from pip_data (already cleaned surveys with correct attributes). If not, pd_deflation will abort with a clear attribute-missing error. |

## Out of Scope

- Batch iteration over multiple surveys (`pd_deflate_pipeline()` — separate roadmap item in pipdata).
- Saving deflated output to stamp (future orchestration concern).
- Adding `pipdata` as a hard Imports in pipload.
