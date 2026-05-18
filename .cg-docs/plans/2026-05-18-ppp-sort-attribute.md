---
date: 2026-05-18
title: "Add ppp_sort attribute to deflation output"
status: completed
completed-date: 2026-05-18
scope: "Lightweight"
brainstorm: null
language: R
estimated-effort: small
tags: [deflation, attributes, output-contract, ppp]
---

# Plan: Add `ppp_sort` Attribute to Deflation Output

## Objective

Record which PPP base year was used to sort the deflated survey as an
attribute (`ppp_sort`) on the output `data.table`. Downstream consumers
can inspect `attr(result, "ppp_sort")` to know the sorting key year
(e.g., `2017`) without parsing column names.

## Context

`finalize_deflation_output()` already sorts rows by the newest
`welfare_ppp_*` column (determined via `sort_by_year_desc()`). The year
is implicitly available but not surfaced to callers. The output contract
currently guarantees two attributes: `welfare_vars` and `adj_pop`. This
adds a third: `ppp_sort`.

## Requirements

| ID  | Requirement                                                      | Source |
|-----|------------------------------------------------------------------|--------|
| R1  | Deflated output must have attr `ppp_sort` = integer PPP base year used for row sorting | user |
| R2  | When no `welfare_ppp_*` columns exist (edge case), `ppp_sort` must be `NULL` | design |

## Implementation Steps

### 1. Set `ppp_sort` attribute in `finalize_deflation_output()`

- **Requirements**: R1, R2
- **Files**: `R/pd_deflation.R`
- **Details**: After the `setorderv()` call, extract the year from
  `wlf_ppp[[1L]]` (already computed) and set
  `data.table::setattr(dt, "ppp_sort", year)`. When `wlf_ppp` is
  empty (no deflated columns), set `NULL`.
- **Test Scenarios**:
  - ✅ Happy path: deflated pipmd/pipgd → `attr(result, "ppp_sort")` is
    an integer matching the newest base year (e.g., `2017L`)
  - 🛑 Edge case: no `welfare_ppp_*` columns → `attr(result, "ppp_sort")` is `NULL`
- **Acceptance criteria**: `attr(pd_deflation(x), "ppp_sort")` returns
  the expected integer year.

### 2. Update output-contract documentation

- **Requirements**: R1
- **Files**: `R/pd_deflation.R` (roxygen for `finalize_deflation_output`
  and `.deflation_pipmd_core` / `.deflation_pipgd_core`)
- **Details**: Add `ppp_sort` to the documented attributes in the
  `@return` tags. Update the `@note` in `pd_deflation()` roxygen if
  relevant.
- **Acceptance criteria**: `devtools::document()` succeeds; `?pd_deflation`
  mentions `ppp_sort`.

### 3. Add test

- **Requirements**: R1, R2
- **Files**: `tests/testthat/test-pd-deflation.R`
- **Details**: Add an expectation to existing deflation tests that checks
  `attr(result, "ppp_sort")` is an integer equal to the expected base
  year from the test fixture.
- **Acceptance criteria**: `devtools::test(filter = "pd-deflation")` passes.

## Testing Strategy

Extend an existing test that exercises `finalize_deflation_output()` or
the full `deflation.pipmd()`/`deflation.pipgd()` path. Add one assertion
for the attribute value.

## Documentation Checklist

- [x] Function documentation (roxygen `@return` update)
- [ ] Inline comment explaining the attribute

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Column naming pattern changes in future → year extraction breaks | `sort_by_year_desc` already encapsulates extraction; reuse the same regex |

## Out of Scope

- Changing the sort order logic itself.
- Propagating `ppp_sort` to the master inventory or stamp metadata.
- Adding other sort-related attributes (e.g., `ppp_sort_col`).
