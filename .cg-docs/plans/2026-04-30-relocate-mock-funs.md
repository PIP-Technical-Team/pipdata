---
date: 2026-04-30
title: "Relocate mock_funs.R to tests/"
status: completed
completed-date: 2026-04-30
scope: "Lightweight"
estimated-effort: small
tags: [testing, cleanup, architecture, inline]
---

# Plan: Relocate mock_funs.R to tests/testthat/

## Objective

Move `R/mock_funs.R` to `tests/testthat/helper-mock_funs.R` so test
helpers live alongside tests, not in the package namespace. Update
`@examples` in affected R files that reference `pipdata:::m_svy_id_to_att()`.

## Files to Move

- `R/mock_funs.R` → `tests/testthat/helper-mock_funs.R`

## @examples to Update (replace `pipdata:::m_svy_id_to_att()` reference)

- `R/pd_process_data.R` (lines 126, 130)
- `R/pd_split_alt_welfare.R` (line 19)
- `R/pd_dlw_clean.R` (lines 15, 21)
- `R/pd_cpfw_merge.R` (lines 15, 19)

## Steps

1. Move `R/mock_funs.R` to `tests/testthat/helper-mock_funs.R`
2. Update affected `@examples` to add comment pointing to helper location
3. Run `devtools::document()`
4. Run `devtools::test()` — no failures from missing functions

## Acceptance Criteria

- `mock_funs.R` absent from `R/`
- `helper-mock_funs.R` present in `tests/testthat/`
- No broken examples in R `CMD check`
- Tests pass
