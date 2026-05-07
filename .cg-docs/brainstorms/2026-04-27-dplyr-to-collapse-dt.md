---
date: 2026-04-27
title: "Migrate dplyr calls to collapse/data.table (Phase 1: 3 files)"
status: decided
scope: "Standard"
chosen-approach: "File-by-file sequential migration"
tags: [refactoring, performance, dependencies, dplyr, collapse, data.table]
---

# Migrate dplyr to collapse/data.table (Phase 1)

## Context

dplyr, tidyr, and tibble are used via `::` in 4 pipdata files but are
**not declared in DESCRIPTION Imports** — phantom dependencies. The project
style guide mandates collapse for statistics and data.table for manipulation.
Some files (e.g., `pd_wbpip_clean.R`, `update_pip_inventory.R`) already use
collapse/data.table correctly.

This brainstorm covers Phase 1: the 3 simpler files. Phase 2
(`dlw_scan_and_validate.R`, ~20 call sites with loops and joins) is tracked
as a separate roadmap idea.

## Requirements

- Pure mechanical translation — same logic, different syntax.
- `dplyr::case_when` → `data.table::fcase()` with `default = NA_real_`
  (or `NA_character_` as appropriate).
- `dplyr::bind_rows` → `data.table::rbindlist()`.
- `dplyr::count` → `data.table` `[, .N, by = ...]`.
- `tibble::tibble` → `data.table::data.table()`.
- `tidyr::as_tibble` → remove (data already data.table from pipeline).
- Leave commented-out dplyr lines untouched.
- Add regression tests for migrated functions covering NA handling and
  edge cases.

## Files in Scope

| File | Active dplyr calls | Key replacements |
|------|--------------------|------------------|
| `pipdata_validation_report.R` | 1 | `dplyr::count` → `[, .N, by]` |
| `pd_dlw_clean.R` | 6 | `dplyr::case_when` → `fcase()` |
| `pipdata_validate_gmd.R` | 3 | `bind_rows`, `as_tibble` → `rbindlist` |

## Approaches Considered

### Approach A: File-by-file sequential migration (CHOSEN)

Migrate one file at a time, simplest first. Write regression tests for
each file before migrating, then verify after.

- **Pros**: Low risk, isolated, easy to bisect.
- **Cons**: More round-trips.
- **Effort**: Medium (3–4 days)

### Approach B: All files at once, tests after

Single pass migration, consolidated test suite.

- **Pros**: Faster.
- **Cons**: Harder to isolate regressions.
- **Effort**: Medium (2–3 days)

### Approach C: TDD-style (tests first, then migrate)

Full regression tests upfront, then migrate.

- **Pros**: Strongest safety net.
- **Cons**: Overkill for mechanical translation; some functions need heavy mocking.
- **Effort**: Large (4–5 days)

## Decision

**Approach A** — file-by-file sequential migration with regression tests.
Order: `pipdata_validation_report.R` → `pd_dlw_clean.R` →
`pipdata_validate_gmd.R`.

`dlw_scan_and_validate.R` deferred to Phase 2 (separate roadmap idea)
due to higher complexity (~20 call sites with loop-based join/filter logic).

## Next Steps

1. Write regression tests for `recode_edu()`, `recode_gndr()`, `recode_age()`
   covering NA, boundary, and type-coercion edge cases.
2. Migrate `pipdata_validation_report.R` (1 call).
3. Migrate `pd_dlw_clean.R` (6 `case_when` → `fcase` swaps).
4. Migrate `pipdata_validate_gmd.R` (3 calls: `bind_rows`, `as_tibble`).
5. Run `devtools::check()` to confirm no regressions.
