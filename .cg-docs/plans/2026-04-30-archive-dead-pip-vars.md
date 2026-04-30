---
date: 2026-04-30
title: "Archive pd_add_pip_vars.R (dead code)"
status: completed
completed-date: 2026-04-30
scope: "Lightweight"
estimated-effort: small
tags: [cleanup, dead-code, architecture, inline]
---

# Plan: Archive pd_add_pip_vars.R

## Objective

Move `pd_add_pip_vars.R` (defines `pd_add_pip_vars()`, `add_pip_vars()`,
`adjust_population()`) to `old_files/`. None are called by the active
pipeline call tree. Remove exports and .Rd files.

## Files to Archive

- `R/pd_add_pip_vars.R` → `old_files/`

## .Rd Files to Delete

- `man/pd_add_pip_vars.Rd`
- `man/add_pip_vars.Rd`
- `man/adjust_population.Rd`

## Steps

1. Move `R/pd_add_pip_vars.R` to `old_files/`
2. Delete `.Rd` files
3. Run `devtools::document()` — NAMESPACE regenerated

## Acceptance Criteria

- `pd_add_pip_vars`, `add_pip_vars` no longer in NAMESPACE
- .Rd files removed
- `devtools::check()` passes
