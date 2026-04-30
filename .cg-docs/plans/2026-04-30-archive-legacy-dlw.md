---
date: 2026-04-30
title: "Archive legacy DLW files"
status: active
scope: "Lightweight"
estimated-effort: small
tags: [cleanup, legacy, dlw, architecture, inline]
---

# Plan: Archive Legacy DLW Files

## Objective

Move 4 pre-stamp legacy DLW files not called by either pipeline wrapper
to `old_files/`. Remove exports and .Rd files. Update NAMESPACE via
`devtools::document()`.

## Files to Archive

- `R/dlw_scan_and_validate.R` → `old_files/`
- `R/dlw_dta_to_qs.R` → `old_files/`
- `R/dlw_get_dta.R` → `old_files/`
- `R/update_dlw_inventory.R` → `old_files/`

## .Rd Files to Delete

- `man/dlw_scan_and_validate.Rd`
- `man/dlw_dta_to_qs.Rd`
- `man/dlw_get_dta.Rd`
- `man/update_dlw_inventory.Rd`

## Steps

1. Move R files to `old_files/`
2. Delete man `.Rd` files
3. Run `devtools::document()` — NAMESPACE regenerated without those exports
4. Run `devtools::check()` — no errors

## Acceptance Criteria

- No `dlw_scan_and_validate`, `dlw_dta_to_qs`, `dlw_get_dta`,
  `update_dlw_inventory` in NAMESPACE
- .Rd files gone from `man/`
- `devtools::check()` passes
