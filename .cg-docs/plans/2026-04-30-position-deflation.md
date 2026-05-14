---
date: 2026-04-30
title: "Position pd_deflation.R as future pipeline step"
status: completed
completed-date: 2026-04-30
scope: "Lightweight"
estimated-effort: small
tags: [documentation, architecture, deflation, inline]
---

# Plan: Position pd_deflation.R as Future Pipeline Step

## Objective

Add `@note` to the `pd_deflation()` roxygen block explaining this function
is not yet integrated into the active pipeline (`pd_process_data()`), and
documenting its intended role as a future step.

## Steps

1. Edit `R/pd_deflation.R` — add `@note` paragraph to roxygen block
2. Run `devtools::document()` to regenerate `man/pd_deflation.Rd`

## Acceptance Criteria

- `man/pd_deflation.Rd` contains the note text
- `devtools::check()` passes
