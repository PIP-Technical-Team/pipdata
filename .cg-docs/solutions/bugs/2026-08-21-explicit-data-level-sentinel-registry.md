---
date: 2026-08-21
title: "Make data-level column sentinels explicit with a resolver registry"
category: "bugs"
language: "R"
tags: [deflation, data-level, sentinel, subnational, mixed-domain]
root-cause: "The string area simultaneously represented a literal level and a pointer to the area column, so consumer branches depended on scattered magic-string checks."
severity: "P1"
plan: ".cg-docs/plans/2026-08-20-explicit-data-level-sentinel-semantics.md"
reviewed-in: ".cg-docs/reviews/2026-08-20-explicit-data-level-sentinel-semantics-verify-review.md"
related: [".cg-docs/solutions/bugs/2026-05-06-subnational-deflation-area-attribute-not-resolved.md"]
---

# Make data-level column sentinels explicit with a resolver registry

## Problem

The `ppp_data_level`, `cpi_data_level`, and `pop_data_level` attributes are
scalar attributes on `pipmd`/`pipgd` objects. The value `"national"` is a
literal level to broadcast, while `"area"` means that each row must look up
its level in the `area` column. The same character value therefore had two
meanings.

The distinction was implemented as repeated `identical(..., "area")` checks in
deflation code. Treating `"area"` as a named-vector key instead of a column
pointer produced `NA` PPP/CPI values for rural and urban surveys. A shared
`reporting_level` discriminator was also unsafe because PPP, CPI, and
population domains can differ for one survey.

## Root Cause

The producer intentionally stores the pointer as the unchanged string
`"area"`, but the consumers had no explicit registry describing which strings
are column pointers. The pointer-vs-literal convention was therefore implicit,
duplicated, and easy to regress. Missing or malformed attributes also needed to
fall through safely rather than crashing a resolver lookup.

## Solution

Declare the sentinel mapping once in `R/aaa.R` and resolve it before each
consumer branch:

```r
.data_level_columns <- list(area = "area")

data_level_column <- function(lvl) {
  if (is.null(lvl) || !is.character(lvl) || length(lvl) != 1L || is.na(lvl)) {
    return(NA_character_)
  }
  col <- .data_level_columns[[lvl]]
  if (is.null(col)) NA_character_ else col
}
```

`add_ppp()` and `add_cpi()` resolve their own attributes and use
`dt[[resolved_column]]` only for registered pointers. Literal values keep the
scalar-broadcast path. The `adj_pop` guard uses the resolver, while
`adjust_population()` remains area-specific and is explicitly documented as a
separate boundary.

The contract tests cover the registered sentinel, literals, unregistered
sentinel-shaped values, `NULL`, empty vectors, `NA`, multi-element inputs, and
non-character scalars. Regression tests cover missing-column errors, mixed PPP
and CPI domains, and population adjustment behavior.

Validation completed with the deflation and population test files plus the full
`devtools::test()` suite: zero failures. The producer, auxiliary-vector
builder, attribute type, and `NAMESPACE` remain unchanged.

## Prevention

- Keep `*_data_level` values as scalar attributes, never materialized columns.
- Branch on each function's own `*_data_level` attribute; never use integer
  `reporting_level` as a shared discriminator.
- Add future column-pointer sentinels to `.data_level_columns` and update the
  area-specific `adjust_population()` and output-ordering consumers before
  enabling them.
- Guard resolver inputs before list lookup; non-character, missing, empty, and
  multi-element values must return `NA_character_` safely.
- Test mixed-domain combinations explicitly because PPP, CPI, and population
  levels are independently assigned.

## Related

- `.cg-docs/solutions/bugs/2026-05-06-subnational-deflation-area-attribute-not-resolved.md`
  - original silent-`NA` subnational deflation bug and mixed-domain rule.
- `.cg-docs/brainstorms/2026-08-20-data-level-sentinel-semantics.md`
  - selected column-lookup registry design.
- `.cg-docs/plans/2026-08-20-explicit-data-level-sentinel-semantics.md`
  - implementation and verification contract.
