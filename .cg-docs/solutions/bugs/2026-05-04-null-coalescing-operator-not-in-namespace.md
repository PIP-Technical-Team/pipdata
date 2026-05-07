---
date: 2026-05-04
title: "NULL-coalescing operator %||% silently unavailable in package functions"
category: "bugs"
language: "R"
tags: [namespace, operator, rlang, null-coalescing, package-development]
root-cause: "%||% is defined in rlang but not re-exported by pipdata; devtools::load_all() makes it appear to work in interactive sessions while it fails silently in installed or checked packages"
severity: "P1"
---

# NULL-coalescing operator `%||%` silently unavailable in package functions

## Problem

`cpi_ppp_years()` in `R/pd_deflation.R` contained:

```r
ppp_versions <- attr(dt, "ppp_versions") %||% attr(ppp, "ppp_versions")
```

This line caused a `could not find function "%||%"` error at runtime. The bug was
not caught during interactive development because `devtools::load_all()` populates
the search path with rlang's exports, making `%||%` available in the session even
though it is not in pipdata's namespace.

## Root Cause

`%||%` is defined in rlang. Unless pipdata explicitly imports it
(`@importFrom rlang %||%`) or defines its own version, the operator is unavailable
in the installed package and in `R CMD check`. `load_all()` masks the gap because
it attaches all dependency namespaces to the session.

The same bug appeared earlier in `pd_deflation()` and was fixed there; `cpi_ppp_years()`
was a second instance that survived the first fix.

## Solution

Replace every `%||%` usage with an explicit `if (is.null(...))` guard, which is
self-contained and requires no import:

```r
# Before (broken outside load_all())
ppp_versions <- attr(dt, "ppp_versions") %||% attr(ppp, "ppp_versions")

# After (correct everywhere)
ppp_versions <- attr(dt, "ppp_versions")
if (is.null(ppp_versions)) {
  ppp_versions <- attr(ppp, "ppp_versions")
}
```

Alternatively, add to `R/utils.R` (or any utilities file):

```r
#' @importFrom rlang `%||%`
#' @export
NULL
```

and add `rlang` to `DESCRIPTION Imports`. This lets the operator be used freely,
but adds a dependency. The explicit `if (is.null(...))` approach has no dependency
cost.

## Prevention

- **Never use `%||%` (or any infix operator from another package) in pipdata
  functions without a corresponding `@importFrom` declaration.**
- After any interactive session where `%||%` appears to work, run
  `R CMD check` or at minimum `devtools::check_man()` before committing.
- If `%||%` is needed regularly, add it once to a utilities file with
  `@importFrom rlang \`%||%\`` rather than relying on session state.
- Code review: grep for `%||%` in files that lack `@importFrom rlang` coverage.

## Related

- `.cg-docs/solutions/build-errors/2026-04-30-r-package-file-archival-checklist.md`
  — another class of "works in load_all but fails in check" issues
