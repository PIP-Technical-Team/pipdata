---
date: 2026-04-29
title: "Roxygen [foo()] cross-reference to unexported function causes R CMD check WARNING"
category: "build-errors"
language: "R"
tags: [r-cmd-check, roxygen2, documentation, unexported, cross-reference]
root-cause: "roxygen2 [foo()] link syntax generates an Rd cross-reference that requires the target to be exported"
severity: "P2"
---

> **RECONFIRMED 2026-08-04**: Still the team's established roxygen practice
> — referenced by multiple other solution docs in this repo. No content
> changes needed.

# Roxygen `[foo()]` cross-reference to unexported function causes R CMD check WARNING

## Problem

Running `devtools::check()` produces a WARNING:

```
checking Rd cross-references ... WARNING
  Missing link or links in documentation object 'valid_dlw_load.Rd':
    'last_ver_inv'
```

The offending line in the roxygen block:

```r
#' 3. Selects the latest version of each survey via [last_ver_inv()].
```

## Root Cause

Roxygen's `[foo()]` inline link syntax compiles to an Rd `\link{foo}` cross-reference,
which R's documentation system resolves at check time. If `foo` is not exported from
the package (or any declared dependency), the check engine cannot resolve the link and
emits a WARNING.

Internal helpers documented with `@noRd` or simply not listed in NAMESPACE will always
trigger this WARNING when referenced via `[foo()]`.

## Solution

Replace `[foo()]` with backtick inline code for any function that is not exported:

```r
# Before (WARNING):
#' 3. Selects the latest version of each survey via [last_ver_inv()].

# After (clean):
#' 3. Selects the latest version of each survey via `last_ver_inv()`.
```

The same applies to internal utilities referenced in `@details`, `@seealso`, or
numbered list descriptions:

```r
# Also affected:
#' Removes surveys already cleaned via [inv_to_process()].

# Fixed:
#' Removes surveys already cleaned via `inv_to_process()`.
```

## Prevention

- Only use `[foo()]` syntax for **exported** functions (i.e., those with `@export`
  that appear in NAMESPACE).
- For internal helpers (`@noRd` or no `@export`), always use `` `foo()` `` backtick code.
- For exported functions in **other packages**, use `[pkg::foo()]`.
- A quick rule: if `?foo` doesn't work in the R console, `[foo()]` will cause a WARNING.

## Related

- [roxygen2 inline links documentation](https://roxygen2.r-lib.org/articles/rd-formatting.html#links)
- See also `.cg-docs/solutions/build-errors/2026-04-16-r-cmd-check-no-visible-binding-datatable-nse.md`
  for related R CMD check NOTE pattern (globalVariables)
