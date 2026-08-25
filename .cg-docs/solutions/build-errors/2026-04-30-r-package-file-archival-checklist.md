---
date: 2026-04-30
title: "R package file archival checklist"
category: "build-errors"
language: "R"
tags: [archiving, r-cmd-check, namespace, examples, dontrun, test-files, dead-code]
root-cause: "Archiving R source files without auditing callers, test files, and example cross-references leaves broken references that fail R CMD check"
severity: "P2"
---

# R Package File Archival Checklist

## Problem

When archiving (moving to `old_files/`) R source files from an active package,
`devtools::check()` failed with multiple errors:

1. **Missing function**: `adjust_population()` was defined in the archived
   `pd_add_pip_vars.R` but still called in `pd_deflation.R`. Package failed to
   build.
2. **Undefined example symbol**: `@examples` blocks that called
   `survey_id_to_attr()` (a replacement for `pipdata:::m_svy_id_to_att()`)
   without `\dontrun{}` caused R CMD check to flag the symbol as undefined.
   The original `:::` calls silently suppressed static analysis — removing
   them exposed the bare call to the checker.
3. **Orphaned test file**: `test-dlw_dta_to_qs.R` tested the archived
   `dlw_dta_to_qs()` function. With the source file gone, `devtools::test()`
   reported errors for a function that no longer exists in the namespace.

## Root Cause

Archiving an R file removes its exports from the build but does not
automatically find:
- Other R files that call functions defined in the archived file
- Test files that exercise the archived function
- `@examples` that reference symbols from the archived file (especially if
  they were previously shielded from static analysis by `:::` access)

The `:::` operator in examples (`pkg:::fun()`) tells R CMD check the call is
intentionally internal — it skips undefined-symbol checks for it. Replacing a
`:::` call with a plain call in an example immediately re-enables static
analysis against that symbol.

## Solution

**Complete checklist before archiving any R file:**

### 1. Find all callers in `R/`
```r
# In project root terminal
grep -r "adjust_population\|function_name" R/
```
For each match that is a real call site (not in the file being archived):
- If the function is still needed, move it to the calling file as `@noRd` internal
- If the function is truly dead, confirm no active caller exists before archiving

### 2. Find the test file
```r
# Test file naming pattern: tests/testthat/test-<stem>.R
# e.g., R/dlw_dta_to_qs.R → tests/testthat/test-dlw_dta_to_qs.R
```
Move the test file to `old_files/` alongside the source file.

### 3. Audit `@examples` in other files that reference the archived file's functions
```r
grep -r "archived_fun_name" R/
```
Any example referencing a function from the archived file must either:
- Be replaced with an equivalent that doesn't depend on the archived function, OR
- Be wrapped in `\dontrun{}` with a comment explaining the dependency

### 4. Check `:::` replacements trigger static analysis
If any examples previously used `pkg:::fun()` to call functions from the
archived file, replacing with a bare call exposes the symbol to R CMD check.
Wrap in `\dontrun{}`:
```r
#' @examples
#' \dontrun{
#' # Requires network access and working release
#' md <- pipload::pip_load_dlw("PHL", 2012)
#' md <- survey_id_to_attr(md, unique(md$survey_id))
#' }
```

### 5. Delete the `.Rd` files
```r
# man/<function>.Rd must be deleted for each exported function in the archived file
# devtools::document() does NOT delete stale .Rd files — manual deletion required
file.remove("man/archived_fun.Rd")
```

### 6. Run `devtools::document()` then `devtools::check()`
`document()` regenerates NAMESPACE (removes the stale exports).
`check()` catches any remaining broken references.

## Prevention

Before archiving any `R/*.R` file run this mental checklist:

| Check | Command |
|-------|---------|
| Any active callers in `R/`? | `grep -r "fun_name" R/` |
| Corresponding test file? | `ls tests/testthat/test-<stem>.R` |
| Referenced in `@examples` elsewhere? | `grep -r "fun_name" R/` |
| Any `:::` calls that will become bare calls? | review replacements |
| `.Rd` files to delete? | `ls man/<fun_name>.Rd` |

## Related

- [2026-04-16-r-cmd-check-no-visible-binding-datatable-nse.md](../build-errors/2026-04-16-r-cmd-check-no-visible-binding-datatable-nse.md) — other R CMD check pitfalls
- [2026-04-16-mocking-external-package-calls-at-function-startup.md](../testing-patterns/2026-04-16-mocking-external-package-calls-at-function-startup.md) — test file patterns for functions with external dependencies
- [Verify exported R API retirement from built package surfaces](../testing-patterns/2026-08-25-verify-exported-r-api-retirement.md) - extends the checklist with baseline/final installed-package evidence for intentional API deletion
