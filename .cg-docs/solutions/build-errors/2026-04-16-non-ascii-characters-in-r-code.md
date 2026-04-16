---
date: 2026-04-16
title: "R CMD check WARNING: non-ASCII characters in R source files"
category: "build-errors"
language: "R"
tags: [r-cmd-check, encoding, ASCII, unicode, CRAN, package-check]
root-cause: "Literal Unicode characters (box-drawing, em-dash, multiplication sign) embedded in R source comments and strings"
severity: "P2"
---

# R CMD check WARNING: non-ASCII characters in R source files

## Problem

`devtools::check()` emits:

```
checking R files for non-ASCII characters ... WARNING
Found the following file with non-ASCII characters:
  log_report.R
Portable packages must use only ASCII characters in their R code,
except perhaps in comments.
Use \uxxxx escapes for other characters.
```

The file passes `devtools::test()` without any failures.

## Root Cause

Three sources of non-ASCII bytes in `R/log_report.R`:

1. **Box-drawing comment decorators** (`──`, `─`) pasted from the RStudio/Positron
   section header snippet: `# ── Internal helpers ─────`
   These are U+2500/U+2502 (UTF-8: E2 94 XX) — invisible to the eye but flagged
   by `R CMD check`.

2. **Multiplication sign in roxygen comment**: `#' Build the country × error_type table`
   (`×` = U+00D7)

3. **Literal em-dash in a `sprintf()` string**: `sprintf("- \`%s\` — %s", ...)` where
   `—` (U+2014) was typed directly instead of using the escape.

## Solution

### Find all non-ASCII lines

```powershell
Select-String -Path "R/log_report.R" -Pattern "[^\x00-\x7F]" | Select-Object LineNumber, Line
```

Or in R:
```r
lines <- readLines("R/log_report.R")
which(grepl("[^\x00-\x7F]", lines, useBytes = TRUE))
```

### Fix each occurrence

| Type | Bad | Good |
|------|-----|------|
| Box-drawing section header | `# ── Internal helpers ─────` | `# -- Internal helpers -----` |
| Unicode in roxygen comment | `#' country × error_type` | `#' country x error_type` |
| Em-dash in string literal | `sprintf("... — %s", ...)` | `sprintf("... \u2014 %s", ...)` |
| Arrow in string literal | `sprintf("... → %s", ...)` | `sprintf("... \u2192 %s", ...)` |

### Bulk cleanup via PowerShell (for box-drawing section headers)

```powershell
$file = "R/log_report.R"
$content = Get-Content $file -Encoding UTF8 -Raw
$content = $content -replace '(?m)^# .+ Internal helpers .+$', '# -- Internal helpers -------'
Set-Content $file -Value $content -Encoding UTF8 -NoNewline
```

Note: `\uXXXX` escapes **inside strings** are valid and do NOT trigger the warning.
They are only forbidden as literal bytes in the source file.

## Prevention

- **Never** paste IDE section header decorators (`# ── ...`) into code that will be
  submitted to CRAN or run through `R CMD check`. Use plain ASCII dashes instead:
  `# -- Section name ----`
- **Always** use `\uXXXX` escapes for any non-ASCII character needed in a string value
  (em-dash `\u2014`, right arrow `\u2192`, en-dash `\u2013`, etc.).
- Add a pre-commit check or CI step:
  ```r
  # Quick scan before check()
  r_files <- list.files("R/", pattern = "\\.R$", full.names = TRUE)
  lapply(r_files, \(f) {
    lines <- readLines(f, warn = FALSE)
    bad <- which(grepl("[^\x00-\x7F]", lines, useBytes = TRUE))
    if (length(bad)) message(f, " lines: ", paste(bad, collapse = ", "))
  })
  ```

## Related

- See also `2026-04-16-r-cmd-check-no-visible-binding-datatable-nse.md`
- [Writing R Extensions — Encoding](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Encoding)
