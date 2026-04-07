---
date: 2026-04-06
title: "Documenting internal functions in the pd_process_data pipeline"
category: "testing-patterns"
language: "R"
tags: [roxygen2, documentation, internal-functions, pipeline, r-package, pd_process_data]
root-cause: "Internal helper functions in the pd_process_data pipeline had no roxygen2 documentation, making them opaque to maintainers and Copilot."
severity: "P3"
---

# Documenting Internal Functions in the pd_process_data Pipeline

## Problem

The top-level wrapper `pd_process_data()` delegates to a chain of internal
functions across four files. None of those functions had roxygen2 documentation,
making it difficult to understand inputs, outputs, and responsibilities when
reading or reviewing the code.

The functions affected were:

| File | Functions |
|---|---|
| `R/inv_dlw_load.R` | `inv_dlw_load`, `data_to_dt`, `survey_id_to_attr` |
| `R/pd_aux_attr.R` | `pd_aux_attr`, `add_attr`, `filter_aux_data`, `create_attr` |
| `R/save_pip.R` | `save_pip_data` |
| `R/valid_dlw_load.R` | `valid_dlw_load`, `filter_aux_inv`, `fix_year_var`, `inv_to_process` |
| `R/update_pip_inventory.R` | `update_pip_inventory`, `format_vrs` |

## Root Cause

During rapid development of the pipeline, inline logic was added without
documentation. Exported functions (from `NAMESPACE`) already had `.Rd` files
from a previous pass, but the internal functions that constitute the actual
pipeline steps were never documented.

## Solution

### Rule of thumb: export top-level, document everything

- **Top-level pipeline steps** (functions called directly by `process_data()`
  or `pd_process_data()`) → `@export` + full roxygen2 block.
- **Internal helpers** (called only by the above) → `@keywords internal` +
  full roxygen2 block (still documents, but does not export).

### Tags used

All documented functions are linked with:

```r
#' @family pd_process_data pipeline
```

This groups them in the pkgdown reference page and in `?` help pages.

### Template for exported pipeline functions

```r
#' <One-line title>
#'
#' <2-4 sentence description of what the function does and why.>
#'
#' @param x Description.
#' @param y Description.
#'
#' @return Description of the return value.
#'
#' @family pd_process_data pipeline
#' @export
my_function <- function(x, y) { ... }
```

### Template for internal helpers

```r
#' <One-line title>
#'
#' <2-4 sentence description.>
#'
#' @param x Description.
#'
#' @return Description.
#'
#' @family pd_process_data pipeline
#' @keywords internal
my_helper <- function(x) { ... }
```

### Regenerate docs after adding roxygen2 blocks

```r
devtools::document("path/to/package")
```

This updates `NAMESPACE` and creates `.Rd` files in `man/`.

## Prevention

- Every new function added to the pipeline (even internal) should include a
  roxygen2 block at the time it is written.
- The R instructions file already requires: *"Every exported function must have
  roxygen2 documentation. Required tags: `@param`, `@return`, `@export`,
  `@examples`."*
- Extend this habit to internal functions: use `@keywords internal` instead of
  `@export`, and omit `@examples` if the function has side effects or requires
  external data.
- Use `@family` tags to group functions that belong to the same pipeline step.
  This makes the help pages navigable.

## Related

- R instructions: `e:\PovcalNet\01.personal\wb535623\PIP\pipdata\.github\instructions\r.instructions.md`
- Affected source files: `R/inv_dlw_load.R`, `R/pd_aux_attr.R`, `R/save_pip.R`,
  `R/valid_dlw_load.R`, `R/update_pip_inventory.R`
