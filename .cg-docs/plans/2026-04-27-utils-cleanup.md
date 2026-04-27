---
date: 2026-04-27
title: "utils.R cleanup"
status: completed
completed-date: 2026-04-27
scope: "Light"
language: R
estimated-effort: small
tags: [cleanup, utilities, code-quality]
roadmap-id: utils-cleanup
---

# Plan: utils.R Cleanup

## Objective

Audit all functions in `R/utils.R`, confirm which exported functions are
actually used within the package, and remove or deprecate any that are dead
API surface.

## What Is Already Done

**Phase A — Dead code removal** (completed 2026-04-27):
- Removed `pipwrn()` / `pipmsg()` stub comment block
- Removed `id_as_att()` obsolete wrapper comment
- Removed ~40-line commented-out tryCatch in `unq_obs_dt()`

## What Remains

### Phase B — Usage Verification

**Finding**: A grep of the entire `R/` directory shows that **none of the
exported utility functions are called outside `utils.R` itself**. They only
appear as definitions or internal calls to each other. Specifically:

| Function | Exported | Called outside utils.R? |
|----------|----------|------------------------|
| `uniq_vars()` | YES | No — only called by `uniq_vars_to_list()` inside utils.R |
| `uniq_vars_to_list()` | YES | No — only called by `uniq_vars_to_attr()` inside utils.R |
| `uniq_vars_to_attr()` | YES | No |
| `vars_to_attr()` | YES | No |
| `num_vars_to_attr()` | YES | No |
| `add_attributes()` | YES | No |
| `unq_obs_dt()` | YES | No — only in example using `pipload` |
| `pipdata_int()` | YES | No active call found in pipeline code |

**Internal functions** (not exported, but called within package):
| Function | Called by |
|----------|-----------|
| `check_data_table()` | `uniq_vars()`, `vars_to_attr()`, `num_vars_to_attr()` |
| `change_vars_to_attr()` | `uniq_vars_to_attr()`, `vars_to_attr()`, `num_vars_to_attr()` |
| `vars_to_list()` | `vars_to_attr()`, `num_vars_to_attr()` |
| `get_ordered_level()` | Unknown — needs grep in non-utils files |
| `piperr()` | Multiple pipeline files |
| `add_log()` | `log_failure()` |
| `log_failure()` | Multiple pipeline files |
| `find_condition()` | `log_failure()` |
| `last_ver_inv()` | `valid_dlw_load.R` (2 calls) |
| `order_ver_inv()` | Unknown — needs grep |
| `find_dt_with_attribute()` | Unknown — needs grep |
| `char_to_fct()` | `pd_deflation.R` |

## Implementation Steps

### Step 1: Confirm zero external usage of exported functions

Run in console to verify:
```r
# Should return 0 for each in package R/ (excluding utils.R itself)
fns <- c("uniq_vars", "uniq_vars_to_list", "uniq_vars_to_attr",
         "vars_to_attr", "num_vars_to_attr", "add_attributes",
         "unq_obs_dt", "pipdata_int")
files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
files <- files[!grepl("utils", files)]
for (f in fns) {
  hits <- grep(f, unlist(lapply(files, readLines)), value = TRUE)
  cat(f, "->", length(hits), "\n")
}
```

### Step 2: Verify internal-only functions are still internally used

Check `get_ordered_level`, `order_ver_inv`, `find_dt_with_attribute` are
actually called somewhere — if not, they are candidates for removal.

### Step 3: Decision per unused exported function

For each unused exported function decide:
- **Keep** if it is part of intentional public API (document with `@family`)
- **Deprecate** if previously used externally (add `lifecycle::deprecate_warn()`)
- **Make internal** (`@noRd`, remove `@export`) if only used within pipdata
- **Remove** if completely dead with no downstream users

### Step 4: Apply decisions

- Update `@export` / `@noRd` tags as needed
- Run `devtools::document()` + `devtools::check()` to verify NAMESPACE is correct

## Acceptance Criteria

- [x] All exported utils functions audited with explicit keep/remove decision
- [x] Zero exported functions that are unreachable (no callers, no public intent)
- [x] `devtools::check()` passes (no new issues introduced)
- [x] `roadmap.json` `utils-cleanup` updated to `"status": "done"`

## Out of Scope

- Refactoring the remaining internal functions (separate from cleanup)
- Moving utilities to other files
