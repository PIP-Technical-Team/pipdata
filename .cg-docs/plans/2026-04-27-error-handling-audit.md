````markdown
---
date: 2026-04-27
title: "Error handling audit: utils.R cleanup and nested tryCatch patterns"
status: superseded
superseded-date: 2026-04-27
superseded-by:
  - .cg-docs/plans/2026-04-27-utils-cleanup.md
  - .cg-docs/plans/2026-04-27-nested-trycatch.md
scope: "Standard"
language: R
estimated-effort: small
tags: [refactoring, code-quality, error-handling, cleanup]
---

# Plan: Error Handling & Utils Audit

## Objective

Audit two structural code quality issues:
1. **utils.R cleanup**: Identify and document unused functions, dead code (commented-out sections), and verify remaining utilities are necessary
2. **Nested tryCatch audit**: Review error handling patterns for complexity, necessity, and consolidation opportunities

## Context

The `utils.R` file contains 400+ lines with a mix of active utilities and commented-out code (notably in `unq_obs_dt()` function). Several R files also use multiple tryCatch blocks in ways that could be simplified.

Current state:
- 20 tryCatch instances found across pipeline files
- No true nested tryCatch (e.g., tryCatch inside tryCatch) patterns detected
- Conditional tryCatch blocks present (if-else choosing between tryCatch and direct execution)
- Commented-out tryCatch block in `unq_obs_dt()` (~40 lines)
- Developer notes flag potential error-handling improvements

## Requirements

| ID  | Requirement                                              | Source      |
|-----|----------------------------------------------------------|-------------|
| R1  | Document all functions in utils.R with classification   | audit       |
| R2  | Identify unused exported functions                       | audit       |
| R3  | List all commented-out code sections                    | audit       |
| R4  | Identify conditional tryCatch patterns                  | audit       |
| R5  | Flag multi-handler tryCatch blocks for simplification   | audit       |
| R6  | Assess developer-flagged error handling notes           | design      |

## Implementation

### Part 1: Utils.R Audit

#### 1.1 Function Inventory (Active Functions)

Build a complete list of all publicly-exported and internal functions in `utils.R` with classifications:

**Exported Functions** (used externally or likely part of public API):
- `uniq_vars()` — find unique variables in data frame (exported)
- `uniq_vars_to_list()` — convert unique values to list (exported)
- `uniq_vars_to_attr()` — convert unique vars to attributes, drop cols (exported)
- `vars_to_attr()` — make vars as attributes (exported)
- `num_vars_to_attr()` — create named vector of attributes (exported)
- `pipdata_int()` — get path to internal data files (exported)
- `add_attributes()` — add new attributes to data.table (exported)

**Internal Functions** (marked as @noRd or used only locally):
- `check_data_table()` — ensure input is data.table (internal, used by exported)
- `uniq_vars_to_list()` — internal helper (actually exported)
- `change_vars_to_attr()` — helper used by multiple exported funcs (private)
- `vars_to_list()` — list subselection and naming helper
- `get_ordered_level()` — return integer level code (internal)
- `piperr()` — custom error class (internal)
- `add_log()` — add entries to .logenv (internal)
- `log_failure()` — log error with timestamp (internal)
- `find_condition()` — traverse condition tree (internal)
- `last_ver_inv()` — filter inventory for latest versions (private)
- `order_ver_inv()` — order inventory by versions (private)
- `find_dt_with_attribute()` — filter list by attribute match (private)
- `char_to_fct()` — convert character columns to factors (private)
- `unq_obs_dt()` — validate uniqueness by keys (exported, has commented tryCatch)

#### 1.2 Dead Code Audit

**Identified commented-out sections**:

1. **`unq_obs_dt()` function** (lines ~466–509):
   - ~40 lines of commented-out tryCatch block with complex error handling
   - Original intent: wrap entire function in tryCatch → piperr handler → log_failure
   - Current code: simple `cli::cli_abort()` without tryCatch guard
   - Status: **Dead code — should be removed**
   - Reason: Function now uses `cli::cli_abort()` directly; the old pattern is obsolete
   - Action: Remove the entire commented block  

2. **`id_as_att()` function** (line ~440):
   - ~3 lines commented out, replaced with `data.table::setattr()`
   - Status: **Safe to remove** (replaced by active code)
   - Action: Delete the comment block

3. **Old error handler functions** (lines ~356–381):
   - `pipwrn()` and `pipmsg()` functions are commented out (not used anywhere)
   - These were alternatives to `cli::cli_warn()` and `cli::cli_inform()`
   - Status: **Dead utility** — never called
   - Action: Remove entirely

#### 1.3 Usage Check (Via Code Search)

Test whether each exported function is actually called in the pipdata package or downstream packages:

| Function | Exported | Called In Package | Action |
|----------|----------|-------------------|--------|
| `uniq_vars()` | YES | ? (need check) | verify |
| `uniq_vars_to_list()` | YES | ? | verify |
| `uniq_vars_to_attr()` | YES | ? | verify |
| `vars_to_attr()` | YES | ? | verify |
| `num_vars_to_attr()` | YES | ? | verify |
| `pipdata_int()` | YES | YES (data loading) | keep |
| `add_attributes()` | YES | ? | verify |

**Task**: Search codebase for calls to each function:
```bash
grep -r "uniq_vars\|nums_vars_to_attr\|vars_to_attr\|add_attributes" R/ tests/
```

If zero calls found in pipdata + dependencies, function is unused external API → candidate for deprecation warning + removal in future version.

### Part 2: Nested tryCatch Audit

#### 2.1 Scan Results Summary

**Total tryCatch instances**: 20  
**Pattern breakdown**:
- **Sequential independent blocks**: 14+ (different purposes, not nested)
- **Conditional blocks** (if-else choosing tryCatch vs direct): 2-3
  - `dlw_scan_and_validate.R:61-77` — if (log) { tryCatch(...) } else { direct read }
  - `pipdata_validate_gmd.R:69-87` — guard wraps error handler only if log=TRUE
- **Multiple handlers in one block**: 3+ (error + finally or similar)

**Files with tryCatch clusters**:
- `pd_deflation.R`: 3 separate tryCatch blocks (lines 106, 200, 447)
- `pipdata_validate_gmd.R`: 3 blocks (lines 69, 146, 321)
- `pipdata_dlw_compare.R`: 4 blocks (lines 21, 83, 192, 219)
- `dlw_scan_and_validate.R`: 2 blocks with conditional logic (lines 61, 180)

#### 2.2 Pattern Analysis

**Pattern A: Conditional tryCatch (if log = TRUE)**

Example from `dlw_scan_and_validate.R:54-77`:
```r
old_inv <- if (file.exists(path)) {
  if (log) {
    tryCatch(qs::qread(path), error = function(e) { log_add(...); NULL })
  } else {
    qs::qread(path)  # No error guard
  }
} else {
  tibble::tibble(...)  # empty default
}
```

**Issues**:
- Asymmetric: log=TRUE path has error guard, log=FALSE does not
- If log=FALSE and file is corrupt, process crashes without warning
- Code is hard to follow due to nested if-else

**Recommendation**: Consolidate logic:
```r
old_inv <- if (!file.exists(path)) {
  tibble::tibble(...)  # empty default
} else {
  result <- tryCatch(
    qs::qread(path),
    error = function(e) {
      if (log) pipfun::log_add("error", ...)
      NULL  # Always return NULL on error (unified)
    }
  )
  result %||na% tibble::tibble(...)
}
```

**Pattern B: Repeated identical blocks** (in `pd_deflation.R`)

The `deflation.pipmd()` and `deflation.pipgd()` methods have nearly identical tryCatch blocks:
- Same error handler structure
- Same logging call
- Same on.exit cleanup

**Recommendation**: Extract shared logic into a helper (`safe_deflation()`) or use a wrapper pattern.

**Pattern C: Multiple sequential blocks without nesting** (in `pipdata_dlw_compare.R`)

4 independent tryCatch blocks in sequence, each loading a different inventory. No interdependencies.

**Current status**: Acceptable — each is independent. No simplification needed.

#### 2.3 Developer Notes Audit

Found comments in code:
- `dlw_scan_and_validate.R:54`: `##  (!!) GC Note: tryCatch potential 1 - file read error ----`
- Suggests developer was already flagging potential improvements

**Action**: Document this as existing awareness + verify if the issue is already addressed.

### Part 3: Action Items

#### Phase A: Dead Code Removal (Immediate)

1. **Remove commented-out tryCatch in `unq_obs_dt()`** (`utils.R:466–509`)
   - File: `R/utils.R`
   - Lines: ~466–509
   - Action: Delete comment block entirely
   - Rationale: Replaced by active `cli::cli_abort()` on line 467 (after removing comments)
   - Risk: None — code is not executed

2. **Remove old error handler stubs** (`utils.R:356–381`)
   - File: `R/utils.R`
   - Lines: ~356–381 (`pipwrn`, `pipmsg` comment blocks)
   - Action: Delete the entire commented functions
   - Rationale: Never called; superseded by cli package
   - Risk: None

3. **Remove dead `id_as_att()` comment** (`utils.R:440`)
   - File: `R/utils.R`
   - Lines: ~440 (commented lines before `setattr()` call)
   - Action: Delete comment
   - Rationale: Replaced by active `data.table::setattr()` code
   - Risk: None

#### Phase B: Usage Verification (Code review)

1. Search for calls to each exported function
2. Check if any are unused API surface
3. Document findings in a summary

#### Phase C: Conditional tryCatch Refactoring (Design decision needed)

1. Review `dlw_scan_and_validate.R:54–77` pattern
2. Decide: Is asymmetric error handling intentional?
3. If not, refactor for clarity (PR for later sprint)

#### Phase D: Repeated Block Consolidation (Nice-to-have)

1. Identify if `deflation.pipmd()` + `deflation.pipgd()` duplication is acceptable
2. If not, extract helper function (separate roadmap item)

## Testing Strategy

- **Phase A (dead code removal)**: No new tests needed; verify `devtools::check()` still passes
- **Phase B (usage check)**: Manual code search + grep for each function name
- **Phase C+D (refactoring)**: Create separate plan if findings warrant changes

## Documentation Checklist

- [ ] List all utils.R functions with export status and usage classification
- [ ] Document all commented-out code sections with reason + action
- [ ] Create summary of tryCatch patterns and findings
- [ ] Identify any functions to deprecate + timeline
- [ ] Flag any architectural recommendations (conditional tryCatch consolidation)

## Acceptance Criteria

- [x] Audit complete: all dead code identified
- [ ] Phase A changes applied: 3 comment blocks removed
- [ ] Usage verified: all exported functions checked against codebase
- [ ] devtools::check() passes after cleanup
- [ ] Summary report filed as `.cg-docs/reviews/2026-04-27-error-handling-audit.md`

## Out of Scope

- Refactoring conditional tryCatch blocks (design decision pending review)
- Extracting helper functions from duplicate blocks (separate roadmap item)
- Changing error handling strategy (requires broader architecture review)

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Removing commented code breaks some workflow | Audit will verify it's truly dead; git history preserved |
| Exported functions are undocumented API surface | Usage search identifies if any are called externally |
| Refactoring tryCatch changes error behavior | Phase C flagged as design decision, not immediate |

## Related Work

- Roadmap item: `utils-cleanup` — Clean utils.R
- Roadmap item: `nested-tryCatch` — Audit nested tryCatch patterns
- Previous work: Phase 1 dplyr migration (2026-04-27) — established code quality baseline

````