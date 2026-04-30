---
plan: .cg-docs/plans/2026-04-30-archive-legacy-dlw.md
findings:
  P3.1: fixed
  P3.2: fixed
---

## Review Report

**Review depth**: light
**Files reviewed**: 1 R source file (`R/pd_deflation.R`)
**Findings**: 2 (P0: 0, P1: 0, P2: 0, P3: 2)

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/pd_deflation.R` (comment block above `adjust_population`) — The new `@param`/`@return` lines use plain `#` comments while the existing `#' @noRd` tag uses roxygen2 `#'` style. Both sit in the same function header.
  **Why**: Mixing `#` and `#'` in the same documentation block is valid but inconsistent. If someone later changes `@noRd` to `@export`, the plain-comment params won't be picked up by roxygen2.
  **Fix**: Convert the `# @param` / `# @return` lines to `#'` prefix so the entire block is uniform roxygen2. Keep the `@noRd` to suppress export.

- **[P3.2]** [cg-testing] `tests/testthat/test-adjust-population.R` — The test suite was last run *before* the `joyn::merge` fix. The console output showed 12 warnings (`number of items to replace is not a multiple of replacement length`) all traced to `joyn:::check_dt_by` — exactly the bug fixed by switching to `by.x`/`by.y`.
  **Why**: It's unknown whether those warnings are now gone, and whether any expectations that were coincidentally passing (tolerating the malformed merge output) still hold.
  **Fix**: Run `devtools::test(filter = "adjust-population")` and confirm the warnings no longer appear.

### ✅ Passed

- **cg-code-quality**: `by.x`/`by.y` refactor is correct, consistent formatting, no magic numbers, no hardcoded paths. `data.table::fifelse()` fully qualified. Line lengths within limits.
- **cg-code-quality**: `.Rbuildignore` already contains `^\.cg-docs$` — no finding needed.
