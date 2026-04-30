---
plan: .cg-docs/plans/2026-04-30-archive-legacy-dlw.md
findings:
  P2.1: fixed
  P2.2: fixed
  P3.1: open
  P3.2: open
---

## Review Report

**Review depth**: standard
**Files reviewed**: 10 R source/test files + NAMESPACE + roadmap.json (31 total in diff)
**Findings**: 4 (P0: 0, P1: 0, P2: 2, P3: 2)

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-testing] `R/pd_deflation.R` (end of file) — `adjust_population()` was moved here from the archived `pd_add_pip_vars.R` but has no unit tests. No tests existed for it in the archived file either.
  **Why**: The function contains non-trivial logic (population-weight scaling via a `joyn::merge`, closest-year matching, weighted mean factor). If `pd_deflation()` is ever integrated into the pipeline, an untested helper is a silent regression risk.
  **Fix**: Add tests in `tests/testthat/test-pd_deflation.R` (or a new `test-adjust-population.R`) covering: normal case (subnational + national levels), single reporting level (no multi-level path), and missing year alignment.

- **[P2.2]** [cg-architecture] `tests/testthat/helper-mock_funs.R` — only `m_svy_id_to_att()` is referenced anywhere in the active codebase (in examples). The other five helpers — `m_inv_load`, `m_inv_valid`, `m_inv_filter`, `m_compare_aux_release`, `fix_inv`, `date_valid` — are not called by any test file.
  **Why**: The roadmap plan (`relocate-mock-funs`) stated "audit which mocks are still needed (remove stale ones)" but the file was moved without the audit. Stale test helpers accumulate maintenance debt and confuse future contributors.
  **Fix**: For each of the five unused helpers, confirm it is truly unreferenced (`grep` across `tests/` and `R/`), then move to `old_files/` or delete. Keep only `m_svy_id_to_att()`.

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `tests/testthat/helper-mock_funs.R:44` — `m_inv_valid()` has a latent bug in the `"random"` branch: it references `inv_clean` which is only defined inside the `"compare"` branch.
  **Why**: Would throw `object 'inv_clean' not found` at runtime. Since `m_inv_valid()` is stale (see P2.2), fixing is only needed if the function is retained.
  **Fix**: Replace `inv_clean` with `inv_valid` in the `"random"` branch — or remove `m_inv_valid()` as part of the P2.2 stale-helper audit.

- **[P3.2]** [cg-documentation] `R/pd_deflation.R` (end of file) — `adjust_population()` has `@noRd` but no inline parameter/return comment block.
  **Why**: Minor — `@noRd` is correct. But the function is 40+ lines of non-obvious logic (joyn merge, weighted mean, reference-semantics mutation). A brief comment block would help the next reader.
  **Fix**: Add a short roxygen-style comment above the function explaining params and the weight-scaling purpose.

### ✅ Passed

- **cg-code-quality**: All 5 updated `@examples` correctly wrapped in `\dontrun{}` with explanatory comments. `data.table::fifelse()` correctly qualified in `adjust_population()`. No hardcoded paths or magic numbers.
- **cg-documentation**: `@note` on `pd_deflation()` is clear and accurate. `adjust_population()` placement and `# Moved here from...` comment well-explained.
- **cg-version-control**: `.Rbuildignore` already contains `^\.cg-docs$`. No secrets or credentials.
- **cg-reproducibility**: `joyn::merge` and `data.table::` calls fully qualified.
- **cg-performance**: `adjust_population()` uses data.table chained `[` ops correctly; reference-semantics mutation is safe (called on `copy(dt)` in outer function).
- **cg-data-quality**: `roadmap.json` — all 4 new `done` features have plan paths; statuses consistent with plan file frontmatter.
