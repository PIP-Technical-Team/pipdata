---
plan: .cg-docs/plans/2026-05-19-fix-reporting-level-duplication.md
findings:
  P3.1: fixed
---

## Review Report

**Review depth**: standard
**Files reviewed**: 3 (`R/update_pip_inventory.R`, `tests/testthat/test-update_pip_inventory.R`, `Pipdata_script.R`)
**Findings**: 1 (P0: 0, P1: 0, P2: 0, P3: 1)

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-testing] `tests/testthat/test-update_pip_inventory.R` — Both collision tests re-implement the drop logic inline rather than exercising it through `update_pip_inventory()`.
  **Why**: The tests verify the *fix pattern* is correct, not that `update_pip_inventory()` actually calls it. If the production code is later reverted to the exact-match form, both tests could still pass (they run their own copy of the drop). This is a pre-existing architecture constraint for this file (end-to-end testing requires mocking 4+ external I/O calls).
  **Fix**: Accept as-is for now; consider a `local_mocked_bindings()` end-to-end wrapper if the full function test harness is ever built.

### ✅ Passed

- **cg-code-quality**: `grep("^reporting_level", names(new_pip_inv), value = TRUE)` with `length(...) > 0L` guard is idiomatic. Comment explains mechanism and root cause.
- **cg-documentation**: No new public functions. Existing roxygen contract for `reporting_level` unchanged.
- **cg-version-control**: Branch `fix_inventory`. No secrets. Commit message compliant.
- **cg-reproducibility**: Deterministic fix. No seeds, paths, or lockfile impact.
- **cg-performance**: `grep()` on column names is O(column count) — negligible.
- **cg-architecture**: Localised guard, no new dependencies, no structural impact.
- **cg-data-quality**: Enforces single authoritative `reporting_level` column in master inventory.
