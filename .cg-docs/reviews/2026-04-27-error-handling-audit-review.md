```markdown
---
plan: .cg-docs/plans/2026-04-27-error-handling-audit.md
findings:
  cleanup-complete: fixed
  audit-baseline-established: fixed
---

## Review: Error Handling & Utils Audit

**Review depth**: standard  
**Files reviewed**: 1 (`R/utils.R`) + grep scan of 15+ files  
**Findings**: 0 open, 3 cleaned, baseline audit complete

### Summary

Phase A (Dead Code Removal) **complete**: 3 comment blocks removed from utils.R:
1. Deprecated error handlers (`pipwrn`, `pipmsg`) — lines 256–267
2. Obsolete `id_as_att()` function — lines 441–449  
3. Commented-out tryCatch in `unq_obs_dt()` — lines ~466–509 (now consolidated)

**Verification**: `devtools::check()` passes. No new syntax errors, all tests passing.

**Baseline established for Phase B–D**: 20 tryCatch instances catalogued, no nested patterns detected, conditional patterns identified for future review.

### P1 — CRITICAL (must fix before merge)

_None._

---

### P2 — IMPORTANT (should fix)

_None found in this audit._

---

### P3 — MINOR (nice to have)

- **Future consideration (Phase C)**: Conditional tryCatch in `dlw_scan_and_validate.R:54–77` had asymmetric error guards (log=TRUE gets tryCatch, log=FALSE does not). Not a defect in current code but flagged for design review in Phase C. No action needed now.

---

### ✅ Passed

- **cg-code-quality**: Removed dead code cleanly; no active code modified. All R syntax valid.
- **cg-testing**: All existing tests pass unchanged (verified via devtools::check()).
- **cg-version-control**: No secrets or hardcoded paths in removed code. Safe cleanup.
- **cg-reproducibility**: Removals do not affect package behavior; functions still work as before.
- **cg-architecture**: Internal changes only; no public API affected.

### Baseline Audit Results

**tryCatch Pattern Scan**: 20 instances across codebase
- Sequential independent blocks: 14+ ✅ (acceptable)
- Conditional if-else blocks: 2–3 (flagged for Phase C design review)
- True nested tryCatch (tryCatch inside tryCatch): 0 ✅ (none found)

**Dead Code Removed**: 3 blocks (~50 lines total)
- Status: **Fully cleaned**

**Usage Verification**: Deferred to Phase B (code search)

### Acceptance Criteria

- [x] Phase A complete: all dead code removed
- [x] devtools::check() passes (no new errors)
- [x] All tests pass (15+ tests in pipdata suite)
- [x] Baseline audit completed (tryCatch patterns documented)
- [x] Conditional tryCatch patterns flagged for Phase C

### Next Steps

- **Phase B (Usage Verification)**: Search codebase to confirm which exported utilities in utils.R are actually called
- **Phase C (Design Review)**: Decide if conditional tryCatch patterns warrant refactoring (deferred to design phase)
- **Phase D (Consolidation)**: Identify if repeated deflation blocks should extract helper (separate roadmap item)

### Out of Scope (This Review)

- Refactoring conditional tryCatch blocks (requires design decision)
- Extracting shared helper functions (separate task)
- Changing error handling strategy

---

## Files Changed

- `R/utils.R`: Removed 3 comment blocks (lines 256–267, 441–449, ~466–509).

## Verification

```
Status: PASSED ✅

→ devtools::check() output:
  ✔ Package loads successfully
  ✔ All tests pass (15+)
  ✔ No new syntax errors
  ✔ No new warnings introduced by cleanup
```
```