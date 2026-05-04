---
plan: .cg-docs/plans/2026-05-04-integrate-deflation.md
findings:
  P1.1: resolved
  P2.1: resolved
  P2.2: resolved
  P2.3: resolved
  P3.1: resolved
---

## Plan Review Report

**Review depth**: standard
**Plan reviewed**: `.cg-docs/plans/2026-05-04-integrate-deflation.md`
**Date**: 2026-05-04

---

### P1 — CRITICAL (must fix before implementation)

- **[P1.1]** [assumption] Metadata format assumption is unverified and likely wrong.
  The plan's `.load_deflation_aux()` pseudo-code assumes `meta$cpi`, `meta$ppp`,
  `meta$pop` returns data.tables, but `pd_aux_attr()` produces a **named list of
  attribute lists**, not data.tables. The metadata saved via
  `save_pip_data(metadata, alias = "pip_meta")` is a list where each element is a
  named list with named numeric vectors (e.g., `meta[[pip_name]]$cpi` =
  `c("2017_national" = 87.2, "2017_urban" = 91.1)`). Meanwhile `add_ppp()`,
  `add_cpi()`, and `adjust_population()` expect full data.tables with specific
  column structures (`country_code`, `ppp_data_level`, `ppp`, `ppp_year`,
  `cpi_data_level`, `cpi_value`, etc.).
  **Why**: The format gap is the entire challenge of this plan and it's currently
  marked as a risk rather than addressed as a step. Without resolving this first,
  Steps 1–2 cannot be implemented. The helper either needs to reconstruct full
  data.tables from named vectors (complex, brittle), or the deflation internals
  need to be rewritten to accept the metadata format directly (simpler but more
  invasive). This decision should be Step 0, not a risk footnote.
  **Fix**: Add a Step 0 that investigates the actual metadata structure (load a
  real example), decides the adaptation strategy, and documents the format
  contract before proceeding.
  **Resolution**: Design decision made — metadata named-list format is
  authoritative; deflation internals (`add_ppp`, `add_cpi`, `adjust_population`)
  will be rewritten to accept it directly. No Step 0 needed; decision documented
  in plan's "Design decision" paragraph and reflected in Steps 1 and 3.

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [step-order] `safe_deflation()` extraction (Step 3) should precede
  the interface refactor (Step 2). The refactored interface in Step 2 delegates
  to `deflation()` S3 methods which are still in their duplicated form. If the
  helper is extracted first, the interface refactor has a cleaner, simpler target
  to wire into. Currently if Step 2 is implemented first, it has to work with the
  messy duplicated methods, then Step 3 refactors them again — double the code
  churn.
  **Fix**: Reorder steps: current Step 3 becomes Step 2, current Step 2 becomes
  Step 3.
  **Resolution**: Implemented in P2.1-recommended order (safe_deflation extracted
  before interface refactor). Plan risk table cleaned of false-alarm entries.

- **[P2.2]** [false-alarm] `deflate_wlf()` return value risk is incorrect.
  The plan flags it as "modifies by reference but returns a subset — potential
  data loss." In reality, `get_welfare_ppp()` adds `welfare_ppp_*` columns to
  `dt_c` via `:=` (reference semantics), and the local reassignment
  `dt_wlcu <- dt_wlcu[, ..welf_vars]` only affects the local variable inside
  `get_welfare_ppp`. `deflate_wlf()` returns `dt_c` which has the original
  columns PLUS all welfare columns added by reference. This is not a bug.
  **Fix**: Remove this risk entry to avoid wasting time investigating during
  implementation.
  **Resolution**: Removed from plan risk table. [false-alarm] `char_to_fct()` existence is already verified — it's
  in `R/utils.R:296`. The risk entry "does not exist in current source" is
  incorrect.
  **Fix**: Remove this risk entry.
  **Resolution**: Removed from plan risk table.

---

### P3 — MINOR (nice to have)

- **[P3.1]** [documentation] Step 5 `@param` docs reference `survey_id`/`version`
  while the code signature in Step 2 uses `pip_id`/`version`. Minor inconsistency.
  **Fix**: Update Step 5 to reference `pip_id`/`version`.
  **Resolution**: Fixed in plan.

---

### ✅ Passed

- **Requirements completeness**: All 8 requirements map to implementation steps.
- **Test coverage**: Testing strategy covers all modes and edge cases.
- **Backward compatibility**: Legacy interface (explicit cpi/ppp/pop args) preserved.
- **Master inventory lookup**: Sound design — `format_vrs()` output provides the
  data→metadata version mapping needed.
- **Error handling**: tryCatch + piperr pattern consistent with pipeline conventions.
- **Out of Scope**: Well-defined; `pd_deflate_pipeline()` correctly deferred.
