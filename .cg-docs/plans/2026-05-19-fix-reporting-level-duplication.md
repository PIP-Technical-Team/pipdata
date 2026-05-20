---
date: 2026-05-19
title: "Fix reporting_level column duplication in master inventory"
status: completed
completed-date: 2026-05-20
scope: "Lightweight"
brainstorm: null
language: "R"
estimated-effort: "small"
tags: [inventory, reporting_level, join-cleanup, bug]
---

# Plan: Fix reporting_level Column Duplication in Master Inventory

## Objective

Remove spurious `reporting_level.x` and `reporting_level.y` columns from the PIP master inventory. These are ALL-NA artifacts from a previous join collision that got persisted and now cascade into every subsequent run.

## Context

`update_pip_inventory()` correctly drops `reporting_level` before re-joining from PFW. However, a past run saved suffixed columns (`reporting_level.x`, `reporting_level.y`) into the master inventory. On re-load, `old_pip_inv` carries these forward via `rowbind(..., fill = TRUE)`, and the cleanup only drops the exact `"reporting_level"` name — not the suffixed variants. The fresh PFW join then adds a valid `reporting_level`, resulting in three columns.

Diagnosis confirmed in session:
- `reporting_level.x` = all NA
- `reporting_level.y` = all NA
- `reporting_level` = correct values ("1" or "2")

## Requirements

| ID  | Requirement                                              | Source |
|-----|----------------------------------------------------------|--------|
| R1  | Drop ALL `reporting_level*` columns before PFW join      | bug    |
| R2  | Master inventory saved with exactly one `reporting_level`| bug    |

## Implementation Steps

### 1. Fix cleanup in `update_pip_inventory()`

- **Requirements**: R1, R2
- **Files**: `R/update_pip_inventory.R`
- **Details**: Replace the single-column drop at line ~243 with a pattern-based drop that removes ALL columns matching `^reporting_level`. This handles the exact name, `.x`/`.y` suffixes, and any future suffix variants (e.g. `.x.x`).
- **Change**:
  ```r
  # Before (line 243-244):
  if ("reporting_level" %in% names(new_pip_inv)) {
    new_pip_inv[, reporting_level := NULL]
  }

  # After:
  rl_cols <- grep("^reporting_level", names(new_pip_inv), value = TRUE)
  if (length(rl_cols) > 0L) {
    new_pip_inv[, (rl_cols) := NULL]
  }
  ```
- **Test Scenarios**:
  - ✅ Normal: `old_pip_inv` has only `reporting_level` → dropped, re-added from PFW
  - 🛑 Edge: `old_pip_inv` has `reporting_level.x`, `reporting_level.y`, `reporting_level` → all dropped, only PFW version remains
  - ❌ Error: PFW missing domain columns → `reporting_level` set to NA (existing behaviour, unaffected)
- **Acceptance criteria**: After a full `pd_process_data()` run, `names(new_pip_inv)` contains exactly one `reporting_level` column (no suffixed variants).

### 2. Re-run pipeline to persist clean inventory

- **Requirements**: R2
- **Details**: Execute `pd_process_data(inv = inv, force = TRUE)` to overwrite the master inventory with a clean schema. Verify the saved inventory no longer carries suffixed columns.
- **Acceptance criteria**: `pipload::load_pip_master_inventory() |> names() |> grep("reporting", x = _, value = TRUE)` returns only `"reporting_level"`.

## Testing Strategy

- Existing tests for `update_pip_inventory()` should continue to pass.
- Manual verification after the fix run (Step 2) is sufficient for this lightweight fix — the column duplication is observable in the persisted artifact.

## Documentation Checklist

- [ ] Update comment near the drop to explain pattern-based cleanup rationale

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Regex `^reporting_level` accidentally matches a future legitimate column | Unlikely — `reporting_level` is the only such column in the schema. Review if new columns are added. |

## Out of Scope

- Root-cause archaeology (which past code version introduced the `.x`/`.y` into the saved inventory). Not needed — the fix is forward-looking.
- Refactoring the PFW join itself.
