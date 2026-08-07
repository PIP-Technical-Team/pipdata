---
date: 2026-08-07
depth: standard
type: standard
plan: .cg-docs/plans/2026-08-06-aux-version-gate-valid-dlw-load-revised.md
findings:
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P1.5: fixed
  P2.1: open
  P2.2: open
  P2.3: open
  P2.4: open
  P2.5: fixed
  P2.6: fixed
  P2.7: fixed
  P2.8: open
  P2.9: open
  P2.10: open
  P2.11: open
  P3.1: fixed
  P3.2: open
---

# Review Report: Aux version gate for `valid_dlw_load()`

**Review mode**: standard  
**Files reviewed**: 33 changed paths from `origin/refactor_dlw_load..HEAD`  
**Findings**: 17 (P0: 0, P1: 5, P2: 10, P3: 2)

## P1 — CRITICAL

- **[P1.1]** `R/valid_dlw_load.R` — Historical master rows are joined only by `survey_id`.
  **Why**: `aux_hash_candidates()` groups by `survey_id + content_hash_dlw` but joins to current inventory using only `survey_id`. A survey with multiple retained historical DLW hashes can violate `many-to-one` or compare against the wrong historical aux hash.
  **Fix**: Match current `inv$content_hash` to `master$content_hash_dlw` together with `survey_id` before reducing and joining. Add a multiple-history regression test.
  **Source**: `cg-code-quality`, `cg-testing`, `cg-reproducibility`, `cg-data-quality`.

- **[P1.2]** `R/build_pip_inventory.R` — Partial successful `pip_id` replacement can leave conflicting aux hashes within one survey.
  **Why**: Upsert removes retained rows by reprocessed `pip_id`, then applies hashes only to `new_versions`. If a split survey has only a subset of `pip_id`s successfully persisted, remaining rows retain old hashes.
  **Fix**: When a survey is reprocessed, drop ALL of its old rows and replace them with the fresh catalog data for that survey (retention is now by `survey_id`, not `pip_id`). This ensures the survey's pip_id set matches the current reprocess exactly; stale pip_id rows (e.g. a welfare-type split that no longer exists) are removed and remain recoverable via stamp. Added a stale-pip_id-drop regression test.
  **Source**: `cg-data-quality`.

- **[P1.3]** `valid_dlw_load()` silently disables auxiliary detection when direct callers omit `aux_hashes`.
  **Why**: Before this change, direct calls always invoked `valid_aux_load()`. With `aux_hashes = NULL`, the new code skips the aux path without warning.
  **Fix**: Preserve compatibility by resolving hashes internally, or fail loudly when hashes are omitted; do not silently skip detection.
  **Source**: `cg-architecture`.

- **[P1.4]** Public positional argument compatibility is broken.
  **Why**: `aux_hashes` was inserted before existing `verbose` in `valid_dlw_load()` and `build_pip_inventory()`, and `dt_master` before `verbose` in `inv_to_process()`.
  **Fix**: Append new parameters after existing arguments or preserve old positional bindings explicitly. Add positional-call tests.
  **Source**: `cg-architecture`.

- **[P1.5]** `valid_dlw_load()` and `build_pip_inventory()` are not tested with a real stateful persistence round trip.
  **Why**: `pip_write()` mocks return version metadata but do not capture written master/release objects and return them on reload.
  **Fix**: Add a stateful write/load mock test proving aux hashes and retained rows survive persistence and reload.
  **Source**: `cg-testing`.

## P2 — IMPORTANT

- **[P2.1]** `R/valid_dlw_load.R` — Failed master load can trigger a second load.
  **Why**: `valid_dlw_load()` stores unavailable master as `NULL`; `inv_to_process(dt_master = NULL)` interprets that as permission to load again.
  **Fix**: Use an explicit unavailable sentinel or separate supplied/available flag so the fallback state is shared without retrying.
  **Source**: `cg-architecture`.

- **[P2.2]** `aux_hashes` input is not validated.
  **Why**: Unnamed, duplicate, invalid, or missing hash values can produce empty/ambiguous measure mappings and incorrect candidate behavior.
  **Fix**: Validate character type, non-empty unique names, names matching requested measures, and non-missing hashes; abort with a classed error.
  **Source**: `cg-code-quality`.

- **[P2.3]** Catalog duplicates in `build_pip_inventory()` are selected by tied `created_at` order.
  **Why**: Equal timestamps make `.SD[1L]` dependent on input row order.
  **Fix**: Add a deterministic secondary key or abort on indistinguishable duplicates; test reordered tied rows.
  **Source**: `cg-reproducibility`.

- **[P2.4]** Current aux catalog artifact and loaded aux artifact are not cross-verified.
  **Why**: Hashes come from `st_catalog_query(alias = "aux")`, while data comes from `pipload::load_aux_data()`. The same working-release/path relationship is assumed but not verified.
  **Fix**: Verify or explicitly document the shared alias/path precondition and add an integration test.
  **Source**: `cg-reproducibility`.

- **[P2.5]** `valid_dlw_load()` documentation incorrectly says the function returns `NULL` when there is no work.
  **Why**: The implementation aborts with class `piperr`.
  **Fix**: Update roxygen and generated Rd return documentation.
  **Source**: `cg-documentation`.

- **[P2.6]** Retained aux-hash behavior is documented incorrectly.
  **Why**: Existing retained rows preserve existing hash values; only absent columns are initialized to `NA`. Comments and the work report say retained rows keep `NA`.
  **Fix**: Correct inline and work-report wording and tests.
  **Source**: `cg-documentation`.

- **[P2.7]** Stage 1 documentation says only previously-cleaned surveys are evaluated, but code passes the full filtered/latest inventory and treats new rows without hashes as candidates.
  **Fix**: Restrict Stage 1 to master-present rows or revise documentation and logging semantics to describe full-inventory candidate behavior.
  **Source**: `cg-documentation`.

- **[P2.8]** The complete pipeline still loads the master multiple times.
  **Why**: The master is loaded once in `valid_dlw_load()`, then again in `build_pip_inventory()`, and again for verification.
  **Fix**: Consider a pipeline-level handoff or document the scope of the single-load guarantee and benchmark large inventories.
  **Source**: `cg-performance`.

- **[P2.9]** `aux_hash_candidates()` performs redundant full-master uniqueness scans.
  **Fix**: Use one grouped reduction for conflict detection and survey-level output, and restrict to relevant survey IDs where safe.
  **Source**: `cg-performance`.

- **[P2.10]** Detailed aux results are materialized before candidate restriction.
  **Fix**: Push the candidate restriction into the filtering path or prefilter the inventory before `filter_aux_inv()` while preserving the requested-survey semantics.
  **Source**: `cg-performance`.

## P3 — MINOR

- **[P3.1]** `get_aux_hashes()` documents a `verbose` argument that is unused.
  **Fix**: Remove it or implement its documented behavior.
  **Source**: `cg-documentation`.

- **[P3.2]** `valid_aux_load(compare = "all")` tests do not verify non-empty release/vintage merge semantics.
  **Fix**: Add cases where both branches and only one branch contain changes.
  **Source**: `cg-testing`.

The force-mode test was strengthened to assert that `aux_hash_candidates()` is
also not called when `force = TRUE`. This is an additional safe test fix.

## Repository hygiene notes

- The implementation commits do not follow the repository's Conventional Commits convention (`Phase 1`, `Phase 2`).
- The current branch `refactor_dlw_load` does not follow the documented `type/short-description` naming convention.
- No tracked R dependency lockfile was found; this is a repository-level reproducibility gap.
- `.Rbuildignore` already excludes `.cg-docs/`; no P2 is needed for that check.
- `git diff --check` reported a trailing blank line in `.cg-docs/active-state/current.json`.
- No secrets or protected-artifact relocation/deletion issues were found.

## Passed

- No P0 findings.
- Production `joyn` calls in the changed path use `reportvar = FALSE`.
- Full R test suite passed before review with 2 pre-existing skips and no failures.
- `get_aux_hashes()` uses catalog `content_hash` and does not call `st_latest()`.
