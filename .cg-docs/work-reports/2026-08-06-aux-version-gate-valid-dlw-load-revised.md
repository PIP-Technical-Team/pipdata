---
plan: ".cg-docs/plans/2026-08-06-aux-version-gate-valid-dlw-load-revised.md"
started: 2026-08-06
status: in-progress
---

# Work Report: Aux version gate for valid_dlw_load — revised run-level hash design

## Run 1 — 2026-08-06 — Phase 1 (Resolve and persist run-level hashes)

### Step 1: Add a run-level auxiliary hash resolver

Added `get_aux_hashes()` in `R/utils.R` (internal, `@keywords internal`). It
queries `stamp::st_catalog_query(alias = "aux")` once, matches each requested
measure to exactly one `<measure>.qs2` artifact, and returns a named character
vector of `content_hash` values. Aborts loudly on: catalog query failure,
empty catalog, missing artifact, ambiguous (multiple matching) artifact, or
missing content_hash. Never uses `st_latest()` or hashes loaded tables.

Wired into `pd_process_data()`: `aux_hashes <- get_aux_hashes(aux_measures, ...)`
is resolved before `lapply(aux_measures, pipload::load_aux_data, ...)`.

Tests: `tests/testthat/test-get-aux-hashes.R` (7 scenarios: all six measures
incl. PFW, subset, missing artifact, ambiguous artifact, empty catalog, catalog
failure, missing hash).

### Step 2: Thread hashes into build_pip_inventory() and persist them

Added `aux_hashes = NULL` parameter to `build_pip_inventory()`. In Step 7b,
current-run rows receive one `aux_<measure>_hash` column per requested measure
from the run-level map. In Step 10, aux hash columns are initialised to
`NA_character_` on the assembled master so the schema is always consistent and
old retained rows (not reprocessed) keep NA. `pd_process_data()` passes
`aux_hashes` to `build_pip_inventory()`.

Tests: extended `tests/testthat/test-build_pip_inventory.R` (3 scenarios:
current-run rows receive hashes; old retained rows keep NA; partial requested
measures leave non-requested columns absent).

### Phase 1 verification

- `testthat::test_local(filter = 'get_aux_hashes|build_pip_inventory')` — PASS.
- `testthat::test_local()` full suite — PASS (2 pre-existing skips, no failures).
- `roxygen2::roxygenise('.')` — regenerated `.Rd` files incl. `get_aux_hashes.Rd`,
  `build_pip_inventory.Rd`, `pd_process_data.Rd`. Cosmetic link warning for
  internal `get_aux_hashes` cross-reference (expected for internal functions).
- `get_aux_hashes` correctly not exported (absent from NAMESPACE).

Phase 1 evidence: V1 (resolver tests), V2 (missing/ambiguous abort), V3
(resolved once before aux loading), V4 (hashes persisted on current-run rows),
V5 (legacy rows NA) — all passed.
