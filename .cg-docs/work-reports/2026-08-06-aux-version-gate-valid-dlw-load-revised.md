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
from the run-level map. In Step 10, missing aux hash columns are initialised to
`NA_character_` on the assembled master so the schema is always consistent;
old retained rows preserve any existing hash values. `pd_process_data()` passes
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

## Run 2 — 2026-08-07 — Phase 2 (Gate and filter aux changes)

### Step 3: Load master once and implement two-stage aux filtering

Rewrote `valid_dlw_load()` in `R/valid_dlw_load.R`:

- Added `aux_hashes = NULL` parameter.
- Loads the master inventory **once** (shared `dt_master`) and passes it to
  both `inv_to_process()` (DLW comparison) and `aux_hash_candidates()` (aux
  comparison). `inv_to_process()` now accepts `dt_master` to avoid a second
  load.
- Stage 1: `aux_hash_candidates()` compares each previously-cleaned survey's
  stored `aux_<measure>_hash` against the current hash over the full
  filtered/latest inventory. Missing historical hash → candidate (migration).
  Aborts on conflicting aux hashes for the same `survey_id`/`content_hash_dlw`.
- Stage 2: `valid_aux_load()` runs only for the changed measures, then
  `filter_aux_inv()` results are intersected with the candidate set.
- `force = TRUE` skips master load and all aux comparisons; processes all
  filtered/latest surveys.
- Preserved logmeta discriminators with revised trigger conditions.

`pd_process_data()` passes `aux_hashes` to `valid_dlw_load()`.

Tests: updated `test-valid_dlw_load.R` (existing scenarios adapted to the
two-stage flow) and `test-valid_aux_load.R` (verbose propagation now exercises
Stage 2). Added 9 new gating tests: unchanged hash skips comparison, changed
hash invokes only changed measures, COL/ARG vs USA/GER non-affected, affected
requested survey returned, missing historical hash candidate, conflicting
hashes abort, master loaded once, force mode skips comparisons, no `.joyn` /
no duplicate survey IDs.

### Phase 2 verification

- `testthat::test_local(filter = 'valid_dlw_load|valid_aux_load')` — PASS.
- `testthat::test_local()` full suite — PASS (2 pre-existing skips, no failures).
- `roxygen2::roxygenise('.')` — regenerated `.Rd` files incl. `valid_dlw_load.Rd`,
  `inv_to_process.Rd`, `aux_hash_candidates.Rd`.

Phase 2 evidence: V6 (master loaded once), V7 (changed measures only invoke
valid_aux_load), V8 (COL/ARG vs USA/GER intersection), V9 (conflict abort),
V10 (new/DLW-changed surveys remain), V11 (force mode skips), V12 (no .joyn /
no duplicates), V13 (targeted tests), V14 (full suite) — all passed.
