---
date: 2026-08-17
title: "Add force_surveys parameter for surgical survey re-processing"
status: completed
completed-date: 2026-08-17
completed-phases: [1, 2]
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-08-15-force-surveys-surgical-reprocessing.md"
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [pd_process_data, valid_dlw_load, force, force_surveys, stamp, versioning, master-inventory, pipeline, surgical-reprocessing]
phases: 2
execution-report: ".cg-docs/work-reports/2026-08-17-force-surveys-surgical-reprocessing.md"
---

# Plan: Add `force_surveys` parameter for surgical survey re-processing

## Objective

Add a `force_surveys` parameter (character vector of `survey_id` and/or
`pip_id`) to `pd_process_data()` and `valid_dlw_load()` that re-processes the
named surveys alongside the normal invalidation candidates, **without** the
destructive global side effects of `force = TRUE` (which switches stamp
versioning to `"timestamp"` for the entire run and bypasses all invalidation
logic).

## Context

`force = TRUE` in `pd_process_data()` (`R/pd_process_data.R:61-64`) does two
things at once:

1. **Global stamp side-effect**: switches `stamp::st_opts(versioning =
   "timestamp")` for the entire run. Under timestamp versioning, every
   re-saved survey gets a *new* version even when its content is
   byte-identical, polluting version history for every survey in the run.
2. **Bypass all invalidation** via `valid_dlw_load(force = TRUE)`
   (`R/valid_dlw_load.R:109-134`): skips the master-inventory load,
   `inv_to_process()` (the DLW content-hash comparison), aux-hash candidate
   detection, and `valid_aux_load()` — so *every* survey is reprocessed.

Production needs the ability to re-run specific surveys (e.g. after fixing a
cleaning bug for one country) without affecting version history for every other
survey. The downstream assembler `build_pip_inventory()`
(`R/build_pip_inventory.R:1-16`) is already a delta upsert: it reads version
facts for current-run surveys only and retains all others unchanged. So the
blast radius of `force = TRUE` is confined to (1) the stamp versioning switch
and (2) the over-broad candidate set — not the assembler.

The confirmed design (see
`.cg-docs/brainstorms/2026-08-15-force-surveys-surgical-reprocessing.md`)
unions forced surveys into the normal candidate set inside
`valid_dlw_load()`, bypassing `inv_to_process()` only, preserving content
versioning, and accepting both `survey_id` and `pip_id` via the master
inventory's `pip_id`→`survey_id` reverse-map (which is already loaded when
`!force`, so `force_surveys` is mutually exclusive with `force = TRUE` → no
extra I/O cost).

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | `force_surveys` re-processes named surveys PLUS the normal invalidation candidates (DLW-new + aux-changed); the forced set is additive, not a replacement | Brainstorm D1 |
| R2 | Forced surveys bypass `inv_to_process()` only; aux-change detection runs normally and overlaps are deduplicated by `unique()` at line 294 | Brainstorm D2 |
| R3 | Stamp versioning stays on its default (`"content"`) for the entire run; `force_surveys` never calls `stamp::st_opts()` | Brainstorm D3 |
| R4 | `force = TRUE` and non-NULL `force_surveys` are mutually exclusive → `cli_abort(class = "piperr")` | Brainstorm D4 |
| R5 | `force_surveys` accepts both `survey_id` and `pip_id`, auto-detected by lookup-first (survey_id membership in `inv_svy_full` → pip_id reverse-map via master → unknown) | Brainstorm D5 |
| R6 | An identifier matching neither a `survey_id` in the (module-filtered, latest-version) inventory nor a `pip_id` in the master is warned about, logged, and skipped — it does not abort the run | Brainstorm edge case |
| R7 | The "nothing to clean" abort (`valid_dlw_load.R:280-288`) accounts for the forced set; a run with empty normal candidates but non-empty forced surveys proceeds | Brainstorm edge case |
| R8 | Emit `force_surveys_inf` (resolved forced survey_ids, distinguishing survey_id-resolved vs pip_id-resolved) and `force_surveys_unknown_inf` (unresolved identifiers) log entries for auditability | Brainstorm edge case |
| R9 | A forced survey outside the module filter is excluded by the `inv_svy_full` intersection; warn if NONE of the forced identifiers resolve to a filtered-inventory survey | Brainstorm edge case |
| R10 | `force = TRUE` behavior is unchanged: skips master/aux comparisons and processes all filtered/latest surveys | Existing behavior; regression test |

## Phase 1: Core implementation

### 1. Add `force_surveys` parameter to `pd_process_data()` and `valid_dlw_load()` with mutual-exclusivity guards

- **Requirements**: R3, R4
- **Files**: `R/pd_process_data.R`, `R/valid_dlw_load.R`
- **Details**:
  - Add `force_surveys = NULL` to the `pd_process_data()` signature, after
    `force` and before `verbose`.
  - Add a mutual-exclusivity guard immediately after the `force` stamp block
    (after line 65): if `force && !is.null(force_surveys)`, call
    `cli::cli_abort("force and force_surveys are mutually exclusive: force = TRUE switches stamp to timestamp versioning globally while force_surveys preserves content versioning. Specify only one.", class = "piperr")`.
    This fires before any inventory/stamp work.
  - The existing `if (force) { st_opts... }` block at lines 61-65 stays
    gated purely on `force`. `force_surveys` must never touch `st_opts()`.
  - Thread `force_surveys` into the `valid_dlw_load()` call at line 85 as a
    new argument.
  - Do not change any other behavior of `pd_process_data()`; the per-survey
    loop, logging, `build_pip_inventory()` call, and return are unchanged.
  - **`valid_dlw_load()` guard** (P1 finding): `valid_dlw_load()` is exported
    and can be called directly, so it must also carry its own
    `force && !is.null(force_surveys)` guard at function entry, before any
    inventory/stamp work. This follows the existing pattern where
    `valid_dlw_load()` validates its own inputs (e.g., `aux_hashes` validation
    at lines 145-170). Use the same `cli::cli_abort()` message and `class =
    "piperr"` as the `pd_process_data()` guard.
- **Test scenarios**:
  - happy path: `force = FALSE, force_surveys = "COL_2020_GEIH"` passes through
    without error;
  - error path: `force = TRUE, force_surveys = "COL_2020_GEIH"` aborts with
    class `piperr`;
  - `force = TRUE` alone still works (no `force_surveys`) — regression.
  - **direct-call guard** (P1 finding): calling `valid_dlw_load()` directly
    with `force = TRUE, force_surveys = "X"` aborts with class `piperr`
    (verified in `tests/testthat/test-valid_dlw_load.R`).
- **Tests**: `tests/testthat/test-pd_process_data.R` (new or existing file —
  check for an existing test file first via `glob`), plus
  `tests/testthat/test-valid_dlw_load.R` for the direct-call guard.
- **Acceptance criteria**: mutual-exclusivity abort fires before any
  stamp/inventory work in BOTH `pd_process_data()` and `valid_dlw_load()`;
  `force_surveys` is threaded to `valid_dlw_load()`; no `st_opts()` call is
  reachable via `force_surveys`.

### 2. Add `force_surveys` parameter, identifier resolution, and candidate union to `valid_dlw_load()`

- **Requirements**: R1, R2, R5, R6, R7, R9
- **Files**: `R/valid_dlw_load.R`, `tests/testthat/test-valid_dlw_load.R`
- **Details**:
  - Add `force_surveys = NULL` to the `valid_dlw_load()` signature after
    `aux_hashes`.
  - Define an internal helper `resolve_force_surveys(force_surveys, inv_svy_full, dt_master, verbose)` in `R/valid_dlw_load.R` (not a separate file — the resolution logic is ~15-20 lines, lives with the function that uses it, and is never reused elsewhere) that:
    - Returns `list(survey_ids = character(0), resolved_from_survey_id = character(0), resolved_from_pip_id = character(0), unknown = character(0))` when `force_surveys` is NULL/empty.
    - **Input validation** (P3 finding): validate `force_surveys` is a character vector — `if (!is.character(force_surveys)) cli::cli_abort(class = "piperr")` — following the existing `aux_hashes` validation pattern at `valid_dlw_load.R:146-170`. This prevents silent no-ops when a numeric or factor is passed.
    - **Early dedup** (P2 finding): deduplicate before the resolution loop — `force_surveys <- unique(force_surveys)` — so log counts (`n_forced`, resolved/unresolved counts) reflect the actual number of unique surveys to force, not redundant caller-supplied entries. Add a test case for duplicate inputs.
    - Otherwise, for each identifier in `force_surveys`:
      1. If it matches a `survey_id` in `inv_svy_full$survey_id` → add to
         `survey_ids` and `resolved_from_survey_id`.
      2. Else if `dt_master` is non-NULL **and** `dt_master` has a `pip_id`
         column (P2 finding: defensive column-existence check) and the
         identifier matches a `pip_id` in `dt_master$pip_id`
         (case-insensitive via `toupper`, matching how `pip_id_map` is built
         at `pd_process_data.R:155-160`) → reverse-map to its `survey_id`.
         Because the master has one row per `(survey_id, pip_id)` (see
         `.cg-docs/solutions/data-quality/2026-08-07-aux-content-hash-gated-recleaning.md`),
         the `survey_id` for a given `pip_id` is unique. Add the resolved
         `survey_id` to `survey_ids` (only if it is also present in
         `inv_svy_full` — otherwise it is out of the module filter, see R9)
         and to `resolved_from_pip_id`.
         **If `dt_master$pip_id` is missing**: warn
         `cli::cli_alert_warning("Master inventory lacks pip_id column; pip_id resolution unavailable. All non-survey_id identifiers treated as unknown.")`
         and treat all pip_id inputs as unknown for this call.
      3. Else → add to `unknown`.
    - **Important**: the `pip_id` reverse-map reuses `dt_master` already loaded
      at `valid_dlw_load.R:109-122` when `!force`. Do NOT call
      `load_pip_master_inventory()` again. If `dt_master` is NULL (master load
      failed), pip_id inputs cannot be resolved and are treated as unknown
      (warn + log + skip); pure survey_id inputs still work because they do
      not need the master. **When `dt_master` is NULL and `force_surveys`
      contains identifiers that did NOT match any `survey_id` in
      `inv_svy_full`**, emit a specific warning (P3 finding):
      `cli::cli_alert_warning("Master inventory unavailable; pip_id resolution skipped. All non-survey_id identifiers treated as unknown.")`
      — this distinguishes "your IDs are wrong" from "the master couldn't
      load, so pip_id resolution is impossible".
  - **Ordering constraint** (P2 finding): `forced_inv` must be computed
    BEFORE the nothing-to-clean abort at line 280, not after the `rbind` at
    line 291. Compute `forced_inv <- inv_svy_full[survey_id %in% resolved_survey_ids]`
    early — after `inv_svy` is computed (from `inv_to_process()`, line 128)
    and before line 280. An intersection with `inv_svy_full` applies module
    filtering + `last_ver_inv` for free (R9).
  - Update the nothing-to-clean abort (lines 280-288): the condition must also
    check `forced_inv`. Concretely, the abort should fire only when
    `inv_svy`/`inv_aux` are empty AND `forced_inv` is empty (R7). The
    `forced_inv` emptiness check must be `(is.null(forced_inv) || nrow(forced_inv) == 0)`.
  - Union the forced rows into the candidate set: after the existing
    `inv_to_clean <- rbind(inv_svy, inv_aux, fill = TRUE)` at line 291, add
    `inv_to_clean <- rbind(inv_to_clean, forced_inv, fill = TRUE)`. The
    existing `unique(inv_to_clean)` at line 294 deduplicates overlaps (R2).
    **Verify no `.joyn` column leaks** — `forced_inv` is a plain
    data.table-subset, so this is safe (see
    `.cg-docs/solutions/data-quality/2026-06-05-joyn-diagnostic-column-discipline.md`).
- **Test scenarios**:
  - happy path: `force_surveys = "COL_2020_GEIH"` where the survey is
    already-cleaned (identical content_hash) is retained in the candidate set;
  - happy path: `force_surveys = "COL_2020_GEIH"` where the survey is new —
    already in `inv_svy`; union dedups;
  - pip_id path: `force_surveys = "SomePipId"` reverse-maps to a survey in
    `inv_svy_full` (master mock must include `pip_id` column);
  - unknown path: `force_surveys = "NOPE_9999_FOO"` warns + logs + skips, does
    not abort;
  - out-of-module: a survey_id in `force_surveys` that is NOT in
    `inv_svy_full` (e.g. wrong module) is excluded; warn if none match;
  - forced-only: `inv_svy` empty, `inv_aux` empty, `forced_inv` non-empty →
    does NOT abort (P2 ordering fix: `forced_inv` computed before the abort);
  - aux overlap: a forced survey that is also an aux candidate is deduped by
    `unique()`.
  - duplicate input (P2 finding): `force_surveys = c("COL_2020_GEIH", "COL_2020_GEIH")`
    → resolver deduplicates; `n_forced = 1` in logmeta, not 2;
  - type validation (P3 finding): `force_surveys = 42` (numeric) → aborts
    with class `piperr` at entry, not a silent no-op;
  - pip_id column missing (P2 finding): `dt_master` has no `pip_id` column →
    warns "pip_id resolution unavailable"; pip_id inputs treated as unknown;
  - master unavailable + pip_id (P3 finding): `dt_master` is NULL and
    force_surveys contains a pip_id-like input (not matching any survey_id) →
    warns "Master inventory unavailable; pip_id resolution skipped";
  - direct-call guard (P1 finding): `valid_dlw_load()` called directly with
    `force = TRUE, force_surveys = "X"` → aborts with class `piperr`.
- **Tests**: `tests/testthat/test-valid_dlw_load.R`.
- **Acceptance criteria**: forced surveys appear in the returned
  `inv_to_clean`; unknown identifiers do not abort; the nothing-to-clean abort
  respects the forced set; no duplicate survey_ids in output.

### 3. Add `force_surveys_inf` and `force_surveys_unknown_inf` logging entries

- **Requirements**: R8
- **Files**: `R/valid_dlw_load.R`, `tests/testthat/test-valid_dlw_load.R`
- **Details**:
  - After resolving `force_surveys` (Step 2), emit a `force_surveys_inf`
    `pipfun::log_info()` entry when `resolved_survey_ids` is non-empty:
    `logmeta = list(info = "force_surveys_inf", n_forced = length(resolved_survey_ids), surveys_forced = resolved_survey_ids, n_from_survey_id = length(resolved_from_survey_id), n_from_pip_id = length(resolved_from_pip_id))`.
  - Emit a `force_surveys_unknown_inf` entry when `unknown` is non-empty:
    `logmeta = list(info = "force_surveys_unknown_inf", unknown_identifiers = unknown)`.
  - Also emit a `cli::cli_alert_info` (gated on `verbose`) summarizing the
    forced count and any unknown identifiers, matching the existing verbose
    pattern in the file.
  - If no `force_surveys` was supplied, emit nothing (no spurious log entries).
- **Test scenarios**:
  - `force_surveys` supplied with all-resolvable IDs → `force_surveys_inf`
    logged, `force_surveys_unknown_inf` NOT logged;
  - mix of resolvable + unknown → both entries logged with correct counts;
  - `force_surveys = NULL` → neither entry logged;
  - pip_id-resolved identifiers increment `n_from_pip_id` not
    `n_from_survey_id`.
- **Tests**: `tests/testthat/test-valid_dlw_load.R` (extend existing logging
  contract tests; see
  `tests/testthat/test-logging-integration.R` for the logging-contract
  pattern).
- **Acceptance criteria**: log entries are emitted with the correct logmeta
  fields; absent when `force_surveys` is not supplied.

## Phase 2: Tests and documentation

### 4. Tests for `valid_dlw_load()` `force_surveys` behavior

- **Requirements**: R1, R2, R5, R6, R7, R8, R9, R10
- **Files**: `tests/testthat/test-valid_dlw_load.R`
- **Details**:
  - Use the existing `make_dlw_inv()` test helper (see the existing force-mode
    test at lines 905-948 for the mocking pattern:
    `testthat::local_mocked_bindings()` for `load_pip_master_inventory`,
    `valid_aux_load`, `aux_hash_candidates`).
  - **P2 finding**: existing test mocks for `load_pip_master_inventory` (e.g.
    `make_master_hash` at lines 61-66) only return `survey_id` +
    `content_hash_dlw` — they lack `pip_id`. Force_surveys tests that exercise
    the pip_id reverse-map must include `pip_id` in their master fragment mock.
  - Add tests for each test scenario listed in Steps 2 and 3, including the
    new scenarios for duplicate input dedup, type validation, pip_id column
    missing, master unavailable + pip_id, and direct-call guard (P1/P2/P3
    findings).
  - Add a regression test that `force = TRUE` still skips master/aux
    comparisons and processes all rows (R10) — the existing test at lines
    905-948 covers this; ensure it still passes after the signature change.
  - Add a test that the output has no duplicate `survey_id` values and no
    `.joyn` column (R2, mirroring the existing test at lines 954+).
  - Add a call-count test that `load_pip_master_inventory` is called exactly
    once (not twice) when `force_surveys` is supplied with a `pip_id` input —
    use a counter via `local_mocked_bindings` (C5).
- **Test scenarios**: see Steps 2 and 3.
- **Tests**: `tests/testthat/test-valid_dlw_load.R`.
- **Acceptance criteria**: all new tests pass; the existing force-mode
  regression test still passes.

### 5. Tests for `pd_process_data()` mutual-exclusivity and versioning preservation

- **Requirements**: R3, R4
- **Files**: `tests/testthat/test-pd_process_data.R` (check for existing file
  via `glob`; if none exists, create a minimal one focused on the guard).
- **Details**:
  - Test that `force = TRUE, force_surveys = "X"` aborts with
    `class = "piperr"` (and the error message mentions both parameters).
  - Test that `force = FALSE, force_surveys = "X"` does NOT call
    `stamp::st_opts()` — mock `stamp::st_opts` with a counter and assert it
    is never called (R3, C1).
  - Test that `force = TRUE` (no `force_surveys`) DOES call `st_opts` with
    `versioning = "timestamp"` — regression for the existing path.
  - If a full `pd_process_data()` integration test is too heavy (requires
    `setup_working_release`), scope the test to the guard + the
    `st_opts`-counter assertion by mocking `valid_dlw_load` and
    `stamp::st_opts` so the function exits at the guard / threading point.
- **Test scenarios**: see above.
- **Tests**: `tests/testthat/test-pd_process_data.R`.
- **Acceptance criteria**: mutual-exclusivity abort fires; `st_opts` is
  unreachable via `force_surveys`; `force = TRUE` still switches versioning.

### 6. Roxygen documentation updates

- **Requirements**: R5, R8
- **Files**: `R/pd_process_data.R`, `R/valid_dlw_load.R`
- **Details**:
  - Add `@param force_surveys` to `pd_process_data()` roxygen: describe it as
    a character vector of `survey_id` and/or `pip_id` values to
    re-process surgically; note it is mutually exclusive with `force`; note
    that it preserves content-based stamp versioning (unlike `force = TRUE`);
    note that unknown identifiers are warned + skipped.
  - Add `@param force_surveys` to `valid_dlw_load()` roxygen with the same
    semantics, plus the detail that forced surveys bypass `inv_to_process()`
    only and are unioned into the candidate set.
  - Update the `@details` block of `valid_dlw_load()` to document the
    `force_surveys` path: forced surveys are added to the candidate set,
    bypass `inv_to_process()`, are deduplicated via `unique()`, and emit
    `force_surveys_inf` / `force_surveys_unknown_inf` log entries.
  - Regenerate `.Rd` files via `devtools::document()` (do not run this in the
    plan; the `/cg-work` step will run it).
  - Update the `@param force` roxygen in `pd_process_data()` to cross-reference
    `force_surveys` (e.g. "For surgical re-processing without the global
    versioning side effect, see `force_surveys`.").
- **Test scenarios**: n/a (documentation).
- **Tests**: n/a.
- **Acceptance criteria**: `devtools::document()` succeeds; `?pd_process_data`
  and `?valid_dlw_load` show the new parameter and the mutual-exclusivity
  note.

## Testing Strategy

- Use `testthat::local_mocked_bindings()` for
  `pipload::load_pip_master_inventory`, `valid_aux_load`,
  `aux_hash_candidates`, and `stamp::st_opts` (for the versioning
  preservation test). Follow the existing mocking pattern in
  `tests/testthat/test-valid_dlw_load.R:905-948`.
- Use `make_dlw_inv()` (test helper) to build synthetic DLW inventories.
- Build a synthetic master inventory `data.table` with `survey_id`,
  `content_hash_dlw`, and `pip_id` columns for the pip_id reverse-map test.
- Run targeted tests after each phase:
  `devtools::test(filter = "valid_dlw_load")` and
  `devtools::test(filter = "pd_process_data")`.
- Run `devtools::test()` as the final regression gate.
- Verify `devtools::document()` regenerates `.Rd` files without error.

## Documentation Checklist

- Update `valid_dlw_load()` roxygen: new `@param force_surveys`, `@details`
  for the forced-survey path, logging entries.
- Update `pd_process_data()` roxygen: new `@param force_surveys`,
  cross-reference from `@param force`, mutual-exclusivity note.
- Regenerate affected `.Rd` files via `devtools::document()`.
- Do NOT update `compound-gpid.context.md` unless the canonical logging
  semantics or inventory schema change (they do not in this plan).

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| `pip_id` reverse-map is non-unique in practice (one pip_id → multiple survey_ids) | The master has one row per `(survey_id, pip_id)` per the aux-content-hash solution; add a call-count + uniqueness assertion test; abort if non-unique rather than silently picking one |
| Forced-survey union introduces a `.joyn` column that breaks `unique()` dedup | `forced_inv` is a plain `inv_svy_full[survey_id %in% ...]` subset with no joins; add an output-schema assertion test (no `.joyn`, no `.x`/`.y`) mirroring the existing test at lines 954+ |
| Unknown identifier silently re-processes nothing when all IDs were typos | Warn + log `force_surveys_unknown_inf` with the unresolved list; if ALL forced identifiers are unknown AND normal candidates are empty, the existing nothing-to-clean abort still fires (acceptable — the user specified nothing valid) |
| `force = TRUE` path is accidentally altered by the `force_surveys` implementation | Keep the `force` stamp block and the `!force` guards untouched; add a regression test that `force = TRUE` still calls `st_opts` and skips master/aux |
| pip_id inputs require a master load even in a "forced" scenario | `force_surveys` is mutually exclusive with `force = TRUE` → the master is always already loaded when `!force`; add a call-count test that `load_pip_master_inventory` is called exactly once |
| `resolve_force_surveys` helper adds hidden complexity | Keep the helper internal (`@keywords internal`), well-named, and covered by tests; return a structured list so callers cannot misinterpret the resolution |
| `force_surveys` with `force = TRUE` is a user error that silently does the wrong thing | Hard abort with `class = "piperr"` and a message naming both parameters (Step 1); mirrored guard in `valid_dlw_load()` for direct callers (P1 finding) |
| `dt_master` lacks `pip_id` column (legacy master inventory from pre-`build_pip_inventory()` era) | Defensive column-existence check in `resolve_force_surveys`; warn "pip_id resolution unavailable" and treat pip_id inputs as unknown; ensure force_surveys test mocks include `pip_id` (P2 finding) |
| Nothing-to-clean abort fires before `forced_inv` is computed | Compute `forced_inv` BEFORE line 280 and add it to the emptiness check; the abort fires only when all three candidates (inv_svy, inv_aux, forced_inv) are empty (P2 finding) |
| Duplicate `force_surveys` entries inflate log counts | Deduplicate at the top of `resolve_force_surveys` via `unique(force_surveys)` before the resolution loop (P2 finding) |

## Out of Scope

- Changes to `force = TRUE` behavior (it stays the "nuclear" option).
- `st_opts` switching for `force_surveys` (content versioning is preserved).
- `pip_id` as a primary processing key for the per-survey loop (the loop stays
  keyed on `survey_id`).
- DAG / step-level invalidation (separate roadmap idea
  `step-level-invalidation-dag`).
- Changes to `build_pip_inventory()` (the assembler already replaces all rows
  for a reprocessed `survey_id` — no change needed).
- Accepting `force_surveys` as a regex/glob pattern (identifiers are exact
  matches only).
- `pd_aux_attr()` or metadata attribute changes.

## Completion Contract

### Outcome

`pd_process_data()` accepts a `force_surveys` character vector that
re-processes the named surveys (by `survey_id` or `pip_id`) alongside the
normal invalidation candidates, while preserving content-based stamp versioning
and leaving `force = TRUE` semantics unchanged. `valid_dlw_load()` resolves
identifiers, unions forced surveys into the candidate set bypassing only
`inv_to_process()`, updates the nothing-to-clean abort to account for the
forced set, and logs the resolution for audit.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Phase | Required |
|----|-------------------|------------------|-------|----------|
| V1 | Forced survey already-cleaned (identical content_hash) is retained in candidate set | `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V2 | Forced + normal candidates union and dedup via `unique()` | `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V3 | `force=TRUE` + `force_surveys` both supplied → `cli_abort(class="piperr")` | `tests/testthat/test-pd_process_data.R` | 2 | yes |
| V4 | `pip_id` input reverse-mapped to `survey_id` via master inventory | `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V5 | Unknown identifier (neither survey_id nor pip_id) → warn + log + skip, no abort | `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V6 | Forced-only run (empty normal candidates) does NOT hit the nothing-to-clean abort | `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V7 | Stamp versioning stays on content (no `st_opts` call) when only `force_surveys` set | `tests/testthat/test-pd_process_data.R` | 2 | yes |
| V8 | `force_surveys_inf` and `force_surveys_unknown_inf` log entries emitted with correct logmeta | `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V9 | Forced survey outside module filter excluded; warn if none match filtered inventory | `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V10 | No duplicate survey_ids or `.joyn` columns in output | Output assertions in `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V11 | `force=TRUE` behavior unchanged (skips master/aux, processes all) | Regression test in `tests/testthat/test-valid_dlw_load.R` | 2 | yes |
| V12 | Direct-call guard: `valid_dlw_load(force=TRUE, force_surveys="X")` aborts with class `piperr` | `tests/testthat/test-valid_dlw_load.R` (P1 finding) | 2 | yes |
| V13 | Duplicate `force_surveys` input deduplicated before resolution; `n_forced=1` in logmeta | `tests/testthat/test-valid_dlw_load.R` (P2 finding) | 2 | yes |
| V14 | Non-character `force_surveys` input (numeric) aborts with class `piperr` | `tests/testthat/test-valid_dlw_load.R` (P3 finding) | 2 | yes |
| V15 | `dt_master` lacks `pip_id` column → warns and treats pip_id inputs as unknown | `tests/testthat/test-valid_dlw_load.R` (P2 finding) | 2 | yes |
| V16 | `dt_master` NULL + pip_id-like input → warns "pip_id resolution skipped" | `tests/testthat/test-valid_dlw_load.R` (P3 finding) | 2 | yes |
| V17 | Targeted tests pass | `devtools::test(filter="valid_dlw_load")` | final | yes |
| V18 | Full suite passes | `devtools::test()` | final | yes |
| V19 | Roxygen docs updated for both functions; `devtools::document()` succeeds | Documentation review | final | yes |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | `force_surveys` never calls `stamp::st_opts()`; only `force=TRUE` may switch versioning | Code review + `st_opts` counter test |
| C2 | Forced surveys bypass `inv_to_process()` only; aux detection runs normally | Test |
| C3 | `force=TRUE` + non-NULL `force_surveys` is a hard error (class `piperr`) | Test |
| C4 | Identifier resolution is lookup-first (survey_id membership → pip_id reverse-map), never pattern matching | Code review |
| C5 | pip_id reverse-map reuses the already-loaded master; no extra `load_pip_master_inventory()` call | Call-count test |
| C6 | No `.joyn`/`.x`/`.y` columns in output; `unique()` dedup unaffected | Output assertions |
| C7 | Nothing-to-clean abort includes the forced set in its emptiness check | Test |
| C8 | `force=TRUE` path is not altered by the `force_surveys` implementation | Regression test |
| C9 | Mutual-exclusivity guard exists in BOTH `pd_process_data()` AND `valid_dlw_load()` (exported, direct-callable) | P1 finding; code review + direct-call test |
| C10 | Non-character `force_surveys` input aborts with `class = "piperr"` | P3 finding; type-validation test |
| C11 | `dt_master` lacking `pip_id` column is handled gracefully (warn + unknown) | P2 finding; defensive-column test |

### Boundaries

- **Allowed**: `force_surveys` parameter on `pd_process_data()` and
  `valid_dlw_load()`; `resolve_force_surveys()` internal helper; candidate-set
  union via `rbind`/`unique`; logging entries; roxygen updates; tests.
- **Out of scope**: changes to `force = TRUE` behavior; `st_opts` switching for
  `force_surveys`; `pip_id` as a primary processing key; DAG/step-level
  invalidation; changes to `build_pip_inventory()`; regex/glob pattern
  matching for `force_surveys`; `pd_aux_attr()` or metadata attribute changes.

### Iteration Policy

1. Implement `pd_process_data()` param + guard, thread through to
   `valid_dlw_load()`.
2. Implement `valid_dlw_load()` identifier resolution helper + candidate
   union + nothing-to-clean abort fix.
3. Add `force_surveys_inf` and `force_surveys_unknown_inf` logging entries.
4. Write tests for `valid_dlw_load()` `force_surveys` behavior.
5. Write tests for `pd_process_data()` mutual-exclusivity + versioning
   preservation.
6. Update roxygen; run `devtools::document()`; run targeted then full test
   suite.
7. Stop and consult the user if the master inventory's `pip_id`→`survey_id`
   mapping proves non-unique in practice.

### Blocked-Stop Conditions

- The `pip_id`→`survey_id` reverse-map is non-unique (one `pip_id` maps to
  multiple `survey_id`s) in the master inventory — abort the reverse-map
  resolution loudly rather than silently picking one.
- Forced-survey union introduces duplicate `survey_id`s that `unique()` cannot
  resolve due to column-schema drift (e.g. a `.joyn` column leaking).
- The `force = TRUE` path is altered by the `force_surveys` implementation
  (e.g. the `!force` guards no longer hold).
