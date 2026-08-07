---
date: 2026-08-06
title: "Aux version gate for valid_dlw_load — revised run-level hash design"
status: completed
completed-date: 2026-08-07
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-08-05-aux-version-gate-valid-dlw-load.md"
prior-plan: ".cg-docs/plans/2026-08-05-aux-version-gate-valid-dlw-load.md"
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
tags: [valid_dlw_load, aux, stamp, content_hash, master-inventory, incremental-processing]
phases: 2
completed-phases: [1, 2]
execution-report: ".cg-docs/work-reports/2026-08-06-aux-version-gate-valid-dlw-load-revised.md"
---

# Plan: Aux version gate for `valid_dlw_load()` — revised run-level hash design

## Objective

Prevent unnecessary survey re-cleaning by comparing the current content hash of
each requested auxiliary artifact with the hash recorded for previously cleaned
surveys, while retaining the detailed `pipaux::compare_aux_*` filter that
identifies which requested surveys were actually affected.

## Context

`valid_dlw_load()` currently invokes `valid_aux_load()` on every run. The
comparison functions detect changes within auxiliary data, but the pipeline has
no gate showing whether the current auxiliary table is the same table already
used for prior cleaning.

The confirmed design is deliberately limited:

- Resolve current hashes once from `stamp::st_catalog_query(alias = "aux")`.
- Use the catalog `content_hash`; do not call `st_latest()`.
- Include every requested measure, including `pfw`.
- Keep the run-level hash map in memory and pass it explicitly through the run.
- Write hashes directly to master-inventory rows for successfully processed
  surveys; do not add hash attributes to `pip_meta` or read serialized metadata
  artifacts.
- Load the master inventory once in `valid_dlw_load()`.
- Retain both global hash gating and detailed `compare_aux_*` filtering.
- When `force = TRUE`, skip all comparisons and process every filtered/latest
  survey.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Resolve one current `content_hash` for every requested aux measure from the `aux` stamp catalog | User decision |
| R2 | Include all requested measures, including `pfw` | User decision |
| R3 | Resolve hashes once before aux data loading and pass the run-level map explicitly | User decision |
| R4 | Persist aux hash columns directly in master-inventory rows produced by the run | User decision |
| R5 | Load the master inventory at most once in `valid_dlw_load()` and share it with DLW and aux comparisons | User decision |
| R6 | Compare stored hashes against current hashes for previously cleaned surveys | Brainstorm/review |
| R7 | Run `valid_aux_load()` only for measures whose global hash changed | User decision |
| R8 | Intersect detailed aux changes with requested candidate surveys | User decision |
| R9 | Abort on conflicting aux hashes for the same `survey_id` and `content_hash_dlw` | User decision |
| R10 | Treat missing historical hashes as migration candidates | Review decision |
| R11 | Preserve new/DLW-content-changed survey processing | Existing behavior |
| R12 | `force = TRUE` bypasses master/hash/aux comparisons and processes all filtered/latest surveys | User decision |

## Phase 1: Resolve and persist run-level hashes

### 1. Add a run-level auxiliary hash resolver

- **Requirements**: R1, R2, R3
- **Files**: `R/pd_process_data.R`; likely a new internal helper in
  `R/valid_dlw_load.R` or `R/utils.R`.
- **Details**:
  - Query `stamp::st_catalog_query(alias = "aux")` once.
  - For every value in `aux_measures`, match exactly the artifact whose path
    is the measure basename with `.qs2`, such as `cpi.qs2`, `ppp.qs2`,
    `pfw.qs2`, `pop.qs2`, `gdp.qs2`, or `pce.qs2`.
  - Return a named character vector or data.table keyed by measure containing
    the `content_hash` and, if useful for provenance, the matched path and
    catalog `version_id`.
  - Abort loudly when the aux alias is unavailable, a requested artifact is
    missing, or multiple rows match a measure.
  - Resolve the map before `lapply(aux_measures, pipload::load_aux_data, ...)`.
  - Do not call `st_latest()` and do not compute hashes from loaded tables.
- **Test scenarios**:
  - all six default measures resolve, including PFW;
  - duplicate or missing catalog artifact aborts;
  - an arbitrary subset of `aux_measures` resolves only those measures;
  - catalog query failure aborts clearly.
- **Tests**: extend `tests/testthat/test-valid_dlw_load.R` or add a focused
  resolver test file.
- **Acceptance criteria**: one deterministic current `content_hash` exists for
  every requested measure before aux data loading begins.

### 2. Thread hashes into `build_pip_inventory()` and persist them

- **Requirements**: R3, R4, R10
- **Files**: `R/pd_process_data.R`, `R/build_pip_inventory.R`,
  `tests/testthat/test-build_pip_inventory.R`.
- **Details**:
  - Pass the run-level hash map to `build_pip_inventory()` explicitly.
  - Add columns such as `aux_cpi_hash`, `aux_ppp_hash`, `aux_pfw_hash`,
    `aux_pop_hash`, `aux_gdp_hash`, and `aux_pce_hash` to rows produced for
    successful current-run surveys.
  - Preserve old master rows unchanged when they are not reprocessed.
  - Ensure missing columns in legacy master inventories are filled with `NA`
    and do not cause `collapse::rowbind(..., fill = TRUE)` class conflicts.
  - If the assembler is called without a hash map by an existing caller,
    define and test the compatible behavior rather than silently inventing
    hashes.
  - Do not add these values to `pd_aux_attr()` or `pip_meta`.
- **Test scenarios**:
  - current-run rows receive all supplied hashes;
  - old retained rows remain intact;
  - missing historical hash columns migrate safely;
  - partial requested measures leave non-requested measure columns as `NA`.
- **Acceptance criteria**: reloading the master inventory shows the expected
  hashes on successfully processed current-run rows.

## Phase 2: Gate and filter aux changes

### 3. Load master once and implement two-stage aux filtering

- **Requirements**: R5, R6, R7, R8, R9, R11, R12
- **Files**: `R/valid_dlw_load.R`, `tests/testthat/test-valid_dlw_load.R`.
- **Details**:
  - Define an explicit master-loading handoff. Load the master once in
    `valid_dlw_load()` and pass the same object to both the DLW comparison
    helper and the aux-hash comparison helper. Refactor `inv_to_process()` as
    needed so it accepts the supplied master rather than loading it again.
  - Preserve the existing master-load fallback semantics explicitly. If the
    master cannot be loaded, do not claim previously cleaned surveys are
    unchanged; treat the available DLW inventory as needing processing and
    define the aux candidate behavior in tests.
  - Apply module filtering and `last_ver_inv()` before survey-level comparison.
  - Reduce master rows to one row per `survey_id` for the same
    `content_hash_dlw`. Require all relevant rows in that group to have equal
    aux hashes; abort on conflict. This protects the invariant that split
    `pip_id`s for one survey/content version use the same aux versions.
  - Stage 1: for each requested measure, compare the survey-level stored hash
    to the current run's catalog hash. A mismatch or missing historical hash
    makes the survey a candidate; new/DLW-content-changed surveys remain on
    the normal `inv_to_process()` path.
  - Stage 2: call `valid_aux_load(measure = changed_measures, compare = "all")`
    only when at least one requested measure has a changed global hash. Use
    `filter_aux_inv()` and intersect its affected survey IDs with the
    requested candidate inventory. This preserves the distinction between a
    globally changed table and changed rows for requested countries/surveys.
  - Preserve existing logging discriminators with revised meanings:
    `aux_no_changes_inf` when no requested global hash changed,
    `aux_changes_no_surveys_inf` when changed measures produce no affected
    requested survey, `aux_changes_inf` when the intersection is non-empty,
    and `surveys_to_clean_inf` after final deduplication.
  - When `force = TRUE`, do not load the master for comparison, resolve/use
    no comparison candidates, and do not call `valid_aux_load()`; process all
    filtered/latest surveys. The current run's hashes are still persisted by
    Phase 1.
  - Ensure all production joins retain `reportvar = FALSE` and that verbose
    values propagate to downstream I/O.
- **Test scenarios**:
  - unchanged hash skips that measure's aux comparison;
  - changed hash invokes comparison only for that measure;
  - changed CPI affecting USA/GER but not requested COL/ARG produces no COL/ARG
    re-cleaning;
  - changed CPI affecting a requested survey returns that survey;
  - new and DLW-content-changed surveys remain processable;
  - missing historical hash is a candidate but still must pass detailed
    affected-row filtering;
  - conflicting hashes for one survey/content-hash group abort;
  - master loader is called once;
  - force mode skips master/hash/aux comparisons and processes all rows;
  - no `.joyn` column or duplicate survey IDs appear in output.
- **Acceptance criteria**: only requested surveys with actual changed aux rows
  are added through the aux path, while new/DLW-changed surveys continue to be
  selected normally.

## Testing Strategy

- Use `testthat::local_mocked_bindings()` for `stamp::st_catalog_query`,
  `pipload::load_pip_master_inventory`, `valid_aux_load`, and the refactored
  `inv_to_process()` handoff.
- Add resolver tests for every supported measure and catalog ambiguity.
- Add assembler tests for hash propagation and retained legacy rows.
- Add integration-style gating tests for global hash change versus detailed
  country/survey change.
- Run targeted tests after each phase:
  `devtools::test(filter = "valid_dlw_load")` and
  `devtools::test(filter = "build_pip_inventory")`.
- Run `devtools::test()` as the final regression gate.

## Documentation Checklist

- Update `valid_dlw_load()` roxygen details to document the two-stage hash and
  detailed-change filter, one-time master loading, conflict abort, migration
  behavior, and force mode.
- Update `build_pip_inventory()` roxygen column provenance with aux hash
  columns and the run-level source.
- Update `pd_process_data()` roxygen to describe resolving aux catalog hashes
  once and passing them to inventory assembly.
- Update `compound-gpid.context.md` only if the canonical logging semantics or
  inventory schema notes change.
- Regenerate affected `.Rd` files with `devtools::document()`.

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Aux artifact path matching is ambiguous | Match exact `<measure>.qs2` basename; abort on zero or multiple matches; test all requested measures |
| Hash map is lost before inventory assembly | Pass it explicitly through `pd_process_data()` to `build_pip_inventory()`; verify reload round trip |
| Master inventory is loaded twice or under inconsistent fallback semantics | Centralize the load in `valid_dlw_load()` and pass the object into comparison helpers; add call-count tests |
| Split `pip_id`s contain conflicting aux hashes | Reduce by `survey_id`/`content_hash_dlw`; abort on conflict |
| Global aux change affects unrelated countries | Keep `valid_aux_load()` and intersect affected keys with the requested candidate inventory |
| Force mode is accidentally gated | Add regression test proving no master/aux comparison calls occur when `force = TRUE` |
| Existing masters lack aux hash columns | Fill missing columns with `NA`; treat missing hashes as migration candidates; test one-time behavior |

## Out of Scope

- Aux hash attributes in `pd_aux_attr()` or `pip_meta`.
- Reading serialized metadata artifacts to recover hashes.
- `st_latest()` or hashing loaded aux tables.
- Modifying `pipaux::compare_aux_*`.
- Deflation changes.
- Rich metadata distinguishing non-applicable rows from not-captured legacy rows.

## Completion Contract

### Outcome

The pipeline resolves current aux `content_hash` values once from the `aux`
stamp catalog, passes them through the run, and persists them directly in
master-inventory rows for successfully processed surveys. `valid_dlw_load()`
loads the master once and uses a global hash gate followed by detailed
`compare_aux_*` filtering; force mode processes all filtered/latest surveys
without comparison.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required |
|----|-------------------|------------------|----------|
| V1 | Exact current hash resolution for all requested measures, including PFW | Resolver tests | yes |
| V2 | Missing/ambiguous aux catalog artifacts abort | Resolver tests | yes |
| V3 | Run-level hashes resolved once before aux loading | `pd_process_data()` test | yes |
| V4 | Hashes persisted on successful current-run master rows | `build_pip_inventory()` test and reload | yes |
| V5 | Legacy master rows without hashes handled as migration candidates | Gating test | yes |
| V6 | Master inventory loaded exactly once | Call-count test | yes |
| V7 | Changed measures only invoke `valid_aux_load()` | Gating test | yes |
| V8 | Detailed aux changes intersect with requested surveys | COL/ARG versus USA/GER test | yes |
| V9 | Conflicting same-survey hashes abort | Validation test | yes |
| V10 | New and DLW-content-changed surveys remain selected | Regression test | yes |
| V11 | Force mode skips all comparison paths and processes all rows | Regression test | yes |
| V12 | No duplicate survey IDs or `.joyn` columns | Output assertions | yes |
| V13 | Targeted tests pass | `devtools::test(filter = "valid_dlw_load")` | yes |
| V14 | Full suite passes | `devtools::test()` | yes |
| V15 | Roxygen and context documentation reflect the revised behavior | Documentation review | yes |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | Use `st_catalog_query(alias = "aux")` `content_hash`; never `st_latest()` | Code review |
| C2 | Include every requested `aux_measures` value, including `pfw` | Tests |
| C3 | Do not add aux hash attributes to `pip_meta` | Code review |
| C4 | Load master inventory at most once in `valid_dlw_load()` | Call-count test |
| C5 | Abort on same-survey/content-hash aux conflicts | Test |
| C6 | Retain both global hash and detailed row-level filters | Integration test |
| C7 | `force = TRUE` bypasses comparisons and processes all filtered/latest surveys | Test |
| C8 | Production `joyn::` joins use `reportvar = FALSE` | Code review |
| C9 | External `pipaux` package is not modified | Diff review |

### Boundaries

- Allowed: run-level aux hash resolver, explicit hash propagation,
  master-inventory hash columns, single-load master handoff, two-stage aux
  filtering, conflict validation, tests, and documentation.
- Out of scope: metadata attributes, serialized metadata reads, `st_latest`,
  pipaux changes, deflation, and rich legacy applicability states.

### Iteration Policy

1. Implement and test the resolver.
2. Thread hashes into inventory assembly and verify reload persistence.
3. Refactor the single-master-load handoff and two-stage aux filtering.
4. Run targeted tests after each step.
5. Run the full suite before completion.
6. Stop and consult the user if the confirmed `aux` catalog layout or API does
   not hold in the configured working release.

### Blocked-Stop Conditions

- A requested measure has zero or multiple matching `<measure>.qs2` catalog rows.
- Hash propagation cannot be verified in the reloaded master inventory.
- Same-survey/content-hash rows contain conflicting aux hashes.
- `force = TRUE` still invokes master or aux comparison.
- Detailed aux results cannot be intersected reliably with requested surveys.
