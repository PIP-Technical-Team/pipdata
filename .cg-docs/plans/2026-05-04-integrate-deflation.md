---
date: 2026-05-04
title: "Integrate pd_deflation.R into the active pipeline"
status: active
completed-date: null
scope: "Standard"
brainstorm: null
language: R
estimated-effort: medium
tags: [deflation, pipeline, architecture, integration, refactoring]
---

# Plan: Integrate pd_deflation.R into the Active Pipeline

## Objective

Make `pd_deflation()` usable as a self-contained single-survey helper: given
one cleaned survey `data.table` (or its `pip_id` + stamp version), it
internally loads the corresponding metadata (CPI, PPP, pop) and returns the
same survey enriched with deflated welfare vectors. Also extract a shared
`safe_deflation()` helper from the duplicated `deflation.pipmd()`/
`deflation.pipgd()` tryCatch structure.

Iteration over many surveys (batch deflation) is intentionally **out of scope**
and belongs to the future `pd_deflate_pipeline()` wrapper.

## Context

`pd_deflation()` is currently exported, buildable, and documented (see
`2026-04-30-position-deflation.md`) but is not called by any active
pipeline wrapper. The `.logenv` migration is complete (env-setup-v2) — it
already uses `pd_env_set()`/`pd_env_get()`/`pd_env_rm()`.

**Current pipeline flow** (`process_data()`):
1. Load DLW survey → `inv_dlw_load()`
2. Merge PFW → `pd_cpfw_merge()`
3. Clean variables → `pd_dlw_clean()` (includes `wbpip_clean`)
4. Create auxiliary metadata → `pd_aux_attr()`
5. Save cleaned data + metadata → `save_pip_data()`
6. Update master inventory → `update_pip_inventory()`

**Key insight**: `pd_aux_attr()` already computes and saves metadata that
contains CPI, PPP, and population values as attributes (filtered per survey,
stored via `save_pip_data(metadata, alias = "pip_meta")`). The master
inventory (written by `update_pip_inventory()`) maps each `pip_id` to its
corresponding data and metadata stamp versions. So given a `pip_id` +
optional data version, the deflation function can look up the matching
metadata version from the master inventory and load it automatically.

**Design decision — metadata format contract**: The metadata is saved as
named lists (named numeric vectors per aux variable, e.g.,
`meta$cpi = c("2017_national" = 87.2)`). This is intentional and will NOT
be changed. Instead, the deflation internals (`add_ppp()`, `add_cpi()`,
`adjust_population()`) will be rewritten to accept this named-vector
format directly. This is a one-time adaptation done when `pd_aux_attr()`'s
format was established — no per-load reconstruction is needed.

**Master inventory columns** (relevant subset):
- `survey_id`, `pip_id`
- Version metadata from stamp (version_id, content_hash, etc.) for both
  `_data` and `_metadata` suffixes
- `welfare_type`, `country_code`, `surveyid_year`, `survey_acronym`

The deflation function uses this inventory to resolve which metadata
corresponds to a given cleaned survey version.

**Interface design**:
- Input: either (a) a cleaned survey `data.table` (class `pipmd`/`pipgd`),
  or (b) a `pip_id` + optional stamp version, in which case the function
  loads the survey from stamp and looks up the corresponding metadata
  version from the master inventory
- Internally: uses the master inventory to resolve which metadata version
  matches the given data version, then loads CPI/PPP/pop from that metadata
- Output: same cleaned survey `data.table` with deflated welfare columns
  appended (e.g., `welfare_ppp_2017_0_1`, `welfare_ppp_2011_0_1`, etc.)

A full pipeline orchestrator (`pd_deflate_pipeline`) that iterates over an
entire inventory is a future idea, not part of this plan.

**Refactoring context**: `deflation.pipmd()` and `deflation.pipgd()` share
~40 lines of identical boilerplate (formals copy, environment set, tryCatch,
error handler). A `safe_deflation()` helper can encapsulate this.

## Requirements

| ID  | Requirement | Source |
|-----|-------------|--------|
| R1  | `pd_deflation()` can be called with just a cleaned survey (or `pip_id` + version) and returns the deflated version — no external aux args required | user |
| R2  | Uses the master inventory to resolve which metadata version corresponds to the given `pip_id`/data version, then loads CPI/PPP/pop from that metadata | user |
| R3  | Output is the same cleaned survey with deflated welfare columns appended | user |
| R4  | Supports two input modes: (a) pass the survey object directly, (b) pass `pip_id` + optional version to load it from stamp | user |
| R5  | Error handling: tryCatch around deflation — informative failure, not a crash | existing pattern |
| R6  | Extract `safe_deflation()` helper to deduplicate `deflation.pipmd()`/`deflation.pipgd()` | refactoring |
| R7  | Tests using self-contained fixtures (no network/file access) | roadmap |
| R8  | `pd_deflation()` `@note` updated to reflect active integration status | documentation |

## Implementation Steps

### 1. Define the input validation and metadata-loading helper

- **Requirements**: R2
- **Files**: `R/pd_deflation.R` (modify — add internal helpers)
- **Details**:
  Create two internal helpers:

  **`.validate_deflation_input(dt)`** — checks:
  - `dt` is a `data.table` with class `pipmd` or `pipgd`
  - Required columns exist: `welfare`, `weight`, `area`
  - Required attributes exist: `survey_id`, `country_code`, `survey_year`,
    `survey_acronym`, `reporting_level`, `ppp_data_level`, `cpi_data_level`
  - Abort with informative `cli::cli_abort()` + `piperr` class if invalid

  **`.load_deflation_aux(pip_id, version = NULL)`** — uses the master
  inventory to find the metadata version that corresponds to the given
  `pip_id` + data version, loads it from stamp, and extracts CPI, PPP, pop:
  ```r
  .load_deflation_aux <- function(pip_id, version = NULL) {
    # Load master inventory
    inv <- pipload::load_pip_master_inventory()

    # Find the row matching this pip_id (optionally filtered by data version)
    row <- inv[pip_id == pip_id]
    if (!is.null(version)) {
      row <- row[version_id_data == version]
    } else {
      # Use the latest entry for this pip_id
      row <- row[1L]  # or pick by most recent version
    }

    # Extract the metadata version that was saved alongside this data version
    meta_version <- row$version_id_metadata

    # Load the metadata from stamp using that version
    meta <- pipload::pip_read(
      id = pip_id,
      alias = "pip_meta",
      version = meta_version
    )

    # Return named-vector metadata directly (no reconstruction needed)
    list(cpi = meta$cpi, ppp = meta$ppp, pop = meta$pop)
  }
  ```
  The exact column names for version IDs in the master inventory need
  verification (comes from `format_vrs()` output — likely `version_id_data`
  and `version_id_metadata` after the join with `_data`/`_metadata` suffixes).

  The returned `cpi`, `ppp`, `pop` are named numeric vectors (as produced
  by `pd_aux_attr()`). The deflation internals (`add_ppp()`, `add_cpi()`,
  `adjust_population()`) will be rewritten in Step 3 to accept this format
  directly — no data.table reconstruction is needed.

  **Design decision**: The cleanest approach is to adapt `pd_deflation()`
  to accept either:
  - (a) Raw auxiliary tables (legacy interface: `cpi`, `ppp`, `pop` args), OR
  - (b) `NULL` for those args, which triggers inventory-based metadata loading

  This preserves backward compatibility while enabling the metadata-driven
  flow.
- **Tests**: Unit tests for valid input, missing column, missing attribute,
  wrong class; unit test for `.load_deflation_aux()` with mocked
  `pipload::load_pip_master_inventory()` and `pipload::pip_read()`
- **Acceptance criteria**: Helper runs before each survey's deflation;
  malformed inputs produce clear errors instead of cryptic downstream failures.
  Metadata loading correctly resolves the matching version from the master
  inventory.

### 2. Refactor `pd_deflation()` interface to support metadata-driven flow

- **Requirements**: R1, R2, R3, R4
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  Update `pd_deflation()` signature to:
  ```r
  pd_deflation <- function(lf = NULL, cpi = NULL, ppp = NULL, pop = NULL,
                           pip_id = NULL, version = NULL) {
    # Mode A: lf provided directly (data.table or list of data.tables)
    # Mode B: pip_id provided → load from stamp
    # In both modes: if cpi/ppp/pop are NULL, use master inventory
    #   to find matching metadata and load CPI/PPP/pop from it
    ...
  }
  ```
  **Mode A** — caller passes `lf` (a cleaned survey or list of surveys):
  1. For each element, extract `pip_id` from the name (or from attributes)
  2. Call `.load_deflation_aux(pip_id)` — looks up master inventory to find
     the corresponding metadata version, loads it, extracts CPI/PPP/pop
  3. Pass to `deflation()` S3 method

  **Mode B** — caller passes `pip_id` (+ optional `version`):
  1. Load cleaned survey from stamp: `pipload::pip_read(id = pip_id, alias = "pip", version = version)`
  2. Use master inventory to find the metadata version matching this
     `pip_id`/`version` combination
  3. Proceed as Mode A

  **Backward-compatible**: if `cpi`/`ppp`/`pop` are explicitly provided,
  use them directly (legacy behavior).

  The output is the **same cleaned survey** with deflated welfare columns
  appended. The original `welfare` column is preserved as `welfare_lcu`.
- **Tests**: Test with explicit aux tables (existing behavior), with
  NULL + mocked inventory/metadata (new behavior), and with `pip_id` input
  (load path via master inventory)
- **Acceptance criteria**: All three calling patterns work; output has
  `welfare_lcu` plus `welfare_ppp_*` columns alongside original columns.
  Master inventory lookup correctly resolves data→metadata version mapping.

### 3. Extract `safe_deflation()` helper

- **Requirements**: R6
- **Files**: `R/pd_deflation.R` (modify)
- **Details**:
  The duplicated structure in `deflation.pipmd()` and `deflation.pipgd()` is:
  ```r
  # 1. Copy formals (for loop)
  # 2. pd_env_set("log_survey_id", ...)
  # 3. on.exit(pd_env_rm("log_survey_id"))
  # 4. tryCatch(expr = { ... }, error = function(cnd) { ... })
  ```
  Extract into:
  ```r
  #' @noRd
  safe_deflation <- function(dt, cpi, ppp, pop, deflation_fn) {
    # Copy args that are data.tables
    dt_c  <- copy(dt)
    cpi   <- copy(cpi)
    ppp   <- copy(ppp)
    pop   <- if (inherits(pop, "data.table")) copy(pop) else qDT(pop)

    pd_env_set("log_survey_id", attributes(dt_c)$survey_id$values)
    on.exit(pd_env_rm("log_survey_id"))

    tryCatch(
      expr = deflation_fn(dt_c, cpi, ppp, pop),
      error = function(cnd) {
        survey_id <- pd_env_get("log_survey_id")
        cli::cli_alert("The survey {survey_id} was skipped")
        log_failure(cnd)
        NA
      }
    )
  }
  ```
  Then `deflation.pipmd()` and `deflation.pipgd()` become thin wrappers:
  ```r
  deflation.pipmd <- function(dt, cpi, ppp, pop, ...) {
    safe_deflation(dt, cpi, ppp, pop, .deflation_pipmd_core)
  }
  ```
  Where `.deflation_pipmd_core()` contains the actual logic
  (add_rep_lvl → add_aux → welfare_lcu → deflate_wlf → adjust_population →
  char_to_fct).
- **Tests**: Existing `test-adjust-population.R` must still pass; add a
  unit test that `safe_deflation()` catches errors and returns `NA`.
- **Acceptance criteria**: Zero code duplication between the two S3 methods
  for the boilerplate scaffolding; logic unchanged.

### 4. Write test fixtures and integration tests

- **Requirements**: R7
- **Files**: `tests/testthat/test-pd-deflation.R` (new),
  `tests/testthat/fixtures/` (fixture files if needed)
- **Details**:
  - Create minimal fixture: a tiny `pipmd` data.table with required columns
    and attributes (survey_id, country_code, etc.), plus matching metadata
    fixture (named list with cpi/ppp/pop attributes as produced by
    `pd_aux_attr()`).
  - Test happy path (Mode A): pass fixture directly with NULL aux →
    produces non-NA result with `welfare_lcu` + `welfare_ppp_*` columns;
    original columns preserved.
  - Test happy path (Mode B): pass `pip_id` → mock `pipload::pip_read`
    and `pipload::load_pip_master_inventory()` to return fixture, verify
    correct metadata version resolution and same output.
  - Test legacy mode: pass explicit cpi/ppp/pop tables → same behavior
    as before.
  - Test error path: fixture with missing required attribute → caught
    gracefully, returns NA, emits log entry.
  - Test metadata-loading: mock `pipload::pip_read("pip_meta")` to return
    fixture metadata, verify correct CPI/PPP/pop extraction.
- **Acceptance criteria**: All tests pass; fixture is self-contained
  (no network/file access).

### 5. Update documentation and `@note`

- **Requirements**: R8
- **Files**: `R/pd_deflation.R`, `man/pd_deflation.Rd`
- **Details**:
  - Update the `@note` to reflect that `pd_deflation()` is now actively
    usable as a self-contained deflation helper.
  - Update `@param` docs: document `cpi`/`ppp`/`pop` NULL defaults and
    new `pip_id`/`version` args
  - Add `@family pd_process_data pipeline` tag
  - Run `devtools::document()`
- **Acceptance criteria**: `@note` no longer says "not yet integrated";
  parameter documentation is complete.

## Testing Strategy

| Layer | What | How |
|-------|------|-----|
| Unit | `.validate_deflation_input()` | Direct calls with valid/invalid fixtures |
| Unit | `.load_deflation_aux()` | Mock `pipload::load_pip_master_inventory()` + `pipload::pip_read()` to return fixture; verify inventory lookup resolves correct metadata version and CPI/PPP/pop extraction |
| Unit | `safe_deflation()` error handling | Inject a function that aborts; verify NA return + log |
| Unit | Core logic (existing helpers) | `test-adjust-population.R` already covers `adjust_population()` |
| Integration | `pd_deflation()` Mode A (NULL aux) | Mock inventory + metadata load; verify deflated output structure |
| Integration | `pd_deflation()` Mode B (pip_id) | Mock `pipload::pip_read` for survey + inventory + metadata; verify same output |
| Round-trip | Attribute preservation | Save fixture via stamp, reload, verify class and custom attributes survive |

## Documentation Checklist

- [ ] `safe_deflation()` has `@noRd` with `@param`/`@return`
- [ ] `.validate_deflation_input()` has `@noRd` documentation
- [ ] `.load_deflation_aux()` has `@noRd` documentation
- [ ] `pd_deflation()` `@note` updated to "actively usable"
- [ ] `pd_deflation()` `@param` updated to document NULL defaults + `pip_id`/`version` args
- [ ] Inline comments explaining master inventory lookup → metadata version resolution pattern

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Deflation internal rewrite: `add_ppp()`/`add_cpi()`/`adjust_population()` currently expect full `data.table` inputs but metadata is stored as named numeric vectors | Rewrite these internals to accept the named-vector format directly (design decision: metadata format is authoritative, deflation adapts). Done once — no per-load transformation needed |
| Master inventory unavailable or stale: `load_pip_master_inventory()` may fail if stamp storage is inaccessible | Wrap in tryCatch; abort with informative error ("Run pd_process_data first") |
| Version column names: `format_vrs()` joins produce suffixed column names (`_data`, `_metadata`) that may vary across releases | Verify exact names from a real inventory; add assertion in `.load_deflation_aux()` |
| Attribute loss in stamp round-trip: `pipload::pip_read()` may not preserve all custom attributes (class, survey_id, etc.) | Test round-trip explicitly; if attributes lost, re-attach from inventory metadata after load |
| `joyn::merge` / `joyn::inner_join` recycling warnings (known issue in joyn 0.3.0) | Use `suppressMessages()` or switch to data.table `[.data.table` joins if warnings persist |
| `ppp_to_wide()` uses `dcast` without qualifying `data.table::dcast` — potential masking | Qualify with `data.table::dcast()` during refactoring |

## Out of Scope

- `pd_deflate_pipeline()` — a full pipeline wrapper that iterates over an
  entire inventory, saves deflated outputs, and logs summary entries.
  Tracked as a separate roadmap idea.
- Rewriting `pd_deflation()` internal logic (PPP/CPI merge mechanics)
- Saving deflated outputs to stamp (no alias registration needed yet)
- Creating the new orchestration script (tracked as `new-orchestration-script`
  in roadmap)
- The `unified-logging-report` plan (DLW/pipeline log harmonization —
  separate effort)
- Performance optimization of the formals-copy loop pattern (already flagged
  in loop-to-apply as "keep: metaprogramming") — partially addressed by
  `safe_deflation()` but not fully eliminated
- Changing the metadata structure produced by `pd_aux_attr()` — the
  named-list format is authoritative; deflation internals adapt to it
