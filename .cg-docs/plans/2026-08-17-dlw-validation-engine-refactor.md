---
date: 2026-08-17
title: "Replace 7 DLW validation functions with a single data-driven engine"
status: completed
completed-date: 2026-08-17
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-08-14-dlw-validation-engine-refactor.md"
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
phases: 2
tags: [validation, dlw, refactor, data-driven, yaml, data.validator]
---

# Plan: DLW Validation Engine Refactor

## Objective

Replace the 7 near-identical module validation functions in
`R/pipdata_dlw_validation.R` (`dlw_validation_gpwg`, `dlw_validation_group`,
`dlw_validation_bin`, `dlw_validation_hist`, `dlw_validation_all`,
`dlw_validation_aspire`, `dlw_validation_l`) plus `dlw_validation_skip` with a
single data-driven `dlw_validation_engine()` driven by a corrected
`inst/extdata/validation_spec.yml`. Output must be byte-identical to the legacy
functions (golden-test enforced via committed `.rds` fixtures), the report
format must stay compatible with `get_validation_report()`/`get_data_status()`,
and dead code must be removed.

## Context

`R/pipdata_dlw_validation.R` (~1,105 lines) defines 8 module validators that
share an identical 8-step skeleton and differ only in (a) which variable-name
regexes select the numeric / character / welfare / weight columns and (b) which
optional checks run (urban, gender, hhid/pid uniqueness, `welfare_type` in-set,
age range). The invariant per-variable chain is `is_numeric →
is_greaterthanzero → validate_cols(not_na) → validate_rows(num_row_NAs,
within_bounds)`, with `error_append` on the NA-threshold check for
weight/welfare and `warning_append` elsewhere.

A declarative `inst/extdata/validation_spec.yml` (483 lines, `schema_version:
1.0`) already exists but is **not consumed by any code** and is **not a faithful
mirror** of the R logic. Gaps confirmed by reading both files (and surfaced in
the 2026-08-17 plan review):

1. **Selection semantics**: R selects columns with `grep()` regex (e.g.
   `^year$|hsize$|welfshprosperity$`); the YAML uses exact-name `variables:`
   lists in several modules. `aspire` selects weights with `grep("hhweight$")`
   (ends-with) in R but the YAML says `^hhweight` (starts-with).
2. **Selection mechanism is split**: availability checks use `startsWith()`
   (literal prefix) via `is_var_startwith_avail`; numeric/character/group loops
   use `grep()` regex. The YAML conflates these under one `pattern:` field.
3. **Severity is per-check, not per-block** — and most checks are helper-fixed:
   the weight/welfare loop uses `warning_append` for `not_na` but `error_append`
   for the NA-threshold check; the character loop uses `error_append` in
   group/hist but `warning_append` in bin. But the check *helpers* hardcode their
   own `error_fun` (`is_numeric`, `is_character`, `is_greaterthanzero`,
   `check_urban`, `check_gender`, `is_valuebtwn0and110` → `warning_append`;
   `is_greaterequale0`, `is_var_avail`, `is_var_startwith_avail` →
   `error_append`). Only `validate_cols()`/`validate_rows()` accept an explicit
   `error_fun =`. So a spec `severity` field is honored **only** for the
   `not_missing` and `na_threshold` checks; it is **inert** on helper-based
   checks.
4. **hhid/pid is three checks with distinct gating**: `not_na(hhid)` runs if
   `hhid` is present (even if `pid` absent); `not_na(pid)` and `is_uniq(hhid,
   pid)` run only if **both** are present. The YAML models them flat.
5. **`description` strings are `glue::glue()` templates** constructed at the call
   site (`"{var} should not be missing"`, `"{var} NAs within %10"`) or hardcoded
   inside helpers (`"{col} is numeric"`). The YAML's prose `description:` fields
   do not match and must not be emitted into the report.
6. **skip module severity**: `dlw_validation_skip` calls `verify(nrow > 0)` with
   **no** `error_fun`, so it inherits assertr's default `error_stop` → captured
   as `type = "error"`. The YAML says `warning_append`, which would flip a blank
   dataset from `invalid` → `valid`.
7. **`labelled::var_label(x) <- NULL` is not dead code**: the active (non-commented)
   calls in the numeric loops modify `dlw_data` **by reference** (clearing column
   labels) and flow downstream. They are inert to the validation *report* but not
   to `dlw_data`.

**In-repo prior art**: `R/recode_spec.R` + `tests/testthat/test-recode-spec.R`
already implement the identical migration shape for recoding —
`load_package_recode_spec()` → `validate_recode_spec()` →
`apply_recode_spec()` dispatcher, with equivalence tests vs. legacy functions
and a "KNOWN DIVERGENCE" test convention. This plan follows that pattern, with
one acknowledged divergence: `recode_spec.R` threads the spec from upstream via
an argument to avoid per-survey reads; the validation engine instead uses a
**lazy memoized accessor** `dlw_validation_spec()` because the dispatch site
(`pipdata_validate_gmd.R:183`) does not thread a pre-resolved spec.

**OOM constraint** (from
`.cg-docs/solutions/performance-issues/2026-07-22-per-survey-logging-retains-large-survey-objects.md`):
the engine is called once per survey inside `pipdata_validate_gmd()` and must
**not** emit per-survey `log_info()` — pipfun log wrappers capture `dt` by
reference, causing RAM blowup across a full-inventory run.

Dependencies `yaml`, `data.validator`, `assertr`, `glue`, `data.table`,
`labelled` are already in DESCRIPTION. No new dependencies.

## Deviations from brainstorm

- **`na_threshold == 0 → 1` guard (D6/D2)**: the brainstorm called for
  standardizing this guard across all modules. **This plan deviates**: the guard
  is encoded as a declared per-module field present **only** for group/bin/hist
  (matching current R), to preserve byte-identical behavior. Standardizing would
  change behavior for gpwg/all/aspire/l on datasets with `nrow < 5` (where
  `round(nrow * 0.10) == 0`). Flagged for user awareness under
  `deviation-policy: ask`; no further approval needed unless implementation
  reveals a contradiction.
- **Memoized accessor (precedent divergence)**: noted in Context above.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | One `dlw_validation_engine(dlw_data, svy_id, module)` drives all 8 modules from `validation_spec.yml` | Brainstorm R1 |
| R2 | Engine output byte-identical to legacy per-module functions (golden test; Phase 2 compares `engine` vs committed `.rds` fixtures) | Brainstorm R2/R3, review P2.3 |
| R3 | Report format compatible with `get_validation_report()`/`get_data_status()`/`pd_env_append("validation_report")`; `description` + `table_name` identical to legacy | Brainstorm R3, review P1.1 |
| R4 | `validation_spec.yml` corrected: `variable_availability` literal prefix (`startsWith`), numeric/character/group regex (`grep`), per-check severity on `not_missing`/`na_threshold` only, hhid/pid 3-entry gating, skip `error_stop`, `na_threshold_min` per-module | Research, review P1.2/P2.1/P2.4/P2.7 |
| R5 | `load_package_validation_spec()` + `validate_validation_spec()` schema check (rejects `severity` on helper-fixed entries) | Brainstorm D1, review P2.1 |
| R6 | Dispatch table in `pipdata_validate_gmd.R` maps `Module` → module id; `DEFAULT` → `skip` | Brainstorm D4 |
| R7 | 7 + skip legacy functions become deprecated adapter wrappers; dead code removed (`core_var`, `is_var_endwith_avail`, `emp_status`); `labelled::var_label` clearing **retained** in engine | Brainstorm D3/D6, review P2.5 |
| R8 | Engine emits no per-survey `log_info()` (OOM regression guard) | Solutions doc 2026-07-22 |
| R9 | Full test suite passes; no regressions outside the validation test file | Brainstorm D6 |

## Implementation Steps

## Phase 1: Faithful spec + engine (legacy functions intact)

### 1. Correct `validation_spec.yml` to faithfully mirror R semantics

- **Requirements**: R4
- **Files**: `inst/extdata/validation_spec.yml`
- **Details**: Rewrite the spec so the engine can reproduce R behavior exactly:
  - **Two selection mechanisms** (review P2.7):
    - `variable_availability` entries store a literal `prefix:` string passed to
      `is_var_startwith_avail()` (which uses `startsWith()`). Keep the legacy
      prefixes verbatim: `weight`, `welfare`, `bins` (bin), `age` (all),
      `hhweight` (aspire), `lstatus`/`empstat` (l). Do **not** store these as
      regex `pattern:`.
    - `numeric_validation`/`character_validation`/`validation_group` entries
      store a `pattern:` regex passed to `grep()`, matching the R `grep`/`grepl`
      expressions verbatim: `^year$|hsize$|welfshprosperity$` (gpwg), `urban`
      (group), `^year$|share$` (bin), `urban$|^year$|hsize$|datayear$|type$`
      (hist), `^year$|hsize$` (aspire), `^year$|whours$` (l),
      `^welfare|^weight` (weight/welfare loops), `code$|type$` (group chr),
      `code$|verm$|vera$|^region|^country` (bin chr), `code$|survname$` (hist
      chr), `^male|^educat|^school` (all demog).
    - Fix `aspire` weight loop to `hhweight$` (ends-with), matching R
      `grep("hhweight$")`. (Note: the availability check still uses the
      `startsWith` prefix `hhweight`; only the numeric-loop selection is the
      ends-with regex. These are two different entries.)
  - **Severity per check, helper-fixed-aware** (review P2.1): add `severity:`
    (`warning`/`critical`) **only** on `not_missing` and `na_threshold` check
    entries (the ones that route through `validate_cols`/`validate_rows` and
    honor an explicit `error_fun`). Do **not** put `severity` on
    `is_numeric`/`is_positive`/`is_character`/`is_positive_or_zero`/
    `value_range`/availability entries — their `error_fun` is helper-fixed and a
    spec `severity` would be silently inert. Preserve the exact R assignments:
    weight/welfare loop `na_threshold` `critical`, `not_missing` `warning`;
    numeric loop all `warning`; character loop `na_threshold` `critical` in
    group/hist but `warning` in bin, `not_missing` `warning`; demog loop
    `na_threshold` `critical`, `not_missing` `warning`.
  - **hhid/pid three entries** (review P2.4), each with a presence condition:
    (1) `hhid_not_missing` — `not_na(hhid)` via `validate_cols`, gated on `hhid`
    present; (2) `pid_not_missing` — `not_na(pid)`, gated on **both** `hhid` and
    `pid` present; (3) `hhid_pid_uniqueness` — `is_uniq(hhid, pid)` via
    `validate_if`, gated on **both** present. Do **not** collapse these into a
    single `key_variables` block.
  - **skip module severity** (review P1.2): the `data_presence` entry must
    declare `severity: critical` and the engine must call
    `verify(nrow(dlw_data) > 0, description = "Data should not blank")` with
    assertr's **default** `error_fun` (i.e. do **not** pass
    `error_fun = warning_append`), so a blank dataset yields `type == "error"`.
  - **`na_threshold_min` per-module** (review P2.2): add a module-level
    `na_threshold_min: 1` field **only** to group/bin/hist (matching the
    `if (na_threshold == 0) { na_threshold <- 1 }` guard). Omit it elsewhere to
    preserve current behavior.
  - Drop the now-misleading block-level `error_function` fields (replaced by
    per-check `severity` and helper-fixed behavior).
- **Test Scenarios**: `yaml::read_yaml` parses; each module has a non-empty
  `validations:` map; `variable_availability` entries have `prefix:` (not
  `pattern:`); numeric/character/group entries have `pattern:`; `severity`
  appears only on `not_missing`/`na_threshold`; skip declares `critical`.
- **Tests**: covered by Step 2 schema test and Step 4 golden test.
- **Acceptance criteria**: spec parses; selection mechanisms split; severity
  only on spec-controlled checks; hhid/pid modeled as 3 entries; skip `critical`.

### 2. Add `load_package_validation_spec()`, `validate_validation_spec()`, and schema tests

- **Requirements**: R5
- **Files**: `R/pipdata_dlw_validation.R` (or new `R/validation_spec.R`),
  `tests/testthat/test-dlw_validation_spec.R`
- **Details**: Mirror `R/recode_spec.R`:
  - `load_package_validation_spec()`: `system.file("extdata",
    "validation_spec.yml", package = "pipdata")` → `yaml::read_yaml()` →
    `validate_validation_spec()`. Abort class
    `c("validation_spec_missing", "piperr")` if missing.
  - `validate_validation_spec(spec)`: assert `schema_version` present; iterate
    `spec$modules`, assert each has `validations`; iterate each validation,
    assert `type` is in the known taxonomy. Per type: `variable_availability`
    must have `prefix:` and must **not** have `pattern:`; numeric/character/group
    must have `pattern:` and non-empty `checks:`; `value_constraint` must have
    `valid_values`; `uniqueness` must have `key_variables`; `data_presence` must
    have `check`. **Reject `severity` on helper-fixed check entries** (review
    P2.1): `severity` is allowed only on `not_missing`/`na_threshold` check
    names; if present on `is_numeric`/`is_positive`/`is_character`/
    `is_positive_or_zero`/`value_range`/availability, abort with
    `c("validation_spec_invalid", "piperr")` explaining it is inert. Abort class
    `c("validation_spec_invalid", "piperr")` for all violations.
  - Lazy memoized accessor `dlw_validation_spec()` (parse once, cache in a
    package-level variable). Note: diverges from `recode_spec.R` (which threads
    the spec via an argument) because the dispatch site does not thread a spec.
  - **Schema tests live in Phase 1** (review P3.3): valid spec → `TRUE`; missing
    `schema_version` → abort; unknown `type` → abort; module without
    `validations` → abort; `variable_availability` with `pattern:` → abort;
    `severity` on a helper-fixed entry → abort; bad `severity` value → abort.
- **Test Scenarios**: as above.
- **Tests**: `tests/testthat/test-dlw_validation_spec.R`;
  `devtools::test(filter = "dlw_validation_spec")`.
- **Acceptance criteria**: `validate_validation_spec(load_package_validation_spec())`
  returns `TRUE`; every malformed-spec scenario aborts with the documented class.

### 3. Implement `dlw_validation_engine()`

- **Requirements**: R1, R2, R3, R8
- **Files**: `R/pipdata_dlw_validation.R`
- **Details**: Add `dlw_validation_engine(dlw_data, svy_id, module)` that
  reproduces the exact per-variable `data.validator`/`assertr` chains. The
  engine **reuses** the existing check helpers (`is_character`, `is_numeric`,
  `check_urban`, `check_gender`, `is_greaterthanzero`, `is_greaterequale0`,
  `is_var_avail`, `is_var_startwith_avail`, `is_valuebtwn0and110`) and assertr
  predicates (`not_na`, `num_row_NAs`, `within_bounds`, `in_set`, `is_uniq`,
  `verify`, `warning_append`, `error_append`); it orchestrates them, it does not
  reimplement them. Structure:
  1. `stopifnot(!is.null(dlw_data))`; `df_var_list <- colnames(dlw_data)`.
  2. `spec <- dlw_validation_spec()[[module]]` (fallback to `skip` on unknown).
  3. `na_threshold <- round(nrow(dlw_data) * 0.10)`; if the module declares
     `na_threshold_min` and `na_threshold < na_threshold_min`, set
     `na_threshold <- na_threshold_min` (review P2.2 — preserves the
     group/bin/hist-only guard).
  4. `report <- data_validation_report()`.
  5. Dispatch over each validation entry in spec order, building the same
     `validate(...) |> <checks> |> add_results(report)` chain:
     - **`variable_availability`**: call `is_var_startwith_avail(prefix)`
       (literal `startsWith`, not regex).
     - **regex-selecting entries** (`numeric_validation`/`character_validation`/
       `validation_group`): resolve `vars <- df_var_list[grep(pattern,
       df_var_list)]`; if `length(vars) == 0`, skip (no iterations) — matches
       legacy `seq_along(character(0))` (review P3.2).
     - **per-variable loop** (review P1.1): for each matched `var`, build the
       chain and construct `description` via the **same `glue::glue()` templates
       as legacy**: `glue::glue("{var} should not be missing")` for `not_na`,
       `glue::glue("{var} NAs within %10")` for the NA-threshold,
       and rely on the helper's own `glue` for helper-based checks
       (`is_numeric` → `"{var} is numeric"`, `is_greaterthanzero` → `"{var} > 0"`,
       `is_character` → `"{var} is character"`, etc.). The YAML `description:`
       field is **documentation-only and never emitted**.
     - **severity** (review P2.1): for `not_missing` (`validate_cols`) and
       `na_threshold` (`validate_rows`), pass `error_fun =` from the entry's
       `severity` (`critical` → `error_append`, `warning` → `warning_append`).
       For helper-based checks, do **not** pass `error_fun` — the helper fixes
       it.
     - **`required: false` skip** (review P2.6): for single-column entries
       (`countrycode`, `urban`, `age`, `male`, `hhid`, `pid`), wrap the whole
       check in `if (col %in% df_var_list)`; for the hhid/pid three entries, use
       the per-entry presence condition from Step 1 (hhid-only vs both-present).
       `categorical_check` entries (`check_urban`/`check_gender`) likewise gate
       on column presence.
     - **skip module** (review P1.2): call
       `verify(nrow(dlw_data) > 0, description = "Data should not blank")`
       **without** `error_fun` (assertr default `error_stop` → `type == "error"`).
  6. `validation_record <- get_results(report, unnest = FALSE) |> setDT()`;
     `err_t <- validation_record[, .(table_name, message, type)]`;
     `pd_env_append("validation_report", validation_record)`;
     `return(invisible(err_t))`.
  - **Do not** add any `log_info()`/`cli` side effects inside the engine (R8).
- **Test Scenarios**: see Step 4.
- **Tests**: `tests/testthat/test-dlw_validation_engine.R`.
- **Acceptance criteria**: engine runs for all 8 module ids and returns the
  3-col `err_t` while appending the full record to env; descriptions match
  legacy `glue` templates; `required:false` and empty-match paths skip cleanly;
  skip blank-data yields `type == "error"`; no `log_info` call in the engine
  body.

### 4. Golden differential test (engine vs. legacy functions) + fixtures

- **Requirements**: R2, R3, R9
- **Files**: `tests/testthat/test-dlw_validation_engine.R`,
  `tests/testthat/fixtures/validation_<module>.rds`
- **Details**: While the legacy functions are still intact:
  1. Build synthetic DLW data.tables for each module with the expected columns
     including intentionally-violating values. **Must include** (review P3.1):
     at least one fixture per module that triggers an actual `type == "error"`
     result (e.g. a weight column with >10% NAs to fire `error_append` on
     `num_row_NAs`; an availability failure; `is_greaterequale0(age)` failure).
  2. **Per-module fixture matrix** (reviews P1.2, P2.6, P3.2):
     - **skip**: a fixture with `nrow == 0` (blank) asserting `type == "error"`
       (review P1.2), plus a non-blank fixture asserting success.
     - **optional-column-omitted** (review P2.6): for gpwg/all/l, a fixture
       omitting `urban`/`age`/`male`/`pid` so the `required:false` skip path is
       exercised and produces zero spurious rows.
     - **empty-match** (review P3.2): a fixture where one regex pattern matches
       zero columns (e.g. a gpwg dataset with no `welfshprosperity`), asserting
       the engine produces zero report rows for that loop.
  3. For each module, call the legacy function and the engine on the same input;
     capture both full records (from `pd_env_get("validation_report")`, isolated
     per test via `pd_env_rm`/`withr::defer`).
  4. Assert equality on the deterministic columns
     `table_name, description, num.violations, message, type` (row order
     preserved). Since descriptions are now reproducible via legacy `glue`
     templates (Step 3), `description` is part of the comparison. If
     `assertion.id`/`call`/`error_df` prove non-deterministic, compare on the
     deterministic subset and additionally assert row count + `table_name`
     equality.
  5. **Commit the legacy outputs as golden fixtures**
     (`tests/testthat/fixtures/validation_<module>.rds`). The Phase 2 test
     (Step 7) compares `engine(input)` against these fixtures — **not** against
     the legacy wrapper (review P2.3): once legacy functions become wrappers in
     Phase 2, comparing engine-vs-wrapper is trivially equal and would not catch
     regressions.
- **Test Scenarios**: all 8 modules; happy path, error path (`type == "error"`),
  blank-data (skip), optional-column-omitted, empty-match.
- **Tests**: `devtools::test(filter = "dlw_validation")`.
- **Acceptance criteria**: all 8 modules pass with `all.equal` on the
  deterministic subset; at least one `error`-type result per module; skip
  blank-data yields `error`; fixtures committed.

## Phase 2: Switch over, cleanup, docs

### 5. Rewire the dispatch table in `pipdata_validate_gmd.R`

- **Requirements**: R6, R9
- **Files**: `R/pipdata_validate_gmd.R`
- **Details**: Replace `validation_functions` (lines 120–129) with a
  `validation_modules` map (`GPWG = "gpwg"`, `GROUP = "group"`, `BIN = "bin"`,
  `HIST = "hist"`, `ALL = "all"`, `ASPIRE = "aspire"`, `L = "l"`,
  `DEFAULT = "skip"`). At the dispatch site (lines 183–187):
  ```r
  md_key <- if (md_type %in% names(validation_modules)) md_type else "DEFAULT"
  check <- dlw_validation_engine(out, nm, validation_modules[[md_key]])
  ```
  The `valid_status` derivation (lines 189–195) is unchanged.
- **Test Scenarios**: `md_type` in map → correct module id; `md_type` absent →
  `skip`; result drives `valid`/`invalid` as before; a blank dataset dispatched
  to `skip` yields `invalid` (review P1.2 end-to-end).
- **Tests**: existing pipeline tests; `devtools::test(filter = "validate_gmd")`.
- **Acceptance criteria**: no `dlw_validation_<module>()` direct call remains in
  `pipdata_validate_gmd.R`; dispatch keys unchanged.

### 6. Convert legacy functions to deprecated wrappers; remove dead code

- **Requirements**: R7
- **Files**: `R/pipdata_dlw_validation.R`
- **Details**:
  - Replace each of the 7 + skip function bodies with a thin wrapper calling the
    engine (`dlw_validation_gpwg <- function(dlw_data, svy_id)
    dlw_validation_engine(dlw_data, svy_id, "gpwg")`), marked
    `@keywords internal` (deprecation via roxygen `@details`; do not use
    `.Deprecated()` because these are dispatched programmatically and would emit
    a warning per survey).
  - Remove dead code: `core_var` assignments (gpwg/group/bin/hist),
    `is_var_endwith_avail` (never called), the `emp_status` variable +
    commented-out loop in `dlw_validation_l`, and the inconsistent inline
    `na_threshold == 0` guards (now declared in the spec).
  - **Keep** `labelled::var_label(dlw_data[[col]]) <- NULL` inside the engine's
    numeric loop (review P2.5): it is an observable `dlw_data` side effect
    (clears column labels), not dead code. Do **not** remove it. (Before
    finalizing, grep downstream code — `pd_dlw_clean.R`, `recode_spec.R`,
    `build_pip_inventory.R`, `pipdata_validate_gmd.R` — for `var_label`/`label`
    reads on numeric columns; if none depend on labels being cleared, document
    that finding in the execution report. Either way, retain the clearing to
    preserve byte-identical `dlw_data` state.)
  - Keep the documentation anchors `dlw_validation()` and `dlw_var_check()` and
    the check helpers (minus `is_var_endwith_avail`).
- **Test Scenarios**: wrappers return engine-equivalent output; grep confirms
  dead identifiers absent; `labelled::var_label` clearing still present in engine.
- **Tests**: Step 7 fixture test remains green;
  `devtools::test(filter = "dlw_validation")`.
- **Acceptance criteria**: `grep -n "core_var\|is_var_endwith_avail\|emp_status" R/pipdata_dlw_validation.R`
  returns nothing; `grep -n "labelled::var_label" R/pipdata_dlw_validation.R`
  returns the engine's retained clearing call; wrappers are one-liners.

### 7. Fixture-comparison test (post-Phase-2 permanent guard) + data-driven + report tests

- **Requirements**: R2, R3, R9
- **Files**: `tests/testthat/test-dlw_validation_engine.R`,
  `tests/testthat/test-dlw_validation_spec.R`
- **Details**:
  - **Fixture comparison** (review P2.3): replace the Phase 1 live differential
    with `engine(input) vs readRDS("fixtures/validation_<module>.rds")` for all
    8 modules, asserting `all.equal` on the deterministic subset. This is the
    permanent regression guard after legacy functions became wrappers.
  - **Data-driven engine test**: iterate all module names in the spec and assert
    the engine returns a 3-col `err_t` and appends a non-empty record; assert
    the record contains expected `type` values.
  - **Report-format compatibility test**: `get_validation_report()` and
    `get_data_status()` run against an engine-produced record without error and
    produce the expected columns (`module_type`, `vermast`, `veralt`,
    `country_code`, `rf_year`; `data_status`, `n`); assert a synthetic
    `table_name` of the form `{country}_{year}_..._M_..._A_{MODULE}` derives the
    correct `module_type`.
- **Test Scenarios**: as above.
- **Tests**: `devtools::test(filter = "dlw_validation")`.
- **Acceptance criteria**: fixture test passes for all 8 modules; report-format
  test derives `module_type` correctly.

### 8. Roxygen docs, NAMESPACE/man, NEWS

- **Requirements**: R1, R7
- **Files**: `R/pipdata_dlw_validation.R`, `man/`, `NAMESPACE`, `NEWS.md`
- **Details**:
  - `@export` `dlw_validation_engine` with roxygen documenting `dlw_data`,
    `svy_id`, `module`; `@keywords internal` for
    `load_package_validation_spec`/`validate_validation_spec`/`dlw_validation_spec`.
  - Run `devtools::document()` to regenerate `man/*.Rd` and `NAMESPACE`.
  - Add a `NEWS.md` entry describing the refactor and the deprecated wrappers.
  - Add any new NSE symbols to `utils::globalVariables()` in `R/aaa.R` if R CMD
    check flags them.
- **Test Scenarios**: `devtools::document()` runs clean; no new NOTEs.
- **Tests**: `devtools::document()`; targeted `devtools::check(manual = FALSE)`.
- **Acceptance criteria**: man/*.Rd regenerated; NAMESPACE exports the engine;
  NEWS updated.

## Testing Strategy

- Phase 1 golden **differential** test (Step 4) is the correctness gate while
  legacy functions are intact; it captures committed `.rds` fixtures.
- Phase 2 **fixture** test (Step 7) compares `engine(input)` vs the committed
  fixtures — the permanent regression guard (review P2.3).
- Data-driven test (Step 7) iterates all modules; schema test (Step 2, Phase 1)
  exercises `validate_validation_spec()` failure paths.
- Every module has at least one `error`-type fixture (review P3.1); skip has a
  blank-data fixture; optional-column-omitted and empty-match fixtures exist.
- Run `devtools::test(filter = "dlw_validation")` after each phase; full
  `devtools::test()` before completion.
- Use the `pd_env_rm`/`withr::defer` teardown pattern from `test-pd-env.R` to
  avoid leaking `validation_report` state between tests.

## Documentation Checklist

- [ ] `dlw_validation_engine` roxygen (`@param` + `@details` + `@examples`)
- [ ] `man/dlw_validation_engine.Rd` and `man/validation_spec*` regenerated
- [ ] `NAMESPACE` exports the engine
- [ ] `NEWS.md` entry
- [ ] Legacy wrappers marked deprecated in roxygen `@details`

## Risks & Mitigations

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| `validation_spec.yml` does not faithfully mirror R, causing golden-test failures | High (already confirmed) | Step 1 rewrites YAML to literal R regexes, split selection mechanisms, per-check severity, 3-entry hhid/pid; golden test arbitrates |
| `description` mismatch (YAML prose vs legacy `glue` templates) | High (review P1.1) | Step 3 hardcodes legacy `glue` templates; YAML `description:` is doc-only; `description` stays in the comparison set |
| skip module `warning_append` flips blank-dataset `valid_status` | High (review P1.2) | Step 1 declares skip `critical`; Step 3 uses assertr default `error_stop`; Step 4 adds blank-data fixture asserting `type == "error"` |
| `severity` set on a helper-fixed check is silently inert | Medium (review P2.1) | Schema (Step 2) rejects `severity` on helper-fixed entries; documented in Step 1 |
| Golden test trivializes post-Phase-2 (engine vs wrapper) | High (review P2.3) | Phase 2 fixture test compares `engine(input)` vs `readRDS(fixture)`, not vs wrapper |
| hhid/pid collapse loses hhid-only `not_na` path | Medium (review P2.4) | 3 separate entries with distinct gating in Step 1 |
| Removing `labelled::var_label` changes `dlw_data` state downstream | Medium (review P2.5) | Retain clearing in the engine; grep downstream label dependencies and document |
| `required:false` not skipped → assertr errors on missing optional cols | Medium (review P2.6) | Step 3 wraps in `if (col %in% names)`; Step 4 adds optional-column-omitted fixture |
| `variable_availability` treated as regex instead of `startsWith` | Medium (review P2.7) | Step 1 splits `prefix:` vs `pattern:`; schema (Step 2) enforces |
| `na_threshold == 0 → 1` guard deviation from brainstorm D6 | Low | Documented in Deviations; preserves byte-identical behavior |
| Per-survey `log_info()` reintroduces OOM regression | Low | R8 constraint + code review |

## Out of Scope

- `get_validation_report()` / `get_data_status()` `table_name` regex-parsing
  fragility (now tracked as roadmap feature
  `validation-report-table-name-parsing`).
- `pd_env_append` / shared mutable `validation_report` env-state redesign.
- Any change to the `data.validator` / `assertr` pipeline DSL.
- Replacing the check helper functions (`is_numeric`, `check_urban`, etc.) — the
  engine reuses them unchanged.

## Completion Contract

### Outcome
`dlw_validation_engine(dlw_data, svy_id, module)` replaces the 7 near-identical
validation functions + `dlw_validation_skip`, driven by a corrected
`inst/extdata/validation_spec.yml`; output is byte-identical to the legacy
functions (golden-test enforced via committed `.rds` fixtures compared
post-Phase-2), the report format is unchanged, and dead code is removed.

### Verification Surface

| ID | Phase | Evidence Required | Command/Artifact | Required |
|----|-------|-------------------|------------------|----------|
| V1 | 1 | `validation_spec.yml` parses; `validate_validation_spec()` returns TRUE; schema rejects `severity` on helper-fixed entries and bad values | `pipdata:::validate_validation_spec(...)` / test | yes |
| V2 | 1 | `dlw_validation_engine()` exists; returns 3-col `err_t`, appends full record; descriptions use legacy `glue` templates; `required:false`/empty-match paths handled | code review + test | yes |
| V3 | 1 | Golden differential test passes for all 8 modules incl. skip blank-dataset (`type=="error"`), `error`-type fixtures, optional-column-omitted, empty-match fixtures | `devtools::test(filter = "dlw_validation")` | yes |
| V4 | 2 | Dispatch rewired; `pipdata_validate_gmd()` maps Module→id, DEFAULT→skip; blank-dataset→`invalid` | code review + test | yes |
| V5 | 2 | Legacy functions are deprecated wrappers; dead code (`core_var`, `is_var_endwith_avail`, `emp_status`) removed; `labelled::var_label` clearing retained in engine | grep / code review | yes |
| V6 | final | Phase 2 fixture test: `engine(input)` == `readRDS(fixtures/validation_<module>.rds)` for all 8 modules | `devtools::test(filter = "dlw_validation")` | yes |
| V7 | final | Full test suite passes, no regressions | `devtools::test()` | yes |
| V8 | final | `devtools::document()` regenerates man/*.Rd; no new R CMD check NOTEs | `devtools::document()` / check | yes |

### Constraints

| ID | Phase | Constraint | Check |
|----|-------|------------|-------|
| C1 | 1 | Engine emits no per-survey `log_info()` (OOM regression) | code review |
| C2 | 1 | `table_name` + `description` values identical to legacy (feed `get_validation_report()`/`get_validation_list()`) | golden test |
| C3 | 1 | Report schema unchanged (8 data.validator columns) | golden test |
| C4 | 1 | `severity` only honored on `not_missing`/`na_threshold`; rejected on helper-fixed entries | schema test |
| C5 | 1 | `variable_availability` uses literal `startsWith` prefix; grep-regex only for numeric/character/group selection | code review + test |
| C6 | 2 | Dispatch keys remain case-sensitive Module values | code review |
| C7 | 2 | Check helpers unchanged; only `is_var_endwith_avail` removed | code review |
| C8 | 2 | `labelled::var_label(dlw_data[[col]]) <- NULL` retained in the engine (observable `dlw_data` side effect) | code review |
| C9 | 1 | No new package dependencies | DESCRIPTION diff |

### Boundaries
- **Allowed**: `R/pipdata_dlw_validation.R`, `R/pipdata_validate_gmd.R`,
  `inst/extdata/validation_spec.yml`, new test file(s),
  `tests/testthat/fixtures/`, `man/`, `NAMESPACE`, `NEWS.md`, `R/aaa.R`
  (globalVariables only).
- **Out of scope**: `get_validation_report()`/`get_data_status()`
  `table_name`-parsing refactor (roadmap feature
  `validation-report-table-name-parsing`); `pd_env_append` mutable-state
  redesign; any `data.validator`/`assertr` DSL change; check-helper
  reimplementation.

### Iteration Policy
1. Implement Phase 1 (spec correction → loader/validator + schema test → engine
   → golden differential). Do not touch dispatch or wrappers until the golden
   differential passes for all 8 modules including the skip blank-dataset,
   error-type, optional-column-omitted, and empty-match fixtures.
2. Capture golden fixtures from the legacy functions while intact; commit them.
   Phase 2 restructures the test to compare `engine(input)` vs
   `readRDS(fixture)`.
3. Under `deviation-policy: ask`, surface any behavioral divergence before
   choosing "fix YAML" vs. "change R" (the per-module `na_threshold` guard
   deviation from brainstorm D6 is pre-flagged — see Deviations).
4. Run `devtools::test(filter = "dlw_validation")` after each phase; full
   `devtools::test()` before completion.

### Blocked-Stop Conditions
- A module's golden test cannot pass without changing legacy behavior (user
  decision under `ask`).
- The skip-module `error_stop` vs `warning_append` severity cannot be reconciled
  (would flip `valid_status`).
- Test regressions outside the validation test file.
- Full `devtools::test()` cannot run.
