---
date: 2026-08-14
title: "Replace 7 DLW validation functions with a single data-driven engine"
status: decided
chosen-approach: "Data-driven YAML engine + differential-test migration"
participants:
  - wb384996
tags: [validation, dlw, refactor, data-driven, yaml, data.validator]
---

# DLW Validation Engine Refactor

## Context

`R/pipdata_dlw_validation.R` (~1,105 lines) defines 7 near-identical module
validation functions (`dlw_validation_gpwg`, `dlw_validation_group`,
`dlw_validation_bin`, `dlw_validation_hist`, `dlw_validation_all`,
`dlw_validation_aspire`, `dlw_validation_l`) plus a simpler 8th
(`dlw_validation_skip`, the DEFAULT fallback). Each follows the same 8-step
skeleton and differs only in (a) which variable-name regexes select the numeric /
character / welfare / weight columns and (b) which optional checks run (urban,
gender, hhid/pid uniqueness, `welfare_type` in-set, age range). The invariant
per-variable chain is: `is_numeric → is_greaterthanzero →
validate_cols(not_na) → validate_rows(num_row_NAs, within_bounds)` with
`error_append` for weight/welfare and `warning_append` elsewhere.

A declarative spec **already exists** at `inst/extdata/validation_spec.yml`
(483 lines, `schema_version: 1.0`) that mirrors these rules via a 10-type
validation taxonomy — but no code consumes it. It is a blueprint, not a
source of truth.

The problem: ~1,100 lines of copy-pasted imperative code with two de-synchronized
sources of truth (live R vs. blueprint YAML), plus dead code accumulating in each
copy.

## Requirements

- R1: One `dlw_validation_engine()` drives all 8 modules from a single spec.
- R2: The engine output must be byte-identical to the current per-module functions
  for the same input (differential-test enforced).
- R3: Report format must stay compatible with `get_validation_report()`,
  `get_data_status()`, and the `pd_env_append("validation_report", ...)` contract.
- R4: No behavior regression — the pipeline dispatch in `pipdata_validate_gmd.R`
  must keep working via the same `Module`-column keys.
- R5: Eliminate dead code (technical-debt removal is a goal, not incidental).

## Decisions

### D1. Spec format: YAML in `inst/extdata/` (chosen over named R list)

The YAML already exists and is the natural declarative home. Editing a module
requires no R recompilation, follows the `recode_spec.yml` precedent, and lets a
data manager add a module without touching code.

**Mitigation for the runtime-only parse failure risk**: add a
`validate_validation_spec()` schema-checking function run at engine load, plus a
testthat that parses `validation_spec.yml` and asserts every referenced
`check_function`/`error_function` exists.

### D2. Spec structure (10-type taxonomy, already in the YAML)

The existing `validation_spec.yml` taxonomy is adopted as-is:

| Type | Key fields | Maps to |
|------|-----------|---------|
| `variable_availability` | `pattern`, `error_function` | `is_var_startwith_avail` |
| `numeric_validation` | `pattern`, `checks: [is_numeric, is_positive, not_missing, na_threshold]` | numeric loop |
| `character_validation` | `pattern`, `checks: [is_character, not_missing, na_threshold]` | chr loop |
| `validation_group` | `pattern`, `checks: [not_missing, na_threshold]` | demog loop (no type/positive) |
| `categorical_check` | `check_function` | `check_urban` / `check_gender` |
| `not_missing` | column | `not_na(hhid)` / `not_na(pid)` |
| `uniqueness` | `key_variables` | `is_uniq(hhid, pid)` |
| `value_constraint` | `valid_values` | `in_set(...)` on `welfare_type` |
| `numeric_range` | `checks` with range | age `[0,110]` |
| `data_presence` | `check: "nrow > 0"` | `dlw_validation_skip` |

Two gaps to close in the YAML:
1. **Per-check `error_fun` override** — weight/welfare use `error_append`,
   everything else `warning_append`. Encode as a `severity` field per validation
   (`critical` vs `warning`) with `defaults.severity = warning`.
2. **`na_threshold == 0 → 1` guard** (present only in group/bin/hist) — fold into
   the engine as a single consistent rule applied to *all* modules (see D5).

### D3. Migration path: all 8 at once, verified by differential test

Replace all 7 + skip in one change. The 7 old functions become thin adapter
wrappers (`dlw_validation_gpwg <- function(dlw_data, svy_id) dlw_validation_engine(dlw_data, svy_id, "gpwg")`)
kept for one release cycle so external callers don't break; they are deprecated
via roxygen `@keywords internal` and removed after confirming no external usage
(grep `pipdata::dlw_validation_` in dependent code).

**Convergence mechanism ("bootstrap from R then switch")**: write a
differential test that runs, for each module, the *old* function and the *engine*
on identical synthetic DLW inputs and asserts `all.equal()` on the
`get_results(report, unnest = FALSE)` output. The current R functions are the
production source of truth. Where the engine disagrees, **fix the YAML**, not the
R. Once all 8 match, the wrappers are the only remaining R logic and can be
dropped. This audited the existing YAML against the R automatically rather than
by manual inspection.

### D4. Dispatch table change (`pipdata_validate_gmd.R` lines 118–129)

The `validation_functions` list becomes a `name → module-id` map, with the spec
loaded once at engine first use (lazy `dlw_validation_spec()` accessor caching the
parsed YAML):

```r
validation_modules <- list(
  GPWG = "gpwg", GROUP = "group", BIN = "bin", HIST = "hist",
  ALL = "all", ASPIRE = "aspire", L = "l", DEFAULT = "skip"
)
```

Dispatch site (lines 182–187) changes to:

```r
md_key <- if (md_type %in% names(validation_modules)) md_type else "DEFAULT"
check <- dlw_validation_engine(out, nm, validation_modules[[md_key]])
```

Module keys stay case-sensitive and match the `Module` column values unchanged;
only the mapped value is lowercase (matching YAML keys).

### D5. Report format: unchanged (compatibility preserved)

The engine produces the same `data.validator` `Report` and appends the same
`get_results(report, unnest = FALSE) |> setDT()` schema via `pd_env_append`.
`get_validation_report()` / `get_data_status()` are untouched. The 3-column
`err_t <- .(table_name, message, type)` return value is preserved so the
`valid`/`invalid` derivation in `pipdata_validate_gmd.R` (lines 189–195) keeps
working.

### D6. Dead-code cleanup (all of it, gated by differential test)

Remove, as part of the same change:
- `core_var` assignments (declared, never used, in gpwg/group/bin/hist)
- `is_var_endwith_avail` (defined, never called)
- `emp_status` variable + commented-out loop in `dlw_validation_l`
- Inconsistent `na_threshold == 0 → 1` guard (standardize in engine)
- Half-commented `labelled::var_label(...) <- NULL` lines (drop all; they were
  inert or commented out)

The differential test (D3) is the safety net: it proves the cleanup does not
change observable behavior. If any test fails during cleanup, the cleanup is
re-scoped to only what is provably inert.

## Consequences

- **Pros**: ~1,100 → ~150 lines of R (engine + accessor + wrappers). Single
  source of truth. Adding a module = one YAML block + one dispatch entry. Dead
  code gone. Differential test gives high-confidence, no-regression migration.
- **Cons / trade-offs accepted**:
  - YAML parse errors surface at runtime, not `load_all()` time (mitigated by
    `validate_validation_spec()` + test).
  - Two sources of truth coexist during the transition window (mitigated by the
    differential test making R authoritative until convergence).
  - The engine is more abstract than 7 explicit functions — debugging a module
    requires reading YAML + the engine's check-dispatch `switch`.

## Out of Scope

- `get_validation_report()` / `get_data_status()` refactor (the `table_name`
  regex-parsing fragility is a separate concern, flagged but not fixed here).
- `pd_env_append` / shared mutable env state (`validation_report` accumulation
  across surveys) — pre-existing design, not touched.
- Any change to the `data.validator` / `assertr` pipeline DSL itself.

## Next Steps

1. Add `dlw_validation_spec()` accessor that lazily parses `inst/extdata/validation_spec.yml`
   and caches the result; add `validate_validation_spec()` schema check.
2. Implement `dlw_validation_engine(dlw_data, svy_id, module)` implementing the
   10-type check dispatch, standard `na_threshold` guard, and the
   `critical`/`warning` severity mapping.
3. Write the differential test: for each of 8 modules, compare old function vs
   engine output on synthetic DLW fixtures; fix YAML where they differ.
4. Rewrite dispatch table in `pipdata_validate_gmd.R` to the `validation_modules`
   map + engine call.
5. Convert the 7 + skip functions to deprecated adapter wrappers; drop dead code.
6. Update `tests/testthat/` with a data-driven engine test + keep legacy
   per-module tests during the transition.
7. Hand off to `/cg-plan` for phased implementation.
