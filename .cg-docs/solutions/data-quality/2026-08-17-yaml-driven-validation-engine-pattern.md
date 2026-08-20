---
date: 2026-08-17
title: "Replace N near-identical validation functions with a YAML-driven engine"
category: "data-quality"
language: "R"
tags: [validation, dlw, refactor, data-driven, yaml, data.validator, assertr, engine, schema]
root-cause: "Eight DLW validation functions shared an identical skeleton and differed only in regex selection and optional checks, making every spec change a multi-file edit."
severity: "P2"
---

# Replace N near-identical validation functions with a YAML-driven engine

## Problem

`R/pipdata_dlw_validation.R` defined 8 module validators (`dlw_validation_gpwg`,
`dlw_validation_group`, `dlw_validation_bin`, `dlw_validation_hist`,
`dlw_validation_all`, `dlw_validation_aspire`, `dlw_validation_l`,
`dlw_validation_skip`) that shared an identical 8-step skeleton. The only
differences were (a) which variable-name regexes selected numeric/character/
welfare/weight columns and (b) which optional checks ran (urban, gender,
hhid/pid uniqueness, welfare_type in-set, age range). Any change to validation
logic required editing all 8 functions, and a 483-line `validation_spec.yml`
existed but was **not consumed by any code** and did not faithfully mirror the R
semantics.

## Root Cause

The code was structurally repetitive — maintenance-concentrated — and the
declarative spec that was meant to drive it had drifted from the actual R logic
in seven specific ways (selection semantics, split selection mechanism,
per-check vs per-block severity, hhid/pid three-check gating, glue templates vs
YAML prose descriptions, skip-module severity, and `labelled` clearing as a
side effect).

## Solution

Introduce a single `dlw_validation_engine(dlw_data, svy_id, module)` that
dispatches over a corrected `inst/extdata/validation_spec.yml`, mirroring the
existing prior-art shape in `R/recode_spec.R`:
`load_package_validation_spec()` → `validate_validation_spec()` → dispatcher.

Key design decisions:

1. **Split selection mechanisms** (review-drive correction): `variable_availability`
   entries store a literal `prefix:` passed to `is_var_startwith_avail()`
   (`startsWith`); numeric/character/group entries store a `pattern:` regex
   passed to `grep()`. Never conflate them under one field.
2. **Per-check severity, helper-fixed-aware**: `severity` is honored only on
   `not_missing`/`na_threshold` (the checks routed through
   `validate_cols`/`validate_rows` with explicit `error_fun =`). Helper-based
   checks (`is_numeric`, `check_urban`, ...) fix their own `error_fun` — a spec
   `severity` there is silently inert. The schema **rejects** `severity` on
   helper-fixed entries.
3. **Lazy memoized accessor**: `dlw_validation_spec()` caches the parsed spec in
   `.pipdataenv` (the package namespace is locking-protected; `<<-` to a
   top-level variable fails). This diverges from `recode_spec.R`, which threads
   the spec as an argument, because the dispatch site
   (`pipdata_validate_gmd.R`) does not thread a pre-resolved spec.
4. **Golden fixtures**: while legacy functions were intact, generate committed
   `.rds` fixtures (`tests/testthat/fixtures/validation_<module>.rds`) plus
   per-module `error` fixtures and a blank skip fixture. Post-refactor, the
   permanent guard compares `engine(input)` to `readRDS(fixture)` on the
   deterministic subset (`table_name`, `description`, `num.violations`,
   `message`, `type`). `assertion.id`/`error_df` are non-deterministic (random
   hashes) and excluded. Because the fixtures were generated from the legacy
   functions, the engine-vs-wrapper equality stays meaningful.
5. **Deterministic fixture regeneration**: keep the synthetic data generators
   and the RNG protocol in one shared `tests/testthat/helper-dlw-data.R`
   (`dlw_fixture_data(module)` uses a per-module seed). Both the test file and
   the regeneration script source the same helper so fixtures never
   desynchronize from tests.
6. **No per-survey logging**: the engine emits no `log_info()`/`log_add()` per
   survey — pipfun log wrappers capture `dt` by reference, causing RAM blowup
   across a full inventory run (see related solution 2026-07-22).

## Prevention

- When a family of functions shares a skeleton, extract the invariant core into
  a dispatcher and keep only the varying axes in data.
- When a YAML spec claims to drive behavior, **schema-validate it** and reject
  entries the engine cannot honor (unknown types, unknown check names, severity
  on inert checks, missing required fields like `variable`, unknown `condition`
  values) — a silently-inert check is a data-integrity hazard.
- Discriminate `required: false` (skip missing column) from presence gating
  (`hhid`-only vs `hhid`+`pid`) explicitly; collapse redundant `if` guards.
- Capture golden outputs from the legacy implementation before deleting it, so
  the refactor is regression-locked rather than self-referential.
- Use `testthat::test_path()` for fixture paths so tests run under both
  `devtools::test()` and bare `test_file()`.

## Related

- `.cg-docs/solutions/performance-issues/2026-07-22-per-survey-logging-retains-large-survey-objects.md`
  (OOM constraint that forces no per-survey logging in the engine)
- `R/recode_spec.R` + `tests/testthat/test-recode-spec.R` — the in-repo prior
  art for the load→validate→dispatch migration shape
- Plan `.cg-docs/plans/2026-08-17-dlw-validation-engine-refactor.md`
- Work report `.cg-docs/work-reports/2026-08-17-dlw-validation-engine-refactor.md`
- Verify review `.cg-docs/reviews/2026-08-06-aux-version-gate-valid-dlw-load-revised-verify-review-2.md`
