---
date: 2026-08-17
depth: light
parent-review: .cg-docs/reviews/2026-08-06-aux-version-gate-valid-dlw-load-revised-review.md
type: verification
findings:
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: fixed
  P2.6: fixed
  P2.7: fixed
  P2.8: skipped
  P3.1: fixed
  P3.2: fixed
  P3.3: fixed
  P3.4: fixed
  P3.5: fixed
  P3.6: fixed
  P3.7: fixed
  P3.8: skipped
  P3.9: skipped
  P3.10: fixed
  P3.11: fixed
---

# Verification Review: DLW Validation Engine Refactor (light verify pass)

**Review mode**: mode:verify / light
**Parent review**: `.cg-docs/reviews/2026-08-06-aux-version-gate-valid-dlw-load-revised-review.md`
**Verification scope**: uncommitted DLW validation engine refactor changes in working tree
**Date**: 2026-08-17

## Verification result

**Full test suite**: PASS — 0 failures, 2 pre-existing skips, 689 passes.
**Validation-suite filter**: `devtools::test(filter = "dlw_validation")` — 174 pass, 0 fail.
**Evidence**: `dlw_validation_engine` fixture comparison vs committed `.rds` fixtures confirmed
(all 15 happy+error fixtures match on the deterministic subset; `assertion.id`/`error_df` are the
only non-deterministic columns, correctly excluded).

## Suppression context

Prior review fixed findings: P1.1–P1.5, P2.1–P2.10, P3.1, P3.2 (aux-gate `valid_dlw_load`/
`build_pip_inventory` work). The changes under verification are the **DLW validation engine
refactor** (`R/pipdata_dlw_validation.R`, `R/pipdata_validate_gmd.R`,
`inst/extdata/validation_spec.yml`, new test files + fixtures). None of the prior fixed findings
target these files, so **no findings were suppressed** — all are reportable.

## Findings

### P0 — BLOCKING
None.

### P1 — CRITICAL
None.

### P2 — IMPORTANT

- **[P2.1]** [cg-code-quality] `R/pipdata_dlw_validation.R:396-401` — `data_presence` severity is
  parsed (`error_fn` computed at line 397) but never passed to `verify()`, so a `severity: warning`
  entry would still hard-error via assertr's default `error_append`. Dead variable.
  **Why**: The schema's mandatory-severity rule makes the severity look meaningful, but it is never
  honored.
  **Fix**: pass `error_fun = error_fn` to `verify()`, or drop `severity` from the schema requirement.

- **[P2.2]** [cg-code-quality] `R/pipdata_dlw_validation.R:364-378, 408-428` — `uniqueness`
  severity is collected but dropped; `dup_type` is hard-coded to `"warning"`. A future
  `severity: critical` uniqueness entry would still emit a warning, letting duplicate-keyed surveys
  pass as valid (silent data-integrity trap). Today's YAML uses `warning` (matches legacy), so no
  current regression.
  **Fix**: honor `uc$severity` when computing `dup_type`.

- **[P2.3]** [cg-code-quality] `R/pipdata_dlw_validation.R:339-378, 380-394` — schema permits
  entry shapes that crash the engine at runtime: `not_missing`/`uniqueness` without `severity`
  (→ `if (logical(0))` at lines 348/372), `not_missing`/`categorical_check`/`single_variable`
  without `variable` (→ `if (logical(0))` at line 328/341), and unknown `condition` values
  silently skip the presence gate.
  **Fix**: enforce required fields (`variable`, and `severity` where the engine reads it) in the
  schema switch; reject unknown `condition` values.

- **[P2.4]** [cg-code-quality] `R/pipdata_dlw_validation.R:166-179, 253-304, 311-335` —
  `.helper_fixed_checks` is the only whitelist and is used solely for the inert-severity abort;
  unknown check names silently no-op (no report row, no error). Check-level `severity` values are
  never validated.
  **Fix**: validate every check name and `check`/`secondary_check` value against one authoritative
  dispatch table in `validate_validation_spec()`; reject unknown check severities.

- **[P2.5]** [cg-code-quality] `R/pipdata_dlw_validation.R:169` — `chk$severity` on a non-list
  check (bare string YAML) crashes `validate_validation_spec()` with
  "`$ operator is invalid for atomic vectors`". Tests only exercise list-form checks.
  **Fix**: guard with `is.list(chk) &&`, or normalize bare strings to
  `list(name = chk, severity = "helper")`.

- **[P2.6]** [cg-code-quality] `R/pipdata_dlw_validation.R:278, 288, 376, 399` — `%||%` is base R
  ≥ 4.4 while `DESCRIPTION` declares `Depends: R (>= 2.10)`. On R < 4.4 the engine fails at
  runtime. rlang (in Imports) provides `%||%`, but it is not imported.
  **Fix**: bump `Depends: R (>= 4.4)` or add `@importFrom rlang %||%`.

- **[P2.7]** [cg-testing] `tests/testthat/test-dlw_validation_engine.R:180-199` — no test
  exercises a failed assertion check (only availability failures); `not_missing`, `na_threshold`
  severity routing, `is_numeric`, `is_character`, `is_greaterthanzero`, `is_greaterequale0`,
  `is_valuebtwn0and110`, `in_set`, and `is_uniq` are never triggered with real violations.
  **Fix**: add fixtures/tests with NAs above/below threshold, wrong-typed columns, out-of-range
  values, duplicated keys, age > 110.

- **[P2.8]** [cg-testing] `R/pipdata_dlw_validation.R:291` — engine crashes on small/empty inputs
  for modules without `na_threshold_min` (`nrow <= 5` → `within_bounds(0, 0)` errors
  "lower bound must be strictly lower than upper bound"). Verify note: this mirrors legacy
  behavior (group/bin/hist had the guard, others did not); flagged for awareness under
  `deviation-policy: ask` rather than a new regression.
  **Fix**: guard the threshold floor in the engine, or document the legacy mirror explicitly.

### P3 — MINOR

- **[P3.1]** [cg-code-quality] `R/pipdata_dlw_validation.R:469-615` — deprecated wrappers carry no
  roxygen `@details` deprecation and no explicit `@keywords internal`; users calling legacy
  functions get no lifecycle note pointing to `dlw_validation_engine()`.
  **Fix**: add `@details` ("Deprecated: use `dlw_validation_engine()` with module ...") and
  `@keywords internal`.

- **[P3.2]** [cg-code-quality] `R/pipdata_dlw_validation.R:309, 328-329, 382-383, 367-371` —
  dead/redundant conditional guards (`required == FALSE || var %in%` never false; duplicated
  `next` conditions).
  **Fix**: collapse to single `if (!(var %in% df_var_list)) next`; document `required` semantics
  in the YAML header.

- **[P3.3]** [cg-code-quality] `R/pipdata_dlw_validation.R:411-417` — uniqueness report
  reimplementation diverges from `assertr::is_uniq` (hard-coded `num.violations = 1L`; cannot
  reproduce actual violation counts), so the NEWS "byte-identical" claim is unsubstantiated for
  the uniqueness row.
  **Fix**: restore `validate_if(..., is_uniq(...))` for the report row, or soften the NEWS claim.

- **[P3.4]** [cg-code-quality] `tests/testthat/test-dlw_validation_engine.R` +
  `tests/testthat/generate-fixtures.R` — nine `make_*_data()` generators are copy-pasted
  identically (~90 duplicated lines); editing one without the other desynchronizes fixtures.
  **Fix**: move generators to a shared `tests/testthat/helper-*.R`.

- **[P3.5]** [cg-code-quality] `tests/testthat/test-dlw_validation_spec.R:68-85` —
  "severity on helper-fixed check aborts" passes for the wrong reason: no `pattern`, so the
  missing-pattern abort fires before the intended inert-severity rule is exercised.
  **Fix**: add `pattern = "x"` to the spec.

- **[P3.6]** [cg-code-quality] `R/pipdata_dlw_validation.R:113-115, 253-255` —
  `numeric_validation = , character_validation = ,` switch fall-through is obscure; deserves a
  comment.
  **Fix**: add a one-line comment.

- **[P3.7]** [cg-testing] `tests/testthat/generate-fixtures.R:2` vs
  `tests/testthat/test-dlw_validation_engine.R:184-194` — RNG protocol mismatch: generator seeds
  once; test re-seeds per module. Works today because generators are value-insensitive, but will
  break once fixtures become value-sensitive.
  **Fix**: align the seeding protocols.

- **[P3.8]** [cg-testing] every engine invocation emits a tidyselect deprecation warning;
  full run produces 268 warnings, all in `dlw_validation_engine`. Noise floods summary output.
  **Fix**: `withr::local_options(lifecycle_verbosity = "quiet")` in a setup/helper, or fix the
  assertr call sites upstream.

- **[P3.9]** [cg-testing] `tests/testthat/test-dlw_validation_engine.R:248-255` — "no per-survey
  log_info" test deparses the function body (implementation detail, vacuously passes/breaks).
  **Fix**: replace with `withCallingHandlers`/`expect_no_message` behavioral test.

- **[P3.10]** [cg-testing] `compare_to_fixture` uses `sort()` per column (cannot detect row
  pairing corruption) and a relative fixture path instead of `testthat::test_path()`.
  **Fix**: use `test_path()`; optionally compare row-wise after `setorder`.

- **[P3.11]** [cg-testing] `tests/testthat/test-dlw_validation_spec.R` — empty-`checks` branch and
  entry-level invalid severity on numeric/character/validation-group entries are untested.
  **Fix**: add two mirror test cases.

## ✅ Passed

- Engine reuses existing check helpers (no logic duplication — [cg-code-quality]).
- Dispatch change in `pipdata_validate_gmd.R` is behavior-equivalent including `DEFAULT` → `skip`
  ([cg-code-quality]).
- `globalVariables` in `R/aaa.R` covers engine NSE symbols; no new undefined globals
  ([cg-code-quality]).
- Teardown hygiene (`pd_env_rm`/`withr::defer`) is clean; no `validation_report` leakage
  ([cg-testing]).
- Empty-regex-match and optional-column-omitted paths implicitly exercised via error fixtures
  ([cg-testing]).
- No cross-file breakage; removed symbols have no remaining references ([cg-code-quality]).

## Triage

Total findings: 18 (P0: 0, P1: 0, P2: 8, P3: 11 — counter-adjusted IDs). Next command:
`/cg-fix-triage`.