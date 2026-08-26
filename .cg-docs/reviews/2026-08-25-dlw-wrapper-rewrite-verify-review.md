---
date: 2026-08-26
depth: light
parent-review: .cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-review.md
type: verification
findings:
  P0.1: fixed
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P1.5: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: skipped
  P2.4: fixed
---

# Verification Review Report

**Review mode**: light verification
**Parent review**: `.cg-docs/reviews/2026-08-25-dlw-wrapper-rewrite-review.md`
**Files reviewed**: all changed R, tests, documentation, and workflow artifacts
**Findings**: 10 (P0: 1, P1: 5, P2: 4, P3: 0)

## P0 - Blocking

- **[P0.1]** `R/pipdata_dlw_compare.R:574` - Corrupt stamp version rows can
  still be omitted because `stamp::st_versions()` warns and drops invalid
  `created_at` rows before the caller validates the returned table.
  **Why**: a dropped row may contain the highest validation pipeline version or
  make an existing artifact look absent.
  **Fix**: use one strict DLW version-catalog loader that converts corruption
  warnings into typed failures and validates returned IDs/uniqueness for
  history, absence, reconciliation, and checkpoint queries.

## P1 - Critical

- **[P1.1]** `R/pipdata_dlw_compare.R:364` - A zero-row artifact with an
  arbitrary schema bypasses required validation-inventory columns and becomes
  trustworthy canonical empty state.
  **Fix**: require full persisted schema for durable empty artifacts while
  retaining a deliberately lenient path only for schema-light caller empties.
- **[P1.2]** `R/pipdata_validate_gmd.R:400` - Canonical report ordering is not
  total for optional numeric `NA` versus `NaN`, so reversed rows can compare
  unequal.
  **Fix**: add deterministic missing-kind tie-breakers or stable serialized row
  keys, plus shuffled regression coverage.
- **[P1.3]** `R/pipdata_validate_gmd.R:724` - Completed worker normalization
  does not require inventory status/checksum/pipeline version to match the
  candidate and worker outcome.
  **Fix**: enforce exact survey ID, status, checksum, next version, and report
  classification agreement; isolate mismatches as `inventory_row` failures.
- **[P1.4]** `tests/testthat/test-dependency-execution.R:1` - Top-level fixture
  constructors in changed test files fail when testthat shuffles execution.
  **Fix**: move shared constructors into a `helper-*.R` file or make tests
  self-contained, then record a passing shuffled run.
- **[P1.5]** `tests/testthat/test-pipdata_validate_gmd.R:792` - Malformed engine
  coverage omits a missing `type` column and a nonempty blank `type` value.
  **Fix**: add both cases and require `validation_engine` failure with no
  persistable rows.

## P2 - Important

- **[P2.1]** `R/pipdata_dlw_compare.R:1065` - `dlw_gmd_list()` validates a
  malformed artifact ID only after write execution.
  **Fix**: validate `inv_gmd_list` before catalog, read, or write operations.
- **[P2.2]** `tests/testthat/test-pipdata_get_gmd.R:363` - Persistence tests use
  handcrafted results and do not fault real temporary stamp payload, sidecar,
  and catalog boundaries.
  **Fix**: add temporary-alias integration fault tests for intended/prior/
  absent/ambiguous durable outcomes.
- **[P2.3]** `tests/testthat/test-pipdata_dlw_process.R:473` - Custom inventory
  coverage verifies mocked forwarding but not validate-only execution with only
  a real custom acquisition artifact present.
  **Fix**: exercise temporary `dlw_inv` storage with the default artifact absent.
- **[P2.4]** `tests/testthat/test-pipdata_get_gmd.R:496` - Forced replacement is
  asserted only through a permissive mock and does not prove changed-checksum
  cached pins reach replacement.
  **Fix**: add a temporary cached-pin boundary test that proves replacement is
  attempted and cached/ambiguous returns cannot succeed.

## Verification Context

- E3: 317 passed.
- E4: 453 passed.
- Final suite before this review: 1667 passed with only the accepted baseline
  fingerprint failure.
- Package check: no new ERROR or WARNING relative to baseline.
- No files were modified by the verification agents.
