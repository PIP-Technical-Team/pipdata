---
date: 2026-08-26
depth: architecture
type: standard
plan: .cg-docs/plans/2026-08-25-dlw-wrapper-rewrite.md
findings:
  P0.1: fixed
  P0.2: fixed
  P0.3: fixed
  P0.4: fixed
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: fixed
  P2.6: fixed
---

# Review Report

**Review mode**: architecture
**Scope**: DLW wrapper rewrite against synchronized base
`55b7a3a1369504dcee8754237bedc260128d7d97`
**Agents**: code quality, testing, documentation, version control,
reproducibility, performance, architecture, and data quality
**Findings**: 14 fixed (P0: 4, P1: 4, P2: 6)

## P0 - Blocking

- **[P0.1]** `R/pipdata_dlw_compare.R` - Catalog-listed unreadable or malformed
  validation history was omitted from pipeline-version maxima. **Fix**: fail
  closed with typed history errors and stream only verified per-version maxima.
- **[P0.2]** `R/pipdata_validate_gmd.R` - Missing, empty, or unknown engine
  result types could be classified valid. **Fix**: require nonempty
  `success|warning|error` types and agreement with extracted report rows.
- **[P0.3]** `R/pipdata_validate_gmd.R` - Optional report schema checks ignored
  coercion-relevant attributes. **Fix**: compare full relevant attributes and
  reject incompatible/additive raw fields.
- **[P0.4]** `R/pipdata_dlw_compare.R` - A transient empty server catalog could
  erase authoritative state. **Fix**: reject zero supported server rows as a
  catalog-load failure before merge or persistence.

## P1 - Critical

- **[P1.1]** `R/pipdata_validate_gmd.R` - Trustworthy unchanged artifacts with
  `success = NA` forced false failed outcomes. **Fix**: treat trustworthy facts
  whose success is not `FALSE` as verified.
- **[P1.2]** `R/pipdata_validate_gmd.R` - Report canonical ordering omitted
  persisted tie-breakers. **Fix**: sort deterministically across all columns.
- **[P1.3]** `R/dependency_execution.R` - Duplicate completed survey rows could
  enter cleaning planning. **Fix**: canonicalize empties, deduplicate exact
  rows, and reject conflicting duplicate survey IDs.
- **[P1.4]** `R/pipdata_dlw_process.R` - Interactive delegate errors could
  return stale pre-attempt inventory. **Fix**: always reload durable state after
  an escaped delegate error.

## P2 - Important

- **[P2.1]** Persistence missing-state checks now reject malformed version
  catalogs rather than treating them as absence.
- **[P2.2]** Checkpoint direct success now rejects malformed `skipped` values
  and reconciles them against durable state.
- **[P2.3]** Public `verbose` now reaches acquisition and validation worker I/O.
- **[P2.4]** `dlw_gmd_unvalidated()` restored invisible-return compatibility.
- **[P2.5]** Validation report extraction and merge now avoid repeated
  whole-report canonicalization and cumulative pairwise reduction.
- **[P2.6]** Public docs now pin artifact schemas, report-unavailable retries,
  required setup arguments, and standalone setup examples.

## Verification

- E3: 317 passed, 0 failed.
- E4: 453 passed, 0 failed.
- E5: 194 passed, 0 failed; 354 protected dependency warnings.
- Final suite: 1667 passed; only the accepted synchronized baseline fingerprint
  failure remains.
- Package check: no new ERROR or WARNING relative to baseline.
- Implementation allowlist: 36 of 36 paths passed before this review artifact
  was written; this review is a post-implementation workflow record.

## Residual Advisories

- Historical maximum recovery must scan existing inventory versions because no
  new durable retry ledger/index is permitted by the plan. The implementation
  streams maxima to bound memory.
- `file_path` and `date_validated` remain operational provenance fields as
  explicitly pinned; they are not bitwise relocation/rebuild invariants.
- The accepted `data_level_column` fingerprint failure predates this branch and
  remains outside the plan path boundary.
