---
date: 2026-08-17
depth: light
parent-review: .cg-docs/reviews/2026-08-06-aux-version-gate-valid-dlw-load-revised-review.md
type: verification
findings:
  P1.1: fixed
  P2.1: fixed
  P2.2: fixed
  P3.1: fixed
  P3.2: skipped
  P3.3: fixed
---

# Verify Review: force_surveys delta

## Review Report

**Review mode**: light (verify)
**Prior review**: `.cg-docs/reviews/2026-08-06-aux-version-gate-valid-dlw-load-revised-review.md`
**Findings**: 6 (P0: 0, P1: 1, P2: 2, P3: 3)

Verify pass following fix-triage on the new `force_surveys` feature (the
force-surveys delta is NOT in the prior review's fixed scope, so none of these
findings are suppressed). Depth forced to `light`; dispatched `@cg-code-quality`
and `@cg-testing` only.

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** cg-code-quality `R/pd_process_data.R:63` — Public positional argument
  compatibility is silently broken.
  **Why**: `force_surveys = NULL` was inserted between `force` and `verbose`,
  shifting `verbose` one position. Existing callers passing `verbose`
  positionally as the 4th argument now bind it to `force_surveys`. This is the
  same bug class flagged as P1.4 in the prior review (mid-signature insertion),
  and `valid_dlw_load()` correctly appends `force_surveys` after `aux_hashes`,
  so the two signatures are now inconsistent.
  **Fix**: Move `force_surveys = NULL` to after `verbose` (append as the last
  parameter) in `pd_process_data()`, matching `valid_dlw_load()`. Add a
  positional-call regression test passing `verbose` in the 4th slot.

### P2 — IMPORTANT (should fix)

- **[P2.1]** cg-code-quality `R/valid_dlw_load.R:452-458` — `master_pip_key`
  reverse-map may be ambiguous when a `pip_id` appears on multiple historical
  master rows.
  **Why**: `build_pip_master_inventory` retains multiple `content_hash_dlw` rows
  per survey, so the same `pip_id` can repeat across historical rows. Indexing
  `master_pip_key[[toupper(id)]]` under duplicate names is name-index-dependent
  (first-hit), which can resolve to a stale/old-row `survey_id`, silently
  misclassifying a valid pip_id as unknown.
  **Fix**: Reduce `dt_master` to one row per `pip_id` (e.g. `collapse::funique()`
  on `.(pip_id, survey_id)` or latest `content_hash_dlw` row) before building
  the reverse-map; add a regression test with a repeated pip_id.

- **[P2.2]** cg-code-quality/cg-testing — `pd_process_data()` guard runs after the
  stamp-versioning side effect, and the mutual-exclusivity test touches real
  `stamp::st_opts`.
  **Why**: `if (force)` switches `stamp::st_opts(versioning="timestamp")` and
  registers `on.exit` before the guard aborts; when both `force` and
  `force_surveys` are set, stamp state is momentarily mutated. The
  `test-pd_process_data.R:13` mutual-exclusivity test passes `force=TRUE` without
  mocking `st_opts`, so it mutates real stamp state and could fail for the wrong
  reason.
  **Fix**: Move the mutual-exclusivity guard ahead of the stamp-versioning switch
  in `pd_process_data()`, and mock `stamp::st_opts` in the abort test.

### P3 — MINOR (nice to have)

- **[P3.1]** cg-code-quality `R/valid_dlw_load.R:418` — `resolve_force_surveys()`
  declares `verbose` but never uses it; the two `cli_alert_warning()` calls fire
  unconditionally. Gate on `verbose` or drop the parameter.
- **[P3.2]** cg-code-quality `R/pd_process_data.R:78` + `R/valid_dlw_load.R:109` —
  the mutual-exclusivity guard message is duplicated verbatim across both entry
  points. Extract to a shared helper/constant or document the single invariant
  location.
- **[P3.3]** cg-testing — two force_surveys branches are untested: (1) a pip_id
  reverse-mapping to a survey_id outside the module/latest filter (R9 unknown
  route); (2) lookup-first precedence when an identifier is both a survey_id and
  a pip_id.

### ✅ Passed

- cg-testing: offline R9-adjacent resolution, mixed-input dedup, load-once (C5),
  no-.joyn/no-dup invariants, force=TRUE→timestamp (R10), and force_surveys-never-
  st_opts (R3/C1) are all correctly covered.
- cg-code-quality: `forced_inv` computed before the empty-abort, union + unique
  dedup, single master load reuse, return-schema consistency — all correct.