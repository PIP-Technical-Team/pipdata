---
date: 2026-08-15
title: "Add force_surveys parameter for surgical survey re-processing"
status: decided
chosen-approach: "Per-survey force_surveys vector accepted by valid_dlw_load(); bypasses inv_to_process only; preserves content versioning; accepts both survey_id and pip_id via master reverse-map"
participants:
  - User
  - Kilo
tags: [pd_process_data, valid_dlw_load, force, stamp, versioning, master-inventory, pipeline]
---

# Add `force_surveys` parameter for surgical survey re-processing

## Context

`force = TRUE` in `pd_process_data()` (`R/pd_process_data.R:61-64`) does two
things at once:

1. **Global stamp side-effect**: switches `stamp::st_opts(versioning =
   "timestamp")` for the entire run. Under timestamp versioning, every
   re-saved survey gets a *new* version even when its content is byte-identical
   to the existing version, polluting version history for every survey in the
   run.
2. **Bypass all invalidation** via `valid_dlw_load(force = TRUE)`: skips the
   master-inventory load, `inv_to_process()` (the DLW content-hash
   comparison), aux-hash candidate detection, and `valid_aux_load()` — so
   *every* survey is reprocessed, not just the ones that actually changed.

Production needs the ability to re-run specific surveys (e.g. after fixing a
cleaning bug for one country) **without** the destructive global side effects
of `force = TRUE`. The downstream assembler `build_pip_inventory()` is already
a delta upsert (`R/build_pip_inventory.R:1-16`): it reads version facts for
current-run surveys only and retains all others unchanged. So the blast radius
of `force = TRUE` is confined to (1) the stamp versioning switch and (2) the
over-broad candidate set — not the assembler.

Goal: add a `force_surveys` parameter that re-processes a specified subset of
surveys while preserving normal content versioning and leaving the
invalidation logic intact for all other surveys.

## Requirements

- R1: Re-process only the surveys named in `force_surveys`, plus whatever the
  normal invalidation logic would already select (DLW-new + aux-changed). The
  forced subset is *added* to the candidate set, not a replacement for it.
- R2: Forced surveys bypass `inv_to_process()` (the master content-hash gate
  that removes already-cleaned surveys) — they may already be cleaned with an
  identical content hash, and that is the whole point of forcing them.
- R3: Stamp versioning stays on its default (`"content"`) for the entire run.
  `force_surveys` must NEVER touch `st_opts()`. Idempotent re-saves produce no
  new version; genuinely changed output produces a new content version
  (correct, meaningful history).
- R4: `force_surveys` and `force = TRUE` are mutually exclusive. Specifying
  both is an error — they conflict on both scope (all vs subset) and
  versioning (timestamp vs content).
- R5: `force_surveys` accepts both `survey_id` and `pip_id`, auto-detected.
  `survey_id` is the pipeline's natural key at this stage; `pip_id` is
  supported for ergonomics (production operators may know the PIP identifier).
- R6: An identifier in `force_surveys` that matches neither a `survey_id` in
  the (module-filtered, latest-version) inventory nor a `pip_id` in the master
  inventory is warned about, logged, and skipped — it does not abort the run.
- R7: The "nothing to clean" abort (`valid_dlw_load.R:280-288`) must account
  for forced surveys: a run whose normal candidates are empty but whose
  forced surveys are non-empty must proceed, not abort.
- R8: Auditability — emit log entries recording which forced surveys were
  retained and which identifiers could not be resolved.

## Approaches Considered

### Approach 1: Filter inside `valid_dlw_load()` (union forced surveys)

Add `force_surveys` as a parameter to `valid_dlw_load()`. After computing the
normal candidate set (`inv_svy` from `inv_to_process()` ∪ `inv_aux` from aux
detection), union in the rows of `inv_svy_full` whose `survey_id` matches the
resolved forced set. `pd_process_data()` validates mutual exclusivity with
`force`, then threads `force_surveys` through to `valid_dlw_load()`.

**Pros**:
- Keeps all invalidation logic in the function that already owns it.
- `pd_process_data()` stays a thin orchestrator.
- Forced surveys naturally intersect with `inv_svy_full` (already
  module-filtered + last-version), so module filtering and version selection
  apply for free.
- Aux detection runs unchanged; `unique()` (line 294) dedups overlaps.

**Cons**:
- `valid_dlw_load()` signature grows by one parameter.

**Effort**: Small

### Approach 2: Pre-filter `inv` in `pd_process_data()` before calling `valid_dlw_load()`

Filter the DLW inventory down to `force_surveys` before the call, so
`valid_dlw_load()` only ever sees the forced subset.

**Pros**:
- No change to `valid_dlw_load()`.

**Cons**:
- Loses the normal invalidation logic entirely for the run — you could not
  force one survey *and* still pick up new/aux-changed surveys in the same
  run. This violates R1 (forced subset is additive).
- Defeats the purpose of a surgical override that coexists with normal
  detection.

**Effort**: Small

**Recommended?**: No — violates the additive requirement (R1).

### Approach 3: Separate forced-survey injection point in `pd_process_data()`, post-`valid_dlw_load()`

Run `valid_dlw_load()` normally, then inject forced survey rows back into
`inv_to_clean` afterwards in `pd_process_data()`.

**Pros**:
- `valid_dlw_load()` untouched for the normal path.

**Cons**:
- Splits invalidation logic across two functions; the "what gets cleaned"
  decision is no longer made in one place.
- Forced surveys would bypass module filtering / `last_ver_inv()` unless
  re-applied — error-prone.
- The emptiness abort inside `valid_dlw_load()` would fire before injection
  could rescue a forced-only run, requiring awkward special-casing.

**Effort**: Medium

**Recommended?**: No — fragments the logic and reintroduces the abort hazard.

## Decision

**Approach 1**: filter inside `valid_dlw_load()`, unioning forced surveys into
the candidate set.

### Resolved decision points

1. **Where to filter**: inside `valid_dlw_load()`. Compute the normal candidate
   set, then union in `inv_svy_full` rows matching the resolved forced
   survey_ids. `pd_process_data()` validates mutual exclusivity with `force`
   and threads the parameter through.

2. **Bypass scope**: forced surveys bypass `inv_to_process()` only. Aux-change
   detection runs normally over `inv_svy_full`; any forced survey that also
   appears as an aux candidate is deduplicated by `unique()` at line 294. No
   special aux exclusion — simpler and correct.

3. **Versioning**: content (stamp default). The `if (force) { st_opts(...) }`
   block at `pd_process_data.R:61-65` stays gated purely on `force`,
   never on `force_surveys`. Forced surveys that produce byte-identical output
   create no new version (idempotent); genuinely changed output creates a new
   content version (meaningful history).

4. **Interaction with `force = TRUE`**: error if both supplied.
   `cli::cli_abort(class = "piperr")` with a clear message that they conflict
   on scope and versioning. This matches the codebase pattern for invalid
   input combinations.

5. **Identifier type**: accept both `survey_id` and `pip_id`, auto-detected.
   Resolution order per identifier:
   - If it matches a `survey_id` in `inv_svy_full` → use directly.
   - Else if it matches a `pip_id` in the master inventory → reverse-map to
     its `survey_id` (the master is keyed one-row-per-`pip_id`, so the map is
     unique; survey_id → pip_id is 1:many, so the reverse is clean).
   - Else → unknown: warn + log + skip (R6).
   Auto-detection is by **lookup-first**, not by pattern matching, because a
   survey_id that exists in the inventory but happens to deviate from the
   canonical `CCC_YYYY_ACRONYM` pattern would be misclassified by a pattern
   rule. Lookup-first is strictly more robust.

   The pip_id reverse-map reuses the master inventory that is *already*
   loaded when `!force` (`valid_dlw_load.R:109-122`). Because `force_surveys`
   is mutually exclusive with `force = TRUE` (decision 4), the master is
   always available in a `force_surveys` run — no extra load cost. If the
   master failed to load (the `error = function(e) NULL` path), pip_ids in
   `force_surveys` cannot be resolved and are treated as unknown (warn + log
   + skip); pure survey_ids still work because they do not need the master.

### Edge-case handling

- **Unknown identifier** (neither survey_id nor pip_id): warn, log
  `force_surveys_unknown_inf` with the unresolved identifiers, skip. Does not
  abort.
- **Forced survey outside the module filter**: excluded by the intersection
  with `inv_svy_full` (already module-filtered). If *none* of the forced
  identifiers resolve to a survey in `inv_svy_full`, warn that no forced
  surveys matched the filtered inventory.
- **Nothing-to-clean abort** (line 280-288): the emptiness check must include
  the forced set. A run with empty normal candidates but non-empty forced
  surveys proceeds.
- **Logging**: emit `force_surveys_inf` listing resolved forced survey_ids
  (distinguishing those resolved from survey_id vs pip_id), and
  `force_surveys_unknown_inf` for unresolved identifiers.

## Consequences

- `valid_dlw_load()` gains a `force_surveys` parameter (default `NULL`/empty
  → current behavior unchanged). `pd_process_data()` gains the same parameter
  and a mutual-exclusivity guard against `force`.
- Production can re-clean a single country's survey without destroying
  version history for every other survey in the run.
- `force = TRUE` remains unchanged for backward compatibility; it is now the
  "nuclear" option, with `force_surveys` as the surgical one.
- Accepting `pip_id` introduces a soft dependency on the master inventory for
  identifier resolution — but only for pip_id inputs, and the master is
  already loaded in every `force_surveys` run, so there is no new I/O cost.

## Next Steps

1. Add `force_surveys = NULL` parameter to `pd_process_data()` with
   mutual-exclusivity validation against `force` (error, class `piperr`).
   Thread it into the `valid_dlw_load()` call.
2. Add `force_surveys = NULL` parameter to `valid_dlw_load()`. Implement
   identifier resolution (survey_id lookup → pip_id reverse-map via master →
   unknown warn/log/skip) and union the resolved rows from `inv_svy_full`
   into the candidate set after the normal `inv_svy` / `inv_aux` computation.
3. Update the emptiness abort (line 280-288) to account for the forced set.
4. Add `force_surveys_inf` and `force_surveys_unknown_inf` log entries.
5. Update roxygen for both functions; document the mutual-exclusivity rule
   and the survey_id/pip_id auto-detection.
6. Add tests: (a) forced survey already-cleaned is retained; (b) forced +
   normal candidates union and dedup; (c) mutual-exclusivity error; (d) pip_id
   reverse-map; (e) unknown identifier warn+skip; (f) forced-only run does not
   abort; (g) stamp versioning stays on content (no `st_opts` call) when only
   `force_surveys` is set.
