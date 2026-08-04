---
date: 2026-06-17
title: "Refactor valid_dlw_load: logging, abort, and content-hash comparison"
status: active
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-06-05-valid-dlw-load-review.md"
language: "R"
estimated-effort: "medium"
phases: 2
completed-phases: []
deviation-policy: "ask"
tags: [pipeline, inventory, valid_dlw_load, inv_to_process, filter_aux_inv, logging, content_hash]
---

# Plan: Refactor valid_dlw_load — logging, abort, and content-hash comparison

## Objective

Implement the 5 decisions from the 2026-06-05 brainstorm:

1. Remove the `max_year` temporary clamp in `filter_aux_inv`.
2. Split the all-NULL guard in `valid_dlw_load` into two distinct conditions with
   separate structured log entries.
3. Replace the silent `return(NULL)` at the combined check with `cli::cli_abort`
   (class `"piperr"`).
4. Add a `surveys_to_clean_inf` summary log entry after the rbind/dedup step.
5. Rewrite `inv_to_process` to compare `content_hash` on `survey_id` instead of
   using a 3-key identity anti-join.

## Context

`valid_dlw_load()` and its helpers (`filter_aux_inv`, `inv_to_process`) determine
which DLW surveys need re-processing. The current implementation has three known
problems:

- `filter_aux_inv` contains a hardcoded `max_year` clamp (lines 165–167) that is
  redundant because `joyn::inner_join` already discards unmatched rows. A "Temporary
  fix" comment confirms this was never meant to stay.
- The all-NULL guard (line 73) conflates two distinct pipeline states — no aux
  changes at all vs. aux changes that affect no surveys — and only emits a generic
  `cli_alert_info` rather than a structured logmeta entry.
- `inv_to_process` uses a 3-key anti-join (`country_code`, `surveyid_year`,
  `survey_acronym`) which misses surveys whose DLW file content changed without a
  key change. The master inventory already stores `content_hash_dlw` (populated by
  `build_pip_inventory`); joining on `survey_id` and comparing hashes is the correct
  approach.

Relevant solution docs:
- `.cg-docs/solutions/bugs/2026-06-05-joyn-anti-join-reportvar-duplicate-survey-id.md`
  — confirms all new `joyn::` calls must carry `reportvar = FALSE`.
- `.cg-docs/solutions/data-quality/2026-06-05-joyn-diagnostic-column-discipline.md`
  — project-wide `reportvar = FALSE` rule.

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | Remove the `max_year` clamp block from `filter_aux_inv` | Brainstorm Decision 1 |
| R2 | Split the all-NULL `if` into two conditions with distinct `log_info` calls: `"aux_no_changes_inf"` and `"aux_changes_no_surveys_inf"` | Brainstorm Decision 2 |
| R3 | Replace `return(NULL)` at combined check (lines 98–101) with `cli::cli_abort(class = "piperr")` | Brainstorm Decision 3 |
| R4 | Add `pipfun::log_info(..., logmeta = list(info = "surveys_to_clean_inf", ...))` after rbind/dedup | Brainstorm Decision 4 |
| R5 | Rewrite `inv_to_process` to join on `survey_id` and filter by `content_hash != content_hash_dlw \| is.na(content_hash_dlw)` | Brainstorm Decision 5 |
| R6 | Every new `joyn::` call carries `reportvar = FALSE` | Project rule (context.md) |
| R7 | Update roxygen `@details` to document the 3 new logmeta discriminator strings | Documentation standard |
| R8 | Add the 3 new discriminator strings to the canonical list in `compound-gpid.context.md` | Project rule (context.md) |

---

## Phase 1: Code changes in valid_dlw_load.R

### 1. Remove `max_year` clamp from `filter_aux_inv`

- **Requirements**: R1
- **Files**: `R/valid_dlw_load.R`
- **Details**: Delete the three-line block:
  ```r
  max_year <- max(inv[!is.na(inv$surveyid_year), ]$surveyid_year)
  changes <- changes[changes$surveyid_year <= max_year, ]
  ```
  and its surrounding comment ("Temporary fix to test data from Rossana"). The
  `joyn::inner_join` below already drops unmatched `surveyid_year` values.
- **Test Scenarios**: filter_aux_inv called with aux changes for a year beyond the
  max survey year → the join discards the unmatched year naturally; result is empty
  or correctly filtered.
- **Acceptance criteria**: No `max_year` variable or clamp expression in
  `filter_aux_inv`.

### 2. Split all-NULL guard into two distinct log conditions

- **Requirements**: R2
- **Files**: `R/valid_dlw_load.R`
- **Details**: Replace the existing single `if` block (lines ~73–80) with two
  sequential checks:

  ```r
  # Check 1: were any aux changes detected at all?
  if (is.null(all_changes_aux) || length(all_changes_aux) == 0) {
    pipfun::log_info(
      "No auxiliary file changes detected for survey cleaning.",
      name = "pipdata_log",
      logmeta = list(info = "aux_no_changes_inf")
    )
    inv_aux <- NULL
  } else if (all(vapply(ls_inv_aux, is.null, logical(1)))) {
    # Check 2: aux changed but no surveys match
    pipfun::log_info(
      "Auxiliary files changed but no surveys affected.",
      name = "pipdata_log",
      logmeta = list(
        info = "aux_changes_no_surveys_inf",
        measures = unique(unlist(lapply(all_changes_aux, names)))
      )
    )
    inv_aux <- NULL
  } else {
    inv_aux <- ls_inv_aux |>
      data.table::rbindlist() |>
      collapse::funique()

    # aux_changes_inf fires here, inside the branch where inv_aux is
    # actually non-empty -- not on the mere non-NULL-ness of all_changes_aux.
    changed_measures <- unique(unlist(lapply(all_changes_aux, names)))
    pipfun::log_info(
      "Auxiliary file changes detected.",
      name = "pipdata_log",
      logmeta = list(
        info = "aux_changes_inf",
        measures = changed_measures,
        n_surveys_affected = nrow(inv_aux),
        surveys_affected = inv_aux$survey_id
      )
    )
  }
  ```

  Delete the separate, standalone `if (!is.null(all_changes_aux))` block further
  down that currently logs `"aux_changes_inf"` — it is fully replaced by the log
  call now inside the `else` branch above. `aux_changes_inf` must fire exactly
  once, gated on `inv_aux` being non-NULL/non-empty (not on `all_changes_aux`
  alone), otherwise it would fire simultaneously with `aux_changes_no_surveys_inf`
  for the same event.
- **Test Scenarios**: (a) `all_changes_aux` is NULL → `"aux_no_changes_inf"` logged,
  `inv_aux` NULL; (b) `all_changes_aux` non-NULL but all `ls_inv_aux` elements NULL →
  `"aux_changes_no_surveys_inf"` logged, `inv_aux` NULL; (c) normal path → exactly
  one `"aux_changes_inf"` entry logged, from inside the `else` branch.
- **Acceptance criteria**: `"aux_no_changes_inf"`, `"aux_changes_no_surveys_inf"`,
  and `"aux_changes_inf"` are string literals; `logmeta$info` is never an R
  condition object; `"aux_changes_inf"` never fires alongside
  `"aux_changes_no_surveys_inf"` for the same call.

### 3. Replace silent `return(NULL)` with `cli_abort`

- **Requirements**: R3
- **Files**: `R/valid_dlw_load.R`
- **Details**: Replace the combined check block (lines ~98–101):
  ```r
  if (
    (is.null(inv_svy) || nrow(inv_svy) == 0) &&
      (is.null(inv_aux) || nrow(inv_aux) == 0)
  ) {
    return(NULL)
  }
  ```
  with:
  ```r
  if (
    (is.null(inv_svy) || nrow(inv_svy) == 0) &&
      (is.null(inv_aux) || nrow(inv_aux) == 0)
  ) {
    cli::cli_abort(
      "No surveys to process: all surveys are up to date and no auxiliary changes affect any survey.",
      class = "piperr"
    )
  }
  ```
- **Test Scenarios**: both `inv_svy` and `inv_aux` empty → `cli_abort` with class
  `"piperr"` raised; only `inv_svy` empty but `inv_aux` non-empty → no abort; vice
  versa.
- **Acceptance criteria**: `expect_error(..., class = "piperr")` in tests; no silent
  `return(NULL)` path remains.

### 4. Add `surveys_to_clean_inf` summary log entry

- **Requirements**: R4
- **Files**: `R/valid_dlw_load.R`
- **Details**: After `inv_to_clean <- unique(rbind(inv_svy, inv_aux, fill = TRUE))`
  and before `setorder`, insert:
  ```r
  pipfun::log_info(
    "Surveys identified for cleaning.",
    name = "pipdata_log",
    logmeta = list(
      info = "surveys_to_clean_inf",
      n_dlw_new      = if (is.null(inv_svy)) 0L else nrow(inv_svy),
      n_aux_changed  = if (is.null(inv_aux)) 0L else nrow(inv_aux),
      n_total_unique = nrow(inv_to_clean),
      aux_measures_triggered = if (is.null(all_changes_aux)) character(0)
                               else unique(unlist(lapply(all_changes_aux, names)))
    )
  )
  ```
- **Test Scenarios**: verify logmeta counts match the size of `inv_svy`/`inv_aux`
  fed in; verify `n_total_unique` equals `nrow(result)`.
- **Acceptance criteria**: log entry present in `pipdata_log` after a successful call;
  `n_total_unique` is never larger than `n_dlw_new + n_aux_changed` (dedup ensures ≤).

---

## Phase 2: Rewrite `inv_to_process` + tests + documentation

### 5. Rewrite `inv_to_process` with content_hash comparison

- **Requirements**: R5, R6
- **Files**: `R/valid_dlw_load.R`
- **Details**: Replace the entire function body with a `joyn::left_join` on
  `survey_id`, comparing `content_hash` (DLW) against `content_hash_dlw` (master).
  **`verbose` must be kept in the signature and propagated** — the existing call
  site in `valid_dlw_load()` (`inv_to_process(inv_svy, verbose = verbose)`) is
  unchanged and would error on an `unused argument` if `verbose` were dropped.
  **`dt_master` must be deduplicated by `survey_id` before the join** — the master
  inventory's primary key is `c("survey_id", "pip_id")` (see `build_pip_inventory.R`),
  so a single `survey_id` can have multiple rows (e.g. `BOL_2022_EH` splits into
  `..._INC_ALL` / `..._INC_GPWG`). Joining on `survey_id` alone against a
  non-deduplicated table fans out matching rows in `inv`, reintroducing the
  duplicate-`survey_id` bug class from
  `.cg-docs/solutions/bugs/2026-06-05-joyn-anti-join-reportvar-duplicate-survey-id.md`:

  ```r
  inv_to_process <- function(inv, verbose = TRUE) {
    dt_master <- tryCatch(
      pipload::load_pip_master_inventory(verbose = verbose),
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(
            "Could not load PIP master inventory. Processing all surveys."
          )
        }
        return(NULL)
      }
    )

    if (is.null(dt_master)) return(inv)

    # Deduplicate by survey_id: content_hash_dlw is expected to be identical
    # across pip_id splits of the same survey_id, but the join must not rely
    # on that being true for row cardinality -- dedup first, and let
    # relationship = "many-to-one" raise if it is ever violated.
    dt_master_hash <- collapse::funique(dt_master[, .(survey_id, content_hash_dlw)])

    # Join on survey_id to compare content hashes
    inv_compare <- joyn::left_join(
      inv,
      dt_master_hash,
      by = "survey_id",
      relationship = "many-to-one",
      verbose = FALSE,
      reportvar = FALSE
    )

    # Keep: new surveys (NA hash in master) or surveys whose DLW content changed
    inv_changed <- inv_compare[
      is.na(content_hash_dlw) | content_hash != content_hash_dlw
    ]
    inv_changed[, content_hash_dlw := NULL]

    inv_changed
  }
  ```

  Remove the `_dlw` suffix-strip block — it is no longer needed because the join
  uses `content_hash_dlw` directly without renaming.

  **Blocked-stop condition**: if `content_hash_dlw` is absent from
  `load_pip_master_inventory()` output → stop and report to user; do not guess a
  column name.

- **Test Scenarios**:
  - Survey in DLW but not in master (`content_hash_dlw` is NA) → kept.
  - Survey in master with same hash → excluded (already clean).
  - Survey in master with different hash → kept (DLW content changed).
  - Survey with multiple `pip_id` rows in master (same `content_hash_dlw`) → only
    one output row per `survey_id`, no fan-out duplication.
  - `load_pip_master_inventory()` throws error → returns all surveys, no abort;
    `verbose = FALSE` suppresses the warning message.
  - `.joyn` column must not be present in result (regression guard from 2026-06-05 bug).
- **Acceptance criteria**: all 6 scenarios pass; no `.joyn` column; no `_dlw` suffix
  renaming in function body; `verbose` param present and propagated; `dt_master`
  hash lookup deduplicated by `survey_id` before the join.

### 6. Update tests in test-valid_dlw_load.R

- **Requirements**: R5, R6 (test coverage for Steps 1–5)
- **Files**: `tests/testthat/test-valid_dlw_load.R`
- **Details**:
  - **Step 1**: Add a `filter_aux_inv` test with an aux change year beyond `max(inv$surveyid_year)` — verify the join handles it without error and returns empty result.
  - **Step 2**: Add two `valid_dlw_load` tests mocking `valid_aux_load` to return (a) `NULL` and (b) non-NULL but `filter_aux_inv` returns all-NULL; assert correct logmeta `info` value captured.
  - **Step 3**: Add a test where both `inv_svy` and `inv_aux` are empty after filtering; assert `expect_error(..., class = "piperr")`.
  - **Step 4**: In the happy-path test, capture the `pipdata_log` and assert a `surveys_to_clean_inf` entry exists with correct `n_total_unique`.
  - **Step 5**: Add the 6 content-hash scenarios described above for `inv_to_process`.
  - Update **both** existing tests that mock `load_pip_master_inventory` with the old
    3-key-only structure: (1) the direct `inv_to_process` unit test, and (2) the
    `"valid_dlw_load returns no duplicate survey_ids..."` test (~line 90–114), which
    exercises `inv_to_process` indirectly via `valid_dlw_load(force = FALSE)`. Grep
    the test file for all `load_pip_master_inventory = function` mocks before
    starting Phase 2 to confirm no others were missed.
- **Acceptance criteria**: `devtools::test(filter = "valid_dlw")` passes; no test
  fixtures reference `country_code`/`surveyid_year`/`survey_acronym` as the
  comparison keys; all `load_pip_master_inventory` mocks in the file include
  `survey_id` and `content_hash_dlw`.

### 7. Update roxygen @details and compound-gpid.context.md

- **Requirements**: R7, R8
- **Files**: `R/valid_dlw_load.R`, `compound-gpid.context.md`
- **Details**:
  - In `valid_dlw_load` roxygen `@details` block, add the three new logmeta
    discriminator strings to the logging section:
    - `"aux_no_changes_inf"` — emitted when no auxiliary files changed.
    - `"aux_changes_no_surveys_inf"` — emitted when auxiliary files changed but no
      surveys match.
    - `"surveys_to_clean_inf"` — emitted after rbind/dedup with counts.
  - In `compound-gpid.context.md`, re-read the **current** canonical logmeta list at
    implementation time (do not trust this plan's snapshot — as of the plan-review
    pass on 2026-08-03 it already lists six entries, not four) and add the three new
    strings, updating the count/prose to match whatever is actually there.
- **Acceptance criteria**: three new strings appear in both locations; the canonical
  list's count/prose accurately reflects the total entries present after the edit
  (verify by counting, don't assume a fixed number).

## Testing Strategy

- All tests use `testthat::local_mocked_bindings()` for `pipload::load_pip_master_inventory`
  and `pipdata::valid_aux_load`.
- `make_dlw_inv()` helper in the existing test file already provides `content_hash`
  and `survey_id` columns — extend it with `content_hash_dlw` for master mock rows.
- Log capture: use `pipfun::log_get("pipdata_log")` or check `pipdata_log` object
  post-call; verify logmeta entries by `info` discriminator.
- Run `devtools::test(filter = "valid_dlw")` after Phase 1 and again after Phase 2
  before the full `devtools::test()` run.

## Documentation Checklist

- [ ] `valid_dlw_load` roxygen `@details` updated with 3 new logmeta strings
- [ ] `compound-gpid.context.md` canonical logmeta list updated
- [ ] `man/valid_dlw_load.Rd` regenerated via `devtools::document()`

## Risks & Mitigations

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| `content_hash_dlw` absent from master inventory (schema drift) | Low | Blocked-stop condition in Step 5; check column names before implementing |
| `joyn::left_join` column suffix collision (`content_hash.x`/`.y`) | Low | `content_hash_dlw` is only in `dt_master`; `content_hash` only in `inv` — no collision; verify with `names()` after join |
| `dt_master` has multiple rows per `survey_id` (one per `pip_id`), causing join fan-out | Medium | Dedup `dt_master[, .(survey_id, content_hash_dlw)]` via `collapse::funique()` before the join; `relationship = "many-to-one"` raises if violated |
| Existing tests break because they mock 3-key master columns | Medium | Step 6 explicitly updates **both** affected test mocks |
| `pipdata_log` not initialized in test context | Low | Existing tests already initialize it via helpers; reuse pattern |
| Dropping `verbose` from `inv_to_process()` breaks the existing call site | High (caught in plan review) | New signature retains `verbose`, propagated to `load_pip_master_inventory()` and gating the warning message |

## Out of Scope

- `pd_process_data.R`, `build_pip_inventory.R`, `valid_aux_load.R` — no changes.
- DLW wrapper rewrite (`dlw-wrapper-rewrite` roadmap feature).
- Unified logging harmonization across the full package (`unified-logging-report`).
- `Checksum` column — must NOT be used as a fallback comparison key.

---

## Completion Contract

### Outcome

`valid_dlw_load()`, `filter_aux_inv()`, and `inv_to_process()` are refactored per
the 5 decisions in the 2026-06-05 brainstorm. The pipeline emits structured log
entries for the no-change and summary states, fails fast via `cli_abort("piperr")`
when truly nothing to clean, and `inv_to_process()` detects DLW source changes via
`content_hash` comparison on `survey_id`.

### Verification Surface

| ID | Evidence Required | Command/Artifact | Required |
|----|-------------------|------------------|----------|
| V1 | `max_year` clamp block removed from `filter_aux_inv` | code review | yes |
| V2 | Two distinct `if` branches replace the single all-NULL guard; `"aux_no_changes_inf"` and `"aux_changes_no_surveys_inf"` used as logmeta discriminators | code review + test | yes |
| V3 | Combined-check `return(NULL)` replaced by `cli::cli_abort(class = "piperr")` | code review + test | yes |
| V4 | `log_info(..., logmeta = list(info = "surveys_to_clean_inf", ...))` present after rbind/dedup | code review + test | yes |
| V5 | `inv_to_process(inv, verbose = ...)` retains `verbose`, propagates it to `load_pip_master_inventory(verbose = verbose)`, gates its `cli_alert_warning` on `verbose`; joins on `survey_id` against a deduplicated `dt_master[, .(survey_id, content_hash_dlw)]`; call site in `valid_dlw_load()` unchanged | code review | yes |
| V6 | `devtools::test(filter = "valid_dlw")` passes | terminal | yes |
| V7 | `devtools::test()` full suite passes with no regressions | terminal | yes |
| V8 | `aux_changes_inf` log call fires exactly once, from inside the `else` branch of the 3-way split, gated on `inv_aux` being non-NULL/non-empty; no standalone duplicate log block remains | code review + test | yes |

### Constraints

| ID | Constraint | Check |
|----|------------|-------|
| C1 | Every new `joyn::` call carries `reportvar = FALSE` | code review |
| C2 | `logmeta$info` values are string literals; never R condition objects | code review |
| C3 | `inv_to_process` returns all surveys when master cannot be loaded (graceful fallback) | code review + test |
| C4 | `Checksum` is NOT used as a fallback for content comparison | code review |
| C5 | Three new logmeta discriminator strings added to canonical list in `compound-gpid.context.md` (re-count actual entries at implementation time; do not assume "four") | documentation step |
| C6 | `dt_master[, .(survey_id, content_hash_dlw)]` deduplicated via `collapse::funique()` before the join; `joyn::left_join(..., relationship = "many-to-one")` set explicitly | code review + test |
| C7 | Both `load_pip_master_inventory` mocks in `test-valid_dlw_load.R` (direct `inv_to_process` test and the duplicate-survey_id test) updated to the new schema | code review + test |

### Boundaries

- **Allowed**: `R/valid_dlw_load.R`, `tests/testthat/test-valid_dlw_load.R`,
  `man/valid_dlw_load.Rd`, `compound-gpid.context.md`
- **Out of scope**: `pd_process_data.R`, `build_pip_inventory.R`, `valid_aux_load.R`,
  any DLW wrapper or unified logging work

### Iteration Policy

1. Implement Phase 1 steps (1–4) in order; run `devtools::test(filter = "valid_dlw")`
   before starting Phase 2.
2. Phase 2 step 5 replaces `inv_to_process` entirely — do not patch incrementally;
   replace the full function body.
3. If `content_hash_dlw` is absent from master inventory output → stop and report
   column names; do not proceed.
4. If `devtools::test()` shows regressions outside `test-valid_dlw_load.R` → stop
   before completing the phase.

### Blocked-Stop Conditions

- `content_hash_dlw` column absent from `pipload::load_pip_master_inventory()` output.
- `devtools::test()` regressions in files other than `test-valid_dlw_load.R`.
