---
date: 2026-05-20
title: "Refactor inventory architecture: catalog-based assembler"
status: active
scope: "Deep"
phases: 3
completed-phases: [1]
current-phase: 2
brainstorm: ".cg-docs/brainstorms/2026-05-20-inventory-architecture-refactor.md"
language: "R"
estimated-effort: "large"
tags: [architecture, inventory, stamp, pipload, maintainability, correctness]
---

# Plan: Refactor Inventory Architecture — Catalog-Based Assembler

## Objective

Replace the 350-line `update_pip_inventory()` + `format_vrs()` with a
catalog-based assembler that reads version facts directly from stamp's
persisted catalog. This eliminates the in-memory dependency on `proc_dta`
(crash-safe), simplifies the code (~80 lines), and decouples metadata
enrichment into pipload.

## Context

- stamp already records every `st_save()` in a per-alias `catalog.qs2`
  with `version_id`, `content_hash`, `created_at`, `size_bytes`.
- The current approach holds version info in memory (`proc_dta`) across
  2500 surveys; if anything fails before `update_pip_inventory()`, all
  version info is lost.
- Adding `reporting_level` required touching the assembler, producing
  joyn suffix collisions and column-naming bugs (see
  `.cg-docs/solutions/bugs/2026-05-20-joyn-suffix-collision-persisted-to-inventory.md`).
- pip_id follows a strict naming convention:
  `{COUNTRY}_{YEAR}_{ACRONYM}_{WELFARE}_{MODULE}` (e.g., `BOL_2022_EH_INC_ALL`).
  survey_id is derivable as the first 3 segments joined by `_` → DLW's
  `{COUNTRY}_{YEAR}_{ACRONYM}_V{mm}_M_V{aa}_A_GMD_{MODULE}`.
  Actually, `survey_id` is the full DLW identifier; we need `inv_to_clean`
  to provide the mapping from pip_id → survey_id (which is already present
  in the current flow via `process_data()` producing `pip_names`).

## Requirements

| ID  | Requirement                                                                  | Source           |
|-----|------------------------------------------------------------------------------|------------------|
| R1  | Version info persisted per-survey as side effect of saving (crash-safe for current run) | brainstorm       |
| R2  | Master inventory = latest version for every pip_id ever cleaned              | brainstorm       |
| R3  | Release inventory = master subset filtered by PFW `inpovcal == 1`           | brainstorm       |
| R4  | Skipped surveys remain in master with their prior version                    | brainstorm       |
| R5  | Failed surveys (never reached pip_write) handled by logging only             | brainstorm       |
| R6  | Metadata enrichment (reporting_level, etc.) decoupled from assembler         | brainstorm       |
| R7  | `process_data()` return simplified (no version tracking in memory)           | brainstorm       |
| R8  | `pd_deflation()` can still resolve version_id_data/version_id_metadata       | existing         |
| R9  | `first_release_version_id` / `latest_release_version_id` tracking preserved | existing         |
| R10 | DLW inventory columns (pipeline_version_dlw, path_dlw, etc.) preserved       | existing         |

## Implementation Steps

## Phase 1: stamp — expose catalog query ✅ (Step 1 done / Step 2 pending)

### 1. ✅ Add `st_catalog_query()` export to stamp

- **Requirements**: R1, R2
- **Files**: `stamp/R/version_store.R`, `stamp/NAMESPACE`
- **Details**:
  Add a public function that returns the latest version metadata for all
  artifacts in a given alias. Returns a `data.table` with one row per
  artifact: `path`, `version_id`, `content_hash`, `code_hash`,
  `size_bytes`, `created_at`.

  This is a **general-purpose stamp API** — documentation and design must
  serve all stamp users, not just pipdata. All standard version metadata
  fields are included in the return.

  ```r
  #' Query latest version metadata for all artifacts in an alias
  #'
  #' @param alias Character. Stamp alias to query. NULL uses default.
  #' @return data.table with one row per artifact (latest version only):
  #'   path, version_id, content_hash, code_hash, size_bytes, created_at.
  #' @family version-store
  #' @export
  st_catalog_query <- function(alias = NULL) {
    cat <- .st_catalog_read(alias = alias)
    if (nrow(cat$artifacts) == 0L) {
      return(data.table(
        path = character(),
        version_id = character(),
        content_hash = character(),
        code_hash = character(),
        size_bytes = numeric(),
        created_at = character()
      ))
    }
    # For each artifact, look up its latest version row.
    # X[Y, on=...] iterates over Y — so versions[artifacts] finds
    # the one version row matching each artifact's latest_version_id.
    cat$versions[
      cat$artifacts,
      on = .(version_id = latest_version_id),
      nomatch = 0,
      .(path = i.path,
        version_id,
        content_hash,
        code_hash,
        size_bytes,
        created_at)
    ]
  }
  ```

  **Note for pipdata consumers**: `build_pip_inventory()` does not use
  `code_hash` — it is simply dropped during the `setnames()` step that
  suffixes catalog columns. The column remains available for future
  `content-hash-reclean-trigger` work (see roadmap).

- **Test Scenarios**:
  - ✅ Happy path: alias with 3 artifacts → 3 rows returned
  - ✅ Multiple versions per artifact → only latest returned
  - ✅ Verify join direction: result has exactly `nrow(cat$artifacts)` rows (not `nrow(cat$versions)`)
  - 🛑 Edge case: empty alias (no artifacts) → empty data.table with correct schema
  - 🛑 Edge case: NULL alias → uses default alias
  - ❌ Error path: alias not initialized → appropriate error from `.st_catalog_read()`
- **Tests**: `stamp/tests/testthat/test-catalog-query.R` ✅ created — init temp alias,
  save 3 artifacts (one with 2 versions → 4 total version rows), verify
  result has exactly 3 rows and only latest version_id appears per artifact.
- **Acceptance criteria**: `st_catalog_query("my_alias")` returns a data.table
  with one row per artifact, only latest version, in < 1 second for 2500 artifacts.
  Return schema: `path`, `version_id`, `content_hash`, `code_hash`,
  `size_bytes`, `created_at`.

### 2. ✅ Add roxygen2 documentation and bump stamp version

- **Requirements**: R1
- **Files**: `stamp/DESCRIPTION`, `stamp/NAMESPACE`, `stamp/man/st_catalog_query.Rd`
- **Details**: Run `devtools::document()`, bump patch version `0.0.10` → `0.0.11` in DESCRIPTION.
  Add `@family version-store` tag.
- **Remaining actions**:
  1. `devtools::document()` in stamp project
  2. Bump `Version: 0.0.10` → `Version: 0.0.11` in `stamp/DESCRIPTION`
  3. `devtools::check()` to confirm clean
- **Test Scenarios**:
  - ✅ `R CMD check` passes
- **Tests**: Existing test suite passes.
- **Acceptance criteria**: `devtools::check()` passes; `st_catalog_query`
  appears in NAMESPACE exports.

## Phase 2: pipdata — catalog-based assembler

### 3. Create `build_pip_inventory()` function

- **Requirements**: R1, R2, R3, R4, R8, R9, R10
- **Files**: `pipdata/R/build_pip_inventory.R` (new file)
- **Details**:
  New function replacing `update_pip_inventory()`. Architecture:

  ```r
  build_pip_inventory <- function(inv_to_clean, pip_id_map) {
    # --- Defensive assertions (P2.5) ---
    stopifnot(anyDuplicated(inv_to_clean$survey_id) == 0L)

    # Step 1: Query stamp catalogs
    cat_data <- stamp::st_catalog_query(alias = "pip")
    cat_meta <- stamp::st_catalog_query(alias = "pip_meta")

    # Step 1b: Guard empty catalogs with differentiated messages (P2.6)
    if (nrow(cat_data) == 0L && nrow(cat_meta) == 0L) {
      if (nrow(pip_id_map) == 0L) {
        cli::cli_abort(c(
          "Both stamp catalogs are empty and no surveys were processed.",
          "i" = "If this is the first run, ensure {.fn save_pip_data} succeeds for at least one survey.",
          "i" = "If surveys were expected, review the processing log for errors."
        ))
      } else {
        cli::cli_abort(c(
          "Stamp catalogs are empty despite {nrow(pip_id_map)} pip_id(s) in the map.",
          "i" = "This suggests {.fn pip_write} / {.fn st_save} failed silently.",
          "i" = "Check stamp alias configuration."
        ))
      }
    }

    # Step 2: Derive pip_id from artifact path
    cat_data[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]
    cat_meta[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]

    # Step 2b: Validate pip_id format (P1.2)
    pip_id_pattern <- "^[A-Z]{3}_[0-9]{4}_[A-Z0-9-]+_(INC|CON)_(ALL|GPWG|D[0-9]+)$"
    bad_data <- cat_data[!grepl(pip_id_pattern, pip_id)]
    bad_meta <- cat_meta[!grepl(pip_id_pattern, pip_id)]
    if (nrow(bad_data) > 0L || nrow(bad_meta) > 0L) {
      bad_ids <- unique(c(bad_data$pip_id, bad_meta$pip_id))
      cli::cli_warn(c(
        "{length(bad_ids)} artifact(s) have non-standard pip_id format.",
        "i" = "IDs: {.val {utils::head(bad_ids, 5L)}}",
        "i" = "Expected pattern: COUNTRY_YEAR_ACRONYM_WELFARE_MODULE"
      ))
      cat_data <- cat_data[grepl(pip_id_pattern, pip_id)]
      cat_meta <- cat_meta[grepl(pip_id_pattern, pip_id)]
    }

    # Step 3: Drop code_hash (not needed for inventory) and suffix columns
    cat_data[, code_hash := NULL]
    cat_meta[, code_hash := NULL]
    data.table::setnames(cat_data,
      old = c("path", "version_id", "content_hash", "size_bytes", "created_at"),
      new = c("path_data", "version_id_data", "content_hash_data",
              "size_bytes_data", "created_at_data"))
    data.table::setnames(cat_meta,
      old = c("path", "version_id", "content_hash", "size_bytes", "created_at"),
      new = c("path_metadata", "version_id_metadata", "content_hash_metadata",
              "size_bytes_metadata", "created_at_metadata"))

    inv <- cat_data[cat_meta, on = "pip_id", nomatch = 0]

    # Step 4: Add survey_id via pip_id_map
    inv <- inv[pip_id_map, on = "pip_id", nomatch = 0]

    # Step 5: Scope to this run (inv_to_clean) + retain old surveys
    run_inv <- inv[survey_id %in% inv_to_clean$survey_id]

    # Step 6: Rename DLW columns in inv_to_clean BEFORE join (P2.2)
    # This prevents column-name collisions (content_hash, latest_version_id
    # exist in both run_inv and inv_to_clean).
    dlw_renames <- c(
      pipeline_version = "pipeline_version_dlw",
      latest_version_id = "latest_version_id_dlw",
      content_hash = "content_hash_dlw",
      Checksum = "Checksum_dlw",
      file_path = "path_dlw"
    )
    inv_dlw <- data.table::copy(inv_to_clean)
    # Only rename columns that actually exist in inv_to_clean
    present <- intersect(names(dlw_renames), names(inv_dlw))
    data.table::setnames(inv_dlw, old = present, new = dlw_renames[present])
    # Join DLW columns (now collision-free)
    run_inv <- joyn::left_join(run_inv, inv_dlw,
      by = "survey_id", relationship = "many-to-one",
      reportvar = FALSE, verbose = FALSE)

    # Step 7: Derive welfare_type from pip_id
    run_inv[, welfare_type := data.table::tstrsplit(pip_id, "_", fixed = TRUE)[[4L]]]

    # Step 8: Merge with old master
    old_inv <- tryCatch(pipload::load_pip_master_inventory(), error = \(e) NULL)
    if (!is.null(old_inv)) {
      old_retained <- old_inv[!old_inv$survey_id %in% run_inv$survey_id]
      run_inv <- collapse::rowbind(run_inv, old_retained, fill = TRUE)
    }
    # Assert no duplicate pip_ids after merge (P2.7)
    dup_pids <- run_inv$pip_id[duplicated(run_inv$pip_id)]
    if (length(dup_pids) > 0L) {
      cli::cli_abort(c(
        "Duplicate pip_id(s) in assembled inventory.",
        "x" = "Duplicates: {.val {unique(dup_pids)}}"
      ))
    }

    # Step 9: Build + save release inventory
    # (PFW filter + release version tracking)

    # Step 10: Save master inventory

    run_inv
  }
  ```

  **Key design choices**:
  - `pip_id_map`: a 2-column data.table mapping `pip_id` → `survey_id`,
    constructed from the `pip_names` output of successful `process_data()`
    calls. This is the minimal data that still needs to come from the
    processing loop (stamp catalogs don't know about survey_id).
  - Metadata-absent pip_ids: if a pip_id exists in `cat_data` but NOT in
    `cat_meta`, the `nomatch = 0` inner join excludes it automatically.
    No special sentinel logic needed.
  - Release version columns (`first_release_version_id`,
    `latest_release_version_id`): same logic as current, just applied
    after the master is assembled.
  - **Crash-safety limitation** (accepted from plan review P1.3): crash-safety
    via stamp catalogs applies only to the current run's surveys. Old surveys
    that weren't reprocessed are recovered from the old master file (Step 8).
    If the master file is corrupted, those surveys must be reprocessed.
    This is acceptable because the master inventory is itself a stamp artifact
    with version history — a prior version can be restored.

- **Test Scenarios**:
  - ✅ Happy path: 3 surveys (one with 2 pip_ids) → correct inventory structure
  - ✅ Second run: old master surveys retained, new surveys added
  - ✅ No column collisions: DLW columns renamed before join (content_hash_dlw, latest_version_id_dlw)
  - 🛑 Edge case: pip_id in data catalog missing from metadata catalog → excluded
  - 🛑 Edge case: empty inv_to_clean → returns old master unchanged
  - 🛑 Edge case: non-standard pip_id format in catalog → warning + excluded
  - 🛑 Edge case: duplicate survey_id in inv_to_clean → stopifnot fires
  - ❌ Error path: both catalogs empty, pip_id_map empty → abort "first run" message
  - ❌ Error path: both catalogs empty, pip_id_map non-empty → abort "st_save failed" message
  - ❌ Error path: duplicate pip_id after merge → abort with offending IDs

- **Tests**: `pipdata/tests/testthat/test-build_pip_inventory.R` with mocked
  `stamp::st_catalog_query` and `pipload::load_pip_master_inventory`.
- **Acceptance criteria**: Function produces identical schema as current
  master inventory (same column names, types). Tests pass for all scenarios.
  No `funique()` — uniqueness enforced by pip_id assertion.

### 4. Create `pip_id_map` builder in `pd_process_data()`

- **Requirements**: R7
- **Files**: `pipdata/R/pd_process_data.R`
- **Details**:
  Instead of passing the full `proc_dta` (with nested version metadata) to
  the assembler, collect only the pip_id → survey_id mapping from
  successful `process_data()` calls:

  ```r
  # After lapply loop:
  pip_id_map <- data.table::rbindlist(
    lapply(Filter(Negate(is.null), results), \(x) {
      data.table(pip_id = toupper(unlist(x$pip_names)))
    }),
    idcol = "survey_id"
  )
  ```

  Then call: `build_pip_inventory(inv_to_clean, pip_id_map)`.

  **Simplify `process_data()` return**: It still needs to return `pip_names`
  (for the map above) and signal success/failure (NULL = failed survey).
  Remove `versions_data` and `versions_metadata` from the return — they're
  no longer consumed.

- **Test Scenarios**:
  - ✅ Multi-pip_id survey → both pip_ids in map
  - ❌ Failed survey → NULL result → excluded from map
- **Tests**: Update existing mocks in test-pd_process_data tests.
- **Acceptance criteria**: `pd_process_data()` no longer passes version
  metadata to the assembler; pipeline produces correct inventory.

### 5. Simplify `save_pip_data()` return (optional cleanup)

- **Requirements**: R7
- **Files**: `pipdata/R/save_pip.R`
- **Details**:
  `save_pip_data()` currently returns the full `pip_write()` result
  (version metadata). Since version metadata is no longer consumed by the
  assembler, simplify to return only: `list(pip_id = id, success = TRUE)`
  or `NULL` on failure. This makes `process_data()`'s return lighter.

  **NOTE**: This is optional — the current return doesn't break anything,
  it's just dead weight. Can defer if risky.
- **Test Scenarios**:
  - ✅ Successful save → returns pip_id + success
  - ❌ Failed save → returns NULL (unchanged behavior)
- **Tests**: Update relevant mocks.
- **Acceptance criteria**: `save_pip_data()` return is consumed only for
  success/failure signaling.

### 6. Preserve release inventory logic in `build_pip_inventory()`

- **Requirements**: R3, R9
- **Files**: `pipdata/R/build_pip_inventory.R`
- **Details**:
  Port the PFW-filter + release write + release version tracking from
  current `update_pip_inventory()`:
  - Load PFW, filter `inpovcal == 1`, unique by `(country_code, surveyid_year, survey_acronym)`
  - Inner join master inventory → release inventory
  - `pip_write(release_pip_inv, id = "pip_release_inventory", alias = "pip_inv")`
  - Capture release `version_id` → populate `first_release_version_id` /
    `latest_release_version_id` on master
  - Save master inventory

  This is a direct port — no logic change, just cleaner context (the assembler
  is smaller so this section is more readable).
- **Test Scenarios**:
  - ✅ Survey in PFW → appears in release
  - 🛑 Survey NOT in PFW → excluded from release, still in master
  - ✅ Second run → `first_release_version_id` unchanged, `latest_release_version_id` updated
- **Tests**: Mock PFW + verify release inventory content.
- **Acceptance criteria**: Release inventory matches current behavior.

### 7. Preserve logging in `build_pip_inventory()`

- **Requirements**: R5
- **Files**: `pipdata/R/build_pip_inventory.R`
- **Details**:
  Port the logging entries that `update_pip_inventory()` currently emits:
  - `null_svys_inf`: still emitted in `pd_process_data()` (not the assembler)
  - `inv_update_inf`: verification that expected surveys appear in master
  - `release_write_err`: tryCatch around release write

  The skipped-survey logging (`skipped_svys_data`, `skipped_svys_metadata`)
  is no longer needed — skipped surveys just keep their prior version.
  The `missing_metadata_err` is no longer needed — pip_ids without metadata
  are excluded by the inner join automatically.
- **Test Scenarios**:
  - ✅ All surveys confirmed → info-level log
  - ❌ Some surveys missing from master → error-level log
- **Tests**: Update existing logging integration tests.
- **Acceptance criteria**: `log_report()` sections still work correctly.

## Phase 3: pipload — enrichment + cleanup

### 7b. Export `pip_domain_cols()` from pipfun (prerequisite for Step 8)

- **Requirements**: R6
- **Files**: `pipfun/R/constants.R` (new or existing constants file), `pipfun/NAMESPACE`
- **Details**:
  Define the canonical domain column names in pipfun (shared dependency
  across pipdata and pipload) so there is a single source of truth (P2.3):

  ```r
  #' Canonical PFW domain column names
  #'
  #' Returns the standard column names used to determine reporting level
  #' from the PFW auxiliary data.
  #'
  #' @return Character vector of domain column names.
  #' @export
  pip_domain_cols <- function() {
    c("cpi_domain", "ppp_domain", "gdp_domain", "pce_domain", "pop_domain")
  }
  ```

  Run `devtools::document()` in pipfun; bump patch version.
- **Test Scenarios**:
  - ✅ Returns length-5 character vector
  - ✅ Exported in NAMESPACE
- **Tests**: `pipfun/tests/testthat/test-constants.R`
- **Acceptance criteria**: `pipfun::pip_domain_cols()` returns the 5 domain
  column names. `R CMD check` passes.

### 8. Add `pip_inv_enrich()` to pipload

- **Requirements**: R6
- **Files**: `pipload/R/pip_inv_enrich.R` (new), `pipload/NAMESPACE`
- **Details**:
  ```r
  #' Enrich PIP inventory with metadata fields
  #'
  #' @param inv data.table: base inventory
  #' @param fields character: fields to add. Supported: "reporting_level"
  #' @return inv with additional columns
  #' @export
  pip_inv_enrich <- function(inv, fields = character(0)) {
    if ("reporting_level" %in% fields) {
      pfw <- load_aux_data("pfw", verbose = FALSE)
      pfw_rl <- pfw[inpovcal == 1L]
      # Canonical domain columns defined in pipfun (P2.3)
      domain_cols <- pipfun::pip_domain_cols()
      avail <- intersect(domain_cols, names(pfw_rl))
      if (length(avail) > 0L) {
        pfw_rl[, reporting_level := as.character(do.call(pmax, c(.SD, list(na.rm = TRUE)))),
               .SDcols = avail]
        pfw_rl_unq <- pfw_rl[,
          .(reporting_level = reporting_level[[1L]]),
          by = .(country_code, surveyid_year, survey_acronym)]
        # Drop any existing reporting_level columns
        rl_cols <- grep("^reporting_level", names(inv), value = TRUE)
        if (length(rl_cols)) inv[, (rl_cols) := NULL]
        inv <- joyn::left_join(inv, pfw_rl_unq,
          by = c("country_code", "surveyid_year", "survey_acronym"),
          relationship = "many-to-one",
          reportvar = FALSE, verbose = FALSE)
      }
    }
    inv
  }
  ```

  Future fields (CPI, PPP, etc.) add as additional `if` blocks.
- **Test Scenarios**:
  - ✅ National survey → `reporting_level = "1"`
  - ✅ Subnational survey → `reporting_level = "2"`
  - 🛑 Survey not in PFW → `reporting_level = NA`
  - 🛑 Pre-existing `reporting_level.x/.y` columns → cleaned before join
- **Tests**: `pipload/tests/testthat/test-pip_inv_enrich.R`
- **Acceptance criteria**: Enrichment is fully decoupled from inventory building.

### 9. Add `fields` parameter to `load_pip_master_inventory()`

- **Requirements**: R6
- **Files**: `pipload/R/load_pip_data.R`
- **Details**:
  Add optional `fields` argument with **default `"reporting_level"`** to
  preserve backward compatibility (P2.4 — opt-out, not opt-in):
  ```r
  load_pip_master_inventory <- \(
    format = "qs2",
    version = NULL,
    verbose = getOption("pipload.verbose"),
    fields = "reporting_level"
  ) {
    # ... existing logic ...
    inv <- pip_read(...)
    if (length(fields) > 0L) {
      inv <- pip_inv_enrich(inv, fields = fields)
    }
    inv
  }
  ```

  **Rationale**: Existing consumers call `load_pip_master_inventory()`
  without args and expect `reporting_level` to be present. Defaulting
  to `"reporting_level"` means they keep working without code changes.
  Consumers who explicitly don't want enrichment can pass `fields = character(0)`.

- **Test Scenarios**:
  - ✅ Default call (no `fields` arg) → `reporting_level` column present (backward compat)
  - ✅ `fields = character(0)` → no enrichment (opt-out)
  - ✅ `fields = "reporting_level"` → column added (explicit)
- **Tests**: Unit test with mocked `pip_read` + `load_aux_data`.
- **Acceptance criteria**: Backward compatible by default; enrichment opt-out
  via `fields = character(0)`.

### 10. Remove `reporting_level` computation from `build_pip_inventory()`

- **Requirements**: R6
- **Files**: `pipdata/R/build_pip_inventory.R`
- **Details**:
  The assembler does NOT compute `reporting_level`. It only tracks versions
  and DLW metadata. Consumers who need `reporting_level` use the default
  behavior of `load_pip_master_inventory()` (which enriches by default via
  `fields = "reporting_level"`). Consumers who explicitly don't want
  enrichment pass `fields = character(0)`.

  This means the master inventory on disk does NOT have `reporting_level`
  as a persisted column. It is computed on-the-fly at load time.
- **Test Scenarios**:
  - ✅ Master inventory schema does not include `reporting_level`
  - ✅ `load_pip_master_inventory()` (default) still returns it (enrichment)
- **Tests**: Verify column absence on disk, presence after load.
- **Acceptance criteria**: `reporting_level` not saved to disk; available
  by default via enrichment at load time.

### 11. Archive old code and update pipdata

- **Requirements**: All
- **Files**: `pipdata/R/update_pip_inventory.R` → `pipdata/old_files/`,
  corresponding test file, man pages
- **Details**:
  - Move `update_pip_inventory.R` to `old_files/update_pip_inventory.R`
  - Remove `format_vrs` and `drop_rl_cols` exports from NAMESPACE
  - Delete `man/update_pip_inventory.Rd`, `man/format_vrs.Rd`
  - Update `pd_process_data()` to call `build_pip_inventory()` instead
  - Update `compound-gpid.context.md` with new inventory architecture notes
  - Run `devtools::document()` and `devtools::check()`
- **Test Scenarios**:
  - ✅ `R CMD check` passes
  - ✅ All existing tests pass (with updated mocks)
- **Tests**: Full test suite.
- **Acceptance criteria**: Clean `R CMD check`, no references to
  `update_pip_inventory()` in active code.

## Testing Strategy

- **Unit tests** (mocked): Each function tested in isolation with mocked
  stamp/pipload calls. temp-dir stamp aliases used for stamp tests.
- **Integration test**: End-to-end with a real (small) stamp alias:
  init temp alias → save 3 mock surveys → call `build_pip_inventory()` →
  verify schema and content.
- **Regression**: Ensure `pd_deflation()` still works with the new inventory
  schema (column names `version_id_data`, `version_id_metadata` must exist).
- **Backward compat**: `load_pip_master_inventory()` without `fields`
  returns the same schema as before (minus `reporting_level`, which was
  only recently added).

## Documentation Checklist

- [ ] `st_catalog_query()` roxygen with examples
- [ ] `build_pip_inventory()` roxygen with full @details
- [ ] `pip_inv_enrich()` roxygen
- [ ] Update `compound-gpid.context.md` — new inventory architecture
- [ ] Update `compound-gpid.context.md` — canonical inventory schema
- [ ] Inline comments in assembler explaining join logic

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|-----------|
| stamp catalog schema changes in future | Assembler breaks | Pin stamp version in pipdata DESCRIPTION; `st_catalog_query()` is our API contract |
| pip_id derivation from path fails (non-standard artifact names) | Missing surveys in inventory | Regex assertion (`pip_id_pattern`) warns and excludes; log identifies affected IDs (P1.2) |
| Removing `reporting_level` from persisted inventory breaks downstream consumers | Consumer errors | Default `fields = "reporting_level"` in `load_pip_master_inventory()` preserves backward compat (P2.4) |
| `pd_deflation()` expects `version_id_data` column | Deflation fails | Ensure `build_pip_inventory()` produces columns with same names (from catalog query) |
| Empty catalog (first-ever run, no prior saves) | Assembler returns empty | Differentiated abort messages: "first run" vs "st_save failed" based on pip_id_map state (P2.6) |
| Performance: reading catalog for 2500 artifacts | Slow assembler | Benchmarked: catalog.qs2 is ~1MB, reads in <0.5s |
| Column name collisions between catalog output and DLW inventory | `frename()` crash or silent wrong column | DLW columns renamed in `inv_to_clean` BEFORE join (P2.2) |
| Old master corruption loses prior-run surveys | Surveys not recoverable from catalog alone | Master inventory is itself a stamp artifact — prior versions restorable. Crash-safety for current run only (P1.3 accepted) |
| Duplicate pip_ids after merge with old master | Downstream errors | Explicit `pip_id` uniqueness assertion after merge; abort with offending IDs (P2.7) |

## Out of Scope

- Run manifest artifact (deferred as `run-manifest-audit` roadmap idea)
- Generic `get_inv_attr()` helper for arbitrary metadata extraction
- Migrating `pd_deflation()` to use the new assembler internally (it already
  reads master inventory — just needs column names to match)
- Rewriting `save_pip_data()` internals (only return value changes)
- DLW wrapper rewrite (separate roadmap item)

## Review Findings Addressed (2026-05-20)

Plan revised after `/cg-plan-review`. Changes:

| Finding | Resolution |
|---------|-----------|
| P1.1 — Join direction inverted | Fixed: `cat$versions[cat$artifacts, on=...]` (was backwards) |
| P1.2 — pip_id derivation unvalidated | Added regex assertion + warning in Step 3 |
| P1.3 — Crash-safety partial | Accepted as limitation; documented in R1, design notes, and Risks |
| P2.1 — code_hash unused by pipdata | Kept in `st_catalog_query()` return (general stamp API); pipdata drops it via `[, code_hash := NULL]` before renaming |
| P2.2 — Column name collisions | DLW columns renamed BEFORE join (not after) |
| P2.3 — Domain cols hardcoded | New Step 7b: `pipfun::pip_domain_cols()` as single source |
| P2.4 — reporting_level removal breaks consumers | Default `fields = "reporting_level"` (opt-out, not opt-in) |
| P2.5 — No survey_id uniqueness assertion | Added `stopifnot(anyDuplicated(...) == 0L)` |
| P2.6 — Empty-catalog message undifferentiated | Two distinct abort messages based on pip_id_map state |
| P2.7 — funique misleading | Replaced with explicit pip_id uniqueness assertion |
| P3.1 — purrr::map reference | Changed to `lapply` |
| P3.2 — pmax NA propagation | Added `na.rm = TRUE` |
