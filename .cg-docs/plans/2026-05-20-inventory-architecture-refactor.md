---
date: 2026-05-20
title: "Refactor inventory architecture: catalog-based assembler"
status: active
scope: "Deep"
phases: 3
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
| R1  | Version info persisted per-survey as side effect of saving (crash-safe)      | brainstorm       |
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

## Phase 1: stamp — expose catalog query

### 1. Add `st_catalog_query()` export to stamp

- **Requirements**: R1, R2
- **Files**: `stamp/R/version_store.R`, `stamp/NAMESPACE`
- **Details**:
  Add a public function that returns the latest version metadata for all
  artifacts in a given alias. Returns a `data.table` with one row per
  artifact: `path`, `version_id`, `content_hash`, `code_hash`,
  `size_bytes`, `created_at`.

  ```r
  #' Query latest versions for all artifacts in an alias
  #'

  #' @param alias Character. Stamp alias to query.
  #' @return data.table with one row per artifact (latest version only):
  #'   path, version_id, content_hash, code_hash, size_bytes, created_at.
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
    # Join artifacts with their latest version row
    cat$artifacts[
      cat$versions,
      on = .(latest_version_id = version_id),
      nomatch = 0,
      .(path,
        version_id = latest_version_id,
        content_hash = i.content_hash,
        code_hash = i.code_hash,
        size_bytes = i.size_bytes,
        created_at = i.created_at)
    ]
  }
  ```

- **Test Scenarios**:
  - ✅ Happy path: alias with 3 artifacts → 3 rows returned
  - ✅ Multiple versions per artifact → only latest returned
  - 🛑 Edge case: empty alias (no artifacts) → empty data.table
  - 🛑 Edge case: NULL alias → uses default alias
  - ❌ Error path: alias not initialized → appropriate error from `.st_catalog_read()`
- **Tests**: `stamp/tests/testthat/test-catalog-query.R` — init temp alias,
  save 3 artifacts (one with 2 versions), verify row count and that only
  latest version_id appears.
- **Acceptance criteria**: `st_catalog_query("pip")` returns a data.table
  with one row per artifact, only latest version, in < 1 second for 2500 artifacts.

### 2. Add roxygen2 documentation and bump stamp version

- **Requirements**: R1
- **Files**: `stamp/DESCRIPTION`, `stamp/NAMESPACE`, `stamp/man/st_catalog_query.Rd`
- **Details**: Run `devtools::document()`, bump patch version in DESCRIPTION.
  Add `@family version-store` tag.
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
    # Step 1: Query stamp catalogs
    cat_data <- stamp::st_catalog_query(alias = "pip")
    cat_meta <- stamp::st_catalog_query(alias = "pip_meta")

    # Step 2: Derive pip_id from artifact path
    cat_data[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]
    cat_meta[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]

    # Step 3: Suffix and join data + metadata
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

    # Step 6: Join DLW inventory columns
    run_inv <- joyn::left_join(run_inv, inv_to_clean,
      by = "survey_id", relationship = "many-to-one",
      reportvar = FALSE, verbose = FALSE)
    # Rename DLW columns
    collapse::frename(run_inv,
      pipeline_version = "pipeline_version_dlw",
      latest_version_id = "latest_version_id_dlw",
      content_hash = "content_hash_dlw",
      Checksum = "Checksum_dlw",
      file_path = "path_dlw")

    # Step 7: Derive welfare_type from pip_id
    run_inv[, welfare_type := data.table::tstrsplit(pip_id, "_", fixed = TRUE)[[4L]]]

    # Step 8: Merge with old master
    old_inv <- tryCatch(pipload::load_pip_master_inventory(), error = \(e) NULL)
    if (!is.null(old_inv)) {
      run_inv <- collapse::rowbind(
        run_inv,
        old_inv[!old_inv$survey_id %in% run_inv$survey_id],
        fill = TRUE
      ) |> collapse::funique() |> as.data.table()
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

- **Test Scenarios**:
  - ✅ Happy path: 3 surveys (one with 2 pip_ids) → correct inventory structure
  - ✅ Second run: old master surveys retained, new surveys added
  - 🛑 Edge case: pip_id in data catalog missing from metadata catalog → excluded
  - 🛑 Edge case: empty inv_to_clean → returns old master unchanged
  - ❌ Error path: both catalogs empty → informative error

- **Tests**: `pipdata/tests/testthat/test-build_pip_inventory.R` with mocked
  `stamp::st_catalog_query` and `pipload::load_pip_master_inventory`.
- **Acceptance criteria**: Function produces identical schema as current
  master inventory (same column names, types). Tests pass for all scenarios.

### 4. Create `pip_id_map` builder in `pd_process_data()`

- **Requirements**: R7
- **Files**: `pipdata/R/pd_process_data.R`
- **Details**:
  Instead of passing the full `proc_dta` (with nested version metadata) to
  the assembler, collect only the pip_id → survey_id mapping from
  successful `process_data()` calls:

  ```r
  # After purrr::map loop:
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
      domain_cols <- c("cpi_domain", "ppp_domain", "gdp_domain",
                       "pce_domain", "pop_domain")
      avail <- intersect(domain_cols, names(pfw_rl))
      if (length(avail) > 0L) {
        pfw_rl[, reporting_level := as.character(do.call(pmax, .SD)),
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
  Add optional `fields` argument:
  ```r
  load_pip_master_inventory <- \(
    format = "qs2",
    version = NULL,
    verbose = getOption("pipload.verbose"),
    fields = character(0)
  ) {
    # ... existing logic ...
    inv <- pip_read(...)
    if (length(fields) > 0L) {
      inv <- pip_inv_enrich(inv, fields = fields)
    }
    inv
  }
  ```
- **Test Scenarios**:
  - ✅ `fields = character(0)` → no enrichment (backward compatible)
  - ✅ `fields = "reporting_level"` → column added
- **Tests**: Unit test with mocked `pip_read` + `load_aux_data`.
- **Acceptance criteria**: Backward compatible; enrichment opt-in.

### 10. Remove `reporting_level` computation from `build_pip_inventory()`

- **Requirements**: R6
- **Files**: `pipdata/R/build_pip_inventory.R`
- **Details**:
  The assembler does NOT compute `reporting_level`. It only tracks versions
  and DLW metadata. Consumers who need `reporting_level` call
  `load_pip_master_inventory(fields = "reporting_level")` or
  `pip_inv_enrich()` directly.

  This means the master inventory on disk does NOT have `reporting_level`
  as a persisted column. It is computed on-the-fly at load time.
- **Test Scenarios**:
  - ✅ Master inventory schema does not include `reporting_level`
- **Tests**: Verify column absence.
- **Acceptance criteria**: `reporting_level` not saved to disk; available
  only via enrichment.

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
| pip_id derivation from path fails (non-standard artifact names) | Missing surveys in inventory | Assert all `inv_to_clean` pip_ids found in catalog; abort if mismatch |
| Removing `reporting_level` from persisted inventory breaks downstream consumers | Consumer errors | Consumer must be updated to use `fields = "reporting_level"` — communicate change |
| `pd_deflation()` expects `version_id_data` column | Deflation fails | Ensure `build_pip_inventory()` produces columns with same names (from catalog query) |
| Empty catalog (first-ever run, no prior saves) | Assembler returns empty | Guard: if both catalogs empty, abort with informative message |
| Performance: reading catalog for 2500 artifacts | Slow assembler | Benchmarked: catalog.qs2 is ~1MB, reads in <0.5s |

## Out of Scope

- Run manifest artifact (deferred as `run-manifest-audit` roadmap idea)
- Generic `get_inv_attr()` helper for arbitrary metadata extraction
- Migrating `pd_deflation()` to use the new assembler internally (it already
  reads master inventory — just needs column names to match)
- Rewriting `save_pip_data()` internals (only return value changes)
- DLW wrapper rewrite (separate roadmap item)
