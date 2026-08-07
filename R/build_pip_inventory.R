#' Build the PIP master and release inventories from stamp catalogs
#'
#' Delta/update assembler replacing `update_pip_inventory()`. Reads version
#' facts for **current-run surveys only** from stamp's persisted catalogs
#' (`"pip"` and `"pip_meta"` aliases), upserts them into the prior master
#' inventory, and saves the result. Old surveys not reprocessed this run are
#' retained unchanged from the prior master.
#'
#' Compared to `update_pip_inventory()`, this function:
#' - Queries catalogs then immediately filters to the current run's pip_ids,
#'   avoiding all catalog-wide validation issues.
#' - Does not require in-memory version metadata (crash-safe for current run).
#' - Does not compute `reporting_level` — enrichment is handled by
#'   [pipload::pip_inv_enrich()] when needed by consumers.
#' - Upserts by `pip_id`: reprocessed surveys replace their old row; all other
#'   surveys are retained from the prior master. One row per `pip_id`, always.
#'
#' @param inv_to_clean A `data.table` of DLW surveys sent for processing (as
#'   returned by [valid_dlw_load()]). Must have unique `survey_id` rows.
#' @param pip_id_map A `data.table` with exactly two columns: `survey_id`
#'   (DLW survey identifier) and `pip_id` (PIP identifier, uppercase). Built
#'   from successful `process_data()` calls in [pd_process_data()].
#' @param aux_hashes A named character vector of current aux `content_hash`
#'   values, one per requested auxiliary measure (e.g. `cpi`, `ppp`, `pfw`).
#'   Resolved once per run by [get_aux_hashes()] and recorded on the
#'   master-inventory rows produced for successfully processed surveys.
#'   Default `NULL` (no aux hashes recorded).
#'
#' @return A `data.table`: the updated PIP master inventory. Does **not**
#'   include `reporting_level` — enrich after load via
#'   `pipload::pip_inv_enrich(inv, fields = "reporting_level")`.
#'
#' @details
#' **Logging**: writes the following entries to `"pipdata_log"`:
#' - `inv_update_inf`: verification that expected surveys appear in master
#'   (info-level if all confirmed, error-level if any missing).
#' - `release_write_err`: tryCatch-caught release inventory write failure.
#'
#' **Column provenance**:
#' - `version_id_data`, `content_hash_data`, `size_bytes_data`,
#'   `created_at_data`, `path_data` â€” from the `"pip"` catalog.
#' - `version_id_metadata`, `content_hash_metadata`, `size_bytes_metadata`,
#'   `created_at_metadata`, `path_metadata` â€” from the `"pip_meta"` catalog.
#' - `pipeline_version_dlw`, `latest_version_id_dlw`, `content_hash_dlw`,
#'   `Checksum_dlw`, `path_dlw` â€” renamed from DLW inventory columns.
#' - `welfare_type` â€” derived from the 4th `_`-delimited segment of `pip_id`.
#' - `aux_<measure>_hash` (e.g. `aux_cpi_hash`, `aux_ppp_hash`, `aux_pfw_hash`)
#'   â€” current aux `content_hash` for each requested measure, from the
#'   run-level `aux_hashes` map passed by [pd_process_data()]. Only populated
#'   for surveys successfully processed in the current run.
#' - `first_release_version_id`, `latest_release_version_id` â€” stamp version
#'   IDs of the release inventory (first appearance and most recent).
#'
#' @param verbose Logical. Controls verbosity of downstream
#'   [pipload::load_pip_master_inventory()] and [pipload::load_aux_data()]
#'   calls. Default: `getOption("pipdata.verbose", default = TRUE)`.
#'
#' @family pd_process_data pipeline
#' @export
build_pip_inventory <- function(
  inv_to_clean,
  pip_id_map,
  verbose = getOption("pipdata.verbose", default = TRUE),
  aux_hashes = NULL
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defensive assertions  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(anyDuplicated(inv_to_clean$survey_id) == 0L)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 1: Load prior master inventory  ---------
  # Base for the upsert. Old surveys not reprocessed this run are carried
  # forward from here unchanged.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  old_inv <- tryCatch(
    expr = pipload::load_pip_master_inventory(verbose = verbose),
    error = function(e) NULL
  )

  # Normalise old_inv column classes written by update_pip_inventory():
  # size_bytes_* were persisted with class c("fs_bytes","numeric") via the
  # fs package. collapse::rowbind aborts on class mismatches even when the
  # storage type is compatible. Strip non-standard classes once here so that
  # old_retained inherits clean types before the upsert rowbind.
  #
  # Also drop legacy columns not produced by build_pip_inventory():
  # reporting_level (previously derived on-the-fly; now opt-in via
  # pipload::pip_inv_enrich()), code_hash_data, file_hash_data,
  # code_label_data, code_hash_metadata, file_hash_metadata,
  # code_label_metadata, format_data, format_metadata.
  # Dropping here migrates any on-disk master to the new schema on next run.
  if (!is.null(old_inv)) {
    fs_cols <- names(old_inv)[vapply(
      old_inv,
      \(x) inherits(x, "fs_bytes"),
      logical(1L)
    )]
    for (col in fs_cols) {
      data.table::set(
        old_inv,
        j = col,
        value = `class<-`(old_inv[[col]], "numeric")
      )
    }

    legacy_cols <- c(
      "reporting_level",
      "code_hash_data",
      "file_hash_data",
      "code_label_data",
      "code_hash_metadata",
      "file_hash_metadata",
      "code_label_metadata",
      "format_data",
      "format_metadata"
    )
    drop_cols <- intersect(legacy_cols, names(old_inv))
    if (length(drop_cols) > 0L) {
      old_inv[, (drop_cols) := NULL]
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 2: Early return when no surveys were processed  ---------
  # pip_id_map is empty when all surveys failed or none were scheduled.
  # Return old master if available, abort if this is a first run.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nrow(pip_id_map) == 0L) {
    if (!is.null(old_inv)) {
      return(old_inv)
    }
    cli::cli_abort(
      c(
        "No surveys processed and no prior master inventory exists.",
        "i" = paste0(
          "If this is the first run, ensure {.fn save_pip_data} succeeds ",
          "for at least one survey."
        ),
        "i" = "If surveys were expected, review the processing log for errors."
      ),
      class = c("build_pip_inventory_empty_first_run", "piperr")
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 3: Query catalogs, filter to current-run pip_ids only  ---------
  # st_catalog_query returns one row per artifact (latest version).
  # We immediately scope to target_ids so all subsequent joins work on
  # a small, known-clean set â€” no full-catalog validation needed.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  target_ids <- unique(pip_id_map$pip_id)

  cat_data    <- stamp::st_catalog_query(alias = "pip")
  cat_meta    <- stamp::st_catalog_query(alias = "pip_meta")
  cat_inv     <- stamp::st_catalog_query(alias = "pip_inv")
  recode_rows <- cat_inv[grepl("recode_spec", cat_inv$path, fixed = TRUE), ]
  recode_spec_vid <- if (nrow(recode_rows) > 0L) {
    recode_rows$version_id[[1L]]
  } else {
    NA_character_
  }

  # Derive pip_id from artifact filename (e.g. "bol_2022_eh_inc_all.qs2")
  cat_data[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]
  cat_meta[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]

  # Scope to current run first (filter-first principle: validate only the
  # current-run set so unrelated historical artifacts in the alias never
  # produce spurious warnings).
  cat_data <- cat_data[pip_id %in% target_ids]
  cat_meta <- cat_meta[pip_id %in% target_ids]

  # P1.2: Validate pip_id format on the already-filtered set.
  # Expected: COUNTRY_YEAR_ACRONYM_WELFARE_MODULE (5 _-delimited segments,
  # e.g. BOL_2022_EH_INC_ALL). Acronym may contain hyphens (e.g. EPHC-S2).
  # Artifacts with non-standard names produce garbage pip_ids — warn
  # explicitly so misconfigurations are visible in the log.
  pip_id_pattern <- "^[A-Z]{3}_[0-9]{4}_[A-Z0-9-]+_[A-Z]+_[A-Z0-9]+$"
  bad_data <- cat_data[!grepl(pip_id_pattern, pip_id), path]
  bad_meta <- cat_meta[!grepl(pip_id_pattern, pip_id), path]
  bad_paths <- union(bad_data, bad_meta)
  if (length(bad_paths) > 0L) {
    cli::cli_warn(
      c(
        paste0(
          "{length(bad_paths)} artifact path(s) produced non-standard pip_id(s) ",
          "and will be excluded."
        ),
        "i" = "Paths: {.val {utils::head(bad_paths, 5L)}}",
        "i" = paste0(
          "Expected pip_id format: ",
          "{.val COUNTRY_YEAR_ACRONYM_WELFARE_MODULE}"
        )
      ),
      class = c("build_pip_inventory_bad_pip_id_format", "piperr")
    )
    cat_data <- cat_data[grepl(pip_id_pattern, pip_id)]
    cat_meta <- cat_meta[grepl(pip_id_pattern, pip_id)]
  }

  # Guard: both catalogs empty after filter â†’ st_save must have failed
  if (nrow(cat_data) == 0L && nrow(cat_meta) == 0L) {
    cli::cli_abort(
      c(
        paste0(
          "Stamp catalogs are empty despite {length(target_ids)} ",
          "pip_id(s) in the map."
        ),
        "i" = "This suggests {.fn pip_write} / {.fn st_save} failed silently.",
        "i" = "Check stamp alias configuration."
      ),
      class = c("build_pip_inventory_empty_catalog", "piperr")
    )
  }

  # Warn about pip_ids in this run not found in one or both catalogs
  missing_any <- union(
    setdiff(target_ids, cat_data$pip_id),
    setdiff(target_ids, cat_meta$pip_id)
  )
  if (length(missing_any) > 0L) {
    cli::cli_warn(
      c(
        "{length(missing_any)} pip_id(s) not found in one or both catalogs.",
        "i" = "IDs: {.val {utils::head(missing_any, 5L)}}",
        "i" = "These surveys will not appear in the inventory."
      ),
      class = c("build_pip_inventory_missing_from_catalog", "piperr")
    )
  }

  # If same pip_id appears at multiple paths, keep the latest by created_at
  if (anyDuplicated(cat_data$pip_id) != 0L) {
    data.table::setorder(cat_data, pip_id, -created_at)
    cat_data <- cat_data[, .SD[1L], by = "pip_id"]
  }
  if (anyDuplicated(cat_meta$pip_id) != 0L) {
    data.table::setorder(cat_meta, pip_id, -created_at)
    cat_meta <- cat_meta[, .SD[1L], by = "pip_id"]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 4: Suffix catalog columns  ---------
  # Drop code_hash (not needed for inventory) and rename remaining columns
  # to distinguish data vs metadata artifact provenance.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat_data[, code_hash := NULL]
  cat_meta[, code_hash := NULL]

  data.table::setnames(
    cat_data,
    old = c("path", "version_id", "content_hash", "size_bytes", "created_at"),
    new = c(
      "path_data",
      "version_id_data",
      "content_hash_data",
      "size_bytes_data",
      "created_at_data"
    )
  )
  data.table::setnames(
    cat_meta,
    old = c("path", "version_id", "content_hash", "size_bytes", "created_at"),
    new = c(
      "path_metadata",
      "version_id_metadata",
      "content_hash_metadata",
      "size_bytes_metadata",
      "created_at_metadata"
    )
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 5: Join data + metadata catalogs  ---------
  # Both tables filtered to current-run pip_ids (small set, no dup risk).
  # pip_ids absent from either catalog are excluded by nomatch = 0.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_versions <- cat_data[cat_meta, on = "pip_id", nomatch = 0L]

  new_versions[, version_id_recode_spec := recode_spec_vid]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 6: Add survey_id from pip_id_map  ---------
  # Enriches new_versions with survey_id for each pip_id.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_versions <- new_versions[pip_id_map, on = "pip_id", nomatch = 0L]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 7: Join DLW columns from inv_to_clean  ---------
  # Rename DLW columns BEFORE the join to prevent name collisions with
  # catalog columns (content_hash, latest_version_id exist in both).
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dlw_renames <- c(
    pipeline_version = "pipeline_version_dlw",
    latest_version_id = "latest_version_id_dlw",
    content_hash = "content_hash_dlw",
    Checksum = "Checksum_dlw",
    file_path = "path_dlw"
  )
  inv_dlw <- data.table::copy(inv_to_clean)
  present <- intersect(names(dlw_renames), names(inv_dlw))
  data.table::setnames(inv_dlw, old = present, new = dlw_renames[present])

  new_versions <- joyn::left_join(
    new_versions,
    inv_dlw,
    by = "survey_id",
    relationship = "many-to-one",
    reportvar = FALSE,
    verbose = FALSE
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 7b: Attach run-level aux hashes  ---------
  # Record the current aux content_hash for each requested measure on the
  # current-run rows. These columns let valid_dlw_load() gate aux-change
  # detection against the aux data actually used in this run.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(aux_hashes) && length(aux_hashes) > 0L) {
    for (m in names(aux_hashes)) {
      col <- paste0("aux_", m, "_hash")
      data.table::set(new_versions, j = col, value = aux_hashes[[m]])
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 8: Derive welfare_type from pip_id  ---------
  # Format: COUNTRY_YEAR_ACRONYM_WELFARE_MODULE.
  # The 4th _-delimited segment is always welfare_type.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_versions[,
    welfare_type := data.table::tstrsplit(
      pip_id,
      "_",
      fixed = TRUE,
      fill = NA_character_
    )[[4L]]
  ]

  # Defensive guard: the pip_id_pattern regex enforced above guarantees 5
  # underscore-delimited segments (COUNTRY_YEAR_ACRONYM_WELFARE_MODULE), so
  # welfare_type (segment 4) is never NA under normal conditions. This block
  # guards against future regex relaxation that could re-open that path.
  bad_wt <- new_versions[is.na(welfare_type), pip_id]
  if (length(bad_wt) > 0L) {
    cli::cli_warn(
      c(
        "{length(bad_wt)} pip_id(s) have fewer than 4 '_'-delimited segments.",
        "i" = "IDs: {.val {utils::head(bad_wt, 5L)}}",
        "i" = "These rows are dropped from the inventory."
      ),
      class = c("build_pip_inventory_bad_welfare_type", "piperr")
    )
    new_versions <- new_versions[!is.na(welfare_type)]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 9: Upsert into old master  ---------
  # When a survey is reprocessed, ALL of its old rows are dropped and replaced
  # by the fresh catalog data for that survey. This ensures the survey's pip_id
  # set in the master exactly matches the current reprocess — stale pip_id rows
  # from a previous content version (e.g. a welfare-type split that no longer
  # exists) are removed. Historical versions remain recoverable via stamp.
  # Surveys not reprocessed this run are retained unchanged.
  # Result: one row per pip_id.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(old_inv)) {
    reprocessed_surveys <- unique(new_versions$survey_id)
    old_retained <- old_inv[!old_inv$survey_id %in% reprocessed_surveys]
    run_inv <- collapse::rowbind(new_versions, old_retained, fill = TRUE)
  } else {
    run_inv <- new_versions
  }

  # Assert no duplicate pip_ids after upsert
  dup_pids <- run_inv$pip_id[duplicated(run_inv$pip_id)]
  if (length(dup_pids) > 0L) {
    cli::cli_abort(
      c(
        "Duplicate pip_id(s) in assembled inventory.",
        "x" = "Duplicates: {.val {unique(dup_pids)}}"
      ),
      class = c("build_pip_inventory_dup_pip_id", "piperr")
    )
  }

  data.table::setDT(run_inv)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 10: Build and save release inventory  ---------
  # PFW inpovcal filter, pip_write + release version tracking.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Initialise release version columns so master schema is always consistent.
  if (!"first_release_version_id" %in% names(run_inv)) {
    run_inv[, first_release_version_id := NA_character_]
  }
  if (!"latest_release_version_id" %in% names(run_inv)) {
    run_inv[, latest_release_version_id := NA_character_]
  }

  # Initialise aux hash columns so the master schema is always consistent.
  # Old retained rows preserve any existing hashes; missing columns are
  # initialised to NA. Current-run rows were populated in Step 7b.
  if (!is.null(aux_hashes) && length(aux_hashes) > 0L) {
    for (m in names(aux_hashes)) {
      col <- paste0("aux_", m, "_hash")
      if (!col %in% names(run_inv)) {
        run_inv[, (col) := NA_character_]
      }
    }
  }

  pfw <- pipload::load_aux_data("pfw", verbose = verbose)

  pfw_release <- pfw |>
    collapse::fsubset(inpovcal == 1) |>
    collapse::fselect(country_code, surveyid_year, survey_acronym) |>
    collapse::funique() |>
    data.table::as.data.table()

  release_pip_inv <- run_inv[
    pfw_release,
    on = .(country_code, surveyid_year, survey_acronym),
    nomatch = 0L
  ]

  release_result <- tryCatch(
    expr = pipload::pip_write(
      x = release_pip_inv,
      id = "pip_release_inventory",
      alias = "pip_inv",
      pk = c("survey_id", "pip_id"),
      verbose = verbose
    ),
    error = function(e) {
      pipfun::log_error(
        paste0(
          "Release inventory write failed. Master inventory will be saved ",
          "without updated release version columns."
        ),
        name = "pipdata_log",
        logmeta = list(
          error = "release_write_err",
          condition_msg = conditionMessage(e)
        )
      )
      NULL
    }
  )

  # Resolve release stamp version_id.
  # pip_write() may return skipped = TRUE when content is unchanged;
  # fall back to st_latest() in that case.
  release_vid <- if (!is.null(release_result)) {
    vid <- release_result$version_id
    if (is.null(vid) || isTRUE(release_result$skipped)) {
      tryCatch(
        stamp::st_latest("pip_release_inventory", alias = "pip_inv"),
        error = function(e) NA_character_
      )
    } else {
      vid
    }
  } else {
    NA_character_
  }

  # Populate release version columns on master.
  # - first_release_version_id: set only when currently NA (first appearance).
  # - latest_release_version_id: always updated for surveys in this release.
  if (!is.na(release_vid)) {
    release_ids <- release_pip_inv$survey_id

    run_inv[
      survey_id %in% release_ids & is.na(first_release_version_id),
      first_release_version_id := release_vid
    ]
    run_inv[
      survey_id %in% release_ids,
      latest_release_version_id := release_vid
    ]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 11: Save master inventory  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pipload::pip_write(
    x = run_inv,
    id = "pip_master_inventory",
    alias = "pip_master",
    pk = c("survey_id", "pip_id"),
    verbose = verbose
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 12: Reload and verify  ---------
  # Verify that surveys from this run appear in the saved master.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pip_inv <- tryCatch(
    pipload::load_pip_master_inventory(verbose = verbose),
    error = function(e) NULL
  )

  successful_ids <- unique(pip_id_map$survey_id)
  confirmed <- if (!is.null(pip_inv)) {
    successful_ids[successful_ids %in% pip_inv$survey_id]
  } else {
    character(0)
  }
  missing_ids <- setdiff(successful_ids, confirmed)

  if (length(missing_ids) == 0L) {
    pipfun::log_info(
      "Master inventory verification complete.",
      name = "pipdata_log",
      logmeta = list(
        info = "inv_update_inf",
        n_expected = length(successful_ids),
        n_confirmed = length(confirmed),
        n_missing = 0L,
        surveys_confirmed = confirmed,
        surveys_missing = character(0)
      )
    )
  } else {
    pipfun::log_error(
      "Some successfully cleaned surveys are missing from the master inventory.",
      name = "pipdata_log",
      logmeta = list(
        error = "inv_update_inf",
        n_expected = length(successful_ids),
        n_confirmed = length(confirmed),
        n_missing = length(missing_ids),
        surveys_confirmed = confirmed,
        surveys_missing = missing_ids
      )
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return  ---------
  # Return the in-memory assembled inventory (run_inv), not the reloaded
  # copy: step 12 reload is only for the verification log.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Enforce canonical column order:
  #   1. Survey identity block
  #   2. Catalog data columns (_data suffix)
  #   3. Catalog metadata columns (_metadata suffix)
  #   4. DLW provenance columns (_dlw suffix + status/dates)
  #   5. Release version tracking
  #   6. Any remaining columns (e.g. from old master via fill = TRUE)
  id_cols <- c(
    "survey_id",
    "pip_id",
    "country_code",
    "surveyid_year",
    "survey_acronym",
    "vermast",
    "veralt",
    "collection",
    "module",
    "tool",
    "welfare_type"
  )
  data_cols <- c(
    "path_data",
    "version_id_data",
    "content_hash_data",
    "size_bytes_data",
    "created_at_data"
  )
  meta_cols <- c(
    "path_metadata",
    "version_id_metadata",
    "content_hash_metadata",
    "size_bytes_metadata",
    "created_at_metadata"
  )
  dlw_cols <- c(
    "pipeline_version_dlw",
    "latest_version_id_dlw",
    "content_hash_dlw",
    "path_dlw",
    "status",
    "data_available",
    "date_validated",
    "Checksum_dlw"
  )
  release_cols <- c("first_release_version_id", "latest_release_version_id")
  spec_cols    <- c("version_id_recode_spec")

  ordered_cols <- c(id_cols, data_cols, meta_cols, dlw_cols, release_cols, spec_cols)
  # Only reorder columns that actually exist (fill = TRUE may add extras)
  present_ordered <- intersect(ordered_cols, names(run_inv))
  remainder <- setdiff(names(run_inv), present_ordered)
  data.table::setcolorder(run_inv, c(present_ordered, remainder))

  return(run_inv)
}
