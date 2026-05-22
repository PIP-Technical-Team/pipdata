#' Build the PIP master and release inventories from stamp catalogs
#'
#' Catalog-based assembler replacing [update_pip_inventory()]. Reads version
#' facts directly from stamp's persisted catalogs (`"pip"` and `"pip_meta"`
#' aliases) and joins them with the current-run `pip_id_map` to produce the
#' master inventory. Old surveys not reprocessed this run are retained from the
#' prior master file. The release inventory is filtered by PFW `inpovcal == 1`
#' and written with the same release-version-tracking logic used previously.
#'
#' Compared to [update_pip_inventory()], this function:
#' - Does not require in-memory version metadata (crash-safe for current run).
#' - Does not compute `reporting_level` — enrichment is handled by
#'   [pipload::load_pip_master_inventory()] via the `fields` argument (Phase 3).
#' - Replaces the `missing_metadata_err` sentinel pattern with a simple inner
#'   join: pip_ids absent from the metadata catalog are excluded automatically.
#'
#' @param inv_to_clean A `data.table` of DLW surveys sent for processing (as
#'   returned by [valid_dlw_load()]). Must have unique `survey_id` rows.
#' @param pip_id_map A `data.table` with exactly two columns: `survey_id`
#'   (DLW survey identifier) and `pip_id` (PIP identifier, uppercase). Built
#'   from successful `process_data()` calls in [pd_process_data()].
#'
#' @return A `data.table`: the updated PIP master inventory. Does **not**
#'   include `reporting_level` — enrich after load via
#'   `pipload::load_pip_master_inventory(fields = "reporting_level")`.
#'
#' @details
#' **Logging**: writes the following entries to `"pipdata_log"`:
#' - `inv_update_inf`: verification that expected surveys appear in master
#'   (info-level if all confirmed, error-level if any missing).
#' - `release_write_err`: tryCatch-caught release inventory write failure.
#'
#' **Column provenance**:
#' - `version_id_data`, `content_hash_data`, `size_bytes_data`,
#'   `created_at_data`, `path_data` — from the `"pip"` catalog.
#' - `version_id_metadata`, `content_hash_metadata`, `size_bytes_metadata`,
#'   `created_at_metadata`, `path_metadata` — from the `"pip_meta"` catalog.
#' - `pipeline_version_dlw`, `latest_version_id_dlw`, `content_hash_dlw`,
#'   `Checksum_dlw`, `path_dlw` — renamed from DLW inventory columns.
#' - `welfare_type` — derived from the 4th `_`-delimited segment of `pip_id`.
#' - `first_release_version_id`, `latest_release_version_id` — stamp version
#'   IDs of the release inventory (first appearance and most recent).
#'
#' @family pd_process_data pipeline
#' @export
build_pip_inventory <- function(inv_to_clean, pip_id_map) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defensive assertions  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(anyDuplicated(inv_to_clean$survey_id) == 0L)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 1: Query stamp catalogs  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat_data <- stamp::st_catalog_query(alias = "pip")
  cat_meta <- stamp::st_catalog_query(alias = "pip_meta")

  # Step 1b: Guard empty catalogs with differentiated messages
  if (nrow(cat_data) == 0L && nrow(cat_meta) == 0L) {
    if (nrow(pip_id_map) == 0L) {
      cli::cli_abort(
        c(
          "Both stamp catalogs are empty and no surveys were processed.",
          "i" = paste0(
            "If this is the first run, ensure {.fn save_pip_data} succeeds ",
            "for at least one survey."
          ),
          "i" = paste0(
            "If surveys were expected, review the processing log for errors."
          )
        ),
        class = c("build_pip_inventory_empty_first_run", "piperr")
      )
    } else {
      cli::cli_abort(
        c(
          paste0(
            "Stamp catalogs are empty despite {nrow(pip_id_map)} pip_id(s) ",
            "in the map."
          ),
          "i" = "This suggests {.fn pip_write} / {.fn st_save} failed silently.",
          "i" = "Check stamp alias configuration."
        ),
        class = c("build_pip_inventory_empty_catalog", "piperr")
      )
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 2: Derive pip_id from artifact path  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat_data[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]
  cat_meta[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]

  # Step 2b: Validate pip_id format
  pip_id_pattern <-
    "^[A-Z]{3}_[0-9]{4}_[A-Z0-9-]+_(INC|CON)_(ALL|GPWG|D[0-9]+)$"

  bad_data <- cat_data[!grepl(pip_id_pattern, pip_id)]
  bad_meta <- cat_meta[!grepl(pip_id_pattern, pip_id)]

  if (nrow(bad_data) > 0L || nrow(bad_meta) > 0L) {
    bad_ids <- unique(c(bad_data$pip_id, bad_meta$pip_id))
    cli::cli_warn(
      c(
        "{length(bad_ids)} artifact(s) have non-standard pip_id format.",
        "i" = "IDs: {.val {utils::head(bad_ids, 5L)}}",
        "i" = "Expected pattern: COUNTRY_YEAR_ACRONYM_WELFARE_MODULE"
      ),
      class = c("build_pip_inventory_bad_pip_id", "piperr")
    )
    cat_data <- cat_data[grepl(pip_id_pattern, pip_id)]
    cat_meta <- cat_meta[grepl(pip_id_pattern, pip_id)]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 3: Rename catalog columns with suffixes  ---------
  # Drop code_hash (not needed for inventory) and suffix all remaining
  # non-key columns to distinguish data vs metadata artifact provenance.
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

  # Inner join on pip_id: pip_ids absent from either catalog are excluded.
  # join direction: cat_data[cat_meta, ...] → one row per cat_meta row that
  # matches a cat_data row (inner join, nomatch = 0).
  inv <- cat_data[cat_meta, on = "pip_id", nomatch = 0]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 4: Add survey_id via pip_id_map  ---------
  # pip_id_map: data.table(survey_id, pip_id) — only current-run surveys.
  # data.table X[Y] syntax: for each Y row, look up in X → returns Y rows
  # enriched with X columns. Since pip_id_map has survey_id that inv lacks,
  # the join adds survey_id from pip_id_map to matched inv rows.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inv <- inv[pip_id_map, on = "pip_id", nomatch = 0]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 5: Scope to this run's surveys  ---------
  # After joining pip_id_map (current run only), inv already only contains
  # current-run entries. This filter is a defense: keep only surveys that
  # were explicitly scheduled for cleaning.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  run_inv <- inv[survey_id %in% inv_to_clean$survey_id]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 6: Join DLW columns from inv_to_clean  ---------
  # Rename DLW columns BEFORE the join to prevent name collisions with
  # catalog columns (e.g., content_hash, latest_version_id already in run_inv).
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

  run_inv <- joyn::left_join(
    run_inv,
    inv_dlw,
    by = "survey_id",
    relationship = "many-to-one",
    reportvar = FALSE,
    verbose = FALSE
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 7: Derive welfare_type from pip_id  ---------
  # Format: COUNTRY_YEAR_ACRONYM_WELFARE_MODULE. Survey acronyms may contain
  # hyphens, so the 4th _-delimited segment is always welfare_type.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  run_inv[,
    welfare_type := data.table::tstrsplit(pip_id, "_", fixed = TRUE)[[4L]]
  ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 8: Merge with old master  ---------
  # Retain old surveys not reprocessed this run. Crash-safety via stamp
  # catalogs applies only to current-run surveys; prior surveys are recovered
  # from the old master file (which is itself a stamp artifact).
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  old_inv <- tryCatch(
    expr = pipload::load_pip_master_inventory(),
    error = function(e) NULL
  )

  if (!is.null(old_inv)) {
    old_retained <- old_inv[!old_inv$survey_id %in% run_inv$survey_id]
    run_inv <- collapse::rowbind(run_inv, old_retained, fill = TRUE)
  }

  # Assert no duplicate pip_ids after merge (P2.7)
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
  # Step 9: Build and save release inventory  ---------
  # Port of update_pip_inventory() release logic: PFW inpovcal filter,
  # inner join to master, pip_write + release version tracking.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Initialise release version columns unconditionally so the master
  # inventory schema is consistent regardless of release write success.
  if (!"first_release_version_id" %in% names(run_inv)) {
    run_inv[, first_release_version_id := NA_character_]
  }
  if (!"latest_release_version_id" %in% names(run_inv)) {
    run_inv[, latest_release_version_id := NA_character_]
  }

  pfw <- pipload::load_aux_data("pfw", verbose = FALSE)

  pfw_release <- pfw |>
    collapse::fsubset(inpovcal == 1) |>
    collapse::fselect(country_code, surveyid_year, survey_acronym) |>
    collapse::funique() |>
    data.table::as.data.table()

  release_pip_inv <- run_inv[
    pfw_release,
    on = .(country_code, surveyid_year, survey_acronym),
    nomatch = 0
  ]

  release_result <- tryCatch(
    expr = pipload::pip_write(
      x = release_pip_inv,
      id = "pip_release_inventory",
      alias = "pip_inv",
      pk = c("survey_id", "pip_id")
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
  # - latest_release_version_id: always updated for surveys in release.
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
  # Step 10: Save master inventory  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pipload::pip_write(
    x = run_inv,
    id = "pip_master_inventory",
    alias = "pip_master",
    pk = c("survey_id", "pip_id")
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 11: Reload and verify  ---------
  # Verify that surveys from this run appear in the saved master.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pip_inv <- tryCatch(
    pipload::load_pip_master_inventory(),
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
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(pip_inv)
}
