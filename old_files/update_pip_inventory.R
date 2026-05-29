#' Update the PIP master and release inventories with newly cleaned data
#'
#' After surveys are processed by [process_data()], this function
#' merges the version metadata for data and metadata files, removes
#' skipped surveys, appends the new entries to the existing master
#' inventory, and writes both the master and release inventories
#' to storage via [pipload::pip_write()].
#'
#' @param inv_to_clean A `data.table` of the DLW surveys that were
#'   sent for processing (as returned by [valid_dlw_load()]).
#' @param proc_dta A named list of processing results, one per survey.
#'   Each element is a list with `pip_names`, `versions_data`, and
#'   `versions_metadata`, or `NULL` for failed surveys.
#'
#' @return A `data.table`: the updated PIP master inventory, including the
#'   following additional columns:
#'   - `reporting_level`: Character `"1"` or `"2"`. Derived from PFW domain
#'     columns (`cpi_domain`, `ppp_domain`, `gdp_domain`, `pce_domain`,
#'     `pop_domain`). `"1"` = national (all domains equal 1); `"2"` =
#'     subnational (at least one domain equals 2, meaning urban/rural-specific
#'     auxiliary data are available for that survey). `NA` when the survey has
#'     no matching PFW row with `inpovcal == 1`.
#'   - `first_release_version_id`: stamp version ID of the release inventory
#'     when this survey first appeared.
#'   - `latest_release_version_id`: stamp version ID of the most recent release
#'     inventory that confirmed this survey.
#'
#' @details
#' Release inventory vintages are tracked via stamp's built-in version history.
#' To load a previous release inventory snapshot:
#' `pipload::pip_read("pip_release_inventory", version = -1, alias = "pip_inv")`
#'
#' **Logging**: This function writes several informational entries to the `"pipdata_log"`:
#' - `null_svys_inf`: List of surveys that failed processing (when applicable).
#' - `release_write_err`: Release inventory write failure.
#' - `inv_update_inf`: Inventory verification summary showing the number of surveys
#'   expected, confirmed in master inventory, and missing. Written as an error-level
#'   entry if any surveys are missing, info-level if all are confirmed.
#' - `skipped_svys_data`: Surveys skipped during data processing with reasons.
#' - `skipped_svys_metadata`: Surveys skipped during metadata creation with reasons.
#' - `missing_metadata_err`: pip_ids excluded from inventory due to absent metadata entry (error-level;
#'   includes `pip_ids` and `surveys` arrays).
#'
#' @family pd_process_data pipeline
#' @export
update_pip_inventory <- function(
  inv_to_clean,
  proc_dta
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check null surveys and clean

  null_ls <- names(Filter(is.null, proc_dta))

  if (length(null_ls) > 0) {
    pipfun::log_add(
      event = "info",
      message = "Some surveys were not cleaned. Review logmeta to identify which ones.",
      name = "pipdata_log",
      logmeta = list(info = "null_svys_inf", surveys = null_ls)
    )
  }

  process_data_clean <- proc_dta[!(names(proc_dta) %in% null_ls)]

  # Pip data cleaned

  svys <- lapply(lapply(process_data_clean, \(x) as.list(x$pip_names)), as.list)

  pip_inv <- data.frame(
    survey_id = rep(names(svys), lengths(svys)),
    pip_id = unlist(svys, use.names = FALSE)
  )

  # Bind versions

  vrs_dt <- format_vrs(
    process_data = process_data_clean,
    version = "versions_data"
  )

  vrs_mdt <- format_vrs(
    process_data = process_data_clean,
    version = "versions_metadata"
  )

  # Use one-to-one so any cross-product is an immediate error rather than
  # silently producing duplicated rows in multi-pip_id surveys (e.g. BOL 2022
  # with BOL_2022_EH_INC_ALL + BOL_2022_EH_INC_GPWG).
  vrs <- vrs_dt |>
    joyn::left_join(
      vrs_mdt,
      by = c("survey_id", "pip_id"),
      suffix = c("_data", "_metadata"),
      relationship = "one-to-one",
      reportvar = FALSE,
      verbose = FALSE
    )

  # Exclude pip_ids for which metadata was not successfully saved.
  # These have NA for every metadata column; allowing them into the inventory
  # would cause pd_deflation() to fail when resolving content_hash_metadata.
  # Use content_hash as the canonical presence sentinel — it is always written
  # by format_vrs() and appears as content_hash_metadata after the suffixed join.
  sentinel_col <- if ("content_hash" %in% names(vrs_mdt)) {
    "content_hash_metadata"
  } else {
    NULL
  }
  if (!is.null(sentinel_col) && sentinel_col %in% names(vrs)) {
    missing_meta <- vrs[is.na(get(sentinel_col)), .(survey_id, pip_id)]
    if (nrow(missing_meta) > 0L) {
      pipfun::log_add(
        event = "error",
        message = "Some pip_ids have no metadata. They will be excluded from the inventory.",
        name = "pipdata_log",
        logmeta = list(
          error = "missing_metadata_err",
          pip_ids = missing_meta$pip_id,
          surveys = missing_meta$survey_id
        )
      )
      vrs <- vrs[!is.na(get(sentinel_col))]
    }
  }

  # Remove skipped surveys from inventory
  if ("skipped_data" %in% names(vrs)) {
    skipped_svys_data <- vrs$survey_id[vrs$skipped_data == TRUE]
    reasons <- vrs$reason_data[vrs$skipped_data == TRUE]

    pipfun::log_add(
      event = "info",
      message = "Some surveys were skipped during processing. Review logmeta to identify which ones.",
      name = "pipdata_log",
      logmeta = list(
        info = "skipped_svys_data",
        surveys = skipped_svys_data,
        reasons = reasons
      )
    )

    vrs <- vrs[vrs$skipped_data != TRUE, ]
  } else if ("skipped_metadata" %in% names(vrs)) {
    skipped_svys_metadata <- vrs$survey_id[vrs$skipped_metadata == TRUE]
    reasons <- vrs$reason_metadata[vrs$skipped_metadata == TRUE]

    pipfun::log_add(
      event = "info",
      message = "Some surveys were skipped during metadata creation. Review logmeta to identify which ones.",
      name = "pipdata_log",
      logmeta = list(
        info = "skipped_svys_metadata",
        surveys = skipped_svys_metadata,
        reasons = reasons
      )
    )

    vrs <- vrs[vrs$skipped_metadata != TRUE, ]
  }

  # Exit if all surveys were skipped
  if (nrow(vrs) == 0) {
    cli::cli_abort(
      "All surveys were skipped during processing. No inventory to update. Review logmeta to identify which ones."
    )
  }

  # Add info from DLW inventory

  pip_inv <- pip_inv |>
    unique() |>
    joyn::inner_join(
      vrs,
      by = c("survey_id", "pip_id"),
      reportvar = FALSE,
      verbose = FALSE
    ) |>
    joyn::left_join(
      inv_to_clean,
      by = "survey_id",
      relationship = "many-to-one",
      reportvar = FALSE,
      verbose = FALSE
    ) |>
    collapse::frename(
      pipeline_version = "pipeline_version_dlw",
      latest_version_id = "latest_version_id_dlw",
      content_hash = "content_hash_dlw",
      Checksum = "Checksum_dlw",
      file_path = "path_dlw"
    ) |>
    # Extract welfare_type from pip_id (format: country_year_acronym_welfare_module).
    # Survey acronyms use hyphens not underscores, so the 4th _-segment is always
    # the welfare type (e.g. "BOL_2022_EH_INC_ALL" -> "INC").
    collapse::fmutate(
      welfare_type = data.table::tstrsplit(pip_id, "_", fixed = TRUE)[[4L]]
    )

  # Save master inventory
  # NOTE: exclude old entries for the same survey_ids only after the join, so
  # surveys dropped by the inner_join (e.g. due to missing version metadata)
  # are not silently removed from the existing inventory.
  old_pip_inv <- tryCatch(
    expr = {
      old_inv <- pipload::load_pip_master_inventory()
      old_inv[!old_inv$survey_id %in% pip_inv$survey_id, ]
    },
    error = function(e) NULL
  )

  new_pip_inv <- pip_inv |>
    collapse::rowbind(old_pip_inv, fill = TRUE) |>
    collapse::funique() |>
    as.data.table()

  # Save release inventory first so its version_id can be recorded in the master

  pfw <- pipload::load_aux_data("pfw", verbose = FALSE)

  # Compute reporting_level from PFW and join into the master inventory.
  # Uses the same domain-max logic as report_lvl() in get_country_pfw.R but
  # operates on the full PFW before splitting by welfare_type, so that one
  # reporting_level row is produced per (country_code, surveyid_year,
  # survey_acronym). Domain columns are the same across welfare_type rows for
  # the same survey, so the per-welfare_type distinction is irrelevant here.
  pfw_rl <- pfw[inpovcal == 1L]
  missing_dcols_inv <- setdiff(.DOMAIN_COLS, names(pfw_rl))
  if (length(missing_dcols_inv) == 0L) {
    pfw_rl[,
      reporting_level := as.character(do.call(pmax, .SD)),
      .SDcols = .DOMAIN_COLS
    ]
    pfw_rl_unq <- pfw_rl[,
      .(reporting_level = reporting_level[[1L]]),
      by = .(country_code, surveyid_year, survey_acronym)
    ]
    # Drop ALL reporting_level* columns before joining pfw_rl_unq.
    # On a re-run, old_pip_inv may carry the exact column, or suffixed variants
    # (reporting_level.x, reporting_level.y) from a historic joyn collision that
    # was persisted to the master inventory.
    drop_rl_cols(new_pip_inv)
    new_pip_inv <- joyn::left_join(
      new_pip_inv,
      pfw_rl_unq,
      by = c("country_code", "surveyid_year", "survey_acronym"),
      relationship = "many-to-one",
      reportvar = FALSE,
      verbose = FALSE
    )
  } else {
    cli::cli_warn(
      "PFW is missing domain columns {.field {missing_dcols_inv}}; {.col reporting_level} will be NA.",
      class = c("update_pip_inventory", "piperr")
    )
    new_pip_inv[, reporting_level := NA_character_]
  }

  pfw_release <- pfw |>
    collapse::fsubset(inpovcal == 1) |>
    collapse::fselect(country_code, surveyid_year, survey_acronym) |>
    collapse::funique() |>
    as.data.table()

  release_pip_inv <- new_pip_inv[
    pfw_release,
    on = .(country_code, surveyid_year, survey_acronym),
    nomatch = 0
  ]

  release_result <- tryCatch(
    pipload::pip_write(
      x = release_pip_inv,
      id = "pip_release_inventory",
      alias = "pip_inv",
      pk = c("survey_id", "pip_id")
    ),
    error = function(e) {
      pipfun::log_error(
        "Release inventory write failed. Master inventory will be saved without release version columns.",
        name = "pipdata_log",
        logmeta = list(
          error = "release_write_err",
          condition_msg = conditionMessage(e)
        )
      )
      NULL
    }
  )

  # Resolve the release inventory stamp version_id.
  # pip_write() returns list(version_id, ...) but may return skipped = TRUE when
  # content is unchanged — fall back to st_latest() in that case.
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

  # Initialise release version columns unconditionally so the master inventory
  # schema is consistent regardless of whether the release write succeeded.
  if (!"first_release_version_id" %in% names(new_pip_inv)) {
    new_pip_inv[, first_release_version_id := NA_character_]
  }
  if (!"latest_release_version_id" %in% names(new_pip_inv)) {
    new_pip_inv[, latest_release_version_id := NA_character_]
  }

  # Populate release version columns on master inventory.
  # - first_release_version_id: set only when currently NA (i.e. first appearance)
  # - latest_release_version_id: always updated for surveys present in the release
  if (!is.na(release_vid)) {
    release_ids <- release_pip_inv$survey_id

    new_pip_inv[
      survey_id %in% release_ids & is.na(first_release_version_id),
      first_release_version_id := release_vid
    ]
    new_pip_inv[
      survey_id %in% release_ids,
      latest_release_version_id := release_vid
    ]
  }

  # Save master inventory (after populating release version columns)
  pipload::pip_write(
    x = new_pip_inv,
    id = "pip_master_inventory",
    alias = "pip_master",
    pk = c("survey_id", "pip_id")
  )

  pip_inv <- tryCatch(
    pipload::load_pip_master_inventory(),
    error = function(e) NULL
  )

  # Verify that successfully processed surveys are in the master inventory
  successful_ids <- names(process_data_clean)
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

#' Reshape version metadata from processing results into a data.table
#'
#' Extracts either `"versions_data"` or `"versions_metadata"` from each
#' survey's processing result and row-binds them into a single
#' `data.table` with a `survey_id` column.
#'
#' @param process_data A named list of processing results (non-NULL
#'   entries only).
#' @param version Character scalar. Which version list to extract:
#'   `"versions_data"` or `"versions_metadata"`.
#'
#' @return A `data.table` with columns `survey_id`, `pip_id`, and
#'   version metadata fields.
#'
#' @family pd_process_data pipeline
#' @keywords internal
format_vrs <- function(
  process_data,
  version = c("versions_data", "versions_metadata")
) {
  version <- match.arg(version)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dt <- data.table::rbindlist(
    lapply(
      process_data,

      \(x) {
        pip_names <- unlist(x$pip_names)

        # Safely access nested lists using [[ ]] and handle missing entries.
        # Iterate over each pip_name separately: x[[version]][[pip_name]] with
        # a length > 1 vector performs recursive indexing in R, not multi-key
        # lookup, silently returning NULL for surveys with multiple pip_names.
        if (is.null(x[[version]])) {
          return(NULL)
        }

        rows <- lapply(pip_names, \(pip_name) {
          if (is.null(x[[version]][[pip_name]])) {
            return(NULL)
          }

          ventry <- x[[version]][[pip_name]]
          vlist <- NULL
          if (!is.null(ventry$metadata) && length(ventry$metadata) > 0) {
            vlist <- ventry$metadata
          }
          vlist$pip_id <- pip_name
          # Capture stamp's version_id at the top level of the pip_write()
          # return. This allows direct version lookup in .load_deflation_aux()
          # without the fragile content_hash -> version_id resolution.
          if (!is.null(ventry$version_id)) {
            vlist$version_id <- ventry$version_id
          }
          if (!is.null(ventry$skipped) && ventry$skipped == TRUE) {
            vlist$skipped <- TRUE
            vlist$reason <- ventry$reason
          }
          vlist$parents <- NULL
          vlist$attrs <- NULL
          vlist
        })

        non_null <- Filter(Negate(is.null), rows)
        if (length(non_null) == 0L) {
          return(NULL)
        }
        data.table::rbindlist(non_null, fill = TRUE)
      }
    ),

    idcol = "survey_id",
    fill = TRUE
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)
}

#' Drop all reporting_level* columns from a data.table in-place
#'
#' Removes `reporting_level`, `reporting_level.x`, `reporting_level.y`, and
#' any other columns whose name starts with `reporting_level` from `dt` by
#' reference. Called by [update_pip_inventory()] before joining the fresh
#' PFW-derived `reporting_level` to ensure exactly one clean column results.
#'
#' @param dt A `data.table`. Modified by reference.
#' @return `dt` invisibly (modification is in-place).
#'
#' @keywords internal
#' @noRd
drop_rl_cols <- function(dt) {
  rl_cols <- grep("^reporting_level", names(dt), value = TRUE)
  if (length(rl_cols) > 0L) {
    dt[, (rl_cols) := NULL]
  }
  invisible(dt)
}
