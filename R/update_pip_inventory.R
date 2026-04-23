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
#' @param date_valid A `POSIXct` timestamp. Only surveys validated
#'   before this date are included in the release inventory.
#'   Defaults to the maximum `date_validated` in `inv_to_clean`.
#'
#' @return A `data.table`: the updated PIP master inventory.
#'
#' @details
#' **Logging**: This function writes several informational entries to the `"pipdata_log"`:
#' - `null_svys_inf`: List of surveys that failed processing (when applicable).
#' - `inv_update_inf`: Inventory verification summary showing the number of surveys
#'   expected, confirmed in master inventory, and missing. Written as an error-level
#'   entry if any surveys are missing, info-level if all are confirmed.
#' - `skipped_svys_data`: Surveys skipped during data processing with reasons.
#' - `skipped_svys_metadata`: Surveys skipped during metadata creation with reasons.
#'
#' @family pd_process_data pipeline
#' @export
update_pip_inventory <- function(
  inv_to_clean,
  proc_dta,
  date_valid = max(inv_to_clean$date_validated)
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses
  if (!inherits(date_valid, "POSIXct")) {
    cli::cli_abort("date_valid should be POSIXct format")
  }

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

  vrs <- vrs_dt |>
    joyn::left_join(
      vrs_mdt,
      by = c("survey_id", "pip_id"),
      suffix = c("_data", "_metadata"),
      relationship = "many-to-many",
      reportvar = FALSE,
      verbose = FALSE
    )

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

  pipload::pip_write(
    x = new_pip_inv,
    id = "pip_master_inventory",
    alias = "pip_master",
    pk = c("survey_id", "pip_id")
  )

  # Save release inventory

  pfw <- pipload::load_aux_data("pfw", verbose = FALSE)

  pfw_release <- pfw |>
    collapse::fsubset(inpovcal == 1) |>
    collapse::fselect(country_code, surveyid_year, survey_acronym) |>
    collapse::funique() |>
    as.data.table()

  release_pip_inv <- new_pip_inv[
    pfw_release,
    on = .(country_code, surveyid_year, survey_acronym),
    nomatch = 0
  ][
    # Need to change it for a warning
    date_validated < date_valid
  ]

  pipload::pip_write(
    x = release_pip_inv,
    id = "pip_release_inventory",
    alias = "pip_inv",
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
