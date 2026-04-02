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
    )

  # Save master inventory

  old_pip_inv <- tryCatch(
    pipload::load_pip_master_inventory(),
    error = function(e) NULL
  )

  old_pip_inv <- old_pip_inv[!old_pip_inv$survey_id %in% pip_inv$survey_id, ]

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(pip_inv)
}

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
        pip_name <- unlist(x$pip_names)

        # Safely access nested lists using [[ ]] and handle missing entries
        vlist <- NULL

        if (!is.null(x[[version]]) && !is.null(x[[version]][[pip_name]])) {
          ventry <- x[[version]][[pip_name]]
          if (!is.null(ventry$metadata) && length(ventry$metadata) > 0) {
            vlist <- ventry$metadata
          }
          vlist$pip_id <- pip_name
          if (!is.null(ventry$skipped) && ventry$skipped == TRUE) {
            vlist$skipped <- TRUE
            vlist$reason <- ventry$reason
          }
        }

        # Remove parents and attrs
        vlist$parents <- NULL
        vlist$attrs <- NULL

        return(vlist)
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
