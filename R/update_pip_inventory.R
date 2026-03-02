update_pip_inventory <- function(
  inv_to_clean,
  process_data,
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

  null_ls <- names(Filter(is.null, process_data))

  if (length(null_ls) > 0) {
    pipfun::log_add(
      event = "info",
      message = "Some surveys were not cleaned. Review logmeta to identify which ones.",
      name = "pipdata_log",
      logmeta = list(info = "null_svys_inf", surveys = null_ls)
    )
  }

  process_data_clean <- process_data[!(names(process_data) %in% null_ls)]

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

  # Add info from DLW inventory

  pip_inv <- pip_inv |>
    unique() |>
    joyn::left_join(
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

  new_pip_inv <- pip_inv |>
    collapse::rowbind(old_pip_inv) |>
    collapse::funique() |>
    as.data.table()

  pipload::pip_write(
    x = new_pip_inv,
    id = "pip_master_inventory",
    dir = pipfun::get_pip_folders(name = "pip_master_inventory")
  )

  # Save release inventory

  pfw <- pipload::pip_load_aux("pfw", verbose = FALSE)

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
    dir = pipfun::get_pip_folders(name = "pip_inventory")
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(new_pip_inv)
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
