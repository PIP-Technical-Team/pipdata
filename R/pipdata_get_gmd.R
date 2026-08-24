#' Retrieve and Save GMD Catalog Datasets to a Local Directory
#'
#' @description
#' This wrapper function automates the process of managing GMD catalog
#' datasets by performing the following tasks:
#' 1. Checks for new datasets in the GMD catalog using the inventory
#' file (`dlw_gmd_inv`), which contains the current list of available GMD datasets.
#' 2. If new datasets are found (specifically `"GPWG"`, `"GROUP"`,
#' `"BIN"`, `"HIST"`, `"ALL"`, `"ASPIRE"`, and `"L"`), downloads them
#' using `dlw::dlw_get_gmd` and save them to the local directory.
#' 3. Updates the inventory file (`dlw_gmd_inv`) with information about
#' the newly downloaded datasets.
#'
#' Logging is unconditional. The function writes `dlw_acquisition_inf` entries
#' for start, no-new-data, per-survey download failures, and completion. Error
#' conditions are represented by `condition_msg` in `logmeta`; the discriminator
#' in `logmeta$error` is always a string.
#'
#' @param inv_gmd_list Character. The name of the inventory file containing the list of GMD datasets.
#' @param check_missing Logical. Whether to check for and retrieve missing data. Default is `TRUE`.
#' @param verbose Logical. Controls verbosity of downstream
#'   [pipload::pip_write()] calls. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#'
#' @note This function expects a working release to be configured via
#'   [pipfun::setup_working_release()]. When called from
#'   [pipdata_dlw_process()], the release is already set. When called
#'   standalone, ensure `setup_working_release()` has been invoked first.
#'
#' @return Invisibly returns `NULL`; the acquisition inventory is persisted as a
#'   side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' pipdata_get_gmd(
#'   inv_gmd_list = "dlw_gmd_inv",
#'   check_missing = TRUE
#' )
#' }
pipdata_get_gmd <- function(
  inv_gmd_list = "dlw_gmd_inv",
  check_missing = TRUE,
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  #### -------------------------------------------------------------------------

  pip_folders <- pipfun::get_pip_folders()

  # check directory existence for root, inventory, and data folders
  check_directory(pip_folders$dlw_data)
  check_directory(pip_folders$dlw_inventory)

  ### -------------------------------------------------------------------------

  # 1) check if there is any new GMD datasets
  inv_gmd <- tryCatch(
    dlw_gmd_new(check_missing = check_missing, update_inventory = TRUE),
    error = function(e) {
      pipfun::log_error(
        "Failed to load the GMD catalog.",
        name = "pipdata_log",
        logmeta = list(
          error = .logtype_dlw_acquisition,
          phase = "catalog_load",
          inventory = inv_gmd_list,
          condition_msg = conditionMessage(e)
        )
      )
      rlang::abort("Failed to load the GMD catalog.", parent = e)
    }
  )

  if (is.null(inv_gmd) || nrow(inv_gmd) == 0) {
    pipfun::log_info(
      "No new GMD data was found.",
      name = "pipdata_log",
      logmeta = list(
        info = .logtype_dlw_acquisition,
        phase = "no_new_data",
        n_surveys = 0L,
        inventory = inv_gmd_list
      )
    )
    return(invisible(NULL))
  }

  # 2) get the data from GMD catalog and pin to local folder -------------------
  cli::cli_alert_info("Working folder: {.dir {pip_folders$dlw_data}}")

  cli::cli_progress_bar("Downloading GMD files", total = nrow(inv_gmd))

  # inv_gmd$data_available <- NA

  # ctry_partial <- c("BOL","CHN", "NGA", "IND", "IDN", "COL", "PHL", "ARG", "LUX", "FRA")
  module_partial <- c("ALL", "GROUP", "HIST", "GPWG", "BIN")
  # inv_gmd <- inv_gmd[(Country %in% ctry_partial & Module %in% module_partial), ]
  inv_gmd <- inv_gmd[(Module %in% module_partial), ]

  if (nrow(inv_gmd) == 0L) {
    pipfun::log_info(
      "No new GMD data matched the active modules.",
      name = "pipdata_log",
      logmeta = list(
        info = .logtype_dlw_acquisition,
        phase = "no_new_data",
        n_surveys = 0L,
        inventory = inv_gmd_list
      )
    )
    return(invisible(NULL))
  }

  inv_gmd[, data_available := NA_character_]
  n_surveys <- nrow(inv_gmd)
  pipfun::log_info(
    "DLW acquisition started.",
    name = "pipdata_log",
    logmeta = list(
      info = .logtype_dlw_acquisition,
      phase = "start",
      n_surveys = n_surveys,
      inventory = inv_gmd_list
    )
  )

  for (i in seq_len(nrow(inv_gmd))) {
    # extract relevant information for the current row to get the data
    country <- inv_gmd[["Country"]][i]
    year <- inv_gmd[["Year"]][i]
    svy_acronym <- inv_gmd[["Survey_acronym"]][i]
    vermst <- inv_gmd[["Vermast"]][i]
    veralt <- inv_gmd[["Veralt"]][i]
    md_type <- inv_gmd[["Module"]][i]
    survey_id <- fs::path_ext_remove(inv_gmd[["FileName"]][i])

    filename <- inv_gmd[["FileName"]][i] |>
      fs::path_ext_remove()

    filename <- paste0(filename, ".qs2")

    tryCatch(
      {
        dlw::dlw_get_gmd(
          country_code = country,
          year = year,
          survey = svy_acronym,
          module = md_type,
          vermast = vermst,
          veralt = veralt,
          local_dir = pip_folders$dlw_data
        )

        # Mark data as available if download is successful
        inv_gmd$data_available[i] <- "Yes"
      },
      error = function(e) {
        msg <- glue::glue(
          'Could not download a file: ({country}, {year}, {svy_acronym}, {md_type}, {vermst}, {veralt})'
        )

        pipfun::log_error(
          msg,
          name = "pipdata_log",
          logmeta = list(
            error = .logtype_dlw_acquisition,
            phase = "download",
            survey = survey_id,
            country = country,
            year = year,
            module = md_type,
            file_name = inv_gmd[["FileName"]][i],
            vermast = vermst,
            veralt = veralt,
            condition_msg = conditionMessage(e)
          )
        )

        cli::cli_inform(msg)
        inv_gmd$data_available[i] <- "No"
      }
    )

    cli::cli_progress_update()
  }

  # Done processing all raw dta files
  cli::cli_progress_done()
  inv_gmd$data_available[is.na(inv_gmd$data_available)] <- "No"

  download_status <- inv_gmd$data_available
  failed_idx <- which(download_status == "No")
  n_failed <- length(failed_idx)
  failed_surveys <- if (n_failed > 0L) {
    fs::path_ext_remove(inv_gmd[["FileName"]][failed_idx])
  } else {
    character(0)
  }

  # 3) save the GMD list -------------------------------------------------------
  # get list of datasets already saved in the local folder
  inv_gmd_match <- tryCatch(
    dlw_gmd_match(),
    error = function(e) {
      pipfun::log_error(
        "Failed to match existing GMD files.",
        name = "pipdata_log",
        logmeta = list(
          error = .logtype_dlw_acquisition,
          phase = "inventory_match",
          artifact = inv_gmd_list,
          condition_msg = conditionMessage(e)
        )
      )
      rlang::abort("Failed to match existing GMD files.", parent = e)
    }
  )

  if (!is.null(inv_gmd_match) && nrow(inv_gmd_match) != 0) {
    inv_gmd <- rbind(inv_gmd, inv_gmd_match, ignore.attr = TRUE, fill = TRUE)
    inv_gmd <- unique(inv_gmd)
  }

  tryCatch(
    {
      write_result <- pipload::pip_write(
        x = inv_gmd,
        id = inv_gmd_list,
        pk = c("Checksum", "FileName"),
        alias = "dlw_inv",
        verbose = verbose
      )
      .validate_pip_write_result(write_result, inv_gmd_list)
    },
    error = function(e) {
      pipfun::log_error(
        "Failed to save the GMD inventory.",
        name = "pipdata_log",
        logmeta = list(
          error = .logtype_dlw_acquisition,
          phase = "inventory_save",
          artifact = inv_gmd_list,
          path = pip_folders$dlw_inventory,
          condition_msg = conditionMessage(e)
        )
      )
      rlang::abort("Failed to save the GMD inventory.", parent = e)
    }
  )

  cli::cli_alert_success(
    "GMD inventory file is saved at: {.dir {pip_folders$dlw_inventory}}"
  )

  pipfun::log_info(
    "DLW acquisition complete.",
    name = "pipdata_log",
    logmeta = list(
      info = .logtype_dlw_acquisition,
      phase = "complete",
      n_surveys = n_surveys,
      n_success = n_surveys - n_failed,
      n_failed = n_failed,
      failed_surveys = failed_surveys,
      saved_at = pip_folders$dlw_inventory,
      artifact = inv_gmd_list
    )
  )

  invisible(NULL)
}
