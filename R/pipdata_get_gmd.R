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
#' @param inv_gmd_list Character. The name of the inventory file containing the list of GMD datasets.
#' @param log Logical. Whether to keep logging information. Default is `TRUE`.
#' @param save_log Logical. Whether to save logging information to a file. Default is `TRUE`.
#' @param check_missing Logical. Whether to check for and retrieve missing data. Default is `TRUE`.
#'
#' @return A `data.table` object saved in the local folder.
#' @export
#'
#' @examples
#' \dontrun{
#' pipdata_get_gmd(
#'   inv_gmd_list = "dlw_gmd_inv",
#'   log = FALSE,
#'   save_log = FLASE
#' )
#' }
pipdata_get_gmd <- function(
    inv_gmd_list = "dlw_gmd_inv",
    log  = TRUE,
    save_log = TRUE,
    check_missing = TRUE
) {

  #### logging -----------------------------------------------------------------

  if (log) {

    pipfun::log_add("info", "Start getting GMD data",
                    name = "pipdata_log",
                    args = list(inv_gmd_list = inv_gmd_list))
  }

  #### -------------------------------------------------------------------------

  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()

  # check directory existence for root, inventory, and data folders
  check_directory(pip_folders$dlw_data)
  check_directory(pip_folders$dlw_inventory)

  ### -------------------------------------------------------------------------

  # 1) check if there is any new GMD datasets
  inv_gmd <- dlw_gmd_new(check_missing = check_missing, update_inventory = TRUE)


  if (is.null(inv_gmd) || nrow(inv_gmd) == 0) cli::cli_abort("There is no new data on GMD catalog")

  # 2) get the data from GMD catalog and pin to local folder -------------------
  cli::cli_alert_info("Working folder: {.dir {pip_folders$dlw_data}}")

  cli::cli_progress_bar("Downloading GMD files",
          total = nrow(inv_gmd))

  # inv_gmd$data_available <- NA

  # ctry_partial <- c("BOL","CHN", "NGA", "IND", "IDN", "COL", "PHL", "ARG", "LUX", "FRA")
  module_partial <- c("ALL", "GROUP", "HIST", "GPWG", "BIN")
  # inv_gmd <- inv_gmd[(Country %in% ctry_partial & Module %in% module_partial), ]
  inv_gmd <- inv_gmd[(Module %in% module_partial), ]

  for (i in seq_along(1:nrow(inv_gmd))) {
    # extract relevant information for the current row to get the data
    country <- inv_gmd[["Country"]][i]
    year <- inv_gmd[["Year"]][i]
    svy_acronym <- inv_gmd[["Survey_acronym"]][i]
    vermst <- inv_gmd[["Vermast"]][i]
    veralt <- inv_gmd[["Veralt"]][i]
    md_type <- inv_gmd[["Module"]][i]
    coll <- inv_gmd[["Collection"]][i]

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

        if (log) {
          # "Failed to download .dta file"
          pipfun::log_add(
            "error",
            msg,
            name = "pipdata_log",
            args = list(
              country = country,
              year = year,
              survey = svy_acronym,
              module = md_type,
              vermast = vermst,
              veralt = veralt,
              local_dir = pip_folders$dlw_data
            ),
            logmeta = list(error = e)
          )
        }

        cli::cli_inform(msg)
        inv_gmd$data_available[i] <- "No"
      }
    )

    cli::cli_progress_update()
  }

  # Done processing all raw dta files
  cli::cli_progress_done()
  inv_gmd$data_available[is.na(inv_gmd$data_available)] <- "No"

  # 3) save the GMD list -------------------------------------------------------
  # get list of datasets already saved in the local folder
  inv_gmd_match <- dlw_gmd_match()

  if (!is.null(inv_gmd_match) & nrow(inv_gmd_match) != 0){

    inv_gmd <- rbind(inv_gmd, inv_gmd_match, ignore.attr=TRUE, fill = TRUE)
    inv_gmd <- unique(inv_gmd)

  }

  pipload::pip_write(x = inv_gmd,
    id = inv_gmd_list,
    pk = c("Checksum", "FileName"),
    alias = "dlw_inv")

  cli::cli_alert_success(
    "GMD inventory file is saved at: {.dir {pip_folders$dlw_inventory}}"
  )

  if (log) {

    pipfun::log_add(
      "info",
      "Inventory file is saved",
      name = "pipdata_log",
      logmeta = list(saved_at = pip_folders$dlw_inventory)
    )
  }

  # 4) save the logging file ---------------------------------------------------
  if (save_log & log) {

    pipfun::log_save(name = "pipdata_log", id = "dlw_gmd_log", alias = "dlw_inv")

    cli::cli_alert_success("GMD logging file is saved")

  }

  invisible(NULL)
}
