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
#' @return A `data.table` object pinned in the local folder.
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

  pipfun::get_wrk_release(verbose = FALSE)
  pip_folders <- pipfun::get_pip_folders()

  dlw_data <- pip_folders$dlw_data
  dlw_inv  <- pip_folders$dlw_inventory

  # check directory existence for inventory and dlw data
  check_directory(dlw_data)
  check_directory(dlw_inv)

  ### -------------------------------------------------------------------------

  # 1) check if there is any new GMD datasets

  inv_gmd <- dlw_gmd_new(check_missing = check_missing)

  if (is.null(inv_gmd) || nrow(inv_gmd) == 0) cli::cli_abort("There is no new data on GMD catalog")

  # 2) get the data from GMD catalog and pin to local folder -------------------

  cli::cli_alert_info("Working folder: {.dir {dlw_data}}")

  cli::cli_progress_bar("Downloading .qs", total = nrow(inv_gmd))

  inv_gmd$data_available <- NA
  inv_gmd <- inv_gmd[c(1:4),]
  for (i in seq_along(1:nrow(inv_gmd))){

    # extract relevant information for the current row to get the data
    country     <- inv_gmd[["Country"]][i]
    year        <- inv_gmd[["Year"]][i]
    svy_acronym <- inv_gmd[["Survey_acronym"]][i]
    vermst      <- inv_gmd[["Vermast"]][i]
    veralt      <- inv_gmd[["Veralt"]][i]
    md_type     <- inv_gmd[["Module"]][i]
    coll        <- inv_gmd[["Collection"]][i]

    tryCatch(
      {
        dlw::dlw_get_gmd(
          country_code = country,
          year = year,
          survey = svy_acronym, 
          module = md_type,
          vermast = vermst,  
          veralt = veralt,  
          local_dir = dlw_data
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
          pipfun::log_add("error", msg,
                          name = "pipdata_log",
                          args = list(country      = country,
                                      year         = year,
                                      survey       = svy_acronym,
                                      module       = md_type,
                                      vermast      = vermst,
                                      veralt       = veralt,
                                      local_dir    = dlw_data),
                          logmeta = list(error = e))
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

  if (!is.null(inv_gmd_match)){

    inv_gmd <- rbind(inv_gmd, inv_gmd_match, ignore.attr=TRUE, fill = TRUE)
  }

  stamp::st_init(dlw_inv)
  # dlw_inv |>
    # pipload::pip_write(inv_gmd, inv_gmd_list)

  pipload::pip_write(x = inv_gmd,
    id = inv_gmd_list,
    dir = dlw_inv,
    format  = "qs2")


  cli::cli_alert_success("GMD inventory file is saved at: {.dir {dlw_inv}}")

  if (log) {

    pipfun::log_add("info", "Inventory file is saved",
                    name = "pipdata_log",
                    logmeta = list(saved_at = dlw_inv))
  }

  # 4) save the logging file ---------------------------------------------------

  if (save_log & log) {

    pipfun::log_save(name = "pipdata_log", board = dlw_inv_board, pin_name = "dlw_gmd_log")

    pipfun::log_add("info", "logging file is saved",
                    name = "pipdata_log",
                    logmeta = list(log_file = "dlw_gmd_log"))

    cli::cli_alert_success("GMD logging file is saved")

  }

  invisible(NULL)
}
