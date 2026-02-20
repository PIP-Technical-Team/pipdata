#' Process DLW Data
#'
#' @description
#' This wrapper function automates key steps in processing DLW data by performing the following tasks:
#' 1. Checks if the list of GMD DLW datasets is available, if not, get the list
#' 2. Checks for new datasets in the GMD catalog, downloads them using `dlw::dlw_get_gmd`,
#'    and save them to the local directory.
#' 3. Validates the downloaded datasets using `pipdata::pipdata_validate_gmd`
#'    and updates the validation inventory (`"gmd_valid_inv"`).
#'
#' @inheritParams pipdata_get_gmd
#' @param get_dlw_data Logical. Whether to check for and download new DLW data. Default is `TRUE`.
#' @param validate_dlw_data Logical. Whether to validate newly downloaded datasets. Default is `TRUE`.
#'
#' @returns Invisibly returns `NULL`. Output files are written to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' pipdata_dlw_process(inv_gmd_list = "dlw_gmd_inv",
#'             get_dlw_data = TRUE,
#'             validate_dlw_data = TRUE,
#'             log             = TRUE,
#'             save_log        = TRUE,
#'             check_missing   = TRUE
#'             )
#' }
pipdata_dlw_process <- function(
    inv_gmd_list = "dlw_gmd_inv",
    get_dlw_data = TRUE,
    validate_dlw_data = TRUE,
    log  = TRUE,
    save_log = TRUE,
    check_missing = TRUE
){

  ## setting up dlw token
  dlwtoken <- Sys.getenv("dlw_token")
  dlw::dlw_set_token(dlwtoken)

  ## setup working environment
  lr <- pipfun::get_latest_pip_release()

  if (is.null(lr$release)) {
    cli::cli_abort("Data release date is not provided")
  }

  if (is.null(lr$identity)) {
    cli::cli_abort("Identity type is not provided")
  }


  # root <- fs::path(Sys.getenv("PIP_ROOT_DIR"),
  #                  "PIP_ingestion_pipeline_v2/testing_folder")

  # pipfun::setup_working_release(release = lr$release,
  #                               identity = lr$identity,
  #                               main_dir = root,
  #                               verbose = FALSE)

  pipfun::setup_working_release(release = lr$release,
                                identity = lr$identity,
                                verbose = FALSE)

  pipfun::get_wrk_release(verbose = FALSE)
  pip_folders <- pipfun::get_pip_folders()

  # check directory existence for inventory, and data folders
  check_directory(pip_folders$dlw_data)
  check_directory(pip_folders$dlw_inventory)

  # 1) Checks if the list of GMD DLW datasets is available ---------------------
  gmd_list <- fs::path(
    pip_folders$dlw_inventory,
    "dlw_gmd_inv.qs2",
    "dlw_gmd_inv.qs2"
  )

  if (!fs::is_file(gmd_list)) {

    cli::cli_text(
      "Local GMD list is not available.\n",
      "Expected location: {.path {pip_folders$dlw_inventory}}\n",
      "What would you like to do?"
    )

    choice <- utils::menu(
      choices = c(
        "Download GMD list",
        "Abort"
      ),
      title = "Select an option"
    )

    # menu() returns 0 if user presses ESC
    if (choice == 0 || choice == 2) {
      cli::cli_abort("Process aborted by user.")
    }

    if (choice == 1) {
      cli::cli_alert_info(
        cli::cli_text(
          "Downloading GMD list to {.path {pip_folders$dlw_inventory}}"
        )
      )

      dlw_gen_gmd_list()
    }

  }


  # 2) Checks for new datasets -------------------------------------------------
  if (get_dlw_data){

    pipdata_get_gmd(inv_gmd_list = inv_gmd_list,
                        log  = log,
                        save_log = save_log,
                        check_missing = check_missing)

  }

  # 3) Validate the datasets and update the inventory
  if (validate_dlw_data){

    pipdata_validate_gmd(log = log, save_log = save_log)
  }

}

