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
#' @param release Character. The data release identifier or date, used to configure the working environment.
#' @param identity Character. One of `"PROD"`, `"INT"`, or `"TEST"`.
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
#'             check_missing   = TRUE,
#'             release         = "20260206",
#'             identity        = "TEST"
#'             )
#' }
pipdata_dlw_process <- function(
    inv_gmd_list = "dlw_gmd_inv",
    get_dlw_data = TRUE,
    validate_dlw_data = TRUE,
    log  = TRUE,
    save_log = TRUE,
    check_missing = TRUE,
    release = NULL,
    identity = NULL,
    verbose = getOption("pipdata.verbose", default = TRUE)
){

  # 0) setup working environment
  if (is.null(release)) {
    cli::cli_abort("Data release date is not provided")
  }

  if (is.null(identity)) {
    cli::cli_abort("Identity type is not provided")
  }

  pipfun::setup_working_release(
    release = release,
    identity = identity,
    #main_dir = root,
    verbose = FALSE
  )

  # Register the dedicated "pip_deflated" stamp alias so deflated outputs
  # from pd_deflate_pipeline() are versioned separately from the cleaned
  # "pip" artifacts (same root pattern as the "piplog" alias).
  stamp::st_init(
    root = fs::path(getOption("pipfun.main_dir"), "pip_repository", "pip_deflated"),
    alias = "pip_deflated"
  )

  # Guard: assert a working release is configured. Downstream delegates

  # (pipdata_get_gmd, pipdata_validate_gmd, dlw_gmd_new, dlw_gmd_list) rely
  # on this guard and do not call get_wrk_release() themselves.
  pipfun::get_wrk_release()
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

      dlw_gmd_list()
    }

  }


  # 2) Checks for new datasets -------------------------------------------------
  if (get_dlw_data){

    pipdata_get_gmd(inv_gmd_list = inv_gmd_list,
                        log  = log,
                        save_log = save_log,
                        check_missing = check_missing,
                        verbose = verbose)

  }

  # 3) Validate the datasets and update the inventory
  if (validate_dlw_data){

    pipdata_validate_gmd(log = log, save_log = save_log, verbose = verbose)
  }

}

