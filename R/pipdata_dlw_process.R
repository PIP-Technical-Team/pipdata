#' Process DLW Data
#'
#' @description
#' This wrapper function automates key steps in processing DLW data by performing the following tasks:
#' 1. Checks for new datasets in the GMD catalog, downloads them using `dlw::dlw_get_gmd`, and pins them to the local directory.
#' 2. Validates the downloaded datasets and updates the validation inventory (`"gmd_valid_inv"`).
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
#'             check_missing   = TRUE
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
    identity = NULL
){

  # 0) setup working environment
  if (is.null(release)) {
    cli::cli_abort("Data release date is not provided")
  }

  if (is.null(identity)) {
    cli::cli_abort("Identity type is not provided")
  }

  pipfun::setup_working_release(release = release,
                                identity = identity)

  # 1) Checks for new datasets -------------------------------------------------
  if (get_dlw_data){

    pipdata_get_gmd(inv_gmd_list = inv_gmd_list,
                        log  = log,
                        save_log = save_log,
                        check_missing = check_missing)

  }

  # 2) Validate the datasets and update the inventory
  if (validate_dlw_data){

    pipdata_validate_gmd(log = log, save_log = save_log)
  }

}

