#' Process DLW data
#'
#' @description
#' A wrapper function that performs two main tasks:
#' 1. Generate GMD catalog inventory file in `.qs` format via `dlw` package.
#' 2. Get datasets from GMD catalog (pinned them in a specific folder), validate and and generate inventory of the datasets as `.qs` format
#'
#' @inheritParams dlw_get_qs
#' @param dlw_inv_list Logical. If \code{TRUE}, generate GMD inventory list `.qs` format.
#' @param dlw_qs_pinned Logical. If \code{TRUE}, pinned (`.qs` format) and validate modular GMD datasets.
#'
#' @returns Invisibly returns `NULL`. Output files are written to disk.
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_process(root_dir = "/path/to/root",
#'             maindir = "dlw_repository",
#'             dlw_inv_folder = "dlw_inventory",
#'             dlw_pin_folder = "dlw_pin",
#'             inv_gmd_list = "dlw_gmd_inv",
#'             inv_qs_list  = "dlw_pin_inv",
#'             validation_report_path = "validation_report",
#'             dlw_inv_list    = TRUE,
#'             dlw_qs_pinned   = TRUE,
#'             log             = TRUE,
#'             save_log        = TRUE
#'             )
#' }
dlw_process <- function(
    root_dir = Sys.getenv("PIP_ROOT_DIR"),
    maindir  = "PIP_ingestion_pipeline_v2/dlw_repository",
    dlw_inv_folder = "dlw_inventory",
    dlw_pin_folder = "dlw_data",
    inv_gmd_list = "dlw_gmd_inv",
    inv_qs_list  = "dlw_pin_inv",
    validation_report_path = "validation_report",
    dlw_inv_list    = FALSE,
    dlw_qs_pinned   = TRUE,
    log             = TRUE,
    save_log        = TRUE
){

 # dlw::dlw_test_token() ----------
  # get working release info----------------------------------------------------
  pipfun::get_wrk_release()
  wrk_release <- pipfun::get_wrk_release()
  release_lbl <- wrk_release$release
  release_idn <- wrk_release$identity

  # directories and paths ------------------------------------------------------

  # path to dlw pin folder
  dlw_pin_fld <- fs::path(root_dir, maindir, dlw_pin_folder)
  if (!dir.exists(dlw_pin_fld)) {

    cli::cli_abort(
      "DLW pin folder ({dlw_pin_fld}) is not avaiable"
    )
  }

  # path to inventory folder
  dlw_inv_fld  <- fs::path(root_dir, maindir, dlw_inv_folder)
  if (!dir.exists(dlw_inv_fld)) {

    cli::cli_abort(
      "DLW QS folder ({dlw_inv_fld}) is not avaiable"
    )
  }

  # construct log file name
  dlw_log_path <- fs::path(dlw_inv_fld,
                           paste0("log_info", "_",
                                  release_lbl, "_",
                                  release_idn), ext = "qs")

  # 1) Generate GMD datasets list ----------------------------------------------

  if (dlw_inv_list) {

    dlw_gmd_list(root_dir = root_dir,
                 maindir  = maindir,
                 dlw_inv_folder = dlw_inv_folder,
                 inv_gmd_list = inv_gmd_list)
  }

  # 2) Download and validate .qs datasets, and generate inventory and validation report

  if (dlw_qs_pinned) {

    dlw_get_qs(root_dir = root_dir,
               maindir  = maindir,
               dlw_inv_folder = dlw_inv_folder,
               dlw_pin_folder = dlw_pin_folder,
               inv_gmd_list = inv_gmd_list,
               inv_qs_list  = inv_qs_list,
               validation_report_path = validation_report_path,
               log             = log)

  }

  # 3) Save logging information
  if (save_log && log) {

   pipfun::log_save("pipdata_log", dlw_log_path)

  }

}


#' Get pin board name/ path
#'
#' @inheritParams dlw_get_qs
#'
#' @returns Pin folder name/ path
#' @export
#'
#' @examples
#' \dontrun{
#' pin_board <- pipdata_pin_board()
#' }
pipdata_pin_board <- function(
    root_dir  = Sys.getenv("PIP_ROOT_DIR"),
    maindir = "PIP_ingestion_pipeline_v2/dlw_repository",
    dlw_pin_folder = "dlw_data"){


  # check if pin board folder exists -------------------------------------------
  if (!dir.exists(fs::path(root_dir, maindir, dlw_pin_folder))) {

    cli::cli_abort(
      "Pin folder ({dlw_pin_folder}) is not avaiable"
    )

  }

  pin_board_fld <- fs::path(root_dir, maindir, dlw_pin_folder)

  return(invisible(pin_board_fld))

}
