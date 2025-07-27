#' Update Main Inventory File and Generate the Latest Release
#'
#' This function updates the main inventory file located in the specified directory and generates the latest release based on the provided parameters. It is designed to facilitate the management of inventory data by allowing users to specify the root directory, inventory folder, and release details.
#'
#' @inheritParams dlw_get_qs
#'
#' @returns Invisibly returns a dataframe containing list of GMD datasets
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_gmd_list(root_dir = "/path/to/root",
#'             maindir = "dlw_repository",
#'             dlw_inv_folder = "dlw_inventory",
#'             inv_gmd_list = "inv_dlw_raw_main")
#'
#' }
dlw_gmd_list <- function(
    root_dir = Sys.getenv("PIP_ROOT_DIR"),
    maindir  = "PIP_ingestion_pipeline_v2/dlw_repository",
    dlw_inv_folder = "dlw_inventory",
    inv_gmd_list = "dlw_gmd_inv"){


  # set-up a release
  pipfun::get_wrk_release()
  wrk_release <- pipfun::get_wrk_release()
  release_lbl <- wrk_release$release
  release_idn <- wrk_release$identity

  # folder path to inventory file
  inv_folder <- fs::path(root_dir, maindir, dlw_inv_folder)
  if (!dir.exists(inv_folder)) {

    cli::cli_abort(
      "Inventory foldr ({inv_folder}) is not avaiable"
    )

  }

  # download GMD catalog list
  ctl <- dlw::dlw_server_catalog()

  # check if inventory file name is provided

  if (is.null(inv_gmd_list)){

    cli::cli_abort(
      "Inventory file name is not provided"
    )
  }

  # file path to inventory file for the specified
  inv_lates_name <- fs::path(inv_folder,
                       paste0(inv_gmd_list, "_",
                              release_lbl, "_",
                              release_idn),
                       ext = "qs")

  if (is.null(ctl)){

    cli::cli_abort(
      "GMD catalog list is not downloaded"
    )

  }

  # save the underline release gmd catalog list
  ctl |> qs::qsave(inv_lates_name)


  invisible(NULL)

}
