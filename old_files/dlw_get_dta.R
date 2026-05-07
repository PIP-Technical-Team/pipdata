#' Get Raw (dta) data
#'
#' This function get raw (dta) datasets from GMD catalog and pinned them to DLW raw folder
#'
#' @param root_dir Character. Root directory where the DLW datasets are saved. Defaults to the environment variable "PIP_ROOT_DIR".
#' @param dta_folder Character. Directory where the raw DLW datasets are saved. Default folder is "dlw_raw".
#' @param inv_folder Character. Name of the folder containing the inventory files. Default is "dlw_inventory".
#' @param gmd_list Character. List of GMD datasets that need to be downloaded.
#'
#' @returns Datases in .dta format
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_get_dta(root_dir = "/path/to/root",
#'             dta_folder = "dlw_raw",
#'             inv_folder = "dlw_inventory",
#'             gmd_list   = "inv_dlw_raw_main")
#'
#' }
dlw_get_dta <- function(root_dir   = Sys.getenv("PIP_ROOT_DIR"),
                        dta_folder = "dlw_raw",
                        inv_folder = "dlw_inventory",
                        gmd_list   = "inv_dlw_raw_main",
                        log  = TRUE){


  # set-up a release
  pipfun::get_wrk_release()


  if (log) {

    pipfun::log_add("info", "Start downloading DLW .dta from GMD catalog",
                    name = "pipdata_log",
                    args = list(root_dir = root_dir,
                                dta_folder = dta_folder,
                                inv_folder = inv_folder,
                                gmd_list = gmd_list))
  }

  # folder path to raw file / pin board folder for raw datasets
  raw_fld <- fs::path(root_dir, dta_folder)
  if (!dir.exists(raw_fld)) {

    cli::cli_abort(
      "Inventory foldr ({raw_fld}) is not avaiable"
    )
  }

  # folder path to inventory file
  inv_fld <- fs::path(root_dir, inv_folder)
  if (!dir.exists(inv_fld)) {

    cli::cli_abort(
      "Inventory foldr ({inv_fld}) is not avaiable"
    )
  }

  # file path to the inventory files
  inv_path <- fs::path(root_dir, inv_folder, gmd_list, ext = "qs")
  if (!file.exists(inv_path )) {

    cli::cli_abort(
      "Main DLW Raw inventory file ({inv_path}) is not avaiable"
    )
  }

  # load main inventory file
  inv_main_data <- tryCatch(
    qs::qread(inv_path),
    error = function(e) {
      pipfun::log_add("error", "Failed to load inventory file",
                      name = "pipdata_log",
                      logmeta = list(error = e$message))
      cli::cli_abort(
        "Could not open main inventory file ({inv_path})")
      NULL
    }
  )

  # select only the basic modules and dta files
  inv_main_data <- inv_main_data[(Module %in% c("GPWG", "GROUP", "BIN",
                                    "HIST", "ALL", "ASPIRE", "L") & ext == "dta"),]

  cli::cli_progress_bar("Downloading .dta", total = nrow(inv_main_data))

  # download .dta from GMD catalog
  for (i in 1:nrow(inv_main_data)) {

    tryCatch(

      {
        dlw::dlw_get_gmd(country_code = inv_main_data[["Country"]][i],
                         year         = inv_main_data[["Year"]][i],
                         module       = inv_main_data[["Module"]][i],
                         vermast      = inv_main_data[["Vermast"]][i],
                         veralt       = inv_main_data[["Veralt"]][i],
                         local_dir    = raw_fld,
                         board_type   = "folder")
      },
      error = function(e) {

        msg <- glue::glue('Could not downaload a file: ({inv_main_data[["Country"]][i]},
          {inv_main_data[["Year"]][i]}, {inv_main_data[["Module"]][i]},
          {inv_main_data[["Vermast"]][i]}, {inv_main_data[["Veralt"]][i]})')

        if (log) {

          # "Failed to download .dta file"
          pipfun::log_add("error", msg,
                          name = "pipdata_log",
                          logmeta = list(error = e))
        } else {

          print(e)

        }

        cli::cli_inform(msg)

        NULL
      }
    )

    # board |> pin_write(data_name, "data_name", type = "qs", versioned = TRUE)

    cli::cli_progress_update()
  }

  # Done processing all raw dta files
  cli::cli_progress_done()

  if (log) {

    pipfun::log_add("info", "End downloading DLW .dta from GMD catalog",
                    name = "pipdata_log")
  }


  invisible(NULL)
}
