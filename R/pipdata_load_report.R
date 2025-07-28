#' @title Load DLW Reports (Inventory, Validation, Logging)
#'
#' @description Load a DLW report of the specified type from the inventory folder.
#'
#' @param root_dir Character: Root directory path. Defaults to `Sys.getenv("PIP_ROOT_DIR")`.
#' @param maindir Character: Main directory. Default is "PIP_ingestion_pipeline_v2/dlw_repository".
#' @param dlw_inv_folder Character: Folder where the DLW reports are stored.
#' @param report_type Character: Type of report. One of "inventory", "validation", or "logging".
#'
#' @return A `data.table` containing the requested report data.
#' @export
#'
#' @examples
#' \dontrun{
#' pipdata_load_report(
#'   root_dir    = Sys.getenv("PIP_ROOT_DIR"),
#'   maindir     = "dlw_repository",
#'   dlw_inv_folder = "dlw_inventory",
#'   report_type = "validation"
#' )}
pipdata_load_report <- function(
    root_dir = Sys.getenv("PIP_ROOT_DIR"),
    maindir  = "PIP_ingestion_pipeline_v2/dlw_repository",
    dlw_inv_folder = "dlw_inventory",
    report_type = c("inventory", "validation", "logging")
) {

  report_type <- match.arg(report_type)

  # set up working release
  wrk_release <- pipfun::get_wrk_release()
  release_lbl <- wrk_release$release
  release_idn <- wrk_release$identity

  dlw_output_fld <- fs::path(root_dir, maindir, dlw_inv_folder)
  if (!dir.exists(dlw_output_fld)) {
    cli::cli_abort("DLW inventory folder ({dlw_output_fld}) doesn't exist")
  }

  report_name <- switch(report_type,
                        "inventory"  = fs::path(dlw_output_fld, paste0("dlw_pin_inv_", release_lbl, "_", release_idn), ext = "qs"),
                        "validation" = fs::path(dlw_output_fld, paste0("validation_report_", release_lbl, "_", release_idn), ext = "qs"),
                        "logging"    = fs::path(dlw_output_fld, paste0("log_info_", release_lbl, "_", release_idn), ext = "qs")
  )

  if (!fs::file_exists(report_name)) {
    cli::cli_abort(c("File does not exist", "x" = "{.file {report_name}} not found."))
  }

  pipdata_report <- qs::qread(report_name)
  return(pipdata_report)
}

#' @describeIn pipdata_load_report Load DLW inventory report
#' @export
pidpata_dlw_gmd_inv <- function(
    root_dir = Sys.getenv("PIP_ROOT_DIR"),
    maindir  = "PIP_ingestion_pipeline_v2/dlw_repository",
    dlw_inv_folder = "dlw_inventory"
) {

  inv_data <- pipdata_load_report(
    root_dir = root_dir,
    maindir  = maindir,
    dlw_inv_folder = dlw_inv_folder,
    report_type = "inventory"
  )
  return(inv_data)
}


#' @describeIn pipdata_load_report Load DLW validation report
#' @export
pidpata_dlw_validation <- function(
    root_dir = Sys.getenv("PIP_ROOT_DIR"),
    maindir  = "PIP_ingestion_pipeline_v2/dlw_repository",
    dlw_inv_folder = "dlw_inventory"
) {

  validation_report <- pipdata_load_report(
    root_dir = root_dir,
    maindir  = maindir,
    dlw_inv_folder = dlw_inv_folder,
    report_type = "validation"
  )
  return(validation_report)
}

# Logging
#' @describeIn pipdata_load_report Load DLW logging report
#' @export
pidpata_dlw_log <- function(
    root_dir = Sys.getenv("PIP_ROOT_DIR"),
    maindir  = "PIP_ingestion_pipeline_v2/dlw_repository",
    dlw_inv_folder = "dlw_inventory"
) {
  log_data <- pipdata_load_report(
    root_dir = root_dir,
    maindir  = maindir,
    dlw_inv_folder = dlw_inv_folder,
    report_type = "logging"
  )
  return(log_data)
}
