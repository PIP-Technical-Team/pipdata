#' Copy DLW Metadata Between Release Folders
#'
#' Copies the DLW metadata (inventory, validation report, and log)
#' from one release folder to another.
#'
#' @param from_release Character. Release period **to copy metadata from**.
#' @param from_identity Character. Identity type **to copy metadata from**.
#' @param to_release Character. Release period **to copy metadata to**.
#' @param to_identity Character. Identity type **to copy metadata to**.
#'
#' @details
#' The function:
#' 1. Sets up a working environment for the source release folder to load,
#'    the metadata (`gmd_valid_inv`, `gmd_valid_report`, and `gmd_valid_log`).
#' 2. Sets up a working environment for the destination release folder to copy 
#' the metadata.
#'
#' @return Invisibly returns `TRUE` if the operation completes successfully.
#'
#' @examples
#' \dontrun{
#' copy_dlw_metadata(
#'   from_release = "20250203",
#'   from_identity = "TEST",
#'   to_release = "20250811",
#'   to_identity = "TEST"
#' )
#' }
#'
#' @export
copy_dlw_metadata <- function(from_release = NULL,
                      from_identity = NULL,
                      to_release = NULL,
                      to_identity = NULL) {

  # 1) Setup: source environment ------------------------------------------------
  if (is.null(from_release)) {
    cli::cli_abort("Release period (to copy dlw metadata from) is not provided.")
  }

  if (is.null(from_identity)) {
    cli::cli_abort("Identity type (to copy dlw metadata from) is not provided.")
  }

  cli::cli_inform(c(i = "Setting up source working folder for release {.val {from_release}} / identity {.val {from_identity}}..."))
  pipfun::setup_working_release(release = from_release,
                                identity = from_identity)

  pipfun::get_wrk_release(verbose = FALSE)
  pip_folders <- pipfun::get_pip_folders()

  dlw_meta <- pip_folders$dlw_metadata
  check_directory(dlw_meta)

  # Load pins from source board
  cli::cli_inform(c(i = "Loading metadata from source board..."))
  stamp::st_init(dlw_meta)
  gmd_inv_df       <- pipload::load_gmd_valid_inv()
  valid_report_df  <- pipload::load_gmd_valid_report()
  log_df           <- pipload::load_gmd_valid_log()

  # 2) Setup: destination environment -------------------------------------------
  if (is.null(to_release)) {
    cli::cli_abort("Release period (to copy dlw metadata to) is not provided.")
  }

  if (is.null(to_identity)) {
    cli::cli_abort("Identity type (to copy dlw metadata to) is not provided.")
  }

  cli::cli_inform(c(i = "Setting up destination working folder for release {.val {to_release}} / identity {.val {to_identity}}..."))
  pipfun::setup_working_release(release = from_release,
                                identity = from_identity)

  pipfun::get_wrk_release(verbose = FALSE)
  pip_folders <- pipfun::get_pip_folders()

  dlw_meta <- pip_folders$dlw_metadata
  check_directory(dlw_meta)

  # 3) Write dlw metadata to destination folder ------------------------------------------
  stamp::st_init(dlw_meta)
  pipload::pip_write(x = gmd_inv_df,
    id = "gmd_valid_inv",
    dir = dlw_meta,
    format  = "qs2")
  
  pipload::pip_write(x = valid_report_df,
    id = "validation_report",
    dir = dlw_meta,
    format  = "qs2")
  
  pipload::pip_write(x = log_df,
    id = "dlw_validation_log",
    dir = dlw_meta,
    format  = "qs2")
  
  cli::cli_alert_success("DLW metadata successfully copied from {.val {from_release}} to {.val {to_release}}.")

  invisible(TRUE)
}
