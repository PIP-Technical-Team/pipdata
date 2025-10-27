#' Match GMD datasets from the server with the local inventory
#'
#' @return A data.table of matched datasets, or NULL if no match found
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_gmd_match(inv_gmd_list = "dlw_gmd_inv")
#' }
dlw_gmd_match <- function() {

  # initialize working release and dlw inventory board
  pipfun::get_wrk_release(verbose = FALSE)
  dlw_inv_board  <- pipfun::get_pins_boards(board = "dlw_inventory")

  # check directory existence for inventory board
  check_directory(dlw_inv_board)

  # Step 1: Get current GMD catalog from server --------------------------------
  gmd_cur_list <- dlw::dlw_server_catalog()

  if (is.null(gmd_cur_list)) {
    cli::cli_abort("Failed to download GMD current list from server.")
  }

  gmd_cur_list <- gmd_cur_list[
    Module %in% c("GPWG", "GROUP", "BIN", "HIST", "ALL", "ASPIRE", "L") & Ext == "dta"
  ]

  # Step 2: Load local GMD inventory list --------------------------------------
  inv_gmd <- tryCatch(
    pipload::load_dlw_gmd_inventory(),
    error = function(e) {
      cli::cli_abort("Failed to read local GMD pin list.")
    }
  )

  # Step 3: Compare lists ------------------------------------------------------
  base_file_name <- names(inv_gmd)

  gmd_compare <- inv_gmd[data_available == "Yes", .(FileName, Checksum, data_available)] |>
    joyn::inner_join(gmd_cur_list, by = c("FileName", "Checksum"))

  gmd_compare <- gmd_compare[, -c(".joyn")] |>
    setcolorder(base_file_name)

  return(invisible(gmd_compare))
}


#' Compare local and server GMD datasets to get new entries
#'
#' @param check_missing Logical. If TRUE, includes missing datasets from either side.
#'
#' @return A data.table with new or unmatched GMD datasets.
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_gmd_new(inv_gmd_list = "dlw_gmd_inv")
#' }
dlw_gmd_new <- function(check_missing = TRUE) {

  # initialize working release and dlw inventory board
  pipfun::get_wrk_release(verbose = FALSE)
  dlw_inv_board  <- pipfun::get_pins_boards(board = "dlw_inventory")

  # check directory existence for inventory board
  check_directory(dlw_inv_board)

  # Step 1: Load current GMD server catalog -----------------------------------

  gmd_cur_list <- dlw::dlw_server_catalog()
  if (is.null(gmd_cur_list)) {
    cli::cli_abort("Failed to download GMD current list from server.")
  }

  gmd_cur_list <- gmd_cur_list[
    Module %in% c("GPWG", "GROUP", "BIN", "HIST", "ALL", "ASPIRE", "L") & Ext == "dta"
  ]

  # Step 2: Load local inventory pin (if available) ---------------------------
  inv_gmd <- tryCatch(
    pipload::load_dlw_gmd_inventory(),
    error = function(e) {
      cli::cli_abort("Failed to read local GMD pin list.")
    }
  )

  # Step 3: Compare the lists -------------------------------------------------
  base_file_name <- names(gmd_cur_list)

  gmd_compare <- if (check_missing) {

    inv_gmd[data_available == "Yes", .(FileName, Checksum, data_available)] |>
      joyn::full_join(gmd_cur_list, by = c("FileName", "Checksum"))

  } else {

    inv_gmd[, .(FileName, Checksum, data_available)] |>
      joyn::full_join(gmd_cur_list, by = c("FileName", "Checksum"))
  }

  # keep only new (non-matching) entries from server
  gmd_compare <- gmd_compare[`.joyn` == "y", !c(".joyn", "data_available")] |>
    setcolorder(base_file_name)

  return(gmd_compare)
}

#' Compare local GMD and validation inventory datasets to get new entries
#'
#' @param check_missing Logical. If TRUE, includes missing datasets from validation inventory list.
#'
#' @return A data.table with new or unmatched local GMD datasets.
#' @export
#'
#' @examples
#' \dontrun{
#' gmd_inv_new()
#' }
gmd_inv_new <- function(check_missing = TRUE) {

  # initialize working release and dlw inventory board
  pipfun::get_wrk_release(verbose = FALSE)
  dlw_inv_board  <- pipfun::get_pins_boards(board = "dlw_inventory")
  dlw_meta_board <- pipfun::get_pins_boards(board = "dlw_metadata")

  # check directory existence of inventory and metadata board
  check_directory(dlw_inv_board)
  check_directory(dlw_meta_board)

  # Step 1: Load local inventory pin (if available) ---------------------------

  gmd_inv <- tryCatch(
    pipload::load_dlw_gmd_inventory(),
    error = function(e) {
      cli::cli_abort("Failed to read local GMD pin list.")
    }
  )

  # Step 2: Load GMD validation inventory pin (if available) -------------------

  valid_inv <- tryCatch(
    pipload::load_gmd_valid_inv(),
    error = function(e) {
      cli::cli_abort("Failed to read validation inventory pin list.")
    }
  )

  # Step 3: Compare the GMD pin and validation pin lists -----------------------

  valid_inv <-
    valid_inv[, survey_id := as.character(fs::path(survey_id, ext = "dta"))][, .(survey_id, Checksum, data_available) ] |>
    setnames("survey_id", "FileName")

  gmd_inv <- gmd_inv[data_available == "Yes", ]

  gmd_new <- if (check_missing) {

    valid_inv[data_available == "Yes", .(FileName, Checksum)] |>
      joyn::full_join(gmd_inv, by = c("FileName", "Checksum"))

  } else {

    valid_inv[, .(FileName, Checksum)] |>
      joyn::full_join(gmd_inv, by = c("FileName", "Checksum"))
  }

  # keep only new (non-matching) entries from GMD pin list
  gmd_new <- gmd_new[`.joyn` == "y", !c(".joyn")]

  return(gmd_new)
}

#' Get List of GMD Pins from DLW Data Board
#'
#' Retrieves and processes the list of `.qs` pins from the `dlw_data` board,
#' converting them to `.dta` filenames for downstream processing.
#'
#' @return A `data.table` with columns:
#'   \describe{
#'     \item{FileName}{The `.dta` version of the pin file name (as a string).}
#'     \item{data_status}{An integer flag indicating presence (1).}
#'   }
#'
#' @examples
#' \dontrun{
#' pins_list <- get_pin_list()
#' }
#'
get_pin_list <- function() {
  dlw_data_board <- pipfun::get_pins_boards(board = "dlw_data")
  check_directory(dlw_data_board)

  dlw_pin_list <- as.data.table(pins::pin_list(dlw_data_board))[
    , .(FileName = as.character(fs::path(fs::path_ext_remove(V1), ext = "dta")),
        data_available = "Yes")
  ]

  return(dlw_pin_list)
}


