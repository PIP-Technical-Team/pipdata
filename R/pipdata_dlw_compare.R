#' Get the list of current GMD datasets that match the local inventory
#'
#' @return A data.table of matched datasets, or NULL if no match found
#' @export
#'
#' @examples
#' \dontrun{
#' df <- dlw_gmd_match()
#' head()
#' }
dlw_gmd_match <- \() {

  # initialize working release and dlw inventory working folder
  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()

  # check directory existence for inventory folder
  check_directory(pip_folders$dlw_inventory)

  # Step 1: Load local GMD inventory list --------------------------------------
  local_gmd_inv <- tryCatch(
    pipload::load_dlw_gmd_inventory(),
     error = function(e) {
      cli::cli_abort("Failed to read local GMD list.")
    }
  )

  # Step 2: Get current GMD catalog from server --------------------------------
  current_gmd_list <- dlw::dlw_server_catalog()

  if (is.null(current_gmd_list)) {
    cli::cli_abort("Failed to download GMD current list from server.")
  }

  current_gmd_list <- current_gmd_list[
    Module %in% c("GPWG", "GROUP", "BIN", "HIST", "ALL", "ASPIRE", "L") & Ext == "dta"
  ]

  # Step 3: Compare lists ------------------------------------------------------
  base_file_name <- names(local_gmd_inv)

  gmd_compare <- local_gmd_inv[
    data_available == "Yes",
    .(FileName, Checksum, data_available)
  ][
    current_gmd_list[,],
    on = .(FileName, Checksum),
    nomatch = 0
  ] |>
    data.table::setcolorder(base_file_name, skip_absent = TRUE)

  return(invisible(gmd_compare))

}


#' Compare the local GMD dataset list with the server version to identify new entries.
#'
#' @param check_missing Logical. If TRUE, includes missing datasets from either side.
#' @param update_inventory Logical. If TRUE, updates the local inventory with new entries. Default is FALSE.
#' 
#' @return A data.table with new or unmatched GMD datasets.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- dlw_gmd_new()
#' head(df)
#' }
dlw_gmd_new <- function(check_missing = TRUE, update_inventory = FALSE) {
  # initialize working release and dlw inventory working folder
  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()

  # check directory existence
  check_directory(pip_folders$dlw_inventory)

  # Step 1: Load local GMD inventory list --------------------------------------
  local_gmd_inv <- tryCatch(
    pipload::load_dlw_gmd_inventory(),
    error = function(e) {
      cli::cli_abort("Failed to read local GMD list.")
    }
  )

  # Step 2: Get current GMD catalog from server --------------------------------
  current_gmd_list <- dlw::dlw_server_catalog()

  if (is.null(current_gmd_list)) {
    cli::cli_abort("Failed to download GMD current list from server.")
  }

  current_gmd_list <- current_gmd_list[
    Module %in%
      c("GPWG", "GROUP", "BIN", "HIST", "ALL", "ASPIRE", "L") &
      Ext == "dta"
  ]

  # Step 3: Compare the lists -------------------------------------------------
  base_file_name <- names(current_gmd_list)

  lhs <- local_gmd_inv[, .(FileName, Checksum, data_available)]

  # if check_missing, also include local records marked as missing
  if (check_missing) {
    lhs <- lhs[!data_available %in% c("No", NA)]
  }

  # keep only records that are in current_gmd_list and NOT in local_gmd_inv
  gmd_compare <- current_gmd_list[
    !lhs,
    on = .(FileName, Checksum)
  ]

  #gmd_compare <- gmd_compare |>
  #  data.table::setcolorder(base_file_name, skip_absent = TRUE)

  # replace NA in data_available with "No"
  gmd_compare <- gmd_compare[,
     data_available := "No"
  ]

  # if update_inventory, append new entries to local inventory and save
  if (update_inventory) {

    # filter local inventory for records that are marked as missing if check_missing is TRUE
    local_gmd_inv <- local_gmd_inv[!data_available %in% c("No", NA)]

    updated_inventory <- rbindlist(list(local_gmd_inv, gmd_compare), use.names = TRUE, fill = TRUE)
    #updated_inventory <- unique(updated_inventory, by = c("FileName", "Checksum"))

    # check if there are duplicates in the updated inventory
    if (any(duplicated(updated_inventory, by = c("FileName", "Checksum")))) {
      cli::cli_abort("Duplicate entries found in the updated inventory.")
    }

    # save the updated inventory in the local dlw_inventory folder
    pipload::pip_write(x = updated_inventory,
                       id = "dlw_gmd_inv",
                       pk = c("Checksum", "FileName"),
                       alias = "dlw_inv")
  }

  return(gmd_compare)
  }

#' Get un-validated datasets list
#'
#' @param check_missing Logical. If TRUE, includes missing datasets from validation inventory list.
#'
#' @return A data.table with new or unmatched local GMD datasets.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- dlw_gmd_unvalidated()
#' head(df)
#' }
dlw_gmd_unvalidated <- function(check_missing = TRUE) {
  # initialize working release and working folder
  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()

  # check directory existence
  check_directory(pip_folders$dlw_inventory)
  check_directory(pip_folders$dlw_metadata)

  # Step 1: Load local GMD inventory list --------------------------------------

  # check if there is gmd inventory list
  gmd_inv_file <- fs::path(
    pip_folders$dlw_inventory,
    "dlw_gmd_inv.qs2",
    "dlw_gmd_inv.qs2"
  )

  if (!fs::is_file(gmd_inv_file)) {
    cli::cli_abort(
      "GMD datasets inventory file does not exist in dlw_inventory folder."
    )
  }

  local_gmd_inv <- tryCatch(
    pipload::load_dlw_gmd_inventory(),
    error = function(e) {
      cli::cli_abort("Failed to read local GMD list.")
    }
  )

  # filter datasets that are already downloaded
  local_gmd_inv <- local_gmd_inv[data_available == "Yes", ]

  # Step 2: Load GMD validation inventory file -------------------

  # check if there is gmd validation inventory list
  valid_inv_file <- fs::path(
    pip_folders$dlw_metadata,
    "gmd_valid_inv.qs2",
    "gmd_valid_inv.qs2"
  )

  if (!fs::is_file(valid_inv_file)) {
    return(local_gmd_inv)

    cli::cli_abort(
      "GMD validation inventory file does not exist in dlw_metadata folder."
    )
  }

  valid_inv <- tryCatch(
    pipload::load_gmd_valid_inv(),
    error = function(e) {
      cli::cli_abort("Failed to read validation inventory data.")
    }
  )

  # Step 3: Compare the GMD and validation inventory lists -----------------------
  # Make survey_id end in `.dta`
  valid_inv[, survey_id := fs::path_ext_set(survey_id, "dta")]
  valid_inv <- valid_inv[,
    .(FileName = as.character(survey_id), Checksum, data_available)
  ]

  if (!check_missing) {
    valid_inv <- valid_inv[!data_available %in% c("No", NA)]
  }

  # Build comparison table
  gmd_new <- local_gmd_inv[
    !valid_inv,
    on = .(FileName, Checksum)
  ]

  return(gmd_new)
}

#' Retrieve a List of GMD datasets from the Server and save it in the local dlw inventory folder.
#'
#' This function fetches a list of GMD datasets from the server,
#' filters them based on Module and file extension.
#'
#' @inheritParams pipdata_get_gmd
#'
#' @return A data table containing the list of GMD datasets.
#' @export
#'
#' @examples
#' \dontrun{
#' gmd_list <- dlw_gmd_list()
#' head(gmd_list)
#' }
dlw_gmd_list <- function(inv_gmd_list = "dlw_gmd_inv"){

  # Step 1: Get current GMD catalog from server --------------------------------
  gmd_list <- dlw::dlw_server_catalog()

  if (is.null(gmd_list)) {
    cli::cli_abort("Failed to download GMD current list from server.")
  }

  # Add data_available column and assign "No" to all entries
  gmd_list[, data_available := "No"]

  # Filter for specific modules and file extension
  gmd_list <- gmd_list[
    Module %in% c("GPWG", "GROUP", "BIN", "HIST", "ALL", "ASPIRE", "L") & Ext == "dta"
  ]

  # Save the gmd_list in the local dlw_inventory folder
  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()

  # check directory existence
  check_directory(pip_folders$dlw_inventory)

  # save the GMD list in the local dlw_inventory folder
  pipload::pip_write(x = gmd_list,
                     id = inv_gmd_list,
                     pk = c("Checksum", "FileName"),
                     alias = "dlw_inv")
}
