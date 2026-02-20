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
  pipfun::get_wrk_release(verbose = FALSE)
  pip_folders <- pipfun::get_pip_folders()

  # set paths to root and working folders
  dlw_inv <- pip_folders$dlw_inventory

  # check directory existence for inventory folder
  check_directory(dlw_inv)

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
    current_gmd_list,
    on = .(FileName, Checksum),
    nomatch = 0
  ] |>
    data.table::setcolorder(base_file_name, skip_absent = TRUE)

  return(invisible(gmd_compare))

}


#' Compare the local GMD dataset list with the server version to identify new entries.
#'
#' @param check_missing Logical. If TRUE, includes missing datasets from either side.
#'
#' @return A data.table with new or unmatched GMD datasets.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- dlw_gmd_new()
#' head(df)
#' }
dlw_gmd_new <- function(check_missing = TRUE) {

   # initialize working release and dlw inventory working folder
  pipfun::get_wrk_release(verbose = FALSE)
  pip_folders <- pipfun::get_pip_folders()

  # set paths to root and working folders
  dlw_inv  <- pip_folders$dlw_inventory

  # check directory existence
  check_directory(dlw_inv)

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

  # Step 3: Compare the lists -------------------------------------------------
  base_file_name <- names(current_gmd_list)

  lhs <- local_gmd_inv[, .(FileName, Checksum, data_available)]

  gmd_compare <- unique(
    rbindlist(
      list(
        lhs[current_gmd_list, on = .(FileName, Checksum)],
        current_gmd_list[lhs, on = .(FileName, Checksum)]
      ),
      use.names = TRUE,
      fill = TRUE
    ),
    by = c("FileName", "Checksum")
  ) |>
    data.table::setcolorder(base_file_name, skip_absent = TRUE)

  # remove datasets names in we aready downloaded the data and save them in local drive
  gmd_compare <- gmd_compare[data_available %in% c("No", NA), ]
  if (!check_missing) {
    gmd_compare <- gmd_compare[is.na(data_available), ]
  }

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
#' df <- gmd_inv_new()
#' head(df)
#' }
gmd_inv_new <- function(check_missing = TRUE) {

  # initialize working release and working folder
  pipfun::get_wrk_release(verbose = FALSE)
  pip_folders <- pipfun::get_pip_folders()

  # set paths to root and working folders
  dlw_inv <- pip_folders$dlw_inventory
  dlw_meta <- pip_folders$dlw_metadata


  # check directory existence
  check_directory(dlw_inv)
  check_directory(dlw_meta)

  # Step 1: Load local GMD inventory list --------------------------------------

  # correct file existence check
  gmd_inv_file <- fs::path(dlw_inv, "dlw_gmd_inv.qs2", "dlw_gmd_inv.qs2")

  if (!fs::is_file(gmd_inv_file)) {

    cli::cli_abort("GMD datasets inventory file does not exist in dlw_inventory folder.")

  }

  local_gmd_inv <- tryCatch(
    pipload::load_dlw_gmd_inventory(),
    error = function(e) {
      cli::cli_abort("Failed to read local GMD list.")
    }
   )


  # Step 2: Load GMD validation inventory pin (if available) -------------------

  # correct file existence check
  valid_inv_file <- fs::path(dlw_meta, "gmd_valid_inv.qs2", "gmd_valid_inv.qs2")
  if (!fs::is_file(valid_inv_file)) {

    return(local_gmd_inv)

    cli::cli_abort("GMD validation inventory file does not exist in dlw_metadata folder.")
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
  valid_inv <- valid_inv[data_available == "Yes",
    .(FileName = as.character(survey_id), Checksum, data_available)]

  # valid_inv <-
    # valid_inv[, survey_id := as.character(fs::path(survey_id, ext = "dta"))][, .(survey_id, Checksum, data_available) ] |>
    # setnames("survey_id", "FileName")

  local_gmd_inv <- local_gmd_inv[data_available == "Yes", ]

  # Build comparison table
  if (check_missing) {
    gmd_new <- joyn::full_join(
      valid_inv[data_available == "Yes", .(FileName, Checksum)],
      local_gmd_inv,
      by = c("FileName", "Checksum")
    )
  } else {
    gmd_new <- joyn::full_join(
      valid_inv[, .(FileName, Checksum)],
      local_gmd_inv,
      by = c("FileName", "Checksum")
    )
  }

  # keep only new (non-matching) entries from GMD pin list
  # gmd_new <- gmd_new[`.joyn` == "y", !c(".joyn")]
  gmd_new <- gmd_new[.joyn == "y"][, .joyn := NULL]
  return(gmd_new)
}

#' Retrieve a List of GMD datasets from the Server
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
#' gmd_list <- dlw_gen_gmd_list()
#' head(gmd_list)
#' }
dlw_gen_gmd_list <- function(inv_gmd_list = "dlw_gmd_inv"){

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
  pipfun::get_wrk_release(verbose = FALSE)
  pip_folders <- pipfun::get_pip_folders()

  # set paths to root and working folders
  dlw_inv  <- pip_folders$dlw_inventory

  # check directory existence
  check_directory(dlw_inv)

  # save the GMD list in the local dlw_inventory folder
  pipload::pip_write(x = gmd_list,
                     id = inv_gmd_list,
                     alias = "dlw_inv")
}
