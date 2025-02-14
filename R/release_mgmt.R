# 1. Release management functions -----
## List all available releases ----
#' List DLW releases from a master `.qs` file
#'
#' This function reads a named list of releases from a `.qs` file (the master list
#' that \code{dlw_store_release()} updates) and extracts each release's label.
#' It then parses the label to extract a \code{year}, \code{month}, and everything after
#' the underscore as \code{type} (if present).
#'
#' @param qs_path Character. Path to the `.qs` file storing a named list of releases
#'   (as created by \code{dlw_store_release(..., update_inventory_list=TRUE)}).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{\code{label}}{Full release label, e.g. "20250202_INT"}
#'   \item{\code{year}}{Characters 1-4 of label, e.g. "2025"}
#'   \item{\code{month}}{Characters 5-6 of label, e.g. "02"}
#'   \item{\code{type}}{Substring after the underscore, or \code{NA} if none found.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_list_releases("pip_raw/pip_raw_inventory_releases.qs")
#' }
dlw_list_releases <- function(qs_path) {

  # 1. Check if file exists ---
  if (!file.exists(qs_path)) {
    cli::cli_alert_warning("No .qs file at '{qs_path}'. No releases found.")
    return(data.frame(
      label = character(),
      year  = character(),
      month = character(),
      type  = character(),
      stringsAsFactors = FALSE
    ))
  }

  # 2. Read the master list (named list) from .qs ---
  master_list <- qs::qread(qs_path)

  # 3. Extract the release labels (the names of master_list) ---
  release_labels <- names(master_list)
  if (length(release_labels) == 0) {
    cli::cli_alert_info("No releases found in '{qs_path}'.")
    return(data.frame(
      label = character(),
      year  = character(),
      month = character(),
      type  = character(),
      stringsAsFactors = FALSE
    ))
  }

  # 4. Build output data frame ---
  out_df <- data.frame(
    label = release_labels,
    stringsAsFactors = FALSE
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      year = substr(label, 1, 4),
      month = substr(label, 5, 6),
      type = {
        # look for underscore
        uscore_pos <- regexpr("_", label)
        if (uscore_pos > 0) {
          substr(label, uscore_pos + 1, nchar(label))
        } else {
          NA_character_
        }
      }
    ) |>
    dplyr::ungroup()

  return(out_df)
}



## Get a specific release ----
#' Retrieve a DLW release data frame from `_release/` folder
#'
#' This function looks for a single `.qs` file named
#' \code{pip_raw_inventory_<release_label>.qs} inside the \code{release_folder}/\code{_release} directory
#' and returns its data frame content. If the file does not exist, an error is raised.
#'
#' @param release_folder Character. Path to the main folder that contains a \code{_release/} subfolder.
#' @param release_label Character. The label used in the file name, e.g. \code{"20250202_INT"}.
#'
#' @return A data frame (the content previously saved for that release).
#' @export
#'
#' @examples
#' \dontrun{
#' df <- dlw_get_release_folder(
#'   release_folder = "data",
#'   release_label  = "20250202_INT"
#' )
#' }
dlw_get_release_folder <- function(
    release_folder,
    release_label
) {
  # 1. Construct the file path
  release_dir  <- file.path(release_folder, "_release")
  release_file <- file.path(release_dir, paste0("pip_raw_inventory_", release_label, ".qs"))

  # 2. Check existence
  if (!file.exists(release_file)) {
    stop("Release file not found: ", release_file, call. = FALSE)
  }

  # 3. Read the data frame
  df <- qs::qread(release_file)

  # (Optional) If you expect a data frame, verify:
  if (!is.data.frame(df)) {
    stop("File '", release_file, "' did not contain a data frame.", call. = FALSE)
  }

  return(df)
}

#' Retrieve a DLW release from the master list `.qs` file
#'
#' This function reads a named list of releases in a `.qs` file (the master list),
#' checks that \code{release_label} exists, and reconstructs the data frame from
#' the stored row-based structure (\code{$data}). If the release has no rows, it returns
#' an empty data frame with no columns.
#'
#' @param qs_path Character. Path to the `.qs` file storing the named list of releases
#'   (as created by \code{dlw_store_release(..., update_inventory_list=TRUE)}).
#' @param release_label Character. Which release to fetch, e.g. \code{"20250202_INT"}.
#'
#' @return A data frame containing the rows for the requested release. If no rows,
#'   returns an empty data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- dlw_get_release_list(
#'   qs_path       = "data/pip_raw_inventory_releases.qs",
#'   release_label = "20250202_INT"
#' )
#' }
dlw_get_release_list <- function(
    qs_path,
    release_label
) {

  # 1. Check file existence
  if (!file.exists(qs_path)) {
    stop("Master list file not found: ", qs_path, call. = FALSE)
  }

  # 2. Load the master list (named list of releases)
  master_list <- qs::qread(qs_path)

  # 3. Check if release_label is in the list
  if (!release_label %in% names(master_list)) {
    stop("Release label '", release_label, "' not found in file: ", qs_path, call. = FALSE)
  }

  # 4. Extract the release object
  release_obj <- master_list[[release_label]]
  row_list    <- release_obj$data

  # 5. If no rows, return an empty data frame
  if (length(row_list) == 0) {
    cli::cli_alert_info("Release '{release_label}' has no rows.")
    return(data.frame())
  }

  # 6. Each element in row_list is a named list => convert each to a data frame
  df_rows <- lapply(row_list, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  })
  out_df <- dplyr::bind_rows(df_rows)

  return(out_df)
}


## Compare two releases ----


# 2. Release simulation functions -----
## Note: they will be superseeded by pipfun functions
create_release_label <- function(
    date      = Sys.Date(),
    ppp_round = "2017",
    rv        = "01",           # release version
    av        = "01",           # adaptation version
    suffix    = "PROD",
    full      = TRUE            # must be one of PROD, TEST, or INT
) {
  # 1. Format date as YYYYMMDD
  date_str <- format(as.Date(date), "%Y%m%d")

  # 2. Validate suffix
  if (!suffix %in% c("PROD", "TEST", "INT")) {
    stop("Suffix must be one of 'PROD', 'TEST', or 'INT'. Got: ", suffix)
  }

  # 3. Build the label
  release_label <- paste(date_str, suffix, sep = "_")
  release_label_full <- paste(date_str, ppp_round, rv, av, suffix, sep = "_")

  if (isTRUE(full)){
    return(release_label_full)
  } else {
    return(release_label)
  }

}

set_current_release <- function(release_label) {

  options(pip_current_release = release_label)
  message("Current release set to: ", release_label)
}

get_current_release <- function() {
  rel <- getOption("pip_current_release", default = NA_character_)
  return(rel)
}
