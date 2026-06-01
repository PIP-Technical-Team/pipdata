#' Retrieve the inventory of aux files that changed from previous release or vintage
#'
#' @param measure measure of auxiliary files to compare
#' @param compare either `release`, `vintage` or `all`
#'
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' valid_aux_load()
#' }
valid_aux_load <- function(
  measure = c("cpi", "ppp", "pfw", "pop"),
  compare = "all",
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!(compare %in% c("all", "release", "vintage"))) {
    cli::cli_abort(
      "The options for {.var compare} should be either: all, release or vintage"
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (compare %in% c("all", "release")) {
    # Load changes

    changes_release <- pipaux::compare_aux_releases(
      measure = measure,
      owner = "RossanaTat",
      verbose = verbose
    )

    # Clean changes

    changes_release <- lapply(changes_release, cln_changes)

    # Eliminate Null values

    changes_release <- purrr::keep(
      changes_release,
      ~ !is.null(.x) && length(.x) > 0 && nrow(.x) > 0
    )

    # Identify unique

    unique_release <- lapply(changes_release, check_unique)

    if (compare %in% c("release")) {
      if (length(unique_release) == 0) {
        return(NULL)
      }

      return(unique_release)
    }
  }

  if (compare %in% c("all", "vintage")) {
    # Load changes

    changes_vintage <- pipaux::compare_aux_vintages(
      measure = measure,
      # owner = "RossanaTat",
      verbose = verbose
    )

    # Clean changes

    changes_vintage <- lapply(changes_vintage, cln_changes)

    # Eliminate Null values

    changes_vintage <- purrr::keep(
      changes_vintage,
      ~ !is.null(.x) && length(.x) > 0 && nrow(.x) > 0
    )

    # Identify unique

    unique_vintage <- lapply(changes_vintage, check_unique)

    if (compare %in% c("vintage")) {
      if (length(unique_vintage) == 0) {
        return(NULL)
      }
      return(unique_vintage)
    }
  }

  if (compare %in% c("all")) {
    # Combine if all changes are considered

    unique_all <- list(
      release = if (length(unique_release) == 0) NULL else unique_release,
      vintage = if (length(unique_vintage) == 0) NULL else unique_vintage
    )

    if (all(sapply(unique_all, is.null))) {
      return(NULL)
    }

    return(unique_all)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(TRUE))
}

#' Clean output from compare_aux_releases and compare_aux_vintages
#'
#' @param changes output from `pipaux::compare_aux_releases` or `pipaux::compare_aux_vintages`
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' changes_vintage <- pipaux::compare_aux_vintages(measure = "pfw", verbose = FALSE)
#' cln_chngs <- cln_changes(changes_vintage)
#' }
cln_changes <- function(changes) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Remove list for difference in columns

  cln_chngs <- changes[!(names(changes) %in% c("diff_cols"))]

  # Row bind lists

  cln_chngs <- data.table::rbindlist(cln_chngs, idcol = "changes", fill = TRUE)

  # Add key as attribute

  key <- attributes(changes)$key_cols
  data.table::setattr(cln_chngs, "key_cols", key)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(cln_chngs)
}


#' Check unique values in aux changes
#'
#' According to the aux key, this function selects the unique values.
#'
#' @param x data.frame with aux changes for specific aux file
#' @param key variables that define difference
#'
#' @return data.frame
#' @keywords internal
check_unique <- function(x, key = attributes(x)$key_cols) {
  if (all(key %in% colnames(x))) {
    unique_values <- unique(x[, ..key])

    return(unique_values)
  }

  return(NULL)
}
