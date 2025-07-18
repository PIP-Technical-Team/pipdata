
#' Retrieve the inventory of aux files that changed from previous release or vintage
#'
#' @param measure measure of auxiliary files to compare
#' @param compare either `release`, `vintage` or `all`
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' valid_aux_load()
#' }
valid_aux_load <- function(measure = c("cpi", "ppp","pfw","pop"),
                           compare = "all") {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(!(compare %in% c("all","release","vintage"))){

    cli::cli_abort("The options for {.var compare} should be either: all, release or vintage")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(compare %in% c("all", "release")){

    # Load changes

    suppressMessages(changes_release <- pipaux::compare_aux_releases(measure = measure, verbose = FALSE))

    # Clean changes

    changes_release <- cln_changes(changes_release)

    # Identify unique

    unique_release <- lapply(changes_release, check_unique)

    if(compare %in% c("release")){

      return(unique_release)

    }

  }

  if(compare %in% c("all", "vintage")){

    # Load changes

    suppressMessages(changes_vintage <- pipaux::compare_aux_vintages(measure = measure, verbose = FALSE))

    # Clean changes

    changes_vintage <- cln_changes(changes_vintage)

    # Identify unique

    unique_vintage <- lapply(changes_vintage, check_unique)

    if(compare %in% c("release")){

      return(unique_vintage)

    }

  }

  if(compare %in% c("all")){

    # Combine if all changes are considered

    unique_all <- list(unique_release, unique_vintage)

    names(unique_all) <- c("release", "vintage")

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
#' changes_vintage <- pipaux::compare_aux_vintages(measure = measure, verbose = FALSE)
#' cln_chngs <- cln_changes(changes_vintage)
#' }
cln_changes <- function(changes) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Remove difference in columns

  changes <- changes[!names(changes) %in% "diff_cols"]

  # Row bind lists

  changes <- suppressWarnings(lapply(changes, \(x) rbindlist(x, idcol = "changes", fill = TRUE)))

  # Eliminate Null values

  changes <- purrr::keep(changes, ~ !is.null(.x) && length(.x) > 0 && nrow(.x) > 0)

  # Add measure name/id as attribute

  changes <- purrr::map2(.x = changes,
                         .y = names(changes),
                         .f = id_as_att)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(changes)

}


#' Check unique values in aux changes
#'
#' According to the aux key, this function selects the unique values.
#'
#' @param x data.frame with aux changes for specific aux file
#' @param name name of the measure or auxiliary file
#'
#' @return data.frame
#' @keywords internal
check_unique <- function(x, name = attributes(x)$id){

  if(name == "cpi"){ # Fix while we add as attributes

    key <- c("country_code", "cpi_year", "reporting_level", "survey_year", "survey_acronym")

  }else if(name == "ppp"){

    key <- c("country_code", "reporting_level", "ppp_year")

  }else if(name == "pop"){

    key <- c("country_code", "reporting_level", "year")

  }else if(name == "pfw"){

    key <- c("country_code", "surveyid_year", "welfare_type")

  }

  if(all(key %in% colnames(x))){

    unique_values <- unique(x[, ..key])

    return(unique_values)
  }

  return(NULL)
}
