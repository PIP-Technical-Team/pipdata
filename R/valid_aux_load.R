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

    # unique_vintage <- collapse::rapply2d(changes_vintage, function(x) purrr::map2(x, measure, check_unique))
    #
    # # Create data.frame with inventory of changes
    #
    # final_vintage <- collapse::unlist2d(unique_vintage, idcols = c("measure", "changes"))
    #
    # if(compare %in% c("release")){
    #
    #   return(final_vintage)
    #
    # }

  }

  if(compare %in% c("all")){

    # Combine if all changes are considered

    unique_all <- as.list(unique_release, unique_vintage)

    names(unique_all) <- c("release", "vintage")

    return(unique_all)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(TRUE))

}

cln_changes <- function(changes) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
