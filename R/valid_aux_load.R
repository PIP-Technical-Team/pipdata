valid_aux_load <- function(measure = c("cpi", "ppp","pfw","pop"),
                           compare = "all") {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(compare %in% c("all","release","vintage")){

    cli::cli_abort("The options for {.var compare} should be either: all, release or vintage")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(compare %in% c("all", "release")){

    # Load changes

    suppressMessages(changes_release <- pipaux::compare_aux_releases(measure = measure, verbose = FALSE))

    # Identify unique countries and years

    unique_release <- collapse::rapply2d(changes_release, check_unique)

    # Create data.frame with inventory of changes

    final_release <- collapse::unlist2d(unique_release, idcols = c("measure", "changes"))

    if(compare %in% c("release")){

      return(final_release)

    }

  }

  if(compare %in% c("all", "vintage")){

    # Load changes

    suppressMessages(changes_vintage <- pipaux::compare_aux_vintages(measure = measure, verbose = FALSE))

    # Identify unique countries and years

    unique_vintage <- collapse::rapply2d(changes_vintage, check_unique)

    # Create data.frame with inventory of changes

    final_vintage <- collapse::unlist2d(unique_vintage, idcols = c("measure", "changes"))

    if(compare %in% c("release")){

      return(final_vintage)

    }

  }

  if(compare %in% c("all")){

    final <- rbind(final_release, final_vintage)

    return(final)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(TRUE))

}


check_unique <- function(x, var = c("country_code", "survey_acronym", "year")){

  if(all(var %in% colnames(x))){

    unique_values <- unique(x[, ..var])

    return(unique_values)
  }

  return(NULL)
}
