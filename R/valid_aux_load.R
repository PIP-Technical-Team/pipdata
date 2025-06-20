valid_aux_load <- function(measure = c("cpi", "ppp","pfw","pop"),
                           load = "inventory") {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(load == "inventory"){

    # Load changes

    # changes <- pipaux::compare_aux_releases(measure = measure)
    changes <- m_compare_aux_release()

    # Identify unique countries and years

    unique <- collapse::rapply2d(changes, check_unique)

    # Create data.frame with inventory of changes

    final <- collapse::unlist2d(unique, idcols = c("measure", "changes"))

    return(final)

  }else if(load == "data.frame"){

    poss_aux_to_df <- purrr::possibly(.f = pipload::pip_load_aux,
                                      otherwise = NULL)

    ls <- purrr::map(.x = measure,
                      .f = poss_aux_to_df)

    return(ls)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(TRUE))

}


check_unique <- function(x){

  if(all(c("country_code", "year") %in% colnames(x))){

    unique_values <- unique(x[, .(country_code, year)])

    return(unique_values)
  }

  return(NULL)
}
