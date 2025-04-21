valid_dlw_load <- function(inv, path) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Order alphabetically
  file_dta <- basename(inv$fullname)
  file_qs <- sub("\\.dta$", ".qs", file_dta)
  file_qs <- sort(file_qs)

  # Load survey files
  ls_svy     <- lapply(1:n, \(x) qs::qread(file.path(path, file_qs[x])))

  poss_data_to_df <- purrr::possibly(.f = data_to_dt,
                                     otherwise = NULL)

  # Order sampled inventory to get same survey_id

  inv <- inv |>
    dplyr::mutate(file_dta = basename(fullname))

  inv <- inv[order(file_dta),]

  # Some data from inventory to data frame

  ls <- purrr::map2(.x = ls_svy,
                    .y = as.list(inv$survey_id),
                    .f = poss_data_to_df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}

data_to_dt <- function(x, y) {

  # df <- haven::read_dta(x)
  df <- x
  df$survey_id <- y

  #--------- leaving just the 'label' attribute ---------
  nn  <- names(df)
  for (j in seq_along(nn)) {

    ats       <- attributes(df[[j]])
    atsn      <- names(ats)
    to_remove <- atsn[!grepl("label", atsn)]

    for (i in seq_along(to_remove)) {
      attr(df[[j]], to_remove[i]) <- NULL
    }

  }

  #--------- Survey ID and its components ---------
  df <- pipload::survey_id_to_vars(df)

  ### Add class ---------
  df <- pipload::as_pip(df)

  return(df)
}
