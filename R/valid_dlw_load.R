valid_dlw_load <- function(inv,
                           folder = "DLW-OUTPUT/",
                           path = fs::path(Sys.getenv("PIP_ROOT_DIR"), folder)) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Order alphabetically

  inv <- inv |>
    collapse::fmutate(file_qs = fs::path_file(pip_file_path))

  setorder(inv, file_qs)

  # Check what aux has changed
  # inv_aux <- valid_aux_load() # It gives a list of the surveys to be updated

  # Load survey files
  n      <- length(inv$file_qs)
  ls_svy <- lapply(1:n, \(x) qs::qread(fs::path(path, inv$file_qs[x])))

  # Some data from inventory to data frame

  poss_data_to_df <- purrr::possibly(.f = data_to_dt,
                                     otherwise = NULL)

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
