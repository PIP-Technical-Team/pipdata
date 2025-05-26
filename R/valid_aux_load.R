valid_aux_load <- function(measure = c("cpi", "ppp","gdp","pfw","pop"),
                           maindir = fs::path(Sys.getenv("PIP_ROOT_DIR"),"PIP_ingestion_pipeline_V2")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # measure = c("cpi", "ppp","gdp","pfw","pop")
  # maindir = fs::path(Sys.getenv("PIP_ROOT_DIR"),"PIP_ingestion_pipeline_V2")

  changes <- pipaux::inventory_aux_changes(measures = measure,
                                           maindir = maindir)

  inv_aux   <- qs::qread(file.path(maindir, "aux_data/20250203_TEST/aux_inv_list.qs"))


  poss_aux_to_df <- purrr::possibly(.f = aux_to_dt,
                                     otherwise = NULL)

  vec <- names(inv_aux)

  ls <- purrr::imap(.x = inv_aux,
                    .f = poss_aux_to_df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}

aux_to_dt <- function(aux,
                      name) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ### Load aux data ---------
  # df <- pipload::pip_load_aux(name)

  ### If not changes ---------
  if(is.null(aux)){
    return(NULL)
  }

  ### Identify survey_id ---------
  svy_update <- aux |>
    # collapse::fsubset(variable == "cpi" )|>
    collapse::fselect(country_code, year, survey_acronym)|>
    collapse::funique()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(svy_update)

}
