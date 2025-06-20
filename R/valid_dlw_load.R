valid_dlw_load <- function(inv,
                           measure = c("cpi", "ppp","pfw","pop"),
                           path = fs::path(Sys.getenv("PIP_ROOT_DIR"), "DLW-OUTPUT/")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(!is.data.table(inv)){
    inv <- data.table::data.table(inv)
  }

  # Check what aux has changed
  changes_aux <- valid_aux_load(measure = measure, load = "inventory")

  # Temporary fix
  names(changes_aux)[names(changes_aux) == "year"] <- "surveyid_year"

  inv_aux <- filter_aux_inv(changes_aux = changes_aux[, c("country_code","surveyid_year")],
                 inv = inv)

  # Create mock changes for the inventory (Temporal)
  inv_svy <- m_inv_filter(inv) # For now is a mock function

  # Bind with inventory from aux changes
  inv_to_clean <- rbind(inv_svy, inv_aux, fill = TRUE)

  # Order alphabetically

  inv_to_clean <- inv_to_clean |>
    collapse::fmutate(file_qs = fs::path_file(pip_file_path))

  setorder(inv_to_clean, file_qs)

  # Load survey files
  n      <- length(inv_to_clean$file_qs)
  ls_svy <- lapply(1:n, \(x) qs::qread(fs::path(path, inv_to_clean$file_qs[x])))

  # Some data from inventory to data frame

  poss_data_to_df <- purrr::possibly(.f = data_to_dt,
                                     otherwise = NULL)

  ls <- purrr::map2(.x = ls_svy,
                    .y = as.list(inv_to_clean$survey_id),
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

filter_aux_inv <- function(inv,
                           changes_aux) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Temporary fix

  max_year <- max(inv[!is.na(inv$surveyid_year),c("surveyid_year")])

  changes_aux <- changes_aux[changes_aux$surveyid_year<=max_year,]

  # Merge inventory with aux changes

  inv_aux  <- merge(inv,
                    changes_aux,
                    by = c("country_code", "surveyid_year"))

  # Choose last version

  inv_aux <-
    inv_aux[,
            # Get max master version and filter
            maxmast := vermast == max(vermast),
            by = .(country_code, surveyid_year, survey_acronym, module, tool)
    ][
      maxmast == 1
    ][,
      # Get max veralt version and filter
      maxalt := veralt == max(veralt),
      by = .(country_code, surveyid_year, survey_acronym, module, tool)
    ][
      maxalt == 1
    ][,
      # Get max veralt version and filter
      maxpip := pipeline_version == max(pipeline_version),
      by = .(country_code, surveyid_year, survey_acronym, module, tool)
    ][
      maxpip == 1
    ][,
      c("maxalt",  "maxmast", "maxpip") := NULL
    ][
      status == "same"
    ][
      module %in% c("GPWG", "GROUP", "BIN", "ALL" , "HIST")
    ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_aux)

}
