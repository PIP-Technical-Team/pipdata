pd_aux_attr <- function(clean_data,
                        aux_measures) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Add attributes already in surveys

  aux_attr <- collapse::rapply2d(clean_data,\(x){

      ls <- attributes(x)
      ls[!names(ls) %in% c("row.names", "notes")]

    })

  # Avoid survey name
  aux_attr <- purrr::flatten(aux_attr)

  # Add aux data as attributes

  ## Check if attributes changed

  ## change_aux() -> valid_aux_load(aux_measures = c("cpi", "ppp", "gdp", "pop", "pce"))

  ## CPI

  cpi  <- pipload::pip_load_aux("cpi")
  keys <- attributes(cpi)$aux_key

  if("year" %in% keys){
    keys <- c(keys, "surveyid_year")
  }

  aux_attr <- lapply(aux_attr, add_cpi_attr,
                     cpi = cpi, keys = keys)


  ## PPP
  ppp  <- pipload::pip_load_aux("ppp")

  ## POP

  pop  <- pipload::pip_load_aux("pop")


  ## GDP

  gdp  <- pipload::pip_load_aux("gdp")

  ## PCE

  pce  <- pipload::pip_load_aux("pce")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(TRUE)

}

add_cpi_attr <- function(ls,
                         cpi,
                         keys) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find the keys in survey
  id <- ls[names(ls) %in% keys]

  # Fix reporting  (Temporal)
  if("reporting_level" %in% names(ls)){
    id$reporting_level <- ls$cpi_data_level
  }

  # Filter survey cpi
  filtered_cpi <- cpi|>
    collapse::fsubset(country_code == id$country_code &
                        year == id$surveyid_year &
                        survey_acronym == id$survey_acronym &
                        reporting_level == id$reporting_level)

  # Create attributes
  cpi_attr <- split(filtered_cpi$cpi_value,
                    filtered_cpi$cpi_year)

  # Add to other attributes
  ls <- append(ls, list("cpi" = cpi_attr))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}
