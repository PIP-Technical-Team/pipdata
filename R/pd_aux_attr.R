pd_aux_attr <- function(clean_data,
                        aux_measures = c("cpi","ppp","pop","gdp","pce")) {

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

  if("cpi" %in% aux_measures){

    cpi  <- pipload::pip_load_aux("cpi", verbose = FALSE)
    keys <- attributes(cpi)$aux_key

    aux_attr <- lapply(aux_attr, add_cpi_attr,
                       cpi = cpi, keys = keys)

  }

  ## PPP

  if("ppp" %in% aux_measures){

    ppp  <- pipload::pip_load_aux("ppp", verbose = FALSE)
    keys <- attributes(ppp)$aux_key

    aux_attr <- lapply(aux_attr, add_ppp_attr,
                       ppp = ppp, keys = keys)
  }

  # Set base years in Deflation function


  ## POP


  if("pop" %in% aux_measures){

    pop  <- pipload::pip_load_aux("pop")
    keys <- attributes(pop)$aux_key

    aux_attr <- lapply(aux_attr, add_pop_attr,
                       pop = pop, keys = keys)
  }

  ## GDP

  if("gdp" %in% aux_measures){

    gdp  <- pipload::pip_load_aux("gdp")
    keys <- attributes(gdp)$aux_key

    aux_attr <- lapply(aux_attr, add_gdp_attr,
                       gdp = gdp, keys = keys)
  }


  ## PCE


  if("pce" %in% aux_measures){

    pce  <- pipload::pip_load_aux("pce")
    keys <- attributes(pce)$aux_key

    aux_attr <- lapply(aux_attr, add_pce_attr,
                       pce = pce, keys = keys)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(aux_attr)

}

add_cpi_attr <- function(ls,
                         cpi,
                         keys) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Find the keys in survey
  if("year" %in% keys){
    keys <- c(keys, "surveyid_year")
  }

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
  cpi_attr <- filtered_cpi$cpi_value
  names(cpi_attr) <- filtered_cpi$cpi_year

  # Add to other attributes
  ls <- append(ls, list("cpi" = cpi_attr))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}

add_ppp_attr <- function(ls,
                         ppp,
                         keys) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find the keys in survey
  id <- ls[names(ls) %in% keys]

  # Fix reporting  (Temporal)
  if("reporting_level" %in% names(ls)){
    id$reporting_level <- ls$ppp_data_level
  }

  # Add ppp_version
  ppp[,
      ppp_version := {
        x <- paste0("ppp_", ppp_year, "_", release_version, "_", adaptation_version)
        x <- gsub("_v", "_0", x )
      }
  ]

  # Filter survey ppp
  filtered_ppp <- ppp|>
    collapse::fsubset(country_code == id$country_code &
                      reporting_level == id$reporting_level)

  # Create attributes
  ppp_attr <- filtered_ppp$ppp
  names(ppp_attr) <- filtered_ppp$ppp_version

  # Add to other attributes
  ls <- append(ls, list("ppp" = ppp_attr))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}

add_pop_attr <- function(ls,
                         pop,
                         keys) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find the keys in survey
  if("year" %in% keys){
    keys <- c(keys, "surveyid_year")
  }

  # Find the keys in survey
  id <- ls[names(ls) %in% keys]

  # Fix reporting  (Temporal)
  if("reporting_level" %in% names(ls)){
    id$reporting_level <- ls$pop_data_level
  }


  # Filter survey pop
  filtered_pop <- pop|>
    collapse::fsubset(country_code == id$country_code &
                        year == id$surveyid_year &
                        reporting_level == id$reporting_level)

  # Create attributes
  pop_attr <- filtered_pop$pop
  names(pop_attr) <- filtered_pop$reporting_level

  # Add to other attributes
  ls <- append(ls, list("pop" = pop_attr))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}

add_gdp_attr <- function(ls,
                         gdp,
                         keys) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find the keys in survey
  if("year" %in% keys){
    keys <- c(keys, "surveyid_year")
  }

  # Find the keys in survey
  id <- ls[names(ls) %in% keys]

  # Fix reporting  (Temporal)
  if("reporting_level" %in% names(ls)){
    id$reporting_level <- ls$gdp_data_level
  }

  # Filter survey gdp
  filtered_gdp <- gdp|>
    collapse::fsubset(country_code == id$country_code &
                        year == id$surveyid_year &
                        reporting_level == id$reporting_level)

  # Create attributes
  gdp_attr <- filtered_gdp$gdp
  names(gdp_attr) <- filtered_gdp$reporting_level

  # Add to other attributes
  ls <- append(ls, list("gdp" = gdp_attr))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}

add_pce_attr <- function(ls,
                         pce,
                         keys) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find the keys in survey
  if("year" %in% keys){
    keys <- c(keys, "surveyid_year")
  }

  # Find the keys in survey
  id <- ls[names(ls) %in% keys]

  # Fix reporting  (Temporal)
  if("reporting_level" %in% names(ls)){
    id$reporting_level <- ls$gdp_data_level
  }

  # Filter survey pce
  filtered_pce <- pce|>
    collapse::fsubset(country_code == id$country_code &
                        year == id$surveyid_year &
                        reporting_level == id$reporting_level)

  # Create attributes
  pce_attr <- filtered_pce$pce
  names(pce_attr) <- filtered_pce$reporting_level

  # Add to other attributes
  ls <- append(ls, list("pce" = pce_attr))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}


