pd_aux_attr <- function(clean_data,
                        aux_measures = c("cpi","ppp")) {

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

    cpi  <- pipload::pip_load_aux("cpi")
    keys <- attributes(cpi)$aux_key

    aux_attr <- lapply(aux_attr, add_cpi_attr,
                       cpi = cpi, keys = keys)

  }

  ## PPP

  if("ppp" %in% aux_measures){

    ppp  <- pipload::pip_load_aux("ppp")
    keys <- attributes(ppp)$aux_key

    aux_attr <- lapply(aux_attr, add_ppp_attr,
                       ppp = ppp, keys = keys)
  }

  # Set base years


  ## POP
#
#   pop  <- pipload::pip_load_aux("pop")
#
#   ## GDP
#
#   gdp  <- pipload::pip_load_aux("gdp")
#
#   ## PCE
#
#   pce  <- pipload::pip_load_aux("pce")

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
  cpi_attr <- split(filtered_cpi$cpi_value,
                    filtered_cpi$cpi_year)

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
  ppp_attr <- split(filtered_ppp$ppp,
                    filtered_ppp$ppp_version)

  # Add to other attributes
  ls <- append(ls, list("ppp" = ppp_attr))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}
