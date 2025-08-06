pd_aux_attr <- function(clean_data,
                        aux_measures) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Add attributes already in dt

  aux_attr <- lapply(clean_data,\(y){

    svy <- lapply(y,\(x){

      ls <- attributes(x)
      ls[!names(ls) %in% c("row.names", "notes")]

      })

    })

  # Add aux data as attributes

  ## Check if attributes changed

  ## change_aux() -> valid_aux_load(aux_measures = c("cpi", "ppp", "gdp", "pop", "pce"))

  ## CPI

  cpi  <- pipload::pip_load_aux("cpi")
  keys <- attributes(cpi)$aux_key

  if("year" %in% keys){
    keys <- c(keys, "surveyid_year")
  }

  dt <- aux_attr[[1]][[1]]

  id <- dt[names(dt) %in% keys]

  # Fix reporting  (Temporal)
  if("reporting_level" %in% names(dt)){
    id$reporting_level <- dt$cpi_data_level
    # id <- c(id, "cpi_data_level" = dt$cpi_data_level)
  }

  # Filter cpi
  filtered_cpi <- cpi|>
    collapse::fsubset(country_code == id$country_code &
                      year == id$surveyid_year &
                      survey_acronym == id$survey_acronym &
                      reporting_level == id$reporting_level)

  # Create attributes
  ### Variables and year ---------

  cpi_vars <- grep("^cpi_[0-9]{4}$", names(filtered_cpi), value = TRUE)

  cpi_years <- gsub("cpi([0-9]+)", "\\1", cpi_vars)|> unique() |> sort()

  setattr(dt, "cpi_years", cpi_years)

  unique_values <- lapply(filtered_cpi[, .SD, .SDcols = c("cpi_year", "cpi_value")], unique)

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
