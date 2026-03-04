pd_aux_attr <- function(
  clean_data,
  aux_list
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Filter cleaning aux data
  aux_list <- aux_list[
    names(aux_list) %in% c("cpi", "ppp", "pop", "gdp", "pce")
  ]

  # Add attributes already in surveys

  aux_attr <- collapse::rapply2d(clean_data, \(x) {
    ls <- attributes(x)
    ls[!names(ls) %in% c(".internal.selfref", "row.names", "notes")]
  })

  # Add aux data as attributes

  ## CPI, PPP, POP, GDP, PCE

  for (measure in names(aux_list)) {
    aux_data <- aux_list[[measure]]
    aux_keys <- stamp::st_get_pk(aux_data)
    aux_attr <- lapply(
      aux_attr,
      add_attr,
      measure = measure,
      aux_data = aux_data,
      keys = aux_keys
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(aux_attr)
}

add_attr <- function(ls, measure, aux_data, keys) {
  # Find the keys in survey
  if ("year" %in% keys) {
    keys <- c(keys, "surveyid_year")
  }

  # Build survey id from matching keys
  id <- ls[names(ls) %in% keys]

  # Filter survey aux data
  filtered_aux <- filter_aux_data(
    measure = measure,
    aux_data = aux_data,
    id = id
  )

  # No aux data to add, return original attributes
  if (nrow(filtered_aux) == 0) {
    return(ls)
  }

  # Create attributes
  aux_attr <- create_attr(measure = measure, filtered_aux = filtered_aux)

  # Add to other attributes under the measure name
  ls <- append(ls, setNames(list(aux_attr), measure))

  # Return   ---------
  return(ls)
}

filter_aux_data <- function(measure, aux_data, id) {
  if (measure == "cpi") {
    # Filter for CPI data
    aux_data <- aux_data |>
      collapse::fsubset(
        country_code == id$country_code &
          year == id$surveyid_year &
          survey_acronym == id$survey_acronym
      )
  } else if (measure == "ppp") {
    # Add ppp_version column (copy to avoid mutating by reference)
    aux_data <- data.table::copy(aux_data)
    aux_data[,
      ppp_version := {
        x <- paste0(
          "ppp_",
          ppp_year,
          "_",
          release_version,
          "_",
          adaptation_version
        )
        gsub("_v", "_0", x)
      }
    ]
    # Filter for PPP data
    aux_data <- aux_data |>
      collapse::fsubset(
        country_code == id$country_code
      )
  } else if (measure %in% c("pop", "gdp", "pce")) {
    # Filter for population, GDP, and PCE data
    aux_data <- aux_data |>
      collapse::fsubset(
        country_code == id$country_code &
          year == id$surveyid_year
      )
  } else {
    cli::cli_abort(
      "Measure {measure} not recognized for filtering auxiliary data"
    )
  }

  return(aux_data)
}

create_attr <- function(measure, filtered_aux) {
  if (measure == "cpi") {
    aux_attr <- filtered_aux$cpi_value
    names(aux_attr) <- paste0(
      filtered_aux$cpi_year,
      "_",
      filtered_aux$reporting_level
    )
  } else if (measure == "ppp") {
    aux_attr <- filtered_aux$ppp
    names(aux_attr) <- paste0(
      filtered_aux$ppp_version,
      "_",
      filtered_aux$reporting_level
    )
  } else if (measure %in% c("pop", "gdp", "pce")) {
    aux_attr <- filtered_aux[[measure]]
    names(aux_attr) <- paste0(
      filtered_aux$year,
      "_",
      filtered_aux$reporting_level
    )
  } else {
    cli::cli_abort(
      "Auxiliary measure {measure} not recognized for creating attributes"
    )
  }

  return(aux_attr)
}
