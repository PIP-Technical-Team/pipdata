#' Build auxiliary metadata attributes for cleaned survey data
#'
#' Takes the list of cleaned survey data.tables and a named list of
#' auxiliary datasets (CPI, PPP, population, GDP, PCE). For each
#' cleaned data.table, it extracts existing attributes and appends the
#' matching auxiliary values (filtered by country, year, and survey).
#' The result is a list of attribute lists suitable for saving as
#' survey metadata.
#'
#' @param clean_data A named list of cleaned `data.table` objects, as
#'   returned by [pd_dlw_clean()].
#' @param aux_list A named list of auxiliary data.tables. Expected names
#'   include `"cpi"`, `"ppp"`, `"pop"`, `"gdp"`, and `"pce"`.
#'   Typically built via `lapply(measures, pipload::load_aux_data)`.
#'
#' @return A named list of attribute lists, one per element of
#'   `clean_data`, enriched with auxiliary metadata.
#'
#' @family pd_process_data pipeline
#' @export
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

#' Add a single auxiliary measure as an attribute to a survey attribute list
#'
#' Matches the survey's identifying keys (country, year, acronym) against
#' the auxiliary dataset, filters the relevant rows, and appends the
#' result as a named element of the attribute list.
#'
#' @param ls A named list of existing survey attributes.
#' @param measure Character scalar. The auxiliary measure name
#'   (e.g., `"cpi"`, `"ppp"`, `"pop"`, `"gdp"`, `"pce"`).
#' @param aux_data A `data.table` of auxiliary data for the given measure.
#' @param keys Character vector of primary-key column names for `aux_data`.
#'
#' @return The input `ls` with a new element named `measure` appended,
#'   or unmodified if no matching auxiliary rows were found.
#'
#' @family pd_process_data pipeline
#' @keywords internal
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
  ls <- append(ls, stats::setNames(list(aux_attr), measure))

  # Return   ---------
  return(ls)
}

#' Filter auxiliary data by measure type and survey identifiers
#'
#' Applies measure-specific filtering logic: CPI is filtered by
#' country, year, and survey acronym; PPP by country only (with a
#' computed `ppp_version` column); population, GDP, and PCE by
#' country and year.
#'
#' @param measure Character scalar. One of `"cpi"`, `"ppp"`, `"pop"`,
#'   `"gdp"`, `"pce"`.
#' @param aux_data A `data.table` of the auxiliary dataset.
#' @param id A named list of survey identifiers (e.g., `country_code`,
#'   `surveyid_year`, `survey_acronym`).
#'
#' @return A filtered `data.table` with only the rows matching the
#'   survey identifiers.
#'
#' @family pd_process_data pipeline
#' @keywords internal
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

#' Create a named attribute vector from filtered auxiliary data
#'
#' Extracts the value column for a given measure and names each element
#' with a combination of version/year and reporting level.
#'
#' @param measure Character scalar. One of `"cpi"`, `"ppp"`, `"pop"`,
#'   `"gdp"`, `"pce"`.
#' @param filtered_aux A `data.table` of already-filtered auxiliary data,
#'   as returned by [filter_aux_data()].
#'
#' @return A named numeric vector of auxiliary values.
#'
#' @family pd_process_data pipeline
#' @keywords internal
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
