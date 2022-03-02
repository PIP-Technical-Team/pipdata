#' A package for prepare data to be ingested in PIP pipeline
#'
#'
#' @section pipdata functions:
#' The pipaux functions ...
#'
#' @docType package
#' @name pipdata
#' @importFrom pipload pip_create_globals add_gls_to_env
#' @import data.table
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom glue glue
#' @export
magrittr::`%>%`

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      ".",
      "!!",
      ":=",
      "creationtime",
      "fullname",
      "lastwritetime",
      "module",
      "survey_id",
      "surveyid_year",
      "area",
      "country_code",
      "distribution_type",
      "na.omit",
      "ppp_data_level",
      "survey_acronym",
      "survey_year",
      "urban",
      "veralt",
      "vermast",
      "..mul_vars"
    ),
    package = utils::packageName()
  )
}


NULL
