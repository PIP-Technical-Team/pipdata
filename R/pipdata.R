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
#' @importFrom glue glue

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
      "..mul_vars",
      "alt_welfare",
      "countrycode",
      "cpi_data_level",
      "educat4",
      "educat4_2",
      "educat5",
      "educat5_2",
      "gdp_data_level",
      "inpovcal",
      "literacy",
      "literacy2",
      "male",
      "male2",
      "pce_data_level",
      "pop_data_level",
      "urban2",
      "use_bin",
      "use_imputed",
      "use_microdata",
      "weight",
      "welfare",
      "welfare_type",
      "cache_id",
      "cpfw",
      "is_alt_welf",
      "oth_welfare1_type",
      "reporting_level",
      "wt",
      "..cpi_to_keep",
      "..welf_vars",
      "adaptation_version",
      "diff_year",
      "pop_fact",
      "ppp_version",
      "ppp_year",
      "release_version",
      "weighted.mean",
      "welfare_lcu",
      "wght"
    ),
    package = utils::packageName()
  )
}

NULL
