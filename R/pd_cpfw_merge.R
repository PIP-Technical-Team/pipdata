#' Merge country/survey PFW info with dataliweb survey data
#'
#' @param lf list (more than one if there are two or more
#' welfare types in the survey)
#' @param cpfw list (more than one if there are two or more
#' welfare types in the survey)
#'
#' @return list
#' @export
#'
#' @examples
#' pfw  <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' lf   <- pd_split_alt_welfare(md, cpfw)
#' l    <- pd_cpfw_merge(lf, cpfw)
pd_cpfw_merge <- function(lf, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot( exprs = {

    ## check that both are lists
  }
  )

  # Early returns ------
  if (FALSE) {
    return()
  }

  if (inherits(lf, "list")) {
    df <- purrr::map2(.x = lf,
                     .y =  cpfw,
                     .f = cpfw_merge)
  } else {
    df <- cpfw_merge(lf, cpfw[[1]])
    df <- list(df)
  }

  #names(y) <- sapply(cpfw, `[[`, "cache_id")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(df)

}


#' Merge country/survey PFW with dataliweb survey data (lower level, S3 methods)
#'
#' @param df data.table loaded with `pipload::pip_load_dlw()`
#' @param cpfw data.table with country/survey PFW
#' @param ...  other parameters
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#'
#' gd  <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' cpfw_merge(gd, cpfw[[1]])
#'
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' cpfw_merge(md, cpfw[[1]])
cpfw_merge <- function(df, cpfw,...) {
  UseMethod("cpfw_merge")
}

#' Merge country/survey PFW with dataliweb survey data
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' dlw_clean(md[[1]], cpfw[[1]])
cpfw_merge.pipmd <- function(df, cpfw, ...){

  #   ____________________________________________________________________________
  #   Initial formatting                                                      ####

  # hard copy
  md <- copy(df)

  variables <- colnames(md)


  # Add variables  from PFW data (same as group)
  if (!c("survey_year") %in% variables) {
    md[, survey_year := cpfw$survey_year]
  }

  # generate countrycode variable if not available in md data
  if (!c("countrycode") %in% variables){
    md[, countrycode := cpfw$country_code]
  }

  # Create welfare_type
  if (!c("welfare_type") %in% variables){
    md[, welfare_type := cpfw$welfare_type]
  }

  # Create distribution_type
  if (cpfw$use_imputed == 1) {

    md[, distribution_type := "imputed"]

  }else {

    md[, distribution_type := "micro"]

  }

  # rename subnatid
  if (c("subnatid") %in% variables){ #Only md
    setnames(md, "subnatid", "subnatid1")
  }

  # Recode urban to string
  if (c("urban") %in% variables){

    setnames(md, "urban", "urban2")
    md[, urban := NA_character_]
    md[urban2 == 1, urban := "urban"]
    md[urban2 == 0, urban := "rural"]
    md[, urban2 := NULL]
  }


  ##  ............................................................................
  ##  Level and domain variables                                              ####

  # Create ppp_data_level
  if (c("ppp_data_level") %in% variables) {
    md[, ppp_data_level := NULL]
  }

  if (cpfw$ppp_domain == 1){
    md[, ppp_data_level := "national"]
  }

  if (cpfw$ppp_domain == 2) {

    md[, ppp_data_level := urban]

  }

  # Create cpi_data_level
  if (c("cpi_data_level") %in% variables) {
    md[, cpi_data_level := NULL]
  }
  if (cpfw$cpi_domain == 1) {

    md[, cpi_data_level := "national"]

  }
  if (cpfw$cpi_domain == 2) {

    md[, cpi_data_level := urban]
  }

  # Create gdp_data_level
  if (cpfw$gdp_domain == 1) {
    md[, gdp_data_level := "national"]
  }
  if (cpfw$gdp_domain == 2) {

    md[, gdp_data_level := urban]

  }

  # Create pce_data_level
  if (c("pce_domain") %in% variables) {
    md[, pce_data_level := NULL]
  }
  if (cpfw$pce_domain == 1) {

    md[, pce_data_level := "national"]

  }
  if (cpfw$pce_domain == 2) {

    md[, pce_data_level := urban]

  }

  # Create pop_data_level
  if (c("pop_domain") %in% variables) {

    md[, pop_data_level := NULL]

  }
  if (cpfw$pop_domain == 1) {
    md[, pop_data_level := "national"]
  }
  if (cpfw$pop_domain == 2) {

    md[, pop_data_level := urban]

  }



}

