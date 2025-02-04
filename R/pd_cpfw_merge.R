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
#' \dontrun{
#' pfw  <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' lf   <- pd_split_alt_welfare(md, cpfw)
#' l    <- pd_cpfw_merge(lf, cpfw)
#' }
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add variables --------


  if (inherits(lf, "list")) {
    df <- purrr::map2(.x = lf,
                     .y =  cpfw,
                     .f = cpfw_merge)
  } else {
    df <- cpfw_merge(lf, cpfw[[1]])
    df <- list(df)
  }

  #names(y) <- sapply(cpfw, `[[`, "cache_id")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Transform into attributes --------

  # Use Zander functions

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
#' \dontrun{
#' pfw <- pipload::pip_load_aux("pfw")
#'
#' gd  <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' cpfw_merge(gd, cpfw[[1]])
#' FIX
#'
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' cpfw_merge(md, cpfw[[1]])
#' FIX
#' }
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
#' \dontrun{
#' pfw <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' FIX...
#' }
cpfw_merge.pipmd <- function(df, cpfw, ...){

  #   ____________________________________________________________________________
  #   Initial formatting                                                      ####

  # hard copy
  md <- copy(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main variables (same md and gd) --------
  md <- add_main_vars(md, cpfw)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Area (same for md and gd) --------



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Domain variables (same for md and gd) --------


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Distribution type  (different) --------







  # Create distribution_type
  if (cpfw$use_imputed == 1) {

    md[, distribution_type := "imputed"]

  }else {

    md[, distribution_type := "micro"]

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
  variables <- colnames(md)

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

#' Add metadata variables to country/survey
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#'
#' @keywords internal
add_main_vars <- function(dt, cpfw, log_wrn = TRUE) {

  tryCatch(
    expr = {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # computations   ---------
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      variables <- colnames(dt)

      main_vars <- c("survey_year",
                     "countrycode",
                     "welfare_type")

      # Inform what country/surveys are missing a main variable

      if(any(!(main_vars %in% variables))){

        svy <- unique(cpfw$link)

        miss_vars <- main_vars[!(main_vars %in% variables)]

       cli::cli_inform(message = "Main variable{?s} {miss_vars} missing in DLW",
                      class = c("mn_var_inf", "pipinf"),
                      log = log_wrn,
                      link = svy,
                      call = sys.call())
      }


      # Add variables if missing

      dt[, (main_vars) :=
           lapply(main_vars, \(x) {

             if (!(x %in% variables)) {
               cpfw[[x]]

               } else {
                 dt[[x]]

                 }
             })]

    },

    mn_var_inf = function(cnd){

      if(cnd$log){ # Log the information

        add_log(cnd)

      }
    }

  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
