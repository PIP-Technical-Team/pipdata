#' Merge country/survey PFW info with dataliweb survey data
#'
#' @param dt DLW country/survey data
#' @param pfw PFW
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' pfw  <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' md  <- survey_id_to_attr(md, unique(md$survey_id))
#' l    <- pd_cpfw_merge(md, pfw)
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' gd  <- survey_id_to_attr(gd, unique(gd$survey_id))
#' l    <- pd_cpfw_merge(gd, pfw)
#' }
pd_cpfw_merge <- function(dt, pfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## PFW for specific country --------

  cpfw <- get_country_pfw(dt, pfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Split data.table for alternative welfare --------

  lf   <- pd_split_alt_welfare(dt, cpfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Map survey to cpfw --------

  lfs <- purrr::map2(.x = lf,
                     .y =  cpfw,
                     .f = cpfw_merge)

  # lfs <- Filter(function(df) !all(is.na(df)), lfs)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(lfs)

}

#' Merge country/survey PFW with dataliweb survey data
#'
#' @param dt data.table
#' @param cpfw data.table with country/survey PFW
#' @param ...  other parameters
#'
#' @return data.table
#' @export
cpfw_merge <- function(dt, cpfw, ...){

      # Create hard copy
      dt_c <- copy(dt)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Main variables (same for md and gd) --------
      dt_c <- add_main_att(dt_c, cpfw)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Domain variables (same for md and gd) --------
      dt_c <- add_dom_vars(dt_c, cpfw)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Distribution type  (different for md and gd) --------
      dt_f <- add_dist_type(dt_c, cpfw)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Transform unique variables into attributes --------

      # dt_f <- col_to_attr(dt_c, tst)


  return(dt_f)

}

#' Add metadata as attributes to data.table
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#'
#' @keywords internal
add_main_att <- function(dt, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  att <- names(attributes(dt))

  main_attr <- c("country_code",
                 "surveyid_year", # Check if survey_year is necessary from pfw
                 "welfare_type")

  att_missing <- main_attr[!(main_attr %in% att)]

  # Inform what country/surveys are missing a main variable
  # (it might not be necessary)

  if(length(att_missing)>0){

    survey_id <- c(pd_env_get("process_survey_id"))

    vars <- cli::cli_vec(att_missing, list("vec-trunc" = 3))

    msg <- cli::format_error("Main variable{?s} {vars} missing")

    pipfun::log_add(event = "info",
                    message = msg,
                    name = "pipdata_log",
                    logmeta = list(info = "mn_var_inf",
                                   survey = survey_id))

  }

  # Add variables if missing

  for (x in att_missing) {

    # attr(dt, x) <- cpfw[[x]][[1]]
    data.table::setattr(dt, x, cpfw[[x]][[1]])

  }

  # Add reporting level from cpf

  setattr(dt, "reporting_level", cpfw$reporting_level)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Add metadata variables to country/survey
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#'
#' @keywords internal
# add_main_vars <- function(dt, cpfw) {
#
#       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       # computations   ---------
#       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       variables <- colnames(dt)
#
#       main_vars <- c("survey_year",
#                      "country_code",
#                      "welfare_type")
#
#       vars <- main_vars[!(main_vars %in% variables)]
#
#       # Inform what country/surveys are missing a main variable
#
#       if(length(vars)>0){
#
#         survey_id <- c(.pipdataenv$survey_id)
#
#         vars <- cli::cli_vec(vars, list("vec-trunc" = 3))
#
#         msg <- cli::format_error("Main variable{?s} {vars} missing")
#
#         pipfun::log_add(event = "info",
#                         message = msg,
#                         name = "pipdata_log",
#                         logmeta = list(info = "mn_var_inf",
#                                     survey = survey_id))
#
#       }
#
#       # Add variables if missing
#
#       dt[, (main_vars) :=
#            lapply(main_vars, \(x) {
#
#              if (!(x %in% variables)) {
#                cpfw[[x]]
#
#              } else {
#                dt[[x]]
#
#              }
#            })]
#
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # Return   ---------
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   return(dt)
#
# }


#' Add Domain variables
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
add_dom_vars <- function(dt, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Level and domain variables    ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  pref             <- c("ppp", "cpi", "gdp", "pce", "pop")
  domain_vars      <- glue("{pref}_domain")

  ## Check domain vars exist

  if(any(!(domain_vars %in% names(cpfw)))){

    miss_vars <- domain_vars[!(domain_vars %in% names(cpfw))]
    miss_vars <- cli::cli_vec(miss_vars, list("vec-trunc" = 3))
    msg <- cli::format_error("Domain variable{?s} {miss_vars} missing in country PFW")

    rlang::abort(message = msg,
                 class = "piperr", "dom_var")
  }

  ## Create data_level attributes

  data_level_vars <- glue("{pref}_data_level")

  same_rep_lvl <- cpfw[, .SD, .SDcols = domain_vars] == cpfw$reporting_level

  if(all(same_rep_lvl)){ # If reporting_level is equal to domain variables

    # CPI and PPP variable cannot mismatch

    if(cpfw$cpi_domain_var!=cpfw$ppp_domain_var){

      rlang::abort(message = "There is a mismatch on the cpi_domain_var or ppp_domain_var",
                   class = c("piperr", "cpi_ppp_var"))

    }


    for (x in data_level_vars) {

      if (cpfw$reporting_level == 1) { # CASE 1: They are all national

        setattr(dt, x, "national")

      } else if (cpfw$reporting_level == 2) {

        if (cpfw$cpi_domain_var == "urban") { # CASE 2: The cpi and ppp domain variable is "urban"

          setattr(dt, x, "area") # Name of the variable to use will be area

        } else if (cpfw$cpi_domain_var != "urban") { # CASE 3: The cpi and ppp domain variable is different than "urban"

          rlang::abort(message = "The cpi domain variable is different than urban",
                       class = c("piperr","cpi_dom_var"))

          # setattr(dt, x, cpfw$cpi_domain_var) # Name of the variable to use
        }

      } else {

        setattr(dt, x, as.character()) # CASE 4: There is no value for the reporting level
      }
    }

    setattr(dt, "aux_data_levels", "same")


  }else if(any(same_rep_lvl==FALSE)){ # If reporting_level is different to any domain variables


    for(x in 1:length(domain_vars)){

      dom_var <- domain_vars[x]
      dta_var <- data_level_vars[x]

      if (cpfw[[dom_var]] == 1) {

        setattr(dt, dta_var, "national" ) # CASE 1: If domain variable is 1, then national

      }else if (cpfw[[dom_var]] == 2) {

        if(dom_var %in% c("cpi_domain","ppp_domain")){

          if(cpfw$cpi_domain_var == "urban" & cpfw$ppp_domain_var == "urban"){

            setattr(dt, dta_var, "area") # CASE 2: If domain variable is cpi or ppp, and the domain_var is "urban", use area

          }else if(cpfw$cpi_domain_var != "urban"){

            rlang::abort(message = "The cpi domain variable is different than urban",
                         class = c("piperr","cpi_dom_var"))

            # setattr(dt, dta_var, cpfw$cpi_domain_var) # CASE 3: If domain variable is cpi or ppp, and the domain_var is not "urban", use domain_var

          }

        }else{

          setattr(dt, dta_var, "area") # CASE 4: For all other domain_var we use area (Need to check if this is correct)

        }

      }else{

        setattr(dt, dta_var, as.character()) # CASE 5: There is no value for domain variable

      }

    }

    setattr(dt, "aux_data_levels", "different")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Add distribution type (lower level, S3 methods)
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @export
add_dist_type <- function(dt, cpfw) {
  UseMethod("add_dist_type")
}


#' Add distribution type micro
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @export
add_dist_type.pipmd <- function(dt, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create distribution_type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (cpfw$use_imputed == 1) {

      # dt[, distribution_type := "imputed"]
      setattr(dt, "distribution_type", "imputed")

    }else {

      # dt[, distribution_type := "micro"]
      setattr(dt, "distribution_type", "micro")

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Add distribution type group
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @export
add_dist_type.pipgd <- function(dt, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create distribution_type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # dt[,
  #      distribution_type := {
  #
  #        if (cpfw$pop_domain == 1) {
  #
  #          y <- "group"
  #
  #        } else if (cpfw$pop_domain ==  2) {
  #
  #          larea <- length(unique(area))
  #
  #          if (larea %in% c(0, 1)) {
  #            y <- "group"
  #          } else {
  #            y <- "aggregate"
  #          }
  #
  #        } else {
  #          y <- ""
  #        }
  #        y
  #
  #      }
  #   ]

  if (cpfw$pop_domain == 1) {

    dist_type <- "group"

  } else if (cpfw$pop_domain ==  2) {

    larea <- length(unique(dt$area))

    if (larea %in% c(0, 1)) {
      dist_type <- "group"
    } else {
      dist_type <- "aggregate"
    }

  } else {
    dist_type <- ""
  }

  setattr(dt, "distribution_type", dist_type)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Transform unique value variable into attributes
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
# col_to_attr <- function(dt, cpfw) {
#
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # computations   ---------
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   pref             <- c("ppp", "cpi", "gdp", "pce", "pop")
#
#   vars <- names(dt)
#
#   fixed_vars <- vars[!(vars %in% data_level_vars)]
#
#   # Use Zander functions (NEED TO FIX TO USE PIPLOAD)
#
#   dt <- all_cols_to_attr(dt, fixed_cols = fixed_vars)
#
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # Return   ---------
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   return(dt)
#
# }
