#' Merge country/survey PFW info with dataliweb survey data
#'
#' @param dt DLW country/survey data
#' @param pfw PFW
#'
#' @return list
#' @export
#'
#' @examples
#' pfw  <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' l    <- pd_cpfw_merge(md, pfw)
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' l    <- pd_cpfw_merge(gd, pfw)
pd_cpfw_merge <- function(dt, pfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## PFW for specific country --------

  cpfw <- get_country_pfw(dt, pfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Split alternative welfare --------

  lf   <- pd_split_alt_welfare(dt, cpfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Map survey to cpfw --------

  lfs <- purrr::map2(.x = lf,
                     .y =  cpfw,
                     .f = cpfw_merge)

  # names(lfs) <- sapply(cpfw, `[[`, "cache_id") #Maybe not needed
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
      dt_c <- add_main_vars(dt_c, cpfw)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Area (Needed for Domain variables)--------
      dt_c <- add_area(dt_c)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Domain variables (same for md and gd) --------
      dt_c <- add_dom_vars(dt_c, cpfw)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Distribution type  (different for md and gd) --------
      dt_c <- add_dist_type(dt_c, cpfw)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Transform unique variables into attributes --------

      dt_f <- col_to_attr(dt_c, cpfw)


  return(dt_f)

}

#' Add metadata variables to country/survey
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#'
#' @keywords internal
add_main_vars <- function(dt, cpfw) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # computations   ---------
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      variables <- colnames(dt)

      main_vars <- c("survey_year",
                     "country_code",
                     "welfare_type")

      vars <- main_vars[!(main_vars %in% variables)]

      # Inform what country/surveys are missing a main variable

      if(length(vars)>0){

        survey_id <- c(.pipdataenv$survey_id)

        vars <- cli::cli_vec(vars, list("vec-trunc" = 3))

        msg <- cli::format_error("Main variable{?s} {vars} missing")


        pipfun::log_add(event = "info",
                        message = msg,
                        name = "pipdata_log",
                        args = list(info = "mn_var_inf",
                                    survey = survey_id))

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Recode urban to area (lower level, S3 methods)
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
add_area <- function(dt) {
  UseMethod("add_area")
}

#' Recode urban to area for micro data
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
add_area.pipmd <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Abort if not urban variable
  if (!(c("urban") %in% colnames(dt))){

    survey_id <- c(.pipdataenv$survey_id)

    pipfun::log_add(event = "info",
                    message = "There is no urban variable",
                    name = "pipdata_log",
                    args = list(info = "urb_var",
                                survey = survey_id))

    # cli::cli_abort(message =  "There is no urban variable",
    #                class = c("piperr", "urb_var"))

    # piperr(message =  "There is no urban variable",
    #        name = "urb_var")

#         cli::cli_abort(message = "There is no urban variable",
#                        class = c("urb_var", "piperr"),
#                        log = log_err,
#                        skip = skip_err,
#                        link =  unique(dt$survey_id),
#                        call = sys.call())

    dt[, area := ""]

    return(dt)
  }

  # Recode urban to area

  dt[, area := fcase(urban == 1, "urban",
                           urban == 0, "rural",
                           is.na(urban), "",
                           default = "")]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Recode urban to area for group data
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
add_area.pipgd <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (c("subnatid") %in% colnames(dt)){

    setnames(dt, "subnatid", "subnatid1")

  }


  # Abort if not urban variable
  if (!any(c("urban", "area") %in% colnames(dt))){

    # piperr(message = "There is no urban or area variable",
    #        name = "urb_var")

    # cli::cli_abort(message = "There is no urban or area variable",
    #                class = c("urb_var", "piperr"),
    #                log = log_err,
    #                skip = skip_err,
    #                link =  unique(dt$survey_id),
    #                call = sys.call())

    survey_id <- c(.pipdataenv$survey_id)

    pipfun::log_add(event = "info",
                    message = "There is no urban or area variable",
                    name = "pipdata_log",
                    args = list(info = "urb_var",
                                survey = survey_id))

    dt[, area := ""]

    return(dt)
  }

  if(c("area") %in% colnames(dt)){

    return(dt)
  }

  if(c("urban") %in% colnames(dt)){
    # Recode urban to area

    dt[, area := fcase(urban == 1, "urban",
                       urban == 0, "rural",
                       is.na(urban), "national",
                       default = "")]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

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
  data_level_vars  <- glue("{pref}_data_level")
  domain_vars      <- glue("{pref}_domain")

  if(any(!(domain_vars %in% names(cpfw)))){

    miss_vars <- domain_vars[!(domain_vars %in% names(cpfw))]
    miss_vars <- cli::cli_vec(vars, list("vec-trunc" = 3))
    msg <- cli::format_error("Domain variable{?s} {miss_vars} missing in country PFW")

    # piperr(message = msg,
    #        name = "dom_var")

    survey_id <- c(.pipdataenv$survey_id)

    pipfun::log_add(event = "info",
                    message = msg,
                    name = "pipdata_log",
                    args = list(info = "dom_var",
                                survey = survey_id))

    # cli::cli_abort(message = "Domain variable{?s} {miss_vars} missing in country `pfw`",
    #                class = c("dom_var", "piperr"),
    #                log = log_err,
    #                skip = skip_err,
    #                link =  svy,
    #                call = sys.call())

  }

  data_level_vars  <- data_level_vars[(domain_vars %in% names(cpfw))]
  domain_vars  <- domain_vars[(domain_vars %in% names(cpfw))]
  trows <- nrow(dt)

  dt[,
     (data_level_vars) :=
       lapply(domain_vars, \(x) {

         if (cpfw[[x]] == 1) {

           y <- rep("national", times = trows)

         } else if (cpfw[[x]] == 2) {
           y <-  area
         } else {
           y <-  as.character()
         }
         y

       })
  ]

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
#' @keywords internal
add_dist_type <- function(dt, cpfw...) {
  UseMethod("add_dist_type")
}


#' Add distribution type
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
add_dist_type.pipmd <- function(dt, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create distribution_type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (cpfw$use_imputed == 1) {

      dt[, distribution_type := "imputed"]

    }else {

      dt[, distribution_type := "micro"]

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Add distribution type
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
add_dist_type.pipgd <- function(dt, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create distribution_type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dt[,
       distribution_type := {

         if (cpfw$pop_domain == 1) {

           y <- "group"

         } else if (cpfw$pop_domain ==  2) {

           larea <- length(unique(area))

           if (larea %in% c(0, 1)) {
             y <- "group"
           } else {
             y <- "aggregate"
           }

         } else {
           y <- ""
         }
         y

       }
    ]

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
col_to_attr <- function(dt, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  var_att_svy <- c("country_code", # We can add this to the internal data
               "survey_id",
               "surveyid_year",
               "survey_acronym",
               "survey_year",
               "welfare_type",
               "distribution_type",
               "gd_type")

  vars <- names(dt)

  fixed_vars <- vars[!(vars %in% var_att_svy)]

  # Use Zander functions (NEED TO FIX TO USE PIPLOAD)

  dt <- all_cols_to_attr(dt, fixed_cols = fixed_vars)

  # Add reporting level from cpfw

  attr(dt, "reporting_level") <- cpfw$reporting_level

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
