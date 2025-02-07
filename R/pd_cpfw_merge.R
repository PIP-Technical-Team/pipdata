#' Merge country/survey PFW info with dataliweb survey data
#'
#' @param dt DLW country/survey data
#' @param cpfw list (more than one if there are two or more
#' welfare types in the survey)
#'
#' @return list
#' @export
#'
#' @examples
#'
#' pfw  <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' l    <- pd_cpfw_merge(md, cpfw)
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' l    <- pd_cpfw_merge(gd, cpfw)
pd_cpfw_merge <- function(dt, cpfw) {

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
  ## Split alternative welfare --------

  lf   <- pd_split_alt_welfare(dt, cpfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Map survey to cpfw --------

  if (inherits(lf, "list")) {
    lfs <- purrr::map2(.x = lf,
                     .y =  cpfw,
                     .f = cpfw_merge)
  } else { #Needed? Maybe all list?
    dt <- cpfw_merge(lf, cpfw[[1]])
    lfs <- list(dt)
  }

  names(lfs) <- sapply(cpfw, `[[`, "cache_id")

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
#'
#' @examples
#'
#' pfw <- pipload::pip_load_aux("pfw")
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' l    <- cpfw_merge(gd, cpfw[[1]])
#'
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' l    <- cpfw_merge(md, cpfw[[1]])
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

  dt_f <- col_to_attr(dt_c)

  return(dt_f)

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
                     "country_code",
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

    },

    mn_var_inf = function(cnd){

      if(cnd$log){ # Log the information

        add_log(cnd)

      }
    },

    finally = {

      # Add variables if missing

      dt[, (main_vars) :=
           lapply(main_vars, \(x) {

             if (!(x %in% variables)) {
               cpfw[[x]]

             } else {
               dt[[x]]

             }
           })]

    }

  )

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
add_area <- function(dt, cpfw...) {
  UseMethod("add_area")
}

#' Recode urban to area for micro data
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
add_area.pipmd <- function(dt, log_err = TRUE, skip_err = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tryCatch(
    expr = {

      # Abort if not urban variable
      if (!(c("urban") %in% colnames(dt))){

        cli::cli_abort(message = "There is no urban variable",
                       class = c("urb_var", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  unique(dt$survey_id),
                       call = sys.call())
      }

      # Recode urban to area

        dt[, area := fcase(urban == 1, "urban",
                           urban == 0, "rural",
                           is.na(urban), "",
                           default = "")]

    },
    urb_var = function(cnd){

      if(cnd$log){ # Log the error

        add_log(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }

      dt[, area := ""]

    }
  )

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
add_area.pipgd <- function(dt, log_err = TRUE, skip_err = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tryCatch(
    expr = {

      # Abort if not urban variable
      if (!(c("urban") %in% colnames(dt))){

        cli::cli_abort(message = "There is no urban variable",
                       class = c("urb_var", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  unique(dt$survey_id),
                       call = sys.call())
      }

      # Recode urban to area

        dt[, area := fcase(urban == 1, "urban",
                           urban == 0, "rural",
                           is.na(urban), "national",
                           default = "")]

    },
    urb_var = function(cnd){

      if(cnd$log){ # Log the error

        add_log(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }

      dt[, area := ""]

    }
  )

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
add_dom_vars <- function(dt, cpfw, log_err = TRUE, skip_err = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Level and domain variables    ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tryCatch(
    expr = {

      pref             <- c("ppp", "cpi", "gdp", "pce", "pop")
      data_level_vars  <- glue("{pref}_data_level")
      domain_vars      <- glue("{pref}_domain")

      if(any(!(domain_vars %in% names(cpfw)))){

        svy <- unique(cpfw$link)

        miss_vars <- domain_vars[!(domain_vars %in% names(cpfw))]

        cli::cli_abort(message = "Domain variable{?s} {miss_vars} missing in country `pfw`",
                       class = c("dom_var", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  svy,
                       call = sys.call())

      }

    },
    dom_var = function(cnd){

      if(cnd$log){ # Log the error

        add_log(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }

    },
    finally = {

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

    }
  )

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
#' @param dt data.table
#'
#' @return data.table
#' @keywords internal
col_to_attr <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  var_att_svy <- c("country_code",
               "survey_id",
               "surveyid_year",
               "survey_acronym",
               "survey_year",
               "welfare_type",
               "distribution_type",
               "cpi_data_level",
               "ppp_data_level",
               "gdp_data_level",
               "pce_data_level",
               "gd_type",
               "alt_welfare")

  vars <- names(dt)

  fixed_vars <- vars[!(vars %in% var_att_svy)]

  # Use Zander functions (NEED TO FIX TO USE PIPLOAD)

  dt <- all_cols_to_attr(dt, fixed_cols = fixed_vars)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
