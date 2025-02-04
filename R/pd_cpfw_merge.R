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


  if (inherits(lf, "list")) { #Needed? Maybe all list?
    lfs <- purrr::map2(.x = lf,
                     .y =  cpfw,
                     .f = cpfw_merge)
  } else {
    dt <- cpfw_merge(lf, cpfw[[1]])
    lfs <- list(dt)
  }

  #names(y) <- sapply(cpfw, `[[`, "cache_id")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(lfs)

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
cpfw_merge <- function(dt, cpfw,...) {
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
cpfw_merge.pipmd <- function(dt, cpfw, ...){

  #   ____________________________________________________________________________
  #   Initial formatting                                                      ####

  # hard copy
  md <- copy(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main variables (same md and gd) --------
  md <- add_main_vars(md, cpfw)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Area (same for md and gd??)(Needed for Domain variables)--------
  md <- add_area(md)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Domain variables (same for md and gd) --------
  md <- add_dom_vars(md, cpfw)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Distribution type  (different) --------

  # Create distribution_type
  if (cpfw$use_imputed == 1) {

    md[, distribution_type := "imputed"]

  }else {

    md[, distribution_type := "micro"]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Transform into attributes --------

  # Use Zander functions

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

#' Recode urban to area
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @keywords internal
add_area <- function(dt, log_err = TRUE, skip_err = TRUE) {

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

      if(any(class(dt)=="pipgd")){

        dt[, area := fcase(urban == 1, "urban",
                           urban == 0, "rural",
                           is.na(urban), "national",
                           default = "")]

      }else if(any(class(dt)=="pipmd")){

        dt[, area := fcase(urban == 1, "urban",
                           urban == 0, "rural",
                           is.na(urban), "",
                           default = "")]
      }
    },
    urb_var = function(cnd){

      if(cnd$log){ # Log the error

        add_log(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }

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
    },
    dom_var = function(cnd){

      if(cnd$log){ # Log the error

        add_log(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }

    }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
