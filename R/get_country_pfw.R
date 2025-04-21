#' Get Country Price framework  data based on PFW and DLW data info
#'
#' @param df data frame with micro data, loaded with `pipload::pip_load_dlw()`
#' @param pfw data frame with Price framework data, loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return list of data.tables
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' gd   <- pipload::pip_load_dlw("PHL", 2012)
#' cpfw <- get_country_pfw(gd, pfw)
get_country_pfw <- function(df, pfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Filter country PFW --------

  uvl <- uniq_vars_to_list(df)  #list with unique values for survey

  cpfw <- pfw[ country_code     == uvl$country_code
               & surveyid_year  == uvl$surveyid_year
               & survey_acronym == uvl$survey_acronym]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add reporting level  --------

  cpfw <- report_lvl(cpfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check other welfare type --------

  cpfw <- othr_wlf(cpfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create cache ID   ---------

  cpfw <- cache_id(cpfw, uvl$module)

  # Return -------------
  return(cpfw)

}

#' Add reporting level variable to country PFW
#'
#' @param cpfw data.table with country Price Framework
#' @inheritParams unq_obs_dt
#'
#' @return data.table
#' @keywords internal
report_lvl <- function(cpfw,
                       log_err = TRUE,
                       skip_err = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tryCatch(
    expr = {

      dcols <- c(
        "cpi_domain",
        "ppp_domain",
        "gdp_domain",
        "pce_domain",
        "pop_domain"
      )

      cpfw <-
        cpfw[
          # filter inpovcal data
          inpovcal == 1
        ][,
          # Find MAX domain per obs
          reporting_level := apply(.SD, MARGIN = 1,
                                   function(x) {
                                     y <- max(x)
                                     as.character(y)
                                   }),
          .SDcols = dcols
        ]

      if(nrow(cpfw)==0){

        survey_id <- c(.logenv$survey_id)

        cli::cli_abort(message = "PFW does not contains info for country, surveyid year, and survey_acronym",
                       class = c("piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  survey_id,
                       call = sys.call())

      }else if(nrow(cpfw) > 1){

        cli::cli_abort(message = "PFW is not unique for country, surveyid year, and survey_acronym",
                       class = c("unq_pfw", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  unique(cpfw$link),
                       call = sys.call())
      }

    },

    unq_pfw = function(cnd){

      if(cnd$log){ # Log the error

        log_failure(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }
    }
    # ,
    #
    # no_pfw = function(cnd){
    #
    #   if(cnd$log){ # Log the error
    #
    #     add_log(cnd)
    #
    #   }
    #
    #   cli::cli_abort(cnd$message,
    #                  parent = cnd,
    #                  call = cnd$call)
    #
    # }

  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(cpfw)

}


#' Duplicate country PFW if there are two types of welfare
#'
#' @param cpfw country PFW data.table
#' @param log_wrn boolean value for logging warning in log.txt
#'
#' @return data.table
#' @keywords internal
othr_wlf <- function(cpfw,
                     log_wrn = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tryCatch(
    expr = {

      cpfw[,
           is_alt_welf := FALSE
      ]

      if (cpfw$oth_welfare1_type != "") {

        cpfw_alt <- copy(cpfw)

        cpfw_alt[
          ,
          welfare_type := fcase(
            grepl("^([Cc])", oth_welfare1_type), "consumption",
            grepl("^([Ii])", oth_welfare1_type), "income",
            default = ""
          )
        ][
          ,
          oth_welfare1_type := NULL # remove variable
        ][
          ,
          is_alt_welf := TRUE
        ]


        cpfw <- rbindlist(l         =  list(cpfw, cpfw_alt),
                          use.names = TRUE,
                          fill      = TRUE)

      }

      if(nrow(cpfw)>1){

        svy <- unique(cpfw$link)

        cli::cli_abort(message = "More than one type of welfare",
                       class = c("othr_wlf_inf", "piperr"),
                       log = log_wrn,
                       link = svy,
                       call = sys.call())

      }

    },

    othr_wlf_inf = function(cnd){

      if(cnd$log){ # Log the warning

        log_failure(cnd)

      }

    }

  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(cpfw)

}

#' Create cache ID for country PFW
#'
#' @param cpfw country PFW data.table
#' @param module survey module
#'
#' @return data.table
#' @keywords internal
cache_id <- function(cpfw,
                     module,
                     log_err = TRUE,
                     skip_err = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tryCatch(
    expr = {

      cpfw[
        ,
        wt := fcase(
          welfare_type == "income", "INC",
          welfare_type == "consumption", "CON",
          default = ""
        )
      ][
        ,
        cache_id := paste(country_code,
                          surveyid_year,
                          survey_acronym,
                          paste0("D", reporting_level),
                          wt,
                          module,
                          sep = "_"
        )
      ]

      if(any(cpfw$wt=="")){

        cli::cli_abort(message = "Welfare type is undefined",
                       class = c("no_wlf_tp", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  unique(cpfw$link),
                       call = sys.call())
      }

    },

    no_wlf_tp = function(cnd){

      if(cnd$log){ # Log the error

        log_failure(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }
    }
  )

  cpfw <- split(cpfw, by = "cache_id")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(cpfw)

}
