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
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
get_country_pfw <- function(df, pfw) {

  # on.exit ------------
  on.exit({

  })


  # Defenses -----------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Unique obs per pfw --------

  keyVar <- c("country_code", "surveyid_year", "survey_acronym")

  pfw <- unq_obs_dt(pfw, keyVar)

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------
  # subset microdata survey; BIN is BIN is treated as microdata in PCN/PIP
  # pfw <- pfw[use_microdata == 1 |
  #              use_bin     == 1 |
  #              use_imputed == 1 |
  #              inpovcal     == 1] # subset country-years in Povcalnet

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## unique variables --------

  # get single-value variables
  uvl <- uniq_vars_to_list(df)  #list with unique value

  # filter country PFW

  cpfw <-
    pfw[ country_code     == uvl$country_code
         & surveyid_year  == uvl$surveyid_year
         & survey_acronym == uvl$survey_acronym
    ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## reporting level  --------

  cpfw <- report_lvl(cpfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Other welfare type --------

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


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Cache ID   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
                      uvl$module,
                      sep = "_"
    )
  ]

  cpfw <- split(cpfw, by = "cache_id")

  # Return -------------
  return(cpfw)

}

#' Find unique values in dt according to some key variables
#'
#' @param dt data.table or data.frame
#' @param keyVar character vector with variables to determine unique observations
#' @param log_err boolean TRUE or FALSE if the error of duplicates should be added
#' to the log
#' @param skip_err boolean TRUE or FALSE if we want to skip the abort when finding
#' the duplicates error
#'
#' @return data.table or data.frame
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' keyVar <- c("country_code", "surveyid_year", "survey_acronym")
#' unq_obs_dt(pfw, keyVar)
unq_obs_dt <- function(dt,
                       keyVar,
                       log_err = TRUE,
                       skip_err = TRUE) {

  tryCatch(

    expr = {

      if(uniqueN(dt, by = keyVar) != nrow(dt)){

        dt_d <- dt[duplicated(dt, by = keyVar)]
        n_rep <- nrow(dt_d)

        cli::cli_abort(message = "There {?is/are} {n_rep} duplicates in `pfw`",
                       class = c("dup_pfw", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  unique(dt_d$link),
                       call = sys.call())
      }

    },

    dup_pfw = function(cnd){

      if(cnd$log){ # Log the error

        add_log(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }

    },

    finally = {

      dt <- unique(dt, by = keyVar) # eliminate duplicates

    }

  )


  return(dt)

}

#' Add reporting level variable
#'
#' @param cpfw data.table with country Price Framework
#' @inheritParams unq_obs_dt
#'
#' @return data.table
#' @export
report_lvl <- function(cpfw,
                       log_err = TRUE,
                       skip_err = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dcols <- c(
    "cpi_domain",
    "ppp_domain",
    "gdp_domain",
    "pce_domain",
    "pop_domain"
  )

  tryCatch(
    expr = {

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
        cli::cli_abort(message = "PFW does not contains info for country, surveyid year, and survey_acronym",
                       class = c("no_pfw", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  unique(cpfw$survey_id),
                       call = sys.call())
      }else if(nrow(cpfw) > 1){
        cli::cli_abort(message = "PFW is not unique for country, surveyid year, and survey_acronym",
                       class = c("unq_pfw", "piperr"),
                       log = log_err,
                       skip = skip_err,
                       link =  unique(cpfw$survey_id),
                       call = sys.call())
      }

    },

    unq_pfw = function(cnd){

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
  return(cpfw)

}
