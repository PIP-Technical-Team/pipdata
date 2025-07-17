#' PD: process data. Source: datalibweb
#'
#' process datalibweb data. Use S3 method to identify whether the data is
#' microdata, groupdata or imputed data
#'
#' @param df dataframe loaded with `pipload::pip_load_dlw()`
#' @param pfw PFW
#' @param ...  other parameters
#'
#' @return data.table
#' @export
#'
#' @examples
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' pfw <- pipload::pip_load_aux("pfw")
#'
#' gd  <- pipload::pip_load_dlw("CHN", 2015)
#' gd  <- pipdata:::m_svy_id_to_att(gd)
#' process_data(gd, pfw)
#'
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' md  <- pipdata:::m_svy_id_to_att(md)
#' process_data(md, pfw)
process_data <- function(df, pfw, ...) {
  UseMethod("process_data")

}

#' @param pfw dataframe with Price Framework data loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @export
#' @rdname process_data
process_data.pipmd <- function(df, pfw, ...) {

  # on.exit ------------
  on.exit({
    rm(survey_id,
       envir = .logenv)
  }) # For now

  svy <- unique(df$survey_id)

  assign("survey_id",
         svy,
         envir = .pipdataenv)

  assign("survey_id",
         svy,
         envir = .logenv) # For now


  # Computations -------
  res <- tryCatch(
    expr = {

      ls_cpfw <- pd_cpfw_merge(df, pfw)

      pd_dlw_clean(ls_cpfw)

    },
    pipinf = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "info",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      output = NA,
                      args = list(error = class(cnd)[2],
                                  survey = survey_id,
                                  status = "The survey was skipped"))

    },
    piperr = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      output = NA,
                      args = list(error = class(cnd)[2],
                                     survey = survey_id,
                                     status = "The survey was skipped"))

    },

    error = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      output = NA,
                      logmeta = list(error = "unknown_error",
                                     survey = survey_id,
                                     status = "The survey was skipped"))

      }
  )


  # Return -------------
  return(res)

}

#' @param pfw dataframe with Price Framework data loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @export
#' @rdname process_data
process_data.pipgd <- function(df, pfw, ...) {

  # on.exit ------------
  on.exit({
    rm(survey_id,
       envir = .logenv)
  })

  svy <- unique(df$survey_id)

  assign("survey_id",
         svy,
         envir = .pipdataenv)

  assign("survey_id",
         svy,
         envir = .logenv) # For now

  # if("countrycode" %in% names(df)){
  #   df$country_code <- df$countrycode
  # }

  # Unique obs per pfw --------
  keyVar <- c("country_code", "surveyid_year", "survey_acronym")
  pfw <- unq_obs_dt(pfw, keyVar)

  # Computations -------
  res <- tryCatch(
    expr = {

      ls_cpfw <- pd_cpfw_merge(df, pfw)

      pd_dlw_clean(ls_cpfw)

    },

    piperr = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      output = NA,
                      args = list(error = class(cnd)[2],
                                  survey = survey_id,
                                  status = "The survey was skipped"))

    },

    error = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      output = NA,
                      logmeta = list(error = "unknown_error",
                                     survey = survey_id,
                                     status = "The survey was skipped"))

    }
  )

  # Return -------------
  return(res)

}


#' @export
#' @rdname process_data
process_data.default <- function(df, ...) {

  cli::cli_alert("no PIP method for this data. Returning same object")
  return(invisible(df))

}

#' Find unique values in PFW according to some key variables
#'
#' @param dt data.table or data.frame
#' @param keyVar character vector with variables to determine unique observations
#'
#' @return data.table or data.frame
#' @export
#'
#' @examples
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' pfw <- pipload::pip_load_aux("pfw")
#' keyVar <- c("country_code", "surveyid_year", "survey_acronym")
#' unq_obs_dt(pfw, keyVar)
unq_obs_dt <- function(dt,
                       keyVar) {

  tryCatch(

    expr = {

      if(uniqueN(dt, by = keyVar) != nrow(dt)){

        dt_d <- dt[duplicated(dt, by = keyVar)]
        n_rep <- nrow(dt_d)

        cli::cli_abort("There {?is/are} {n_rep} duplicates in PFW",
                       class = c("piperr","dup_pfw"))

        # msg <- cli::format_error("There {?is/are} {n_rep} duplicates in PFW")
        #
        # piperr(message = msg,
        #        name = "dup_pfw")

        # cli::cli_abort(message = "There {?is/are} {n_rep} duplicates in `pfw`",
        #                class = c("dup_pfw", "piperr"),
        #                link =  unique(dt_d$link),
        #                call = sys.call())
      }

    },

    piperr = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      dt <- unique(dt, by = keyVar)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      output = dt,
                      args = list(error = class(cnd)[2],
                                  survey = survey_id,
                                  status = "The survey was skipped"))

    }

  )

  return(dt)

}
