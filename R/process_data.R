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
#' \dontrun{
#' pfw <- pipload::pip_load_aux("pfw")
#'
#' gd  <- pipload::pip_load_dlw("CHN", 2015)
#' process_data(gd, pfw)
#'
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' process_data(md, pfw)
#' }
process_data <- function(df, pfw, ...) {
  UseMethod("process_data")

}

#' @param pfw dataframe with Price Framework data loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @export
#' @rdname process_data
process_data.pipmd <- function(df, pfw, ...) {
  cli::cli_alert_info("Using microdata method")

  assign("survey_id",
         unique(df$survey_id),
         envir = .logenv)

  # on.exit ------------
  on.exit({
      rm(survey_id,
         envir = .logenv)
  })

  # Add country code variable (small fix)
  if("countrycode" %in% names(df)){
    df$country_code <- df$countrycode
  }

  # Unique obs per pfw --------
  keyVar <- c("country_code", "surveyid_year", "survey_acronym")
  pfw <- unq_obs_dt(pfw, keyVar)


  # Computations -------
  res <- tryCatch(
    expr = {

      ls_cpfw <- pd_cpfw_merge(df, pfw)

      pd_dlw_clean(ls_cpfw)

    },

    error = function(cnd){

      log_failure(cnd)

      survey_id <- c(.logenv$survey_id)

      cli::cli_alert("The survey {survey_id} was skipped")

      return(NA)

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
  cli::cli_alert_info("Using group data method")

  assign("survey_id",
         unique(df$survey_id),
         envir = .logenv)

  # on.exit ------------
  on.exit({
    rm(survey_id,
       envir = .logenv)
  })

  if("countrycode" %in% names(df)){
    df$country_code <- df$countrycode
  }

  # Unique obs per pfw --------
  keyVar <- c("country_code", "surveyid_year", "survey_acronym")
  pfw <- unq_obs_dt(pfw, keyVar)

  # Computations -------
  res <- tryCatch(
    expr = {

      ls_cpfw <- pd_cpfw_merge(df, pfw)

      pd_dlw_clean(ls_cpfw)

    },

    error = function(cnd){

      survey_id <- c(.logenv$survey_id)

      cli::cli_alert("The survey {survey_id} was skipped")

      log_failure(cnd)

      return(NA)

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

        msg <- cli::cli_format("There {?is/are} {n_rep} duplicates in PFW")

        piperr(message = msg,
               name = "dup_pfw")

        # cli::cli_abort(message = "There {?is/are} {n_rep} duplicates in `pfw`",
        #                class = c("dup_pfw", "piperr"),
        #                link =  unique(dt_d$link),
        #                call = sys.call())
      }

    },

    dup_pfw = function(cnd){

      log_failure(cnd)

    },

    finally = {

      dt <- unique(dt, by = keyVar) # eliminate duplicates

    }

  )


  return(dt)

}
