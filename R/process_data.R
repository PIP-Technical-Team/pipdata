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
  # on.exit({
  #   rm(survey_id,
  #      envir = .pipdataenv)
  # }) # For now

  svy <- attributes(df)$survey_id

  assign("survey_id",
         svy,
         envir = .pipdataenv)

  if("countrycode" %in% names(df)){
    df$country_code <- df$countrycode
  }

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
                      logmeta = list(error = class(cnd)[2],
                                     survey = survey_id,
                                     status = "The survey was skipped"))

      NULL

    },

    error = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      logmeta = list(error = "unknown_error",
                                     survey = survey_id,
                                     status = "The survey was skipped"))

      NULL

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
  # on.exit({
  #   rm(survey_id,
  #      envir = .pipdataenv)
  # })

  svy <- attributes(df)$survey_id

  assign("survey_id",
         svy,
         envir = .pipdataenv)

  if("countrycode" %in% names(df)){
    df$country_code <- df$countrycode
  }

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
                      logmeta = list(error = class(cnd)[2],
                                  survey = survey_id,
                                  status = "The survey was skipped"))

      NULL

    },

    error = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      logmeta = list(error = "unknown_error",
                                     survey = survey_id,
                                     status = "The survey was skipped"))

      NULL

    }
  )

  # Return -------------
  return(res)

}


#' @export
#' @rdname process_data
process_data.default <- function(df, ...) {

  cli::cli_alert("no PIP method for this data. Returning NULL")
  return(NULL)

}
