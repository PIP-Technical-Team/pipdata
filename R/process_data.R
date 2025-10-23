#' Process datalibweb data: merge PFW data and clean variables
#'
#' @param df dataframe loaded with `valid_dlw_load`
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


}
