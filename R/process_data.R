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

  # Add country code variable
  if("countrycode" %in% names(df)){
    df$country_code <- df$countrycode
  }

  # Computations -------
  y <- tryCatch(
    expr = {

      ls_cpfw <- pd_cpfw_merge(df, pfw)

      ls_clean  <- pd_dlw_clean(ls_cpfw)

      pd_wbpip_clean(ls_clean)

    },

    error = function(cnd){

      survey_id <- c(.logenv$survey_id)

      cli::cli_alert("The survey {survey_id} was skipped")

      log_failure(cnd)

      return(NA)

      }
  )


  # Return -------------
  return(invisible(y))

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

  # Computations -------
  res <- tryCatch(
    expr = {

      ls_cpfw <- pd_cpfw_merge(df, pfw)

      ls_clean  <- pd_dlw_clean(ls_cpfw)

      pd_wbpip_clean(ls_clean)

    },

    error = function(cnd){

      survey_id <- c(.logenv$survey_id)

      cli::cli_alert("The survey {survey_id} was skipped")

      log_failure(cnd)

      return(NA)

    }
  )

  # Return -------------
  return(invisible(res))

}


#' @export
#' @rdname process_data
process_data.default <- function(df, ...) {

  cli::cli_alert("no PIP method for this data. Returning same object")
  return(invisible(df))

}
