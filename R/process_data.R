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

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------
  y <- tryCatch(
    expr = {

      cpfw <- get_country_pfw(df, pfw)

      ls_cpfw <- pd_cpfw_merge(df, cpfw)

      x  <- pd_dlw_clean(ls_cpfw)

      pd_wbpip_clean(lf = x)

    },

    error = function(cnd){

      survey_id <- c(.logenv$survey_id)

      # if(rlang::cnd_inherits(cnd, "piperr")){

      cli::cli_alert("The survey {survey_id} was skipped")

      #   return(NA)
      # }
      #
      # cnd_err <- rlang::catch_cnd(cli::cli_abort(message = "[Unknown error] The survey was skipped",
      #                 class = c("Unk_err", "piperr"),
      #                 link = survey_id,
      #                 call = cnd$call))
      #
      # add_log(cnd_err)

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

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------
  y <- tryCatch(
    expr = {

      cpfw <- get_country_pfw(df, pfw)

      ls_cpfw <- pd_cpfw_merge(df, cpfw)

      x  <- pd_dlw_clean(ls_cpfw)

      pd_wbpip_clean(lf = x)

    },

    error = function(cnd){

      survey_id <- c(.logenv$survey_id)

      # if(rlang::cnd_inherits(cnd, "piperr")){

      cli::cli_alert("The survey {survey_id} was skipped")

      #   add_log(cnd)
      #
      #   return(NA)
      # }
      #
      # cnd_err <- rlang::catch_cnd(cli::cli_abort(message = "[Unknown error] The survey was skipped",
      #                                            class = c("Unk_err", "piperr"),
      #                                            link = survey_id,
      #                                            call = cnd$call))
      #
      # add_log(cnd_err)

      log_failure(cnd)

      return(NA)

    }
  )

  # Return -------------
  return(invisible(y))

}


#' @export
#' @rdname process_data
process_data.default <- function(df, ...) {

  cli::cli_alert("no PIP method for this data. Returning same object")
  return(invisible(df))

}
