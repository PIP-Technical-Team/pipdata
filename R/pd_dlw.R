#' PD: process data. Source: datalibweb
#'
#' process datalibweb data. Use S3 method to identify whether the data is
#' microdata, groupdata or imputed data
#'
#' @param df dataframe loaded with `pipload::pip_load_dlw()`
#' @param ...  other parameters
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#'
#' gd  <- pipload::pip_load_dlw("CHN", 2015)
#' pd_dlw(gd, pfw)
#'
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pd_dlw(md, pfw)
pd_dlw <- function(df, ...) {
  UseMethod("pd_dlw")

}

#' @param pfw dataframe with Price Framework data loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @export
#' @rdname pd_dlw
pd_dlw.pipmd <- function(df, pfw, ...) {

  cli::cli_alert_info("Using microdata method")

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot(

  )

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------
  df <- pd_dlw_clean(df = df, pfw = pfw)



  # Return -------------
  return(invisible(df))

}

#' @param pfw dataframe with Price Framework data loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @export
#' @rdname pd_dlw
pd_dlw.pipgd <- function(df, pfw, ...) {
  cli::cli_alert_info("Using group data method")

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot(

  )

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------
  df <- pd_dlw_clean(df = df, pfw = pfw)


  # Return -------------
  return(invisible(df))
}


#' @export
#' @rdname pd_dlw
pd_dlw.default <- function(df, ...) {

  cli::cli_alert("no PIP method for this data. Returning same object")
  return(invisible(df))

}
