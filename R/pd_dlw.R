#' PD: process data. Case: datalibweb
#'
#' process datalibweb data. Use S3 method to identify whether the data is
#' microdata, groupdata or imputed data
#'
#' @return
#' @export
pd_dlw <- function(df) {
  UseMethod("pd_dlw")

}


#' @export
#' @rdname pd_dlw
pd_dlw.pipmd <- function(df) {

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



  # Return -------------
  return(invisible(df))

}

#' @export
#' @rdname pd_dlw
pd_dlw.pipgd <- function(df) {
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


  # Return -------------
  return(invisible(df))
}


#' @export
#' @rdname pd_dlw
pd_dlw.default <- function(df) {

  cli::cli_alert("no PIP method for this data. Returning same object")
  return(invisible(df))

}
