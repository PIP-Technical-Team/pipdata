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
#' process_data(gd, pfw)
#'
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' process_data(md, pfw)
process_data <- function(df, ...) {
  UseMethod("process_data")

}

#' @param pfw dataframe with Price Framework data loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @export
#' @rdname process_data
process_data.pipmd <- function(df, pfw, ...) {
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
  cpfw <- get_country_pfw(df, pfw)
  x    <- pd_dlw_clean(df, cpfw)
  y    <- pd_wbpip_clean(x)



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
  cpfw <- get_country_pfw(df, pfw)
  x    <- pd_dlw_clean(df, cpfw)
  y    <- pd_wbpip_clean(x)



  # Return -------------
  return(invisible(y))

}


#' @export
#' @rdname process_data
process_data.default <- function(df, ...) {

  cli::cli_alert("no PIP method for this data. Returning same object")
  return(invisible(df))

}
