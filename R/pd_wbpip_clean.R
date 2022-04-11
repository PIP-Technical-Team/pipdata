#' Clean data to meet wbpip requirements and formats
#'
#' @param df dataframe returned by `pd_dlw_clean()`
#'
#' @return dataframe
#' @export
#'
#' @examples
pd_wbpip_clean <- function(df, ...) {
  UseMethod("pd_wbpip_clean")
}



#' pd_wbpip_clean method for pipmd class (microdata)
#'
#' @inheritParams pd_wbpip_clean
#'
#' @return
#' @export
#'
#' @examples
pd_wbpip_clean.pipmd <- function(df, ...) {

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot( exprs = {

    }
  )

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------
  df <- wbpip:::md_clean_data(
    df,
    welfare = "welfare",
    weight = "weight",
    quiet = TRUE
  )$data

  # Return -------------
  return(invisible(df))

}




#' pd_wbpip_clean method for pipgd class (group data)
#'
#' @inheritParams pd_wbpip_clean
#'
#' @return
#' @export
#'
#' @examples
pd_wbpip_clean.pipgd <- function(df, ...) {

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot( exprs = {

    }
  )

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------


  # Return -------------
  return(invisible(TRUE))

}
