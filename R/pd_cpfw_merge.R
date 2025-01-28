#' Merge cpfw info to data.frame
#'
#' @param lf list (more than one if there are two or more
#' welfare types in the survey)
#' @param cpfw list (more than one if there are two or more
#' welfare types in the survey)
#'
#' @return list
#' @export
#'
#' @examples
#' pfw  <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' lf   <- pd_split_alt_welfare(md, cpfw)
#' l    <- pd_cpfw_merge(lf, cpfw)
pd_cpfw_merge <- function(lf, cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot( exprs = {

    ## check that both are lists
  }
  )

  # Early returns ------
  if (FALSE) {
    return()
  }

  if (inherits(lf, "list")) {
    df <- purrr::map2(.x = lf,
                     .y =  cpfw,
                     .f = cpfw_merge)
  } else {
    df <- cpfw_merge(lf, cpfw[[1]])
    df <- list(df)
  }

  #names(y) <- sapply(cpfw, `[[`, "cache_id")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(df)

}
