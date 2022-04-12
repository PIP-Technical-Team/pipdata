#' Split data based on alternative welfare
#'
#' Split data into two dataframes when there is alternative welfare in the same
#' survey
#'
#' @param df data frame with group data, loaded with `pipload::pip_load_dlw()`
#' @param cpfw data frame with Price framework data for country/survey in `df`.
#'   It is loaded with `get_country_pfw(df, pfw)`. `pfw` is loaded in
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return list
#' @export
#'
#' @examples
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' pfw  <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' pd_split_alt_welfare(md, cpfw)
pd_split_alt_welfare <- function(df, cpfw) {

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot( exprs = {

    }
  )

  welfare_type <- cpfw$wt[[1]]
  df[,
     welfare_type := get(welfare_type)
      ]

  # Early returns ------
  if (length(cpfw$cache_id)  == 1) {

    l <- list(df)
    names(l) <- cpfw$cache_id[[1]]
    return(l)
  }

  # Computations -------



  # Return -------------
  return(invisible(TRUE))

}
