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

  # Computations -------
  welfare_type <- cpfw[[1]]$wt
  df[,
     welfare_type := welfare_type
      ]

  ## one data frame ------
  if (length(cpfw)  == 1) {

    l <- list(df)
    names(l) <- cpfw[[1]]$cache_id
    return(l)
  }

  ## Two data frames -----
  other_welfare      <- cpfw[[2]]$oth_welfare1_var #alternative wlf is in position 2
  other_welfare_type <- cpfw[[2]]$wt
  dfa <- copy(df)
  dfa[,
      `:=`(
        welfare = get(other_welfare),
        welfare_type = other_welfare_type
      )]

  l <- list(df, dfa)
  names(l) <- sapply(cpfw, `[[`, "cache_id")

  # Return -------------
  return(l)

}
