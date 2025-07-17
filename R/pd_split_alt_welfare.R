#' Split data based on alternative welfare
#'
#' Split data into two dataframes when there is alternative welfare in the same
#' survey
#'
#' @param dt data table loaded with `pipload::pip_load_dlw()`
#' @param cpfw data frame with Price framework data for country/survey in `df`.
#'   It is loaded with `get_country_pfw(df, pfw)`. `pfw` is loaded in
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return list
#' @export
#'
#' @examples
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' pfw  <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' df   <- pd_split_alt_welfare(md, cpfw)
pd_split_alt_welfare <- function(dt, cpfw) {

  # Computations -------

  welf_type <- cpfw[[1]]$welfare_type

  # df[,
  #    welfare_type := welf_type
  #     ]
  attr(dt, "welfare_type") <- welf_type


  ## One data frame ------

  if (length(cpfw)== 1) {

    l <- list(dt)
    names(l) <- cpfw[[1]]$cache_id

    return(l)

  }

  ## Two data frames -----

  other_welfare      <- cpfw[[2]]$oth_welfare1_var #alternative wlf is in position 2
  other_welfare_type <- cpfw[[2]]$welfare_type

  dta <- copy(dt)

  dta[,
      # `:=`(
      #   welfare = get(other_welfare),
      #   welfare_type = other_welfare_type )
      welfare := get(other_welfare)
      ]

  attr(dta, "welfare_type") <- other_welfare_type

  l <- list(dt, dta)

  names(l) <- sapply(cpfw, `[[`, "cache_id")

  # Return ------------

  return(l)

}
