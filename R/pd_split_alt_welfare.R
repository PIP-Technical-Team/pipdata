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
#' md  <- pipdata:::m_svy_id_to_att(md)
#' pfw  <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' df   <- pd_split_alt_welfare(md, cpfw)
pd_split_alt_welfare <- function(dt, cpfw) {

  # Computations -------

  ## One data frame ------

  if (length(cpfw)== 1) {

    welf_type <- cpfw[[1]]$welfare_type

    attr(dt, "welfare_type") <- welf_type

    l <- list(dt)

    names(l) <- cpfw[[1]]$cache_id

    return(l)

  }

  ## Two data frames -----

  if(cpfw[[1]]$is_alt_welf == FALSE & cpfw[[2]]$is_alt_welf == TRUE){

    mcpfw <- cpfw[[1]]
    scpfw <- cpfw[[2]]

  }else if(cpfw[[2]]$is_alt_welf == FALSE & cpfw[[1]]$is_alt_welf == TRUE){

    mcpfw <- cpfw[[2]]
    scpfw <- cpfw[[1]]

  }else{

    rlang::abort(message = "The country PFW has two welfare types and issues on the `alt_welf` var",
                 class = c("piperr", "alt_welf_issue"),
                 use_cli_format = TRUE)
  }

  welf_type <- mcpfw$welfare_type

  attr(dt, "welfare_type") <- welf_type

  other_welfare      <- scpfw$oth_welfare1_var #alternative wlf is in position 2
  other_welfare_type <- scpfw$welfare_type

  dta <- copy(dt)

  dta[,
      welfare := get(other_welfare)
      ]

  setattr(dta, "welfare_type", other_welfare_type)

  l <- list(dt, dta)

  names(l) <- sapply(cpfw, `[[`, "cache_id")

  # Return ------------

  return(l)

}
