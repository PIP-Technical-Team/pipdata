#' Clean data for wbpip compatibility (high level)
#'
#' @param lf list of dataframe returned by `pd_dlw_clean()`
#'
#' @return list with data.tables
#' @export
#'
#' @examples
#' \dontrun{
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' pfw  <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' ls   <- pd_cpfw_merge(md, cpfw)
#' ls_c    <- pd_dlw_clean(ls)
#' ls_f    <- pd_wbpip_clean(lf = ls_c)
#' summary(y$weight)
#'
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)[[1]]
#' y[, unique(area)]
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x <- pd_dlw_clean(gd, cpfw)
#' y <- pd_wbpip_clean(lf = x, cpfw = cpfw)[[1]]
#' y[, unique(area)]
#' }
pd_wbpip_clean <- function(lf) {

  # Computations -------

    # if (inherits(lf, "list")) {
  rl <- purrr::map(.x = lf, .f = wbpip_clean)
    # } else {
    #   y <- wbpip_clean(lf)
    #   y <- list(y)
    # }

    # names(y) <- sapply(cpfw, `[[`, "cache_id")



  # Return -------------
  return(rl)

}

#' Clean data to meet wbpip requirements and formats
#'
#' @param df dataframe returned by `pd_dlw_clean()`
#' @param ... Other parameters
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' md   <- pd_split_alt_welfare(md, cpfw)
#' x <- pd_dlw_clean(md, cpfw)
#' y <- wbpip_clean(x[[1]])
#' summary(y$weight)
#'
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- wbpip_clean(x[[1]])
#' y[, unique(area)]
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x <- pd_dlw_clean(gd, cpfw)
#' y <- wbpip_clean(x[[1]])
#' y[, unique(area)]
#' }
wbpip_clean <- function(df, ...) {
  UseMethod("wbpip_clean")
}



#' wbpip_clean method for pipmd class (microdata)
#'
#' @inheritParams wbpip_clean
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' md   <- pd_split_alt_welfare(md, cpfw)
#' x <- pd_dlw_clean(md, cpfw)
#' y <- wbpip_clean(x[[1]])
#' summary(y$weight)
#' }
wbpip_clean.pipmd <- function(df, ...) {

  md <- copy(df)

  # Computations -------
  md <- wbpip:::md_clean_data(
    md,
    welfare = "welfare",
    weight = "weight",
    quiet = TRUE
  )$data

  # df <- pipload::as_pipmd(df) Not needed because it will repeated

  # Return -------------
  return(md)

}

#' wbpip_clean method for pipgd class (group data)
#'
#' @inheritParams wbpip_clean
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' pfw  <- pipload::pip_load_aux("pfw")
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- wbpip_clean(x[[1]])
#' y[, unique(area)]
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x <- pd_dlw_clean(gd, cpfw)
#' y <- wbpip_clean(x[[1]])
#' y[, unique(area)]
#' }
wbpip_clean.pipgd <- function(df, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Select gd_type --------

  gd_type <- get_gd_type(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Clean per area --------

  dt <- area_gd_clean(df, gd_type)

  # Return -------------
  return(dt)

}


#' Find group data type for cleaning
#'
#' @param df data.table
#'
#' @return data.table
#' @keywords internal
get_gd_type <- function(df) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gd_type <- attributes(df)$gd_type$values

  gd_type <- as.numeric(sub("T0", "", gd_type))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(gd_type)

}

#' Clean group data per area
#'
#' @param df data.frame
#' @param gd_type group data type
#'
#' @return data.table
#' @keywords internal
area_gd_clean <- function(df, gd_type) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  attr <- attributes(df)

  dt <- df |>
    _[, wbpip::gd_clean_data(
      .SD,
      welfare = "welfare",
      population = "weight",
      gd_type = gd_type,
      quiet = TRUE
    ),
    by = .(area)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Assign missing attributes --------

  attr_to_add <- attr[!names(attr) %in% names(attributes(dt))]

  dt <- add_attributes(dt, attr_to_add)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
