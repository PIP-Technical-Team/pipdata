#' Clean data for wbpip compatibility (high level)
#'
#' @param lf list of dataframe returned by `pd_dlw_clean()`
#'
#' @return list with data.tables
#' @export
pd_wbpip_clean <- function(lf) {

  # Computations -------

  rl <- purrr::map(.x = lf, .f = wbpip_clean)

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
wbpip_clean <- function(df, ...) {
  UseMethod("wbpip_clean")
}



#' wbpip_clean method for pipmd class (microdata)
#'
#' @inheritParams wbpip_clean
#'
#' @return data.table
#' @export
wbpip_clean.pipmd <- function(df, ...) {

  md <- copy(df)

  # Computations -------
  md <- wbpip:::md_clean_data(
    md,
    welfare = "welfare",
    weight = "weight",
    quiet = TRUE
  )$data

  # Return -------------
  return(md)

}

#' wbpip_clean method for pipgd class (group data)
#'
#' @inheritParams wbpip_clean
#'
#' @return data.table
#' @export
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

  gd_type <- attributes(df)$gd_type

  if (is.null(gd_type)) {
    # piperr(message = "There is no gd_type in pfw",
    #        name = "gd_type_miss")

    # survey_id <- pd_env_get("log_survey_id")
    #
    cli::cli_abort(
      message = "There is no gd_type variable",
      class = c("piperr", "gd_type_miss")
    )
  }

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

  dt <- df|>
    collapse::fgroup_by(area)|>
    collapse::fmutate(wbpip::gd_clean_data(.data,
      welfare = "welfare",
      population = "weight",
      gd_type = gd_type,
      quiet = TRUE
    ))|>
    collapse::fungroup()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
