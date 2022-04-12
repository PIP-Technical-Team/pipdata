#' Clean data to meet wbpip requirements and formats
#'
#' @param df dataframe returned by `pd_dlw_clean()`
#' @param ... Other parameters
#'
#' @return dataframe
#' @export
#'
#' @examples
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' x <- pd_dlw_clean(df = md, cpfw)
#' y <- pd_wbpip_clean(x)
#' summary(y$weight)
#'
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- pd_wbpip_clean(x)
#' y[, unique(area)]
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' x <- pd_dlw_clean(gd, cpfw)
#' y <- pd_wbpip_clean(x)
#' y[, unique(area)]
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
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' x <- pd_dlw_clean(df = md, cpfw)
#' y <- pd_wbpip_clean(x)
#' summary(y$weight)
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

  df <- pipload::as_pipmd(df)

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
#' pfw  <- pipload::pip_load_aux("pfw")
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- pd_wbpip_clean(x)
#' y[, unique(area)]
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' x <- pd_dlw_clean(gd, cpfw)
#' y <- pd_wbpip_clean(x)
#' y[, unique(area)]
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
  gd_type <- df[, unique(gd_type)]
  gd_type <- as.numeric(sub("T0", "", gd_type))

  areas <- df[, unique(area)]

  dl <- lapply(areas, function(a) {
    dtt <- df[area == a]

    dtt <- wbpip:::gd_clean_data(
      dtt,
      welfare = "welfare",
      population = "weight",
      gd_type = gd_type,
      quiet = TRUE
    )
  })

  ndf <- rbindlist(l = dl,
                   use.names = TRUE,
                   fill = TRUE)
  ndf <- pipload::as_pipgd(ndf)


  # Return -------------
  return(ndf)

}
