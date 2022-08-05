#' Clean data for wbpip compatibility (high level)
#'
#' @param lf list of dataframe returned by `pd_dlw_clean()`
#' @inheritParams pd_dlw_clean
#'
#' @return list with data.tables
#' @export
#'
#' @examples
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw  <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' md   <- pd_split_alt_welfare(md, cpfw)
#' x    <- pd_dlw_clean(md, cpfw)
#' y    <- pd_wbpip_clean(x)[[1]]
#' summary(y$weight)
#'
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- pd_wbpip_clean(x)[[1]]
#' y[, unique(area)]
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x <- pd_dlw_clean(gd, cpfw)
#' y <- pd_wbpip_clean(x)[[1]]
#' y[, unique(area)]
pd_wbpip_clean <- function(lf, cpfw) {

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
  rl <-
    tryCatch(
      expr = {
        # Your code...
        if (inherits(lf, "list")) {
          y <- purrr::map(.x = lf, .f = wbpip_clean)
        } else {
          y <- wbpip_clean(lf)
          y <- list(y)
        }

        names(y) <- sapply(cpfw, `[[`, "cache_id")
        y
      }, # end of expr section

      error = function(e) {
        glue("Error: {e$message}")
      }, # end of error section

      warning = function(w) {
        glue("Warning: {w$message}")
      }, # end of warning section

      finally = {
        # Do this at the end before quitting the tryCatch structure...
      } # end of finally section

    ) # End of trycatch



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
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' md   <- pd_split_alt_welfare(md, cpfw)
#' x <- pd_dlw_clean(md, cpfw)
#' y <- wbpip_clean(x[[1]])
#' summary(y$weight)
wbpip_clean.pipmd <- function(df, ...) {

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

#' wbpip_clean method for pipgd class (group data)
#'
#' @inheritParams wbpip_clean
#'
#' @return data.table
#' @export
#'
#' @examples
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
wbpip_clean.pipgd <- function(df, ...) {

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
