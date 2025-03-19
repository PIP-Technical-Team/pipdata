#' Deflation of welfare using auxiliary data
#'
#' @param lf cleaned DLW surveys from `pd_wbpip_clean`
#' @param cpi aux cpi
#' @param ppp aux_ppp
#' @param pop aux_pop
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' ppp  <- pipload::pip_load_aux("ppp")
#' cpi  <- pipload::pip_load_aux("cpi")
#' pop  <- pipload::pip_load_aux("pop")
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd)
#' y    <- pd_wbpip_clean(x)
#'
#' pd_deflation(y, cpi = cpi, ppp = ppp, pop = pop)
#' }
pd_deflation <- function(lf, cpi, ppp, pop) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PPP manipulation
  ppp           <- ppp_to_wide(ppp = ppp)

  # deflate per list
  rl <- purrr::map(.x = lf,
                   .f = deflation,
                   cpi = cpi,
                   ppp = ppp,
                   pop = pop)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(rl)

}

#' Deflation of welfare using auxiliary data (lower level)
#'
#' @param df data.table of cleaned DLW survey from `wbpip_clean`
#' @inheritParams pd_deflation
#' @param ... extra arguments
#'
#' @return data.table
#' @export
deflation <- function(df,  cpi, ppp, pop,...) {
  UseMethod("deflation")
}

#' Deflation of welfare for micro data
#'
#' @inheritParams deflation
#' @return data.table
#' @export
#'
#' @examples
deflation.pipmd <- function(df,  cpi, ppp, pop,...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Defenses (??)
  fnms <- names(formals())
  fnms <- fnms[!fnms %in% "..."]

  for (i in seq_along(fnms)) {
    rr <- get(fnms[[i]])
    if (inherits(rr, "data.table")) {
      assign(fnms[i], copy(rr))

    } else {
      assign(fnms[i], qDT(rr))
    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add reporting level --------

  df <- add_rep_lvl(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Deflation --------

  ### Merge survey with ppp ---------
  df <- add_ppp(df, ppp)





  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(TRUE)

}

#' Deflation of welfare for group data
#'
#' @inheritParams deflation
#' @return data.table
#' @export
#'
#' @examples
deflation.pipgd <- function(df,  cpi, ppp, pop,...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(TRUE)

}


#' Convert PPP data from `pipload` to wide format
#'
#' @param ppp data frame with ppp data from `pipload::pip_load_aux("ppp")`
#'
#' @return data.table with PPP values to wide format based on versioning
#' @export
#'
#' @examples
#' ppp <-  pipload::pip_load_aux("ppp")
#' x   <-  ppp_to_wide(ppp)
#' names(x)
ppp_to_wide <- function(ppp) {

  #   ____________________________________________________________________________
  #   Defenses                                                           ####
  if (inherits(ppp, "data.table")) {
    ppp <- copy(ppp)
  } else {
    ppp <- qDT(ppp)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Identify ppp versions --------

  ppp[,
      ppp_version := {
        x <- paste0("ppp_", ppp_year, "_", release_version, "_", adaptation_version)
        x <- gsub("_v", "_0", x )
      }
  ]

  ppp_v <- ppp[, unique(ppp_version)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Transfor ppp data.table from long to wide --------

  ppp <- dcast(ppp,
               formula = country_code + ppp_data_level ~ ppp_version,
               value.var = "ppp",
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add all ppp_version to attributes --------

  setattr(ppp, "ppp_versions", ppp_v)


  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(ppp)

}

#' Identify reporting level from data_level variables
#'
#' @param dt data.table
#'
#' @return data.table with reporting_level variable
#' @keywords internal
add_rep_lvl <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dl_var        <- grep("data_level", names(dt), value = TRUE) # data_level vars
  ordered_level <- purrr::map_dbl(dl_var, ~ get_ordered_level(dt, .x))
  report_lvl_cpfw <- as.numeric(attributes(dt)$reporting_level)
  select_var    <- dl_var[ordered_level==report_lvl_cpfw]

  dt[, reporting_level := get(select_var[1])]

  setorder(dt, reporting_level)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Merge survey with PPP
#'
#' @param dt data.table of the survey
#' @param ppp data.table of all ppp
#'
#' @return data.table with specific ppp
#' @keywords internal
add_ppp <- function(dt, ppp) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ppp_c <- ppp[ppp$country_code==attributes(dt)$country_code$values]

  dt <- joyn::merge(dt, ppp_c,
                    by         = c("ppp_data_level"),
                    match_type = "m:1",
                    keep       = "left",
                    reportvar  = FALSE,
                    verbose    = FALSE
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
