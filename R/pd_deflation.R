#' Deflation of welfare using auxiliary data
#'
#' @param lf cleaned DLW surveys from `pd_wbpip_clean`
#' @param cpi aux_cpi from `pipload::pip_load_aux`
#' @param ppp aux_ppp from `pipload::pip_load_aux`
#' @param pop aux_pop from `pipload::pip_load_aux`
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
  ppp <- ppp_to_wide(ppp = ppp)

  # CPI manipulation
  if ("cpi2005_SM21" %in% names(cpi)) {
    setnames(cpi, "cpi2005_SM21", "cpi2005") # temporal solution
  }

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
#' @param dt data.table of cleaned DLW survey from `wbpip_clean`
#' @inheritParams pd_deflation
#' @param ... extra arguments
#'
#' @return data.table
#' @export
deflation <- function(dt,  cpi, ppp, pop,...) {
  UseMethod("deflation")
}

#' Deflation of welfare for micro data
#'
#' @inheritParams deflation
#' @return data.table
#' @export
deflation.pipmd <- function(dt,  cpi, ppp, pop,...) {

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

  dt <- add_rep_lvl(dt)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Deflation --------

  ### Merge survey with ppp and cpi ---------

  dt <- add_aux(dt, ppp, cpi)

  ### Welfare LCU ---------

  dt <- welfare_lcu(dt)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Deflation of welfare for group data
#'
#' @inheritParams deflation
#' @return data.table
#' @export
deflation.pipgd <- function(dt,  cpi, ppp, pop,...) {

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

#' Add auxiliary data for deflation
#'
#' @inheritParams pd_deflate
#' @return data.table
#' @keywords internal
add_aux <- function(dt, ppp ,cpi) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tryCatch(
    expr = {

      ### Merge ppp ---------

      dt <- add_ppp(dt, ppp)

      ### Merge cpi ---------

      dt <- add_cpi(dt, cpi)

      ### Check and add base years

      dt <- cpi_ppp_years(dt, ppp)

    },
    cpi_ppp = function(cnd){

      if(cnd$log){ # Log the error

        add_log(cnd)

      }

      if(!cnd$skip){ # Abort if you don't want to skip, but after logging

        cli::cli_abort(cnd$message, call = cnd$call)

      }

    }

  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Merge survey with PPP
#'
#' @param dt data.table of the survey
#' @inheritParams pd.deflation
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

#' Merge survey with CPI
#'
#' @param dt
#' @inheritParams pd.deflation
#'
#' @return data.table with all cpi
#' @keywords internal
add_cpi <- function(dt, cpi) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ### Country cpi ---------

  con <- attributes(dt)$country_code$value
  svy_year <- attributes(dt)$survey_year$value
  svy_acr <- attributes(dt)$survey_acronym$value

  cpi_c <- cpi[(country_code == con &
              survey_year == svy_year &
              survey_acronym == svy_acr)] # Check that is only one value?

  ### Variables and year ---------

  cpi_vars <- grep("^cpi[0-9]{4}$", names(cpi_c), value = TRUE)

  cpi_years <- gsub("cpi([0-9]+)", "\\1", cpi_vars)|> unique() |> sort()

  attr(dt, "cpi_years") <- cpi_years

  cpi_to_keep <- c("cpi_data_level", cpi_vars)
  cpi_c <- cpi_c[, ..cpi_to_keep]

  ### Join all cpi ---------

  dt <- joyn::merge(dt, cpi_c,
                    by = "cpi_data_level",
                    match_type = "m:1",
                    keep = "left",
                    reportvar = FALSE,
                    verbose = FALSE
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Identify base years for deflation
#'
#' @inheritParams pd_deflation
#'
#' @return data.table
#' @keywords internal
cpi_ppp_years <- function(dt, ppp) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ppp_versions  <- attr(ppp, "ppp_versions")
  ppp_years     <-
    gsub("ppp_([0-9]+)(_.*)", "\\1", ppp_versions) |>
    unique() |>
    sort()

  cpi_years <- attributes(dt)$cpi_years

  if (setequal(cpi_years , ppp_years)) {

    attr(dt, "base_years") <-  cpi_years # deflate years

  } else {
    attr(dt, "base_years") <-  intersect(cpi_years , ppp_years)

    svy <- attributes(dt)$survey_id$values
    cli::cli_abort(message = "CPI and PPP years available do NOT match.
                          Only the intersect will be used: {.field {base_years}}",
                   class = c("cpi_ppp", "piperr"),
                   log = log_err,
                   skip = skip_err,
                   link =  svy,
                   call = sys.call())

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}


#' Create welfare_lcu variable
#'
#' @inheritParams pd_deflation
#'
#' @return data.table
#' @keywords internal
welfare_lcu <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dt[,
     welfare_lcu := welfare
  ]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

