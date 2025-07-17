#' deflate welfare to add_pip_vars (Higher level)
#'
#' @param lf list of dataframes with welfare variable called welfare
#' @param  cpi dataframe from `pipload::pip_load_aux("cpi")`
#' @param  ppp dataframe from `pipload::pip_load_aux("ppp")`
#' @param  pop dataframe from `pipload::pip_load_aux("pop")`
#'
#' @return data.table with new variable add_pip_vars
#' @export
#'
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#' ppp  <- pipload::pip_load_aux("ppp")
#' cpi  <- pipload::pip_load_aux("cpi")
#' pop  <- pipload::pip_load_aux("pop")
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw  <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' md   <- pd_split_alt_welfare(md, cpfw)
#' x    <- pd_dlw_clean(md, cpfw)
#' y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)
#'
#' pd_add_pip_vars(lf = y, cpi = cpi, ppp = ppp, pop = pop)
#'
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)
#'
#' pd_add_pip_vars(lf = y, cpi = cpi, ppp = ppp, pop = pop)
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)
#'
#' pd_add_pip_vars(lf = y, cpi = cpi, ppp = ppp, pop = pop)
#' }
pd_add_pip_vars <- function(lf, cpi, ppp, pop) {

  # Computations -------

  # PPP manipulation
  ppp           <- ppp_to_wide(ppp = ppp)

    # if (inherits(lf, "list")) { # It is always a list by default
      rl <- purrr::map(.x = lf,
                       .f = add_pip_vars,
                       cpi = cpi,
                       ppp = ppp,
                       pop = pop)
    # } else {
    #   y <- add_pip_vars(df = lf,
    #                    cpfw = cpfw,
    #                    cpi = cpi,
    #                    ppp = ppp,
    #                    pop = pop)
    #   y <- list(y)
    # }
    # names(y) <- sapply(cpfw, `[[`, "cache_id")

  # Return -------------
  return(rl)

}


#' Estimate welfare in PPP values (lower level)
#'
#' @param  df  dataframe from `pd_wbpip_clean()`
#' @param  cpi dataframe from `pipload::pip_load_aux("cpi")`
#' @param  ppp dataframe from `pipload::pip_load_aux("ppp")`
#' @param  pop dataframe from `pipload::pip_load_aux("pop")`
#' @param ... extra arguments
#' @return data.table with extra PIP variables
#' @export
#'
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' ppp  <- pipload::pip_load_aux("ppp")
#' cpi  <- pipload::pip_load_aux("cpi")
#' pop  <- pipload::pip_load_aux("pop")
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw  <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(md, pfw)
#' md   <- pd_split_alt_welfare(md, cpfw)
#' x    <- pd_dlw_clean(md, cpfw)
#' y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)[[1]]
#'
#' add_pip_vars(df = y, cpi = cpi, ppp = ppp, pop = pop)
#'
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)[[1]]
#'
#' add_pip_vars(df = y, cpi = cpi, ppp = ppp, pop = pop)
#'
#' gd   <- pipload::pip_load_dlw("ARE", 2019)
#' cpfw <- get_country_pfw(gd, pfw)
#' gd   <- pd_split_alt_welfare(gd, cpfw)
#' x    <- pd_dlw_clean(gd, cpfw)
#' y    <- pd_wbpip_clean(lf = x, cpfw = cpfw)[[1]]
#'
#' add_pip_vars(df = y, cpi = cpi, ppp = ppp, pop = pop)
#' }
add_pip_vars <- function(df, cpi, ppp, pop, ...) {


  # Defenses -----------
  # if (inherits(df, "data.table")) {
  #  df <- data.table::copy(df)
  # } else {
  #   df <- data.table::as.data.table(df)
  # }

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


  # Computations -------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## reporting level variable --------

  df <- add_rep_lvl(df)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Deflated data --------


### . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### ppp treatment                                                     ####

  # ppp_table <- ppp_table[ppp_default == TRUE]

  # Merge survey table with PPP (left join)
  df <- add_ppp(df,ppp)

  # Merge survey table with CPI (left join)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### CPI treatment                                                           ####

  if ("cpi2005_SM21" %in% names(cpi)) {
    setnames(cpi, "cpi2005_SM21", "cpi2005") # temporal solution
  }

  cpi_by      <- c("country_code", "survey_year",
               "survey_acronym", "cpi_data_level")
  cpi_vars    <- grep("^cpi[0-9]{4}$", names(cpi), value = TRUE)
  cpi_years     <-
    gsub("cpi([0-9]+)", "\\1", cpi_vars) |>
    unique() |>
    sort()


  cpi_to_keep <- c(cpi_by, cpi_vars)
  cpi <- cpi[, ..cpi_to_keep]

  df <- joyn::merge(df, cpi,
                    by = cpi_by,
                    match_type = "m:1",
                    keep = "left",
                    reportvar = FALSE,
                    verbose = FALSE
  )

  df[,
    welfare_lcu := welfare
  ]


###  .......................................................
###  Check CPI and PPP years                        ####

  ppp_versions  <- attr(ppp, "ppp_versions")
  ppp_years     <-
    gsub("ppp_([0-9]+)(_.*)", "\\1", ppp_versions) |>
    unique() |>
    sort()

  if (setequal(cpi_years , ppp_years)) {
    base_years <-  cpi_years # deflate years
  } else {
    base_years <-  intersect(cpi_years , ppp_years)
    cli::cli_alert_danger("CPI and PPP years available do NOT match.
                          Only the intersect will be used: {.field {base_years}}")
  }


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Deflation of welfare variables                                          ####

  dt_w <- purrr::map_dfc(.x = base_years,
                         .f = get_welfare_ppp,
                         df = df)

  df  <- cbind(df, dt_w)
  rm(dt_w)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## scale subnational population to National accounts (WDI) --------


  nrl <- length(df[, unique(reporting_level)]) # number of reporting level
  dst <- df[, unique(distribution_type)]       # distribution type

  if ( nrl > 1  &&  !(dst %in% c("group", "aggregate")))  {
    # sd <- split(df, by = "imputation_id")
    # lf <- purrr::map(.x = sd,
    #                  adjust_population,
    #                  pop_table = pop_table)
    #
    df <- adjust_population(df, pop)
  }  # end of population adjustment


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert character to factors --------

  chr_vars <- names(df)[sapply(df, is.character)]

  df[,
     (chr_vars) := lapply(.SD, as.factor),
     .SDcols = chr_vars
  ]



  # Return -------------
  return(df)

}


#' Adjust microdata to WDI population levels when the number of reporting levels
#' is equal or greater than 2
#'
#' @param df dataframe with microdata
#' @param pop population data from WDI.
#'
#' @return dataframe
adjust_population <- function(df, pop) {

  spop <- df[,
             # get total population by level
             .(weight = sum(weight, na.rm = TRUE)),
             by = c("country_code", "survey_year", "reporting_level")]


  dpop <- joyn::merge(pop, spop,
                      by         = c("country_code",
                                     "pop_data_level = reporting_level"),
                      match_type =  "m:1",
                      keep       = "inner",
                      reportvar  =  FALSE)

  dpop <-
    dpop[,
         # Abs difference in year
         diff_year := abs(year - survey_year)
    ][,
      # get the min in each data level
      .SD[diff_year == min(diff_year)],
      by = pop_data_level
    ][,
      # get weights for weighted mean
      wght := fifelse(diff_year == 0, 1, 1/diff_year) ]

  fact <-
    dpop[,
         # get mean of population.
         lapply(.SD, stats::weighted.mean, w = wght),
         by = "pop_data_level",
         .SDcols = c("pop", "weight")
    ][,
      pop_fact := pop/weight
    ][,
      c("pop", "weight") := NULL]

  df <- joyn::merge(x  = df,
                    y  = fact,
                    by = c("reporting_level = pop_data_level"),
                    match_type = "m:1",
                    reportvar = FALSE)

  df[,
     weight := weight*pop_fact]

  return(df)
}
