#' deflate welfare to add_pip_vars (Higher level)
#'
#' @param lf list of dataframes with welfare variable called welfare
#' @inheritParams pd_dlw_clean
#' @param  cpi dataframe from `pipload::pip_load_aux("cpi")`
#' @param  ppp dataframe from `pipload::pip_load_aux("ppp")`
#'
#' @return data.table with new variable add_pip_vars
#' @export
#'
#' @examples
pd_add_pip_vars <- function(lf, cpfw, cpi, ppp) {

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
          y <- purrr::map2(.x = lf,
                           .y =  cpfw,
                           .f = add_pip_vars,
                           cpi = cpi,
                           ppp = ppp)
        } else {
          y <- add_pip_vars(df = lf,
                           cpfw = cpfw,
                           cpi = cpi,
                           ppp = ppp)
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


#' Estimate welfare in PPP values (lower level)
#' @inheritParams pd_add_pip_vars
#' @param ... extra arguments
#' @return
#' @export
#'
#' @examples
add_pip_vars <- function(df, cpfw, cpi, ppp, ...) {
  UseMethod("add_pip_vars")
}


#' Title
#'
#' @inheritParams add_pip_vars
#'
#' @return data.table.
#' @export
#'
#' @examples
add_pip_vars.default <- function(df, cpfw, cpi, ppp, ...) {

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
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## reporting level variable --------

  dl_var        <- grep("data_level", names(df), value = TRUE) # data_level vars
  ordered_level <- purrr::map_dbl(dl_var, ~ get_ordered_level(df, .x))
  select_var    <- dl_var[which.max(ordered_level)]

  df[, reporting_level := get(select_var)]

  data.table::setorder(df, reporting_level)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Deflated data --------
  # ppp_table <- ppp_table[ppp_default == TRUE]

  # Merge survey table with PPP (left join)
  df <- joyn::merge(df, ppp_table,
                    by         = c("country_code", "ppp_data_level"),
                    match_type = "m:1",
                    yvars      = "ppp",
                    keep       = "left",
                    reportvar  = FALSE,
                    verbose    = FALSE
  )

  # Merge survey table with CPI (left join)
  df <- joyn::merge(df, cpi_table,
                    by = c(
                      "country_code", "survey_year",
                      "survey_acronym", "cpi_data_level"
                    ),
                    match_type = "m:1",
                    yvars = "cpi",
                    keep = "left",
                    reportvar = FALSE,
                    verbose = FALSE
  )

  df[
    ,
    welfare_lcu := welfare
  ][
    ,
    welfare_ppp := wbpip::deflate_welfare_mean(
      welfare_mean = welfare_lcu,
      ppp          = ppp,
      cpi          = cpi
    )
  ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## scale subnational population to National accounts (WDI) --------


  nrl <- length(df[, unique(reporting_level)]) # number of reporting level
  dst <- df[, unique(distribution_type)]       # distribution type

  if ( nrl > 1  &&  dst == "micro")  {
    df <- adjust_population(df, pop_table)
  }  # end of population adjustment


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert character to factors --------

  chr_vars <- names(df)[sapply(df, is.character)]

  df[,
     (chr_vars) := lapply(.SD, as.factor),
     .SDcols = chr_vars
  ]



  # Return -------------
  return(invisible(TRUE))

}
