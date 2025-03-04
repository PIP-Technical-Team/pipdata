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
#'
#' @return data.table
#' @export
deflation <- function(df,  cpi, ppp, pop,...) {
  UseMethod("deflation")
}

#' Deflation of welfare for micro data
#'
#' @return data.table
#' @export
#'
#' @examples
deflation.pipmd <- function() {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(TRUE)

}
