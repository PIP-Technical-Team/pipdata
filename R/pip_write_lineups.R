#' Function to minimize and save reference year distribution for one
#' country and one reference year
#'
#' @param df_refy output from [get_refy_distributions]
#' @param path path to save the output - "P:\03.pip\lineup_distributions\output-lineup-ref-years"
#'
#' @return invisible logical - purpose is to save file
#' @keywords internal
write_refy_dist <- function(df_refy,
                            path = Sys.getenv("PIP_LINEUPS_DIR")) {

  cntry_code <- attributes(df_refy)$country_code
  ref_year   <- attributes(df_refy)$reporting_year

  # save
  qs::qsave(x        = df_refy,
            file     = fs::path(path,
                                paste0(cntry_code,
                                       "_",
                                       ref_year,
                                       ".qs")),
            preset   = "fast",
            nthreads = 4)

  invisible(TRUE)
}




#' Estimate and save the reference year distributions for all countries-ref-years
#'
#' A wrapper over [get_refy_distributions]  and [write_refy_dist]
#' applied on a list of data frames
#'
#' @param df_refy Output from [get_refy_distributions]
#' @param cntry_refy list: each element is another list containing
#'                   country_code scalar (e.g. "ZAF") and year vector
#'                   (e.g.`c(2001, 2002, 2003)`). If there are four countries,
#'                   the `length(cntry_refy) = 4`, one for each country with its
#'                   year vector.
#' @param path path to save the output - "P:\03.pip\lineup_distributions\output-lineup-ref-years"
#'
#' @return invisible logical: writes files
#' @export
#'
#' @examples
#' \dontrun{
#' write_multiple_refy_dist(df_refy = df_refy,
#'                          cntry_refy = list(list(country_code = "ZAF",
#'                                                 year         = 2000:2005),
#'                                            list(country_code = "COL",
#'                                                 year         = 2000:2005)),
#'                          path = Sys.getenv("PIP_LINEUPS_DIR"))
#'                          }
write_multiple_refy_dist <-
  function(df_refy,
           cntry_refy,
           path,
           gls,
           dl_aux) {

    # ppp year
    py <- strsplit(gls$vintage_dir, "_")[[1]][2]

    lapply(cli::cli_progress_along(cntry_refy,
                                   total = length(cntry_refy)),
           FUN = \(i) {

             x <- cntry_refy[[i]]

             lapply(x$year,
                    FUN = \(year         = x$year,
                            country_code = x$country_code){

                      suppressMessages(get_refy_distributions(df_refy    = df_refy,
                                                              cntry_code = country_code,
                                                              ref_year   = year,
                                                              gls        = gls) |>
                                         add_aux_data_attr(dl_aux          = dl_aux,
                                                           df_refy         = df_refy,
                                                           py              = py) |>
                                         write_refy_dist(path = path))
                    }
             )
           })
    invisible(TRUE)
  }


