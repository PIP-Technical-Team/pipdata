
#' Estimate reference year distribution for given country, year
#'
#' @param df_refy data frame: output from [get_refy_mult_factor]
#' @param cntry_code country code
#' @param ref_year reference year for lineup
#' @param gls global list
#'
#' @return data frame:
#' @export
get_refy_distributions <- function(df_refy, cntry_code, ref_year, gls) {

  # ensure no factors
  lapply(df_refy,
         FUN = function(x) {
           if (is.factor(x)) {
             as.character(x)
           } else {
             x
           }
         }) |>
    qDT()

  # Filter df_refy
  df_refy <-
    df_refy |>
    fsubset(country_code == cntry_code &
              reporting_year == ref_year) |>
    fselect("country_code",
            "reporting_level",
            "welfare_type",
            #"income_group_code",
            "survey_year",
            "reporting_year",
            #"nac",
            #"nac_sy",
            "relative_distance",
            #"estimation_type",
            #"lineup_case",
            #"interpolation_id",
            #"predicted_mean_ppp",
            #"reporting_gdp",
            #"reporting_pce",
            "reporting_pop",
            #"monotonic",
            #"same_direction",
            #"svy_mean",
            "survey_id",
            "cache_id",
            "wb_region_code",
            #"pcn_region_code",
            "survey_acronym",
            #"survey_coverage",
            #"survey_comparability",
            #"comparable_spell",
            #"surveyid_year",
            #"survey_time",
            #"survey_mean_lcu",
            #"survey_mean_ppp",
            #"ppp",
            #"cpi",
            #"pop_data_level",
            #"gdp_data_level",
            #"pce_data_level",
            #"cpi_data_level",
            #"ppp_data_level",
            "distribution_type",
            #"gd_type",
            "is_interpolated",
            #"is_used_for_line_up",
            #"is_used_for_aggregation",
            #"display_cp",
            "lineup_approach",
            "mult_factor")


  # reduce df_refy
  df_refy <-
    df_refy |>
    vars_to_attr(vars = c(#"country_code",
      #"reporting_level",
      #"welfare_type",
      #"income_group_code",
      #"survey_year",
      #"reporting_year",
      "survey_id",
      #"cache_id",
      "wb_region_code",
      #"pcn_region_code",
      "survey_acronym",
      "distribution_type",
      "is_interpolated",
      "lineup_approach"#,
      #"mult_factor"
    ))

  # Load surveys
  cache_id <- df_refy$cache_id |>
    funique()
  gv(df_refy,
     "cache_id") <- NULL
  survey_year_rows <- list()
  df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                     FUN = function(x){
                                       pipload::pip_load_cache(cache_id = x,
                                                               version  = gls$vintage_dir) |>
                                         fselect(country_code,
                                                 surveyid_year,
                                                 survey_acronym,
                                                 survey_year,
                                                 welfare_ppp,
                                                 weight,
                                                 reporting_level,
                                                 welfare_type,
                                                 imputation_id)
                                     }))

  # Join welfare & weights vectors from surveys to df_refy
  df <-
    df_refy |>
    joyn(y          = df_svy,
         by         = c("country_code",
                        "survey_year",
                        "reporting_level",
                        "welfare_type"),
         keep       = "left",
         match_type = "1:m",
         verbose    = FALSE,
         sort       = FALSE,
         reportvar  = FALSE) |>
    # Group by survey year
    fgroup_by(survey_year) |>
    # number of imputations per survey year (if micro data then n_imp = 1)
    fmutate(n_imp      = data.table::uniqueN(imputation_id),
            # population at survey (decimal) year found by summing survey weights
            svy_pop    = fsum(weight),
            # weights scaled by ratio of ref year and svy year pop
            #         to make relative to reference year
            # weights adj by rel dist to get weighted average of population at ref year
            weight = weight * (reporting_pop / svy_pop) * # "adjust to WDI population" --> Andres, your comment
              relative_distance#,
            # ref year weights divided by number of imputations
            #      this should sum to population amount
            #weight_refy_adj = weight / n_imp
    ) |>
    fungroup() |>
    fmutate(welfare = welfare_ppp * mult_factor)

  if (any(diff(df$survey_year) < 0) &
      !any(diff(df_svy$survey_year) < 0)) {
    data.table::setorder(df,
                         survey_year)
  }

  # temp
  setkey(df, NULL)

  # Make survey year rows an attribute
  survey_year_rows <- df_svy |>
    fselect(survey_year) |>
    fmutate(rows = 1:fnrow(df_svy)) |>
    fgroup_by(survey_year) |>
    fsummarise(rows = fmax(rows))

  survey_year_rows <-
    list(survey_year  = survey_year_rows$survey_year,
         rows         = survey_year_rows$rows)

  # Make reporting level rows an attribute
  reporting_level_rows <- df_svy |>
    fselect(reporting_level, survey_year) |>
    fmutate(rows = 1:fnrow(df_svy),
            rl   = paste0(reporting_level,
                          survey_year)) |>
    fgroup_by(rl) |>
    fmutate(rows = fmax(rows)) |>
    fungroup() |>
    fselect(reporting_level, rows) |>
    funique()

  reporting_level_rows <-
    list(reporting_level = as.character(reporting_level_rows$reporting_level),
         rows            = reporting_level_rows$rows)

  # Add dist stats to attributes
  dist_stats <- get_dist_stats(df = df)
  attr(df,
       "dist_stats") <- dist_stats$dist_stats
  attr(df,
       "dt_dist_stats") <- dist_stats$dt_dist

  # Make welfare type an attribute
  df <-
    df |>
    vars_to_attr(vars = c("welfare_type"))

  # keep attributes of df_refy
  attributes(df) <- c(attributes(df),
                      attributes(df_refy)[-which(names(attributes(df_refy)) %in%
                                                   c("dim",
                                                     "row.names",
                                                     "names",
                                                     "class",
                                                     ".internal.selfref",
                                                     names(attributes(df))))])


  # Add columns to attributes
  df <- vars_to_attr(df, "n_imp")
  attr(df,
       "survey_year_rows")    <- survey_year_rows
  df <- df |>
    vars_to_attr(var = c("country_code",
                         "survey_acronym",
                         "survey_year",
                         "reporting_year"))
  attr(df,
       "reporting_level_rows") <- reporting_level_rows

  # rm cols
  gv(df,
     c("svy_pop",
       "relative_distance",
       "reporting_pop",
       "surveyid_year",
       "mult_factor",
       "welfare_ppp",
       "reporting_level")) <- NULL

  df

}

#
#
#
#
# get_dist_stats <- function(df) {
#
#   # min
#   min <- fmin(df$welfare,
#               g = df$reporting_level) |>
#     as.list()
#
#   # max
#   max <- fmax(df$welfare,
#               g = df$reporting_level) |>
#     as.list()
#
#   # mean
#   mean <- fmean(x = df$welfare,
#                 w = df$weight,
#                 g = df$reporting_level) |>
#     as.list()
#
#   # median
#   median <- fmedian(x = df$welfare,
#                     w = df$weight,
#                     g = df$reporting_level) |>
#     as.list()
#
#   # gini
#   gini <- sapply(df$reporting_level |> funique(),
#                  FUN = \(x) {
#                    wbpip::md_compute_gini(welfare = df$welfare[df$reporting_level == x],
#                                           weight  = df$weight[df$reporting_level == x])
#                  }) |>
#     as.list()
#
#   # mld
#   mld <- sapply(df$reporting_level |> funique(),
#                 FUN = \(x) {
#                   wbpip::md_compute_mld(welfare = df$welfare[df$reporting_level == x],
#                                         weight  = df$weight[df$reporting_level == x],
#                                         mean    = mean$x)
#                 }) |>
#     as.list()
#
#   # polarization
#   pol <- sapply(df$reporting_level |> funique(),
#                 FUN = \(x) {
#                   wbpip::md_compute_polarization(welfare = df$welfare[df$reporting_level == x],
#                                                  weight  = df$weight[df$reporting_level == x],
#                                                  gini    = gini[[x]],
#                                                  mean    = mean[[x]],
#                                                  median  = median[[x]])
#                 }) |>
#     as.list()
#
#   # results
#   dist_stats <- list(min          = min,
#                      max          = max,
#                      mean         = mean,
#                      median       = median,
#                      gini         = gini,
#                      mld          = mld,
#                      polarization = pol)
#   dist_stats
#
#   # data.table
#   # rplev <- funique(df$reporting_level)
#   # cc    <- funique(df$country_code)
#   # ry    <- funique(df$reporting_year)
#   # dt_dist <- data.table(country_code    = cc,
#   #                       reporting_year  = ry,
#   #                       reporting_level = rplev)
#   # print(dt_dist)
#   # print(dist_stats$polarization |>
#   #         qDT())# |>
#   #         #pivot()) #|>
#   #         # fmutate(reporting_level = as.character(variable),
#   #         #         polarization    = value) |>
#   #         # fselect(reporting_level,
#   #         #         polarization))
#   # dt_dist <- joyn::left_join(dt_dist,
#   #                            dist_stats$polarization |>
#   #                              qDT() |>
#   #                              pivot() |>
#   #                              fmutate(reporting_level = as.character(variable),
#   #                                      polarization    = value) |>
#   #                              fselect(reporting_level,
#   #                                      polarization),
#   #                            by           = c("reporting_level"),
#   #                            relationship = "one-to-one",
#   #                            reportvar    = F) |>
#   #   joyn::left_join(dist_stats$mean |>
#   #                     qDT() |>
#   #                     pivot() |>
#   #                     fmutate(reporting_level = as.character(variable),
#   #                                      mean    = value) |>
#   #                     fselect(reporting_level,
#   #                             mean),
#   #                   by           = c("reporting_level"),
#   #                   relationship = "one-to-one",
#   #                   reportvar    = F) |>
#   #   joyn::left_join(dist_stats$median |>
#   #                              qDT() |>
#   #                              pivot() |>
#   #                              fmutate(reporting_level = as.character(variable),
#   #                                      median    = value) |>
#   #                              fselect(reporting_level,
#   #                                      median),
#   #                    by           = c("reporting_level"),
#   #                    relationship = "one-to-one",
#   #                    reportvar    = F) |>
#   #   joyn::left_join( dist_stats$gini |>
#   #                              qDT() |>
#   #                              pivot() |>
#   #                              fmutate(reporting_level = as.character(variable),
#   #                                      gini    = value) |>
#   #                              fselect(reporting_level,
#   #                                      gini),
#   #                    by           = c("reporting_level"),
#   #                    relationship = "one-to-one",
#   #                    reportvar    = F) |>
#   #   joyn::left_join(dist_stats$mld |>
#   #                              qDT() |>
#   #                              pivot() |>
#   #                              fmutate(reporting_level = as.character(variable),
#   #                                      mld    = value) |>
#   #                              fselect(reporting_level,
#   #                                      mld),
#   #                   by           = c("reporting_level"),
#   #                   relationship = "one-to-one",
#   #                   reportvar    = F)
#   #
#   #
#   # list(dist_stats,
#   #      dt_dist)
#
# }


#' Distribution statistics of lineup distribution
#'
#' @param df data frame: output of [get_refy_distributions]
#'
#' @return list with:
#' - dist_stats: original list of distributional stats
#' - dt_stats: a data.table with flattened statistics
#' @export
get_dist_stats <- function(df) {

  # Ensure necessary columns exist
  if (!all(c("reporting_level", "welfare", "weight", "country_code", "reporting_year") %in% names(df))) {
    stop("df must contain columns: reporting_level, welfare, weight, country_code, reporting_year")
  }

  # Extract grouping levels
  levels <- funique(df$reporting_level)

  # === Compute distributional stats ===
  min    <- fmin(df$welfare,
                 g = df$reporting_level) |>
    as.list()
  max    <- fmax(df$welfare,
                 g = df$reporting_level) |>
    as.list()
  mean   <- fmean(df$welfare,
                  w = df$weight,
                  g = df$reporting_level) |>
    as.list()
  median <- fmedian(df$welfare,
                    w = df$weight,
                    g = df$reporting_level) |>
    as.list()

  # Gini
  gini <- sapply(levels, \(x) {
    wbpip::md_compute_gini(
      welfare = df$welfare[df$reporting_level == x],
      weight  = df$weight[df$reporting_level == x]
    )
  }) |>
    as.list()

  # MLD (requires mean)
  mld <- sapply(levels, \(x) {
    wbpip::md_compute_mld(
      welfare = df$welfare[df$reporting_level == x],
      weight  = df$weight[df$reporting_level == x],
      mean    = mean[[x]]
    )
  }) |>
    as.list()

  # Polarization
  pol <- sapply(levels, \(x) {
    wbpip::md_compute_polarization(
      welfare = df$welfare[df$reporting_level == x],
      weight  = df$weight[df$reporting_level == x],
      gini    = gini[[x]],
      mean    = mean[[x]],
      median  = median[[x]]
    )}) |>
    as.list()

  # === Output: original list ===
  dist_stats <- list(
    min          = min,
    max          = max,
    mean         = mean,
    median       = median,
    gini         = gini,
    mld          = mld,
    polarization = pol)

  # === Output: data.table version ===
  country_code   <- unique(df$country_code)
  reporting_year <- unique(df$reporting_year)

  dt_dist <- data.table(
    country_code    = country_code,
    reporting_year  = reporting_year,
    reporting_level = names(mean),
    min             = unlist(min),
    max             = unlist(max),
    mean            = unlist(mean),
    median          = unlist(median),
    gini            = unlist(gini),
    mld             = unlist(mld),
    polarization    = unlist(pol))

  # === Return both ===
  list(
    dist_stats = dist_stats,
    dt_dist   = dt_dist)

}












#' Add auxiliary data as attribute to estimated lineup data
#'
#' @param df data frame of estimated lineups - output of [get_refy_distributions]
#' @inheritParams aux_data
#'
#' @return data frame with added attributes
#' @export
add_aux_data_attr <- function(df,
                              dl_aux,
                              df_refy,
                              py = 2021) {

  code <- attr(x = df,
               which = "country_code")
  year <- attr(x = df,
               which = "reporting_year")
  reporting_level <- attr(x     = df,
                          which = "reporting_level_rows")$reporting_level |>
    funique()

  aux_data_list <- aux_data(cde             = code,
                            yr              = year,
                            reporting_level = reporting_level,
                            dl_aux          = dl_aux,
                            df_refy         = df_refy,
                            py              = py)

  attr(df,
       "aux_data") <- aux_data_list

  df
}




#' Prepare auxiliary data for attributes to be extracted for country-reference-year
#'
#' @param cde character: country code
#' @param yr numeric: reference year
#' @param reporting_level character: national, rural, urban
#' @param dl_aux list: auxiliary data
#' @param df_refy data frame: reference year data
#' @param py numeric: PPP year. Default is 2017
#'
#' @return list
#' @export
aux_data <- function(cde,
                     yr,
                     reporting_level,
                     dl_aux,
                     df_refy,
                     py = 2021) {

  if (length(yr) > 1) cli::cli_alert_warning("reporting year non-unique")
  if (length(cde) > 1) cli::cli_alert_warning("country code non-unique")
  if (length(reporting_level) > 1) cli::cli_alert_warning("reporting level non-unique")

  stopifnot(py %in% c(2011, 2017, 2021))
  output <- list()
  yr     <- as.character(yr)

  # PCE
  output[["pce"]] <-
    dl_aux$pce[country_code == cde,
               ..yr]  |>
    unlist() |>
    unname() |>
    funique()

  # POP
  tempvs  <- c("data_level",
               yr)
  temp    <- dl_aux$pop[country_code == cde,
                        ..tempvs]
  result <- setNames(as.list(temp[[yr]]),
                     temp$data_level)
  output[["pop"]] <-
    result

  # GDP
  output[["gdp"]] <-
    dl_aux$gdp[country_code == cde &
               data_level   %in% reporting_level,
               ..yr] |>
    funique() |>
    as.numeric()

  # PPP
  tempvs   <- c("data_level", paste(py))
  temp    <- dl_aux$ppp[country_code == cde,
                        ..tempvs]
  result <- setNames(as.list(temp[[tempvs[2]]]),
                     temp$data_level)
  output[["ppp"]] <-
    result

  # CPI
  tempvs  <- c("data_level",
               yr)
  temp    <- dl_aux$cpi[country_code == cde,
                        ..tempvs]
  result <- setNames(as.list(temp[[yr]]),
                     temp$data_level)
  output[["cpi"]] <-
    result

  # return
  output

}



#' Get multiplication factor and add to refy data frame
#'
#' Multiplication factor
#'
#' @param df_refy data frame: reference year (refy) table with estimation type, monotonicity, national accounts growth rates, means, etc. already calculated for each country ref-year
#'
#' @return data frame: refy table with a mult_factor column
#' @export
get_refy_mult_factor <- function(df_refy) {

  df_refy |>
    fmutate(
      lineup_approach = fcase(
        estimation_type == "extrapolation" ,
        "extrapolation",
        estimation_type == "interpolation" & monotonic == TRUE & same_direction == TRUE, "interpolation_same",
        estimation_type == "interpolation" & !(monotonic == TRUE & same_direction == TRUE),
        "interpolation_diverge",
        default = "survey"
      ),
      mult_factor = fcase(
        lineup_approach == "extrapolation" | lineup_approach == "interpolation_diverge",
        nac / nac_sy,
        lineup_approach == "interpolation_same",
        predicted_mean_ppp / svy_mean,
        default = 1
      )
    )

}

