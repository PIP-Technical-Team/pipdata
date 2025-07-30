
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
            "income_group_code",
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
            "pcn_region_code",
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
      "income_group_code",
      #"survey_year",
      "reporting_year",
      "survey_id",
      #"cache_id",
      "wb_region_code",
      "pcn_region_code",
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
            weight_refy = weight * (reporting_pop / svy_pop) * # "adjust to WDI population" --> Andres, your comment
              relative_distance#,
            # ref year weights divided by number of imputations
            #      this should sum to population amount
            #weight_refy_adj = weight_refy / n_imp
    ) |>
    fungroup() |>
    fmutate(welfare_refy = welfare_ppp * mult_factor)

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

  df <- vars_to_attr(df, "n_imp")
  attr(df,
       "survey_year_rows")    <- survey_year_rows



  df <- df |>
    vars_to_attr(var = c("country_code",
                         "survey_acronym",
                         "survey_year"))

  dist_stats <- get_dist_stats(df = df)
  attr(df,
       "dist_stats") <- dist_stats

  attr(df,
       "reporting_level_rows") <- reporting_level_rows


  gv(df,
     c("svy_pop",
       "relative_distance",
       "reporting_pop",
       "surveyid_year",
       "mult_factor",
       "welfare_ppp",
       "weight",
       "reporting_level")) <- NULL

  df

}




#' Distribution statistics of lineup distribution
#'
#' @param df data frame: output of [get_refy_distributions]
#'
#' @return list with distributional statistics
#' @export
get_dist_stats <- function(df) {

  # min
  min <- fmin(df$welfare_refy,
              g = df$reporting_level) |>
    as.list()

  # max
  max <- fmax(df$welfare_refy,
              g = df$reporting_level) |>
    as.list()

  # mean
  mean <- fmean(x = df$welfare_refy,
                w = df$weight_refy,
                g = df$reporting_level) |>
    as.list()

  # median
  median <- fmedian(x = df$welfare_refy,
                    w = df$weight_refy,
                    g = df$reporting_level) |>
    as.list()

  # gini
  gini <- sapply(df$reporting_level |> funique(),
                 FUN = \(x) {
                   wbpip::md_compute_gini(welfare = df$welfare_refy[df$reporting_level == x],
                                          weight  = df$weight_refy[df$reporting_level == x])
                 }) |>
    as.list()

  # mld
  mld <- sapply(df$reporting_level |> funique(),
                FUN = \(x) {
                  wbpip::md_compute_mld(welfare = df$welfare_refy[df$reporting_level == x],
                                        weight  = df$weight_refy[df$reporting_level == x],
                                        mean    = mean$x)
                }) |>
    as.list()

  # polarization
  pol <- sapply(df$reporting_level |> funique(),
                FUN = \(x) {
                  wbpip::md_compute_polarization(welfare = df$welfare_refy[df$reporting_level == x],
                                                 weight  = df$weight_refy[df$reporting_level == x],
                                                 gini    = gini[[x]],
                                                 mean    = mean[[x]],
                                                 median  = median[[x]])
                }) |>
    as.list()

  # results
  dist_stats <- list(min          = min,
                     max          = max,
                     mean         = mean,
                     median       = median,
                     gini         = gini,
                     mld          = mld,
                     polarization = pol)

  dist_stats

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
                              filter_aux_data = FALSE) {

  code <- attr(x = df,
               which = "country_code")
  year <- attr(x = df,
               which = "reporting_year")
  reporting_level <- attr(x = df,
                          which = "reporting_level_rows")$reporting_level

  aux_data_list <- aux_data(cde             = code,
                            yr              = year,
                            reporting_level = reporting_level,
                            dl_aux          = dl_aux,
                            all             = TRUE,
                            filter_aux_data = filter_aux_data,
                            py              = 2017)

  aux_data_list <-
    c(aux_data_list,
      aux_data(cde             = code,
               yr              = year,
               reporting_level = reporting_level,
               dl_aux          = dl_aux,
               all             = FALSE,
               filter_aux_data = filter_aux_data,
               py              = 2011))

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
#' @param all logical: whether all aux data should be included. Default is TRUE. If FALSE, then GDP, PCE, and POP are excluded
#' @param filter_aux_data logical: should the aux data list be filtered or not? Default is FALSE, which is means data has been pre-filtered to make appropriate selections.
#' @param py numeric: PPP year. Default is 2017
#'
#' @return list
#' @export
aux_data <- function(cde,
                     yr,
                     reporting_level,
                     dl_aux,
                     df_refy,
                     all = TRUE,
                     filter_aux_data = FALSE,
                     py = 2017) {

  stopifnot(py %in% c(2011, 2017))
  output <- list()

  if (filter_aux_data) {
    vars     <- c("ppp_year", "release_version", "adaptation_version")
    ppp_v    <- unique(dl_aux$ppp[, ..vars], by = vars)
    data.table::setnames(x = ppp_v,
                         old = c("release_version", "adaptation_version"),
                         new = c("ppp_rv", "ppp_av"))

    # max release version
    m_rv <- ppp_v[ppp_year == py, max(ppp_rv)]

    # max adaptation year
    m_av <- ppp_v[ppp_year == py & ppp_rv == m_rv,
                  max(ppp_av)]


    ppp <- dl_aux$ppp[ppp_year == py
                      & release_version    == m_rv
                      & adaptation_version == m_av
    ][,
      ppp_default := TRUE] |>
      copy()
  } else {
    ppp <- dl_aux$ppp
  }

  # CPI
  reporting_level <- as.character(reporting_level)
  cpi <-
    sapply(reporting_level,
           \(x){
             p <- dl_aux$cpi |>
               fsubset(country_code == cde &
                         cpi_year   == yr &
                         cpi_data_level %in% c(x),
                       get(paste0("cpi", py))) |>
               as.numeric()
           },
           simplify  = FALSE,
           USE.NAMES = TRUE)

  output[[paste0("cpi", py)]] <- cpi

  # PPP
  output[[paste0("ppp", py)]] <-
    sapply(reporting_level,
           \(x){
             p <- ppp |>
               fsubset(country_code == cde &
                         ppp_data_level %in% c(x),
                       ppp) |>
               as.numeric()
           },
           simplify  = FALSE,
           USE.NAMES = TRUE)

  # GDP
  if (isTRUE(all)) {
    output[["gdp"]] <-
      dl_aux$gdp |>
      fsubset(country_code     == cde &
                year           == yr &
                gdp_data_level %in% reporting_level,
              gdp) |>
      funique() |>
      as.numeric()


    # GDM - group data

    # CP

    # NPL

    # PCE
    output[["pce"]] <-
      dl_aux$pce |>
      fsubset(country_code == cde &
                year       == yr,
              pce) |>
      funique() |>
      as.numeric()

    # POP
    output[["pop"]] <-
      sapply(reporting_level,
             \(x){
               p <- dl_aux$pop  |>
                 fsubset(country_code == cde &
                           year       == yr &
                           pop_data_level %in% c(x),
                         pop) |>
                 as.numeric()
               p
             },
             simplify  = FALSE,
             USE.NAMES = TRUE)
  }

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

