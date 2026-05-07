#' Get Country Price framework  data based on PFW and DLW data info
#'
#' @param dt data frame with micro data, loaded with `pipload::pip_load_dlw()`
#' @param pfw data frame with Price framework data, loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return list of data.tables
#' @export
#'
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' pfw <- pipload::pip_load_aux("pfw")
#' gd   <- pipload::pip_load_dlw("PHL", 2012)
#' gd  <- survey_id_to_attr(gd, unique(gd$survey_id))
#' cpfw <- get_country_pfw(gd, pfw)
#' }

# Domain columns used by report_lvl() to compute per-row reporting_level.
# reporting_level = max across all domain columns:
#   "1" = national (all domains == 1)
#   "2" = subnational (at least one domain == 2, e.g. cpi_domain == 2
#         when urban/rural CPI values are available)
# Tracked for migration to sysdata.rda (see roadmap: sysdata-domain-cols).
.DOMAIN_COLS <- c(
  "cpi_domain",
  "ppp_domain",
  "gdp_domain",
  "pce_domain",
  "pop_domain"
)
get_country_pfw <- function(dt, pfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Filter country PFW --------

  att <- attributes(dt)

  cpfw <- pfw[ country_code     == att$country_code
               & surveyid_year  == att$surveyid_year
               & survey_acronym == att$survey_acronym]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add reporting level  --------

  cpfw <- report_lvl(cpfw)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create cache ID   ---------

  cpfw <- cache_id(cpfw =  cpfw,
                   att = att)

  # Return -------------
  return(cpfw)

}

#' Add reporting level variable to country PFW
#'
#' Computes `reporting_level` as the per-row maximum across the five domain
#' columns (CPI, PPP, GDP, PCE, pop). Values are stored as character:
#' `"1"` = national (all domains are national); `"2"` = subnational (at least
#' one domain, e.g. `cpi_domain`, is 2 meaning urban/rural-specific data are
#' available). This value is later read by `add_main_att()` as the integer
#' `reporting_level` attribute on the survey `data.table`.
#'
#' @param cpfw data.table with country Price Framework containing the five
#'   `*_domain` columns (see `.DOMAIN_COLS`) and an `inpovcal` indicator.
#'
#' @return `cpfw` with a new `reporting_level` character column (`"1"` or
#'   `"2"`) added by reference. Rows with `inpovcal != 1` are dropped.
#' @keywords internal
report_lvl <- function(cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # computations   ---------
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      dcols <- .DOMAIN_COLS

      missing_dcols <- setdiff(dcols, names(cpfw))
      if (length(missing_dcols) > 0L) {
        cli::cli_abort(
          "PFW is missing expected domain columns: {.field {missing_dcols}}.",
          class = c("report_lvl", "piperr")
        )
      }

      # Filter to inpovcal rows first so nrow() checks below reflect the
      # actual number of usable PFW observations.
      cpfw <- cpfw[inpovcal == 1]

      # do.call(pmax, .SD) computes per-row max across domain columns in a
      # single vectorised C call — avoids apply(MARGIN=1) R-level loop.
      cpfw[,
        reporting_level := as.character(do.call(pmax, .SD)),
        .SDcols = dcols
      ]

      n_cpfw_wt <- length(unique(cpfw$welfare_type))

      if (nrow(cpfw) == 0) {
        cli::cli_abort(
          "PFW does not contain info for country, surveyid year, and survey_acronym.",
          class = c("piperr", "info_pfw")
        )
      } else if (nrow(cpfw) > 1 & n_cpfw_wt == 1) {
        cli::cli_abort(
          "PFW is not unique for country, surveyid year, survey_acronym and welfare_type.",
          class = c("piperr", "no_unq_pfw")
        )
      } else if (nrow(cpfw) > 1) {
        survey_id <- c(pd_env_get("process_survey_id"))

        pipfun::log_add(
          event = "info",
          message = "More than one value for country/year PFW",
          name = "pipdata_log",
          logmeta = list(info = "othr_wlf_inf", survey = survey_id)
        )
      }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(cpfw)

}


#' Duplicate country PFW if there are two types of welfare
#'
#' @param cpfw country PFW data.table
#' @param log_wrn boolean value for logging warning in log.txt
#'
#' @return data.table
#' @keywords internal
# othr_wlf <- function(cpfw) {
#
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # computations   ---------
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#       cpfw[,
#            is_alt_welf := FALSE
#       ]
#
#       if (cpfw$oth_welfare1_type != "") {
#
#         cpfw_alt <- copy(cpfw)
#
#         cpfw_alt[
#           ,
#           welfare_type := fcase(
#             grepl("^([Cc])", oth_welfare1_type), "consumption",
#             grepl("^([Ii])", oth_welfare1_type), "income",
#             default = ""
#           )
#         ][
#           ,
#           oth_welfare1_type := NULL # remove variable
#         ][
#           ,
#           is_alt_welf := TRUE
#         ]
#
#
#         cpfw <- rbindlist(l         =  list(cpfw, cpfw_alt),
#                           use.names = TRUE,
#                           fill      = TRUE)
#
#       }
#
#       if(nrow(cpfw)>1){
#
#         rlang::abort(message = "More than one type of welfare",
#                      class = c("piperr", "othr_wlf_inf"),
#                      use_cli_format = TRUE)
#       }
#
#
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # Return   ---------
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   return(cpfw)
#
# }

#' Create cache ID for country PFW
#'
#' Constructs a `cache_id` string of the form
#' `{country_code}_{surveyid_year}_{survey_acronym}_{INC|CON}_{module}`
#' and adds it as a column to `cpfw`. The table is then split by `cache_id`
#' and returned as a named list — one element per welfare type.
#'
#' @param cpfw data.table with country Price Framework, containing at minimum
#'   `welfare_type` and `reporting_level` columns.
#' @param att Named list of survey attributes (from `attributes(dt)`).
#'   Must contain: `country_code`, `surveyid_year`, `survey_acronym`, `module`.
#'
#' @return Named list of data.tables, one element per unique `cache_id`.
#' @keywords internal
cache_id <- function(att,
                     cpfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cpfw[
    ,
    wt := fcase(
      welfare_type == "income", "INC",
      welfare_type == "consumption", "CON",
      default = ""
    )
  ][
    ,
    cache_id := paste(att$country_code,
                      att$surveyid_year,
                      att$survey_acronym,
                      # paste0("D", reporting_level),
                      wt,
                      att$module,
                      sep = "_"
    )
  ]

  if(any(cpfw$wt=="")){

    rlang::abort(message = "Welfare type is undefined",
                 class = c("piperr", "no_wlf_tp"),
                 use_cli_format = TRUE)

  }

  cpfw[,
    wt := NULL
  ]

  cpfw <- split(cpfw, by = "cache_id")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(cpfw)

}
