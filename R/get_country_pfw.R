#' GEt Country Price framework  data based on PFW and DLW data info
#'
#' @param df data frame with micro data, loaded with `pipload::pip_load_dlw()`
#' @param pfw data frame with Price framework data, loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
get_country_pfw <- function(df, pfw) {

  # on.exit ------------
  on.exit({

  })


  # Defenses -----------
  keyVar <- c("country_code", "surveyid_year", "survey_acronym")
  stopifnot( exprs = {
    # 1) Check for duplicates
    uniqueN(pfw, by = keyVar) == nrow(pfw)
    }
  )

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------
  # subset microdata survey; BIN is BIN is treated as microdata in PCN/PIP
  # pfw <- pfw[use_microdata == 1 |
  #              use_bin     == 1 |
  #              use_imputed == 1 |
  #              inpovcal     == 1] # subset country-years in Povcalnet

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## unique variables --------

  # get single-value variables
  uvl <- uniq_vars_to_list(df)  #list with unique value

  # filter PFW

  cpfw <- # country price framework
    pfw[ country_code     == uvl$country_code
         & surveyid_year  == uvl$surveyid_year
         & survey_acronym == uvl$survey_acronym
    ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## reporting level  --------
  dcols <- c(
    "cpi_domain",
    "ppp_domain",
    "gdp_domain",
    "pce_domain",
    "pop_domain"
  )

  cpfw <-
    cpfw[
      # filter inpovcal data
      inpovcal == 1
    ][,
      # Find MAX domain per obs
      reporting_level := apply(.SD, MARGIN = 1,
                               function(x) {
                                 y <- max(x)
                                 as.character(y)
                               }),
      .SDcols = dcols
    ]




  # check if there is a unique record for country, survey ID year and survey_acronym
  stopifnot(exprs =  {
    "PFW is not unique for country, surveyid year, and survey_acronym" = nrow(cpfw) == 1
    "PFW does not contains info for country, surveyid year, and survey_acronym" = nrow(cpfw) != 0
  })



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Other welfare type --------

  cpfw[,
       is_alt_welf := FALSE
  ]
  if (cpfw$oth_welfare1_type != "") {

    cpfw_alt <- copy(cpfw)
    cpfw_alt[
      ,
      welfare_type := fcase(
        grepl("^([Cc])", oth_welfare1_type), "consumption",
        grepl("^([Ii])", oth_welfare1_type), "income",
        default = ""
      )
    ][
      ,
      oth_welfare1_type := NULL # remove variable
    ][
      ,
      is_alt_welf := TRUE
    ]


    cpfw <- rbindlist(l         =  list(cpfw, cpfw_alt),
                      use.names = TRUE,
                      fill      = TRUE)

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Cache ID   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cpfw[
    ,
    wt := fcase(
      welfare_type == "income", "INC",
      welfare_type == "consumption", "CON",
      default = ""
    )
  ][
    ,
    cache_id := paste(country_code,
                      surveyid_year,
                      survey_acronym,
                      paste0("D", reporting_level),
                      wt,
                      uvl$module,
                      sep = "_"
    )
  ]

  cpfw <- as.list(cpfw)

  # Return -------------
  return(cpfw)

}
