#' PD: process data. Source: datalibweb. Action: Clean
#'
#' Clean data from datalibweb structure
#'
#' @inheritParams pd_dlw
#' @param ... Other parameters
#'
#' @return data.table
#' @export
#'
#' @examples
#' x   <- pipload::pip_load_dlw("CHN", 2015)
#' pfw <- pipload::pip_load_aux("pfw")
#' pd_dlw_clean(x, pfw)
pd_dlw_clean <- function(df,...) {
  UseMethod("pd_dlw_clean")
}

#' Clean micro data from Datalibweb original file
#'
#' @param df data frame with group data, loaded with `pipload::pip_load_dlw()`
#' @param pfw data frame with Price framework data, loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return data.table
#' @export
#'
#' @examples
#' x   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' pfw <- pipload::pip_load_aux("pfw")
#' pd_dlw_clean(x, pfw)
pd_dlw_clean.pipmd <- function(df) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initial formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # hard copy
  md <- copy(df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## process PFQ data --------

  # 1) Check for duplicates
  keyVar <- c("country_code", "surveyid_year", "survey_acronym")
  stopifnot("PFW is not unique for country, surveyid year, and survey_acronym" =
              uniqueN(pfw, by = keyVar) == nrow(pfw))

  # subset microdata survey; BIN is BIN is treated as microdata in PCN/PIP
  pfw <- pfw[use_microdata == 1 |
               use_bin     == 1 |
               use_imputed == 1 |
               inpovcal     == 1] # subset country-years in Povcalnet

  # filter data by country, survey ID year and survey_acronym
  # get single-value variables
  uvl <- uniq_vars_to_list(md)  #list with unique value

  # filter PFW

  cpfw <- # country price framework
    pfw[ country_code     == uvl$country_code
         & surveyid_year  == uvl$surveyid_year
         & survey_acronym == uvl$survey_acronym
    ]

  cpfw <- as.list(cpfw)

  # check if there is a unique record for country, survey ID year and survey_acronym
  stopifnot("PFW is not unique for country, surveyid year, and survey_acronym" =
              nrow(cpfw) == 1)
  # check if there are no records
  stopifnot("PFW does not contains info for country, surveyid year, and survey_acronym" =
              nrow(cpfw) != 0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## process dlw data --------

  ##
  ## clean weight variable
  variables <- colnames(md)
  if (!c("weight") %in% variables){
    if (c("weight_p") %in% variables){
      setnames(md, old = "weight_p", new = "weight")
    }
    else if (c("weight_h") %in% variables){
      setnames(md, old = "weight_h", new = "weight")
    }
    else{
      md[, weight := 1 / .N]
      ## add message
    }
  }

  #### Make sure no information is lost
  # change class type of "welfare", "weight"
  varNames <- c("welfare", "weight")
  md[,(varNames):= lapply(.SD, as.double),
      .SDcols = varNames]
  md[, welfare := welfare / 365]

  # create alt_welfare variable
  pfw[oth_welfare1_type == "", oth_welfare1_type := NA]
  pfw[oth_welfare1_var == "", oth_welfare1_var := NA]
  oth_welfare1_type <- pfw[, oth_welfare1_type]
  oth_welfare1_var  <- pfw[, oth_welfare1_var]

  if (!is.na(oth_welfare1_var)){
    md[, alt_welfare := oth_welfare1_var /365]
  }else{
    md[, alt_welfare := NA]
  }

  ### --------- RECODE VARIABLES ------------ ###
  ## Education
  # educat4
  if (c("educat4") %in% variables){
    setnames(md, old = "educat4", new = "educat4_2")
    md[, educat4 := NA]
    #md[, as.character(educat4) := educat4]

    md[educat4_2 == 1, educat4 := "No education"]
    md[educat4_2 == 2, educat4 := "Primary"]
    md[educat4_2 == 3, educat4 := "Secondary"]
    md[educat4_2 == 4, educat4 := "Tertiary"]

    md[, educat4_2:= NULL]
  }

  # educat4
  if (c("educat5") %in% variables){
    setnames(md, old = "educat5", new = "educat5_2")
    md[, educat5 := NA]
    #md[, as.character(educat5) := educat5]

    md[educat5_2 == 1, educat5 := "No education"]
    md[educat5_2 == 2, educat5 := "Primary incomplete"]
    md[educat5_2 == 3, educat5 := "Primary complete but secondary incomplete"]
    md[educat5_2 == 4, educat5 := "Secondary complete"]
    md[educat5_2 == 5, educat5 := "Some tertiary/post-secondary"]
    md[, educat5_2:= NULL]
  }

  # literacy
  if (c("literacy") %in% variables){
    setnames(md, "literacy", "literacy2")
    md[, literacy := NA]
    #md[, as.character(literacy) := literacy]
    md[literacy2 ==1, literacy := "yes"]
    md[literacy2 ==0, literacy := "no"]
    md[, literacy2 := NULL]
  }

  ###################################################
  ## Variables needed for PIP
  # rename subnatid
  if (c("subnatid") %in% variables){
    setnames(md, "subnatid", "subnatid1")
  }

  # Recode urban to string
  if (c("urban") %in% variables){
    setnames(md, "urban", "urban2")
    md[, urban := NA]
    md[, urban := as.character(urban)]
    md[urban2 == 1, urban := "urban"]
    md[urban2 == 0, urban := "rural"]
    md[, urban2 := NULL]
  }

  # Recode male to string
  if (c("male") %in% variables){
    setnames(md, "male", "male2")
    md[, male := NA]
    md[, male := as.character(urban)]
    md[male2 == 1, male := "male"]
    md[male2 == 0, male := "female"]
    md[, male2 := NULL]
  }

  # Add variables  from PFW data
  if (!c("survey_year") %in% variables){
    md[, survey_year := pfw[, survey_year]]
  }

  # generate countrycode variable if not available in md data
  if (!c("countrycode") %in% variables){
    md[, countrycode := pfw[, country_code]]
  }

  # Create welfare_type
  if (!c("welfare_type") %in% variables){
    md[, welfare_type := pfw[, welfare_type]]
  }

  # Create distribution_type
  if (pfw[, use_imputed] == 1){
    md[, distribution_type := "imputed"]
  }else {
    md[, distribution_type := "micro"]
  }

  # Create ppp_data_level
  if (c("ppp_data_level") %in% variables){
    md[, ppp_data_level := NULL]
  }

  if (pfw[, ppp_domain] == 1){
    md[, ppp_data_level := "national"]
  }

  if (pfw[, ppp_domain] == 2){
    md[, ppp_data_level := urban]
  }

  # Create cpi_data_level
  if (c("cpi_data_level") %in% variables){
    md[, cpi_data_level := NULL]
  }
  if (pfw[, cpi_domain] == 1){
    md[, cpi_data_level := "national"]
  }
  if (pfw[, cpi_domain] == 2){
    md[, cpi_data_level := urban]
  }

  # Create gdp_data_level
  if (pfw[, gdp_domain] == 1){
    md[, gdp_data_level := "national"]
  }
  if (pfw[, gdp_domain] == 2){
    md[, gdp_data_level := urban]
  }

  # Create pce_data_level
  if (c("pce_domain") %in% variables){
    md[, pce_data_level := NULL]
  }
  if (pfw[, pce_domain] == 1){
    md[, pce_data_level := "national"]
  }
  if (pfw[, pce_domain] == 2){
    md[, pce_data_level := urban]
  }

  # Create pop_data_level
  if (c("pop_domain") %in% variables){
    md[, pop_data_level := NULL]
  }
  if (pfw[, pop_domain] == 1){
    md[, pop_data_level := "national"]
  }
  if (pfw[, pop_domain] == 2){
    md[, pop_data_level := urban]
  }

  ###---------------- Clean md data ---------------###
  ### load variable cleaning csv file
  varclean <- read.csv("md_pip_var_type.csv")
  md_var_names <- varclean[, c("md_var_names")]
  pip_var_names <- varclean[, c("pip_var_names")]
  pip_vars_class <- varclean[, c("pip_vars_class")]

  nVar <- length(pip_var_names)
  for (j in seq_along(1:nVar)){
    if (!c(pip_var_names[j]) %in% variables){
      if (!c(md_var_names[j]) %in% variables){
        md <- md[, pip_var_names[j] := NA]
      } else {
        setnames(md, md_var_names[j], pip_var_names[j])
      }
    }
  }

  ### Recode variable types
  coltostr <- varclean %>%
    filter(pip_vars_class == "string")
  coltostr <- coltostr$pip_var_names

  coltonum <- varclean %>%
    filter(pip_vars_class == "numeric")
  coltonum <- coltonum$pip_var_names

  # to string
  md <- md[,(coltostr):= lapply(.SD, as.character),
             .SDcols = coltostr]
  # to numeric
  md <- md[,(coltonum):= lapply(.SD, as.numeric),
             .SDcols = coltonum]
  # order columns in correct order
  setcolorder(md, pip_var_names)
  md <- md[, .SD, .SDcols = pip_var_names]
  # Sort by country_code, surveyid_year and welfare
  sortbycol <- c("country_code", "surveyid_year", "welfare", "hhid" ,"pid")
  setorderv(md, sortbycol)
}


#' Clean group data from Datalibweb original file
#'
#' @param df data frame with group data, loaded with `pipload::pip_load_dlw()`
#' @param pfw data frame with Price framework data, loaded with
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return data.table
#' @export
#'
#' @examples
#' x   <- pipload::pip_load_dlw("CHN", 2015)
#' pfw <- pipload::pip_load_aux("pfw")
#' pd_dlw_clean(x, pfw)
pd_dlw_clean.pipgd <- function(df, pfw) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initial formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # hard copy
  gd <- copy(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Rename variables --------
  gd[, area := fcase(urban == 1, "urban",
                     urban == 0, "rural",
                     is.na(urban), "national",
                     default = "")]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## unique variables --------

  # get single-value variables
  uvl <- uniq_vars_to_list(gd)  #list with unique value

  # filter PFW

  cpfw <- # country price framework
    pfw[ country_code     == uvl$country_code
         & surveyid_year  == uvl$surveyid_year
         & survey_acronym == uvl$survey_acronym
    ]

  cpfw <- as.list(cpfw)


  #--- Is this necessary?
  gd[, survey_year := cpfw$survey_year]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Format types --------

  string <- c("country_code", "survey_acronym", "area", "welfare_type", "gd_type")
  nume   <- c("surveyid_year", "survey_year", "weight", "welfare")

  gd[, (string) := lapply(.SD, as.character),
     .SDcols = string]

  gd[, (nume) := lapply(.SD, as.numeric),
     .SDcols = nume]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # data level vars   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (cpfw$ppp_domain == 1) {

    gd[, ppp_data_level := "national"]

  } else if (cpfw$ppp_domain == 2) {

    gd[, ppp_data_level := area]

  } else {

    gd[, ppp_data_level := as.character()]

  }


  pref             <- c("ppp", "cpi", "gdp", "pce", "pop")
  data_level_vars  <- glue("{pref}_data_level")
  domain_vars      <- glue("{pref}_domain")

  trows <- nrow(gd)

  gd[,
     (data_level_vars) :=
       lapply(domain_vars, \(x) {

         if (cpfw[[x]] == 1) {

           y <- rep("national", times = trows)

         } else if (cpfw[[x]] == 2) {
           y <-  area
         } else {
           y <- ""
         }
         y

       })
  ]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Distribution type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  gd[,
     distribution_type := {

       if (cpfw$pop_domain == 1) {

         # y <- rep("national", times = trows)
         Y <- "group"

       } else if (cpfw$pop_domain ==  2) {

         larea <- length(unique(area))

         if (larea %in% c(0, 1)) {
           y <- "group"
         } else {
           y <- "aggregate"
         }

       } else {
         y <- ""
       }
       y

     }
  ]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create variables that do not exist   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # get from internal data `pip_var_type`
  pip_vars  <- pip_var_type$pip_vars_pc
  pip_type  <- pip_var_type$pip_vars_pc_class

  miss_ind  <- !(pip_vars %in% names(gd))
  miss_vars <- pip_vars[miss_ind]
  miss_type <- pip_type[miss_ind]

  miss_type <- glue("as.{miss_type}")

  gd[,
     (miss_vars) := lapply(miss_type, \(x) get(x)())]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Order and format --------

  # select columns
  gd <- gd[,  .SD, .SDcols = pip_vars]


  # of variable (columns)
  setcolorder(gd, pip_vars)

  # sorting

  varsort <- c("country_code", "surveyid_year", "area", "welfare")
  setorderv(gd, varsort)

  return(gd)
}
