#' Clean data from datalibweb structure (High level)
#'
#' @param lf List of data frames or single dataframe. In pipeline, the data
#'   frames come from `pd_split_alt_welfare()`
#' @param cpfw data frame with Price framework data for country/survey in `df`.
#'   It is loaded with `get_country_pfw(df, pfw)`. `pfw` is loaded in
#'   `pipload::pip_load_aux("pfw")`
#'
#' @return
#' @export
#'
#' @examples
#' pfw  <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' lf   <- pd_split_alt_welfare(md, cpfw)
#' l    <- pd_dlw_clean(lf, cpfw)
#' names(l)
pd_dlw_clean <- function(lf, cpfw) {

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot( exprs = {

    ## check that both are lists
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
                           .f = dlw_clean)
        } else {
          y <- dlw_clean(lf, cpfw[[1]])
          y <- list(y)
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


#' Clean data from datalibweb structure (lower level, S2 methods)
#'
#' PD: process data. Source: datalibweb. Action: Clean
#'
#' @param df dataframe loaded with `pipload::pip_load_dlw()`
#' @param ...  other parameters
#'
#' @return data.table
#' @export
#'
#' @examples
#' gd  <- pipload::pip_load_dlw("CHN", 2015)
#' pfw <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(gd, pfw)
#' dlw_clean(gd, cpfw[[1]])
#'
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' dlw_clean(md, cpfw[[1]])
dlw_clean <- function(df,...) {
  UseMethod("dlw_clean")
}

#' Clean micro data from Datalibweb original file
#'
#' @param df data frame with micro data, loaded with `pipload::pip_load_dlw()`
#' @param cpfw data frame with Price framework data for country/survey in `df`.
#'   It is loaded with `get_country_pfw(df, pfw)`. `pfw` is loaded in
#'   `pipload::pip_load_aux("pfw")`
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' dlw_clean(md, cpfw[[1]])
dlw_clean.pipmd <- function(df, cpfw, ...) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initial formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # hard copy
  md <- copy(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## process dlw data --------

  ##
  ## clean weight variable
  variables <- colnames(md)

  if (!c("weight") %in% variables) {
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


  ### --------- RECODE VARIABLES ------------ ###
  ## Education
  # educat4
  if (c("educat4") %in% variables){

    setnames(md, old = "educat4", new = "educat4_2")
    md[, educat4 := NA_character_]
    md[educat4_2 == 1, educat4 := "No education"]
    md[educat4_2 == 2, educat4 := "Primary"]
    md[educat4_2 == 3, educat4 := "Secondary"]
    md[educat4_2 == 4, educat4 := "Tertiary"]

    md[, educat4_2:= NULL]
  }

  # educat5
  if (c("educat5") %in% variables){

    setnames(md, old = "educat5", new = "educat5_2")
    md[, educat5 := NA_character_]

    md[educat5_2 == 1, educat5 := "No education"]
    md[educat5_2 == 2, educat5 := "Primary incomplete"]
    md[educat5_2 == 3, educat5 := "Primary complete but secondary incomplete"]
    md[educat5_2 == 4, educat5 := "Secondary complete"]
    md[educat5_2 == 5, educat5 := "Some tertiary/post-secondary"]
    md[, educat5_2:= NULL]
  }

  # literacy
  if (c("literacy") %in% variables) {
    setnames(md, "literacy", "literacy2")
    md[, literacy := NA_character_]
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
    md[, urban := NA_character_]
    md[urban2 == 1, urban := "urban"]
    md[urban2 == 0, urban := "rural"]
    md[, urban2 := NULL]
  }

  # Recode male to string
  if (c("male") %in% variables){

    setnames(md, "male", "male2")
    md[, male := NA_character_]
    md[male2 == 1, male := "male"]
    md[male2 == 0, male := "female"]
    md[, male2 := NULL]

  }

  # Add variables  from PFW data
  if (!c("survey_year") %in% variables) {
    md[, survey_year := cpfw$survey_year]
  }

  # generate countrycode variable if not available in md data
  if (!c("countrycode") %in% variables){
    md[, countrycode := cpfw$country_code]
  }

  # Create welfare_type
  if (!c("welfare_type") %in% variables){
    md[, welfare_type := cpfw$welfare_type]
  }

  # Create distribution_type
  if (cpfw$use_imputed == 1) {

    md[, distribution_type := "imputed"]

  }else {

    md[, distribution_type := "micro"]

  }

  # Create ppp_data_level
  if (c("ppp_data_level") %in% variables) {
    md[, ppp_data_level := NULL]
  }

  if (cpfw$ppp_domain == 1){
    md[, ppp_data_level := "national"]
  }

  if (cpfw$ppp_domain == 2) {

    md[, ppp_data_level := urban]

  }

  # Create cpi_data_level
  if (c("cpi_data_level") %in% variables) {
    md[, cpi_data_level := NULL]
  }
  if (cpfw$cpi_domain == 1) {

    md[, cpi_data_level := "national"]

  }
  if (cpfw$cpi_domain == 2) {

    md[, cpi_data_level := urban]
  }

  # Create gdp_data_level
  if (cpfw$gdp_domain == 1) {
    md[, gdp_data_level := "national"]
  }
  if (cpfw$gdp_domain == 2) {

    md[, gdp_data_level := urban]

  }

  # Create pce_data_level
  if (c("pce_domain") %in% variables) {
    md[, pce_data_level := NULL]
  }
  if (cpfw$pce_domain == 1) {

    md[, pce_data_level := "national"]

  }
  if (cpfw$pce_domain == 2) {

    md[, pce_data_level := urban]

  }

  # Create pop_data_level
  if (c("pop_domain") %in% variables) {

    md[, pop_data_level := NULL]

  }
  if (cpfw$pop_domain == 1) {
    md[, pop_data_level := "national"]
  }
  if (cpfw$pop_domain == 2) {

    md[, pop_data_level := urban]

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create variables that do not exist   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # get from internal data `pip_var_type`
  pip_vars  <- pip_var_type$pip_vars_pc
  pip_type  <- pip_var_type$pip_vars_pc_class

  miss_ind  <- !(pip_vars %in% names(md))
  miss_vars <- pip_vars[miss_ind]
  miss_type <- pip_type[miss_ind]

  miss_type <- glue("as.{miss_type}")

  md[,
     (miss_vars) := lapply(miss_type, \(x) get(x)())]

  # order columns in correct order
  setcolorder(md, pip_vars)
  md <- md[, .SD, .SDcols = pip_vars]

  # Sort by country_code, surveyid_year and welfare
  sortbycol <- c("country_code", "surveyid_year", "welfare", "hhid" ,"pid")
  setorderv(md, sortbycol)
  return(md)
}


#' Clean group data from Datalibweb original file
#'
#' @param df data frame with group data, loaded with `pipload::pip_load_dlw()`
#' @param cpfw data frame with Price framework data for country/survey in `df`.
#'   It is loaded with `get_country_pfw(df, pfw)`. `pfw` is loaded in
#'   `pipload::pip_load_aux("pfw")`
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' dlw_clean(gd, cpfw[[1]])
dlw_clean.pipgd <- function(df, cpfw, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initial formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # hard copy
  gd <- copy(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Rename variables --------

  #--- Is this necessary?
  gd[, survey_year := cpfw$survey_year]

  gd[, area := fcase(urban == 1, "urban",
                     urban == 0, "rural",
                     is.na(urban), "national",
                     default = "")]

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
         y <- "group"

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
