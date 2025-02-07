#' Clean data from datalibweb structure (High level)
#'
#' @param lf List of data frames or single dataframe.
#'
#' @return list with data.tables
#' @export
#'
#' @examples
#' pfw  <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' l    <- pd_cpfw_merge(md, cpfw)
#' lf    <- pd_dlw_clean(l)
#' names(lf)
pd_dlw_clean <- function(lf) {

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

    if (inherits(lf, "list")) {
      rl <- purrr::map(.x = lf,
                       #.y =  cpfw,
                       .f = dlw_clean)
    } else {
      #y <- dlw_clean(lf, cpfw[[1]])
      rl <- dlw_clean(lf)
      rl <- list(rl)
    }

    #names(y) <- sapply(cpfw, `[[`, "cache_id")


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
#' l    <- pd_cpfw_merge(gd, cpfw)
#' dlw_clean(l[[1]])
#'
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' l    <- pd_cpfw_merge(md, cpfw)
#' dlw_clean(l[[1]])
dlw_clean <- function(df,...) {
  UseMethod("dlw_clean")
}

#' Clean micro data from Datalibweb original file
#'
#' @param df data frame with micro data, loaded with `pipload::pip_load_dlw()`
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' l    <- pd_cpfw_merge(md, cpfw)
#' dlw_clean(l[[1]])
dlw_clean.pipmd <- function(df, ...) {

#   ____________________________________________________________________________
#   Initial formatting                                                      ####

  # hard copy
  md <- copy(df)

  ## clean weight variable
  md <- format_wgt(md)

  #### Make sure no information is lost
  # change class type of "welfare", "weight"
  varNames <- c("welfare", "weight")
  md[,(varNames):= lapply(.SD, as.double),
      .SDcols = varNames]
  md[, welfare := welfare / 365]


#   ____________________________________________________________________________
#   Recoding variables                                                      ####

##  ............................................................................
##  Education                                                               ####

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


##  ............................................................................
##  Geographical variables                                                  ####

  # rename subnatid
  if (c("subnatid") %in% variables){
    setnames(md, "subnatid", "subnatid1")
  }


##  ............................................................................
##  Other variables                                                         ####

  # Recode male to string
  if (c("male") %in% variables){

    setnames(md, "male", "male2")
    md[, male := NA_character_]
    md[male2 == 1, male := "male"]
    md[male2 == 0, male := "female"]
    md[, male2 := NULL]

  }

#   ____________________________________________________________________________
#   Variables that do not exist                                             ####

  # get from internal data `pip_var_type`
  pip_vars  <- pip_var_type$pip_vars_pc
  pip_type  <- pip_var_type$pip_vars_pc_class

  miss_ind  <- !(pip_vars %in% names(md))
  miss_vars <- pip_vars[miss_ind]
  miss_type <- pip_type[miss_ind]

  miss_type <- glue("as.{miss_type}")

  md[,
     (miss_vars) := lapply(miss_type, \(x) get(x)())]


#   ____________________________________________________________________________
#   Final formatting                                                        ####

  # order columns in correct order
  setcolorder(md, pip_vars)
  md <- md[, .SD, .SDcols = pip_vars]

  # Sort by country_code, surveyid_year and welfare
  sortbycol <- c("country_code",
                 "surveyid_year",
                 "welfare",
                 "hhid",
                 "pid")
  setorderv(md, sortbycol)
  return(md)
}


#' Clean group data from Datalibweb original file
#'
#' @param df data frame with group data, loaded with `pipload::pip_load_dlw()`
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' l    <- pd_cpfw_merge(gd, cpfw)
#' dlw_clean(l[[1]])
dlw_clean.pipgd <- function(df, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initial formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # hard copy
  gd <- copy(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Format types --------

  string <- c("country_code", "survey_acronym", "area", "welfare_type", "gd_type")
  nume   <- c("surveyid_year", "survey_year", "weight", "welfare")

  gd[, (string) := lapply(.SD, as.character),
     .SDcols = string]

  gd[, (nume) := lapply(.SD, as.numeric),
     .SDcols = nume]

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

#' Format weight variable for micro data
#'
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @keywords internal
format_wgt <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clean weight variable   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tryCatch(
    expr = {

      variables <- colnames(dt)

      if (!c("weight") %in% variables) {

        if (c("weight_p") %in% variables){
          setnames(dt, old = "weight_p", new = "weight")
        }
        else if (c("weight_h") %in% variables){
          setnames(dt, old = "weight_h", new = "weight")
        }
        else{

          dt[, weight := 1 / .N]

          cli::cli_inform(message = "Weight variable missing in DLW",
                          class = c("mn_wgt_inf", "pipinf"),
                          log = log_wrn,
                          link = svy,
                          call = sys.call())

        }
      }
    },
    mn_wgt_inf = function(cnd){

      if(cnd$log){ # Log the information

        add_log(cnd)

      }
    },
    finally={

      dt[, weight := as.double(weight)]

    }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Format welfare variable for micro data
#'
#' @inheritParams dlw_clean
#'
#' @return data.table
#'
#' @keywords internal
format_wlf <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  dt[, welfare := as.double(welfare)]

  dt[, welfare := welfare / 365]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
