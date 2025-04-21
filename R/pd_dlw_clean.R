#' Clean data from datalibweb structure (High level)
#'
#' @param ls List of data frames or single dataframe.
#'
#' @return list with data.tables
#' @export
#'
#' @examples
#' pfw  <- pipload::pip_load_aux("pfw")
#' # dt   <- pipload::pip_load_dlw(survey_id = "BRA_2008_PNAD_v02_M_v04_A_GMD_ALL")
#'
#' gd    <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw  <- get_country_pfw(gd, pfw)
#' ls    <- pd_cpfw_merge(gd, cpfw)
#' lf    <- pd_dlw_clean(ls)
#' names(lf)
#'
#' md    <- pipload::pip_load_dlw(country = "PHL", 2012)
#' cpfw  <- get_country_pfw(md, pfw)
#' ls    <- pd_cpfw_merge(md, cpfw)
#' lf    <- pd_dlw_clean(ls)
#' names(lf)
pd_dlw_clean <- function(ls) {

  # Computations -------

      rl <- purrr::map(.x = ls,
                       .f = dlw_clean)

  # Return -------------
  return(rl)

}


#' Clean data from datalibweb structure (lower level, S3 methods)
#'
#' @param df data.table
#' @param ...  other parameters
#'
#' @return data.table
#' @export
#'
#' @examples
#' gd  <- pipload::pip_load_dlw("CHN", 2015)
#' pfw <- pipload::pip_load_aux("pfw")
#' cpfw <- get_country_pfw(gd, pfw)
#' ls    <- pd_cpfw_merge(gd, cpfw)
#' dlw_clean(ls[[1]])
#'
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' ls    <- pd_cpfw_merge(md, cpfw)
#' dlw_clean(ls[[1]])
dlw_clean <- function(df,...) {
  UseMethod("dlw_clean")
}

#' Clean micro data from Datalibweb original file
#'
#' @param df data frame with micro data,
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' md   <- pipload::pip_load_dlw(country = "PRY", 2012)
#' cpfw <- get_country_pfw(md, pfw)
#' ls    <- pd_cpfw_merge(md, cpfw)
#' dlw_clean(ls[[1]])
dlw_clean.pipmd <- function(df, ...) {

#   ____________________________________________________________________________
#   Initial formatting                                                      ####

  # hard copy
  md <- copy(df)

  ## clean weight variable
  md <- format_wgt(md)

  ## format welfare variable
  md <- format_wlf(md)

#   ____________________________________________________________________________
#   Recode variables                                                      ####

  ## Education
  md <- recode_edu(md)

  ## Gender
  md <- recode_gndr(md)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Wbpip clean (need to updata) --------

  md <- wbpip_clean(md)

#   ____________________________________________________________________________
#   Final formatting                                                        ####

  md <- pip_vars(md)

  # Sort by welfare (commented because it gives an error)
  # sortbycol <- c(
  #   "welfare",
  #   "hhid", # Why hhid if they are character? Should they be numeric?
  #   "pid")

  # setorderv(md, cols = "welfare")

  return(md)
}


#' Clean group data from Datalibweb original file
#'
#' @param df data frame with group data
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @export
#'
#' @examples
#' pfw <- pipload::pip_load_aux("pfw")
#' gd   <- pipload::pip_load_dlw("CHN", 2015)
#' cpfw <- get_country_pfw(gd, pfw)
#' ls    <- pd_cpfw_merge(gd, cpfw)
#' dlw_clean(ls[[1]])
dlw_clean.pipgd <- function(df, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initial formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # hard copy
  gd <- copy(df)

  # NEED TO CHECK FORMATTING TO WELFARE AND WEIGHT IN WBPIP!

  # sorting
  varsort <- c("area", "welfare") # Why area in group and not in micro?
  setorderv(gd, varsort)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Wbpip clean (need to updata) --------

  gd <- wbpip_clean(gd)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Format types --------

  gd <- pip_vars(gd)

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

          piperr(message = "Weight variable missing in DLW",
                 name = "mn_wgt_inf")

          # svy <- .logenv$survey_id
          #
          # cli::cli_abort(message = "Weight variable missing in DLW",
          #                 class = c("mn_wgt_inf", "piperr"),
          #                 log = log_wrn,
          #                 link = svy,
          #                 call = sys.call())

        }
      }
    },
    mn_wgt_inf = function(cnd){

      log_failure(cnd)

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

#' Recoding education variables
#'
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @keywords internal
recode_edu <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Education   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  variables <- colnames(dt)

  # educat4
  if (c("educat4") %in% variables){

    dt <- dt |>
      collapse::ftransform(educat4 = dplyr::case_when(
        educat4 == 1 ~ "No education",
        educat4 == 2 ~ "Primary",
        educat4 == 3 ~ "Secondary",
        educat4 == 4 ~ "Tertiary",
        .default = NA_character_))

  }

  # educat5
  if (c("educat5") %in% variables){

    dt <- dt |>
      collapse::ftransform(educat5 = dplyr::case_when(
        educat5 == 1 ~ "No education",
        educat5 == 2 ~ "Primary incomplete",
        educat5 == 3 ~ "Primary complete but secondary incomplete",
        educat5 == 4 ~ "Secondary complete",
        educat5 == 5 ~ "Some tertiary/post-secondary",
        .default = NA_character_))

  }

  # literacy
  if (c("literacy") %in% variables) {

    dt <- dt |>
      collapse::ftransform(literacy = dplyr::case_when(
        literacy == 1 ~ "yes",
        literacy == 0 ~ "no",
        .default = NA_character_))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Recoding gender variable
#'
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @keywords internal
recode_gndr <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Recode male to string   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (c("male") %in% colnames(dt)){

    dt <- dt |>
      collapse::ftransform(gender = dplyr::case_when(
        male == 1 ~ "male",
        male == 0 ~ "female",
        .default = NA_character_))

  } # Do we need message about not having this variable?

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Final formating of pip variables
#'
#' @inheritParams dlw_clean
#' @return data.table
#'
#' @keywords internal
pip_vars <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add missing pip variables   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  var_att_svy <- c("country_code", # We can add this to the internal data
                   "survey_id",
                   "surveyid_year",
                   "survey_acronym",
                   "survey_year",
                   "welfare_type",
                   "distribution_type",
                   "gd_type")

  # get from internal data `pip_var_type`
  pip_vars  <- pip_var_type$pip_vars_pc
  pip_type  <- pip_var_type$pip_vars_pc_class

  # add education to pip_vars
  # pip_vars <- c(pip_vars, "educat4","educat5","literacy")
  # pip_type  <- c(pip_type, "character","character","character")

  no_att_vars <- !(pip_vars %in% var_att_svy)
  pip_vars_col <- pip_vars[no_att_vars]
  pip_type_col <- pip_type[no_att_vars]

  miss_ind  <- !(pip_vars_col %in% names(dt))
  miss_vars <- pip_vars_col[miss_ind]
  miss_type <- pip_type_col[miss_ind]

  miss_type <- glue("as.{miss_type}")

  dt[,
     (miss_vars) := lapply(miss_type, \(x) get(x)())]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Final Formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # order columns in correct order
  setcolorder(dt, pip_vars_col)
  dt <- dt[, .SD, .SDcols = pip_vars_col]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
