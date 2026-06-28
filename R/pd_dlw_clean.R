#' Clean data from datalibweb structure (High level)
#'
#' @param ls List of data frames or single dataframe.
#' @param verbose Logical. Print progress messages.
#'   Default: `getOption("pipdata.verbose", TRUE)`.
#' @param recode_spec Optional pre-resolved recode spec (as returned by
#'   [sync_recode_spec()]) threaded down to [apply_recode_spec()] so the spec is
#'   read once upstream instead of once per survey. Default `NULL` (each survey
#'   reads the spec from stamp).
#'
#' @return list with data.tables
#' @export
#'
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' pfw  <- pipload::pip_load_aux("pfw")
#'
#' gd    <- pipload::pip_load_dlw("CHN", 2015)
#' gd  <- survey_id_to_attr(gd, unique(gd$survey_id))
#' ls    <- pd_cpfw_merge(gd, pfw)
#' lf    <- pd_dlw_clean(ls)
#' names(lf)
#'
#' md    <- pipload::pip_load_dlw(country = "PHL", 2012)
#' md  <- survey_id_to_attr(md, unique(md$survey_id))
#' ls    <- pd_cpfw_merge(md, pfw)
#' lf    <- pd_dlw_clean(ls)
#' names(lf)
#' }
pd_dlw_clean <- function(ls, verbose = getOption("pipdata.verbose", TRUE),
                         recode_spec = NULL) {

  # Computations -------

  rl <- purrr::map(.x = ls, .f = dlw_clean, verbose = verbose,
                   recode_spec = recode_spec)

  # Return -------------
  return(rl)

}


#' Clean data from datalibweb structure (lower level, S3 methods)
#'
#' @param df data.table
#' @param verbose Logical. Print progress messages.
#'   Default: `getOption("pipdata.verbose", TRUE)`.
#' @param recode_spec Optional pre-resolved recode spec (see [pd_dlw_clean()]).
#' @param ...  other parameters
#'
#' @return data.table
#' @export
dlw_clean <- function(df, verbose = getOption("pipdata.verbose", TRUE),
                      recode_spec = NULL, ...) {
  UseMethod("dlw_clean")
}


#' Clean micro data from Datalibweb original file
#'
#' @param df data frame with micro data,
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @export
dlw_clean.pipmd <- function(df, verbose = getOption("pipdata.verbose", TRUE),
                            recode_spec = NULL, ...) {
  md <- copy(df)

  md <- shift_subnatid(md)   # normalise subnatid columns before area recode
  md <- format_wgt(md)       # weight column must exist before apply_recode_spec
  md <- format_wlf(md)

  # Replaces add_area(), recode_edu(), recode_gndr(), recode_age()
  # Spec lives in inst/extdata/recode_spec.yml; auto-synced to stamp on change
  md <- apply_recode_spec(md, verbose = verbose, recode_spec = recode_spec)

  md <- wbpip_clean(md)
  md <- pip_vars(md)

  return(md)
}


#' Clean group data from Datalibweb original file
#'
#' @param df data frame with group data
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @export
dlw_clean.pipgd <- function(df, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initial formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # hard copy
  gd <- copy(df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Area --------
  gd <- add_area(gd)

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

          survey_id <- pd_env_get("process_survey_id")

          pipfun::log_add(event = "warning",
                          message = "Weight variable missing in DLW",
                          name = "pipdata_log",
                          logmeta = list(warning = "mn_wgt_inf",
                                      survey = survey_id)
                          )

        }
      }

      dt[, weight := as.double(weight)]


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

  # educy
  if (c("educy") %in% variables) {
    dt <- dt |>
      collapse::fmutate(educy = as.double(educy)) |>
      collapse::ftransform(
        educy = data.table::fcase(
          educy < 0                , NA_real_ ,
          educy >= 0 & educy <= 50 , educy    ,
          educy > 50               , NA_real_ ,
          default = NA_real_
        )
      )
  }

  # educat4
  if (c("educat4") %in% variables) {
    # dt <- dt |>
    #   collapse::ftransform(educat4 = dplyr::case_when(
    #     educat4 == 1 ~ "No education",
    #     educat4 == 2 ~ "Primary",
    #     educat4 == 3 ~ "Secondary",
    #     educat4 == 4 ~ "Tertiary",
    #     .default = NA_character_))

    dt <- dt |>
      collapse::ftransform(
        educat4 = haven::as_factor(educat4)
      )
  }

  # educat5
  if (c("educat5") %in% variables) {
    # dt <- dt |>
    #   collapse::ftransform(educat5 = dplyr::case_when(
    #     educat5 == 1 ~ "No education",
    #     educat5 == 2 ~ "Primary incomplete",
    #     educat5 == 3 ~ "Primary complete but secondary incomplete",
    #     educat5 == 4 ~ "Secondary complete",
    #     educat5 == 5 ~ "Some tertiary/post-secondary",
    #     .default = NA_character_))

    dt <- dt |>
      collapse::ftransform(
        educat5 = haven::as_factor(educat5)
      )
  }

  # educat5
  if (c("educat7") %in% variables) {
    # dt <- dt |>
    #   collapse::ftransform(educat7 = dplyr::case_when(
    #     educat7 == 1 ~ "No education",
    #     educat7 == 2 ~ "Primary incomplete",
    #     educat7 == 3 ~ "Primary complete but secondary incomplete",
    #     educat7 == 4 ~ "Secondary incomplete",
    #     educat7 == 5 ~ "Secondary complete ",
    #     educat7 == 6 ~ "Post secondary but not university",
    #     educat7 == 7 ~ "university incomplete or complete",
    #     .default = NA_character_))

    dt <- dt |>
      collapse::ftransform(
        educat7 = haven::as_factor(educat7)
      )
  }

  # literacy
  if (c("literacy") %in% variables) {
    dt <- dt |>
      collapse::ftransform(
        literacy = data.table::fcase(
          literacy == 1 , "yes" ,
          literacy == 0 , "no"  ,
          default = NA_character_
        )
      )
  }

  # school
  if (c("school") %in% variables) {
    dt <- dt |>
      collapse::ftransform(
        school = data.table::fcase(
          school == 1 , "yes" ,
          school == 0 , "no"  ,
          default = NA_character_
        )
      )
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
      collapse::ftransform(
        gender = data.table::fcase(
          male == 1 , "male"   ,
          male == 0 , "female" ,
          default = NA_character_
        )
      )

  } # Do we need message about not having this variable?

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Recoding age variable
#'
#' @inheritParams dlw_clean
#'
#' @return data.table
#' @keywords internal
recode_age <- function(dt) {
  if (c("age") %in% colnames(dt)) {
    dt <- dt |>
      collapse::fmutate(age = as.double(age)) |>
      collapse::ftransform(
        age = data.table::fcase(
          age < 0               , NA_real_ ,
          age >= 0 & age <= 110 , age      ,
          age > 110             , NA_real_ ,
          default = NA_real_
        )
      )
  }

  return(dt)
}

#' Recode urban to area (lower level, S3 methods)
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @export
add_area <- function(dt) {
  UseMethod("add_area")
}

#' Recode urban to area for micro data
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @method add_area pipmd
#' @export
add_area.pipmd <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if ("subnatid" %in% colnames(dt)) {
    # Find all subnatid columns with numbers
    subnatid_cols <- grep("^subnatid[0-9]+$", colnames(dt), value = TRUE)

    # Extract the maximum number
    if (length(subnatid_cols) > 0) {
      nums <- as.numeric(gsub("subnatid", "", subnatid_cols))
      max_num <- max(nums)
    } else {
      max_num <- 0
    }

    # Rename from largest to smallest to avoid conflicts
    for (i in max_num:1) {
      old_name <- paste0("subnatid", i)
      new_name <- paste0("subnatid", i + 1)
      if (old_name %in% colnames(dt)) {
        setnames(dt, old_name, new_name)
      }
    }

    # Finally rename subnatid to subnatid1
    setnames(dt, "subnatid", "subnatid1")
  }

  # Abort if not urban variable
  if (!any(c("urban", "area") %in% colnames(dt))){

    survey_id <- c(pd_env_get("process_survey_id"))

    pipfun::log_add(event = "info",
                    message = "There is no urban variable",
                    name = "pipdata_log",
                    logmeta = list(info = "urb_var",
                                   survey = survey_id))

    dt[, area := ""]

    return(dt)

  }else if(c("urban") %in% colnames(dt)){

    # Recode urban to area

    dt[,
      area := data.table::fcase(
        urban == 1   , "urban" ,
        urban == 0   , "rural" ,
        is.na(urban) , ""      ,
        default = ""
      )
    ]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

#' Recode urban to area for group data
#'
#' @inheritParams cpfw_merge
#'
#' @return data.table
#' @method add_area pipgd
#' @export
add_area.pipgd <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # Abort if not urban variable
  if (!any(c("urban", "area") %in% colnames(dt))){

    survey_id <- c(pd_env_get("process_survey_id"))

    pipfun::log_add(event = "info",
                    message = "There is no urban or area variable",
                    name = "pipdata_log",
                    logmeta = list(info = "urb_var",
                                   survey = survey_id))

    dt[, area := ""]

    return(dt)

  }else if(c("urban") %in% colnames(dt)){

    # Recode urban to area

    dt[,
      area := data.table::fcase(
        urban == 1   , "urban"    ,
        urban == 0   , "rural"    ,
        is.na(urban) , "national" ,
        default = ""
      )
    ]

  }

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

  variables <- colnames(dt)

  # get from internal data `pip_var_type`
  # pip_vars  <- pip_var_type$pip_vars_pc
  # pip_type  <- pip_var_type$pip_vars_pc_class

  # add education to pip_vars
  pip_vars <- c("welfare", "weight", "area")

  if (any(class(dt) == "pipmd")) {
    pip_vars_all <- c(
      pip_vars,
      "educy",
      "educat4",
      "educat5",
      "literacy",
      "school",
      "age",
      "gender"
    )
  } else if (any(class(dt) == "pipgd")) {
    pip_vars_all <- pip_vars
  }

  # pip_type  <- c(pip_type, "character","character","character")

  # no_att_vars <- !(pip_vars %in% var_att_svy)
  # pip_vars_col <- pip_vars[no_att_vars]
  # pip_type_col <- pip_type[no_att_vars]

  # miss_ind <- !(pip_vars_col %in% names(dt))
  # miss_vars <- pip_vars_col[miss_ind]
  # miss_type <- pip_type_col[miss_ind]

  # miss_type <- glue("as.{miss_type}")

  # dt[,
  # (miss_vars) := lapply(miss_type, \(x) get(x)())
  # ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Final Formatting   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # order columns in correct order
  pip_vars_all <- pip_vars_all[pip_vars_all %in% colnames(dt)]
  setcolorder(dt, pip_vars_all)
  # dt <- dt[, .SD, .SDcols = pip_vars_all]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)
}
