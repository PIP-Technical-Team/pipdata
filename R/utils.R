#' Identify unique variables in data frame
#' @param x data frame.
#'
#' @return character vector of unique variable names
#' @examples
#' \dontrun{
#'  df <- data.frame(a = 1, b = rnorm(5), c = 4)
#'  uniq_vars(df)
#' }
#'
#' @export
uniq_vars <- function(x) {

  x <- check_data_table(x)
  N_vars   <- x[, lapply(.SD, uniqueN)]
  uni_vars <- names(N_vars)[N_vars == 1]

  return(uni_vars)

}

#' Turn data to data.table if it is not already
#' @noRd
check_data_table <- function(x) {
  if (!is.data.table(x)) {
    x <- collapse::qDT(x)
  }
  x
}

#' convert variables with unique values along the data set to attributes and then
#' remove those unique variables
#'
#' @param x data frame.
#'
#' @return list of single-value variables from dataframe `x`
#' @examples
#' \dontrun{
#'  df <- data.frame::data.frame(a = 1, b = rnorm(5), c = 4)
#'  uniq_vars_to_list(df)
#' }
#' @export
uniq_vars_to_list <- function(x) {

  uni_vars <- uniq_vars(x)

  y <- x[, lapply(.SD, unique),
         .SDcols = uni_vars]

  as.list(y)
}

#' Return a named list with unique values of variables
#'
#' @param x A data.table
#' @param vars variable to be turn to attributes.
#' @param nm variables for naming attributes
#'
#' @return a named list with unique values
#'
vars_to_list <- function(x, vars, nm = NULL) {
  var1 <- lapply(x[, ..vars], unique)
  if(!is.null(nm)) {
    var2 <- lapply(x[, ..nm], unique)
    if(!all(mapply(\(x, y) length(x) == length(y), var1, var2))) {
      cli::cli_abort("The unique values in {.arg num_var} and {.arg name_var} column are not equal")
    }
    var1 <- Map(stats::setNames, var1, var2)
  }
  var1
}

#' convert variables with unique values along the data set to attributes and then
#' remove those unique variables
#'
#' @param x a data.frame
#' @param exclude_vars variables to be excluded from turning to attributes (default NULL)
#'
#' @return data.frame with multiple-value variables only and single-value
#'   variables as attributes
#' @export
#' @examples
#' dt <- data.table::data.table(a = 1, b = 1:10, c = 5)
#' out <- uniq_vars_to_attr(dt)
#' out[]
#' attr(out, "a")
#' attr(out, "c")
#'
#' # Exclude `a` from being added as attribute
#' out <- uniq_vars_to_attr(dt, "a")
#' out[]
#'
#' # var `a` is not included as part of the attributes
#' attr(out, "a")
#'
#' # Var `c` is
#' attr(out, "c")
uniq_vars_to_attr <- function(x, exclude_vars = NULL) {
  nm <- names(x) |>
    copy() # make sure names are not modified by reference
  # Doing everything on copy of x since we want to preserve x in it's original form
  x1 <- copy(x)

  # Drop exclude_vars columns
  if(!is.null(exclude_vars)) {
    # Make sure that the column names in exclude_vars is a part of data
    if( !all(exclude_vars %in% nm) ) {
      ev <- exclude_vars[!exclude_vars %in% nm]
      cli::cli_abort("{.var {ev}} {?is/are} not {?a/} column name{?s} in data.
                     Choose one of {.var {nm}}")
    }

    #Dropping columns from x1
    x1[, (exclude_vars) := NULL]
  }
  uvl <- uniq_vars_to_list(x1)

  uni_vars <- names(uvl)
  mul_vars <- setdiff(nm, uni_vars)
  x <- change_vars_to_attr(x, uvl)
  x <- x[, ..mul_vars]

  return(x)
}

change_vars_to_attr <- function(df, uvl) {
  for (i in seq_along(uvl)) {
    var   <- names(uvl)[i]
    value <- uvl[[i]]

    # make sure that attributes are set correctly for data.table.
    if (inherits(df, "data.table")) {
      setattr(df, var, value)
    } else {
      attr(df, var) <- value
    }

  }
  df
}


#' Get path to pipdata original files
#'
#' pipdata comes bundled with a number of internal datasets originally created
#' in CSV format and then converted to proper R format. They are placed in  its
#' `inst/extdata` directory. This function make them easy to access. This
#' function is based (mainly copied) from `readr_example` in the `readr` package
#'
#' @param file Name of file. If `NULL`, the internal files will be listed.
#' @export
#' @examples
#' pipdata_int()
#' pipdata_int("pip_pc_var_type.csv")
pipdata_int <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "pipdata"))
  } else {
    system.file("extdata", file, package = "pipdata", mustWork = TRUE)
  }
}


#' get ordered level of data_level variables
#'
#' @param dt cleaned dataframe
#' @param x data_level variable name
#'
#' @return integer
#' @noRd
get_ordered_level <- function(dt, x) {

    x_level <- unique(dt[[x]])
    d1 <- c("national")
    d2 <- c("rural", "urban")

    if (identical(x_level, d1)) {
      1
    } else if (identical(x_level, d2)) {
      2
    } else {
      piperr(message = "Reporting level is not 1 or 2")
    }

}

#' Make vars as attributes
#'
#' @param df A data.frame
#' @param vars variables to changed to attributes
#'
#' @return A data.frame with vars variables as attributes
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- data.table(a = c(1, 2), b = 1:10, c = 5)
#' out <- vars_to_attr(dt, "a")
#' }
vars_to_attr <- function(df, vars) {
  df <- check_data_table(df)
  uvl <- vars_to_list(df, vars)
  df <- change_vars_to_attr(df, uvl)
  df[, !..vars]
}


#' Create a named vector of attributes
#'
#' @param df A data.frame
#' @param num_var Column name with numerical values
#' @param name_var Column name with name values
#'
#' @return Data.table with named attributes
#' @export
#'
#' @examples
#' \dontrun{
#'  dt <- data.table(a = c(1, 2), b = 1:10, c = c("a", "b"))
#'  out <- num_vars_to_attr(dt, "a", "c")
#' }
num_vars_to_attr <- function(df, num_var, name_var) {
  dt <- check_data_table(df)

  if(length(num_var) != length(name_var)) {
    cli::cli_abort("{.arg num_var} and {.arg name_var} should be of same length.
                   You have passed {length(num_var)} variable{?s} in {.arg num_var}
                   whereas {.arg name_var} consists of {length(name_var)} variable{?s}.")
  }
  uvl <- vars_to_list(dt, num_var, name_var)
  dt <- change_vars_to_attr(dt, uvl)
  c_col <- c(num_var, name_var)
  dt[, !..c_col]
}


#' Customized PIP error
#'
#' @param message message
#' @param name name assigned to the error. When "skip"
#' @param call parent call
#' @param ...
#'
#' @return error
#' @keywords internal
piperr <- function(message,
                   name = "skip"){

  svy <- .logenv$survey_id

  rlang::abort(message = message,
               class = c(name, "piperr"),
               id =  svy,
               call = sys.call(sys.parent()),
               use_cli_format = TRUE)

}

# pipwrn <- function(message, call = NULL){
#   cli::cli_warn(message = message,
#                 call = call,
#                  class = c("pipwrn"))
# }
#
# pipmsg <- function(message, call = NULL){
#   cli::cli_inform(message = message,
#                   call = call,
#                 class = c("pipmsg"))
# }


#' Add errors to a .logenv
#'
#' @param line line to be added to the log
#' @param class PIP error or warning class
#' @param error name of error or warning list
#'
#' @return a message in .logenv
#' @keywords internal
add_log <- function(line, error = NULL, class = "piperr") {

  # Check if the pip class exists
  if (!rlang::env_has(class, env = .logenv)) {

    rlang::env_poke(.logenv, class, list())
  }

  # load list
  log_list     <- get(class, envir = .logenv)

  key <- if (is.null(error)) "unknown errors" else error

  # Check if the error name already exists
  if (key %in% names(log_list)) {

    log_list[[key]][[1]] <- append(log_list[[key]][[1]], line)

  } else {

    log_list[[key]] <- list(line)

  }

  assign(class,
         log_list,
         envir = .logenv)

  invisible()

}

#' Add new attributes to data.table
#'
#' @param dt data.table which is missing the new attributes
#' @param new_attrs list with new attributes
#'
#' @return data.table
#' @export
add_attributes <- function(dt, new_attrs) {

  for (name in names(new_attrs)) {

    attr(dt, name) <- new_attrs[[name]]

  }

  return(dt)
}

char_to_fct <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chr_vars <- names(collapse::char_vars(dt))

  dt[,
     (chr_vars) := lapply(.SD, kit::charToFact),
     .SDcols = chr_vars
  ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}


#' Log the error
#'
#' @param e condition from the error
#'
#' @return NULL
#' @keywords internal
log_failure <- function(e) {

  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  root <- find_condition(e, "piperr")

  if (!is.null(root)) {
    line <- sprintf("[%s] %s for %s", ts, cli::ansi_strip(conditionMessage(root)), root$id)
    add_log(line, error =  deparse(root$call[[1]]), class = "piperr")

  } else {
    line <- sprintf("[%s] %s for %s", ts, cli::ansi_strip(conditionMessage(e)), deparse(conditionCall(e)))
    add_log(line, error = deparse(e$call[[1]]), class = "unk_err")

  }

  return(NULL)
}


find_condition <- function(cnd, class) {
  while (!is.null(cnd)) {
    if (inherits(cnd, class)) return(cnd)
    cnd <- cnd$parent
  }
  NULL
}


last_ver_inv <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dt <- dt[,
      # Get max master version and filter
      maxmast := vermast == max(vermast),
      by = .(country_code, surveyid_year, survey_acronym, module, tool)
    ][
      maxmast == 1
    ][,
      # Get max veralt version and filter
      maxalt := veralt == max(veralt),
      by = .(country_code, surveyid_year, survey_acronym, module, tool)
    ][
      maxalt == 1
    ][,
      # Get max pip version and filter
      maxpip := pipeline_version == max(pipeline_version),
      by = .(country_code, surveyid_year, survey_acronym, module, tool)
    ][
      maxpip == 1
    ][,
      c("maxmast","maxalt","maxpip") := NULL
    ][
      status == "same"
    ][
      module %in% c("GPWG", "GROUP", "BIN", "ALL" , "HIST")
    ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

find_dt_with_attribute <- function(lst, attr_name, attr_value) {
  Filter(function(dt) attr(dt, attr_name) == attr_value, lst)
}
#
# id_as_att <- function(dt, id_lst) {
#   # Add the id as an attribute
#   # attr(dt, "id") <- id_lst
#   data.table::setattr(dt, "id", id_lst)
#   return(dt)
# }

#' Find unique values in PFW according to some key variables
#'
#' @param dt data.table or data.frame
#' @param keyVar character vector with variables to determine unique observations
#'
#' @return data.table or data.frame
#' @export
#'
#' @examples
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' pfw <- pipload::pip_load_aux("pfw")
#' keyVar <- c("country_code", "survey_year", "survey_acronym", "welfare_type")
#' unq_obs_dt(pfw, keyVar)
unq_obs_dt <- function(dt,
                       keyVar) {

  # tryCatch(
  #
  #   expr = {

      if(uniqueN(dt, by = keyVar) != nrow(dt)){

        dt_d <- dt[duplicated(dt, by = keyVar)]
        n_rep <- nrow(dt_d)

        cli::cli_abort("There {?is/are} {n_rep} duplicates in PFW",
                       class = c("piperr","dup_pfw"))
      }

  #   },
  #
  #   piperr = function(cnd){
  #
  #     survey_id <- c(.pipdataenv$survey_id)
  #
  #     pipfun::log_add(event = "error",
  #                     message = cnd$message,
  #                     name = "pipdata_log",
  #                     .trace = cnd$call,
  #                     logmeta = list(error = class(cnd)[2],
  #                                    survey = survey_id,
  #                                    status = "The survey was skipped"))
  #
  #
  #   },
  #
  #   finally = {
  #
  #      unique(dt, by = keyVar)
  #
  #   }
  #
  # )

  return(dt)

}
