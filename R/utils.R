#' Identify unique variables in data frame
#' @param x data frame.
#'
#' @return character vector of unique variable names
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
  if (!data.table::is.data.table(x)) {
    x <- as.data.table(x)
  }
  x
}
#' convert variables with unique values along the data set to attributes and then
#' remove those unique variables
#'
#' @param x data frame.
#'
#' @return list of single-value variables from dataframe `x`
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
#'
#' @return a named list with unique values
#'
vars_to_list <- function(x, vars) {
  lapply(x[, ..vars], unique)
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
uniq_vars_to_attr <- function(x, exclude_vars = NULL) {
  nm <- names(x)
  # Doing everything on copy of x since we want to preserve x in it's original form
  x1 <- data.table::copy(x)
  # Drop exclude_vars columns
  if(!is.null(exclude_vars)) {
    # Make sure that the column names in exclude_vars is a part of data
    if(!all(exclude_vars %in% nm)) cli::cli_abort("{exclude_vars} is not a column name in data. Choose one of {names(x)}")
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
    attr(df, var) <- value
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
    3
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
