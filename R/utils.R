#' Identify unique variables in data frame
#'
#'
#' @param x data frame.
#'
#' @return character vector of unique variable names
#' @export
uniq_vars <- function(x) {

  if (!data.table::is.data.table(x)) {
    x <- as.data.table(x)
  }

  N_vars   <- x[, lapply(.SD, uniqueN)]
  uni_vars <- names(N_vars)[N_vars == 1]

  return(uni_vars)

}

#' convert variables with unique values along the data set to attrbitus and then
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
  # If not a data.table turn it to data.table
  if(!is.data.table(x)) x <- data.table::data.table(x)
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

  for (i in seq_along(uvl)) {

    var   <- names(uvl)[i]
    value <- uvl[[i]]
    attr(x, var) <- value
  }

  x <- x[, ..mul_vars]

  return(x)

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
