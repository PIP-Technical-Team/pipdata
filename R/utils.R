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



#' convert variables with unique values along the data set to attrbitus and then
#' remove those unique variables
#'
#' @param x data frame.
#'
#' @return data.frame with multiple-value variables only and single-value
#'   variables as attrbitues
#' @export
uniq_vars_to_attr <- function(x) {

  uvl <- uniq_vars_to_list(x)

  uni_vars <- names(uvl)
  mul_vars <- names(x)[!(names(x) %in% uni_vars )]


  for (i in seq_along(uvl)) {

    var   <- names(uvl)[i]
    value <- uvl[[i]]
    attr(x, var) <- value

  }


  x <- x[, ..mul_vars]

  return(x)

}
