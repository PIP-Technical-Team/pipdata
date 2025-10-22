#' @title Convert Columns to Attributes
#'
#' @description
#' `all_cols_to_attr()` takes a data frame and moves most of its columns into
#' attributes. Only the columns specified in `fixed_cols` remain in the
#' top-level of the data frame. This can reduce memory usage by storing repeated
#' or constant values more efficiently.
#'
#' @param df A data frame containing the columns to be transformed.
#' @param fixed_cols A character vector naming the columns that should remain
#'   in the data frame. All other columns will be compressed into attributes.
#'
#' @details
#' For each column not in `fixed_cols`:
#' \itemize{
#'   \item If the column has only one unique value, it stores that value in a
#'         small list of the form \code{list(values = <the_value>)}.
#'   \item Otherwise, if the column has multiple unique values, it stores a
#'         dictionary-like structure with:
#'         \code{list(values = <vector_of_unique_values>, index = <index_map>)}.
#'         Each row in the original data maps to the appropriate entry in
#'         \code{values}.
#' }
#'
#' @return A data frame with the same number of rows as `df`, but containing
#'   only the `fixed_cols` in its top-level. The other columns are in the
#'   data frame's attributes, named after the original column.
#'
#' @examples
#' library(collapse)
#'
#' # Example data
#' df <- data.frame(
#'   key      = 1:5,
#'   welfare  = runif(5),
#'   weight   = rnorm(5),
#'   x_var    = c("A", "A", "B", "B", "B"),
#'   y_var    = "constant"
#' )
#'
#' # Compress columns
#' df_comp <- all_cols_to_attr(df, fixed_cols = c("key", "welfare", "weight"))
#'
#' # Check the new 'df_comp'
#' df_comp
#' # Only key, welfare, weight remain visible
#' attributes(df_comp)$x_var
#' # The x_var attribute contains values and index
#' attributes(df_comp)$y_var
#' # The y_var attribute is just a single value
#'
#' @export
all_cols_to_attr <- function(df,
                            fixed_cols = c("key",
                                           "welfare",
                                           "weight")) {
  # data frame to return
  ret <- df |>
    fselect(fixed_cols)

  # cols to attributes
  attr_cols <- setdiff(names(df),
                       fixed_cols)

  # create attributes
  attr_list <- lapply(attr_cols,
                      function(x) {

    column_data  <- df[[x]]
    unique_vals  <- funique(column_data) |>
      as.vector()

    # single unique value
    if (length(unique_vals) == 1L) {

      list(values = unique_vals)

    } else {

      # Otherwise with index for row mapping
      index_map <- fmatch(column_data,
                          unique_vals)
      list(values = unique_vals,
           index  = index_map)
    }
  })

  # prep attr
  names(attr_list) <- attr_cols

  # add attr
  attributes(ret) <- c(attributes(ret),
                       attr_list)

  ret
}

#' @title Convert Attributes Back to Columns
#'
#' @description
#' `all_attr_to_cols()` reverses the transformation done by
#' [all_cols_to_attr()]. It reconstructs the original columns from the
#' attributes and binds them back into the data frame.
#'
#' @param df A data frame produced by [all_cols_to_attr()]. It must contain
#'   attributes named after the compressed columns, with either a single
#'   \code{values} element or a \code{values/index} pair.
#'
#' @details
#' If an attribute has a single value in \code{values}, it is repeated for all
#' rows of `df`. If it has both \code{values} and \code{index}, each row is
#' reconstructed via \code{values[index]}. The resulting columns are appended
#' to the data frame in the order they appear in the attributes.
#'
#' @return A data frame with all columns restored to the top-level, matching
#'   the structure of the original pre-compression data frame.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   key      = 1:5,
#'   welfare  = runif(5),
#'   weight   = rnorm(5),
#'   x_var    = c("A", "A", "B", "B", "B"),
#'   y_var    = "constant"
#' )
#'
#' # Compress columns
#' df_comp <- all_cols_to_attr(df, fixed_cols = c("key", "welfare", "weight"))
#'
#' # Suppose we have 'df_comp' from above
#' df_orig <- all_attr_to_cols(df_comp)
#' df_orig
#' # This should match the original 'df'
#'
#' @export
all_attr_to_cols <- function(df) {

  standard_atts <- c("names",
                     "row.names",
                     "class")
  all_atts      <- attributes(df)
  keep_atts     <- all_atts[standard_atts]
  stored_cols   <- setdiff(names(all_atts),
                           standard_atts)

  new_cols <- lapply(all_atts[stored_cols],
                     function(att) {

    # If single_val is present, the entire column has that one value
    if (length(att$values) == 1) {

      rep(att$values,
          fnrow(df))
    } else {
      # 'values' + 'index' approach
      att$values[att$index]

    }
  })

  # Combine the main (fixed) columns and the newly reconstructed columns
  attributes(df) <- NULL
  attributes(df) <- keep_atts
  new_cols <-
    data.frame(new_cols)

  df <- cbind(df,
              new_cols)

  df
}
