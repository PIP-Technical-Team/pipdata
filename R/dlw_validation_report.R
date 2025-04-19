#' Get the validation report data
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' validation_report <- get_validation_report()
#' }
get_validation_report <- function(){

  if (!rlang::env_has(.pipdata, "validation_report")){

    cli::cli_abort("Validation data is not available the environment varaible")

  } else {

    validation_report <- .pipdata$validation_report[, -c("assertion.id", "call", "error_df")]
    # validation_report <- .pipdata$validation_report[, -c("call")]

  }

  # extract module type
  validation_report <-
    validation_report[, module_type := sub(".*_(.*)", "\\1", table_name)]

  # extract master data version
  validation_report <-
    validation_report[, vermast := sub(".*_([^_]+)_M.*", "\\1", table_name)]

  # extract adaptation version
  validation_report <-
    validation_report[, veralt  := sub(".*_M_([^_]+)_A.*", "\\1", table_name)]

  return(invisible(validation_report))

}

#' Get a simple frequency that shows number of valid and invalid datasets
#'
#' @returns a table
#' @export
#'
#' @examples
#' \dontrun{
#' get_data_status()
#' }
get_data_status <- function(){

  if (!rlang::env_has(.pipdata, "validation_report")){

    cli::cli_abort("Validation data is not available the environment varaible")

  } else {

    valid_data <- .pipdata$validation_report[, .(table_name, type)]
    valid_data <- valid_data[, status := fifelse(type == "error", 1, 0)]
    valid_data <- valid_data[, .(status_count = sum(status)), by = table_name]
    valid_data <- valid_data[, count_valid := fifelse(status_count > 0, 1, 0)]
    valid_data <- valid_data[, data_status := factor(count_valid,
                                                     levels = c(0, 1),
                                                     labels = c("Valid", "In valid"))]
    valid_data |> dplyr::count(data_status)
  }
}
