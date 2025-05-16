#' Validate GPWG dlw raw data
#'
#' @param dlw_data a GPWG raw data in qs format
#' @param svy_id survey id extracted from the dlw_data
#' @import data.validator assertr
#'
#' @return a data.frame that contains validation result
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_gpwg(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_gpwg <- function(dlw_data, svy_id){

  # set-up a release
  pipfun::get_wrk_release()

  stopifnot("Data data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables

  # num_var_list <- df_var_list[grep("year$|welfare$|weight$|hsize$|
  #                                  welfshprosperity$", df_var_list)]

  num_var_list <- df_var_list[grep("^year$|welfare$|weight$|hsize$|
                                   welfshprosperity$", df_var_list)]

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  core_var <- c("countrycode", "year", "hhid", "pid", "welfare",
                "welfshprosperity", "weight", "hsize")

  report   <- data_validation_report()

  validate(dlw_data, name = svy_id) |>

    verify(description = "All core variables available in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_all_names("countrycode", "year", "hhid", "pid", "welfare",
                         "welfshprosperity", "weight", "hsize"))  |>
    verify(description = "No additional variables in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_only_names("countrycode", "year", "hhid", "pid", "welfare",
                          "welfshprosperity", "weight", "hsize"))  |>
    add_results(report)

  if ("countrycode" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>
      is_character("countrycode") |>
      add_results(report)
  }

  if ("hhid" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>
      validate_cols(not_na, hhid,
                    description = "hhid should not be missing") |>
      add_results(report)

    if ("pid" %in% df_var_list){

      validate(dlw_data, name = svy_id) |>
        validate_cols(description = "pid should not be missing",
                      skip_chain_opts = TRUE,
                      error_fun = warning_append, not_na, pid) |>
        validate_if(description = "No duplicate records in key variables hhid, pid",
                    is_uniq(hhid, pid)) |>
        add_results(report)
    }

  }

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
      add_results(report)

  }

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()


  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record, ignore.attr=TRUE)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}


#' Validate GROUP dlw raw data
#'
#' @param dlw_data a GROUP raw data in qs format
#' @param svy_id survey id extracted from the dlw_data
#'
#' @import data.validator assertr
#' @keywords internal
#'
#' @return a data.frame that contains validation result
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_group(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_group <- function(dlw_data, svy_id){

  # set-up a release
  pipfun::get_wrk_release()

  stopifnot("Data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables
  num_var_list <- df_var_list[grepl("urban|welfare$|weight", df_var_list)]

  # subset character variables
  chr_var_list <- df_var_list[grep("code|type$", df_var_list)]

  report   <- data_validation_report()
  core_var <- c("weight", "welfare", "urban", "gd_type", "welfare_type", "code")

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  if (na_threshold == 0) { na_threshold <- 1}

  validate(dlw_data, name = svy_id) |>
    verify(description = "All core variables available in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_all_names(core_var))  |>
    verify(description = "No additional variables in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_only_names(core_var)) |>
    add_results(report)

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
      add_results(report)

    if (num_var_list[i] == "urban") {

      validate(dlw_data, name = svy_id) |>
        check_urban("urban") |>
        add_results(report)

    }

  }

  # validate character variables
  for (i in seq_along(chr_var_list)) {

    validate(dlw_data, name = svy_id) |>
      is_character(chr_var_list[i]) |>
      validate_cols(description = glue::glue("{chr_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, chr_var_list[i]) |>
      validate_rows(description = glue::glue("{chr_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), chr_var_list[i]) |>
      add_results(report)

    if (chr_var_list[i] == "welfare_type") {
      validate(dlw_data, name = svy_id) |>
        validate_cols(description = glue::glue("{chr_var_list[i]} should not contain out of range values"),
                      skip_chain_opts = TRUE,
                      error_fun = warning_append,
                      in_set(c("C", "I", "income", "consumption")), chr_var_list[i]) |>
        add_results(report)
    }

  }

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record, ignore.attr=TRUE)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}


#' Validate BIN dlw raw data
#'
#' @param dlw_data a BIN raw data in qs format
#' @param svy_id survey id extracted from the dlw_data
#'
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq has_all_names has_only_names verify warning_append within_bounds
#' @keywords internal
#'
#' @return a data.frame that contains validation result
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_bin(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_bin <- function(dlw_data, svy_id){

  # set-up a release
  pipfun::get_wrk_release()

  stopifnot("Data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables
  num_var_list <- df_var_list[grep("^year$|welfare$|weight$|share$", df_var_list)]

  # subset character variables
  chr_var_list <- df_var_list[grep("code$|verm$|vera$|^region|^country", df_var_list)]

  report   <- data_validation_report()
  core_var <- c("code", "year", "bins", "weight", "welfare", "verm",
                "vera", "region", "countryname")

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  if (na_threshold == 0) { na_threshold <- 1}

  validate(dlw_data, name = svy_id) |>
    verify(description = "All core variables available in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_all_names(core_var)) |>
    verify(description = "No additional variables in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_only_names(core_var)) |>
    add_results(report)

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
      add_results(report)

  }

  # validate character variables
  for (i in seq_along(chr_var_list)) {

    validate(dlw_data, name = svy_id) |>
      is_character(chr_var_list[i]) |>
      validate_cols(description = glue::glue("{chr_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, chr_var_list[i]) |>
      validate_rows(description = glue::glue("{chr_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), chr_var_list[i]) |>
      add_results(report)

  }

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record, ignore.attr=TRUE)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}


#' Validate HIST dlw raw data
#'
#' @param dlw_data a HIST raw data in qs format
#' @param svy_id survey id extracted from the dlw_data
#'
#' @import data.validator assertr
#' @keywords internal
#'
#' @return a data.frame that contains validation result
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_hist(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_hist <- function(dlw_data, svy_id){

  # set-up a release
  pipfun::get_wrk_release()

  stopifnot("Data data is not loaded" = !is.null(dlw_data))
  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables
  num_var_list <- df_var_list[grep("urban$|^year$|welfare$|weight$|
                                   hsize$|datayear$|type$", df_var_list)]

  # subset character variables
  chr_var_list <- df_var_list[grep("code$|survname$", df_var_list)]

  report   <- data_validation_report()
  core_var <- c("regioncode", "countrycode", "year", "datayear", "survname",
                "hhid", "hsize", "weight", "urban", "welfare", "coveragetype",
                "datatype", "code")

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  if (na_threshold == 0) { na_threshold <- 1}

  validate(dlw_data, name = svy_id) |>

    verify(description = "All core variables available in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_all_names(core_var)) |>
    verify(description = "No additional variables in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_only_names(core_var)) |>
    add_results(report)

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
      add_results(report)

  }

  # validate character variables
  for (i in seq_along(chr_var_list)) {

    validate(dlw_data, name = svy_id) |>
      is_character(chr_var_list[i]) |>
      validate_cols(description = glue::glue("{chr_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, chr_var_list[i]) |>
      validate_rows(description = glue::glue("{chr_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), chr_var_list[i]) |>
      add_results(report)

  }

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record, ignore.attr=TRUE)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}

#' Non-validate module information
#'
#' @param dlw_data a raw data in qs format
#' @param svy_id name of the data
#'
#' @returns an empty data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_skip(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = survey_id
#' )
#' }
dlw_validation_skip <- function(dlw_data, svy_id){

  # set-up a release
  pipfun::get_wrk_release()

  stopifnot("Data data is not loaded" = !is.null(dlw_data))

  df_var_list <- colnames(dlw_data)
  report   <- data_validation_report()

  validate(dlw_data, name = svy_id)  |>
    verify(nrow(dlw_data) > 0, description = "Data should not blank") |>
    # verify(num_row_NAs, df_var_list, description = "Rows shouldn't be missing") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record, ignore.attr=TRUE)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}

#' Check if a variable is character
#'
#' @param val variable name
#' @param col_name data name
#'
#' @returns a validation report as text
#' @export
#'
#' @examples
#' \dontrun{
#' is_character(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_character <-  function(val, col_name){
  # set-up a release
  pipfun::get_wrk_release()

  expr = bquote(is.character(.(val)[[.(col_name)]]))
  validate_if(val, eval(expr), description = glue::glue("{col_name} is character"))
}

#' Check if a variable is numeric
#'
#' @param val variable name
#' @param col_name data
#'
#' @returns a validation report as text
#' @export
#'
#' @examples
#' \dontrun{
#' is_numeric(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_numeric <- function(val, col_name){

  # set-up a release
  pipfun::get_wrk_release()

  expr = bquote(is.numeric(.(val)[[.(col_name)]]))
  validate_if(val, eval(expr), description = glue::glue("{col_name} is numeric"))
}


#' Check if a urban has more than one reporting level in group data
#'
#' @param val variable name
#' @param col_name data
#'
#' @returns a validation report as text
#' @export
#'
#' @examples
#' \dontrun{
#' check_urban(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
check_urban <- function(val, col_name){

  # set-up a release
  pipfun::get_wrk_release()

  # extract unique URBAN values
  urban_info <- unique(val[[col_name]])

  # Logical vector
  expr = bquote(urban_info == 1 | is.na(urban_info))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("Urban - more than one reporting level"),
    skip_chain_opts = TRUE,
    error_fun = warning_append
  )
}

