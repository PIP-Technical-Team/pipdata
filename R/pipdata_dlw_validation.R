#' Validate DLW data (Generic Documentation)
#'
#' This is a generic validation interface for DLW datasets across different module types.
#' Specific functions handle validation logic for GPWG, GROUP, BIN, HIST, ALL, ASPIRE, and L module types.
#'
#' @param dlw_data A DLW dataset in `qs` format.
#' @param svy_id A survey identifier extracted from the dataset.
#'
#' @return A data.frame containing validation results.
#'
#' @keywords internal
#' @export
dlw_validation <- function(dlw_data, svy_id) {
  stop("This is a documentation anchor. Use a method like dlw_validation_gpwg(), dlw_validation_group(), dlw_validation_bin(), dlw_validation_hist(), dlw_validation_all(), dlw_validation_aspire(), or dlw_validation_l().")
}

#' @describeIn dlw_validation Validate GPWG data
#'
#' Performs variable and structural checks on GPWG data, such as availability of core variables,
#' non-missingness, valid value ranges, and duplication checks.
#'
#' @import data.validator assertr
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

  stopifnot("Data data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables (not included weight and welfare variables)
  num_var_list <- df_var_list[grep("^year$|hsize$|welfshprosperity$",
                                   df_var_list)]

  # subset weight and welfare variable names
  # wgt_welfare <- df_var_list[grep("welfare$|weight$", df_var_list)]
  wgt_welfare <- df_var_list[grep("^welfare|^weight", df_var_list)]

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  core_var <- c("countrycode", "year", "hhid", "pid", "welfare",
                "welfshprosperity", "weight", "hsize")

  report   <- data_validation_report()

  validate(dlw_data, name = svy_id) |>
    is_var_startwith_avail("weight") |>
    is_var_startwith_avail("welfare") |>
    add_results(report)

  if ("countrycode" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>
      is_character("countrycode") |>
      add_results(report)
  }

  if ("urban" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>
      check_urban("urban") |>
      add_results(report)
  }

  if ("hhid" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>
      # validate_cols(not_na, hhid,
      #               description = "hhid should not be missing") |>
      validate_cols(description = "hhid should not be missing",
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, hhid) |>
      add_results(report)

    if ("pid" %in% df_var_list){

      validate(dlw_data, name = svy_id) |>
        validate_cols(description = "pid should not be missing",
                      skip_chain_opts = TRUE,
                      error_fun = warning_append, not_na, pid) |>
        # validate_if(description = "No duplicate records in key variables hhid, pid",
        #             is_uniq(hhid, pid)) |>
        validate_if(description = "No duplicate records in key variables hhid, pid",
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, is_uniq(hhid, pid)) |>
        add_results(report)
    }

  }

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      is_greaterthanzero(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
      add_results(report)

  }

  # validate weight and welfare variables
  for (i in seq_along(wgt_welfare)) {

    # labelled::var_label(dlw_data[[wgt_welfare[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(wgt_welfare[i]) |>
      is_greaterthanzero(wgt_welfare[i]) |>
      validate_cols(description = glue::glue("{wgt_welfare[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, wgt_welfare[i]) |>
      validate_rows(description = glue::glue("{wgt_welfare[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), wgt_welfare[i]) |>
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


#' @describeIn dlw_validation Validate GROUP data
#'
#' Checks for missing values, type mismatches, and invalid entries in GROUP datasets.
#'
#' @import data.validator assertr
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

  stopifnot("Data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables
  num_var_list <- df_var_list[grepl("urban", df_var_list)]

  # subset weight and welfare variable names
  wgt_welfare <- df_var_list[grep("^welfare|^weight", df_var_list)]

  # subset character variables
  chr_var_list <- df_var_list[grep("code|type$", df_var_list)]

  report   <- data_validation_report()
  core_var <- c("weight", "welfare", "urban", "gd_type", "welfare_type", "code")

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  if (na_threshold == 0) { na_threshold <- 1}

  validate(dlw_data, name = svy_id) |>
    is_var_startwith_avail("weight") |>
    is_var_startwith_avail("welfare") |>
    # is_var_avail("gd_type") |>
    add_results(report)

  # validate weight and welfare variables
  for (i in seq_along(wgt_welfare)) {

    # labelled::var_label(dlw_data[[wgt_welfare[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(wgt_welfare[i]) |>
      is_greaterthanzero(wgt_welfare[i]) |>
      validate_cols(description = glue::glue("{wgt_welfare[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, wgt_welfare[i]) |>
      validate_rows(description = glue::glue("{wgt_welfare[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), wgt_welfare[i]) |>
      add_results(report)

  }

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      is_greaterthanzero(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
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


#' @describeIn dlw_validation Validate BIN data
#'
#' Performs structural and value-based validation for BIN datasets,
#' checking numeric, character, and key variable consistency.
#'
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq has_all_names has_only_names verify warning_append within_bounds
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

  stopifnot("Data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables
  # num_var_list <- df_var_list[grep("^year$|welfare$|weight$|share$", df_var_list)]
  num_var_list <- df_var_list[grep("^year$|share$", df_var_list)]

  # subset weight and welfare variable names
  # wgt_welfare <- df_var_list[grep("welfare$|weight$", df_var_list)]
  wgt_welfare <- df_var_list[grep("^welfare|^weight", df_var_list)]

  # subset character variables
  chr_var_list <- df_var_list[grep("code$|verm$|vera$|^region|^country", df_var_list)]

  report   <- data_validation_report()
  core_var <- c("code", "year", "bins", "weight", "welfare", "verm",
                "vera", "region", "countryname")

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  if (na_threshold == 0) { na_threshold <- 1}

  validate(dlw_data, name = svy_id) |>
    is_var_startwith_avail("weight") |>
    is_var_startwith_avail("welfare") |>
    is_var_startwith_avail("bins") |>
    add_results(report)

  # validate weight and welfare variables
  for (i in seq_along(wgt_welfare)) {

    # labelled::var_label(dlw_data[[wgt_welfare[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(wgt_welfare[i]) |>
      is_greaterthanzero(wgt_welfare[i]) |>
      validate_cols(description = glue::glue("{wgt_welfare[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, wgt_welfare[i]) |>
      validate_rows(description = glue::glue("{wgt_welfare[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), wgt_welfare[i]) |>
      add_results(report)

  }

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      is_greaterthanzero(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
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
                    error_fun = warning_append, num_row_NAs, within_bounds(0, na_threshold), chr_var_list[i]) |>
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


#' @describeIn dlw_validation Validate HIST data
#'
#' Conducts data validation for HIST datasets, including checks for key variables like
#' `urban`, `weight`, and `welfare`, as well as common structural validations.
#'
#' @import data.validator assertr
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

  stopifnot("Data data is not loaded" = !is.null(dlw_data))
  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables
  # num_var_list <- df_var_list[grep("urban$|^year$|welfare$|weight$|
  #                                  hsize$|datayear$|type$", df_var_list)]
  num_var_list <- df_var_list[grep("urban$|^year$|hsize$|datayear$|type$",
                                   df_var_list)]

  # subset weight and welfare variable names
  # wgt_welfare <- df_var_list[grep("welfare$|weight$", df_var_list)]
  wgt_welfare <- df_var_list[grep("^welfare|^weight", df_var_list)]

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
    is_var_startwith_avail("weight") |>
    is_var_startwith_avail("welfare") |>
    add_results(report)

  # validate weight and welfare variables
  for (i in seq_along(wgt_welfare)) {

    # labelled::var_label(dlw_data[[wgt_welfare[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(wgt_welfare[i]) |>
      is_greaterthanzero(wgt_welfare[i]) |>
      validate_cols(description = glue::glue("{wgt_welfare[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, wgt_welfare[i]) |>
      validate_rows(description = glue::glue("{wgt_welfare[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), wgt_welfare[i]) |>
      add_results(report)

  }

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      is_greaterthanzero(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
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

#' @describeIn dlw_validation Validate ALL data
#'
#' Validates general ALL module type data containing core variables such as `welfare`, `weight`, and optionally `urban`.
#' Ensures basic structure and NA thresholds.
#'
#' @import data.validator assertr
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_all(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_all <- function(dlw_data, svy_id){

  stopifnot("Data data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset weight and welfare variable names
  wgt_welfare <- df_var_list[grep("^welfare|^weight", df_var_list)]
  
  # gender, age, and education variables
  demog_vars <- df_var_list[grep("^male|^educat|^school", df_var_list)]

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  report   <- data_validation_report()

  validate(dlw_data, name = svy_id) |>
    is_var_startwith_avail("weight") |>
    is_var_startwith_avail("welfare") |>
    is_var_startwith_avail("age") |>
    # is_var_avail("age") |>
    add_results(report)

  if ("urban" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>
      check_urban("urban") |>
      add_results(report)
  }

  if ("age" %in% df_var_list) {
    validate(dlw_data, name = svy_id) |>
      is_greaterequale0("age") |>
      is_valuebtwn0and110("age") |>
      add_results(report)
  }
  
  if ("male" %in% df_var_list) {
    validate(dlw_data, name = svy_id) |>
      check_gender("male") |>
      add_results(report)
  }
  
  # validate weight and welfare variables
  for (i in seq_along(wgt_welfare)) {

    # labelled::var_label(dlw_data[[wgt_welfare[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(wgt_welfare[i]) |>
      is_greaterthanzero(wgt_welfare[i]) |>
      validate_cols(description = glue::glue("{wgt_welfare[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, wgt_welfare[i]) |>
      validate_rows(description = glue::glue("{wgt_welfare[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), wgt_welfare[i]) |>
      add_results(report)

  }

  # validate gender and education variables
  for (i in seq_along(demog_vars)) {

    validate(dlw_data, name = svy_id) |>
      validate_cols(description = glue::glue("{demog_vars[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, demog_vars[i]) |>
      validate_rows(description = glue::glue("{demog_vars[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), demog_vars[i]) |>
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


#' @describeIn dlw_validation Validate ASPIRE data
#'
#' Handles validation for ASPIRE DLW datasets by checking structure and numeric variable consistency.
#' Special attention is paid to `hhweight`, `urban`, and household size.
#'
#' @import data.validator assertr
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_aspire(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_aspire <- function(dlw_data, svy_id){

  stopifnot("Data data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables
  num_var_list <- df_var_list[grep("^year$|hsize$",
                                   df_var_list)]

  # subset hhweight variable name
  wgt_welfare <- df_var_list[grep("hhweight$", df_var_list)]

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  report   <- data_validation_report()

  validate(dlw_data, name = svy_id) |>
    is_var_startwith_avail("hhweight") |>
    add_results(report)

  if ("urban" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>
      check_urban("urban") |>
      add_results(report)
  }

  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      is_greaterthanzero(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
      add_results(report)

  }

  # validate weight variables
  for (i in seq_along(wgt_welfare)) {

    # labelled::var_label(dlw_data[[wgt_welfare[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(wgt_welfare[i]) |>
      is_greaterthanzero(wgt_welfare[i]) |>
      validate_cols(description = glue::glue("{wgt_welfare[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, wgt_welfare[i]) |>
      validate_rows(description = glue::glue("{wgt_welfare[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), wgt_welfare[i]) |>
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

#' @describeIn dlw_validation Validate Labor (L) DLW data
#'
#' Validates DLW datasets containing labor-specific data, such as employment status (`lstatus`, `empstat`),
#' person-level identifiers (`hhid`, `pid`), and working hours (`whours`).
#'
#' @import data.validator assertr
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_l(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_l <- function(dlw_data, svy_id){

  stopifnot("Data data is not loaded" = !is.null(dlw_data))

  # get variable names
  df_var_list <- colnames(dlw_data)

  # subset numeric variables (not included weight and welfare variables)
  num_var_list <- df_var_list[grep("^year$|whours$",
                                   df_var_list)]

  # subset weight and welfare variable names
  emp_status <- df_var_list[grep("^lstatus|^empstat", df_var_list)]

  # threshold to validate availability of data/variable
  na_threshold <- round(nrow(dlw_data) * .10 )

  report   <- data_validation_report()

  validate(dlw_data, name = svy_id) |>
    is_var_startwith_avail("lstatus") |>
    is_var_startwith_avail("empstat") |>
    add_results(report)

  if ("countrycode" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>
      is_character("countrycode") |>
      add_results(report)
  }

  if ("hhid" %in% df_var_list){

    validate(dlw_data, name = svy_id) |>

      validate_cols(description = "hhid should not be missing",
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, hhid) |>
      add_results(report)

    if ("pid" %in% df_var_list){

      validate(dlw_data, name = svy_id) |>
        validate_cols(description = "pid should not be missing",
                      skip_chain_opts = TRUE,
                      error_fun = warning_append, not_na, pid) |>
        validate_if(description = "No duplicate records in key variables hhid, pid",
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, is_uniq(hhid, pid)) |>
        add_results(report)
    }

  }

  # validate numeric variables
  for (i in seq_along(num_var_list)) {

    labelled::var_label(dlw_data[[num_var_list[i]]]) <- NULL
    validate(dlw_data, name = svy_id) |>
      is_numeric(num_var_list[i]) |>
      is_greaterthanzero(num_var_list[i]) |>
      validate_cols(description = glue::glue("{num_var_list[i]} should not be missing"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, not_na, num_var_list[i]) |>
      validate_rows(description = glue::glue("{num_var_list[i]} NAs within %10"),
                    skip_chain_opts = TRUE,
                    error_fun = warning_append, num_row_NAs, within_bounds(0, na_threshold), num_var_list[i]) |>
      add_results(report)

  }

  # validate lstatus and empstat variables
  # for (i in seq_along(emp_status)) {
  #
  #   validate(dlw_data, name = svy_id) |>
  #     is_numeric(emp_status[i]) |>
  #     is_greaterthanzero(emp_status[i]) |>
  #     validate_cols(description = glue::glue("{emp_status[i]} should not be missing"),
  #                   skip_chain_opts = TRUE,
  #                   error_fun = warning_append, not_na, emp_status[i]) |>
  #     validate_rows(description = glue::glue("{emp_status[i]} NAs within %10"),
  #                   skip_chain_opts = TRUE,
  #                   error_fun = error_append, num_row_NAs, within_bounds(0, na_threshold), emp_status[i]) |>
  #     add_results(report)
  #
  # }


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

#' @describeIn dlw_validation Skip Validation
#'
#' Used for DLW modules that require no validation. Ensures only that the dataset is not blank.
#'
#' @return An empty data.frame with minimal checks applied.
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


#' Validating Specific Conditions of a Variable (Generic Documentation)
#'
#' This interface serves as a generic check for variables in DLW datasets across various scenarios.
#' It includes specific functions designed to assess different conditions, such as determining if a variable is of character or numeric type,
#' checking the number of reporting levels for urban/rural variables, verifying if values are greater than zero,
#' and confirming the availability of a variable within the dataset.
#'
#'
#' @param val variable name
#' @param col_name data
#'
#' @returns a validation report as text
#' @export
#'
#' @keywords internal
#' @export
dlw_var_check <- function(val, col_name) {
  stop(
    "This is a documentation anchor. Use a method like is_character(), is_numeric(), 
    check_urban(), check_gender(), is_greaterthanzero(), is_var_avail(), is_var_startwith_avail(), 
    is_var_endwith_avail(), is_valuebtwn0and120() or is_greaterequale0."
  )
}

#' @describeIn dlw_var_check Check a variable is character
#'
#' @examples
#' \dontrun{
#' is_character(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_character <-  function(val, col_name){

  expr = bquote(is.character(.(val)[[.(col_name)]]))
  validate_if(val,
              eval(expr),
              description = glue::glue("{col_name} is character"),
              skip_chain_opts = TRUE,
              error_fun = warning_append)
}

#' @describeIn dlw_var_check Check a variable is numeric
#'
#' @examples
#' \dontrun{
#' is_numeric(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_numeric <- function(val, col_name){

  expr = bquote(is.numeric(.(val)[[.(col_name)]]))
  validate_if(val,
              eval(expr),
              description = glue::glue("{col_name} is numeric"),
              skip_chain_opts = TRUE,
              error_fun = warning_append)
}


#' @describeIn dlw_var_check Check residential variable (urban/rural) has more than one reporting level in group data
#'
#' @examples
#' \dontrun{
#' check_urban(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
check_urban <- function(val, col_name){

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


#' @describeIn dlw_var_check Check gender (male - variable) has more than two categories in ALL data
#'
#' @examples
#' \dontrun{
#' check_gender(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
check_gender <- function(val, col_name) {
  # extract unique gender values
  gender_info <- unique(val[[col_name]])

  # Logical vector
  expr = bquote(gender_info == 2 | is.na(gender_info))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("Gender values are more than two categories"),
    skip_chain_opts = TRUE,
    error_fun = warning_append
  )
}

#' @describeIn dlw_var_check Check a numeric variable is greater than 0
#'
#' @examples
#' \dontrun{
#' is_greaterthanzero(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_greaterthanzero <- function(val, col_name){

  # Logical vector
  expr = bquote(any(val[[col_name]] > 0) |
                  any(is.na(.(val)[[.(col_name)]])))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} > 0"),
    skip_chain_opts = TRUE,
    error_fun = warning_append
  )
}

#' @describeIn dlw_var_check Check a numeric variable is greater than or equal to 0
#'
#' @examples
#' \dontrun{
#' is_greaterequale0(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_greaterequale0 <- function(val, col_name) {
  # Logical vector
  expr = bquote(
    any(val[[col_name]] >= 0) |
      any(is.na(.(val)[[.(col_name)]]))
  )

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} >= 0"),
    skip_chain_opts = TRUE,
    error_fun = error_append
  )
}

#' @describeIn dlw_var_check Check a variable is available in a dataset with specified variable name
#'
#' @examples
#' \dontrun{
#' is_var_avail(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_var_avail <- function(val, col_name){

  # Logical vector
  expr = bquote(col_name %in% names(val))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} variable should be in the data"),
    skip_chain_opts = TRUE,
    error_fun = error_append
  )
}

#' @describeIn dlw_var_check Check a variable is available in a dataset with variable name starting with a specified text
#'
#' @examples
#' \dontrun{
#' is_var_startwith_avail(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_var_startwith_avail <- function(val, col_name){

  # Logical vector
  expr = bquote(any(startsWith(names(val), col_name)))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} variable should be in the data"),
    skip_chain_opts = TRUE,
    error_fun = error_append
  )
}

#' @describeIn dlw_var_check Check a variable is available in a dataset with variable name end with a specified text
#'
#' @examples
#' \dontrun{
#' is_var_endwith_avail(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_var_endwith_avail <- function(val, col_name){

  # Logical vector
  expr = bquote(any(endsWith(names(val), col_name)))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} variable should be in the data"),
    skip_chain_opts = TRUE,
    error_fun = error_append
  )
}

#' @describeIn dlw_var_check Check age is available in a dataset with value between 0 and 110
#'
#' @examples
#' \dontrun{
#' is_valuebtwn0and110(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_valuebtwn0and110 <- function(val, col_name) { 
  
  expr <- bquote(
    all((.(val)[[.(col_name)]] >= 0 & .(val)[[.(col_name)]] <= 110) |
          is.na(.(val)[[.(col_name)]]))
  )
  
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} btwn 0 and 110 or NA"),
    skip_chain_opts = TRUE,
    error_fun = warning_append
  )
}

