#' Validate GPWG dlw raw data
#'
#' @param dlw_data a GPWG raw data in qs format
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq has_all_names has_only_names
#' @keywords internal
#'
#' @export
dlw_validation_gpwg <- function(dlw_data, svy_id){

  stopifnot("Data data is not loaded" = !is.null(dlw_data))

  report   <- data_validation_report()
  core_var <- c("countrycode", "year", "hhid", "pid", "welfare",
                "welfshprosperity", "weight", "hsize")

  na_threshold <- round(nrow(df) * .10 )

  validate(dlw_data, name = svy_id) |>

    verify(description = "All core variables available in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_all_names("countrycode", "year", "hhid", "pid", "welfare", "welfshprosperity",
                           "weight", "hsize"))  |>
      verify(description = "No additional variables in the data",
             skip_chain_opts = TRUE,
             error_fun = warning_append,
             has_only_names(core_var))  |>

      # year
      validate_if(is.numeric(year),
                  description = "year should be numeric") |>
      validate_cols(not_na, year,
                    description = "year should not be missing") |>
      # hhid
      # validate_cols(not_na, hhid,
      #               description = "hhid should not be missing") |>
      # # pid
      # validate_cols(not_na, pid,
      #               description = "pid should not be missing") |>

      # weight
      # validate_if(is.numeric(weight),
      #           description = "weight should be numeric") |>
      # validate_cols(description = "weight should not be missing",
      #             skip_chain_opts = TRUE,
      #             error_fun = warning_append, not_na, weight) |>
      # validate_rows(num_row_NAs, within_bounds(0, na_threshold), weight,
      #             description = "weight NAs within %10") |>
      # # welfare
      # validate_if(is.numeric(welfare),
      #             description = "welfare should be numeric") |>
      # validate_cols(description = "welfare should not be missing",
      #               skip_chain_opts = TRUE,
      #               error_fun = warning_append, not_na, welfare) |>
      # validate_rows(num_row_NAs, within_bounds(0, na_threshold), welfare,
      #             description = "welfare NAs within %10") |>

      # hsize
      # validate_if(is.numeric(hsize),
      #             description = "hsize should be numeric") |>
      # validate_if(hsize > 0,
      #             description = "hsize has only positive values") |>
      # validate_cols(not_na, hsize,
      #               description = "hsize should not be missing") |>

      validate_if(description = "No duplicate records in key variables hhid, pid",
                  is_uniq(hhid, pid)) |>
      add_results(report)

    validation_record <- get_results(report, unnest = FALSE) |>
      setDT()

    df_var_list <- colnames(df)
    report0   <- data_validation_report()

    for (i in seq_along(df_var_list)) {

      df_var_mem <- df_var_list[i]

      if (df_var_mem %in% c("countrycode", "hhid", "pid",
                            "weight", "welfare",
                            "welfshprosperity", "hsize")) {

        if (df_var_mem == "hhid"){

          # "GNB_1993_ICOF_v01_M_v01_A_GMD_GPWG.dta"
          validate(dlw_data, name = svy_id) |>
            validate_cols(not_na, hhid,
                          description = "hhid should not be missing") |>
            add_results(report0)

        } else if (df_var_mem == "pid"){

          validate(dlw_data, name = svy_id) |>

            validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                          skip_chain_opts = TRUE,
                          error_fun = warning_append, not_na, df_var_mem) |>
            add_results(report0)

        } else if (df_var_mem == "welfshprosperity"){

          validate(dlw_data, name = svy_id) |>
            validate_if(is.numeric(welfshprosperity),
                        description = glue::glue("{df_var_mem} should be character")) |>
            validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                          skip_chain_opts = TRUE,
                          error_fun = warning_append, not_na, welfshprosperity) |>
            add_results(report0)

        } else if (df_var_mem == "weight") {

          # weight
          validate(dlw_data, name = svy_id) |>
            validate_if(is.numeric(weight),
                      description = "weight should be numeric") |>
            validate_cols(description = "weight should not be missing",
                          skip_chain_opts = TRUE,
                          error_fun = warning_append, not_na, weight) |>
            validate_rows(num_row_NAs, within_bounds(0, na_threshold), weight,
                          description = "weight NAs within %10") |>
            add_results(report0)

        } else if (df_var_mem == "welfare"){

          # welfare
          validate(dlw_data, name = svy_id) |>
            validate_if(is.numeric(welfare),
                      description = "welfare should be numeric") |>
            validate_cols(description = "welfare should not be missing",
                          skip_chain_opts = TRUE,
                          error_fun = warning_append, not_na, welfare) |>
            validate_rows(num_row_NAs, within_bounds(0, na_threshold), welfare,
                          description = "welfare NAs within %10") |>
            add_results(report0)

        } else if (df_var_mem == "hsize"){

          validate(dlw_data, name = svy_id) |>
            validate_if(is.numeric(hsize),
                      description = "hsize should be numeric") |>
            validate_cols(not_na, hsize,
                          description = "hsize should not be missing") |>
            add_results(report0)

        } else {

          validate(dlw_data, name = svy_id) |>
            validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                          skip_chain_opts = TRUE,
                          error_fun = warning_append, not_na, df_var_mem) |>
            add_results(report0)
        }

     validation_record0 <- get_results(report0, unnest = FALSE) |>
          setDT()
        validation_record <- rbind(validation_record, validation_record0,
                                   ignore.attr=TRUE)
      }
    }

  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}


#' Validate GROUP dlw raw data
#'
#' @param dlw_data a GROUP raw data in qs format
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq has_all_names has_only_names
#' @keywords internal
#'
#' @export
dlw_validation_group <- function(dlw_data, svy_id){

  stopifnot("Data is not loaded" = !is.null(dlw_data))

  report   <- data_validation_report()
  core_var <- c("weight", "welfare", "urban", "gd_type", "welfare_type", "code")

  na_threshold <- round(nrow(df) * .10 )

  if (na_threshold == 0) { na_threshold <- 1}

  validate(dlw_data, name = svy_id) |>

    verify(description = "All core variables available in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_all_names("weight", "welfare", "urban",
                         "gd_type", "welfare_type", "code"))  |>
    verify(description = "No additional variables in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_only_names(core_var)) |>

    # code
    validate_if(is.character(code),
                description = "code should be character") |>
    validate_cols(not_na, code,
                  description = "code should not be missing") |>

    # weight
    validate_if(is.numeric(weight),
                description = "weight should be numeric") |>
    validate_cols(description = "weight should not be missing",
                  skip_chain_opts = TRUE,
                  error_fun = warning_append, not_na, weight) |>
    validate_rows(num_row_NAs, within_bounds(0, na_threshold), weight,
                  description = "weight NAs within %10") |>
    # welfare
    validate_if(is.numeric(welfare),
                description = "welfare should be numeric") |>
    validate_cols(description = "welfare should not be missing",
                  skip_chain_opts = TRUE,
                  error_fun = warning_append, not_na, welfare) |>
    validate_rows(num_row_NAs, within_bounds(0, na_threshold), welfare,
                  description = "welfare NAs within %10") |>

    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  df_var_list <- colnames(df)
  report0   <- data_validation_report()

  for (i in seq_along(df_var_list)) {

    df_var_mem <- df_var_list[i]

    if (df_var_mem %in% c("welfare_type", "gd_type", "urban")) {

      if (df_var_mem == "welfare_type"){

        validate(dlw_data, name = svy_id) |>
          validate_if(description = glue::glue("{df_var_mem} should be character"),
                      skip_chain_opts = TRUE,
                      error_fun = warning_append, is.character(welfare_type)) |>
          validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                        skip_chain_opts = TRUE,
                        error_fun = warning_append, not_na, df_var_mem) |>
          validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                        skip_chain_opts = TRUE,
                        error_fun = warning_append,
                        in_set(c("C", "I", "income")), df_var_mem) |>
          add_results(report0)

      } else if (df_var_mem == "urban"){

        validate(dlw_data, name = svy_id) |>
          validate_if(description = glue::glue("{df_var_mem} should be numeric"),
                      skip_chain_opts = TRUE,
                      error_fun = warning_append, is.numeric(urban)) |>
          validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                        skip_chain_opts = TRUE,
                        error_fun = warning_append, not_na, df_var_mem) |>
          add_results(report0)

      } else {

        validate(dlw_data, name = svy_id) |>
          validate_if(description = glue::glue("{df_var_mem} should be character"),
                      skip_chain_opts = TRUE,
                      error_fun = warning_append, is.character(gd_type)) |>
          validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                        skip_chain_opts = TRUE,
                        error_fun = warning_append, not_na, df_var_mem) |>
          add_results(report0)
      }

      validation_record0 <- get_results(report0, unnest = FALSE) |>
        setDT()
      validation_record <- rbind(validation_record, validation_record0,
                                 ignore.attr=TRUE)
    }
  }

  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}


#' Validate BIN dlw raw data
#'
#' @param dlw_data a BIN raw data in qs format
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq has_all_names has_only_names
#' @keywords internal
#'
#' @export
dlw_validation_bin <- function(dlw_data, svy_id){

  stopifnot("Data is not loaded" = !is.null(dlw_data))

  report   <- data_validation_report()
  core_var <- c("code", "year", "bins", "weight", "welfare", "verm",
                "vera", "region", "countryname")

  na_threshold <- round(nrow(df) * .10 )

  validate(dlw_data, name = svy_id) |>

    verify(description = "All core variables available in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_all_names("code", "year", "bins", "weight", "welfare", "verm",
                         "vera", "region", "countryname"))  |>
    verify(description = "No additional variables in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_only_names(core_var))  |>

    # code
    validate_if(is.character(code),
                description = "code should be character") |>
    validate_cols(not_na, code,
                  description = "code should not be missing") |>

    # year
    validate_if(is.numeric(year),
                description = "year should be numeric") |>
    validate_cols(not_na, year,
                  description = "year should not be missing") |>

    # bins
    validate_if(is.numeric(bins),
                description = "bins should be numeric") |>
    validate_cols(not_na, bins,
                  description = "bins should not be missing") |>

    # weight
    validate_if(is.numeric(weight),
                description = "weight should be numeric") |>
    validate_cols(description = "weight should not be missing",
                  skip_chain_opts = TRUE,
                  error_fun = warning_append, not_na, weight) |>
    validate_rows(num_row_NAs, within_bounds(0, na_threshold), weight,
                  description = "weight NAs within %10") |>
    # welfare
    validate_if(is.numeric(welfare),
                description = "welfare should be numeric") |>
    validate_cols(description = "welfare should not be missing",
                  skip_chain_opts = TRUE,
                  error_fun = warning_append, not_na, welfare) |>
    validate_rows(num_row_NAs, within_bounds(0, na_threshold), welfare,
                  description = "welfare NAs within %10") |>

    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  df_var_list <- colnames(df)
  report0   <- data_validation_report()

  for (i in seq_along(df_var_list)) {

    df_var_mem <- df_var_list[i]

    if (df_var_mem %in% c("verm", "vera", "region","countryname")) {

      validate(dlw_data, name = svy_id) |>
        validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                      skip_chain_opts = TRUE,
                      error_fun = warning_append, not_na, df_var_mem) |>
        add_results(report0)

      validation_record0 <- get_results(report0, unnest = FALSE) |>
        setDT()
      validation_record <- rbind(validation_record, validation_record0,
                                     ignore.attr=TRUE)
    }
  }


  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}


#' Validate HIST dlw raw data
#'
#' @param dlw_data a HIST raw data in qs format
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq has_all_names has_only_names
#' @keywords internal
#'
#' @export
dlw_validation_hist <- function(dlw_data, svy_id){

  stopifnot("Data data is not loaded" = !is.null(dlw_data))

  report   <- data_validation_report()
  core_var <- c("regioncode", "countrycode", "year", "datayear", "survname",
                "hhid", "hsize", "weight", "urban", "welfare", "coveragetype",
                "datatype", "code")

  na_threshold <- round(nrow(df) * .10 )

  validate(dlw_data, name = svy_id) |>

    verify(description = "All core variables available in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_all_names("regioncode", "countrycode", "year", "datayear",
                         "survname", "hhid", "hsize", "weight", "urban", "welfare",
                         "coveragetype", "datatype", "code"))  |>
    verify(description = "No additional variables in the data",
           skip_chain_opts = TRUE,
           error_fun = warning_append,
           has_only_names(core_var))  |>

    # regioncode
    validate_if(is.character(regioncode),
                description = "regioncode should be character") |>
    validate_cols(not_na, regioncode,
                  description = "regioncode should not be missing") |>

    # countrycode
    validate_if(is.character(countrycode),
                description = "countrycode should be character") |>
    validate_cols(not_na, countrycode,
                  description = "countrycode should not be missing") |>

    # year
    validate_if(is.numeric(year),
                description = "year should be numeric") |>
    validate_cols(not_na, year,
                  description = "year should not be missing") |>

    # datayear
    validate_if(is.numeric(datayear),
                description = "datayear should be numeric") |>
    validate_cols(not_na, datayear,
                  description = "datayear should not be missing") |>

    # hhid
    validate_cols(not_na, hhid,
                  description = "hhid should not be missing") |>

    # hsize
    validate_if(is.numeric(hsize),
                description = "hsize should be numeric") |>
    validate_cols(not_na, hsize,
                  description = "hsize should not be missing") |>

    # weight
    validate_if(is.numeric(weight),
              description = "weight should be numeric") |>
    validate_cols(description = "weight should not be missing",
                skip_chain_opts = TRUE,
                error_fun = warning_append, not_na, weight) |>
    validate_rows(num_row_NAs, within_bounds(0, na_threshold), weight,
                description = "weight NAs within %10") |>

    # urban
    validate_if(description = "urban should be numeric",
                skip_chain_opts = TRUE,
                error_fun = warning_append, is.numeric(urban)) |>
    validate_cols(description = "urban should not be missing",
                  skip_chain_opts = TRUE,
                  error_fun = warning_append, not_na, urban) |>

    # welfare
    validate_if(is.numeric(welfare),
                description = "welfare should be numeric") |>
    validate_cols(description = "welfare should not be missing",
                  skip_chain_opts = TRUE,
                  error_fun = warning_append, not_na, welfare) |>
    validate_rows(num_row_NAs, within_bounds(0, na_threshold), welfare,
                description = "welfare NAs within %10") |>

    # coveragetype
    validate_if(is.numeric(coveragetype),
                description = "coveragetype should be numeric") |>
    validate_cols(not_na, coveragetype,
                  description = "coveragetype should not be missing") |>

    # datatype
    validate_if(is.numeric(datatype),
                description = "datatype should be numeric") |>
    validate_cols(not_na, datatype,
                  description = "datatype should not be missing") |>

    # code
    validate_if(is.character(code),
                description = "code should be character") |>
    validate_cols(not_na, code,
                  description = "code should not be missing") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  df_var_list <- colnames(df)
  report0   <- data_validation_report()

  for (i in seq_along(df_var_list)) {

    df_var_mem <- df_var_list[i]

    if (df_var_mem %in% c("survname", "pid", "relation", "hhead", "age", "male")) {

      validate(dlw_data, name = svy_id) |>
        validate_cols(description = glue::glue("{df_var_mem} should not be missing"),
                      skip_chain_opts = TRUE,
                      error_fun = warning_append, not_na, df_var_mem) |>
        add_results(report0)

      validation_record0 <- get_results(report0, unnest = FALSE) |>
        setDT()
      validation_record <- rbind(validation_record, validation_record0,
                                 ignore.attr=TRUE)

    }
  }

  err_t <- validation_record[, .(table_name, message, type)]

  if (!rlang::env_has(.pipdata, "validation_report")){

    rlang::env_poke(.pipdata, "validation_report", validation_record)

  } else {

    compiled_result <- rbind(.pipdata$validation_report, validation_record)
    rlang::env_poke(.pipdata, "validation_report", compiled_result)

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipdata}).")

  }

  return(invisible(err_t))

}
