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

  validation_report <-
    validation_report[, module_type := fifelse(module_type %in%
                                                 c("GPWG", "GROUP", "BIN",
                                                   "HIST", "ALL", "ASPIRE", "L"),
                                               module_type, "OTHER")]

  # extract master data version
  validation_report <-
    validation_report[, vermast := sub(".*_([^_]+)_M.*", "\\1", table_name)]

  # extract adaptation version
  validation_report <-
    validation_report[, veralt  := sub(".*_M_([^_]+)_A.*", "\\1", table_name)]

  # extract country code
  validation_report <-
    validation_report[, country_code := fifelse(module_type %in%
                                                  c("GPWG", "GROUP", "BIN",
                                                    "HIST", "ALL", "ASPIRE", "L"),
                                                sub("^(.{3}).*", "\\1", table_name), NA)]

  # reference year
  validation_report <-
    validation_report[, rf_year := fifelse(module_type %in%
                                             c("GPWG", "GROUP", "BIN",
                                               "HIST", "ALL", "ASPIRE", "L"),
                                           sub("^[^_]*_([^_]*)_.*", "\\1", table_name), NA)]

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
    valid_data[, .(n = .N), by = data_status]
  }
}

#' List of validation result by survey ID and module type
#'
#' @param e_type Character. Validation result type, error/warning, defulat is `error`
#' @param m_type Character. Module type, GPWG/GROUP/BIN/HIST/OTHER
#'
#' @returns data in DT format
#' @export
#'
#' @examples
#' \dontrun{
#' get_validation_list(report_data,
#'   e_type = "warning",
#'   m_type = "GPWG"
#' )
#' }
get_validation_list <- function(
    # root_dir = Sys.getenv("PIP_ROOT_DIR"),
    # maindir = "PIP_ingestion_pipeline_v2/dlw_repository",
    # dlw_inv_folder = "dlw_inventory",
    # release_lbl = NULL,
    # release_idn = NULL,
    # report_type = "validation",
    e_type = c("error", "warning", "success"),
    m_type = NULL){

  e_type <- match.arg(e_type)

  # load validation report data
  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()
  dlw_meta <- pip_folders$dlw_metadata

  rp_data <- pipload::load_gmd_valid_report()

  # ensure the validation report data is loaded
  stopifnot("Validation report data is not loaded" = !is.null(rp_data))

  # filter data based on event type (default is "error")
  rp_data <- rp_data[(type == e_type), ]

  if (!is.null(m_type)) {

    if (m_type %in% c("GPWG", "GROUP", "BIN", "HIST", "ALL", "ASPIRE", "L", "OTHER")) {

      cli::cli_alert_info("List of {e_type} by survey id and module type {m_type}")

      DT::datatable(unique(rp_data[(module_type %in% m_type), .(table_name, description)]),
                    options = list(autoWidth = TRUE, pageLength = 10,
                                   scrollY = "200px"),
                    caption = glue::glue('List of {e_type} for {m_type}'))

    } else {

      cli::cli_abort("Specified module type {m_type} is not part of module list")

    }

  } else {

    cli::cli_alert_info("Descriptions of {e_type} by survey id and module type")

    DT::datatable(unique(rp_data[, .(table_name, description, module_type)]),
                  options = list(autoWidth = TRUE, pageLength = 10,
                                 scrollY = "200px"),
                  caption = 'Description of validation check by module types')
  }

}


#' List of validation result by country and module type
#'
#' @param e_type Character. Validation result type, error/warning, defulat is `error`
#' @param ctry Character. Country 3-digits ISO code, defualt is `NULL`
#' @param by_year Logical. Defualt is `FALSE`. If `TRUE`, generates wide table by module types
#' @param r_year Character. Ref years, defualt is `NULL`.
#' If value(s) is provided, generates wide table for specified ref years by module types.
#' Note: If r_year has value(s), `by_year` argument should be `FALSE`
#'
#' @returns data in DT format
#' @export
#'
#' @examples
#' \dontrun{
#' get_validation_ctry(report_data,
#'   e_type = "warning",
#'   ctry = c("ARG", "CHL", "HRV"),
#'   by_year = TRUE
#' )
#' }
get_validation_ctry <- function(
    e_type = c("error", "warning", "success"),
    ctry = NULL,
    by_year = FALSE,
    r_year = NULL) {

  e_type <- match.arg(e_type)

  # load validation report data
  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()
  dlw_meta <- pip_folders$dlw_metadata

  rp_data <- pipload::load_gmd_valid_report()

  # ensure the validation report data is loaded
  stopifnot("Validation report data is not loaded" = !is.null(rp_data))

  # filter data based on event type (default is "error")
  rp_data <- rp_data[(type == e_type), ]

  if (!is.null(ctry)) {

    if (!is.null(r_year)) {

      cli::cli_alert_info("List of {e_type} by country {ctry}, ref years {r_year} and module types")
      rp_data[(country_code %in% ctry & rf_year %in% r_year),
              .(freq = .N), by = .(country_code, rf_year, module_type)] |>
        dcast(country_code + module_type ~ rf_year, value.var = "freq") |>
        DT::datatable(caption = 'Selected countries and specified ref years by all module types')


    } else if (is.null(r_year) & by_year == TRUE){

      cli::cli_alert_info("List of {e_type} by country {ctry}, module types and ref years")
      rp_data[(country_code %in% ctry),
              .(freq = .N), by = .(country_code, rf_year, module_type)] |>
        dcast(country_code + module_type ~ rf_year, value.var = "freq") |>
        DT::datatable(caption = 'Selected countries and by all module types and ref years')

    } else {

      cli::cli_alert_info("List of {e_type} by country {ctry} and module types")

      rp_data[(country_code %in% ctry),
              .(freq = .N), by = .(country_code, module_type)] |>
        DT::datatable(caption = 'Selected countries and by all module types')

    }

  } else {

    if (by_year == TRUE){

      cli::cli_alert_info("List of {e_type} by country, module types and ref years")
      rp_data[, .(freq = .N), by = .(country_code, rf_year, module_type)] |>
        dcast(country_code + module_type ~ rf_year, value.var = "freq") |>
        DT::datatable(caption = 'By country, module types and ref years')

    } else {

      cli::cli_alert_info("List of {e_type} by country and module types")
      rp_data[, .(freq = .N), by = .(country_code, module_type)] |>
        DT::datatable(caption = 'By country and module types')

    }
  }

}

