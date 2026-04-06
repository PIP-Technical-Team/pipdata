#' Load DLW survey data and prepare it for the PIP pipeline
#'
#' Loads a survey from the datalibweb (DLW) storage using
#' [pipload::load_dlw_data()], converts it to a `data.table`, attaches
#' survey-ID components as attributes, and adds the PIP class.
#'
#' @param inv A one-row data.frame (or data.table) from the DLW inventory.
#'   Must contain a `survey_id` column.
#'
#' @return A `data.table` with PIP class and survey-ID attributes.
#'
#' @family pd_process_data pipeline
#' @export
inv_dlw_load <- function(inv) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Load survey files

  dt <- pipload::load_dlw_data(id_name = inv$survey_id, verbose = FALSE)

  # Add data from inventory to attributes of data table and add pip class

  dt <- data_to_dt(dt, inv$survey_id)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)
}

#' Convert loaded DLW data to a data.table with survey attributes
#'
#' Ensures the input is a `data.table`, parses the `survey_id` into
#' individual attributes, and assigns the PIP S3 class via
#' [pipload::as_pip()].
#'
#' @param dt A data.frame or data.table of survey micro-data.
#' @param survey_id Character scalar. The survey identifier string
#'   (e.g., `"ALB_2012_LSMS_V01_M_V01_A_PIP_ALL"`).
#'
#' @return A `data.table` with PIP class and survey-ID attributes.
#'
#' @family pd_process_data pipeline
#' @keywords internal
data_to_dt <- function(dt, survey_id) {
  #--------- defenses --------------------------

  if (!is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }

  #--------- leaving just the 'label' attribute ---------

  # dt <- only_labels(dt)

  #--------- Survey ID and its components ---------

  dt <- survey_id_to_attr(dt, survey_id)

  #--------- Add class ---------

  dt <- pipload::as_pip(dt)

  # Temporary fix

  dt[,
    module := NULL
  ]

  return(dt)
}

# only_labels <- function(dt) {
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # computations   ---------
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   nn <- names(dt)

#   for (j in seq_along(nn)) {
#     ats <- attributes(dt[[j]])
#     atsn <- names(ats)
#     to_remove <- atsn[!grepl("label", atsn)]

#     for (i in seq_along(to_remove)) {
#       attr(dt[[j]], to_remove[i]) <- NULL
#     }
#   }

#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   # Return   ---------
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   return(dt)
# }

#' Parse a survey ID string and attach its components as data.table attributes
#'
#' Splits `survey_id` on `"_"` into country code, year, survey acronym,
#' master/alt versions, collection, and module.
#' Sets derived attributes (`tool`, `gd_type`, `survey_id`) and subsets
#' columns to only welfare, weight, and demographic variables.
#'
#' @param dt A `data.table` of survey micro-data.
#' @param survey_id Character scalar. The full survey identifier string.
#'
#' @return The input `data.table` with added attributes and reduced columns.
#'
#' @family pd_process_data pipeline
#' @keywords internal
survey_id_to_attr <- function(dt, survey_id) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Computations -------

  # Variables to attributes (Add them to package later in pipload)
  cnames <-
    c(
      "country_code",
      "surveyid_year",
      "survey_acronym",
      "vermast",
      "M",
      "veralt",
      "A",
      "collection",
      "module"
    )

  svy_id_attr <- tstrsplit(survey_id, "_", fixed = TRUE, names = cnames)

  attributes(dt) <- c(attributes(dt), svy_id_attr)

  attr(dt, "tool") <- ifelse(attributes(dt)$module == "ALL", "TB", "PC")
  attr(dt, "vermast") <- tolower(attributes(dt)$vermast)
  attr(dt, "veralt") <- tolower(attributes(dt)$veralt)
  attr(dt, "surveyid_year") <- as.numeric(attributes(dt)$surveyid_year)
  attr(dt, "M") <- NULL
  attr(dt, "A") <- NULL
  attr(dt, "survey_id") <- survey_id

  # Add gd_type

  if ("gd_type" %in% names(dt)) {
    attr(dt, "gd_type") <- collapse::funique(dt$gd_type)
  }

  # Check year and surveyid_year is the same

  if ("year" %in% names(dt)) {
    year <- unique(dt$year)
    year <- purrr::discard(year, is.na)
    surveyid_year <- attributes(dt)$surveyid_year

    if (surveyid_year != year) {
      rlang::abort(
        "Year variables in DLW survey different from survey_id year in inventory.
                     {cli::col_blue('Surveyid_year added to attributes')}",
        class = c("piperr", "yr_wrng"),
        use_cli_format = TRUE
      )
    }
  }

  # Remove variables

  fnames <- c(
    grep("^(welf|weight|subnatid|edu)", names(dt), value = TRUE),
    "urban",
    "area",
    "age",
    "male",
    "gender",
    "literacy",
    "school"
  )

  to_keep <- names(dt)[names(dt) %in% fnames]

  dt <- dt[, ..to_keep]

  # Temporary fix for it to work on pip_class:

  mod <- attributes(dt)$module

  dt <- dt[, module := mod]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)
}
