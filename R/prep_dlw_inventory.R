# Add global variables to avoid NSE notes in R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "creationtime",
      "fullname",
      "lastwritetime",
      "module",
      "survey_id",
      "surveyid_year",
      ".",
      "!!",
      ":="
    )
  )
}

#' @title Prepare DatalibWeb inventory
#'
#' @description takes dlw inventory in csv form in the official folder structure and format
#' it to be included in the pipeline. The original csv file is updated each time
#' the dlw inventory is updated
#'
#' @param dlw_dir character: path of dlw raw data
#'
#' @return data.table
#' @export
#'
#' @examples
#' library(pipload)
#' dlw_dir <- pip_create_globals()$DLW_RAW_DIR
#' prep_dlw_inventory(dlw_dir)
prep_dlw_inventory <- function(dlw_dir) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # directoires and paths   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  dlw_inv_path <- fs::path(dlw_dir,"_Inventory",
                       "DLWRAW_all_DTAs", ext = "txt")

  dlw_inv_path <- fs::path(dlw_dir,"_Inventory",
                       "DLWRAW_all_DTAs", ext = "csv")

  if (!fs::file_exists(dlw_inv_path)) {

    msg     <- c(
      "File does not exists",
      "x" = "{dlw_inv_path} not found.",
      "i" = "check connection or {.field pipload} globals"
    )
    cli::cli_abort(msg,
                   class = "pipdata_error"
    )
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## variables --------
  id_vars <-
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


  pip_modules <-
    c("GPWG",
      "ALL",
      "BIN",
      "GROUP",
      "HIST")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # clean data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  dlw_inv <- as.data.table(readr::read_csv(dlw_inv_path,
                                           name_repair    = tolower,
                                           progress       = FALSE,
                                           show_col_types = FALSE))

  dlw_inv[,
          survey_id := {
            x <- stringr::str_extract(fullname, "[^\\\\]+\\.dta$")
            x <- stringr::str_replace_all(x, "\\.dta$", "")
          }
  ][,
    `:=`(
      creationtime  = lubridate::mdy_hms(creationtime),
      lastwritetime = lubridate::mdy_hms(lastwritetime)
    )]


  # add variables from survey ID
  dlw_inv[, (id_vars) := tstrsplit(survey_id, split = c("_"), fixed = TRUE)]

  dlw_inv <- dlw_inv[module %chin% pip_modules] # keep important modules
  dlw_inv[, c("M", "A")   := NULL] # remove M and A

  # Classify as PC or TB
  dlw_inv[,
          `:=`(
            surveyid_year = as.numeric(surveyid_year),
            tool          = fifelse(module == "ALL", "TB", "PC")
          )]

  return(dlw_inv)
}
