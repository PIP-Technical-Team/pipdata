#' @title Prepare DatalibWeb inventory
#'
#' @description takes dlw inventory in csv form in the official folder structure and format
#' it to be included in the pipeline. The original csv file is updated each time
#' the dlw inventory is updated
#'
#' @param root_dir character: root directory of the PIP data
#' @param dlw_dir character: path of dlw raw data
#' @param force logical: force update of data
#'
#' @return logical. TRUE if data changed. FALSE otherwise
#' @export
#'
#' @examples
#' update_dlw_inventory()
update_dlw_inventory <-
  function(root_dir = Sys.getenv("PIP_ROOT_DIR"),
           dlw_dir  = pipfun::pip_create_globals(root_dir)$DLW_RAW_DIR,
           force    = FALSE)
    {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # directories and paths   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  dlw_inv_path <- fs::path(dlw_dir,"_Inventory")

  dlw_inv_file <- fs::path(dlw_inv_path,
                           "DLWRAW_all_DTAs", ext = "csv")

  if (!fs::file_exists(dlw_inv_file)) {

    msg     <- c(
      "File does not exists",
      "x" = "{dlw_inv_file} not found.",
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

  dlw_inv <- fread(file = dlw_inv_file,
              showProgress = FALSE)
  setnames(dlw_inv,tolower)


  dlw_inv[,
          fullname := {
            x <- gsub("\\\\", "/", fullname)
            x <- gsub(root_dir, "", x)
            x
          }
  ][,
    survey_id := {
      fullname |>
        fs::path_file() |>
        fs::path_ext_remove()
    }
  ][,
    `:=`(
      creationtime  = lubridate::mdy_hms(creationtime),
      lastwritetime = lubridate::mdy_hms(lastwritetime)
    )]


  # add variables from survey ID
  dlw_inv <- suppressWarnings(pipload::survey_id_to_vars(dlw_inv))
  dlw_inv <- na.omit(dlw_inv)
  dlw_inv <- dlw_inv[module %chin% pip_modules] # keep important modules

  setorder(dlw_inv, country_code, surveyid_year, survey_acronym, vermast, veralt)


  # check if data has changed

  status <- pipfun::pip_sign_save(x       =  dlw_inv,
                                  measure = "dlw_inventory",
                                  msrdir  = dlw_inv_path,
                                  force   = force)


  return(invisible(status))
}
