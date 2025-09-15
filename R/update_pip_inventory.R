update_pip_inventory <- function(inv_to_clean,
                                 process_data,
                                 date_valid = max(inv_to_clean$date_validated),
                                 test = FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses
  if(!inherits(date_valid, "POSIXct")){
    cli::cli_abort("date_valid should be POSIXct format")
  }

  # Check null surveys and clean

  null_ls <- names(Filter(is.null, process_data))

  if(length(null_ls)>0){

    pipfun::log_add(event = "info",
                    message = "Some surveys were not cleaned. Review logmeta to identify which ones.",
                    name = "pipdata_log",
                    logmeta = list(info = "null_svys_inf",
                                   surveys = null_ls))
  }

  process_data_clean <- process_data[!(names(process_data) %in% null_ls)]

  # Pip data cleaned

  svys <- lapply(lapply(process_data_clean,
                        \(x) as.list(x$pip_names)),
                 as.list)

  pip_inv <- data.frame(
    survey_id = rep(names(svys), lengths(svys)),
    pip_id = unlist(svys, use.names = FALSE)
  )

  # Bind versions

  vrs_dt <- format_vrs(process_data = process_data_clean,
                       version = "versions_data")

  vrs_mdt <- format_vrs(process_data = process_data_clean,
                        version = "versions_metadata")

  vrs <- vrs_dt |>
    joyn::left_join(vrs_mdt, by = c("survey_id", "pip_id"),
                    suffix = c("_data","_metadata"),
                    relationship = "many-to-many",
                    reportvar = FALSE,
                    verbose = FALSE)

  # Add info from DLW inventory

  pip_inv <- pip_inv |>
    unique()|>
    joyn::left_join(vrs, by = c("survey_id", "pip_id"),
                    reportvar = FALSE,
                    verbose = FALSE)|>
    joyn::left_join(inv_to_clean, by = "survey_id",
                    relationship = "many-to-one",
                    reportvar = FALSE,
                    verbose = FALSE)|>
    collapse::frename(version = "version_dlw",
                      created = "created_dlw",
                      hash    = "hash_dlw",
                      pins_folder = "pins_dlw")

  # Save master inventory

  old_pip_inv <- pipload::load_pip_master_inventory()

  new_pip_inv <- pip_inv|>
    collapse::rowbind(old_pip_inv)|>
    collapse::funique()|>
    as.data.table()

  if(test){

    board_release <- pins::board_folder("//tsclient/P/03.pip/pip_data/master_inventory")
  }else{

    board_master <- pipfun::get_pins_boards(board = "pip_master_inventory")
  }

  pipload::pip_write(board                 = board_master,
                     x                     = new_pip_inv,
                     pin_name              = "pip_master_inventory")

  # Save release inventory

  pfw <- pipload::pip_load_aux("pfw", verbose = FALSE)

  pfw_release <- pfw |>
    collapse::fsubset(inpovcal == 1)|>
    collapse::fselect(country_code,
                      surveyid_year,
                      survey_acronym)|>
    collapse::funique()|>
    as.data.table()

  release_pip_inv  <- new_pip_inv[pfw_release,
                                  on = .(country_code,
                                         surveyid_year,
                                         survey_acronym),
                                  nomatch = 0][ # Need to change it for a warning
                                    date_validated < date_valid]

  if(test){

    board_release <- pins::board_folder("//tsclient/P/03.pip/pip_data/release_inventory")
  }else{

    board_release <- pipfun::get_pins_boards(board = "pip_inventory")
  }


  pipload::pip_write(board                 = board_release,
                     x                     = release_pip_inv,
                     pin_name              = "pip_release_inventory" )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(new_pip_inv)

}

format_vrs <- function(process_data,
                       version = c("versions_data",
                                   "versions_metadata")) {

  version <- match.arg(version)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dt <- data.table::rbindlist(lapply(process_data,

                               \(x) data.table::rbindlist(
                                 lapply(x[[version]], as.data.table),
                                 idcol = "pip_id")),

                              idcol = "survey_id")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
