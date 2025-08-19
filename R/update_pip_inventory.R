update_pip_inventory <- function(inv_to_clean,
                                 process_data,
                                 date_valid = max(inv_to_clean$date_validated)) {

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

  vrs_dt <- sapply(process_data_clean, \(x){ x$versions_data })
  vrs_dt <- purrr::flatten(vrs_dt)
  vrs_dt <- data.table::rbindlist(vrs_dt, idcol = "pip_id")

  vrs_mdt <- sapply(process_data_clean, \(x){ x$versions_metadata })
  vrs_mdt <- purrr::flatten(vrs_mdt)
  vrs_mdt <- data.table::rbindlist(vrs_mdt, idcol = "pip_id")

  vrs <- vrs_dt |>
    joyn::left_join(vrs_mdt, by = "pip_id",
                    suffix = c("_data","_metadata"),
                    reportvar = FALSE,
                    verbose = FALSE)

  # Add info from DLW inventory

  pip_inv <- pip_inv |>
    joyn::left_join(vrs, by = "pip_id",
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

  board_master <- pipfun::get_pins_boards(board = "pip_master_inventory")

  pipload::pip_write(board                 = board_master,
                     x                     = new_pip_inv,
                     pin_name                  = "pip_master_inventory")

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
                                  nomatch = 0][
                                    date_validated < date_valid]

  board_release <- pipfun::get_pins_boards(board = "pip_inventory")

  pipload::pip_write(board                 = board_release,
                     x                     = release_pip_inv,
                     pin_name              = "pip_release_inventory" )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(new_pip_inv)

}
