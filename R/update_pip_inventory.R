update_pip_inventory <- function(inv_to_clean,
                                 clean_data,
                                 pins_versions_data,
                                 pins_versions_metadata) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Pip data cleaned

  svys <- lapply(lapply(clean_data, names), as.list) # temporary
  svys <- Filter(function(y) !(is.list(y) && length(y) == 0), svys)

  pip_inv <- data.frame(
    survey_id = rep(names(svys), lengths(svys)),
    pip_id = unlist(svys, use.names = FALSE)
  )

  # Bind versions

  vrs_dt <- data.table::rbindlist(pins_versions_data, idcol = "pip_id")

  vrs_mdt <- data.table::rbindlist(pins_versions_metadata, idcol = "pip_id")

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
    collapse::funique()

  board_master <- pipfun::get_pins_boards(board = "pip_master_inventory")

  pins::pin_write(board                 = board_master,
                   x                     = new_pip_inv,
                   name                  = "pip_master_inventory",
                   force_identical_write = FALSE,
                   type                  = "qs",
                  versioned              = TRUE )

  # Save release inventory

  pfw <- pipload::pip_load_aux("pfw", verbose = FALSE)

  pfw_release <- pfw |>
    collapse::fsubset(inpovcal == 1)|>
    collapse::fselect(country_code,
                      surveyid_year,
                      survey_acronym)

  release_pip_inv  <- new_pip_inv[pfw_release, on = .(country_code,
                                                      surveyid_year,
                                                      survey_acronym)]

  board_release <- pipfun::get_pins_boards(board = "pip_release_inventory")

  pins::pin_write(board                 = board_release,
                  x                     = release_pip_inv,
                  name                  = "pip_master_inventory",
                  force_identical_write = FALSE,
                  type                  = "qs",
                  versioned              = TRUE )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(new_pip_inv)

}
