update_pip_inventory <- function(inv_to_clean,
                                 clean_data,
                                 pins_versions_data,
                                 pins_versions_metadata) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  svys <- lapply(lapply(clean_data, names), as.list)
  svys <- Filter(function(y) !(is.list(y) && length(y) == 0), svys)

  pip_inv <- data.frame(
    survey_id = rep(names(svys), lengths(svys)),
    pip_id = unlist(svys, use.names = FALSE)
  )

  vrs_dt <- data.table::rbindlist(pins_versions_data, idcol = "pip_id")

  vrs_mdt <- data.table::rbindlist(pins_versions_metadata, idcol = "pip_id")

  vrs <- vrs_dt |>
    joyn::left_join(vrs_mdt, by = "pip_id",
                    suffix = c("_data","_metadata"),
                    reportvar = FALSE,
                    verbose = FALSE)

  pip_inv <- pip_inv |>
    joyn::left_join(vrs, by = "pip_id",
                    reportvar = FALSE,
                    verbose = FALSE)|>
    joyn::left_join(inv_to_clean, by = "survey_id",
                    relationship = "many-to-one",
                    reportvar = FALSE,
                    verbose = FALSE)

  board <- pipfun::get_pins_boards()$pip_inventory

  # inv_name <- paste("pip_inv_",format(Sys.time(), "%Y%m%d"), sep = "")

  pins::pin_write(board                 = board,
                   x                     = pip_inv,
                   name                  = "pip_inventory",
                   force_identical_write = FALSE,
                   type                  = "qs",
                  versioned              = TRUE )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(pip_inv)

}
