#' Validate GMD data and generate inventory report data
#'
#'
#' @param log Logical. Keep logging file, TRUE/FALSE default value is `TRUE`
#' @param save_log Logical. Save logging file, TRUE/FALSE default value is `TRUE`
#'
#' @return data.table, inventory report
#' @export
#'
#' @examples
#' \dontrun{
#' pipdata_validate_gmd(
#'   log = FALSE,
#'   save_log = FLASE
#' )
#' }
pipdata_validate_gmd <- function(
    log  = TRUE,
    save_log = TRUE
) {

  #### logging -----------------------------------------------------------------
  if (log) {

    pipfun::log_add("info", "Start validation workflow",
                    name = "pipdata_log",
                    args = list(log = log,
                                save_log  = save_log))
  }

  ### --------------------------------------------------------------------------

  # 0) set-up release and dlw data, inventory, and  metadata working folders
  pipfun::get_wrk_release(verbose = FALSE)

  pip_folders <- pipfun::get_pip_folders()

  dlw_data <- pip_folders$dlw_data
  dlw_inv  <- pip_folders$dlw_inventory
  dlw_meta <- pip_folders$dlw_metadata

  # check directory existence for working folders
  check_directory(dlw_data)
  check_directory(dlw_inv)
  check_directory(dlw_meta)

  ### -------------------------------------------------------------------------
  # 1) get list of local gmd datasets that are not yet validated
  gmd_new <- gmd_inv_new()

  if (is.null(gmd_new) || nrow(gmd_new) == 0){

    cli::cli_abort(
      "There is no new GMD local datasets to validate"
    )
  }

  # 2) Load validated gmd inventory file ---------------------------------------
  stamp::st_init(dlw_meta)
  valid_inv_file <- fs::path(dlw_meta, "gmd_valid_inv.qs2")
  if (!fs::is_file(valid_inv_file)) {

    old_inv <- NULL

    cli::cli_alert("GMD validation inventory file does not exist in the {.dir {dlw_data}} folder.")

  } else {
    old_inv <- tryCatch(

      pipload::load_gmd_valid_inv(),

      error = function(e) {
        msg <- glue::glue('Failed to read inventory file.')

        if (log) {

          pipfun::log_add("error", msg,
                          name = "pipdata_log",
                          logmeta = list(error = e))
        }

        NULL
        cli::cli_abort(msg)
      }
    )
  }

  # 3) get the list of datasets that are new and already validated -------------
  if (!is.null(old_inv)) {
    validate_this  <- gmd_to_validate(gmd_new, old_inv)

  } else {
    validate_this  <- NULL
  }

  if (!is.null(old_inv)) {
    validated_data <- gmd_validated(gmd_new, old_inv)
  } else {
    validated_data <- NULL
  }

  # 4) validate gmd local datasets ---------------------------------------------
  cli::cli_alert_info("Location of GMD data: {.dir {dlw_data}}")

  all_names <- unique(gmd_new$FileName)
  new_inv   <- vector("list", length(all_names))

  cli::cli_progress_bar("Downloading .qs", total = nrow(gmd_new))

  ##############################################################################
  # validation functions
  validation_functions <- list(
    GPWG   = dlw_validation_gpwg,
    GROUP  = dlw_validation_group,
    BIN    = dlw_validation_bin,
    HIST   = dlw_validation_hist,
    ALL    = dlw_validation_all,
    ASPIRE = dlw_validation_aspire,
    L      = dlw_validation_l,
    DEFAULT = dlw_validation_skip
  )

  # get the GMD data
  new_inv <- lapply(seq_len(nrow(gmd_new)), function(i) {

    stamp::st_init(dlw_data)

    file_name  <- gmd_new[["FileName"]][i]
    inv_pin_name  <- file_name |>
      fs::path_ext_remove() |>
      fs::path(ext = "qs2")
    nm         <- fs::path_ext_remove(file_name)
    md_type    <- gmd_new[["Module"]][i]
    data_avail <- gmd_new[["data_available"]][i]
    Checksum   <- gmd_new[["Checksum"]][i]

    pipeline_version  = 1

    file_id  <- file_name |>
      fs::path_ext_remove()

    # load GMD data from local repository
    out <- tryCatch({

      pipload::pip_read(
        id= file_id,
        dir = dlw_data)

      }, error = function(e) {
        
        msg <- glue::glue('Could not load data from GMD data folder.')

        if (log) {
          pipfun::log_add("error", msg, name = "pipdata_log",
                          args = list(file_path = dlw_data, file_name = file_id),
                          logmeta = list(error = e))
        }
        cli::cli_inform(msg)
        NULL
      })

    if (!is.null(out)) {

      version_info <- stamp::st_info(fs::path(dlw_data, file_id, ext = "qs2"))

      # Validate the data using the appropriate function
      check <- if (md_type %in% names(validation_functions)) {
        validation_functions[[md_type]](out, nm)
      } else {
        validation_functions[["DEFAULT"]](out, nm)
      }

      valid_status <- if (any(check[["type"]] == "error")) {
        check <- check[type == "error", .(message)]
        cli::cli_alert_danger("Validation failed for {nm} : {check$message}")
        "invalid"
      } else {
        "valid"
      }

      # Update the new_inv entry based on previous processes
      if (!is.null(validate_this) && (nm %in% validate_this$survey_id)) {

        row_svyid <- validate_this[survey_id == nm, "pipeline_version"]
        workflow_vrs <- row_svyid$pipeline_version + 1
        new_inv[[i]] <- data.table(
          survey_id         = nm,
          pipeline_version  = workflow_vrs,
          latest_version_id = version_info$catalog$latest_version_id,
          content_hash      = version_info$sidecar$content_hash,
          file_path         = version_info$sidecar$path,
          status            = valid_status,
          data_available    = "Yes",
          date_validated    = Sys.time(),
          Checksum          = Checksum
        )
      } else {
        new_inv[[i]] <- data.table(
          survey_id         = nm,
          pipeline_version  = pipeline_version,
          latest_version_id = version_info$catalog$latest_version_id,
          content_hash      = version_info$sidecar$content_hash,
          file_path         = version_info$sidecar$path,
          status            = valid_status,
          data_available    = "Yes",
          date_validated    = Sys.time(),
          Checksum          = Checksum
        )
      }

    } else {

      new_inv[[i]] <- data.table(
        survey_id         = nm,
        pipeline_version  = pipeline_version,
        latest_version_id = "",
        content_hash      = "",
        file_path         = "",
        status            = "",
        data_available    = "No",
        date_validated    = Sys.time(),
        Checksum          = Checksum
      )

    }

    # cli::cli_progress_update()

    return(new_inv[[i]])
  })

  # Done - validated data
  # cli::cli_progress_done()

  # ##############################################################################

  # 4. merge new_inv rows into final_inv ---------------------------------------
  final_inv <- dplyr::bind_rows(new_inv) |>
    pipload::survey_id_to_vars() |>
    tidyr::as_tibble() |>
    # tidyr::unnest(pin_version, keep_empty = TRUE) |>
    as.data.table()
  final_inv <- final_inv[, pipeline_version := fifelse(is.na(pipeline_version), 1, pipeline_version)]

  # update inventory file with the newly validated data
  if (!is.null(validated_data) && nrow(validated_data) !=0){

    base_file_name <- names(final_inv)
    final_inv <- rbind(validated_data, final_inv, ignore.attr=TRUE, fill = TRUE)

    setcolorder(final_inv, base_file_name)

  }

  # 5. save inventory file DLW inventory folder---------------------------------
  ## check if the inventory file is generated and save it to DLW inventory file
  stamp::st_init(dlw_meta)
  if (is.null(final_inv)) {

    cli::cli_alert_danger("Inventory file is not generated")

    if (log) {

      pipfun::log_add("error", "Inventory file is not generated",
                      name = "pipdata_log",
                      logmeta = list(dataset = "inventory"))
    }


  } else {

    pipload::pip_write(x = final_inv,
      id = "gmd_valid_inv",
      dir = dlw_meta,
      format  = "qs2")

    cli::cli_alert_success("Inventory file is saved at: {.dir {dlw_meta}}")

    if (log) {

      pipfun::log_add("info", "Inventory file is saved",
                      name = "pipdata_log",
                      logmeta = list(saved_at = dlw_meta))
    }

  }


  # 6. save validation report file in DLW inventory folder ---------------------
  # generate validation report
  valid_report <- get_validation_report()

  if (is.null(valid_report)) {

    cli::cli_alert_danger("Validation report data is not compiled")

    if (log) {

      pipfun::log_add("error", "Validation report is not available to save",
                      name = "pipdata_log")
    }


  } else {

    # survey names in validation data
    valid_all_names <- unique(valid_report$table_name)
    old_valid_report <- tryCatch(
      pipload::load_gmd_valid_report(),
      error = function(e) {
        msg <- "Failed to read validation report file."

        if (log){
          pipfun::log_add("error", msg,
                          name = "pipdata_log",
                          logmeta = list(error = e))
        }

        cli::cli_inform(msg)
        NULL
      }
    )

    if (!is.null(old_valid_report)){

      old_valid_report <- old_valid_report[!(table_name %in% valid_all_names), ]
      valid_report <- old_valid_report |> dplyr::bind_rows(valid_report)

    }

    pipload::pip_write(x = valid_report,
      id = "validation_report",
      dir = dlw_meta,
      format  = "qs2")

    cli::cli_alert_success("Validation report is saved")

    if (log) {

      pipfun::log_add("info", "Validation report is saved",
                      name = "pipdata_log",
                      logmeta = list(saved_as = "validation_report"))
    }
  }

  # 7. save logging file in DLW metadaa folder---------------------------------
  if (save_log && log) {
    pipfun::log_save(name = "pipdata_log", dir = dlw_meta, id = "dlw_validation_log")

    pipfun::log_add("info", "logging file is saved",
                    name = "pipdata_log",
                    logmeta = list(log_info_name = "dlw_validation_log"))

    cli::cli_alert_success("GMD logging file is saved")

  }

  invisible(NULL)
}


#' Check whether the working folder exists and abort if it does not
#'
#' @param wrk_folder A working folder path
#'
#' @returns Message if working folder is not available
#' @export
#'
#' @examples
#' \dontrun{
#' check_directory(dlw_data)
#' }
check_directory <- function(wrk_folder) {
  if (!dir.exists(wrk_folder)) {
    cli::cli_abort(
      "Folder {.dir {wrk_folder}} is not available"
    )
  }
}

#' Get datasets list that needs to be validated
#'
#' This function filters and returns the subset of new GMD records that match the validated inventory.
#'
#' @param gmd_new A data.table containing the new GMD records. Must include
#' columns `FileName` and `Checksum`.
#' @param inv_validated A data.table of validated inventory records
#' with `survey_id` and `Checksum` columns.
#'
#' @return A data.table containing only GMD records that match the validated inventory.
#' Returns all of `gmd_new` if `inv_validated` is NULL or empty. Result is returned invisibly.
#'
#' @examples
#' \dontrun{
#' validated_records <- gmd_to_validate(gmd_new, inv_validated)
#' }
gmd_to_validate <- function(gmd_new, inv_validated) {

  stopifnot("GMD new dataset is not loaded" = !is.null(gmd_new))

  # generate survey id to be used to merge the data with inventory file
  gmd_new0 <- gmd_new[
    , survey_id := as.character(fs::path_ext_remove(FileName))
  ][
    , .(survey_id, Checksum)
  ]

  if (is.null(inv_validated) || nrow(inv_validated) == 0) return(gmd_new)

  # keep records that are going to be validated
  new_gmd <- joyn::right_join(inv_validated, gmd_new0, by = c("survey_id", "Checksum"))
  new_gmd <- new_gmd[, !c(".joyn")]

  return(invisible(new_gmd))
}


#' Return Validated GMD Records
#'
#' This function filters the GMD dataset to return only the records that match entries in the validated inventory.
#'
#' @param gmd_new A data.table containing new GMD records. Must include
#' columns `FileName` and `Checksum`.
#' @param inv_validated A data.table of validated inventory records
#' with `survey_id` and `Checksum` columns.
#'
#' @return A data.table with only validated GMD records that exist in both `gmd_new` and `inv_validated`.
#'         Returns `NULL` if `inv_validated` is NULL or empty. Result is returned invisibly.
#'
#' @examples
#' \dontrun{
#' validated_gmd <- gmd_validated(gmd_new, inv_validated)
#' }
gmd_validated <- function(gmd_new, inv_validated) {

  stopifnot("GMD new dataset is not loaded" = !is.null(gmd_new))

  # generate survey id to be used to merge the data with inventory file
  gmd_new0 <- gmd_new[
    , survey_id := as.character(fs::path_ext_remove(FileName))
  ][
    , .(survey_id, Checksum)
  ]

  if (is.null(inv_validated) || nrow(inv_validated) == 0) return(NULL)

  # keep only validated GMD entries
  gmd_validated_records <- joyn::full_join(inv_validated,
                                           gmd_new0,
                                           by = c("survey_id", "Checksum"))
  gmd_validated_records <- gmd_validated_records[`.joyn` == "x", !c(".joyn")]

  return(invisible(gmd_validated_records))
}
