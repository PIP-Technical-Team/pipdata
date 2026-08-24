#' Validate GMD data and generate inventory report data
#'
#' Logging is unconditional. The function writes `dlw_validation_inf` entries
#' for validation start, no-new-data, load/validation failures, inventory and
#' report workflow phases. Error conditions are stored as `condition_msg` and
#' the discriminator in `logmeta$error` is always a string.
#'
#' @note This function expects a working release to be configured via
#'   [pipfun::setup_working_release()]. When called from
#'   [pipdata_dlw_process()], the release is already set. When called
#'   standalone, ensure `setup_working_release()` has been invoked first.
#'
#' @param verbose Logical. Controls verbosity of downstream I/O calls
#'   (including [pipload::pip_write()]). Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#' @return Invisibly returns `NULL`; validation inventory and report artifacts
#'   are persisted as side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' pipdata_validate_gmd()
#' }
pipdata_validate_gmd <- function(
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  ### --------------------------------------------------------------------------

  # 0) set-up release and dlw data, inventory, and  metadata working folders
  pip_folders <- pipfun::get_pip_folders()

  # check directory existence for working folders
  check_directory(pip_folders$dlw_data)
  check_directory(pip_folders$dlw_inventory)
  check_directory(pip_folders$dlw_metadata)

  ### -------------------------------------------------------------------------
  # 1) get list of local gmd datasets that are not yet validated
  gmd_new <- tryCatch(
    dlw_gmd_unvalidated(),
    error = function(e) {
      pipfun::log_error(
        "Failed to load unvalidated GMD data.",
        name = "pipdata_log",
        logmeta = list(
          error = .logtype_dlw_validation,
          phase = "catalog_load",
          condition_msg = conditionMessage(e)
        )
      )
      rlang::abort("Failed to load unvalidated GMD data.", parent = e)
    }
  )

  if (is.null(gmd_new) || nrow(gmd_new) == 0) {
    pipfun::log_info(
      "No new GMD data was available for validation.",
      name = "pipdata_log",
      logmeta = list(
        info = .logtype_dlw_validation,
        phase = "no_new_data",
        n_surveys = 0L
      )
    )
    return(invisible(NULL))
  }

  # 2) Load validated gmd inventory file ---------------------------------------
  valid_inv_file <- fs::path(
    pip_folders$dlw_metadata,
    "gmd_valid_inv.qs2",
    "gmd_valid_inv.qs2"
  )
  if (!fs::is_file(valid_inv_file)) {
    old_inv <- NULL

    cli::cli_alert(
      "GMD validation inventory file does not exist in the {.dir {pip_folders$dlw_data}} folder."
    )
  } else {
    old_inv <- tryCatch(
      # stamp::st_load(fs::path(dlw_meta, "gmd_valid_inv.qs2"), alias = "dlw_meta"),
      pipload::load_gmd_valid_inv(),

      error = function(e) {
        msg <- glue::glue('Failed to read inventory file.')

        pipfun::log_error(
          msg,
          name = "pipdata_log",
          logmeta = list(
            error = .logtype_dlw_validation,
            phase = "inv_load_fail",
            artifact = "gmd_valid_inv",
            path = valid_inv_file,
            condition_msg = conditionMessage(e)
          )
        )

        cli::cli_abort(msg)
      }
    )
  }

  # 3) get the list of datasets that are new and already validated -------------
  if (!is.null(old_inv)) {
    validate_this <- gmd_to_validate(gmd_new, old_inv)
  } else {
    validate_this <- NULL
  }

  if (!is.null(old_inv)) {
    validated_data <- gmd_validated(gmd_new, old_inv)
  } else {
    validated_data <- NULL
  }

  # 4) validate gmd local datasets ---------------------------------------------
  cli::cli_alert_info("Location of GMD data: {.dir {pip_folders$dlw_data}}")

  gmd_new <- gmd_new[data_available == "Yes", ]

  if (nrow(gmd_new) == 0L) {
    pipfun::log_info(
      "No available GMD data was found for validation.",
      name = "pipdata_log",
      logmeta = list(
        info = .logtype_dlw_validation,
        phase = "no_new_data",
        n_surveys = 0L
      )
    )
    return(invisible(NULL))
  }

  # The validation inventory is keyed by survey_id. If the local acquisition
  # inventory contains multiple checksums for one file, validate only the
  # deterministically newest version rather than creating duplicate keys.
  sort_cols <- intersect(
    c("FileName", "Vermast", "Veralt", "Checksum"),
    names(gmd_new)
  )
  if (length(sort_cols) > 1L) {
    data.table::setorderv(
      gmd_new,
      cols = sort_cols,
      order = c(1L, rep(-1L, length(sort_cols) - 1L))
    )
  }
  gmd_new <- gmd_new[!duplicated(FileName)]

  n_surveys <- nrow(gmd_new)
  pipfun::log_info(
    "DLW validation started.",
    name = "pipdata_log",
    logmeta = list(
      info = .logtype_dlw_validation,
      phase = "start",
      n_surveys = n_surveys
    )
  )

  all_names <- unique(gmd_new$FileName)
  new_inv <- vector("list", length(all_names))

  cli::cli_progress_bar("Downloading .qs", total = nrow(gmd_new))

  ##############################################################################
  # validation module ids (data-driven engine)
  validation_modules <- list(
    GPWG = "gpwg",
    GROUP = "group",
    BIN = "bin",
    HIST = "hist",
    ALL = "all",
    ASPIRE = "aspire",
    L = "l",
    DEFAULT = "skip"
  )

  # get the GMD data
  new_inv <- lapply(seq_len(nrow(gmd_new)), function(i) {
    file_name <- gmd_new[["FileName"]][i]
    inv_pin_name <- file_name |>
      fs::path_ext_remove() |>
      fs::path(ext = "qs2")

    nm <- fs::path_ext_remove(file_name)
    md_type <- gmd_new[["Module"]][i]
    data_avail <- gmd_new[["data_available"]][i]
    Checksum <- gmd_new[["Checksum"]][i]

    pipeline_version = 1L

    file_id <- file_name |>
      fs::path_ext_remove() |>
      tolower()

    # load GMD data from local repository
    out <- tryCatch(
      {
        pipload::load_dlw_data(
          id_name = file_id
        )
      },
      error = function(e) {
        msg <- glue::glue('Could not load data from GMD data folder.')

        pipfun::log_error(
          msg,
          name = "pipdata_log",
          logmeta = list(
            error = .logtype_dlw_validation,
            phase = "load",
            survey = fs::path_ext_remove(file_name),
            file_name = file_name,
            module = md_type,
            path = pip_folders$dlw_data,
            condition_msg = conditionMessage(e)
          )
        )
        cli::cli_inform(msg)
        NULL
      }
    )

    if (!is.null(out)) {
      file_id <- file_id |>
        fs::path_ext_set("qs2")

      version_info <- tryCatch(
        stamp::st_info(file_id, alias = "dlw"),
        error = function(e) {
          pipfun::log_error(
            "Failed to read GMD artifact metadata.",
            name = "pipdata_log",
            logmeta = list(
              error = .logtype_dlw_validation,
              phase = "artifact_info_fail",
              survey = nm,
              file_name = file_name,
              module = md_type,
              condition_msg = conditionMessage(e)
            )
          )
          rlang::abort("Failed to read GMD artifact metadata.", parent = e)
        }
      )

      # Validate the data using the data-driven engine
      check <- if (md_type %in% names(validation_modules)) {
        dlw_validation_engine(out, nm, validation_modules[[md_type]])
      } else {
        dlw_validation_engine(out, nm, validation_modules[["DEFAULT"]])
      }

      valid_status <- if (any(check[["type"]] == "error")) {
        validation_messages <- check[type == "error", message]
        pipfun::log_error(
          "GMD validation failed.",
          name = "pipdata_log",
          logmeta = list(
            error = .logtype_dlw_validation,
            phase = "validation",
            survey = nm,
            module = md_type,
            validation_messages = as.character(validation_messages)
          )
        )
        cli::cli_alert_danger(
          "Validation failed for {nm} : {validation_messages}"
        )
        "invalid"
      } else {
        "valid"
      }

      # Update the new_inv entry based on previous processes
      if (!is.null(validate_this) && (nm %in% validate_this$survey_id)) {
        row_svyid <- validate_this[survey_id == nm, "pipeline_version"]
        workflow_vrs <- row_svyid$pipeline_version + 1L
        new_inv[[i]] <- data.table(
          survey_id = nm,
          pipeline_version = workflow_vrs,
          latest_version_id = version_info$catalog$latest_version_id,
          content_hash = version_info$sidecar$content_hash,
          file_path = version_info$sidecar$path,
          status = valid_status,
          data_available = "Yes",
          date_validated = Sys.time(),
          Checksum = Checksum
        )
      } else {
        new_inv[[i]] <- data.table(
          survey_id = nm,
          pipeline_version = pipeline_version,
          latest_version_id = version_info$catalog$latest_version_id,
          content_hash = version_info$sidecar$content_hash,
          file_path = version_info$sidecar$path,
          status = valid_status,
          data_available = "Yes",
          date_validated = Sys.time(),
          Checksum = Checksum
        )
      }
    } else {
      new_inv[[i]] <- data.table(
        survey_id = nm,
        pipeline_version = pipeline_version,
        latest_version_id = "",
        content_hash = "",
        file_path = "",
        status = "",
        data_available = "No",
        date_validated = Sys.time(),
        Checksum = Checksum
      )
    }

    # cli::cli_progress_update()

    return(new_inv[[i]])
  })

  # Done - validated data
  # cli::cli_progress_done()

  # ##############################################################################

  # 4. merge new_inv rows into final_inv ---------------------------------------
  # Note: Filter(Negate(is.null), ...) is not needed here because all lapply
  # branches explicitly return a data.table (even on load failure). Kept as
  # documentation for future developers if that assumption ever changes.
  final_inv <- data.table::rbindlist(new_inv, fill = TRUE) |>
    pipload::survey_id_to_vars()
  # tidyr::unnest(pin_version, keep_empty = TRUE)
  final_inv <- final_inv[,
    pipeline_version := fifelse(is.na(pipeline_version), 1L, pipeline_version)
  ]

  # update inventory file with the newly validated data
  if (!is.null(validated_data) && nrow(validated_data) != 0) {
    base_file_name <- names(final_inv)
    final_inv <- rbind(
      validated_data,
      final_inv,
      ignore.attr = TRUE,
      fill = TRUE
    )

    data.table::setcolorder(final_inv, base_file_name)
  }

  # 5. save inventory file DLW inventory folder---------------------------------
  ## check if the inventory file is generated and save it to DLW inventory file
  if (is.null(final_inv)) {
    cli::cli_alert_danger("Inventory file is not generated")

    pipfun::log_error(
      "Inventory file is not generated",
      name = "pipdata_log",
      logmeta = list(
        error = .logtype_dlw_validation,
        phase = "inventory_fail",
        artifact = "gmd_valid_inv"
      )
    )
  } else {
    tryCatch(
      {
        write_result <- pipload::pip_write(
          x = final_inv,
          id = "gmd_valid_inv",
          pk = "survey_id",
          alias = "dlw_meta",
          verbose = verbose
        )
        .validate_pip_write_result(write_result, "gmd_valid_inv")
      },
      error = function(e) {
        pipfun::log_error(
          "Failed to save the validation inventory.",
          name = "pipdata_log",
          logmeta = list(
            error = .logtype_dlw_validation,
            phase = "inventory_save",
            artifact = "gmd_valid_inv",
            path = pip_folders$dlw_metadata,
            condition_msg = conditionMessage(e)
          )
        )
        rlang::abort("Failed to save the validation inventory.", parent = e)
      }
    )

    cli::cli_alert_success(
      "Inventory file is saved at: {.dir {pip_folders$dlw_metadata}}"
    )

    pipfun::log_info(
      "Validation inventory saved.",
      name = "pipdata_log",
      logmeta = list(
        info = .logtype_dlw_validation,
        phase = "inventory_save",
        artifact = "gmd_valid_inv",
        saved_at = pip_folders$dlw_metadata,
        n_surveys = n_surveys,
        n_valid = sum(final_inv$status == "valid", na.rm = TRUE),
        n_invalid = sum(final_inv$status == "invalid", na.rm = TRUE),
        n_load_failed = sum(final_inv$data_available == "No", na.rm = TRUE)
      )
    )
  }

  # 6. save validation report file in DLW inventory folder ---------------------
  # generate validation report
  report_error_logged <- FALSE
  valid_report <- tryCatch(
    get_validation_report(),
    error = function(e) {
      report_error_logged <<- TRUE
      pipfun::log_error(
        "Validation report is not available to save",
        name = "pipdata_log",
        logmeta = list(
          error = .logtype_dlw_validation,
          phase = "report_unavailable",
          artifact = "validation_report",
          condition_msg = conditionMessage(e)
        )
      )
      NULL
    }
  )

  if (is.null(valid_report)) {
    cli::cli_alert_danger("Validation report data is not compiled")

    if (!report_error_logged) {
      pipfun::log_error(
        "Validation report is not available to save",
        name = "pipdata_log",
        logmeta = list(
          error = .logtype_dlw_validation,
          phase = "report_unavailable",
          artifact = "validation_report"
        )
      )
    }
  } else {
    # survey names in validation data
    valid_all_names <- unique(valid_report$table_name)
    old_valid_report <- tryCatch(
      pipload::load_gmd_valid_report(),
      error = function(e) {
        msg <- "Failed to read validation report file."

        pipfun::log_error(
          msg,
          name = "pipdata_log",
          logmeta = list(
            error = .logtype_dlw_validation,
            phase = "report_load_fail",
            artifact = "validation_report",
            condition_msg = conditionMessage(e)
          )
        )

        cli::cli_inform(msg)
        NULL
      }
    )

    if (!is.null(old_valid_report)) {
      old_valid_report <- old_valid_report[!(table_name %in% valid_all_names), ]
      # Check for schema drift: warn if column sets diverge
      cols_old <- setdiff(names(old_valid_report), names(valid_report))
      if (length(cols_old) > 0) {
        cli::cli_warn(c(
          "Schema drift detected in validation_report:",
          "i" = "Columns in old but missing from new: {.val {cols_old}}"
        ))
      }
      cols_new <- setdiff(names(valid_report), names(old_valid_report))
      if (length(cols_new) > 0) {
        cli::cli_warn(c(
          "Schema drift detected in validation_report:",
          "i" = "New columns not in old report: {.val {cols_new}}"
        ))
      }
      valid_report <- data.table::rbindlist(
        list(old_valid_report, valid_report),
        fill = TRUE
      )
    }

    tryCatch(
      {
        write_result <- pipload::pip_write(
          x = valid_report,
          id = "validation_report",
          alias = "dlw_meta",
          verbose = verbose
        )
        .validate_pip_write_result(write_result, "validation_report")
      },
      error = function(e) {
        pipfun::log_error(
          "Failed to save the validation report.",
          name = "pipdata_log",
          logmeta = list(
            error = .logtype_dlw_validation,
            phase = "report_save",
            artifact = "validation_report",
            path = pip_folders$dlw_metadata,
            condition_msg = conditionMessage(e)
          )
        )
        rlang::abort("Failed to save the validation report.", parent = e)
      }
    )

    cli::cli_alert_success("Validation report is saved")

    pipfun::log_info(
      "Validation report saved.",
      name = "pipdata_log",
      logmeta = list(
        info = .logtype_dlw_validation,
        phase = "report_save",
        artifact = "validation_report",
        saved_as = "validation_report",
        saved_at = pip_folders$dlw_metadata
      )
    )
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
  new_gmd <- joyn::right_join(
    inv_validated,
    gmd_new0,
    by = c("survey_id", "Checksum"),
    reportvar = FALSE,
    verbose = FALSE
  )

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
  # reportvar left TRUE (default) intentionally: .joyn == "x" filters to
  # rows present in inv_validated only (right_join semantics via full_join).
  gmd_validated_records <- joyn::full_join(
    inv_validated,
    gmd_new0,
    by = c("survey_id", "Checksum"),
    verbose = FALSE
  )
  gmd_validated_records <- gmd_validated_records[`.joyn` == "x", !c(".joyn")]

  return(invisible(gmd_validated_records))
}
