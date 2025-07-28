#' Scan DLW .dta files, validate them, and version them in pip_raw_inventory
#'
#' This function checks a folder of .qs files (dlw_qs_folder), compares them to
#' an existing pip_raw_inventory (pip_raw_inventory_path), and performs
#' validation. It then versions and copies valid files into a pip_raw_folder,
#' updating the inventory with the new file information.
#'
#' @param root_dir Character. Root directory (pip).  Defaults to the environment variable "PIP_ROOT_DIR".
#' @param maindir Character. Main directory where the DLW datasets are saved.
#' @param dlw_pin_folder Character. Pin board folder containing .qs files.
#' @param dlw_inv_folder Character. Name of the folder containing the inventory files. Default is "dlw_inventory".
#' @param inv_gmd_list Character. Name of inventory file that contains GMD datasets list
#' @param inv_qs_list Character. Path where inventory file is resided
#' @param validation_report_path Character. Path to validation report
#' @param log TRUE/FALSE default value is `TRUE`
#'
#' @return Invisibly returns the updated pip_raw_inventory as a dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_get_qs(
#'   root_dir  = Sys.getenv("PIP_ROOT_DIR"),
#'   maindir = "dlw_repository",
#'   dlw_pin_folder = "dlw_qs",
#'   dlw_inv_folder = "dlw_inventory",
#'   inv_gmd_list = "dlw_gmd_inv",
#'   inv_qs_list= "inv_dlw_qs_pin",
#'   validation_report_path = "validation_report"
#' )
#' }
dlw_get_qs <- function(
    root_dir  = Sys.getenv("PIP_ROOT_DIR"),
    maindir = "PIP_ingestion_pipeline_v2/dlw_repository",
    dlw_pin_folder = "dlw_data",
    dlw_inv_folder = "dlw_inventory",
    inv_gmd_list = "dlw_gmd_inv",
    inv_qs_list  = "dlw_pin_inv",
    validation_report_path = "validation_report",
    log  = TRUE
) {

  #### logging -----------------------------------------------------------------
  if (log) {

    pipfun::log_add("info", "Start validate and generate DLW PIP data",
                    name = "pipdata_log",
                    args = list(root_dir = root_dir,
                                maindir  = maindir,
                                dlw_pin_folder = dlw_pin_folder,
                                dlw_inv_folder = dlw_inv_folder,
                                inv_gmd_list = inv_gmd_list,
                                inv_qs_list = inv_qs_list,
                                validation_report_path = validation_report_path))
  }
  #### -------------------------------------------------------------------------

  # set-up a release
  pipfun::get_wrk_release()
  wrk_release <- pipfun::get_wrk_release()
  release_lbl <- wrk_release$release
  release_idn <- wrk_release$identity

  # check if pin folder exists
  if (!dir.exists(fs::path(root_dir, maindir, dlw_pin_folder))) {
    print(fs::path(root_dir, maindir, dlw_pin_folder))
    cli::cli_abort(
      "Pin folder ({dlw_pin_folder}) is not avaiable"
    )

  }

  # check if inventory folder exists
  if (!dir.exists(fs::path(root_dir, maindir, dlw_inv_folder))) {

    cli::cli_abort(
      "Inventory folder ({inv_folder}) is not avaiable"
    )

  }

  ### -------------------------------------------------------------------------

  # 1. Load gmd list that is going to be used to download datasets

  ## construct path to the gmd inventory file
  inv_gmd_path <- fs::path(root_dir, maindir,
                           dlw_inv_folder,
                           paste0(inv_gmd_list, "_",
                                  release_lbl, "_",
                                  release_idn),
                           ext ="qs")

  if (!file.exists(inv_gmd_path)) {

        msg <- glue::glue('GMD datasets list file ({inv_gmd_path}) is not available')

        if (log) {

          pipfun::log_add("error", msg,
                          name = "pipdata_log",
                          logmeta = list(inventory = "GMD list", dataset = inv_gmd_path))
        } else {

          cli::cli_abort(msg)

        }


  }

  inv_gmd <- tryCatch(
    qs::qread(inv_gmd_path),
    error = function(e) {
      msg <- glue::glue('Failed to load gmd inventory file ({inv_gmd_path})')

      if (log) {

        pipfun::log_add("error", msg,
                        name = "pipdata_log",
                        args = list(inv_path = inv_gmd_path),
                        logmeta = list(error = e))
      } else {

        print(e)

      }

      cli::cli_abort(msg)

      NULL
    }
  )

  # 2. Load or create inventory file -------------------------------------------

  ## construct path to the inventory file
  inv_pin_path <- fs::path(root_dir, maindir,
                           dlw_inv_folder,
                           paste0(inv_qs_list, "_",
                                  release_lbl, "_", release_idn),
                           ext ="qs")

  old_inv <- if (file.exists(inv_pin_path)) {

      tryCatch(
        qs::qread(inv_pin_path),
        error = function(e) {

          msg <- glue::glue('Failed to load inventory file ({inv_pin_path})')

          if (log) {

            pipfun::log_add("error", msg,
                            name = "pipdata_log",
                            args = list(inv_pin_path = inv_pin_path),
                            logmeta = list(error = e))
          }

          cli::cli_abort(msg)

          NULL
        }
      )
  } else {

    cli::cli_alert_info("No previous pinned .qs inventory; creating empty.")
    tibble::tibble(
      survey_id        = character(),
      pipeline_version = integer(),
      pin_version      = list(),
      pins_folder    = character(),
      status           = character(),
      date_validated   = as.POSIXct(character())
    )
  }


  # 3. Get the data and pinned it to the board --------------------------------

  cli::cli_alert_info("Get, pinned and validate dataset: {dlw_pin_folder}")

  # select only the basic modules and dta files
  inv_gmd <- inv_gmd[(Module %in% c("GPWG", "GROUP", "BIN",
                                    "HIST", "ALL", "ASPIRE", "L") &
                        Ext == "dta"),]

  # inv_gmd <- inv_gmd[(Country != "IND"), ]
  # inv_gmd <- inv_gmd[(1:1000), ]
  inv_gmd <- inv_gmd[(Country %in% c("IND", "GHA")), ]

  all_names <- unique(inv_gmd$FileName)
  new_inv   <- vector("list", length(all_names))

  local_dir <- fs::path(root_dir, maindir, dlw_pin_folder)
  cli::cli_alert_info("pinn folder: {local_dir}")

  cli::cli_progress_bar("Downloading .qs", total = nrow(inv_gmd))

  # download .dta from GMD catalog
  for (i in seq_along(1:nrow(inv_gmd))){

    pin_name <- inv_gmd[["FileName"]][i] |>
      fs::path_ext_remove() |>
      fs::path(ext = "qs")
    country     <- inv_gmd[["Country"]][i]
    year        <- inv_gmd[["Year"]][i]
    svy_acronym <- inv_gmd[["Survey_acronym"]][i]
    vermst      <- inv_gmd[["Vermast"]][i]
    veralt      <- inv_gmd[["Veralt"]][i]
    md_type     <- inv_gmd[["Module"]][i]
    coll        <- inv_gmd[["Collection"]][i]

    out <- tryCatch(

      {
        dlw::dlw_get_gmd(country_code = country,
                         year         = year,
                         module       = md_type,
                         vermast      = vermst,
                         veralt       = veralt,
                         local_dir    = local_dir)
      },
      error = function(e) {

        msg <- glue::glue('Could not downaload a file: ({inv_gmd[["Country"]][i]},
          {inv_gmd[["Year"]][i]}, {inv_gmd[["Module"]][i]},
          {inv_gmd[["Vermast"]][i]}, {inv_gmd[["Veralt"]][i]})')

        if (log) {

          # "Failed to download .dta file"
          pipfun::log_add("error", msg,
                          name = "pipdata_log",
                          args = list(country      = country,
                                      year         = year,
                                      module       = md_type,
                                      vermast      = vermst,
                                      veralt       = veralt,
                                      local_dir    = local_dir),
                          logmeta = list(error = e))
        }

        cli::cli_inform(msg)

        NULL
      }
    )


    if (!is.null(out)){

      board    <- dlw::get_from_dlwenv("current_board")
      versions <- pins::pin_versions(board, pin_name)

      # validate the data
      if (md_type == "GPWG"){
        check <- dlw_validation_gpwg(out, nm)
      } else if (md_type == "GROUP") {
        check <- dlw_validation_group(out, nm)
      } else if (md_type == "BIN") {
        check <- dlw_validation_bin(out, nm)
      } else if (md_type == "HIST") {
        check <- dlw_validation_hist(out, nm)
      } else if (md_type == "ALL") {
        check <- dlw_validation_all(out, nm)
      } else if (md_type == "ASPIRE") {
        check <- dlw_validation_aspire(out, nm)
      } else if (md_type == "L") {
        check <- dlw_validation_l(out, nm)
      } else {
        check <- dlw_validation_skip(out, nm)
      }

      if (any(check[["type"]] == "error")){

        check <- check[type == "error", .(message)]
        cli::cli_alert_danger("Validation failed for {nm} : {check$message}")

        valid_status <- "invalid"

      } else {

        valid_status <- "valid"

      }

      # check if the survey id is available in the previous processes

      if (!is.null(old_inv) && (nm %in% old_inv$survey_id)) {

        row_svyid <- old_inv[nm == survey_id, "pipeline_version"]
        workflow_vrs <- row_svyid$pipeline_version + 1

        new_inv[[i]] <- tibble::tibble(
          survey_id         = nm,
          pipeline_version  = workflow_vrs,
          pin_version       = list(versions),
          pins_folder     = pin_name,
          status            = valid_status,
          date_validated    = Sys.time()
        )

      } else {

        new_inv[[i]] <- tibble::tibble(
          survey_id         = nm,
          pipeline_version  = 1,
          pin_version       = list(versions),
          pins_folder     = pin_name,
          status            = valid_status,
          date_validated    = Sys.time()
        )

      }


      cli::cli_progress_update()
      next

    } else {

      cli::cli_progress_update()
      next
    }

  }

  # Done processing all raw dta files
  cli::cli_progress_done()

  # 4. merge new_inv rows into final_inv ---------------------------------------
  ## Combine all new_inv items into a single data frame.
  final_inv <- dplyr::bind_rows(new_inv) |>
    pipload::survey_id_to_vars()

  # 5. save inventory file DLW inventory folder---------------------------------

  ## check if the inventory file is generated and save it to DLW inventory file
  if (is.null(final_inv)) {

    cli::cli_alert_danger("Inventory file is not generated")

    if (log) {

      pipfun::log_add("error", "Inventory file is not generated",
                      name = "pipdata_log",
                      logmeta = list(dataset = "inventory"))
    }


  } else {

    qs::qsave(final_inv, inv_pin_path)

    cli::cli_alert_success("Inventory file is saved at: {inv_pin_path}")

    if (log) {

      pipfun::log_add("info", "Inventory file is saved",
                      name = "pipdata_log",
      logmeta = list(saved_at = inv_pin_path))
    }

  }

  # 6. save validation report file in DLW inventory folder ---------------------

  ## check if the validation report is generated and save it to DLW inventory file

  ### construct path to the gmd inventory file
  validation_path <- fs::path(root_dir, maindir, dlw_inv_folder,
                              paste0(validation_report_path, "_",
                                     release_lbl, "_",
                                     release_idn),
                              ext ="qs")

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

    # load validation report if it exists for the release
    if (file.exists(validation_path)) {

      if (log){

        old_valid <- tryCatch(
          qs::qread(validation_path),
          error = function(e) {
            pipfun::log_add("error", "Failed to load validation report file",
                            name = "pipdata_log",
                            logmeta = list(error = e$message))

            NULL
          }
        )

      } else {

        old_valid <- qs::qread(validation_path)

      }

      old_valid <- old_valid[!(table_name %in% valid_all_names), ]
      valid_report <- old_valid |> dplyr::bind_rows(valid_report)
    }

    qs::qsave(valid_report, validation_path)
    cli::cli_alert_success("Validation report is saved at: {validation_path}")
    if (log) {

      pipfun::log_add("info", "Validation report is saved",
                      name = "pipdata_log",
       logmeta = list(saved_at = validation_path))
    }
  }

  # -----------------------------------------------------------------------------
  if (log) {

    pipfun::log_add("info", "End validating and generating DLW PIP data",
                    name = "pipdata_log",
                    logmeta = list(last_stage = "Saved data"))
  }

  cli::cli_alert_success("GENERATING PINNED .qs FILES END!!!")

  # 7. save log file DLW inventory folder---------------------------------

  ## construct logging file path
  log_file_path <- fs::path(root_dir,
                            maindir,
                            dlw_inv_folder,
                            paste0("log_info_", release_lbl, "_",
                                   release_idn),
                            ext ="qs")

  if (log) {

    pipfun::log_add("info", "logging file is saved",
                    name = "pipdata_log",
                    logmeta = list(log_path = log_file_path))
  }

  return(invisible(final_inv))
}
