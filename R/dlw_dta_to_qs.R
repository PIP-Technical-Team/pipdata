
#' Convert DLW .dta files to .qs
#'
#' @param dlw_raw_folder DLW-RAW folder with .dta files
#' @param dlw_qs_folder Permanent or temporary folder to store .qs files
#' @param log TRUE/FALSE default value is `TRUE`
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#'  dlw_dta_to_qs(
#'  dlw_raw_folder = "dlw_raw/folder_time1",
#'  dlw_qs_folder  = "dlw_qs")
#'  }
#'
dlw_dta_to_qs <- function(
    dlw_raw_folder,
    dlw_qs_folder,
    log  = TRUE
) {

  # set-up a release
  pipfun::get_wrk_release()

  if (log) {

    # capturing arguments in the log file
    pipfun::log_add("info", "Convert dta raw into qs format",
                    name = "pipdata_log",
                    args = list(dlw_raw_folder = dlw_raw_folder,
                                dlw_qs_folder = dlw_qs_folder))
  }

  # 1. Record start time ------
  start_time <- Sys.time()

  # 2. List .dta files -----
  dta_files <- list.files(dlw_raw_folder, pattern = "\\.dta$", full.names = TRUE)

  # 2a. Abort if no .dta files found ----
  if (length(dta_files) == 0) {

    if (log){

      pipfun::log_add("error", "No .dta files are found",
                      name = "pipdata_log",
                      logmeta = list(dta_files = 0))
    }

    cli::cli_abort(
      sprintf("No .dta files found in '%s'. Nothing to convert.", dlw_raw_folder)#,
      # class = c("no_dlw_files", "piperr"),
      # call  = sys.call()
    )
  }

  # 3. Make sure QS folder exists ore create -------
  if (!dir.exists(dlw_qs_folder)) {
    dir.create(dlw_qs_folder, recursive = TRUE)
  }

  # 4. Remove old .qs files ----
  old_qs <- list.files(dlw_qs_folder, pattern = "\\.qs$", full.names = TRUE)
  if (length(old_qs) > 0) {
    file.remove(old_qs)
  }

  # 5. Progress bar starts -----
  cli::cli_progress_bar(
    name  = "Converting .dta to .qs",
    total = length(dta_files)
  )

  # 6. Loop over each .dta file -----

  if (log){

    pipfun::log_add("info", "Read .dta files convert them to .qs",
                    name = "pipdata_log",
                    logmeta = list(raw_folder = dlw_raw_folder,
                                   qs_folder  = dlw_qs_folder))
  }


  for (f in dta_files) {

    base_no_ext <- tools::file_path_sans_ext(basename(f))
    out_path    <- file.path(dlw_qs_folder, paste0(base_no_ext, ".qs"))

    # tryCatch 1. dta_read_err and 2. write
    tryCatch(
      expr = {
        # A) Attempt to read the .dta
        df <- tryCatch(
          expr = haven::read_dta(f),
          error = function(e) {

            pipfun::log_add("error", "Could not read .dta file",
                            name = "pipdata_log",
                            logmeta = list(dta_file_name = basename(f)))

            # # 1) dta_read_err
            cli::cli_abort(
              message = paste0("Could not read '", basename(f), "'")#,
              # class   = c("dta_read_err", "piperr"),
              # log     = log_err,
              # skip    = skip_err,
              # call    = quote(read_dta(f))
            )
          }
        )


        if (!inherits(df, "data.frame")) {
          cli::cli_abort(
            message = paste("Invalid data frame returned for", f)#,
            # class   = c("dta_read_err", "piperr"),
            # log     = log_err,
            # skip    = skip_err,
            # call    = quote(read_dta(f))
          )
        }

        # B) Attempt to save as .qs
        tryCatch(
          expr = qs::qsave(df, out_path),
          error = function(e) {
            # 2) Raise a custom error for "dta_save_err"
            pipfun::log_add("error", "Could not save data to .qs format",
                            name = "pipdata_log",
                            logmeta = list(qs_file_name = basename(out_path)))

            cli::cli_abort(
              message = paste0("Could not save '", basename(out_path),"'")#,
              # class   = c("dta_save_err", "piperr"),
              # log     = log_err,
              # skip    = skip_err,
              # call    = quote(qsave(df, out_path))
            )
          }
        )
      },

      # # Handler for "dta_read_err"
      # dta_read_err = function(cnd) {
      #   if (isTRUE(cnd$log)) {
      #     add_log(cnd)
      #   }
      #   if (!isTRUE(cnd$skip)) {
      #     # re-throw => abort everything
      #     cli::cli_abort(cnd$message, call = cnd$call)
      #   } else {
      #     # skip => show warning => move on
      #     cli::cli_alert_warning(sprintf(
      #       "Skipping file '%s' due to read error. Original error: %s",
      #       basename(f), cnd$message
      #     ))
      #   }
      # },
      #
      error = function(cnd){

        cli::cli_alert_warning(sprintf(
          "Skipping file '%s' because it could not be saved or read the files.",
          basename(f)))

      }

      # # Handler for "dta_save_err"
      # dta_save_err = function(cnd) {
      #   if (isTRUE(cnd$log)) {
      #     add_log(cnd)
      #   }
      #   if (!isTRUE(cnd$skip)) {
      #     # re-throw => abort everything
      #     cli::cli_abort(cnd$message, call = cnd$call)
      #   } else {
      #     # skip => show warning => move on
      #     cli::cli_alert_warning(sprintf(
      #       "Skipping file '%s' because it could not be saved. Original error: %s",
      #       basename(f), cnd$message
      #     ))
      #   }
      # }
    )

    cli::cli_progress_update()
  }

  if (log){

    pipfun::log_add("info", "Completed converting datasets from .dta to .qs format",
                    name = "pipdata_log",
                    logmeta = list(raw_folder = dlw_raw_folder,
                                   qs_folder  = dlw_qs_folder))
  }

  # Done processing all files
  cli::cli_progress_done()

  # 7. Final message
  end_time <- Sys.time()
  elapsed_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cli::cli_alert_info(sprintf(
    "%d .dta files processed into .qs in '%s' (took %.1f seconds).",
    length(dta_files), dlw_qs_folder, elapsed_sec
  ))

  invisible(NULL)
}

