# NEW ----
dlw_dta_to_qs <- function(
    dlw_raw_folder,
    dlw_qs_folder
) {

  # 1. Record start time
  start_time <- Sys.time()

  # 2. List all .dta files
  dta_files <- list.files(dlw_raw_folder, pattern = "\\.dta$", full.names = TRUE)

  if (!dir.exists(dlw_qs_folder)) {
    dir.create(dlw_qs_folder, recursive = TRUE)
  }

  # 3. Remove existing .qs
  old_qs <- list.files(dlw_qs_folder, pattern = "\\.qs$", full.names = TRUE)
  if (length(old_qs) > 0) {
    file.remove(old_qs)
  }

  # 4. If no .dta files, just give a message
  if (length(dta_files) == 0) {
    cli_alert_info("No .dta files found in '{dlw_raw_folder}'. No conversion needed.")
    return(invisible(NULL))
  }

  # 5. Use a CLI progress bar to convert each .dta -> .qs
  cli_progress_bar(
    name = "Converting .dta to .qs",
    total = length(dta_files)
  )

  for (i in seq_along(dta_files)) {
    f <- dta_files[i]
    base_no_ext <- tools::file_path_sans_ext(basename(f))

    # Read .dta
    df <- haven::read_dta(f)

    # Write .qs
    out_path <- file.path(dlw_qs_folder, paste0(base_no_ext, ".qs"))
    qs::qsave(df, out_path)

    # Update progress
    cli_progress_update()
  }

  cli_progress_done()

  # 6. Compute elapsed time
  end_time <- Sys.time()
  elapsed_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # 7. Final message: how many files, where they ended up, how long it took
  cli_alert_info(sprintf(
    "%d .dta files converted to .qs in '%s' (took %.1f seconds).",
    length(dta_files), dlw_qs_folder, elapsed_sec
  ))

  return(invisible(NULL))
}
