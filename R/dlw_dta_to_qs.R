dlw_dta_to_qs <- function(dlw_raw_folder,
                          dlw_qs_folder) {

  # 1. List .dta files in dlw_raw_folder
  dta_files <- list.files(dlw_raw_folder, pattern = "\\.dta$", full.names = TRUE)

  if (length(dta_files) == 0) {
    message("No .dta files found in ", dlw_raw_folder)
    return(invisible(NULL))
  }

  # 2. Ensure dlw_qs_folder exists
  if (!dir.exists(dlw_qs_folder)) {
    dir.create(dlw_qs_folder, recursive = TRUE)
  }

  # 3. Remove any existing .qs files except the inventory file
  all_qs_files <- list.files(dlw_qs_folder, pattern = "\\.qs$", full.names = TRUE)
  inventory_file_path <- file.path(dlw_qs_folder, "dlw_qs_inventory.qs")
  qs_files_to_remove <- setdiff(all_qs_files, inventory_file_path)

  if (length(qs_files_to_remove) > 0) {
    file.remove(qs_files_to_remove)
    message("Removed old .qs files (except the inventory).")
  }

  # 4. For each .dta, read & write .qs
  for (f in dta_files) {
    fname_no_ext <- tools::file_path_sans_ext(basename(f))
    df <- haven::read_dta(f)
    qs_path <- file.path(dlw_qs_folder, paste0(fname_no_ext, ".qs"))
    qsave(df, qs_path)
    message(basename(f), " -> to -> ", basename(qs_path), " completed.")
  }

  message("Conversion to .qs complete. Files located at: ", dlw_qs_folder)
  return(invisible(NULL))
}

