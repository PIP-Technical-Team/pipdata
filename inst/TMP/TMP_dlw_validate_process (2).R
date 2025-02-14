
# 0. Set-up -----
library(devtools)
load_all()
## I. Directory with dlw files and the dlw inventory: ----
dlw_raw_dir <- "E:/PovcalNet/01.personal/wb622077/pipdata_test/dlw_raw_test"

## II. Directory with the pip_in_dlw directory: ----
dlw_to_pip_dir <- "E:/PovcalNet/01.personal/wb622077/pipdata_test/dlw_to_pip_test"

## III. Note that the pip_raw directory should be emptied before starting each simulation
## "cycle".

## III. Dependencies ----
library(syncdr)
library(data.table)
library(fs)
library(qs)
library(digest)
library(cli)
library(tictoc)
library(dplyr)


# 1. Dynamic folder simulation (trigger) ----
## I. Set up folders ----

# Define the base folder and output folders
base_folder <- paste0(dlw_raw_dir, "/folder_base")
folder_time1 <- paste0(dlw_raw_dir, "/folder_time1")
folder_time2 <- paste0(dlw_raw_dir, "/folder_time2")
folder_time3 <- paste0(dlw_raw_dir, "/folder_time3")

# Create the output folders if they do not exist (needs to be done only first time)
# if (!dir.exists(folder_time1)) dir.create(folder_time1)
# if (!dir.exists(folder_time2)) dir.create(folder_time2)
# if (!dir.exists(folder_time3)) dir.create(folder_time3)

# Copy all files from base_folder to folder_time1 (needs to be done only first time)
# files_to_copy <- list.files(base_folder, pattern = "\\.dta$", full.names = TRUE)
# file.copy(files_to_copy, folder_time1, overwrite = TRUE)




## II. Changes simulation ----
simulate_changes_exact <- function(
    folder_time1,
    folder_time2,
    n_add    = 1,
    n_remove = 1,
    n_change = 1,
    n_rename = 1,
    seed     = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  # 1. List .dta files in folder_time1
  files <- list.files(folder_time1, pattern = "\\.dta$", full.names = TRUE)

  # Build a data frame with relevant columns
  file_info <- data.frame(
    full_path = files,
    name      = basename(files),
    stringsAsFactors = FALSE
  )
  # parse year & veralt from name (if found)
  file_info$year <- as.numeric(sub(".*_(\\d{4})_.*", "\\1", file_info$name))
  file_info$veralt <- as.numeric(sub(".*_M_V(\\d+)_.*", "\\1", file_info$name))

  # 2. Ensure folder_time2 exists
  dir.create(folder_time2, showWarnings = FALSE, recursive = TRUE)

  # 3. Decide how many files to remove
  n_remove_actual <- min(n_remove, nrow(file_info))
  remove_files <- data.frame()
  if (nrow(file_info) > 0 && n_remove_actual > 0) {
    indices <- sample(seq_len(nrow(file_info)), size = n_remove_actual)
    remove_files <- file_info[indices, , drop = FALSE]
  }

  # 4. The files we keep
  keep_df <- file_info[ !(file_info$full_path %in% remove_files$full_path), , drop = FALSE]

  # 5. Copy kept files to folder_time2
  if (nrow(keep_df) > 0) {
    file.copy(keep_df$full_path, folder_time2, overwrite = TRUE)
  }

  # 6. Among kept files, rename some fraction => "version up" the name
  rename_df <- data.frame(old_name = character(), new_name = character(), stringsAsFactors = FALSE)
  if (n_rename > 0 && nrow(keep_df) > 0) {
    n_rename_actual <- min(n_rename, nrow(keep_df))
    rename_candidates_idx <- sample(seq_len(nrow(keep_df)), size = n_rename_actual)
    rename_candidates <- keep_df[rename_candidates_idx, , drop = FALSE]

    for (i in seq_len(nrow(rename_candidates))) {
      old <- rename_candidates[i, ]
      old_path_2 <- file.path(folder_time2, old$name)

      old_v <- ifelse(is.na(old$veralt), 1, old$veralt)
      new_v <- old_v + 1
      # new name: replace _M_Vxx_
      new_file_basename <- sub(
        "_M_V\\d+_",
        paste0("_M_V", sprintf("%02d", new_v), "_"),
        old$name
      )

      new_path_2 <- file.path(folder_time2, new_file_basename)

      if (file.exists(old_path_2)) {
        file.rename(old_path_2, new_path_2)
        rename_df <- rbind(rename_df,
                           data.frame(old_name = old$name,
                                      new_name = new_file_basename,
                                      stringsAsFactors = FALSE))
      }
    }
  }

  # 7. Overwrite content for n_change files among what's now in folder_time2
  files_after_rename <- list.files(folder_time2, pattern = "\\.dta$", full.names=TRUE)
  changed_files <- character(0)
  if (n_change > 0 && length(files_after_rename) > 0) {
    final_df <- data.frame(
      path = files_after_rename,
      name = basename(files_after_rename),
      stringsAsFactors = FALSE
    )
    n_change_actual <- min(n_change, nrow(final_df))
    change_candidates_idx <- sample(seq_len(nrow(final_df)), size=n_change_actual)
    change_candidates <- final_df[change_candidates_idx, , drop=FALSE]

    for (i in seq_len(nrow(change_candidates))) {
      cf <- change_candidates[i, ]
      dummy_df <- data.frame(
        ID=1:3,
        random_val=runif(3)
      )
      haven::write_dta(dummy_df, cf$path)
      changed_files <- c(changed_files, cf$name)
    }
  }

  # 8. Add n_add new files => pick a truly new year
  add_files <- character(0)
  if (n_add > 0) {
    # find existing years
    existing_years <- sort(unique(file_info$year[!is.na(file_info$year)]))
    if (length(existing_years) == 0) {
      existing_years <- c(2000)
    }
    max_year <- max(existing_years, na.rm=TRUE)

    for (j in seq_len(n_add)) {
      # define a brand-new year: e.g. max_year + j
      new_year <- max_year + j  # or + sample(1:5,1)

      # we fix veralt=1 for brand-new files
      new_v <- 1
      suffix <- ifelse(runif(1) < 0.5, "GMD_GPWG", "GMD_BIN")

      # name: "ITA_<year>_SHIW-LIS_V01_M_V01_A_<suffix>.dta"
      new_basename <- paste0("ITA_", new_year, "_SHIW-LIS_V01_M_V", sprintf("%02d", new_v), "_A_", suffix, ".dta")
      new_file_path <- file.path(folder_time2, new_basename)

      # dummy content
      dummy_df <- data.frame(ID=1:3, random_val=runif(3))
      haven::write_dta(dummy_df, new_file_path)
      add_files <- c(add_files, new_basename)
    }
  }

  # 9. Log changes
  message("\n==== simulate_changes_exact SUMMARY ====")
  message("Removed files: ", nrow(remove_files))
  if (nrow(remove_files) > 0) {
    print(remove_files[, c("full_path","name","year","veralt"), drop=FALSE])
  }

  message("Renamed files: ", nrow(rename_df))
  if (nrow(rename_df) > 0) {
    print(rename_df)
  }

  message("Changed contents in-place: ", length(changed_files))
  if (length(changed_files) > 0) {
    print(changed_files)
  }

  message("Added new files: ", n_add)
  if (length(add_files) > 0) {
    print(add_files)
  }

  message("folder_time2 is ready with simulated changes.\n")

  # 10. Return details invisibly
  return(invisible(list(
    removed = remove_files,
    renamed = rename_df,
    changed_files = changed_files,
    added_files   = add_files
  )))
}

### This generates folder_time2
# It is possible to re-run it again with different seed.
# It will 'reset" folder_time2 in any case.
simulate_changes_exact(
  folder_time1 = folder_time1,
  folder_time2 = folder_time2,
  n_add    = 2,
  n_remove = 1,
  n_change = 1,
  n_rename = 3,
  seed     = 123
)

### This generates folder_time3
# Same is true for this instance.
simulate_changes_exact(
  folder_time1 = folder_time2,
  folder_time2 = folder_time3,
  n_add    = 1,
  n_remove = 0,
  n_change = 1,
  n_rename = 3,
  seed     = 345
)


# 2. Release set-up ----
## 1. Create a label
release_label <- create_release_label(
  suffix    = "INT",
  full = FALSE
)

## 2. Set that as the current release
set_current_release(release_label)

## 3. Retrieve
get_current_release()




# 3. Copy files to dlw_qs ----
## I. Set-up for simulation ----
base_folder <- paste0(dlw_raw_dir, "/folder_base")
folder_time1 <- paste0(dlw_raw_dir, "/folder_time1")
folder_time2 <- paste0(dlw_raw_dir, "/folder_time2")
dlw_qs_folder  = paste0(dlw_to_pip_dir, "/", "dlw_qs")
pip_raw_folder = paste0(dlw_to_pip_dir, "/", "pip_raw")


## II. folder_time1/folder_time2/folder_time3 ----
dlw_raw_folder = folder_time1 # Change to other folders for generating proper simulation


## III. Convert .dta files to .qs files -----
dlw_dta_to_qs(dlw_raw_folder = dlw_raw_folder,
              dlw_qs_folder  = dlw_qs_folder)

# 4. Scan dlw_qs, validate, version to pip_raw ----
new_pip_raw_inv <- dlw_scan_and_validate(
  dlw_qs_folder        = dlw_qs_folder,
  pip_raw_folder       = pip_raw_folder,
  pip_raw_inventory_path = paste0(pip_raw_folder, "/pip_raw_inventory.qs"),
)


new_pip_raw_inv |> View() # Have a look, this is what the inventory for a single release looks like.

# 5. Store release ----
dlw_store_release(
   pip_raw_inventory_df  = new_pip_raw_inv,
   release_label         = get_current_release(),
   release_folder        = pip_raw_folder,
   update_inventory_list = TRUE,
   pip_raw_releases      = paste0(pip_raw_folder, "/","pip_raw_inventory_releases.qs"),
   log_err  = TRUE,
   skip_err = TRUE
 )

# 6. Release mgmt ----

release_inventory_path <- paste0(pip_raw_folder, "/","pip_raw_inventory_releases.qs")

## Get a list of releases ----
releases_df <- dlw_list_releases(release_inventory_path)

## Get a single release ----





