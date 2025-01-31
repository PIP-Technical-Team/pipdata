
# 0. Set-up -----
## I. Directory with dlw files and the dlw inventory: ----
dlw_raw_dir <- "E:/PovcalNet/01.personal/wb622077/pipdata_test/dlw_raw_test"
# Within this folder (copy of original but with ITA data only and simulation folders created):
## - subfolder : folder_base: all dlw-raw files in .dta format
## - subfolder : folder_time1: all dlw-raw files in .dta format time 1.
## - subfolder : folder_time2: all dlw-raw files in .dta format time 2.
## - subfolder: _Inventory:
### - a .qs or .fst file with all observations recorded (metadata). the most recent one.
### - DLWRAW_all_DTA.csv: content of all .dta files in one .csv
## - subsubfolder: _Inventory/_vintage: folder with previous versions of the dlw inventory. (see above)

## II. Directory with the pip_in_dlw directory: ----
dlw_to_pip_dir <- "E:/PovcalNet/01.personal/wb622077/pipdata_test/dlw_to_pip_test"
# Within this folder:
## - validated GPWG files in .qs format (all versions)
## - subfolder: _inventory: a .qs or .fst file with all observations' recorded metadata. the most recent one.
## - subfolder: _report: a .qs file with a summary of the differences between previous and subsequent version.
## - subsubfolder: _report/_vintage: folder with previous versions of the dlw in pip inventory reports.
## - subsubfolder: _inventory/_vintage: folder with previous versions of the dlw in pip inventory.

## III. Dependencies ----
library(syncdr)
#library(myrror)
library(data.table)
library(fs)
library(qs)
library(digest)


# 1. Dynamic folder simulation (trigger) ----
## I. Set up folders ----

# Define the base folder and output folders
base_folder <- paste0(dlw_raw_dir, "/folder_base")
folder_time1 <- paste0(dlw_raw_dir, "/folder_time1")
folder_time2 <- paste0(dlw_raw_dir, "/folder_time2")

# Create the output folders if they do not exist
if (!dir.exists(folder_time1)) dir.create(folder_time1)
if (!dir.exists(folder_time2)) dir.create(folder_time2)

# Copy all files from base_folder
files_to_copy <- list.files(base_folder, pattern = "\\.dta$", full.names = TRUE)
file.copy(files_to_copy, folder_time1, overwrite = TRUE)




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


simulate_changes_exact(
  folder_time1 = folder_time1,
  folder_time2 = folder_time2,
  n_add    = 2,
  n_remove = 1,
  n_change = 1,
  n_rename = 3,
  seed     = 123
)





# 2. Update DLW list, inventory, and vintage ----
## I. Custom DLRRAW_all_DTAs.csv creation ----
## (R version of GMD-RAW list in powershell)
generate_dlw_raw_list <- function(input_dir, output_file) {
  # Record the start time
  begintime <- Sys.time()

  # Get a list of .dta files with metadata
  file_info <- data.frame(
    FullName = list.files(input_dir, pattern = "\\.dta$", full.names = TRUE, recursive = TRUE),
    stringsAsFactors = FALSE
  )

  # Add metadata columns
  file_info <- file_info |>
    dplyr::mutate(
      CreationTime = purrr::map_chr(FullName, ~ as.character(file.info(.x)$ctime)),
      LastWriteTime = purrr::map_chr(FullName, ~ as.character(file.info(.x)$mtime)),
      Length = purrr::map_dbl(FullName, ~ file.info(.x)$size)
    )

  # Save to CSV with a semicolon delimiter
  write.table(
    file_info,
    file = output_file,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE
  )

  # Record the end time
  endtime <- Sys.time()

  # Calculate and print the time difference
  timediff <- difftime(endtime, begintime, units = "secs")
  message(sprintf(
    "Time to create list was %dh %dm %ds",
    as.integer(timediff) %/% 3600, # Hours
    (as.integer(timediff) %% 3600) %/% 60, # Minutes
    as.integer(timediff) %% 60 # Seconds
  ))
}

# Run it once for time1
generate_dlw_raw_list(folder_time1, paste0(dlw_raw_dir, "/_Inventory/DLWRAW_all_DTAs.csv"))
# Then subsequent times come from time2
generate_dlw_raw_list(folder_time2, paste0(dlw_raw_dir, "/_Inventory/DLWRAW_all_DTAs.csv"))

## II. Custom update_dlw ----
update_dlw_inventory_test <-
  function(dlw_dir = dlw_raw_dir,
           root_dir = "E:/PovcalNet/01.personal/wb622077/pipdata_test/",
           force    = FALSE)
  {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # directories and paths   ---------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    dlw_inv_path <- fs::path(dlw_dir,"_Inventory")

    dlw_inv_file <- fs::path(dlw_inv_path,
                             "DLWRAW_all_DTAs", ext = "csv")

    if (!fs::file_exists(dlw_inv_file)) {

      msg     <- c(
        "File does not exists",
        "x" = "{dlw_inv_file} not found.",
        "i" = "check connection or {.field pipload} globals"
      )
      cli::cli_abort(msg,
                     class = "pipdata_error"
      )
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## variables --------
    id_vars <-
      c(
        "country_code",
        "surveyid_year",
        "survey_acronym",
        "vermast",
        "M",
        "veralt",
        "A",
        "collection",
        "module"
      )


    pip_modules <-
      c("GPWG",
        "ALL",
        "BIN",
        "GROUP",
        "HIST")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clean data   ---------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    dlw_inv <- fread(file = dlw_inv_file,
                     showProgress = FALSE)
    setnames(dlw_inv,tolower)


    dlw_inv[,
            fullname := {
              x <- gsub("\\\\", "/", fullname)
              x <- gsub(root_dir, "", x)
              x
            }
    ][,
      survey_id := {
        fullname |>
          fs::path_file() |>
          fs::path_ext_remove()
      }
    ][,
      `:=`(
        #creationtime  = lubridate::mdy_hms(creationtime),
        #lastwritetime = lubridate::mdy_hms(lastwritetime)
        creationtime  = as.POSIXct(creationtime, tz = "UTC", format = "%m/%d/%Y %T"),
        lastwritetime = as.POSIXct(lastwritetime, tz = "UTC", format = "%m/%d/%Y %T")
      )]


    # add variables from survey ID
    dlw_inv <- suppressWarnings(pipload::survey_id_to_vars(dlw_inv))
    dlw_inv <- na.omit(dlw_inv)
    dlw_inv <- dlw_inv[module %chin% pip_modules] # keep important modules

    setorder(dlw_inv, country_code, surveyid_year, survey_acronym, vermast, veralt)


    # check if data has changed

    status <- pipfun::pip_sign_save(x       =  dlw_inv,
                                    measure = "dlw_inventory",
                                    msrdir  = dlw_inv_path,
                                    force   = force)


    return(invisible(status))
  }

# Run to update directory inventory
update_dlw_inventory_test()


# 3. Release set-up ----
## 1. Create a label
release_label <- create_release_label(
  ppp_round = "2017",
  rv        = "01",
  av        = "01",
  suffix    = "INT"
)

## 2. Set that as the current release
set_current_release(release_label)

## 3. Retrieve
get_current_release()



# 3. Versioning and Validation Process ----
## Set-up for simulation:
## Clear the folders
dlw_qs_folder  = paste0(dlw_to_pip_dir, "/", "dlw_qs")
pip_raw_folder = paste0(dlw_to_pip_dir, "/", "pip_raw")


## First run with folder_time1 and then with folder_time2
dlw_raw_folder = folder_time2


## Step 1: Convert .dta files to .qs files -----

tic()
dlw_dta_to_qs(dlw_raw_folder = dlw_raw_folder,
              dlw_qs_folder  = dlw_qs_folder)
toc()
# 13 seconds for N files.

# Step 2: Scan the .qs folder -----
# - Record changes into changes df object
# - archive timestamped dlw_qs
# - update dlw_qs_inventory
changes <- dlw_scan_qs_folder(
  dlw_qs_folder_path    = dlw_qs_folder,
  dlw_qs_inventory_path = paste0(dlw_qs_folder, "/", "dlw_qs_inventory.qs"),
  vintage_folder_path = paste0(dlw_qs_folder, "/_vintage")
)

changes |> filter(status != "same")
dlw_qs_inv <- qread(paste0(dlw_qs_folder, "/", "dlw_qs_inventory.qs"))


# Step 3: Validation and versioning ------
validation <- dlw_validate_and_version(
  comp_df = changes,
  dlw_qs_folder_path = dlw_qs_folder,
  pip_raw_folder_path = pip_raw_folder,
  pip_raw_inventory_path = paste0(pip_raw_folder, "/", "pip_raw_inventory.qs"),
  validation_fn = NULL,
  release_label = get_current_release()
)

pip_raw_inv <- qread(paste0(pip_raw_folder, "/", "pip_raw_inventory.qs"))
pip_raw_inv |>
  filter(is_changed == TRUE)|>
  View()

# Step 4: Report creation -------
















# OLD VERSION ----
## Step 1: Scan DLW-RAW and Detect Changes in inventory -----
new_or_changed_df <- dlw_scan_folder(dlw_folder = folder_time1,
                                     inventory_file = paste0(pip_in_dlw_dir,
                                                             "/", "pip_in_dlw_inventory.qs")) # first run would be empty

## Step 2: Validate & Convert to .qs file ----
validated_df <- dlw_validate_convert(new_or_changed_df = new_or_changed_df,
                                     inventory_file = paste0(pip_in_dlw_dir,
                                                             "/", "pip_in_dlw_inventory.qs"),
                                     validated_folder = pip_in_dlw_dir)









