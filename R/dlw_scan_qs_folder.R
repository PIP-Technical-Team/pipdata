# Set-up
library(dplyr)
library(qs)
library(digest)
library(tools)

# NEW version ----
parse_dlw_filename <- function(fname_no_ext) {
  # Example raw base name: "ITA_2004_EU-SILC_V01_M_V05_A_GMD_BIN"
  # stable_id => remove or mask out the _V## bits to unify them:
  stable_id <- gsub("_V\\d+", "_VXX", fname_no_ext)

  # survey_id => the original
  survey_id <- fname_no_ext

  return(list(
    stable_id = stable_id,
    survey_id  = survey_id
  ))
}

dlw_scan_qs_folder <- function(
    dlw_qs_folder_path,
    dlw_qs_inventory_path,
    vintage_folder_path = NULL
) {

  # 1. Load or create dlw_qs_inventory
  #    which stores (survey_id, stable_id, file_hash)
  if (!file.exists(dlw_qs_inventory_path)) {
    message("dlw_qs_inventory.qs not found. Creating empty.")
    old_inv_df <- tibble::tibble(
      survey_id  = character(),
      stable_id = character(),
      file_hash = character()
    )
    qsave(old_inv_df, dlw_qs_inventory_path)
  } else {
    old_inv_df <- qread(dlw_qs_inventory_path)
  }

  # 2. List all .qs in folder (excluding the inventory file)
  all_qs_files <- list.files(dlw_qs_folder_path, pattern="\\.qs$", full.names=TRUE)
  if (file.exists(dlw_qs_inventory_path)) {
    all_qs_files <- setdiff(all_qs_files, dlw_qs_inventory_path)
  }

  # 3. Build new_inv_df with stable_id, survey_id, file_hash
  new_inv_df <- tibble::tibble(survey_id=character(),
                               stable_id=character(),
                               file_hash=character())
  if (length(all_qs_files) > 0) {
    new_info_list <- lapply(all_qs_files, function(f) {
      bn <- tools::file_path_sans_ext(basename(f))
      parsed <- parse_dlw_filename(bn)   # stable_id + survey_id
      h <- digest::digest(file=f, algo="md5")
      tibble::tibble(
        survey_id  = parsed$survey_id,
        stable_id = parsed$stable_id,
        file_hash = h
      )
    })
    new_inv_df <- bind_rows(new_info_list)
  }

  # 4. Compare old vs new by 'survey_id'
  comp_df <- full_join(
    old_inv_df,
    new_inv_df,
    by="stable_id",  # so if survey_id changes => "new"
    suffix=c("_old","_new")
  ) |>
    mutate(
      status = case_when(
        is.na(file_hash_old) & !is.na(file_hash_new) ~ "new",       # new survey_id
        !is.na(file_hash_old) & is.na(file_hash_new) ~ "missing",   # old survey_id gone
        !is.na(file_hash_old) & !is.na(file_hash_new) & (file_hash_old != file_hash_new) ~ "changed",
        !is.na(file_hash_old) & !is.na(file_hash_new) & (file_hash_old == file_hash_new) ~ "same",
        TRUE ~ NA_character_
      )
    ) |>
    pipload::survey_id_to_vars()

  # 5. Update the dlw_qs_inventory with the new info
  # Keep rows from new_inv_df (survey_id, stable_id, file_hash)
  updated_inv <- comp_df %>%
    select(survey_id, stable_id=stable_id_new, file_hash=file_hash_new) |>
    filter(!is.na(survey_id) & !is.na(file_hash))

  # 5a. Archive old inventory if desired
  if (!is.null(vintage_folder_path)) {
    if (!dir.exists(vintage_folder_path)) {
      dir.create(vintage_folder_path, recursive=TRUE)
    }
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    archive_path <- file.path(vintage_folder_path, paste0("dlw_qs_inventory_", ts, ".qs"))
    qsave(updated_inv, archive_path)
    message("Archived old inventory to ", archive_path)
  }

  # 5b. Overwrite the current
  qsave(updated_inv, dlw_qs_inventory_path)
  message("Updated dlw_qs_inventory at ", dlw_qs_inventory_path)

  # 6. Return comp_df
  return(comp_df)
}




# OLD version(s) ----
dlw_scan_qs_folder_v2 <- function(
    dlw_qs_folder_path,
    dlw_qs_inventory_path,
    vintage_folder_path = NULL
) {

  # 1. Load or create dlw_qs_inventory
  if (!file.exists(dlw_qs_inventory_path)) {
    message("dlw_qs_inventory.qs does not exist. Creating an empty one.")
    old_inv_df <- tibble::tibble(
      file_name = character(),
      file_hash = character()
    )
    qsave(old_inv_df, dlw_qs_inventory_path)
  } else {
    old_inv_df <- qread(dlw_qs_inventory_path)
  }

  # 2. List .qs files in dlw_qs_folder (excluding inventory)
  all_qs_files <- list.files(dlw_qs_folder_path, pattern = "\\.qs$", full.names = TRUE)
  if (file.exists(dlw_qs_inventory_path)) {
    all_qs_files <- setdiff(all_qs_files, dlw_qs_inventory_path)
  }

  if (length(all_qs_files) == 0) {
    message("No .qs files found in dlw_qs folder (excluding inventory).")
  }

  # 3. Build a new data frame for current status
  if (length(all_qs_files) > 0) {
    new_info_list <- lapply(all_qs_files, function(f) {
      base_name <- file_path_sans_ext(basename(f))
      h <- digest::digest(file = f, algo = "md5")
      tibble::tibble(
        file_name = base_name,
        file_hash = h
      )
    })
    new_inv_df <- bind_rows(new_info_list)
  } else {
    # No files => empty tibble
    new_inv_df <- tibble::tibble(
      file_name = character(),
      file_hash = character()
    )
  }

  # 4. Compare old vs new (join by file_name)
  comp_df <- full_join(
    old_inv_df,
    new_inv_df,
    by = "file_name",
    suffix = c("_old", "_new")
  ) |>
    mutate(
      status = case_when(
        is.na(file_hash_old) & !is.na(file_hash_new) ~ "new",
        !is.na(file_hash_old) & is.na(file_hash_new) ~ "missing",
        !is.na(file_hash_old) & !is.na(file_hash_new) & (file_hash_old != file_hash_new) ~ "changed",
        !is.na(file_hash_old) & !is.na(file_hash_new) & (file_hash_old == file_hash_new) ~ "same",
        TRUE ~ NA_character_
      )
    )

  # 5. Update the dlw_qs_inventory
  updated_inv <- comp_df |>
    select(file_name, file_hash = file_hash_new) |>
    filter(!is.na(file_hash))

  # 5.1 Archive old inventory into vintage
  if (!is.null(vintage_folder_path)) {
    if (!dir.exists(vintage_folder_path)) {
      dir.create(vintage_folder_path, recursive = TRUE)
    }
    time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    vintage_file_path <- file.path(
      vintage_folder_path,
      paste0("dlw_qs_inventory_", time_stamp, ".qs")
    )
    qsave(updated_inv, vintage_file_path)
    message("Archived timestamped inventory to: ", vintage_file_path)
  }

  # 5.2 Overwrite current dlw_qs_inventory
  qsave(updated_inv, dlw_qs_inventory_path)
  message("Updated dlw_qs_inventory at: ", dlw_qs_inventory_path)

  # 6. Return comparison data
  return(comp_df)
}
dlw_scan_qs_folder_v1 <- function(
    dlw_qs_folder_path,
    dlw_qs_inventory_path,
    vintage_folder_path
) {
  # 1. Load or create the inventory
  if (!file.exists(dlw_qs_inventory_path)) {
    message("dlw_qs_inventory.qs does not exist. Creating an empty one.")
    old_inv_df <- tibble::tibble(
      file_name = character(),
      file_hash = character()
    )
    qsave(old_inv_df, dlw_qs_inventory_path)
  } else {
    old_inv_df <- qread(dlw_qs_inventory_path)
  }


  # 2. List .qs files in dlw_qs_folder
  all_qs_files <- list.files(
    dlw_qs_folder,
    pattern = "\\.qs$",
    full.names = TRUE
  )

  ## Remove the inventory file from the list
  if (file.exists(dlw_qs_inventory_path)) {
    all_qs_files <- setdiff(all_qs_files, dlw_qs_inventory_path)
  }


  if (length(all_qs_files) == 0) {
    message("No .qs files found in dlw_qs folder (excluding inventory).")
  }

  # 3. Build a new data frame for the current status
  if (length(all_qs_files) > 0) {
    new_info_list <- lapply(all_qs_files, function(f) {
      base_name <- tools::file_path_sans_ext(basename(f))
      h <- digest::digest(file = f, algo = "md5")
      tibble::tibble(
        file_name = base_name,
        file_hash = h
      )
    })
    new_inv_df <- dplyr::bind_rows(new_info_list)
  } else {
    # No files => empty tibble
    new_inv_df <- tibble::tibble(
      file_name = character(),
      file_hash = character()
    )
  }

  # 4. Compare old vs new using a 'full_join' on file_name
  comp_df <- dplyr::full_join(
    old_inv_df,
    new_inv_df,
    by = "file_name",
    suffix = c("_old", "_new")
  )

  # 5. Assign status
  #    - If file_hash_old is NA and file_hash_new is not -> "new"
  #    - If file_hash_old is not NA and file_hash_new is NA -> "missing"
  #    - If both not NA but differ -> "changed"
  #    - Else -> "same"

  comp_df <- comp_df |>
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(file_hash_old) & !is.na(file_hash_new) ~ "new",
        !is.na(file_hash_old) & is.na(file_hash_new) ~ "missing",
        !is.na(file_hash_old) & !is.na(file_hash_new) &
          (file_hash_old != file_hash_new) ~ "changed",
        !is.na(file_hash_old) & !is.na(file_hash_new) &
          (file_hash_old == file_hash_new) ~ "same",
        TRUE ~ NA_character_
      )
    )

  # 6. Update inventory ----
  updated_inv <- comp_df |>
    select(file_name, file_hash = file_hash_new) |>
    filter(!is.na(file_hash))

  ## 6.1 Vintage dlw_qs_inventory_(timestamp).qs -----
  if (file.exists(dlw_qs_inventory_path)) {
  if (is.null(vintage_folder_path)) {
    vintage_folder_path <- file.path(dirname(dlw_qs_folder), "/", "_vintage")
  }
  if (!dir.exists(vintage_folder_path)) {
    dir.create(vintage_folder_path, recursive = TRUE)
  }

  time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  vintage_file_path <- file.path(
    vintage_folder_path,
    paste0("dlw_qs_inventory_", time_stamp, ".qs")
  )

  qsave(updated_inv, vintage_file_path)
  message("Archived timestamped inventory to: ", vintage_file_path)
  }


  ## 6.2 Current dlw_qs_inventory.qs -----
  if (file.exists(dlw_qs_inventory_path)) {
    qsave(updated_inv, dlw_qs_inventory_path)
    message("Updated dlw_qs_inventory at: ", dlw_qs_inventory_path)
  }



  # 6. Return the full comparison data frame
  return(comp_df)
}


