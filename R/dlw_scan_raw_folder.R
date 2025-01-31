library(dplyr)
library(stringr)
library(qs)
library(fs)
library(tools)
library(digest)

# 1. Main function to scan DLW-RAW folder ----
dlw_scan_folder <- function(
    dlw_folder,
    inventory_file,
    gpwg_only = TRUE
){

  # 1. Load existing inventory or create an empty inventory file

  if (!file.exists(inventory_file)) {

    message("Inventory file does not exist. Creating an empty inventory.")

    inventory <- tibble::tibble(
      file_name         = character(),
      file_hash         = character(),
      country           = character(),
      surveyid_year     = character(),
      survey_acronym    = character(),
      vermast           = character(),
      veralt            = character(),
      collection        = character(),
      module            = character(),
      last_modified     = as.POSIXct(character()),
      status            = character(),
      validation_passed = logical(),
      release_label     = character(),
      notes             = character()
    )

    qsave(inventory, inventory_file)

  } else {
    inventory <- qread(inventory_file)
  }

  # 2. List all .dta files in the DLW-RAW folder
  all_dta_files <- list.files(
    path = dlw_folder,
    pattern = "\\.dta$",
    full.names = TRUE
  )

  # 3. Keep only GPWG
  if(gpwg_only) {
    gpwg_files <- grep("_GPWG\\.dta$", all_dta_files, value = TRUE)
  }

  # 4. For each file, parse the name and gather last-modified time and hash
  file_info <- lapply(gpwg_files, function(f) {

    fbase <- file_path_sans_ext(basename(f)) # remove .dta
    parsed <- dlw_parse_filename(fbase)

    # last_modified time & hash
    mod_time <- file.info(f)$mtime
    hash <- digest::digest(file = f, algo = "md5")

    # Return a combined list
    c(parsed, list(
      full_path      = f,
      last_modified  = mod_time,
      file_hash = hash
    ))
  })

  file_info_df <- dplyr::bind_rows(lapply(file_info, tibble::as_tibble))

  # 5. NO HASH: Compare each (file_name) & (last_modified) to what's in the inventory
  # inv_info <- inventory %>%
  #   group_by(file_name) %>%
  #   summarise(inv_last_modified = max(last_modified, na.rm = TRUE), .groups = "drop")
  #
  # # Left join to see how mod times compare
  # candidates <- file_info_df %>%
  #   left_join(inv_info, by = "file_name") %>%
  #   mutate(
  #     status = dplyr::case_when(
  #       is.na(inv_last_modified) ~ "new",
  #       last_modified > inv_last_modified ~ "changed",
  #       TRUE ~ "same"
  #     )
  #   ) %>%
  #   # Keep only rows where status != "same"
  #   filter(status %in% c("new", "changed"))

  # 5. HASH: Skip any file_name + file_hash combos that are already in inventory
  known_combos <- inventory |>
    select(file_name, file_hash) |>
    distinct()

  new_or_changed <- file_info_df |>
    anti_join(known_combos, by = c("file_name", "file_hash"))

  if (nrow(new_or_changed) == 0) {
    message("No new or changed files based on hash.")
  }

  # Return only the new or changed files
  return(new_or_changed)
}


# 2. Helper function to parse file names -----
filename_example <- "AFG_2007_NRVA_V01_M_V01_A_GMD_GPWG"

dlw_parse_filename <- function(filename){

  parts <- strsplit(filename, "_")[[1]]

  country_code     <- if (length(parts) >= 1) parts[1] else NA_character_
  surveyid_year    <- if (length(parts) >= 2) parts[2] else NA_character_
  survey_acronym   <- if (length(parts) >= 3) parts[3] else NA_character_
  vermast          <- if (length(parts) >= 4) parts[4] else NA_character_
  veralt           <- if (length(parts) >= 6) parts[6] else NA_character_
  collection       <- if (length(parts) >= 8) parts[8] else NA_character_
  module           <- if (length(parts) >= 9) parts[9] else NA_character_

  list(
    file_name         = filename,
    country_code      = country_code,
    surveyid_year     = surveyid_year,
    survey_acronym    = survey_acronym,
    vermast           = vermast,
    veralt            = veralt,
    collection        = collection,
    module            = module
  )

}


