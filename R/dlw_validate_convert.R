library(dplyr)
library(haven)
library(qs)
library(fs)
library(stringr)
library(lubridate)

# HASH (NEW) ----
## V3 ----
dlw_validate_convert_v3 <- function(
    new_or_changed_df,
    validated_folder,
    validation_fn
){

  # If there are no changes, give message:
  if (nrow(candidates_df) == 0) {
    message("No candidates to validate.")
    return(tibble::tibble())
  }

  # Start list of validated for storage
  results_list <- vector("list", nrow(candidates_df))

  # Check existing validated files
  existing_qs <- list.files(validated_folder, pattern = "\\.qs$",
                            full.names = TRUE)


}


### .qs filename parser ----
# Should take only last v as our own versioning number
parse_qs_filename <- function(qs_filename) {

  fname_no_ext <- file_path_sans_ext(qs_filename)

  version_str <- str_extract(fname_no_ext, "_v\\d+$")

  if (is.na(version_str)) {

    return(list(
      base_name = fname_no_ext,
      version   = NA_integer_
    ))

  } else {

    version_digits <- sub("^_v", "", version_str)
    version_num    <- suppressWarnings(as.integer(version_digits))


    base_name <- sub("_v\\d+$", "", fname_no_ext)

    return(list(
      base_name = base_name,
      version   = version_num
    ))
  }
}



## V2 ----
dlw_validate_convert_v2 <- function(
    new_or_changed_df,
    inventory_file,
    validated_folder
) {

  if (nrow(new_or_changed_df) == 0) {
    message("No new or changed files to process.")
    return(tibble::tibble())
  }

  inventory <- qread(inventory_file)

  results_list <- vector("list", nrow(new_or_changed_df))

  for (i in seq_len(nrow(new_or_changed_df))) {
    row_i <- new_or_changed_df[i, ]

    file_path <- row_i$full_path
    file_name <- row_i$file_name
    file_hash  <- row_i$file_hash

    # Check file_hash and file_name, if already in inventory, skip.
    already_validated <- inventory %>%
      filter(file_name == file_name, file_hash == file_hash) |>
      nrow() > 0

    if (already_validated) {
      message("Skipping already validated file: ", file_name)
      results_list[[i]] <- tibble::tibble(
        file_name = file_name,
        file_hash = file_hash,
        validated = FALSE,  # Because we didn't re-validate
        reason    = "Duplicate hash",
        qs_path   = NA_character_
      )
      next
    }

    # Otherwise, proceed with reading & validating
    data_raw <- tryCatch(
      haven::read_dta(file_path),
      error = function(e) {
        message("Error reading file: ", file_path, ": ", e$message)
        NULL
      }
    )

    if (is.null(data_raw)) {
      results_list[[i]] <- tibble::tibble(
        file_name = file_name,
        file_hash = file_hash,
        validated = FALSE,
        reason    = "File read error",
        qs_path   = NA_character_
      )
      next
    }

    # Simple validation for test: must have >0 rows
    is_valid <- (nrow(data_raw) > 0)
    val_reason <- if (!is_valid) "Empty dataset" else NA_character_

    if (!is_valid) {
      results_list[[i]] <- tibble::tibble(
        file_name = file_name,
        file_hash = file_hash,
        validated = FALSE,
        reason    = val_reason,
        qs_path   = NA_character_
      )
      next
    }

    # Passed validation => convert to .qs
    time_stamp <- format(Sys.time(), "%Y%m%d_%H%M")
    qs_filename <- paste0(file_name, "_", time_stamp, ".qs")
    qs_fullpath <- file.path(validated_folder, qs_filename)

    qsave(data_raw, qs_fullpath)

    # Build result
    results_list[[i]] <- tibble::tibble(
      file_name = file_name,
      file_hash = file_hash,
      validated = TRUE,
      reason    = NA_character_,
      qs_path   = qs_fullpath,
      date_versioned = as.character(Sys.time())
    )
  }

  final_df <- bind_rows(results_list)
  return(final_df)
}


# NO HASH (OLD) ----
dlw_validate_convert_old <- function(
    candidates_df, # file produced by dlw_scan_folder()
    validated_folder  = NULL,    # folder with validated files which Diana will use
    validation_fn     = NULL                   # validation function to process files before storing
) {

  if (nrow(candidates_df) == 0) {
    message("No new or changed files to process.")
    return(tibble::tibble())
  }

  # 1. Start empty list
  results <- vector("list", nrow(candidates_df))

  for (i in seq_len(nrow(candidates_df))) {
    row_i <- candidates_df[i, ]

    file_path <- row_i$full_path
    base_name <- row_i$file_name

    # 2. Read the .dta file
    data_raw <- tryCatch(
      haven::read_dta(file_path),
      error = function(e) {
        message("Error reading file: ", file_path, " - ", e$message)
        NULL
      }
    )

    if (is.null(data_raw)) {
      results[[i]] <- tibble::tibble(
        file_name         = base_name,
        full_path         = file_path,
        validated         = FALSE,
        validation_error  = "File read error",
        qs_path           = NA_character_,
        status            = "invalid",
        date_versioned    = as.character(Sys.time())
      )
      next
    }

    # 3. Run validation
    ## Example with random validation step
    is_valid <- TRUE
    validation_message <- NA_character_

    if (nrow(data_raw) == 0) {
      is_valid <- FALSE
      validation_message <- "No rows in dataset."
    }

    # Validation function
    if (!is.null(validation_fn) && is_valid) {
      custom_val <- validation_fn(data_raw)
      is_valid   <- custom_val$is_valid
      validation_message <- custom_val$message
    }

    if (!is_valid) {
      results[[i]] <- tibble::tibble(
        file_name         = base_name,
        full_path         = file_path,
        validated         = FALSE,
        validation_error  = validation_message,
        qs_path           = NA_character_,
        status            = "invalid",
        date_versioned    = as.character(Sys.time())
      )
      next
    }

    # 4. If validation passed, write .qs file
    time_stamp <- format(Sys.time(), "%Y%m%d_%H%M")

    qs_filename <- paste0(base_name, "_", time_stamp, ".qs")
    qs_fullpath <- file.path(validated_folder, qs_filename)

    qsave(data_raw, qs_fullpath)

    # 5. Prepare result row for inventory update
    results[[i]] <- tibble::tibble(
      file_name         = base_name,
      full_path         = file_path,
      validated         = TRUE,
      validation_error  = NA_character_,
      qs_path           = qs_fullpath,
      status            = "validated",
      date_versioned    = as.character(Sys.time())
    )
  }

  # Combine all results
  out_df <- dplyr::bind_rows(results)
  return(out_df)
}
