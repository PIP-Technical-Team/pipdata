# Set-up
library(dplyr)
library(qs)
library(tools)

# NEW (release) -----
# - Uses comp_df to see which files are new or changed.
# - For each new/changed file, we create a new version _vNN.qs in pip_raw and a new row in pip_raw_inventory.qs.
# - Unchanged files remain in pip_raw_inventory from previous runs, so the final inventory has all needed files.
# - We include a release_label argument if we want to mark these new versions for a specific release.
# - If no release is set, we do NA.

dlw_validate_and_version <- function(
    comp_df,
    dlw_qs_folder_path,
    pip_raw_folder_path,
    pip_raw_inventory_path,
    release_label = NA_character_,
    validation_fn = NULL
) {

  # 1. Filter comp_df for new/changed
  new_or_changed_df <- comp_df |> filter(status %in% c("new", "changed"))

  if (nrow(new_or_changed_df) == 0) {
    message("No new or changed files to validate.")
    return(tibble::tibble())
  }

  # 2. Ensure pip_raw_folder exists
  if (!dir.exists(pip_raw_folder_path)) {
    dir.create(pip_raw_folder_path, recursive = TRUE)
  }

  # 3. Load or create pip_raw_inventory
  if (!file.exists(pip_raw_inventory_path)) {
    message("pip_raw_inventory.qs does not exist. Creating an empty one.")
    pip_inv_df <- tibble::tibble(
      file_name         = character(),
      pip_raw_file_path = character(),
      version           = integer(),
      is_latest         = logical(),
      is_changed        = logical(),
      release_label     = character(),
      date_validated    = as.POSIXct(character())
    )
    qsave(pip_inv_df, pip_raw_inventory_path)
  } else {
    pip_inv_df <- qread(pip_raw_inventory_path)
  }

  # 4. We'll store results for each file validated
  validation_results <- vector("list", nrow(new_or_changed_df))

  for (i in seq_len(nrow(new_or_changed_df))) {
    row_i <- new_or_changed_df[i, ]
    base_nm <- row_i$file_name

    # Path to the .qs in the mirror folder
    dlw_qs_file_path <- file.path(dlw_qs_folder_path, paste0(base_nm, ".qs"))

    # Attempt to read the data (for validation)
    df <- tryCatch(
      qread(dlw_qs_file_path),
      error = function(e) {
        message("Error reading mirror file: ", dlw_qs_file_path, " - ", e$message)
        return(NULL)
      }
    )

    if (is.null(df)) {
      # Mark validation failed
      validation_results[[i]] <- tibble::tibble(
        file_name        = base_nm,
        validated        = FALSE,
        reason           = "Could not read .qs",
        new_version_path = NA_character_
      )
      next
    }

    # Apply simple or custom validation checks
    is_valid <- TRUE
    fail_reason <- NA_character_

    # Example: must have > 0 rows
    if (nrow(df) == 0) {
      is_valid <- FALSE
      fail_reason <- "Empty dataset"
    }
    if (!is.null(validation_fn) && is_valid) {
      # Suppose validation_fn returns list(is_valid=TRUE/FALSE, reason=...)
      val_res <- validation_fn(df)
      is_valid <- val_res$is_valid
      fail_reason <- val_res$reason
    }

    if (!is_valid) {
      validation_results[[i]] <- tibble::tibble(
        file_name        = base_nm,
        validated        = FALSE,
        reason           = fail_reason,
        new_version_path = NA_character_
      )
      next
    }

    # If valid => create a new version in pip_raw
    # 1) Find current max version for base_nm
    existing_versions <- pip_inv_df %>%
      filter(file_name == base_nm) %>%
      pull(version)

    max_ver <- if (length(existing_versions) == 0) 0 else max(existing_versions, na.rm=TRUE)
    new_ver <- max_ver + 1

    # 2) Build new versioned file name
    version_str <- sprintf("v%02d", new_ver)
    versioned_file_name <- paste0(base_nm, "_", version_str, ".qs")
    versioned_file_path <- file.path(pip_raw_folder_path, versioned_file_name)

    # 3) Save
    qsave(df, versioned_file_path)

    # 4) Mark older versions as is_latest=FALSE
    pip_inv_df <- pip_inv_df %>%
      mutate(is_latest = if_else(file_name == base_nm, FALSE, is_latest))

    # 5) Insert new row
    new_row <- tibble::tibble(
      file_name         = base_nm,
      pip_raw_file_path = versioned_file_path,
      version           = new_ver,
      is_latest         = TRUE,
      is_changed        = TRUE,
      release_label     = release_label,   # <--- assigned or NA
      date_validated    = Sys.time()
    )
    pip_inv_df <- bind_rows(pip_inv_df, new_row)

    # 6) Record success
    validation_results[[i]] <- tibble::tibble(
      file_name        = base_nm,
      validated        = TRUE,
      reason           = NA_character_,
      new_version_path = versioned_file_path
    )
  }

  # Combine validation results
  validation_report <- bind_rows(validation_results)

  # Save updated pip_raw_inventory
  # Optional: archive old if needed, similar to how we do vintage in dlw_qs
  qsave(pip_inv_df, pip_raw_inventory_path)
  message("Updated pip_raw_inventory at: ", pip_raw_inventory_path)

  return(validation_report)
}







# OLD (no release) -----
dlw_validate_and_version <- function(
    comp_df,
    dlw_qs_folder_path,
    pip_raw_folder_path,
    pip_raw_inventory_path,
    validation_fn = NULL
) {
  library(dplyr)
  library(qs)

  # 1. Split comp_df into subsets
  #    - new_or_changed: c("new", "changed")
  #    - same_files: status == "same"
  #    - missing_files: status == "missing" (not handled here)
  new_or_changed_df <- comp_df %>%
    filter(status %in% c("new", "changed"))

  same_df <- comp_df %>%
    filter(status == "same")

  # 2. If no new/changed, we might only update "same" files in the inventory
  #    so we won't return early, we do still want to update the inventory
  #    for "same" statuses.

  # 3. Ensure pip_raw_folder exists
  if (!dir.exists(pip_raw_folder_path)) {
    dir.create(pip_raw_folder_path, recursive = TRUE)
  }

  # 4. Load or create the pip_raw_inventory
  if (!file.exists(pip_raw_inventory_path)) {
    message("pip_raw_inventory.qs does not exist. Creating an empty one.")
    pip_inv_df <- tibble::tibble(
      base_name         = character(),
      original_qs_path  = character(),  # from dlw_qs_folder
      pip_raw_file_path = character(),  # the versioned file in pip_raw
      version           = integer(),
      is_latest         = logical(),
      is_changed        = logical(),
      date_validated    = as.POSIXct(character())
    )
    qsave(pip_inv_df, pip_raw_inventory_path)
  } else {
    pip_inv_df <- qread(pip_raw_inventory_path)
  }

  # 5. Build a data frame to store validation results
  #    (which files validated OK, which failed, etc.)
  validation_results <- vector("list", nrow(new_or_changed_df))

  # 6. Process "new" or "changed" files => create new version row
  if (nrow(new_or_changed_df) > 0) {
    for (i in seq_len(nrow(new_or_changed_df))) {
      row_i <- new_or_changed_df[i, ]
      base_nm <- row_i$file_name

      dlw_qs_file_path <- file.path(dlw_qs_folder_path, paste0(base_nm, ".qs"))

      # 6a. Attempt to read the .qs from mirror
      df <- tryCatch(
        qread(dlw_qs_file_path),
        error = function(e) {
          message("Error reading mirror file: ", dlw_qs_file_path, " - ", e$message)
          return(NULL)
        }
      )

      if (is.null(df)) {
        # Mark as validation failed
        validation_results[[i]] <- tibble::tibble(
          base_name         = base_nm,
          validated         = FALSE,
          reason            = "Could not read .qs",
          new_version_path  = NA_character_
        )
        next
      }

      # 6b. Apply validation
      is_valid <- TRUE
      fail_reason <- NA_character_

      if (nrow(df) == 0) {
        is_valid <- FALSE
        fail_reason <- "Empty dataset"
      }
      if (!is.null(validation_fn) && is_valid) {
        val_res <- validation_fn(df)
        is_valid <- val_res$is_valid
        fail_reason <- val_res$reason
      }

      if (!is_valid) {
        validation_results[[i]] <- tibble::tibble(
          base_name         = base_nm,
          validated         = FALSE,
          reason            = fail_reason,
          new_version_path  = NA_character_
        )
        next
      }

      # 6c. If valid => create a new version
      existing_versions <- pip_inv_df %>%
        filter(base_name == base_nm) %>%
        pull(version)

      max_ver <- if (length(existing_versions) == 0) 0 else max(existing_versions, na.rm=TRUE)
      new_ver <- max_ver + 1

      version_str <- sprintf("v%02d", new_ver)
      versioned_file_name <- paste0(base_nm, "_", version_str, ".qs")
      versioned_file_path <- file.path(pip_raw_folder_path, versioned_file_name)

      # Save
      qsave(df, versioned_file_path)

      # Mark older versions is_latest=FALSE
      pip_inv_df <- pip_inv_df %>%
        mutate(
          is_latest = if_else(base_name == base_nm, FALSE, is_latest)
        )

      # is_changed = TRUE for both "new" and "changed"
      new_row <- tibble::tibble(
        base_name         = base_nm,
        original_qs_path  = dlw_qs_file_path,
        pip_raw_file_path = versioned_file_path,
        version           = new_ver,
        is_latest         = TRUE,
        is_changed        = TRUE,
        date_validated    = Sys.time()
      )
      pip_inv_df <- bind_rows(pip_inv_df, new_row)

      # Record success
      validation_results[[i]] <- tibble::tibble(
        base_name         = base_nm,
        validated         = TRUE,
        reason            = NA_character_,
        new_version_path  = versioned_file_path
      )
    }
  }

  # 7. Combine validation results
  validation_report <- bind_rows(validation_results)

  # 8. Process "same" => set is_changed=FALSE for the existing is_latest row
  #    We do NOT create a new version row, just update the existing top version if it exists.
  if (nrow(same_df) > 0) {
    for (i in seq_len(nrow(same_df))) {
      row_i <- same_df[i, ]
      base_nm <- row_i$file_name

      # Find the *most recent* (is_latest=TRUE) row for this file
      # and set is_changed=FALSE
      # If there's no row for some reason, we skip.
      existing_latest_idx <- pip_inv_df %>%
        mutate(row_index = row_number()) %>%
        filter(base_name == base_nm, is_latest == TRUE) %>%
        pull(row_index)

      if (length(existing_latest_idx) == 1) {
        # Update that row
        pip_inv_df[existing_latest_idx, "is_changed"] <- FALSE
      }
    }
  }

  # 9. Archive old inventory, then overwrite with updated
  if (file.exists(pip_raw_inventory_path)) {
    vintage_dir <- file.path(dirname(pip_raw_inventory_path), "_vintage")
    if (!dir.exists(vintage_dir)) dir.create(vintage_dir, recursive = TRUE)
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    archive_name <- file.path(vintage_dir, paste0("pip_raw_inventory_", ts, ".qs"))

    old_inventory <- qread(pip_raw_inventory_path)
    qsave(old_inventory, archive_name)
    message("Archived old pip_raw_inventory to: ", archive_name)
  }

  # Save the new inventory
  qsave(pip_inv_df, pip_raw_inventory_path)
  message("Updated pip_raw_inventory at: ", pip_raw_inventory_path)

  # 10. Return validation report
  return(validation_report)
}

