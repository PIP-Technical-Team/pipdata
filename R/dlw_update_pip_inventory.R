

dlw_update_inventory <- function(
    # inventory update
    input_validated_df,
    input_inventory_file,
    output_inventory_vintage_folder,
    # inventory report
    output_report_file        = "y:/PIP-VALIDATED/_report/dlw_diff_report.qs",
    output_report_vintage_folder     = "y:/PIP-VALIDATED/_report/_vintage"
) {

  # 1. If nothing is validated, skip updating -----
  if (nrow(validated_df) == 0) {
    message("No validated files to update in the inventory.")
    return(invisible(NULL))
  }

  # 2. Load current inventory ----
  if (!file.exists(inventory_file)) {
    stop("Inventory file does not exist: ", inventory_file)
  }

  old_inventory <- qread(inventory_file)

  # 3. Archive the old inventory ----
  time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  if (!dir.exists(inventory_vintage_folder)) {
    dir_create(inventory_vintage_folder, recurse = TRUE)
  }
  vintage_path <- file.path(
    inventory_vintage_folder,
    paste0("dlw_inventory_", time_stamp, ".qs")
  )
  qsave(old_inventory, vintage_path)

  # 4. Combine old inventory with new records from validated_df.
  #    - Typically, validated_df contains only *successful* validations
  #      plus rows that might have validated=FALSE if they failed or were duplicates.
  #    - Decide how you want to handle invalid rows or duplicates.

  # We'll convert validated_df to the same schema as inventory (so we can bind).
  # That may require adding missing columns. Let’s do a simple approach:

  # Make a template of columns from old_inventory
  inv_cols <- names(old_inventory)

  # We’ll ensure validated_df has these columns (even if blank or NA).
  for (col in inv_cols) {
    if (!col %in% names(validated_df)) {
      validated_df[[col]] <- NA
    }
  }

  # And ensure validated_df has *only* columns in old_inventory
  validated_df <- validated_df[, inv_cols]

  # Now bind them
  new_inventory <- bind_rows(old_inventory, validated_df)

  # 5. Optionally produce a difference report (compare old vs new)
  diff_info <- tryCatch({
    # For demonstration, a trivial summary of how many new rows were added.
    list(
      timestamp    = Sys.time(),
      old_rows     = nrow(old_inventory),
      new_rows     = nrow(new_inventory),
      added_rows   = nrow(validated_df)
    )
  }, error = function(e) {
    list(timestamp = Sys.time(), error_message = e$message)
  })

  # 6. Archive old diff if it exists, then save the new diff
  if (file.exists(report_file)) {
    if (!dir.exists(report_vintage_folder)) {
      dir_create(report_vintage_folder, recurse = TRUE)
    }
    old_diff <- qread(report_file)
    vintage_diff_path <- file.path(
      report_vintage_folder,
      paste0("dlw_diff_", time_stamp, ".qs")
    )
    qsave(old_diff, vintage_diff_path)
  }

  qsave(diff_info, report_file)

  # 7. Save the updated inventory
  qsave(new_inventory, inventory_file)

  message("Inventory updated. Old inventory archived at: ", vintage_path)
  message("Diff report updated at: ", report_file)

  # Return info in case you want to use it
  return(list(
    new_inventory = new_inventory,
    diff_info     = diff_info
  ))
}
