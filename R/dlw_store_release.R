dlw_store_release <- function(
    release_label,
    pip_raw_inventory_df,    # a data frame with your pipeline rows
    pip_raw_releases
) {
  # 1. Load existing JSON or create empty list
  if (file.exists(pip_raw_releases)) {
    # use simplifyVector=FALSE so we keep it as a named list-of-lists
    master_list <- fromJSON(pip_raw_releases, simplifyVector=FALSE)
  } else {
    master_list <- list()  # empty
  }

  # 2. Convert pip_raw_inventory_df to a list-of-rows in a way
  #    that fromJSON(simplifyVector=TRUE) can read as a data frame
  # 'jsonlite' automatically interprets an array of consistent objects
  # as a data frame if each row has identical columns & types.

  # We'll ensure each column is *atomic*, then we do:
  # row_lists <- apply(pip_raw_inventory_df, 1, as.list)
  # But if you want each column to remain typed properly, that's fine as long
  # as they're not nested. We'll do something simpler:

  row_lists <- lapply(seq_len(nrow(pip_raw_inventory_df)), function(i) {
    # for each row, build a named list
    row_as_list <- as.list(pip_raw_inventory_df[i, , drop=FALSE])
    # Flatten any factor columns to character, etc.
    row_as_list <- lapply(row_as_list, function(x) {
      if (is.factor(x)) as.character(x) else x
    })
    row_as_list
  })

  # 3. Build the new release entry
  new_entry <- list(
    timestamp = as.character(Sys.time()),
    data      = row_lists
  )

  # 4. Insert it under the top-level key = release_label
  master_list[[release_label]] <- new_entry

  # 5. Write JSON back
  # 'auto_unbox=TRUE' ensures single-value fields remain scalars,
  # 'pretty=TRUE' for readability
  write_json(master_list, pip_raw_releases, pretty=TRUE, auto_unbox=TRUE)
  cli_alert_success("Archived release '{release_label}' with {nrow(pip_raw_inventory_df)} rows into {pip_raw_releases}")
}

