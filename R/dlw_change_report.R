#' Generate a change report between two releases
#'
#' @param current_release Character. Label of the current release (e.g. "20250202_INT")
#' @param previous_release Character. Optional. Label of previous release to compare against.
#'   If NULL, uses the chronologically previous release.
#' @param release_folder Character. Path to folder containing releases.
#' @param release_inventory_path Character. Path to release inventory file.
#' @param save_report Logical. Whether to save the report as a .qs file.
#' @param report_path Character. Optional path where to save the report if save_report=TRUE.
#'
#' @return A list containing the comparison report with components:
#'   - summary: data.frame of high-level changes
#'   - details: list of detailed comparisons per file
#'   - metadata: information about the comparison
#' @export
dlw_change_report <- function(
    current_release,
    previous_release = NULL,
    release_folder,
    release_inventory_path,
    save_report = FALSE,
    report_path = NULL
) {

  # 1. Validate and load releases ----
  if (!file.exists(release_inventory_path)) {
    cli::cli_abort("Release inventory not found at {release_inventory_path}")
  }

  # Load releases list
  releases_df <- dlw_list_releases(release_inventory_path)

  # If previous_release not specified, get chronologically previous
  if (is.null(previous_release)) {
    release_dates <- as.POSIXct(releases_df$timestamp)
    current_idx   <- which(releases_df$release == current_release)
    if (length(current_idx) == 0) {
      cli::cli_abort("Current release '{current_release}' not found")
    }
    prev_idx <- which(release_dates < release_dates[current_idx])
    if (length(prev_idx) == 0) {
      cli::cli_abort("No previous release found")
    }
    previous_release <- releases_df$release[prev_idx[length(prev_idx)]]
  }

  # Load both releases
  current_df  <- dlw_get_release_folder(release_folder, current_release)
  previous_df <- dlw_get_release_folder(release_folder, previous_release)

  # 2. Initialize report structure ----
  report <- list(
    summary = NULL,
    details = list(),
    metadata = list(
      current_release = current_release,
      previous_release = previous_release,
      comparison_date = Sys.time()
    )
  )

  # 3. Match files between releases ----
  # Join by survey identifiers
  matched <- dplyr::full_join(
    current_df,
    previous_df,
    by = c("survey_id", "veralt", "vermast"),
    suffix = c("_current", "_previous")
  )

  # 4. Categorize changes ----
  matched <- matched |> 
    dplyr::mutate(
      change_type = dplyr::case_when(
        is.na(pip_file_path_previous) ~ "new",
        is.na(pip_file_path_current)  ~ "removed",
        file_hash_current != file_hash_previous ~ "modified",
        TRUE ~ "unchanged"
      )
    )

  # 5. Generate summary ----
  report$summary <- list(
    total_files = nrow(matched),
    new_files = sum(matched$change_type == "new"),
    removed_files = sum(matched$change_type == "removed"),
    modified_files = sum(matched$change_type == "modified"),
    unchanged_files = sum(matched$change_type == "unchanged")
  )

  # 6. Generate detailed comparisons ----
  cli::cli_progress_bar(
    "Comparing files",
    total = sum(matched$change_type == "modified")
  )

  for (i in seq_len(nrow(matched))) {
    if (matched$change_type[i] == "modified") {
      # Load both versions
      current_data  <- qs::qread(matched$pip_file_path_current[i])
      previous_data <- qs::qread(matched$pip_file_path_previous[i])

      # Compare using waldo
      comparison <- waldo::compare(
        previous_data,
        current_data,
        max_diffs = 10
      )

      # Store comparison details
      report$details[[matched$survey_id[i]]] <- list(
        change_type = "modified",
        previous_path = matched$pip_file_path_previous[i],
        current_path = matched$pip_file_path_current[i],
        differences = comparison
      )

      cli::cli_progress_update()
    } else {
      # For non-modified files, just store basic info
      report$details[[matched$survey_id[i]]] <- list(
        change_type = matched$change_type[i],
        previous_path = matched$pip_file_path_previous[i],
        current_path = matched$pip_file_path_current[i]
      )
    }
  }

  cli::cli_progress_done()

  # 7. Save report if requested ----
  if (isTRUE(save_report)) {
    if (is.null(report_path)) {
      report_path <- file.path(
        release_folder,
        sprintf(
          "change_report_%s_vs_%s.qs",
          current_release,
          previous_release
        )
      )
    }
    qs::qsave(report, report_path)
    cli::cli_alert_success("Saved report to {report_path}")
  }

  # Return report
  return(invisible(report))
}
