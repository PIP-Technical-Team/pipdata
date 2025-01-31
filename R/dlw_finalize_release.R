dlw_finalize_release <- function(
    pip_raw_inventory_path,
    release_name
) {
  # 1. Read current inventory
  inv <- qs::qread(pip_raw_inventory_path)

  # 2. Create a subfolder for releases
  release_folder <- file.path(dirname(pip_raw_inventory_path), "_releases")
  if (!dir.exists(release_folder)) {
    dir.create(release_folder, recursive = TRUE)
  }

  # 3. Write out a snapshot
  time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  snap_name <- paste0("pip_raw_inventory_", release_name, "_", time_stamp, ".qs")
  snap_path <- file.path(release_folder, snap_name)

  qs::qsave(inv, snap_path)
  message("Release snapshot saved: ", snap_path)
}
