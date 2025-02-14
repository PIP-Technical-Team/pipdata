# 1. update_inventory_list = FALSE ----
test_that("Usage with update_inventory_list=FALSE", {
  local_dir <- withr::local_tempdir()
  df <- data.frame(x = 1:3)

  dlw_store_release(
    pip_raw_inventory_df  = df,
    release_label         = "RELEASE_LABEL",
    release_folder        = local_dir,
    update_inventory_list = FALSE,
    pip_raw_releases      = NULL,
    log_err  = TRUE,
    skip_err = FALSE
  )

  # Check the file
  release_file <- file.path(local_dir, "_release", "pip_raw_inventory_RELEASE_LABEL.qs")
  expect_true(file.exists(release_file))
  df2 <- qs::qread(release_file)
  expect_equal(df, df2)
})


# 2. update_inventory_list = TRUE ----
test_that("Usage with update_inventory_list=TRUE", {
  local_dir <- withr::local_tempdir()
  inventory_path <- file.path(local_dir, "inventory_list.qs")
  df <- data.frame(survey_id = "ABC", val = 1:2)

  out_list <- dlw_store_release(
    pip_raw_inventory_df  = df,
    release_label         = "RELEASE_LABEL",
    release_folder        = local_dir,
    update_inventory_list = TRUE,
    pip_raw_releases      = inventory_path,
    log_err  = TRUE,
    skip_err = FALSE
  )

  # The single .qs file in _release
  release_file <- file.path(local_dir, "_release", "pip_raw_inventory_RELEASE_LABEL.qs")
  expect_true(file.exists(release_file))

  # The inventory list
  expect_true(file.exists(inventory_path))
  inventory_data <- qs::qread(inventory_path)
  # Should have an entry named "REL_2025"
  expect_true("RELEASE_LABEL" %in% names(inventory_data))

  # Function returns the updated list
  expect_equal(out_list, inventory_data)
})

