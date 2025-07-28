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

# 3. skip_err = FALSE ----
test_that("skip_err=FALSE + directory creation failure aborts", {

  # haven't found a way to simuate this yet..
})


# 4. skip_err = TRUE but saving inventory list fails ----
test_that("skip_err=TRUE => saving inventory list fails => we skip & return NULL", {
  local_dir <- withr::local_tempdir()
  # We'll create a directory, but we make pip_raw_releases invalid
  master_path <- ""

  df <- data.frame(a=1:3)

  # Clear logs
  if ("piperr" %in% ls(.logenv)) rm("piperr", envir=.logenv)

  out <- dlw_store_release(
    pip_raw_inventory_df  = df,
    release_label         = "BAD_SAVE",
    release_folder        = local_dir,
    update_inventory_list = TRUE,
    pip_raw_releases      = master_path,
    skip_err = TRUE,
    log_err  = TRUE
  )

  # Should skip & not throw an error
  expect_null(out)

  release_file <- file.path(local_dir, "_release", "pip_raw_inventory_BAD_SAVE.qs")
  expect_true(file.exists(release_file)) # The first step can succeed if the folder is valid

  # But the master list was not saved (master_path = "")
  pip_logs <- get("piperr", envir=.logenv)
  idx <- which(names(pip_logs) == "store_release_err")
  expect_true(length(idx) >= 1)
  expect_match(pip_logs[[idx]], "Could not save master list ''|No valid file path specified")
})
