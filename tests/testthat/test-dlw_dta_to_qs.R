# Test for dlw_dta_to_qs

# 1. No .dta in folder -> do_dlw_files -> abort
test_that("Function aborts if no .dta files are found", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir  <- withr::local_tempdir()

  # No .dta in local_dta_dir => expect error with class no_dlw_files
  expect_error(
    dlw_dta_to_qs(dlw_raw_folder = local_dta_dir, dlw_qs_folder = local_qs_dir),
    class = "no_dlw_files"  # From your function's class=c("no_dlw_files", "piperr")
  )
})

# 2. .dta file is found -> normal flow
test_that("Normal flow with a valid .dta file", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir  <- withr::local_tempdir()

  # Create a small test data frame
  df_test <- data.frame(x = 1:5, y = letters[1:5])

  # Write it as a .dta using haven
  test_dta_path <- file.path(local_dta_dir, "test_ok.dta")
  haven::write_dta(df_test, test_dta_path)

  # Run the function
  dlw_dta_to_qs(dlw_raw_folder = local_dta_dir, dlw_qs_folder = local_qs_dir)

  # Check that a .qs file is created
  qs_files <- list.files(local_qs_dir, pattern = "\\.qs$", full.names = TRUE)
  expect_length(qs_files, 1)

  # Check that the .qs can be read and matches original
  df_qs <- qs::qread(qs_files)
  expect_equal(df_qs, df_test, ignore_attr = TRUE)
})

# 3. Read error is logged/skipped if skip_err=TRUE
test_that("Read error is logged/skipped if skip_err=TRUE", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir  <- withr::local_tempdir()

  # Clean up .logenv
  if (exists("piperr", envir = .logenv)) {
    rm("piperr", envir = .logenv)
  }

  # 3a) Make a valid file
  df_test <- data.frame(x = 1:5)
  valid_dta_path <- file.path(local_dta_dir, "valid.dta")
  haven::write_dta(df_test, valid_dta_path)

  # 3b) Make a corrupted file (renamed text file)
  broken_dta_path <- file.path(local_dta_dir, "corrupt.dta")
  writeLines("Not a real DTA file", con = broken_dta_path)

  # Run with skip_err=TRUE => expect a warning, not an error
  expect_message(
    dlw_dta_to_qs(
      dlw_raw_folder = local_dta_dir,
      dlw_qs_folder  = local_qs_dir,
      skip_err       = TRUE,
      log_err        = TRUE
    ),
    regexp = "Skipping file 'corrupt.dta' due to read error"
  )

  # The valid file should still be converted
  qs_files <- list.files(local_qs_dir, pattern = "\\.qs$")
  expect_equal(qs_files, "valid.qs")

  # Confirm that something was logged in .logenv under "piperr"
  expect_true("piperr" %in% ls(.logenv))
  piperr_logs <- get("piperr", envir = .logenv)
  # Expect that there's an entry with class "dta_read_err"
  read_err_idx <- which(names(piperr_logs) == "dta_read_err")
  expect_true(length(read_err_idx) == 1)
  expect_match(piperr_logs[[read_err_idx]], "Could not read 'corrupt.dta'")
})

# 4. Read error aborts if skip_err=FALSE
test_that("Read error aborts if skip_err=FALSE", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir  <- withr::local_tempdir()

  # Clean .logenv if needed
  if (exists("piperr", envir = .logenv)) {
    rm("piperr", envir = .logenv)
  }

  # Create a corrupted file
  broken_dta_path <- file.path(local_dta_dir, "broken_read.dta")
  writeLines("Bad content for DTA", broken_dta_path)

  # Also create a valid file
  df_test <- data.frame(x = 1:5)
  good_dta_path <- file.path(local_dta_dir, "good_read.dta")
  haven::write_dta(df_test, good_dta_path)

  # Because skip_err=FALSE, we expect the function to throw an error right away
  expect_error(
    dlw_dta_to_qs(
      dlw_raw_folder = local_dta_dir,
      dlw_qs_folder  = local_qs_dir,
      skip_err       = FALSE,
      log_err        = TRUE
    ),
    regexp = "Could not read 'broken_read.dta'"
  )

  # Check that no .qs was created (the process should've halted)
  qs_files <- list.files(local_qs_dir, pattern = "\\.qs$")
  expect_length(qs_files, 0)

  # Check logs
  expect_true("piperr" %in% ls(.logenv))
  piperr_logs <- get("piperr", envir = .logenv)
  read_err_idx <- which(names(piperr_logs) == "dta_read_err")
  expect_true(length(read_err_idx) == 1)
})

