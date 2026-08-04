# Test for dlw_dta_to_qs
#
# Note: dlw_dta_to_qs() calls pipfun::get_wrk_release() at startup to assert a
# working release exists.  In unit tests there is no real release, so we mock
# that call with local_mocked_bindings() — it becomes a no-op and lets the
# tests exercise the actual file logic.

# 1. No .dta in folder -> abort
test_that("Function aborts if no .dta files are found", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir  <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(NULL),
    .package = "pipfun"
  )

  expect_error(
    dlw_dta_to_qs(dlw_raw_folder = local_dta_dir, dlw_qs_folder = local_qs_dir),
    regexp = "No .dta files found"
  )
})

# 2. .dta file is found -> normal flow
test_that("Normal flow with a valid .dta file", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(NULL),
    .package = "pipfun"
  )

  df_test <- data.frame(x = 1:5, y = letters[1:5])
  haven::write_dta(df_test, file.path(local_dta_dir, "test_ok.dta"))

  dlw_dta_to_qs(dlw_raw_folder = local_dta_dir, dlw_qs_folder = local_qs_dir)

  qs_files <- list.files(local_qs_dir, pattern = "\\.qs$", full.names = TRUE)
  expect_length(qs_files, 1)

  df_qs <- qs::qread(qs_files)
  expect_equal(df_qs, df_test, ignore_attr = TRUE)
})

# 3. Corrupted .dta file is skipped with a warning; valid file is still converted
test_that("Read error is skipped with a warning and valid files are still converted", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(NULL),
    .package = "pipfun"
  )

  df_test <- data.frame(x = 1:5)
  haven::write_dta(df_test, file.path(local_dta_dir, "valid.dta"))
  writeLines("Not a real DTA file", file.path(local_dta_dir, "corrupt.dta"))

  # cli_alert_warning() emits a message-class condition in testthat
  expect_message(
    dlw_dta_to_qs(dlw_raw_folder = local_dta_dir, dlw_qs_folder = local_qs_dir),
    regexp = "corrupt\\.dta"
  )

  qs_files <- list.files(local_qs_dir, pattern = "\\.qs$")
  expect_true("valid.qs" %in% qs_files)
})

# 4. All files corrupted -> no .qs files created, function completes without abort
test_that("All files corrupted -> no .qs files created, no abort", {
  local_dta_dir <- withr::local_tempdir()
  local_qs_dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(NULL),
    .package = "pipfun"
  )

  writeLines("Bad content", file.path(local_dta_dir, "broken.dta"))

  expect_no_error(
    suppressMessages(
      dlw_dta_to_qs(
        dlw_raw_folder = local_dta_dir,
        dlw_qs_folder = local_qs_dir
      )
    )
  )

  qs_files <- list.files(local_qs_dir, pattern = "\\.qs$")
  expect_length(qs_files, 0)
})


# GC Note: Save errors need to be figured out, not sure how to simulate them.


