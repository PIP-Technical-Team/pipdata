
# Tests removed: dlw_list_releases(), dlw_get_release_folder(), and
# dlw_get_release_list() do not exist in the package.

if (FALSE) {
  test_that("dlw_list_releases() -> no file => returns empty df", {
    local_path <- file.path(tempdir(), "nonexistent_releases.qs")

    out_df <- dlw_list_releases(local_path)
    expect_s3_class(out_df, "data.frame")
    expect_equal(nrow(out_df), 0)
    expect_named(out_df, c("label", "year", "month", "type"))
  })

  test_that("dlw_list_releases() -> empty named list => returns empty df", {
    local_file <- withr::local_file("empty_master_list.qs")

    qs::qsave(list(), local_file)

    out_df <- dlw_list_releases(local_file)
    expect_s3_class(out_df, "data.frame")
    expect_equal(nrow(out_df), 0)
  })

  test_that("dlw_list_releases() -> multiple entries => parse labels", {
    local_file <- withr::local_file("multi_master_list.qs")

    my_list <- list(
      "20250202_INT" = list(timestamp = "...", data = list()),
      "20230302_PROD" = list(timestamp = "...", data = list())
    )
    qs::qsave(my_list, local_file)

    out_df <- dlw_list_releases(local_file)

    # We expect 2 rows
    expect_equal(nrow(out_df), 2)
    expect_true(all(c("label", "year", "month", "type") %in% names(out_df)))

    # Check parsing
    row_int <- out_df[out_df$label == "20250202_INT", ]
    row_simple <- out_df[out_df$label == "20230302_PROD", ]

    # row_int => year=2025, month=02, type= everything after underscore => INT
    expect_equal(row_int$year, "2025")
    expect_equal(row_int$month, "02")
    expect_equal(row_int$type, "INT")

    # row_simple => there's an underscore? "202312_SIMPLE"
    # year=2023, month=12, type=SIMPLE
    expect_equal(row_simple$year, "2023")
    expect_equal(row_simple$month, "03")
    expect_equal(row_simple$type, "PROD")
  })

  # dlw_get_release_folder() ----

  test_that("dlw_get_release_folder() -> missing file => error", {
    local_dir <- withr::local_tempdir()
    release_label <- "FAKE_LABEL"
    # no .qs is created
    expect_error(
      dlw_get_release_folder(
        release_folder = local_dir,
        release_label = release_label
      ),
      "Release file not found"
    )
  })

  test_that("dlw_get_release_folder() -> file exists, returns data frame", {
    local_dir <- withr::local_tempdir()
    dir.create(file.path(local_dir, "_release"))

    df_test <- data.frame(x = 1:3, y = letters[1:3])

    qs_file <- file.path(
      local_dir,
      "_release",
      "pip_raw_inventory_20250202_INT.qs"
    )
    qs::qsave(df_test, qs_file)

    out <- dlw_get_release_folder(local_dir, "20250202_INT")
    expect_s3_class(out, "data.frame")
    expect_equal(out, df_test)
  })

  test_that("dlw_get_release_folder() -> file not data frame => error", {
    local_dir <- withr::local_tempdir()
    dir.create(file.path(local_dir, "_release"))

    a_list <- list(a = 1, b = 2)
    qs_file <- file.path(local_dir, "_release", "pip_raw_inventory_LABEL.qs")
    qs::qsave(a_list, qs_file)

    expect_error(
      dlw_get_release_folder(local_dir, "LABEL"),
      "did not contain a data frame"
    )
  })

  # dlw_get_release_list() tests ----

  test_that("dlw_get_release_list() -> file not found => error", {
    local_path <- file.path(tempdir(), "not_there.qs")
    expect_error(
      dlw_get_release_list(local_path, "NOPE"),
      "Master list file not found"
    )
  })

  test_that("dlw_get_release_list() -> label not in the file => error", {
    local_file <- withr::local_file("test_master_list.qs")
    my_list <- list(
      "REL_ABC" = list(
        timestamp = "2025-01-01 12:00:00",
        data = list(list(x = 1, y = 2))
      )
    )
    qs::qsave(my_list, local_file)

    expect_error(
      dlw_get_release_list(local_file, "REL_XYZ"),
      "Release label 'REL_XYZ' not found"
    )
  })

  test_that("dlw_get_release_list() -> label has no rows => returns empty df", {
    local_file <- withr::local_file("empty_release.qs")

    my_list <- list(
      "REL_EMPTY" = list(
        timestamp = "2025-02-02 10:00:00",
        data = list() # no rows
      )
    )
    qs::qsave(my_list, local_file)

    out <- dlw_get_release_list(local_file, "REL_EMPTY")
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 0)
  })

  test_that("dlw_get_release_list() -> label has some rows => returns correct df", {
    local_file <- withr::local_file("multirow_release.qs")

    # We'll store a row-based structure
    row_1 <- list(survey_id = "AAA", val = 123)
    row_2 <- list(survey_id = "BBB", val = 999)
    my_list <- list(
      "REL_NONEMPTY" = list(
        timestamp = "2025-02-03 09:00:00",
        data = list(row_1, row_2)
      )
    )
    qs::qsave(my_list, local_file)

    out <- dlw_get_release_list(local_file, "REL_NONEMPTY")
    expect_equal(nrow(out), 2)
    expect_true(all(c("survey_id", "val") %in% names(out)))

    # Check the content
    expect_equal(out$survey_id, c("AAA", "BBB"))
    expect_equal(out$val, c(123, 999))
  })
} # end if (FALSE)
