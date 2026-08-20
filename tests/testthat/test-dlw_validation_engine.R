library(withr)

# Shared synthetic data generators + deterministic per-module builder
# (RNG protocol aligned with generate-fixtures.R via dlw_fixture_data()).

cleanup_env <- function() {
  pd_env_rm("validation_report")
}

data_generators <- list(
  gpwg = "gpwg",
  group = "group",
  bin = "bin",
  hist = "hist",
  all = "all",
  aspire = "aspire",
  l = "l"
)

test_that("engine returns 3-col err_t for all modules", {
  withr::defer(cleanup_env())

  modules <- c("gpwg", "group", "bin", "hist", "all", "aspire", "l", "skip")

  for (mod in modules) {
    cleanup_env()
    dlw <- dlw_fixture_data(mod)
    result <- dlw_validation_engine(dlw, paste0("test_", mod), mod)

    expect_s3_class(result, "data.table")
    expect_true(all(c("table_name", "message", "type") %in% names(result)))

    record <- pd_env_get("validation_report")
    expect_s3_class(record, "data.table")
    expect_true(nrow(record) > 0)
  }
})

test_that("skip blank data yields type == error", {
  withr::defer(cleanup_env())
  dlw <- make_empty_data()
  result <- dlw_validation_engine(dlw, "test_skip_empty", "skip")
  expect_true(any(result$type == "error"))
})

test_that("skip non-blank data yields no error", {
  withr::defer(cleanup_env())
  dlw <- make_skip_data()
  result <- dlw_validation_engine(dlw, "test_skip_ok", "skip")
  expect_false(any(result$type == "error"))
})

fixtures_path <- function(name) {
  file.path(testthat::test_path(), paste0("fixtures/", name, ".rds"))
}

compare_to_fixture <- function(engine_record, fixture_path) {
  expect_true(file.exists(fixture_path))
  fixture <- readRDS(fixture_path)

  expect_equal(nrow(engine_record), nrow(fixture))

  em <- vapply(engine_record$message, function(x) paste(as.character(x), collapse = "|"), "")
  fm <- vapply(fixture$message, function(x) paste(as.character(x), collapse = "|"), "")
  expect_equal(sort(em), sort(fm))

  ed <- vapply(engine_record$description, function(x) paste(as.character(x), collapse = "|"), "")
  fd <- vapply(fixture$description, function(x) paste(as.character(x), collapse = "|"), "")
  expect_equal(sort(ed), sort(fd))

  expect_equal(sort(engine_record$type), sort(fixture$type))

  en <- vapply(engine_record$num.violations, function(x) paste(as.character(x), collapse = "|"), "")
  fn <- vapply(fixture$num.violations, function(x) paste(as.character(x), collapse = "|"), "")
  expect_equal(sort(en), sort(fn))

  expect_equal(sort(engine_record$table_name), sort(fixture$table_name))
}

test_that("engine matches committed fixtures for all modules", {
  withr::defer(cleanup_env())

  for (mod in names(data_generators)) {
    dlw <- dlw_fixture_data(mod)
    cleanup_env()
    dlw_validation_engine(dlw, paste0("fixture_", mod), mod)
    compare_to_fixture(pd_env_get("validation_report"), fixtures_path(paste0("validation_", mod)))

    dlw_err <- dlw_fixture_data(mod)
    dlw_err[[dlw_error_column(mod)]] <- NULL
    cleanup_env()
    dlw_validation_engine(dlw_err, paste0("fixture_error_", mod), mod)
    eng_err <- pd_env_get("validation_report")
    expect_true(any(eng_err$type == "error"))
    compare_to_fixture(eng_err, fixtures_path(paste0("validation_error_", mod)))
  }
})

test_that("engine matches committed skip fixtures", {
  withr::defer(cleanup_env())

  cleanup_env()
  dlw_validation_engine(make_skip_data(), "fixture_skip", "skip")
  compare_to_fixture(pd_env_get("validation_report"), fixtures_path("validation_skip"))

  cleanup_env()
  dlw_validation_engine(make_empty_data(), "fixture_skip_blank", "skip")
  skip_blank <- pd_env_get("validation_report")
  expect_true(any(skip_blank$type == "error"))
  compare_to_fixture(skip_blank, fixtures_path("validation_skip_blank"))
})

test_that("engine is data-driven: iterates all modules in spec", {
  withr::defer(cleanup_env())
  spec <- dlw_validation_spec()
  spec_modules <- names(spec$modules)

  expect_true(all(c("gpwg", "group", "bin", "hist", "all", "aspire", "l", "skip") %in% spec_modules))
  expect_true(all(spec_modules %in% c("gpwg", "group", "bin", "hist", "all", "aspire", "l", "skip")))
})

test_that("unknown module falls back to skip", {
  withr::defer(cleanup_env())
  dlw <- make_skip_data()
  result <- dlw_validation_engine(dlw, "test_unknown", "nonexistent_module")
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 1)
})

test_that("report format is compatible with get_validation_report/get_data_status", {
  withr::defer(cleanup_env())
  dlw <- dlw_fixture_data("gpwg")
  nm <- "ARG_2018_EPHC-S2_INC_ALL_M_2020_01_A_2019_02_GPWG"
  dlw_validation_engine(dlw, nm, "gpwg")

  report <- get_validation_report()
  expect_true(all(c("module_type", "vermast", "veralt", "country_code", "rf_year") %in% names(report)))
  expect_true(all(report$module_type == "GPWG"))

  status <- get_data_status()
  expect_true(all(c("n", "data_status") %in% names(status)))
})

test_that("engine emits no per-survey log_info call", {
  withr::defer(cleanup_env())
  dlw <- make_gpwg_data()
  body_text <- paste(deparse(body(dlw_validation_engine)), collapse = "\n")
  expect_false(grepl("log_info", body_text))
  expect_false(grepl("log_add", body_text))
})

test_that("duplicated keys yield a warning from uniqueness check", {
  withr::defer(cleanup_env())
  dlw <- make_gpwg_data()
  dlw$hhid[1] <- dlw$hhid[2]
  dlw$pid[1] <- dlw$pid[2]
  result <- dlw_validation_engine(dlw, "test_dup", "gpwg")
  expect_true(any(grepl("is_uniq", result$message)))
  expect_true(any(result$type == "warning"))
})

test_that("NA threshold above 10 percent still emits warning rows (not silent)", {
  withr::defer(cleanup_env())
  dlw <- make_gpwg_data()
  dlw$weight[1:20] <- NA_real_
  dlw$welfare[1:20] <- NA_real_
  result <- dlw_validation_engine(dlw, "test_na_critical", "gpwg")
  expect_true(any(result$type == "warning"))
  expect_true(any(grepl("not_na", result$message)))
})

test_that("NA threshold within 10 percent stays warning for warning checks", {
  withr::defer(cleanup_env())
  dlw <- make_gpwg_data()
  dlw$year[1:5] <- NA_real_
  result <- dlw_validation_engine(dlw, "test_na_warning", "gpwg")
  expect_false(any(result$type == "error"))
})

test_that("wrong-typed column triggers a warning failure", {
  withr::defer(cleanup_env())
  dlw <- make_gpwg_data()
  dlw$year <- as.character(1:100)
  result <- dlw_validation_engine(dlw, "test_type", "gpwg")
  expect_true(any(result$type == "warning"))
  expect_false(any(result$type == "error"))
})

test_that("optional-column-omitted skips cleanly", {
  withr::defer(cleanup_env())
  dlw <- make_group_data()
  dlw[["urban"]] <- NULL
  result <- dlw_validation_engine(dlw, "test_omit_optional", "group")
  expect_false(any(grepl("urban", result$message)))
})

test_that("age above 110 triggers secondary_check failure", {
  withr::defer(cleanup_env())
  dlw <- make_all_data()
  dlw$age[1:5] <- 115
  result <- dlw_validation_engine(dlw, "test_age_high", "all")
  record <- pd_env_get("validation_report")
  expect_true(any(result$type == "warning"))
  descs <- vapply(record$description, function(x) paste(as.character(x), collapse = "|"), "")
  expect_true(any(grepl("age", descs)))
})

test_that("value outside valid_values triggers value_constraint warning", {
  withr::defer(cleanup_env())
  dlw <- make_group_data()
  dlw$welfare_type[1:5] <- "bogus"
  result <- dlw_validation_engine(dlw, "test_vc", "group")
  record <- pd_env_get("validation_report")
  expect_true(any(result$type == "warning"))
  descs <- vapply(record$description, function(x) paste(as.character(x), collapse = "|"), "")
  expect_true(any(grepl("welfare_type", descs)))
})

test_that("character-loop wrong type triggers warning", {
  withr::defer(cleanup_env())
  dlw <- make_group_data()
  dlw$code <- as.integer(1:50)
  result <- dlw_validation_engine(dlw, "test_chr_type", "group")
  expect_true(any(result$type == "warning"))
})