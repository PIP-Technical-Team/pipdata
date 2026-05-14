# Helper: build a minimal validation_report data.table and inject into .pipdataenv
with_validation_report <- function(dt, code) {
  pd_env_set("validation_report", dt)
  on.exit(pd_env_rm("validation_report"), add = TRUE)
  force(code)
}

# ── get_data_status ───────────────────────────────────────────────────────────

test_that("get_data_status() returns a data.table with columns data_status and n", {
  vr <- data.table::data.table(
    table_name = c("BOL_1990_A", "BOL_1990_A", "CHL_2000_B"),
    type = c("error", "success", "success"),
    assertion.id = c("a1", "a2", "a3"),
    call = c("", "", ""),
    error_df = list(NULL, NULL, NULL)
  )
  with_validation_report(vr, {
    result <- get_data_status()
    expect_s3_class(result, "data.table")
    expect_named(result, c("data_status", "n"), ignore.order = FALSE)
  })
})

test_that("get_data_status() counts Valid and Invalid surveys correctly", {
  # BOL_1990_A has an error → "In valid"; CHL_2000_B has none → "Valid"
  vr <- data.table::data.table(
    table_name = c("BOL_1990_A", "BOL_1990_A", "CHL_2000_B"),
    type = c("error", "success", "success"),
    assertion.id = c("a1", "a2", "a3"),
    call = c("", "", ""),
    error_df = list(NULL, NULL, NULL)
  )
  with_validation_report(vr, {
    result <- get_data_status()
    expect_equal(result[data_status == "Valid", n], 1L)
    expect_equal(result[data_status == "In valid", n], 1L)
  })
})

test_that("get_data_status() aborts when validation_report is not in .pipdataenv", {
  # Ensure validation_report is absent
  if (!is.null(pd_env_get("validation_report"))) {
    pd_env_rm("validation_report")
  }
  expect_error(get_data_status(), class = "rlang_error")
})
