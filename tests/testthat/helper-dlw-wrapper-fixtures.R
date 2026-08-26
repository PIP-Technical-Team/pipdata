# Load fixture declarations before testthat optionally shuffles test blocks.
.dlw_fixture_files <- c(
  "test-dependency-execution.R",
  "test-dlw-unified-logging.R",
  "test-log_report.R",
  "test-pd-change-report.R",
  "test-pd_process_data.R",
  "test-pipdata_dlw_compare.R",
  "test-pipdata_dlw_process.R",
  "test-pipdata_get_gmd.R",
  "test-pipdata_validate_gmd.R"
)

for (.dlw_fixture_file in .dlw_fixture_files) {
  .dlw_fixture_expressions <- parse(
    testthat::test_path(.dlw_fixture_file),
    keep.source = FALSE
  )
  for (.dlw_fixture_expression in .dlw_fixture_expressions) {
    if (is.call(.dlw_fixture_expression) &&
        identical(.dlw_fixture_expression[[1L]], as.name("<-"))) {
      eval(.dlw_fixture_expression, envir = environment())
    }
  }
}

rm(
  .dlw_fixture_file,
  .dlw_fixture_files,
  .dlw_fixture_expression,
  .dlw_fixture_expressions
)
