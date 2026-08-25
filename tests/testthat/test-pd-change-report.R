test_that("change report returns the shared plan without writes", {
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  output <- capture.output(plan <- pd_change_report(
    inv = data.table::data.table(survey_id = "s"),
    master = data.table::data.table(), manifest = manifest, context = context
  ))
  expect_match(paste(output, collapse = "\n"), "PIP dependency plan")
  expect_s3_class(plan, "pip_dependency_plan")
})
