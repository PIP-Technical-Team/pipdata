test_that("dependency contracts reject invalid keys and stages", {
  context <- list(scope_id = "x")
  manifest <- pd_empty_manifest(context)
  manifest$records <- data.table::data.table(
    stage = "bad", entity_id = "x", output_version_id = "v",
    output_hash = "h", input_hash = "i", code_hash = "c"
  )
  expect_error(pd_validate_manifest(manifest), class = "pipdata_dependency_manifest_invalid")
})

test_that("scope includes release identity roots and namespace", {
  local_mocked_bindings(
    st_alias_list = function() data.frame(alias = character(), root = character(),
                                          state_dir = character(), stamp_path = character()),
    .package = "stamp"
  )
  a <- pd_dependency_context("r1", "TEST", "C:/repo", "a")
  b <- pd_dependency_context("r2", "TEST", "C:/repo", "a")
  c <- pd_dependency_context("r1", "PROD", "C:/repo", "a")
  expect_length(unique(c(a$scope_id, b$scope_id, c$scope_id)), 3L)
})
