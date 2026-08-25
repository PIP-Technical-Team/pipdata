test_that("required public storage APIs retain exact-version contracts", {
  expect_named(formals(stamp::st_versions), c("path", "alias"))
  expect_named(formals(stamp::st_hash_obj), "x")
  expect_true("version" %in% names(formals(pipload::load_aux_data)))
  expect_true("version" %in% names(formals(pipload::pip_read)))
  expect_identical(names(formals(pd_process_data))[1:5],
                   c("inv", "aux_measures", "force", "verbose", "force_surveys"))
})
