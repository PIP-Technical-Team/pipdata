test_that("aux-only metadata replacement preserves base fields", {
  old <- list(country = "COL", year = 2020L, cpi = 1, gdp = 2)
  aux <- list(cpi = 3, gdp = 4)
  out <- pd_metadata_refresh(old, aux, "COL_2020")
  expect_identical(out$country, "COL")
  expect_identical(out$cpi, 3)
})

test_that("metadata-only worker reads only pinned compact metadata", {
  action <- data.table::data.table(
    pip_id = "P1", metadata_version_id = "m1", metadata_hash = "mh"
  )
  action[, aux_projection := list(list(list(cpi = 2)))]
  loaded_alias <- NULL
  testthat::local_mocked_bindings(
    load_pip_data = function(pip_id, version, alias, verbose) {
      loaded_alias <<- alias
      list(base = 1)
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_hash_obj = function(x) "mh",
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pd_assert_execution_fence = function(execution) invisible(execution),
    pd_save_receipt = function(...) list(success = TRUE),
    .package = "pipdata"
  )
  result <- pd_execute_metadata(action, list(), list(lease = NULL))
  expect_true(result$success)
  expect_identical(loaded_alias, "pip_meta")
})

test_that("metadata restart reconstructs from exact cleaned artifact", {
  action <- data.table::data.table(
    pip_id = "P1", data_version_id = "d2", data_hash = "dh2",
    input_hash = "ih", code_hash = "ch", reconstruct_base_metadata = TRUE
  )
  action[, aux_projection := list(list(list(cpi = 2)))]
  loaded <- list()
  testthat::local_mocked_bindings(
    load_pip_data = function(pip_id, version, alias, verbose) {
      loaded <<- list(pip_id = pip_id, version = version, alias = alias)
      structure(data.table::data.table(welfare = 1), base = "fresh")
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_hash_obj = function(x) "dh2",
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pd_aux_attr = function(clean_data, aux_list) {
      list(P1 = list(base = attr(clean_data$P1, "base")))
    },
    pd_assert_execution_fence = function(execution) invisible(execution),
    pd_save_receipt = function(...) list(success = TRUE),
    .package = "pipdata"
  )
  snapshot <- list(aux = list(objects = list()))
  result <- pd_execute_metadata(action, snapshot, list(lease = NULL))
  expect_identical(loaded, list(pip_id = "P1", version = "d2", alias = "pip"))
  expect_true(result$success)
  expect_identical(result$data_version_id, "d2")
  expect_identical(result$data_hash, "dh2")
})

test_that("metadata restart rejects a cleaned artifact hash mismatch", {
  action <- data.table::data.table(
    pip_id = "P1", data_version_id = "d2", data_hash = "expected",
    reconstruct_base_metadata = TRUE
  )
  testthat::local_mocked_bindings(
    load_pip_data = function(...) list(base = 1),
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_hash_obj = function(x) "different",
    .package = "stamp"
  )
  expect_error(
    pd_execute_metadata(
      action, list(aux = list(objects = list())), list(lease = NULL)
    ),
    class = "pipdata_metadata_base_invalid"
  )
})
