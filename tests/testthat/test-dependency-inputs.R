test_that("canonical projections are order stable and reject duplicate keys", {
  x <- data.table::data.table(id = c(2L, 1L), value = c(3, NA))
  expect_identical(pd_canonical_projection(x, "id")$hash,
                   pd_canonical_projection(x[2:1], "id")$hash)
  expect_error(pd_canonical_projection(x[c(1, 1)], "id"),
               class = "pipdata_dependency_input_duplicate")
})

test_that("expected output IDs reject zero-output mappings", {
  expect_error(expected_pip_ids(list(), data.table::data.table()),
               class = "pipdata_pfw_mapping_error")
})

test_that("reporting level selector uses the requested value", {
  aux <- data.table::data.table(
    country_code = "COL", year = 2020L,
    reporting_level = c("national", "urban"), value = 1:2
  )
  selected <- pd_select_aux(aux, "gdp", "COL", 2020L,
                            reporting_level = "urban")$data
  expect_identical(selected$reporting_level, "urban")
})

test_that("metadata keys are canonical and duplicate-normalized keys fail", {
  expect_named(pd_normalize_metadata_keys(list(CPI = 1, GDP = 2)),
               c("cpi", "gdp"))
  expect_error(pd_normalize_metadata_keys(list(CPI = 1, cpi = 2)),
               class = "pipdata_metadata_base_invalid")
})
