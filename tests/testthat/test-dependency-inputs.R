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

test_that("expected clean IDs use exact PFW keys and cache ID semantics", {
  inv_row <- data.table::data.table(
    survey_id = "COL_2020_GEIH_V01_M_V01_A_GMD_ALL",
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", module = "ALL"
  )
  pfw <- data.table::data.table(
    country_code = c("COL", "COL", "PER"),
    surveyid_year = c(2020L, 2020L, 2020L),
    survey_acronym = c("GEIH", "GEIH", "ENAHO"),
    welfare_type = c("consumption", "income", "income"),
    inpovcal = 1L,
    cpi_domain = 1L, ppp_domain = 1L, gdp_domain = 1L,
    pce_domain = 1L, pop_domain = 1L
  )

  expected <- c("COL_2020_GEIH_CON_ALL", "COL_2020_GEIH_INC_ALL")
  expect_identical(pd_expected_clean_pip_ids(inv_row, pfw), expected)
  expect_identical(pd_expected_clean_pip_ids(inv_row, pfw[3:1]), expected)

  missing_module <- data.table::copy(inv_row)[, module := NULL]
  expect_error(
    pd_expected_clean_pip_ids(missing_module, pfw),
    class = "pipdata_dependency_input_missing"
  )

  ambiguous <- data.table::rbindlist(list(pfw, pfw[2L]))
  expect_error(
    pd_expected_clean_pip_ids(inv_row, ambiguous),
    class = "pipdata_dependency_input_ambiguous"
  )
})

test_that("auxiliary projections use exact metadata keys and per-domain levels", {
  keys <- list(
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", module = "ALL", welfare_type = "income"
  )
  pfw_row <- data.table::data.table(
    cpi_domain = 1L, ppp_domain = 2L, pop_domain = 1L,
    gdp_domain = 2L, pce_domain = 1L,
    cpi_domain_var = "urban", ppp_domain_var = "urban"
  )
  cpi <- data.table::data.table(
    country_code = c("COL", "COL", "PER"), year = 2020L,
    survey_acronym = c("GEIH", "OTHER", "GEIH"),
    cpi_year = 2017L, reporting_level = "national",
    cpi_value = c(1, 99, 88)
  )
  ppp <- data.table::data.table(
    country_code = c("COL", "COL", "PER"), ppp_year = 2017L,
    release_version = "v01", adaptation_version = "v01",
    reporting_level = c("urban", "rural", "national"),
    ppp = c(2, 3, 99)
  )

  cpi_projection <- pd_aux_component_projection("cpi", cpi, keys, pfw_row)
  ppp_projection <- pd_aux_component_projection("ppp", ppp, keys, pfw_row)
  unrelated_change <- data.table::copy(cpi)
  unrelated_change[2L, cpi_value := 1000]
  matching_change <- data.table::copy(cpi)
  matching_change[1L, cpi_value := 2]

  expect_identical(cpi_projection$data_level, "national")
  expect_identical(ppp_projection$data_level, "area")
  expect_identical(unname(cpi_projection$value), 1)
  expect_identical(
    names(ppp_projection$value),
    c("ppp_2017_001_001_rural", "ppp_2017_001_001_urban")
  )
  expect_identical(
    pd_aux_component_projection(
      "cpi", unrelated_change, keys, pfw_row
    )$hash,
    cpi_projection$hash
  )
  expect_false(identical(
    pd_aux_component_projection("cpi", matching_change, keys, pfw_row)$hash,
    cpi_projection$hash
  ))

  duplicate <- data.table::rbindlist(list(cpi, cpi[1L]))
  expect_error(
    pd_aux_component_projection("cpi", duplicate, keys, pfw_row),
    class = "pipdata_dependency_input_ambiguous"
  )
})

test_that("planning and workers reject the same mixed-domain disagreement", {
  keys <- list(
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", module = "ALL", welfare_type = "income"
  )
  pfw_row <- data.table::data.table(
    reporting_level = 2L,
    cpi_domain = 1L, ppp_domain = 2L, pop_domain = 1L,
    gdp_domain = 1L, pce_domain = 1L,
    cpi_domain_var = "rural", ppp_domain_var = "urban"
  )
  ppp <- data.table::data.table(
    country_code = "COL", ppp_year = 2017L, release_version = "v01",
    adaptation_version = "v01", reporting_level = "urban", ppp = 2
  )

  planning <- rlang::catch_cnd(
    pd_aux_component_projection("ppp", ppp, keys, pfw_row)
  )
  worker <- rlang::catch_cnd(
    add_dom_vars(data.table::data.table(value = 1), pfw_row)
  )

  expect_s3_class(planning, "pipdata_dependency_domain_mismatch")
  expect_s3_class(worker, "pipdata_dependency_domain_mismatch")
})

test_that("named input canonical rows are deterministic and non-artifact tokens", {
  components <- data.table::data.table(
    name = c("pfw", "dlw"),
    version_id = c("pfw-v2", "dlw-v7"),
    content_hash = c("pfw-h2", "dlw-h7")
  )
  forward <- pd_build_input_rows("clean", "survey", components)
  reverse <- pd_build_input_rows("clean", "survey", components[2:1])

  expect_identical(forward, reverse)
  expect_identical(forward$name, c("canonical", "dlw", "pfw"))
  expect_true(nzchar(forward[name == "canonical", version_id]))
  expect_identical(
    forward[name == "canonical", content_hash],
    pd_hash_object(components[order(name), .(name, content_hash)])
  )
})

test_that("canonical rows separate source identity from semantic content", {
  first <- pd_build_input_rows(
    "metadata", "PIP_ID",
    data.table::data.table(
      name = c("clean_data", "aux_cpi"),
      version_id = c("data-v1", "cpi-catalog-v1"),
      content_hash = c("data-h1", "entity-cpi-h1")
    )
  )[name == "canonical"]
  next_catalog <- pd_build_input_rows(
    "metadata", "PIP_ID",
    data.table::data.table(
      name = c("clean_data", "aux_cpi"),
      version_id = c("data-v1", "cpi-catalog-v2"),
      content_hash = c("data-h1", "entity-cpi-h1")
    )
  )[name == "canonical"]

  expect_false(identical(first$version_id, next_catalog$version_id))
  expect_identical(first$content_hash, next_catalog$content_hash)
})

test_that("entity input state emits the frozen stage-specific named rows", {
  pfw <- data.table::data.table(
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", welfare_type = "income", inpovcal = 1L,
    cpi_domain = 1L, ppp_domain = 1L, pop_domain = 1L,
    gdp_domain = 1L, pce_domain = 1L
  )
  aux <- list(
    pfw = pfw,
    cpi = data.table::data.table(
      country_code = "COL", year = 2020L, survey_acronym = "GEIH",
      cpi_year = 2017L, reporting_level = "national", cpi_value = 1
    ),
    ppp = data.table::data.table(
      country_code = "COL", ppp_year = 2017L, release_version = "v01",
      adaptation_version = "v01", reporting_level = "national", ppp = 2
    ),
    pop = data.table::data.table(
      country_code = "COL", year = 2020L,
      reporting_level = "national", pop = 3
    ),
    gdp = data.table::data.table(
      country_code = "COL", year = 2020L,
      reporting_level = "national", gdp = 4
    ),
    pce = data.table::data.table(
      country_code = "COL", year = 2020L,
      reporting_level = "national", pce = 5
    )
  )
  measures <- names(aux)
  snapshot <- list(aux = list(
    catalog = data.table::data.table(
      measure = measures, version_id = paste0(measures, "-v1"),
      content_hash = paste0(measures, "-artifact-h1")
    ),
    objects = aux
  ))
  inv_row <- data.table::data.table(
    survey_id = "COL_2020_GEIH_V01_M_V01_A_GMD_ALL",
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", module = "ALL",
    latest_version_id = "dlw-v1", content_hash = "dlw-h1"
  )
  master_row <- data.table::data.table(
    survey_id = inv_row$survey_id, pip_id = "COL_2020_GEIH_INC_ALL",
    country_code = "COL", surveyid_year = 2020L,
    survey_acronym = "GEIH", module = "ALL", welfare_type = "income",
    version_id_data = "data-v1", content_hash_data = "data-h1",
    version_id_metadata = "meta-v1", content_hash_metadata = "meta-h1"
  )

  clean <- pd_entity_input_state(snapshot, inv_row, "clean", "pfw")
  metadata <- pd_entity_input_state(
    snapshot, master_row, "metadata", c("cpi", "ppp", "pop", "gdp", "pce")
  )
  metadata_subset <- pd_entity_input_state(
    snapshot, master_row, "metadata", "cpi"
  )
  deflate <- pd_entity_input_state(
    snapshot, master_row, "deflate", c("cpi", "ppp", "pop")
  )

  expect_identical(clean$input_rows$name, c("canonical", "dlw", "pfw"))
  expect_identical(
    clean$expected_pip_ids, "COL_2020_GEIH_INC_ALL"
  )
  expect_setequal(
    metadata$input_rows$name,
    c(
      "canonical", "clean_data", "aux_cpi", "aux_ppp", "aux_pop",
      "aux_gdp", "aux_pce"
    )
  )
  expect_setequal(
    deflate$input_rows$name,
    c(
      "canonical", "clean_data", "metadata", "aux_cpi", "aux_ppp",
      "aux_pop"
    )
  )
  expect_setequal(
    metadata_subset$input_rows$name,
    c("canonical", "clean_data", "aux_cpi")
  )
  expect_false(identical(metadata_subset$input_hash, metadata$input_hash))
})

test_that("legacy hashes reproduce the prior canonical algorithm by stage", {
  cpi <- data.table::data.table(
    country_code = c("COL", "COL"), year = c(2019L, 2020L),
    survey_acronym = "GEIH", reporting_level = "national",
    cpi_value = c(1, 2)
  )
  pfw <- data.table::data.table(
    country_code = "COL", survey_acronym = "GEIH",
    reporting_level = "national", welfare_type = "income"
  )
  snapshot <- list(aux = list(objects = list(pfw = pfw, cpi = cpi)))
  clean_row <- data.table::data.table(
    survey_id = "survey", country_code = "COL", surveyid_year = 2020L,
    year = 2019L, survey_acronym = "GEIH", module = "ALL",
    reporting_level = "national", latest_version_id = "dlw-v1",
    content_hash = "dlw-h1"
  )
  downstream_row <- data.table::data.table(
    survey_id = "survey", pip_id = "COL_2020_GEIH_INC_ALL",
    country_code = "COL", surveyid_year = 2020L, year = 2019L,
    survey_acronym = "GEIH", module = "ALL", welfare_type = "income",
    reporting_level = "national", version_id_data = "data-v1",
    content_hash_data = "data-h1", version_id_metadata = "meta-v1",
    content_hash_metadata = "meta-h1"
  )
  prior_aux_hash <- function(row, measure) {
    row <- as.list(row[1L])
    selected <- pd_select_aux(
      snapshot$aux$objects[[measure]], measure, row$country_code,
      row$year, row$survey_acronym, row$reporting_level
    )$hash
    stats::setNames(selected, measure)
  }
  expected <- c(
    clean = pd_hash_object(list(
      as.list(clean_row[1L]), prior_aux_hash(clean_row, "pfw")
    )),
    metadata = pd_hash_object(list(
      downstream_row$version_id_data,
      downstream_row$content_hash_data,
      prior_aux_hash(downstream_row, "cpi")
    )),
    deflate = pd_hash_object(list(
      downstream_row$version_id_data,
      downstream_row$content_hash_data,
      downstream_row$version_id_metadata,
      downstream_row$content_hash_metadata,
      prior_aux_hash(downstream_row, "cpi")
    ))
  )
  actual <- c(
    clean = pd_legacy_input_hash(snapshot, clean_row, "clean", "pfw"),
    metadata = pd_legacy_input_hash(
      snapshot, downstream_row, "metadata", "cpi"
    ),
    deflate = pd_legacy_input_hash(
      snapshot, downstream_row, "deflate", "cpi"
    )
  )

  expect_identical(actual, expected)
})

test_that("legacy canonical versions retain prior stage semantics", {
  row <- data.table::data.table(
    version_id_data = "data-v1", version_id_metadata = "meta-v1"
  )
  input_hash <- "legacy-input-h1"

  expect_identical(
    pd_legacy_input_version(row, "clean", input_hash), input_hash
  )
  expect_identical(
    pd_legacy_input_version(row, "metadata", input_hash), "data-v1"
  )
  expect_identical(
    pd_legacy_input_version(row, "deflate", input_hash),
    pd_hash_object(list("data-v1", "meta-v1"))
  )
})

test_that("clean worker rejects output-defined IDs before artifact writes", {
  writes <- 0L
  action <- data.table::data.table(
    stage = "clean", entity_id = "survey", survey_id = "survey",
    pip_id = NA_character_, action = "rebuild", input_hash = "input",
    code_hash = "code", expected_pip_ids = list("EXPECTED_ID")
  )
  execution <- list(
    snapshot = list(aux = list(objects = list(pfw = data.table::data.table()))),
    lease = list()
  )
  wrong <- stats::setNames(list(data.table::data.table(x = 1)), "WORKER_ID")

  testthat::local_mocked_bindings(
    inv_dlw_load = function(...) data.table::data.table(),
    pd_cpfw_merge = function(...) data.table::data.table(),
    pd_dlw_clean = function(...) wrong,
    pd_aux_attr = function(...) wrong,
    pd_assert_execution_fence = function(...) invisible(NULL),
    pd_save_receipt = function(...) {
      writes <<- writes + 1L
      list(success = TRUE)
    },
    .package = "pipdata"
  )

  result <- pd_execute_clean(
    action, data.table::data.table(survey_id = "survey"), execution,
    recode_spec = list()
  )

  expect_false(result$success)
  expect_identical(writes, 0L)
  expect_match(result$error, "accepted expected PIP IDs")
})
