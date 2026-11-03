# Offline integration only: real validators/producers/workers/storage, transport
# substituted. This does not certify live DLW, supplemental producers, or M2.

test_that("V1: real selected GPWG validation associates severity and gates missing weight", {
  isolation <- upstream_acceptance_fixture()
  evidence <- new.env(parent = emptyenv())
  evidence$isolation <- isolation
  withr::defer(upstream_acceptance_evidence("V1-validation-gate", evidence))
  id <- upstream_acceptance_identity()
  acquired <- upstream_acceptance_acquire()
  evidence$acquisition <- acquired
  expect_true(acquired$result$eligible_for_clean)
  expect_identical(acquired$result$selection$survey_id, id$survey_id)
  expect_identical(acquired$result$selection$pip_id, id$pip_id)
  inv <- acquired$result$validation$inventory
  expect_identical(inv$survey_id, id$survey_id)
  expect_identical(inv$status, "valid")
  expect_identical(inv$latest_version_id, acquired$raw_pin$version_id)
  expect_identical(inv$content_hash, acquired$raw_pin$metadata$content_hash)
  report <- pipload::load_gmd_valid_report(verbose = FALSE)
  evidence$valid_report <- report
  expect_true(nrow(report) > 0L)
  expect_true(all(report$table_name == id$survey_id))
  # Match description AND severity on the same record, never sorted multisets.
  expect_true(any(report$description == "weight variable should be in the data" &
                    report$type == "success"))
  expect_true(any(report$description == "countrycode is character" &
                    report$type == "success"))
  expect_false(any(report$type == "error"))
  expect_identical(nrow(.filter_completed_dlw_validation_inventory(inv)), 1L)

  bad <- upstream_acceptance_raw()
  bad[, weight := NULL]
  evidence$invalid_raw_pin <- pipload::pip_write(
    bad, id = tolower(id$survey_id), alias = "dlw", verbose = FALSE
  )
  candidate <- data.table::copy(acquired$catalog)
  candidate[, data_available := "Yes"]
  pd_env_rm("validation_report")
  rejected <- .validate_one_gmd(candidate, next_pipeline_version = 2L, verbose = FALSE)
  evidence$invalid_validation <- rejected
  expect_identical(rejected$survey_id, id$survey_id)
  expect_identical(rejected$status, "invalid")
  expect_identical(rejected$inventory_row$status, "invalid")
  expect_true(all(rejected$report_rows$table_name == id$survey_id))
  expect_true(any(rejected$report_rows$description ==
                    "weight variable should be in the data" &
                    rejected$report_rows$type == "error"))
  expect_identical(nrow(.filter_completed_dlw_validation_inventory(
    rejected$inventory_row)), 0L)

  counts <- upstream_acceptance_trace(
    c("inv_dlw_load", "pd_dlw_clean", "pd_aux_attr", "pd_save_receipt")
  )
  before <- lapply(c("pip", "pip_meta"), function(a) stamp::st_catalog_query(alias = a))
  action <- data.table::data.table(survey_id = id$survey_id)
  # The real eligibility guard must return before it needs execution or recode.
  result <- pd_execute_clean(action, rejected$inventory_row, execution = NULL,
                             recode_spec = NULL, verbose = FALSE)
  evidence$invalid_clean <- result
  evidence$worker_calls <- as.list(counts)
  expect_false(result$success)
  expect_identical(result$condition$code, "pipdata_clean_validation_error")
  expect_identical(unname(unlist(as.list(counts))), rep(0L, 4L))
  after <- lapply(c("pip", "pip_meta"), function(a) stamp::st_catalog_query(alias = a))
  evidence$output_catalogs <- list(before = before, after = after)
  expect_equal(after, before)
})

test_that("V2: actual CPI PPP PFW producers persist literal keys values and consumer pins", {
  isolation <- upstream_acceptance_fixture()
  evidence <- new.env(parent = emptyenv())
  evidence$isolation <- isolation
  withr::defer(upstream_acceptance_evidence("V2-producer-consumer", evidence))
  aux <- upstream_acceptance_produce()
  evidence$aux <- aux
  expect_identical(aux$transport_calls, c("cpi", "ppp", "pfw"))
  country <- pipload::load_aux_data("country_list", verbose = FALSE)
  expect_identical(country$country_code, "AAA")
  expect_identical(country$region_code, "SSA")
  for (measure in c("country_list", "pop", "gdp", "pce")) {
    fixture <- pipload::load_aux_data(
      measure, version = aux$fixture_pins[[measure]]$version_id, verbose = FALSE
    )
    expect_identical(fixture$country_code, "AAA")
    if (measure != "country_list") {
      expect_identical(fixture$year, 2010L)
      expect_identical(fixture$reporting_level, "national")
      expect_identical(fixture[[measure]],
                        switch(measure, pop = 1000, gdp = 3500, pce = 2400))
      expect_identical(stamp::st_hash_obj(fixture),
                        stamp::st_hash_obj(aux$snapshot$objects[[measure]]))
    }
  }
  keys <- list(
    cpi = c("country_code", "year", "survey_acronym", "reporting_level", "cpi_year"),
    ppp = c("country_code", "reporting_level", "ppp_year", "release_version", "adaptation_version"),
    pfw = c("country_code", "surveyid_year", "welfare_type", "is_alt_welf")
  )
  for (measure in names(keys)) {
    actual <- aux$snapshot$objects[[measure]]
    pin <- aux$producer_pins[[measure]]
    target_measure <- measure
    row <- aux$snapshot$catalog[aux$snapshot$catalog$measure == target_measure]
    expect_identical(nrow(row), 1L)
    sidecar <- stamp::st_read_sidecar(paste0(measure, ".qs2"), alias = "aux")
    expect_s3_class(actual, "data.table")
    expect_identical(stamp::st_get_pk(actual), keys[[measure]])
    expect_identical(data.table::key(actual), keys[[measure]])
    expect_identical(row$version_id, pin$version_id)
    expect_identical(row$content_hash, stamp::st_hash_obj(actual))
    exact <- pipload::load_aux_data(
      measure, version = pin$version_id, ppp_defaults = FALSE, verbose = FALSE
    )
    expect_identical(stamp::st_hash_obj(exact), row$content_hash)
    expect_identical(sidecar$gh$gh_raw_sha, paste0("synthetic-", measure, "-v1"))
    expect_identical(sidecar$code_label, paste0("aux_", measure, "_update"))
    producer <- get(paste0("aux_", measure, "_update"), asNamespace("pipaux"))
    expect_identical(sidecar$code_hash, stamp:::st_hash_code(producer))
  }
  cpi <- aux$snapshot$objects$cpi
  expect_identical(cpi$country_code, rep("AAA", 4L))
  expect_identical(cpi$year, rep(2010L, 4L))
  expect_identical(cpi$survey_acronym, rep("SYNA", 4L))
  expect_identical(cpi$reporting_level, rep("national", 4L))
  expect_identical(cpi$cpi_year, c(2005L, 2011L, 2017L, 2021L))
  expect_identical(cpi$cpi_value, c(1, 1, 1.2, 1.4))
  expect_identical(cpi$survey_year, rep(2010, 4L))
  expect_identical(cpi$ccf, rep(1, 4L))
  ppp <- aux$snapshot$objects$ppp
  expect_identical(ppp$country_code, rep("AAA", 9L))
  expect_identical(ppp$reporting_level, rep("national", 9L))
  expect_identical(ppp$ppp_year, c(2005, 2011, 2011, 2011, 2011, 2017, 2017, 2021, 2021))
  expect_identical(ppp$release_version, c("v1", "v1", "v1", "v2", "v2", "v1", "v1", "v1", "v1"))
  expect_identical(ppp$adaptation_version, c("v1", "v1", "v2", "v1", "v2", "v1", "v2", "v1", "v2"))
  expect_identical(ppp$ppp, c(1, 1.1, 1.2, 1.3, 1.35, 1.4, 1.5, 1.6, 1.7))
  expect_identical(ppp$ppp_default, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(ppp$ppp_default_by_year, c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
  pfw <- aux$snapshot$objects$pfw
  expect_identical(nrow(pfw), 1L)
  expect_identical(pfw$country_code, "AAA")
  expect_identical(pfw$surveyid_year, 2010L)
  expect_identical(pfw$survey_year, 2010)
  expect_identical(pfw$survey_acronym, "SYNA")
  expect_identical(pfw$welfare_type, "income")
  expect_identical(pfw$is_alt_welf, FALSE)
  expect_identical(pfw$survey_coverage, "national")
  expect_identical(unlist(pfw[, c("cpi_domain", "ppp_domain", "pop_domain",
                                  "gdp_domain", "pce_domain"), with = FALSE],
                         use.names = FALSE), rep(1, 5L))
})

test_that("V3: exact validated raw pin survives a newer decoy and real clean metadata readback", {
  isolation <- upstream_acceptance_fixture()
  evidence <- new.env(parent = emptyenv())
  evidence$isolation <- isolation
  withr::defer(upstream_acceptance_evidence("V3-clean-readback", evidence))
  aux <- upstream_acceptance_produce()
  acquired <- upstream_acceptance_acquire()
  evidence$aux <- aux
  evidence$acquisition <- acquired
  id <- upstream_acceptance_identity()
  inv <- acquired$result$validation$inventory
  expect_true(acquired$result$eligible_for_clean)
  expect_identical(inv$latest_version_id, acquired$raw_pin$version_id)
  decoy <- upstream_acceptance_raw()
  decoy[, welfare := c(3650, 7300, 10950, 14600)]
  evidence$decoy_pin <- pipload::pip_write(
    decoy, id = tolower(id$survey_id), alias = "dlw", verbose = FALSE
  )
  expect_false(identical(evidence$decoy_pin$version_id, acquired$raw_pin$version_id))
  reads <- upstream_acceptance_trace_raw_reads()
  consumed <- inv_dlw_load(inv)
  evidence$consumed_raw <- consumed
  evidence$consumer_raw_reads <- reads$calls
  expect_identical(reads$calls[[1L]]$id_name, id$survey_id)
  expect_identical(reads$calls[[1L]]$version, acquired$raw_pin$version_id)
  expect_identical(consumed$welfare, c(365, 730, 1095, 1460))
  expect_identical(consumed$weight, c(10, 20, 30, 40))
  exact_raw <- pipload::load_dlw_data(id_name = id$survey_id,
                                     version = inv$latest_version_id, verbose = FALSE)
  expect_identical(stamp::st_hash_obj(exact_raw), acquired$raw_pin$metadata$content_hash)
  counts <- upstream_acceptance_trace(c("pd_dlw_clean", "pd_aux_attr"))
  x <- upstream_acceptance_execution(aux$snapshot)
  evidence$recode_pin <- x$recode$version_id
  clean <- pd_execute_clean(x$action, inv, x$execution, x$recode, verbose = FALSE)
  evidence$clean_result <- clean
  evidence$consumer_raw_reads <- reads$calls
  expect_identical(reads$calls[[length(reads$calls)]]$version,
                    acquired$raw_pin$version_id)
  evidence$worker_calls <- as.list(counts)
  expect_identical(counts$pd_dlw_clean, 1L)
  expect_identical(counts$pd_aux_attr, 1L)
  expect_true(clean$success)
  expect_identical(clean$expected_pip_ids, id$pip_id)
  expect_identical(nrow(clean$receipts), 1L)
  receipt <- as.list(clean$receipts[1L])
  expect_no_error(pd_revalidate_receipt(receipt))
  saved <- pipload::pip_read(id$pip_id, version = receipt$version_id,
                             alias = "pip", verbose = FALSE)
  evidence$clean_readback <- saved
  evidence$oracle <- data.table::data.table(
    hhid = c("h1", "h2", "h3", "h4"), pid = c("p1", "p2", "p3", "p4"),
    welfare = c(1, 2, 3, 4), weight = c(10, 20, 30, 40),
    area = c("urban", "rural", "urban", "rural")
  )
  expect_identical(stamp::st_hash_obj(saved), receipt$content_hash)
  expect_s3_class(saved, "pipmd")
  expect_s3_class(saved, "data.table")
  # Preserve row correspondence: do not sort welfare or weights independently.
  expect_identical(saved$hhid, c("h1", "h2", "h3", "h4"))
  expect_identical(saved$pid, c("p1", "p2", "p3", "p4"))
  expect_identical(saved$welfare, c(1, 2, 3, 4))
  expect_identical(saved$weight, c(10, 20, 30, 40))
  expect_identical(as.character(saved$area), c("urban", "rural", "urban", "rural"))
  expect_identical(as.character(saved$gender), c("male", "female", "male", "female"))
  expect_identical(saved$age, c(25, 35, 45, 55))
  expect_identical(attr(saved, "survey_id"), id$survey_id)
  expect_identical(attr(saved, "country_code"), "AAA")
  expect_identical(attr(saved, "survey_acronym"), "SYNA")
  expect_equal(attr(saved, "surveyid_year"), 2010)
  expect_identical(attr(saved, "welfare_type"), "income")
  expect_identical(attr(saved, "reporting_level"), "1")
  expect_identical(attr(saved, "distribution_type"), "micro")
  expect_identical(attr(saved, "aux_data_levels"), "same")
  for (measure in c("cpi", "ppp", "pop", "gdp", "pce")) {
    expect_identical(attr(saved, paste0(measure, "_data_level")), "national")
  }
  metadata <- clean$metadata[[id$pip_id]]
  evidence$metadata_receipt <- pd_save_receipt(
    metadata, id$pip_id, "pip_meta", verbose = FALSE, lease = x$execution$lease
  )
  expect_true(evidence$metadata_receipt$success)
  expect_no_error(pd_revalidate_receipt(evidence$metadata_receipt))
  meta <- pipload::pip_read(id$pip_id, version = evidence$metadata_receipt$version_id,
                            alias = "pip_meta", verbose = FALSE)
  evidence$metadata_readback <- meta
  expect_identical(stamp::st_hash_obj(meta), evidence$metadata_receipt$content_hash)
  expect_identical(meta$survey_id, id$survey_id)
  expect_identical(meta$country_code, "AAA")
  expect_identical(meta$survey_acronym, "SYNA")
  expect_identical(meta$welfare_type, "income")
  expect_identical(meta$distribution_type, "micro")
  expect_identical(meta$reporting_level, "1")
  expect_identical(meta$cpi, c(`2005_national` = 1, `2011_national` = 1,
                              `2017_national` = 1.2, `2021_national` = 1.4))
  expect_identical(meta$ppp, c(
    ppp_2005_01_01_national = 1, ppp_2011_01_01_national = 1.1,
    ppp_2011_01_02_national = 1.2, ppp_2011_02_01_national = 1.3,
    ppp_2011_02_02_national = 1.35, ppp_2017_01_01_national = 1.4,
    ppp_2017_01_02_national = 1.5, ppp_2021_01_01_national = 1.6,
    ppp_2021_01_02_national = 1.7
  ))
  expect_identical(meta$pop, c(`2010_national` = 1000))
  expect_identical(meta$gdp, c(`2010_national` = 3500))
  expect_identical(meta$pce, c(`2010_national` = 2400))
})

test_that("V4: missing or ambiguous required aux rows reject before any downstream save", {
  isolation <- upstream_acceptance_fixture()
  evidence <- new.env(parent = emptyenv())
  evidence$isolation <- isolation
  withr::defer(upstream_acceptance_evidence("V4-aux-negative-controls", evidence))
  aux <- upstream_acceptance_produce()
  acquired <- upstream_acceptance_acquire()
  evidence$aux <- aux
  evidence$acquisition <- acquired
  inv <- acquired$result$validation$inventory
  expect_true(acquired$result$eligible_for_clean)
  counts <- upstream_acceptance_trace("pd_save_receipt")
  x <- upstream_acceptance_execution(aux$snapshot)
  controls <- list()
  for (measure in c("pfw", "cpi", "ppp")) {
    for (mode in c("missing", "ambiguous")) {
      selected <- paste(measure, mode, sep = "-")
      execution <- x$execution
      objects <- lapply(aux$snapshot$objects, data.table::copy)
      object <- objects[[measure]]
      # Mutate only the input to this negative control, not the producer store.
      objects[[measure]] <- if (mode == "missing") object[0L] else
        object[c(seq_len(nrow(object)), 1L)]
      execution$snapshot$aux$objects <- objects
      before <- lapply(c("pip", "pip_meta"), function(a) stamp::st_catalog_query(alias = a))
      counts$pd_save_receipt <- 0L
      result <- tryCatch(
        pd_execute_clean(x$action, inv, execution, x$recode, verbose = FALSE),
        error = identity
      )
      after <- lapply(c("pip", "pip_meta"), function(a) stamp::st_catalog_query(alias = a))
      controls[[selected]] <- list(result = result, saves = counts$pd_save_receipt,
                                    before = before, after = after)
      evidence$controls <- controls
      expect_true(inherits(result, "condition") || identical(result$success, FALSE),
                  info = selected)
      expected_code <- if (measure == "pfw") {
        if (mode == "missing") "info_pfw" else "no_unq_pfw"
      } else if (mode == "missing") {
        "pipdata_dependency_input_missing"
      } else {
        "pipdata_dependency_input_ambiguous"
      }
      if (inherits(result, "condition")) {
        expect_true(inherits(result, expected_code), info = selected)
      } else {
        expect_identical(result$condition$code, expected_code, info = selected)
      }
      expect_identical(counts$pd_save_receipt, 0L, info = selected)
      expect_equal(after, before, info = selected)
    }
  }
})
