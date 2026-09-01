test_that("aux-only metadata replacement preserves base fields", {
  old <- list(
    country = "COL", year = 2020L,
    cpi = stats::setNames(1, "2017_national"),
    ppp = stats::setNames(2, "ppp_2017_v01_v01_national"),
    pop = stats::setNames(3, "2020_national"), gdp = 2
  )
  aux <- list(cpi = stats::setNames(3, "2017_national"), gdp = 4)
  out <- pd_metadata_refresh(old, aux, "COL_2020")
  expect_identical(out$country, "COL")
  expect_identical(out$cpi, stats::setNames(3, "2017_national"))
})

test_that("metadata-only worker reads only pinned compact metadata", {
  action <- data.table::data.table(
    pip_id = "P1", metadata_version_id = "m1", metadata_hash = "mh",
    data_version_id = "d1", data_hash = "dh", input_hash = "ih",
    code_hash = "ch"
  )
  action[, aux_projection := list(list(list(
    cpi = stats::setNames(2, "2017_national")
  )))]
  loaded_alias <- NULL
  testthat::local_mocked_bindings(
    load_pip_data = function(pip_id, version, alias, verbose) {
      loaded_alias <<- alias
      list(
        cpi = stats::setNames(1, "2017_national"),
        ppp = stats::setNames(2, "ppp_2017_v01_v01_national"),
        pop = stats::setNames(3, "2020_national")
      )
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
  action[, aux_projection := list(list(list(
    cpi = stats::setNames(2, "2017_national")
  )))]
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
      list(P1 = list(
        base = attr(clean_data$P1, "base"),
        cpi = stats::setNames(1, "2017_national"),
        ppp = stats::setNames(2, "ppp_2017_v01_v01_national"),
        pop = stats::setNames(3, "2020_national")
      ))
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

test_that("legacy canonical migration reconstructs exact clean and frozen aux", {
  action <- data.table::data.table(
    stage = "metadata", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    action = "refresh", data_version_id = "clean-v2", data_hash = "clean-h2",
    metadata_version_id = "meta-v1", metadata_hash = "meta-h1",
    input_hash = "metadata-input", code_hash = "metadata-code"
  )
  action[, aux_projection := list(list(
    cpi = stats::setNames(2, "2017_national")
  ))]
  clean_receipt <- list(
    alias = "pip", artifact = "P1", path = "p1.qs2",
    version_id = "clean-v2", content_hash = "clean-h2"
  )
  execution <- list(
    plan = list(
      actions = action,
      reasons = data.table::data.table(
        stage = "metadata", entity_id = "P1",
        reason = "legacy_input_changed", input = "canonical",
        old = "clean-v1:legacy-h1", new = "clean-v2:legacy-h2"
      )
    ),
    manifest = list(records = data.table::data.table(
      stage = "clean", entity_id = "S1",
      output_receipts = list(list(clean_receipt))
    )),
    manifest_identity = NULL,
    snapshot = list(
      metadata_measures = "cpi",
      aux = list(objects = list(cpi = structure(list(), frozen = "aux-v1")))
    ),
    lease = list()
  )
  loaded <- NULL
  frozen <- NULL
  saved <- NULL
  testthat::local_mocked_bindings(
    load_pip_data = function(pip_id, version, alias, verbose) {
      loaded <<- list(pip_id = pip_id, version = version, alias = alias)
      if (identical(alias, "pip_meta")) {
        return(list(
          source = "stale-metadata",
          cpi = stats::setNames(1, "2017_national")
        ))
      }
      structure(data.table::data.table(welfare = 1), source = "clean-v2")
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_hash_obj = function(x) {
      if (is.list(x) && identical(x$source, "stale-metadata")) {
        "meta-h1"
      } else {
        "clean-h2"
      }
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pd_aux_attr = function(clean_data, aux_list) {
      frozen <<- attr(aux_list$cpi, "frozen")
      list(P1 = list(
        source = attr(clean_data$P1, "source"),
        cpi = stats::setNames(1, "2017_national")
      ))
    },
    pd_assert_execution_fence = function(execution, ...) invisible(execution),
    pd_save_receipt = function(x, ...) {
      saved <<- x
      list(
        success = TRUE, alias = "pip_meta", artifact = "P1",
        path = "p1.qs2", version_id = "meta-v2", content_hash = "meta-h2"
      )
    },
    pd_finalize_checkpoint = function(execution, master, ...) {
      list(candidate = master, execution = execution)
    },
    .package = "pipdata"
  )

  result <- pd_run_metadata_stage_prepared(
    execution, action, "run", list(run_id = "run"),
    data.table::data.table(
      survey_id = "S1", pip_id = "P1",
      version_id_data = "clean-v2", content_hash_data = "clean-h2"
    ),
    pd_pipeline_options(checkpoint_seconds = Inf), verbose = FALSE
  )

  expect_identical(loaded, list(
    pip_id = "P1", version = "clean-v2", alias = "pip"
  ))
  expect_identical(frozen, "aux-v1")
  expect_identical(saved$source, "clean-v2")
  expect_identical(result$outcome$units$status, "success")
  expect_identical(
    result$outcome$units$reason_codes,
    list("legacy_input_changed")
  )
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

test_that("invalid pinned metadata schema reconstructs from exact clean data", {
  action <- data.table::data.table(
    pip_id = "P1", metadata_version_id = "m1", metadata_hash = "mh",
    data_version_id = "d2", data_hash = "dh2", input_hash = "ih",
    code_hash = "ch"
  )
  action[, aux_projection := list(list(
    cpi = stats::setNames(1, "2017_national")
  ))]
  loaded_aliases <- character()
  testthat::local_mocked_bindings(
    load_pip_data = function(pip_id, version, alias, verbose) {
      loaded_aliases <<- c(loaded_aliases, alias)
      if (identical(alias, "pip_meta")) {
        return(list(cpi = "wrong", ppp = 2, pop = 3))
      }
      data.table::data.table(welfare = 1)
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    st_hash_obj = function(x) {
      if (is.list(x) && !data.table::is.data.table(x)) "mh" else "dh2"
    },
    .package = "stamp"
  )
  testthat::local_mocked_bindings(
    pd_aux_attr = function(...) list(P1 = list(
      cpi = stats::setNames(1, "2017_national"),
      ppp = stats::setNames(2, "ppp_2017_v01_v01_national"),
      pop = stats::setNames(3, "2020_national")
    )),
    pd_assert_execution_fence = function(execution) invisible(execution),
    pd_save_receipt = function(...) list(success = TRUE),
    .package = "pipdata"
  )

  result <- pd_execute_metadata(
    action, list(aux = list(objects = list())), list(lease = NULL)
  )

  expect_true(result$success)
  expect_identical(loaded_aliases, c("pip_meta", "pip"))
})

test_that("metadata base schema requires all deflation vectors", {
  valid <- list(
    cpi = stats::setNames(1, "2017_national"),
    ppp = stats::setNames(2, "ppp_2017_v01_v01_national"),
    pop = stats::setNames(3, "2020_national")
  )
  expect_no_error(pd_validate_metadata_base(valid))

  invalid <- list(
    missing_pop = valid[c("cpi", "ppp")],
    character_cpi = within(valid, cpi <- "1"),
    unnamed_ppp = within(valid, ppp <- unname(ppp)),
    duplicate_pop = within(valid, {
      pop <- stats::setNames(c(3, 4), c("2020_national", "2020_national"))
    })
  )
  for (name in names(invalid)) {
    expect_error(
      pd_validate_metadata_base(invalid[[name]]),
      class = "pipdata_metadata_base_invalid",
      info = name
    )
  }
})

test_that("metadata base schema validates only requested legacy measures", {
  subset <- list(
    pce = stats::setNames(5, "2020_national"),
    cpi = stats::setNames(1, "2017_national")
  )

  expect_identical(
    names(pd_validate_metadata_base(subset, c("pce", "cpi"))),
    c("cpi", "pce")
  )
  expect_error(
    pd_validate_metadata_base(subset, c("pce", "cpi", "ppp")),
    class = "pipdata_metadata_base_invalid"
  )
})

test_that("prepared metadata core accounts for cached and blocked nodes", {
  actions <- data.table::data.table(
    stage = "metadata", entity_id = c("P1", "P2"), survey_id = c("S1", "S2"),
    pip_id = c("P1", "P2"), action = c("none", "refresh"),
    scheduling_state = c("cached", "blocked")
  )
  worker_calls <- 0L
  testthat::local_mocked_bindings(
    pd_execute_metadata = function(...) {
      worker_calls <<- worker_calls + 1L
      stop("cached or blocked metadata reached worker")
    },
    pd_finalize_checkpoint = function(...) stop("metadata checkpoint ran"),
    .package = "pipdata"
  )

  out <- pd_run_metadata_stage_prepared(
    execution = list(plan = list(actions = actions), lease = list()),
    actions = actions, run_id = "run", context = list(run_id = "run"),
    master = data.table::data.table(
      survey_id = c("S1", "S2"), pip_id = c("P1", "P2")
    ),
    options = pd_pipeline_options(), verbose = FALSE
  )

  expect_identical(out$outcome$units$status, c("cached", "skipped"))
  expect_identical(out$outcome$units$reason_codes[[2L]], "upstream_failed")
  expect_identical(worker_calls, 0L)
})

test_that("prepared metadata core lets unknown worker conditions escape", {
  action <- data.table::data.table(
    stage = "metadata", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    action = "refresh", input_hash = "input", code_hash = "code",
    data_version_id = "d1", data_hash = "dh1",
    reconstruct_base_metadata = TRUE
  )
  manifest <- list(records = data.table::data.table(
    stage = "clean", entity_id = "S1", output_receipts = list(list(list(
      alias = "pip", artifact = "P1", path = "p.qs2",
      version_id = "d1", content_hash = "dh1"
    )))
  ))
  testthat::local_mocked_bindings(
    pd_execute_metadata = function(...) {
      rlang::abort("unknown metadata failure", class = "unknown_metadata_failure")
    },
    .package = "pipdata"
  )

  expect_error(
    pd_run_metadata_stage_prepared(
      execution = list(plan = list(actions = action), manifest = manifest,
                       lease = list()),
      actions = action, run_id = "run", context = list(run_id = "run"),
      master = data.table::data.table(
        survey_id = "S1", pip_id = "P1", version_id_data = "d1",
        content_hash_data = "dh1"
      ),
      options = pd_pipeline_options(), verbose = FALSE
    ),
    class = "unknown_metadata_failure"
  )
})

test_that("recoverable metadata failure is durable and remains aggregateable", {
  action <- data.table::data.table(
    stage = "metadata", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    action = "refresh", input_hash = "input", code_hash = "code",
    data_version_id = "d1", data_hash = "dh1",
    reconstruct_base_metadata = TRUE
  )
  manifest <- list(records = data.table::data.table(
    stage = "clean", entity_id = "S1", output_receipts = list(list(list(
      alias = "pip", artifact = "P1", path = "p.qs2",
      version_id = "d1", content_hash = "dh1"
    )))
  ))
  reasons <- data.table::data.table(
    stage = "metadata", entity_id = "P1", reason = "aux_cpi_changed",
    input = "aux_cpi", old = "old", new = "new"
  )
  condition <- new_stage_condition_record(
    severity = "error", code = "report_lvl", message = "bad level",
    stage = "metadata", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    operation = "metadata", recoverable = TRUE
  )
  persisted <- 0L
  testthat::local_mocked_bindings(
    pd_execute_metadata = function(...) list(success = FALSE, condition = condition),
    pd_persist_failed_invalidation = function(execution, master, action, ...) {
      persisted <<- persisted + 1L
      master[, version_id_metadata := NA_character_]
      list(candidate = master, execution = execution)
    },
    pd_log_stage_condition = function(...) invisible(NULL),
    .package = "pipdata"
  )
  execution <- list(
    plan = list(actions = action, reasons = reasons), manifest = manifest,
    manifest_identity = NULL, lease = list()
  )
  master <- data.table::data.table(
    survey_id = "S1", pip_id = "P1", version_id_data = "d1",
    content_hash_data = "dh1", version_id_metadata = "m1"
  )

  out <- pd_run_metadata_stage_prepared(
    execution, action, "run", list(run_id = "run"), master,
    pd_pipeline_options(checkpoint_seconds = Inf), verbose = FALSE
  )

  expect_false(out$terminal)
  expect_identical(persisted, 1L)
  expect_true(is.na(out$master$version_id_metadata))
  expect_identical(out$outcome$units$reason_codes, list("entity_failed"))
  expect_identical(out$outcome$errors[[1L]]$code, "report_lvl")
  expect_no_error(validate_stage_units(out$outcome$units))
})

test_that("V7 V9 metadata refresh blocks one chain and commits its sibling", {
  actions <- data.table::data.table(
    stage = "metadata",
    entity_id = c("P1", "P2"),
    survey_id = c("S1", "S2"),
    pip_id = c("P1", "P2"),
    action = "refresh",
    scheduling_state = c("blocked", "runnable"),
    input_hash = c("input-1", "input-2"),
    code_hash = "metadata-code",
    data_version_id = c("d1", "d2"),
    data_hash = c("dh1", "dh2"),
    metadata_version_id = c("m1", "m2"),
    metadata_hash = c("mh1", "mh2")
  )
  manifest <- list(records = data.table::data.table(
    stage = "clean",
    entity_id = c("S1", "S2"),
    output_receipts = list(
      list(list(
        alias = "pip", artifact = "P1", path = "p1.qs2",
        version_id = "d1", content_hash = "dh1"
      )),
      list(list(
        alias = "pip", artifact = "P2", path = "p2.qs2",
        version_id = "d2", content_hash = "dh2"
      ))
    )
  ))
  reasons <- data.table::data.table(
    stage = "metadata", entity_id = "P2", reason = "aux_cpi_changed",
    input = "aux_cpi", old = "old", new = "new"
  )
  execution <- list(
    plan = list(actions = actions, reasons = reasons),
    manifest = manifest,
    manifest_identity = NULL,
    lease = list()
  )
  master <- data.table::data.table(
    survey_id = c("S1", "S2"), pip_id = c("P1", "P2"),
    version_id_data = c("d1", "d2"),
    content_hash_data = c("dh1", "dh2")
  )
  workers <- character()
  refreshes <- 0L
  testthat::local_mocked_bindings(
    pd_execute_metadata = function(action, ...) {
      workers <<- c(workers, action$entity_id[[1L]])
      list(
        success = TRUE,
        pip_id = action$pip_id[[1L]],
        alias = "pip_meta",
        artifact = action$pip_id[[1L]],
        path = paste0(tolower(action$pip_id[[1L]]), ".qs2"),
        version_id = "meta-new",
        content_hash = "meta-hash-new",
        input_hash = action$input_hash[[1L]],
        code_hash = action$code_hash[[1L]],
        data_version_id = action$data_version_id[[1L]],
        data_hash = action$data_hash[[1L]]
      )
    },
    pd_finalize_checkpoint = function(execution, master, ...) {
      execution$manifest_identity <- list(
        filename = "manifest-v1-2.rds", uuid = "u2", checksum = "c2",
        generation = 2
      )
      list(candidate = master, execution = execution)
    },
    .package = "pipdata"
  )

  out <- pd_run_metadata_stage_prepared(
    execution = execution,
    actions = actions,
    run_id = "run",
    context = list(run_id = "run"),
    master = master,
    options = pd_pipeline_options(checkpoint_seconds = Inf),
    verbose = FALSE,
    checkpoint_callback = function(execution, master) {
      refreshes <<- refreshes + 1L
      execution
    }
  )

  expect_identical(workers, "P2")
  expect_identical(refreshes, 1L)
  expect_identical(
    out$outcome$units$status,
    c("skipped", "success")
  )
  expect_identical(
    out$outcome$units$reason_codes,
    list("upstream_failed", "aux_cpi_changed")
  )
})
