test_that("planner scales to 2500 entities without external IO", {
  calls <- 0L
  testthat::local_mocked_bindings(
    st_catalog_query = function(...) {
      calls <<- calls + 1L
      stop("planner must not perform catalog IO")
    },
    .package = "stamp"
  )
  inv <- data.table::data.table(survey_id = sprintf("s%04d", 1:2500))
  context <- list(scope_id = "scope")
  manifest <- pd_empty_manifest(context)
  elapsed <- system.time(plan <- pd_dependency_plan(
    inv, data.table::data.table(), manifest, context, fingerprints = list()
  ))[["elapsed"]]
  expect_length(plan$actions$entity_id, 2500L)
  expect_lt(as.numeric(utils::object.size(plan)), 5e6)
  expect_gte(elapsed, 0)
  expect_identical(calls, 0L)
})

test_that("2500 execution units use bounded checkpoints and zero catalog IO", {
  catalog_calls <- 0L
  checkpoints <- integer()
  testthat::local_mocked_bindings(
    st_catalog_query = function(...) {
      catalog_calls <<- catalog_calls + 1L
      stop("execution scheduler must not query catalogs")
    }, .package = "stamp"
  )
  pd_run_checkpoint_batches(
    as.list(seq_len(2500L)),
    worker = function(id) list(success = TRUE, id = id),
    checkpoint = function(batch) checkpoints <<- c(checkpoints, length(batch)),
    checkpoint_n = 100L, checkpoint_seconds = Inf
  )
  expect_identical(catalog_calls, 0L)
  expect_length(checkpoints, 25L)
  expect_true(all(checkpoints == 100L))
})

test_that("fact construction binds accumulated rows once", {
  current <- data.table::data.table(
    stage = "metadata", entity_id = paste0("p", 1:20),
    survey_id = paste0("s", 1:20), pip_id = paste0("p", 1:20),
    output_version_id = NA_character_, output_hash = NA_character_,
    input_hash = paste0("input-", 1:20),
    legacy_input_hash = paste0("legacy-", 1:20),
    code_hash = "metadata-code"
  )
  snapshot <- list(
    current = current,
    fingerprints = list(
      components = data.table::data.table(
        stage = character(), component = character(), hash = character()
      )
    )
  )
  manifest <- pd_empty_manifest(list(scope_id = "scope"))
  bind_calls <- 0L
  original_rbindlist <- data.table::rbindlist
  testthat::local_mocked_bindings(
    rbindlist = function(...) {
      bind_calls <<- bind_calls + 1L
      original_rbindlist(...)
    },
    .package = "data.table"
  )

  facts <- pd_snapshot_facts(snapshot, manifest)

  expect_identical(nrow(facts), 20L)
  expect_lte(bind_calls, 1L)
})

v22_country_codes <- function(n) {
  grid <- expand.grid(LETTERS, LETTERS, LETTERS, stringsAsFactors = FALSE)
  apply(grid[seq_len(n), ], 1L, paste0, collapse = "")
}

v22_authoritative_snapshot <- function(n) {
  country <- v22_country_codes(n)
  acronym <- sprintf("S%04d", seq_len(n))
  pip_id <- paste(country, 2018L, acronym, "INC", "ALL", sep = "_")
  survey_id <- paste(country, 2018L, acronym, "V01_M_V01_A_GMD_ALL", sep = "_")
  pfw <- data.table::data.table(
    country_code = country,
    surveyid_year = 2018L,
    survey_acronym = acronym,
    welfare_type = "income",
    inpovcal = 1L,
    cpi_domain = 1L,
    ppp_domain = 1L,
    gdp_domain = 1L,
    pce_domain = 1L,
    pop_domain = 1L,
    cpi_domain_var = "urban",
    ppp_domain_var = "urban"
  )
  aux <- list(
    pfw = pfw,
    cpi = data.table::data.table(
      country_code = country, year = 2018L, survey_acronym = acronym,
      cpi_year = 2017L, reporting_level = "national", cpi_value = 1
    ),
    ppp = data.table::data.table(
      country_code = country, ppp_year = 2017L, release_version = "v01",
      adaptation_version = "v01", reporting_level = "national", ppp = 2
    ),
    pop = data.table::data.table(
      country_code = country, year = 2018L,
      reporting_level = "national", pop = 3
    ),
    gdp = data.table::data.table(
      country_code = country, year = 2018L,
      reporting_level = "national", gdp = 4
    ),
    pce = data.table::data.table(
      country_code = country, year = 2018L,
      reporting_level = "national", pce = 5
    )
  )
  master <- data.table::data.table(
    survey_id, pip_id, country_code = country, surveyid_year = 2018L,
    survey_acronym = acronym, module = "ALL", welfare_type = "income",
    version_id_data = paste0("data-v-", seq_len(n)),
    content_hash_data = paste0("data-h-", seq_len(n)),
    version_id_metadata = paste0("meta-v-", seq_len(n)),
    content_hash_metadata = paste0("meta-h-", seq_len(n)),
    deflated = FALSE
  )
  inv <- data.table::data.table(
    survey_id,
    pipeline_version = 1L,
    latest_version_id = paste0("dlw-v-", seq_len(n)),
    content_hash = paste0("dlw-h-", seq_len(n)),
    file_path = paste0(survey_id, ".qs2"),
    status = "valid",
    data_available = "Yes",
    date_validated = as.POSIXct("2026-08-31 10:00:00", tz = "UTC"),
    Checksum = paste0("checksum-", seq_len(n)),
    country_code = country,
    surveyid_year = 2018L,
    survey_acronym = acronym,
    vermast = "v01",
    veralt = "v01",
    collection = "GMD",
    module = "ALL",
    tool = "TB"
  )
  fingerprints <- list(
    summary = data.table::data.table(
      stage = c("clean", "metadata", "deflate"),
      hash = c("clean-code", "metadata-code", "deflate-code")
    ),
    components = data.table::data.table(
      stage = character(), component = character(), hash = character()
    ),
    audit = list()
  )
  catalog <- data.table::data.table(
    path = paste0(names(aux), ".qs2"),
    version_id = paste0(names(aux), "-v1"),
    content_hash = vapply(aux, stamp::st_hash_obj, character(1L)),
    created_at = as.POSIXct("2026-08-31 10:00:00", tz = "UTC")
  )
  objects <- aux
  names(objects) <- names(aux)
  list(inv = inv, master = master, aux = aux, objects = objects, catalog = catalog,
       fingerprints = fingerprints)
}

test_that("V22 public checkpoints have bounded linear dependency operations", {
  run_size <- function(n) {
    fixture <- v22_authoritative_snapshot(n)
    context <- list(
      schema_version = 1L, release = "20260831", identity = "TEST",
      roots = as.list(stats::setNames(
        paste0("root/", c("pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv")),
        c("pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv")
      )),
      namespace = paste0("v22-", n)
    )
    context$scope_id <- pd_context_hash(context)
    absent_manifest <- structure(list(), class = "pipdata_manifest_absent")
    catalog_calls <- stats::setNames(integer(5L), c(
      "aux", "pip", "pip_meta", "pip_deflated", "pip_inv"
    ))
    household_reads <- 0L
    projection_calls <- 0L
    snapshot_builds <- 0L
    join_calls <- 0L
    original_select <- pd_indexed_aux_rows
    original_snapshot <- pd_build_dependency_snapshot
    original_facts <- pd_snapshot_facts
    original_merge <- getS3method("merge", "data.table")
    cached_snapshot <- NULL
    cached_facts <- NULL
    actions <- data.table::rbindlist(list(
      fixture$inv[, .(
        stage = "clean", entity_id = survey_id, survey_id,
        pip_id = NA_character_, action = "create"
      )],
      fixture$master[, .(
        stage = "metadata", entity_id = pip_id, survey_id, pip_id,
        action = "create"
      )],
      fixture$master[, .(
        stage = "deflate", entity_id = pip_id, survey_id, pip_id,
        action = "create"
      )]
    ))
    reasons <- actions[, .(
      stage, entity_id, reason = "unknown_provenance",
      input = NA_character_, old = NA_character_, new = NA_character_
    )]
    static_plan <- structure(
      list(
        context = context, actions = actions, reasons = reasons,
        snapshot = list()
      ),
      class = "pip_dependency_plan"
    )

    cached_core <- function(
      execution, actions, run_id, context, master, options,
      checkpoint_callback = NULL, ...
    ) {
      stage <- unique(actions$stage)
      outcome <- pd_new_stage_outcome(stage, execution$manifest_identity)
      if (nrow(actions) && is.function(checkpoint_callback)) {
        execution <- checkpoint_callback(execution, master)
      }
      if (nrow(actions)) {
        outcome$units <- data.table::rbindlist(lapply(
          seq_len(nrow(actions)),
          function(i) pd_stage_unit_row(
            actions[i], stage, "cached", "current"
          )
        ))
      }
      outcome$completed_at <- pd_utc_time(Sys.time())
      list(
        execution = execution, master = master, context = context,
        outcome = outcome, terminal = FALSE, error = NULL
      )
    }

    testthat::local_mocked_bindings(
      st_catalog_query = function(alias, ...) {
        catalog_calls[[alias]] <<- catalog_calls[[alias]] + 1L
        if (identical(alias, "aux")) fixture$catalog else data.table::data.table()
      },
      .package = "stamp"
    )
    testthat::local_mocked_bindings(
      load_aux_data = function(measure, version = NULL, ...) {
        fixture$objects[[measure]]
      },
      pip_read = function(...) {
        household_reads <<- household_reads + 1L
        rlang::abort("V22 snapshot must not read household data")
      },
      .package = "pipload"
    )
    testthat::local_mocked_bindings(
      pd_indexed_aux_rows = function(...) {
        projection_calls <<- projection_calls + 1L
        original_select(...)
      },
      pd_build_dependency_snapshot = function(...) {
        snapshot_builds <<- snapshot_builds + 1L
        if (is.null(cached_snapshot)) {
          cached_snapshot <<- original_snapshot(...)
        }
        snapshot <- cached_snapshot
        arguments <- list(...)
        snapshot$master <- data.table::copy(
          data.table::as.data.table(arguments$master)
        )
        snapshot$captured_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
        snapshot
      },
      pd_snapshot_facts = function(snapshot, manifest) {
        if (is.null(cached_facts)) {
          cached_facts <<- original_facts(snapshot, manifest)
        }
        cached_facts
      },
      pd_snapshot_identity = function(...) paste0("snapshot-", n),
      pd_dependency_plan = function(...) static_plan,
      pd_dependency_context = function(...) context,
      pd_lease_acquire = function(...) list(token = "lease"),
      pd_lease_release = function(...) invisible(NULL),
      pd_manifest_read = function(...) absent_manifest,
      pd_assert_execution_refresh_fence = function(...) invisible(NULL),
      pd_assert_execution_fence = function(...) invisible(NULL),
      pd_final_retained_manifest = function(execution) execution,
      pd_code_fingerprints = function(...) fixture$fingerprints,
      pd_run_clean_stage_prepared = cached_core,
      pd_run_metadata_stage_prepared = cached_core,
      pd_run_deflate_stage_prepared = cached_core,
      sync_recode_spec = function(...) list(version = "test"),
      pd_log_clean_summary = function(result) invisible(result),
      pd_log_deflate_summary = function(result) invisible(result),
      pd_log_pipeline_summary = function(result) invisible(result),
      .package = "pipdata"
    )
    testthat::local_mocked_bindings(
      merge.data.table = function(...) {
        join_calls <<- join_calls + 1L
        original_merge(...)
      },
      .package = "data.table"
    )
    testthat::local_mocked_bindings(
      load_pip_master_inventory = function(...) data.table::copy(fixture$master),
      .package = "pipload"
    )

    elapsed <- system.time(result <- pd_run_pipeline(
      inv = fixture$inv, bootstrap = TRUE, checkpoint_size = n,
      checkpoint_seconds = Inf, verbose = FALSE
    ))[["elapsed"]]
    expect_s3_class(result, "pipdata_pipeline_result")
    expect_identical(result$counts$selected, 3L * n)
    list(
      snapshot_builds = snapshot_builds,
      projection_calls = projection_calls,
      join_calls = join_calls,
      catalog_calls = catalog_calls,
      household_reads = household_reads,
      elapsed = unname(elapsed)
    )
  }

  small <- run_size(1250L)
  large <- run_size(2500L)
  fixed_setup_c <- 2L

  for (operation in c(
    "snapshot_builds", "projection_calls", "join_calls", "household_reads"
  )) {
    expect_lte(
      large[[operation]], 2L * small[[operation]] + fixed_setup_c
    )
  }
  expect_true(all(
    large$catalog_calls <= 2L * small$catalog_calls + fixed_setup_c
  ))
  expect_identical(small$household_reads, 0L)
  expect_identical(large$household_reads, 0L)
  expect_gte(small$elapsed, 0)
  expect_gte(large$elapsed, 0)
})
