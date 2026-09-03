deflation_action <- function() {
  data.table::data.table(
    survey_id = "S1", pip_id = "ABC_2015_TST_INC_D1",
    version_id_data = "d1", content_hash_data = "dh",
    version_id_metadata = "m1", content_hash_metadata = "mh"
  )
}

test_that("authoritative deflation uses only exact pinned inputs", {
  action <- deflation_action()
  called <- list()
  testthat::local_mocked_bindings(
    pd_deflation_exact_strict = function(pip_id, data_version_id,
                                  metadata_version_id, data_hash,
                                  metadata_hash, verbose) {
      called <<- as.list(environment())
      data.table::data.table(welfare = 1)
    },
    pd_deflation = function(...) stop("latest fallback must be unreachable"),
    pd_assert_execution_fence = function(execution) invisible(execution),
    pd_save_receipt = function(x, id, alias, verbose, lease) list(
      alias = alias, artifact = id, path = "p.qs2", version_id = "o1",
      content_hash = "oh", success = TRUE
    ),
    .package = "pipdata"
  )
  attr(action, "execution") <- list(lease = list())
  attr(action, "lease") <- list()
  result <- pd_execute_deflate(action, FALSE)
  expect_true(result$success)
  expect_identical(called$data_version_id, "d1")
  expect_identical(called$metadata_version_id, "m1")
  expect_identical(called$data_hash, "dh")
  expect_identical(called$metadata_hash, "mh")
})

test_that("authoritative deflation fails closed on incomplete actions", {
  action <- deflation_action()[, content_hash_metadata := NULL]
  latest_called <- FALSE
  testthat::local_mocked_bindings(
    pd_deflation = function(...) {
      latest_called <<- TRUE
      data.table::data.table()
    },
    .package = "pipdata"
  )
  expect_error(
    pd_execute_deflate(action, FALSE),
    class = "pipdata_deflation_action_invalid"
  )
  expect_false(latest_called)
})

test_that("unknown worker errors fail closed", {
  action <- deflation_action()
  testthat::local_mocked_bindings(
    pd_deflation_exact_strict = function(...) rlang::abort(
      "unexpected fence failure", class = "unrecognized_storage_failure"
    ),
    .package = "pipdata"
  )
  expect_error(
    pd_execute_deflate(action, FALSE),
    class = "unrecognized_storage_failure"
  )
})

test_that("legacy single-survey deflation adapter remains compatible", {
  action <- data.table::data.table(pip_id = "ABC_2015_TST_INC_D1")
  testthat::local_mocked_bindings(
    pd_deflation = function(pip_id, verbose) data.table::data.table(welfare = 1),
    save_pip_data = function(data, alias, verbose) list(list(success = TRUE)),
    .package = "pipdata"
  )
  expect_identical(
    deflate_one(action, FALSE),
    list(pip_id = "ABC_2015_TST_INC_D1", success = TRUE)
  )
})

test_that("pipeline exported signature remains stable", {
  expect_identical(
    names(formals(pd_deflate_pipeline)),
    c("inv", "force", "verbose", "bootstrap", "bootstrap_entities",
      "dependency_plan")
  )
})

test_that("pd_deflate_pipeline executes fresh exact actions and returns candidate", {
  inv <- deflation_action()
  inv[, `:=`(deflated = FALSE, input_hash = "ih", code_hash = "ch")]
  executed <- character()
  testthat::local_mocked_bindings(
    pd_deflate_pipeline_core = function(inv, ...) {
      executed <<- inv$pip_id
      inv[, deflated := TRUE]
      list(result = structure(list(), class = "pipdata_stage_result"),
           master = inv, context = structure(list(), class = "pipeline_context"))
    },
    .package = "pipdata"
  )
  out <- pd_deflate_pipeline(inv, verbose = FALSE)
  expect_identical(executed, inv$pip_id)
  expect_true(out$deflated)
})

test_that("failed deflation invalidation is written durably for restart", {
  action <- deflation_action()
  action[, `:=`(stage = "deflate", entity_id = pip_id)]
  master <- data.table::copy(action)
  master[, `:=`(
    deflated = TRUE, version_id_deflated = "old-v",
    content_hash_deflated = "old-h"
  )]
  writes <- list()
  writer <- function(name) function(candidate, lease) {
    writes[[name]] <<- data.table::copy(candidate)
    list(success = TRUE, version_id = paste0(name, "-v1"))
  }
  testthat::local_mocked_bindings(
    pd_assert_execution_fence = function(execution) invisible(execution),
    pd_invalidate_failed_action = function(master, action) {
      master <- data.table::copy(master)
      master[, `:=`(
        deflated = FALSE, version_id_deflated = NA_character_,
        content_hash_deflated = NA_character_
      )]
      master
    },
    .package = "pipdata"
  )
  out <- pd_persist_failed_invalidation(
    list(lease = list()), master, action,
    writer("release"), writer("master")
  )
  expect_false(out$candidate$deflated)
  expect_false(writes$master$deflated)
  expect_true(is.na(writes$master$version_id_deflated))
  expect_identical(
    writes$master$latest_release_version_id, "release-v1"
  )
})

test_that("failed invalidation fences exact advanced real Stamp receipts", {
  root <- withr::local_tempdir()
  suffix <- gsub("[^A-Za-z0-9]", "", fs::path_file(root))
  aliases <- c(
    release = paste0("failed_release_", suffix),
    master = paste0("failed_master_", suffix)
  )
  roots <- stats::setNames(
    file.path(root, paste0("store-", names(aliases))), names(aliases)
  )
  for (name in names(aliases)) {
    stamp::st_init(root = roots[[name]], alias = aliases[[name]])
  }
  context <- list(scope_id = paste0("failed-invalidation-", suffix))
  lease <- pd_lease_acquire(context, root, run_id = "failed-invalidation")
  withr::defer(pd_lease_release(lease))
  action <- deflation_action()
  action[, `:=`(stage = "deflate", entity_id = pip_id)]
  master <- data.table::copy(action)
  master[, `:=`(
    deflated = TRUE, version_id_deflated = "old-v",
    content_hash_deflated = "old-h"
  )]
  old_release <- pd_save_receipt(
    master, "pip_release_inventory", aliases[["release"]], lease = lease
  )
  master[, latest_release_version_id := old_release$version_id]
  pd_save_receipt(
    master, "pip_master_inventory", aliases[["master"]], lease = lease
  )
  inventory_aliases <- unname(aliases[c("release", "master")])
  catalogs <- stats::setNames(lapply(inventory_aliases, function(alias) {
    data.table::as.data.table(stamp::st_catalog_query(alias = alias))
  }), inventory_aliases)
  execution <- list(
    context = context,
    lease = lease,
    manifest = pd_empty_manifest(context),
    manifest_identity = NULL,
    snapshot = list(
      context = context,
      fingerprints = list(),
      aux = list(catalog = data.table::data.table()),
      catalogs = catalogs,
      master = data.table::copy(master)
    )
  )
  writer <- function(alias, id) {
    function(candidate, active_lease) {
      pd_save_receipt(candidate, id, alias, lease = active_lease)
    }
  }

  testthat::local_mocked_bindings(
    pd_dependency_context = function() context,
    pd_code_fingerprints = function() list(),
    pd_manifest_read = function(...) structure(
      list(), class = "pipdata_manifest_absent"
    ),
    .package = "pipdata"
  )
  real_catalog_query <- stamp::st_catalog_query
  testthat::local_mocked_bindings(
    st_catalog_query = function(alias, ...) {
      if (identical(alias, "aux")) {
        return(data.table::data.table())
      }
      real_catalog_query(alias = alias, ...)
    },
    .package = "stamp"
  )
  out <- pd_persist_failed_invalidation(
    execution, master, action,
    writer(aliases[["release"]], "pip_release_inventory"),
    writer(aliases[["master"]], "pip_master_inventory")
  )

  candidate <- out$candidate
  expect_false(candidate$deflated)
  expect_true(is.na(candidate$version_id_deflated))
  expect_identical(
    candidate$latest_release_version_id,
    out$release_receipt$version_id
  )
  expect_identical(
    nrow(stamp::st_versions(
      "pip_release_inventory.qs2", alias = aliases[["release"]]
    )),
    2L
  )
  expect_identical(
    nrow(stamp::st_versions(
      "pip_master_inventory.qs2", alias = aliases[["master"]]
    )),
    2L
  )
})

test_that("real core rejects changed inputs before executing a worker", {
  inv <- deflation_action()
  action <- data.table::data.table(
    stage = "deflate", entity_id = inv$pip_id, survey_id = inv$survey_id,
    pip_id = inv$pip_id, action = "create", input_hash = "ih",
    code_hash = "ch", data_version_id = "changed", data_hash = "dh",
    metadata_version_id = "m1", metadata_hash = "mh",
    reason = list("new_entity")
  )
  context <- list(release = "20260826", identity = "TEST", roots = list(),
                  namespace = "test")
  context$scope_id <- pd_context_hash(context)
  execution <- list(
    context = context,
    snapshot = list(
      fingerprints = list(summary = data.table::data.table(
        stage = "deflate", hash = "ch"
      )), captured_at = Sys.time()
    ),
    plan = list(
      actions = action,
      reasons = data.table::data.table(
        stage = "deflate", entity_id = inv$pip_id, reason = "new_entity",
        input = NA_character_, old = NA_character_, new = NA_character_
      )
    ),
    manifest = pd_empty_manifest(context), manifest_identity = NULL,
    lease = list()
  )
  worker_called <- FALSE
  testthat::local_mocked_bindings(
    pd_dependency_context = function() context,
    pd_prepare_execution = function(...) execution,
    pd_pipeline_storage = function(...) list(
      aliases = c(pip = "pip", pip_meta = "pip_meta",
                  pip_deflated = "pip_deflated", pip_master = "pip_master",
                  pip_inv = "pip_inv"),
      roots = c(pip = "p", pip_meta = "m", pip_deflated = "d",
                pip_master = "pm", pip_inv = "pi"), log_name = "pipdata_log"
    ),
    pd_execute_deflate = function(...) {
      worker_called <<- TRUE
      list(success = TRUE)
    },
    pd_lease_release = function(...) invisible(NULL),
    .package = "pipdata"
  )
  expect_error(
    pd_deflate_pipeline_core(
      inv, FALSE, FALSE, FALSE, NULL, NULL, character(), "continue", "abort"
    ),
    class = "pipdata_deflation_action_invalid"
  )
  expect_false(worker_called)
})

test_that("real core accounts for checkpoint-pending and unattempted units", {
  inv <- data.table::rbindlist(replicate(3L, deflation_action(), simplify = FALSE))
  inv[, pip_id := paste0("id", seq_len(.N))]
  inv[, survey_id := paste0("s", seq_len(.N))]
  actions <- inv[, .(
    stage = "deflate", entity_id = pip_id, survey_id, pip_id,
    action = "create", input_hash = paste0("ih", seq_len(.N)),
    code_hash = "ch", data_version_id = version_id_data,
    data_hash = content_hash_data, metadata_version_id = version_id_metadata,
    metadata_hash = content_hash_metadata, reason = list("new_entity")
  )]
  context <- list(release = "20260826", identity = "TEST", roots = list(),
                  namespace = "test")
  context$scope_id <- pd_context_hash(context)
  execution <- list(
    context = context,
    snapshot = list(
      fingerprints = list(summary = data.table::data.table(
        stage = "deflate", hash = "ch"
      )), captured_at = Sys.time()
    ),
    plan = list(
      actions = actions,
      reasons = actions[, .(stage, entity_id, reason = "new_entity",
                            input = NA_character_, old = NA_character_,
                            new = NA_character_)]
    ),
    manifest = pd_empty_manifest(context), manifest_identity = NULL,
    lease = list()
  )
  old_n <- getOption("pipdata.manifest_checkpoint_n")
  options(pipdata.manifest_checkpoint_n = 2L)
  on.exit(options(pipdata.manifest_checkpoint_n = old_n), add = TRUE)
  testthat::local_mocked_bindings(
    pd_dependency_context = function() context,
    pd_prepare_execution = function(...) execution,
    pd_pipeline_storage = function(...) list(
      aliases = c(pip = "pip", pip_meta = "pip_meta",
                  pip_deflated = "pip_deflated", pip_master = "pip_master",
                  pip_inv = "pip_inv"),
      roots = c(pip = "p", pip_meta = "m", pip_deflated = "d",
                pip_master = "pm", pip_inv = "pi"), log_name = "pipdata_log"
    ),
    pd_execute_deflate = function(action, verbose) list(
      stage = "deflate", pip_id = action$pip_id, success = TRUE,
      alias = "pip_deflated", artifact = action$pip_id, path = "p",
      version_id = "v", content_hash = paste0("oh", action$pip_id),
      data_version_id = action$data_version_id,
      metadata_version_id = action$metadata_version_id,
      input_hash = action$input_hash, code_hash = action$code_hash
    ),
    pd_finalize_checkpoint = function(...) rlang::abort(
      "checkpoint failed", class = "pipdata_checkpoint_release_error"
    ),
    pd_lease_release = function(...) invisible(NULL),
    pd_log_deflate_summary = function(...) invisible(NULL),
    .package = "pipdata"
  )
  result <- pd_deflate_pipeline_core(
    inv, FALSE, FALSE, FALSE, NULL, NULL, character(), "continue",
    "capture_at_run_boundary"
  )$result
  expect_true(result$terminal)
  expect_identical(result$units$status, c("failed", "failed", "skipped"))
  expect_identical(
    vapply(result$units$reason_codes, `[[`, "", 1L),
    c("checkpoint_uncommitted", "checkpoint_uncommitted", "upstream_failed")
  )
  expect_identical(unname(result$counts[c("selected", "attempted", "skipped")]),
                   c(3L, 2L, 1L))
})

test_that("recoverable deflation failure keeps its condition code out of reasons", {
  action <- deflation_action()
  action[, `:=`(
    stage = "deflate", entity_id = pip_id, action = "refresh",
    input_hash = "ih", code_hash = "ch", data_version_id = version_id_data,
    data_hash = content_hash_data, metadata_version_id = version_id_metadata,
    metadata_hash = content_hash_metadata
  )]
  condition <- new_stage_condition_record(
    severity = "error", code = "deflation_na", message = "bad data",
    stage = "deflate", entity_id = action$pip_id, survey_id = action$survey_id,
    pip_id = action$pip_id, operation = "transform", recoverable = TRUE
  )
  reasons <- action[, .(
    stage, entity_id, reason = "aux_cpi_changed", input = "aux_cpi",
    old = "old", new = "new"
  )]
  master <- data.table::copy(action)
  master[, `:=`(
    deflated = TRUE, version_id_deflated = "old-v",
    content_hash_deflated = "old-h"
  )]
  execution <- list(
    plan = list(actions = action, reasons = reasons), manifest_identity = NULL,
    lease = list()
  )
  persisted <- 0L
  testthat::local_mocked_bindings(
    pd_execute_deflate = function(...) list(success = FALSE, condition = condition),
    pd_persist_failed_invalidation = function(execution, master, action, ...) {
      persisted <<- persisted + 1L
      master[, `:=`(
        deflated = FALSE, version_id_deflated = NA_character_,
        content_hash_deflated = NA_character_
      )]
      list(candidate = master, execution = execution)
    },
    pd_log_stage_condition = function(...) invisible(NULL),
    .package = "pipdata"
  )

  out <- pd_run_deflate_stage_prepared(
    execution, action, "run", list(run_id = "run"), master,
    pd_pipeline_options(checkpoint_seconds = Inf), verbose = FALSE
  )

  expect_false(out$terminal)
  expect_identical(persisted, 1L)
  expect_false(out$master$deflated)
  expect_identical(out$outcome$units$reason_codes, list("entity_failed"))
  expect_identical(out$outcome$errors[[1L]]$code, "deflation_na")
  expect_no_error(validate_stage_units(out$outcome$units))
})

test_that("prepared deflate path accounts for cached nodes without preparation", {
  action <- data.table::data.table(
    stage = "deflate", entity_id = "P1", survey_id = "S1", pip_id = "P1",
    action = "none"
  )
  prepare_calls <- 0L
  worker_calls <- 0L
  testthat::local_mocked_bindings(
    pd_prepare_execution = function(...) {
      prepare_calls <<- prepare_calls + 1L
      stop("prepared path must not prepare execution")
    },
    pd_execute_deflate = function(...) {
      worker_calls <<- worker_calls + 1L
      stop("cached deflate work reached worker")
    },
    .package = "pipdata"
  )

  out <- pd_run_deflate_stage_prepared(
    execution = list(plan = list(actions = action), lease = list(),
                     manifest_identity = NULL),
    actions = action, run_id = "run", context = list(run_id = "run"),
    master = data.table::data.table(survey_id = "S1", pip_id = "P1"),
    options = pd_pipeline_options(checkpoint_seconds = Inf), verbose = FALSE
  )

  expect_identical(out$outcome$units$status, "cached")
  expect_identical(prepare_calls, 0L)
  expect_identical(worker_calls, 0L)
})
