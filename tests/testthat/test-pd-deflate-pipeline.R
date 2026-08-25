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
    pd_deflation_exact = function(pip_id, data_version_id,
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
  expect_null(pd_execute_deflate(action, FALSE))
  expect_false(latest_called)
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
  action <- inv[, .(stage = "deflate", entity_id = pip_id, survey_id, pip_id,
                    action = "refresh", input_hash, code_hash)]
  executed <- character()
  testthat::local_mocked_bindings(
    pd_dependency_context = function() list(scope_id = "scope"),
    pd_prepare_execution = function(...) list(
      plan = list(actions = action), lease = list(),
      snapshot = list(), manifest = pd_empty_manifest(list(scope_id = "scope")),
      manifest_identity = NULL, context = list(scope_id = "scope")
    ),
    pd_lease_release = function(...) invisible(NULL),
    pd_execute_deflate = function(action, verbose) {
      executed <<- c(executed, action$pip_id)
      list(pip_id = action$pip_id, success = TRUE, version_id = "f1",
           content_hash = "fh", input_hash = "ih", code_hash = "ch")
    },
    pd_finalize_checkpoint = function(execution, master, stage, results, ...) {
      master[results, on = "pip_id", deflated := TRUE]
      list(candidate = master, execution = execution)
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
  expect_false(out$deflated)
  expect_false(writes$master$deflated)
  expect_true(is.na(writes$master$version_id_deflated))
  expect_identical(
    writes$master$latest_release_version_id, "release-v1"
  )
})
