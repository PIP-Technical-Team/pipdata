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
