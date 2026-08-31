test_that("pipeline options retain exact order and Inf timeout", {
  options <- pd_pipeline_options(checkpoint_seconds = Inf)
  expect_named(options, c(
    "verbose", "force", "force_surveys", "bootstrap", "bootstrap_entities",
    "checkpoint_size", "checkpoint_seconds", "entity_error_policy",
    "fatal_error_policy"
  ))
  expect_identical(options$checkpoint_seconds, Inf)
  expect_identical(options$force_surveys, character())
})

test_that("plan hash ignores row order and snapshot timestamps", {
  context <- list(release = "20260101", identity = "TEST", roots = list(),
                  namespace = "")
  context$scope_id <- pd_context_hash(context)
  actions <- data.table::data.table(
    stage = c("deflate", "clean"), entity_id = c("b", "a"),
    survey_id = NA_character_, pip_id = c("b", "a"),
    action = c("create", "create"), input_hash = c("2", "1"),
    code_hash = c("d", "c")
  )
  reasons <- data.table::data.table(
    stage = actions$stage, entity_id = actions$entity_id,
    reason = "new_entity", input = NA_character_, old = NA_character_,
    new = NA_character_
  )
  execution <- list(context = context, plan = list(actions = actions,
                    reasons = reasons, snapshot = list(captured_at = Sys.time())))
  reordered <- execution
  reordered$plan$actions <- actions[2:1]
  reordered$plan$reasons <- reasons[2:1]
  reordered$plan$snapshot$captured_at <- Sys.time() + 100
  expect_identical(pd_plan_hash(execution), pd_plan_hash(reordered))
})

test_that("plan hash includes deterministic current-node rows", {
  context <- list(release = "20260101", identity = "TEST", roots = list(),
                  namespace = "")
  context$scope_id <- pd_context_hash(context)
  inv <- data.table::data.table(survey_id = "s1")
  master <- data.table::data.table(survey_id = "s1", pip_id = "p1")
  current <- data.table::data.table(
    stage = c("metadata", "clean", "deflate"),
    entity_id = c("p1", "s1", "p1"),
    survey_id = "s1",
    pip_id = c("p1", NA_character_, "p1"),
    input_hash = c("metadata-input", "clean-input", "deflate-input")
  )
  facts <- data.table::data.table(
    stage = "metadata", entity_id = "p1", survey_id = "s1", pip_id = "p1",
    reason = "aux_cpi_changed", input = "aux_cpi", old = "old", new = "new"
  )
  manifest <- pd_empty_manifest(context)
  make_execution <- function(current_rows, fact_rows, captured_at) {
    plan <- pd_dependency_plan(
      inv,
      master,
      manifest,
      context = context,
      fingerprints = list(),
      snapshot = list(
        current = current_rows,
        facts = fact_rows,
        captured_at = captured_at
      )
    )
    list(context = context, plan = plan)
  }

  execution <- make_execution(current, facts, Sys.time())
  reordered <- make_execution(current[3:1], facts, Sys.time() + 100)
  actionable_only <- execution
  actionable_only$plan$actions <- execution$plan$actions[action != "none"]

  expect_identical(execution$plan$actions$action, c("none", "none", "refresh"))
  expect_identical(pd_plan_hash(execution), pd_plan_hash(reordered))
  expect_false(
    identical(pd_plan_hash(execution), pd_plan_hash(actionable_only))
  )
})
