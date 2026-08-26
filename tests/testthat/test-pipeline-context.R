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
