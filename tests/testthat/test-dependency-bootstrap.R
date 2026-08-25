test_that("bootstrap selector intersects unknown actions", {
  plan <- pd_dependency_plan(
    data.table::data.table(survey_id = c("s1", "s2")),
    data.table::data.table(survey_id = c("s1", "s2"), pip_id = c("p1", "p2")),
    context = list(scope_id = "scope"), fingerprints = list()
  )
  selected <- pd_assert_bootstrap(plan, TRUE, "s1")
  expect_true(all(selected$actions$survey_id == "s1"))
  expect_false(pd_dependency_completeness(selected)$complete)
})

test_that("bootstrap canary leaves unselected unknown entities actionable", {
  plan <- pd_dependency_plan(
    data.table::data.table(survey_id = c("s1", "s2")),
    data.table::data.table(
      survey_id = c("s1", "s2"), pip_id = c("p1", "p2")
    ), context = list(scope_id = "scope"), fingerprints = list()
  )
  canary <- pd_assert_bootstrap(plan, TRUE, "s1")
  expect_setequal(canary$actions$entity_id, c("s1", "p1"))
  resumed <- pd_assert_bootstrap(plan, TRUE, "s2")
  expect_setequal(resumed$actions$entity_id, c("s2", "p2"))
})

test_that("bootstrap rejection precedes execution preparation side effects", {
  plan <- pd_dependency_plan(
    data.table::data.table(survey_id = "s1"),
    context = list(scope_id = "scope"), fingerprints = list()
  )
  expect_error(pd_assert_bootstrap(plan), class = "pipdata_bootstrap_required")
})
