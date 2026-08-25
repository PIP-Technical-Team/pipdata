test_that("planner covers the controlled invalidation matrix deterministically", {
  reasons <- setdiff(.PD_REASON_CODES, "unknown_provenance")
  facts <- data.table::data.table(
    stage = rep(c("clean", "metadata", "deflate"), length.out = length(reasons)),
    entity_id = paste0("e", seq_along(reasons)), survey_id = "s",
    pip_id = paste0("p", seq_along(reasons)), reason = reasons
  )
  inv <- data.table::data.table(survey_id = "s")
  manifest <- pd_empty_manifest(list(scope_id = "scope"))
  plan <- pd_dependency_plan(inv, data.table::data.table(), manifest,
                             context = list(scope_id = "scope"),
                             fingerprints = list(), snapshot = list(facts = facts))
  expect_setequal(plan$reasons$reason, reasons)
  expect_identical(plan$actions, data.table::copy(plan$actions)[order(stage, entity_id)])
})

test_that("unknown provenance is guarded before execution", {
  plan <- pd_dependency_plan(data.table::data.table(survey_id = "s"),
                             context = list(scope_id = "scope"), fingerprints = list())
  expect_error(pd_assert_bootstrap(plan), class = "pipdata_bootstrap_required")
})

test_that("force_surveys reverse maps pip IDs case-insensitively", {
  inv <- data.table::data.table(survey_id = "COL_2020_GEIH")
  master <- data.table::data.table(
    survey_id = "COL_2020_GEIH", pip_id = "COL_2020_GEI_INC"
  )
  plan <- pd_dependency_plan(
    inv, master, pd_empty_manifest(list(scope_id = "scope")),
    context = list(scope_id = "scope"), fingerprints = list(),
    force_surveys = "col_2020_gei_inc"
  )
  expect_true(any(plan$actions$stage == "clean" &
                    plan$actions$survey_id == "COL_2020_GEIH"))
})

test_that("unknown force_surveys identifiers warn", {
  expect_warning(
    pd_dependency_plan(
      data.table::data.table(survey_id = "s1"),
      data.table::data.table(survey_id = "s1", pip_id = "p1"),
      pd_empty_manifest(list(scope_id = "scope")),
      context = list(scope_id = "scope"), fingerprints = list(),
      force_surveys = "missing"
    ), class = "pipdata_force_surveys_unknown"
  )
})
