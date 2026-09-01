test_that("planner covers the controlled invalidation matrix deterministically", {
  reasons <- setdiff(.PD_REASON_CODES, "unknown_provenance")
  stages <- rep(c("clean", "metadata", "deflate"), length.out = length(reasons))
  entity_ids <- paste0("e", seq_along(reasons))
  survey_ids <- ifelse(stages == "clean", entity_ids, "s")
  pip_ids <- ifelse(stages == "clean", NA_character_, entity_ids)
  facts <- data.table::data.table(
    stage = stages, entity_id = entity_ids, survey_id = survey_ids,
    pip_id = pip_ids, reason = reasons
  )
  inv <- data.table::data.table(
    survey_id = unique(facts[stage == "clean", survey_id])
  )
  master <- facts[stage != "clean", .(survey_id, pip_id)]
  manifest <- pd_empty_manifest(list(scope_id = "scope"))
  plan <- pd_dependency_plan(inv, master, manifest,
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

test_that("planner represents every selected current node exactly once", {
  inv <- data.table::data.table(survey_id = c("s2", "s1"))
  master <- data.table::data.table(
    survey_id = c("s2", "s1"),
    pip_id = c("p2", "p1")
  )
  current <- data.table::rbindlist(list(
    data.table::data.table(
      stage = "clean", entity_id = inv$survey_id,
      survey_id = inv$survey_id, pip_id = NA_character_
    ),
    data.table::data.table(
      stage = "metadata", entity_id = master$pip_id,
      survey_id = master$survey_id, pip_id = master$pip_id
    ),
    data.table::data.table(
      stage = "deflate", entity_id = master$pip_id,
      survey_id = master$survey_id, pip_id = master$pip_id
    )
  ))

  plan <- pd_dependency_plan(
    inv,
    master,
    pd_empty_manifest(list(scope_id = "scope")),
    context = list(scope_id = "scope"),
    fingerprints = list(),
    snapshot = list(current = current)
  )
  states <- pd_plan_node_states(plan)

  expect_named(plan, c("context", "actions", "reasons", "snapshot"))
  expect_identical(nrow(plan$actions), 6L)
  expect_identical(anyDuplicated(plan$actions[, .(stage, entity_id)]), 0L)
  expect_true(all(plan$actions$action == "none"))
  expect_identical(nrow(plan$reasons), 0L)
  expect_true(all(states$state == "current"))
  expect_true(all(states$scheduling_state == "cached"))
  expect_identical(
    plan$actions,
    data.table::copy(plan$actions)[order(stage, entity_id)]
  )
})

test_that("planner mixes cached and stale nodes from one authoritative plan", {
  inv <- data.table::data.table(survey_id = "s1")
  master <- data.table::data.table(survey_id = "s1", pip_id = "p1")
  current <- data.table::data.table(
    stage = c("clean", "metadata", "deflate"),
    entity_id = c("s1", "p1", "p1"),
    survey_id = "s1",
    pip_id = c(NA_character_, "p1", "p1"),
    input_hash = c("clean-input", "metadata-input", "deflate-input")
  )
  facts <- data.table::data.table(
    stage = "metadata", entity_id = "p1", survey_id = "s1", pip_id = "p1",
    reason = "aux_cpi_changed", input = "aux_cpi", old = "old", new = "new"
  )

  plan <- pd_dependency_plan(
    inv,
    master,
    pd_empty_manifest(list(scope_id = "scope")),
    context = list(scope_id = "scope"),
    fingerprints = list(),
    snapshot = list(current = current, facts = facts)
  )
  states <- pd_plan_node_states(plan)
  blocked <- pd_plan_node_states(
    plan,
    blocked = data.table::data.table(stage = "metadata", entity_id = "p1")
  )

  expect_identical(plan$actions$action, c("none", "none", "refresh"))
  expect_identical(plan$reasons$reason, "aux_cpi_changed")
  expect_identical(states$state, c("current", "current", "stale"))
  expect_identical(
    states$scheduling_state,
    c("cached", "cached", "runnable")
  )
  expect_identical(
    blocked[stage == "metadata", scheduling_state],
    "blocked"
  )
})

test_that("targeted force makes the selected current chain runnable", {
  inv <- data.table::data.table(survey_id = c("s1", "s2"))
  master <- data.table::data.table(
    survey_id = c("s1", "s2"), pip_id = c("p1", "p2")
  )
  current <- data.table::rbindlist(list(
    inv[, .(
      stage = "clean", entity_id = survey_id, survey_id,
      pip_id = NA_character_
    )],
    master[, .(
      stage = "metadata", entity_id = pip_id, survey_id, pip_id
    )],
    master[, .(
      stage = "deflate", entity_id = pip_id, survey_id, pip_id
    )]
  ))

  plan <- pd_dependency_plan(
    inv,
    master,
    pd_empty_manifest(list(scope_id = "scope")),
    context = list(scope_id = "scope"),
    fingerprints = list(),
    force_surveys = "p1",
    snapshot = list(current = current)
  )
  states <- pd_plan_node_states(plan)
  selected <- states[survey_id == "s1"]
  unselected <- states[survey_id == "s2"]

  expect_true(all(selected$state == "forced"))
  expect_true(all(selected$scheduling_state == "runnable"))
  expect_true(all(selected$action != "none"))
  expect_identical(selected[stage == "clean", wave_state], "accepted")
  expect_true(all(selected[stage != "clean", wave_state] == "forecast"))
  expect_true(all(unselected$state == "current"))
  expect_true(all(unselected$scheduling_state == "cached"))
  expect_true(all(unselected$wave_state == "accepted"))
  expect_identical(
    plan$reasons[reason == "forced", .N, by = .(stage, entity_id)]$N,
    rep(1L, 3L)
  )
})

test_that("targeted force is additive to unrelated ordinary invalidation", {
  inv <- data.table::data.table(survey_id = c("s1", "s2"))
  master <- data.table::data.table(
    survey_id = c("s1", "s2"), pip_id = c("p1", "p2")
  )
  current <- data.table::rbindlist(list(
    inv[, .(
      stage = "clean", entity_id = survey_id, survey_id,
      pip_id = NA_character_
    )],
    master[, .(
      stage = "metadata", entity_id = pip_id, survey_id, pip_id
    )],
    master[, .(
      stage = "deflate", entity_id = pip_id, survey_id, pip_id
    )]
  ))
  facts <- data.table::data.table(
    stage = "metadata", entity_id = "p2", survey_id = "s2", pip_id = "p2",
    reason = "aux_cpi_changed", input = "aux_cpi", old = "old", new = "new"
  )

  plan <- pd_dependency_plan(
    inv,
    master,
    pd_empty_manifest(list(scope_id = "scope")),
    context = list(scope_id = "scope"),
    fingerprints = list(),
    force_surveys = "s1",
    snapshot = list(current = current, facts = facts)
  )

  expect_true(all(plan$actions[survey_id == "s1", action != "none"]))
  expect_identical(
    plan$actions[stage == "metadata" & entity_id == "p2", action],
    "refresh"
  )
  expect_true(all(plan$reasons[reason == "forced", entity_id] %in%
                    c("s1", "p1")))
  expect_identical(
    plan$reasons[stage == "metadata" & entity_id == "p2", reason],
    "aux_cpi_changed"
  )
  expect_false(any(plan$reasons[entity_id == "p2", reason == "forced"]))
})

test_that("zero selection returns an empty complete plan", {
  plan <- pd_dependency_plan(
    data.table::data.table(survey_id = character()),
    data.table::data.table(survey_id = character(), pip_id = character()),
    pd_empty_manifest(list(scope_id = "scope")),
    context = list(scope_id = "scope"),
    fingerprints = list(),
    snapshot = list(current = data.table::data.table())
  )

  expect_identical(plan$actions, pd_empty_actions())
  expect_identical(plan$reasons, pd_empty_reasons())
  expect_identical(pd_plan_node_states(plan), pd_empty_plan_node_states())
})

test_that("plan validation rejects invalid complete-node relationships", {
  context <- list(scope_id = "scope")
  action <- data.table::data.table(
    stage = "metadata", entity_id = "p1", survey_id = "s1",
    pip_id = "p1", action = "none"
  )
  reason <- data.table::data.table(
    stage = "metadata", entity_id = "p1", reason = "aux_cpi_changed",
    input = "aux_cpi", old = "old", new = "new"
  )
  invalid_none <- structure(
    list(
      context = context,
      actions = action,
      reasons = reason,
      snapshot = list()
    ),
    class = "pip_dependency_plan"
  )
  invalid_mapping <- invalid_none
  invalid_mapping$actions[, `:=`(
    entity_id = "different", action = "refresh"
  )]

  expect_error(
    pd_validate_plan(invalid_none),
    class = "pipdata_dependency_plan_invalid"
  )
  expect_error(
    pd_validate_plan(invalid_mapping),
    class = "pipdata_dependency_plan_invalid"
  )
})

test_that("planner rejects duplicate current nodes and duplicate facts", {
  inv <- data.table::data.table(survey_id = "s1")
  current <- data.table::data.table(
    stage = "clean", entity_id = "s1", survey_id = "s1",
    pip_id = NA_character_
  )
  fact <- data.table::data.table(
    stage = "clean", entity_id = "s1", survey_id = "s1",
    pip_id = NA_character_, reason = "dlw_changed",
    input = "dlw", old = "old", new = "new"
  )
  args <- list(
    inv = inv,
    master = data.table::data.table(),
    manifest = pd_empty_manifest(list(scope_id = "scope")),
    context = list(scope_id = "scope"),
    fingerprints = list()
  )

  expect_error(
    do.call(
      pd_dependency_plan,
      c(args, list(snapshot = list(current = rbind(current, current))))
    ),
    class = "pipdata_dependency_facts_invalid"
  )
  expect_error(
    do.call(
      pd_dependency_plan,
      c(args, list(snapshot = list(current = current, facts = rbind(fact, fact))))
    ),
    class = "pipdata_dependency_facts_invalid"
  )
})
