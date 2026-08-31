pd_dependency_plan <- function(inv, master = NULL, manifest = NULL,
                               context = pd_dependency_context(),
                               fingerprints = pd_code_fingerprints(),
                               force = FALSE, force_surveys = NULL,
                               snapshot = list()) {
  inv <- data.table::as.data.table(data.table::copy(inv))
  master <- data.table::as.data.table(data.table::copy(master %||% data.table::data.table()))
  if ((nrow(inv) && !"survey_id" %in% names(inv)) ||
      (nrow(master) &&
       !all(c("survey_id", "pip_id") %in% names(master)))) {
    rlang::abort(
      "Selected dependency entities are incomplete.",
      class = "pipdata_dependency_facts_invalid"
    )
  }
  invalid_inv <- nrow(inv) &&
    (anyNA(inv$survey_id) || any(!nzchar(inv$survey_id)))
  invalid_master <- nrow(master) &&
    (anyNA(master[, .(survey_id, pip_id)]) ||
     any(!nzchar(master$survey_id)) || any(!nzchar(master$pip_id)))
  if (invalid_inv || invalid_master) {
    rlang::abort(
      "Selected dependency entity identifiers must be complete.",
      class = "pipdata_dependency_facts_invalid"
    )
  }
  if (nrow(master) && anyDuplicated(master$pip_id)) {
    rlang::abort(
      "Selected PIP entities do not map one-to-one to surveys.",
      class = "pipdata_dependency_facts_invalid"
    )
  }
  clean_nodes <- if (nrow(inv)) {
    unique(inv[, .(
      stage = "clean", entity_id = survey_id, survey_id,
      pip_id = NA_character_
    )])
  } else {
    pd_empty_actions()[, action := NULL]
  }
  downstream_nodes <- if (nrow(master)) {
    data.table::rbindlist(list(
      master[, .(
        stage = "metadata", entity_id = pip_id, survey_id, pip_id
      )],
      master[, .(
        stage = "deflate", entity_id = pip_id, survey_id, pip_id
      )]
    ))
  } else {
    pd_empty_actions()[, action := NULL]
  }
  nodes <- data.table::rbindlist(
    list(clean_nodes, downstream_nodes), use.names = TRUE
  )
  data.table::setorder(nodes, stage, entity_id)
  mapping_differs <- function(mapped) {
    differs <- function(x, y) {
      xor(is.na(x), is.na(y)) |
        (!is.na(x) & !is.na(y) & x != y)
    }
    nrow(mapped) && any(
      differs(mapped$survey_id, mapped$i.survey_id) |
        differs(mapped$pip_id, mapped$i.pip_id)
    )
  }
  actions <- pd_empty_actions()
  reasons <- pd_empty_reasons()
  add <- function(stage, entity_id, survey_id, pip_id, action, reason,
                  input = NA_character_, old = NA_character_, new = NA_character_) {
    actions <<- data.table::rbindlist(list(actions, data.table::data.table(
      stage, entity_id, survey_id, pip_id, action)), use.names = TRUE)
    reasons <<- data.table::rbindlist(list(reasons, data.table::data.table(
      stage, entity_id, reason, input, old, new)), use.names = TRUE)
  }
  absent <- is.null(manifest) || inherits(manifest, "pipdata_manifest_absent")
  if (nrow(inv)) {
    for (survey_id in sort(unique(inv$survey_id))) {
      reason <- if (absent) "unknown_provenance" else if (!survey_id %in% master$survey_id) "new_entity" else NA_character_
      if (!is.na(reason)) add("clean", survey_id, survey_id, NA_character_,
                              if (reason == "new_entity") "create" else "rebuild", reason)
    }
  }
  if (nrow(master) && "pip_id" %in% names(master)) {
    for (pip_id in sort(unique(master$pip_id))) {
      current_id <- pip_id
      survey_id <- master[pip_id == current_id, survey_id][1L]
      if (absent) {
        add("metadata", pip_id, survey_id, pip_id, "refresh", "unknown_provenance")
        add("deflate", pip_id, survey_id, pip_id, "refresh", "unknown_provenance")
      }
    }
  }
  facts <- snapshot$facts %||% data.table::data.table()
  if (nrow(facts)) {
    facts <- data.table::as.data.table(data.table::copy(facts))
    required <- c("stage", "entity_id", "survey_id", "pip_id", "reason")
    duplicate_fields <- intersect(
      c(required, "input", "old", "new"), names(facts)
    )
    if (!all(required %in% names(facts)) ||
        anyDuplicated(facts[, ..duplicate_fields])) {
      rlang::abort("Planning facts are incomplete.",
                   class = "pipdata_dependency_facts_invalid")
    }
    for (i in seq_len(nrow(facts))) {
      action <- if (facts$stage[i] == "clean") "rebuild" else "refresh"
      add(facts$stage[i], facts$entity_id[i], facts$survey_id[i], facts$pip_id[i],
          action, facts$reason[i], facts$input[i] %||% NA_character_,
          facts$old[i] %||% NA_character_, facts$new[i] %||% NA_character_)
    }
  }
  forced <- unique(toupper(force_surveys %||% character()))
  if (force || length(forced)) {
    mapped_surveys <- forced
    if (length(forced) && nrow(master) &&
        all(c("pip_id", "survey_id") %in% names(master))) {
      mapped_surveys <- unique(c(
        mapped_surveys,
        toupper(master[toupper(pip_id) %in% forced, survey_id])
      ))
    }
    clean_entities <- unique(inv[, .(stage = "clean", entity_id = survey_id,
                                     survey_id, pip_id = NA_character_,
                                     action = "rebuild")])
    downstream <- if (nrow(master) && "pip_id" %in% names(master)) {
      data.table::rbindlist(list(
        unique(master[, .(stage = "metadata", entity_id = pip_id, survey_id,
                          pip_id, action = "refresh")]),
        unique(master[, .(stage = "deflate", entity_id = pip_id, survey_id,
                          pip_id, action = "refresh")])
      ))
    } else pd_empty_actions()
    forced_actions <- data.table::rbindlist(list(clean_entities, downstream), fill = TRUE)
    if (!force) {
      forced_actions <- forced_actions[
        toupper(survey_id) %in% mapped_surveys | toupper(pip_id) %in% forced
      ]
      known <- unique(c(toupper(inv$survey_id), toupper(master$survey_id),
                        toupper(master$pip_id)))
      unknown <- setdiff(forced, known)
      if (length(unknown)) {
        rlang::warn(
          paste("Unknown force_surveys identifiers:", paste(unknown, collapse = ", ")),
          class = "pipdata_force_surveys_unknown",
          unknown_identifiers = unknown
        )
      }
    }
    actions <- data.table::rbindlist(list(actions, forced_actions), fill = TRUE)
    forced_reasons <- forced_actions[, .(
      stage, entity_id, reason = "forced", input = NA_character_,
      old = NA_character_, new = NA_character_
    )]
    reasons <- data.table::rbindlist(list(reasons, forced_reasons), fill = TRUE)
  }
  current <- data.table::as.data.table(data.table::copy(
    snapshot$current %||% data.table::data.table()
  ))
  if (nrow(current)) {
    required <- c("stage", "entity_id", "survey_id", "pip_id")
    if (!all(required %in% names(current)) ||
        anyDuplicated(current[, .(stage, entity_id)])) {
      rlang::abort(
        "Current dependency facts contain duplicate or incomplete nodes.",
        class = "pipdata_dependency_facts_invalid"
      )
    }
    current_keys <- current[, .(stage, entity_id, survey_id, pip_id)]
    unmatched <- current_keys[!nodes, on = c("stage", "entity_id")]
    mapped <- nodes[current_keys, on = c("stage", "entity_id")]
    if (nrow(unmatched) || mapping_differs(mapped)) {
      rlang::abort(
        "Current dependency facts do not match selected entities.",
        class = "pipdata_dependency_facts_invalid"
      )
    }
    details <- setdiff(
      names(current), c("stage", "entity_id", "survey_id", "pip_id")
    )
    if (length(details)) {
      nodes <- current[
        nodes,
        on = c("stage", "entity_id"),
        c(
          list(
            stage = i.stage, entity_id = i.entity_id,
            survey_id = i.survey_id, pip_id = i.pip_id
          ),
          mget(details)
        )
      ]
    }
  }
  if (nrow(actions)) {
    action_keys <- actions[, .(stage, entity_id, survey_id, pip_id)]
    unmatched <- action_keys[!nodes, on = c("stage", "entity_id")]
    mapped <- nodes[action_keys, on = c("stage", "entity_id")]
    if (nrow(unmatched) || mapping_differs(mapped)) {
      rlang::abort(
        "Planning facts do not match selected entities.",
        class = "pipdata_dependency_facts_invalid"
      )
    }
    actions[, action_rank := match(
      action, c("create", "rebuild", "refresh", "none")
    )]
    data.table::setorder(actions, stage, entity_id, action_rank)
    actions <- actions[, .SD[1L], by = .(stage, entity_id)]
    actions[, action_rank := NULL]
  }
  nodes[, action := "none"]
  if (nrow(actions)) {
    nodes[actions, on = c("stage", "entity_id"), action := i.action]
  }
  data.table::setcolorder(
    nodes,
    c("stage", "entity_id", "survey_id", "pip_id", "action")
  )
  actions <- nodes
  data.table::setorder(actions, stage, entity_id)
  reasons <- unique(reasons)
  data.table::setorder(reasons, stage, entity_id, reason, input)
  plan <- structure(list(context = context, actions = actions, reasons = reasons,
                         snapshot = c(snapshot, list(fingerprints = fingerprints))),
                    class = "pip_dependency_plan")
  pd_validate_plan(plan)
  plan
}

pd_assert_bootstrap <- function(plan, bootstrap = FALSE, bootstrap_entities = NULL) {
  unknown <- plan$reasons[reason == "unknown_provenance"]
  if (nrow(unknown) && !isTRUE(bootstrap)) {
    rlang::abort("Unknown legacy provenance requires bootstrap = TRUE.",
                 class = "pipdata_bootstrap_required")
  }
  if (isTRUE(bootstrap) && !is.null(bootstrap_entities)) {
    keep <- plan$actions$entity_id %in% bootstrap_entities |
      plan$actions$survey_id %in% bootstrap_entities |
      plan$actions$pip_id %in% bootstrap_entities
    plan$actions <- plan$actions[keep]
    plan$reasons <- plan$reasons[
      paste(stage, entity_id) %in% paste(plan$actions$stage, plan$actions$entity_id)]
  }
  plan
}

pd_plan_node_states <- function(plan, blocked = NULL) {
  pd_validate_plan(plan)
  if (!nrow(plan$actions)) {
    return(pd_empty_plan_node_states())
  }
  nodes <- data.table::copy(plan$actions)
  nodes[, `:=`(
    state = data.table::fifelse(action == "none", "current", "stale"),
    scheduling_state = data.table::fifelse(
      action == "none", "cached", "runnable"
    ),
    wave_state = "accepted"
  )]
  forced <- unique(plan$reasons[
    reason == "forced", .(stage, entity_id)
  ])
  if (nrow(forced)) {
    nodes[forced, on = c("stage", "entity_id"), state := "forced"]
  }
  actionable_clean <- unique(nodes[
    stage == "clean" & action != "none", survey_id
  ])
  if (length(actionable_clean)) {
    nodes[
      stage != "clean" & survey_id %in% actionable_clean,
      wave_state := "forecast"
    ]
  }
  if (!is.null(blocked)) {
    blocked <- data.table::as.data.table(data.table::copy(blocked))
    required <- c("stage", "entity_id")
    if (!all(required %in% names(blocked)) ||
        anyDuplicated(blocked[, ..required])) {
      rlang::abort(
        "Blocked dependency nodes are invalid.",
        class = "pipdata_dependency_plan_invalid"
      )
    }
    blocked_nodes <- nodes[blocked, on = required]
    if (nrow(blocked_nodes) != nrow(blocked) ||
        anyNA(blocked_nodes$action) || any(blocked_nodes$action == "none")) {
      rlang::abort(
        "Only selected runnable nodes can become blocked.",
        class = "pipdata_dependency_plan_invalid"
      )
    }
    nodes[blocked, on = required, scheduling_state := "blocked"]
  }
  data.table::setorder(nodes, stage, entity_id)
  return(nodes)
}
