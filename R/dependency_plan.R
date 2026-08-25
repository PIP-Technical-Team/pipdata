pd_dependency_plan <- function(inv, master = NULL, manifest = NULL,
                               context = pd_dependency_context(),
                               fingerprints = pd_code_fingerprints(),
                               force = FALSE, force_surveys = NULL,
                               snapshot = list()) {
  inv <- data.table::as.data.table(data.table::copy(inv))
  master <- data.table::as.data.table(data.table::copy(master %||% data.table::data.table()))
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
    if (!all(required %in% names(facts))) {
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
  data.table::setorder(actions, stage, entity_id)
  actions <- unique(actions, by = c("stage", "entity_id"))
  data.table::setorder(reasons, stage, entity_id, reason, input)
  current <- data.table::as.data.table(snapshot$current %||% data.table::data.table())
  if (nrow(current)) {
    actions <- current[actions, on = c("stage", "entity_id")]
  }
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
