.filter_completed_dlw_validation_inventory <- function(inv) {
  if (!is.data.frame(inv)) {
    .abort_dlw_validation_inventory_schema(
      "The completed validation inventory must be tabular."
    )
  }
  completed <- .normalize_dlw_validation_inventory(
    inv,
    allow_schema_light_empty = TRUE
  )
  completed <- unique(completed)
  if (anyDuplicated(completed$survey_id)) {
    .abort_dlw_validation_inventory_schema(
      "Completed validation inventory contains duplicate survey IDs."
    )
  }
  completed[]
}

pd_freeze_aux_snapshot <- function(measures, verbose = FALSE) {
  catalog <- data.table::as.data.table(stamp::st_catalog_query(alias = "aux"))
  if (!nrow(catalog)) {
    rlang::abort("Auxiliary catalog is empty.",
                 class = "pipdata_dependency_aux_missing")
  }
  catalog[, measure := tolower(fs::path_ext_remove(fs::path_file(path)))]
  data.table::setorder(catalog, measure, -created_at, path)
  rows <- catalog[measure %in% measures, .SD[1L], by = measure]
  if (!setequal(rows$measure, measures)) {
    rlang::abort("One or more requested auxiliary artifacts are absent.",
                 class = "pipdata_dependency_aux_missing")
  }
  objects <- lapply(seq_len(nrow(rows)), function(i) {
    object <- pipload::load_aux_data(rows$measure[i],
                                     version = rows$version_id[i],
                                     verbose = verbose)
    if (!identical(stamp::st_hash_obj(object), rows$content_hash[i])) {
      rlang::abort("Pinned auxiliary artifact failed hash verification.",
                   class = "pipdata_dependency_aux_drift")
    }
    object
  })
  names(objects) <- rows$measure
  list(catalog = rows, objects = objects)
}

pd_build_dependency_snapshot <- function(
  inv,
  master,
  context,
  measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  metadata_measures = c("cpi", "ppp", "pop"),
  verbose = FALSE,
  aux = NULL,
  catalogs = NULL,
  fingerprints = NULL
) {
  if (is.null(aux)) {
    aux <- pd_freeze_aux_snapshot(measures, verbose)
  }
  if (is.null(catalogs)) {
    aliases <- c("pip", "pip_meta", "pip_deflated", "pip_inv")
    catalogs <- lapply(aliases, function(alias) {
      data.table::as.data.table(stamp::st_catalog_query(alias = alias))
    })
    names(catalogs) <- aliases
  }
  if (is.null(fingerprints)) {
    fingerprints <- pd_code_fingerprints()
  }
  aux$indexes <- pd_build_aux_projection_indexes(aux$objects)
  snapshot <- list(
    context = context,
    inventory = data.table::copy(data.table::as.data.table(inv)),
    master = data.table::copy(data.table::as.data.table(master)),
    measures = unique(tolower(measures)),
    metadata_measures = unique(tolower(metadata_measures)),
    aux = aux,
    catalogs = catalogs,
    fingerprints = fingerprints,
    captured_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  snapshot$current <- pd_snapshot_current(snapshot)
  return(snapshot)
}

pd_canonical_snapshot_value <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.environment(x) || inherits(x, "externalptr")) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    return(pd_canonical_snapshot_table(x))
  }
  if (is.list(x)) {
    if (!is.null(names(x))) {
      x <- x[order(names(x))]
    }
    return(lapply(x, pd_canonical_snapshot_value))
  }
  if (!is.null(names(x))) {
    x <- x[order(names(x))]
  }
  return(x)
}

pd_canonical_snapshot_table <- function(x) {
  dt <- data.table::as.data.table(data.table::copy(x))
  if (!ncol(dt)) {
    return(list(columns = character(), data = list()))
  }
  data.table::setcolorder(dt, sort(names(dt)))
  for (column in names(dt)) {
    values <- dt[[column]]
    if (inherits(values, "POSIXt")) {
      data.table::set(
        dt,
        j = column,
        value = format(values, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
      )
    } else if (is.factor(values)) {
      data.table::set(dt, j = column, value = as.character(values))
    } else if (is.list(values)) {
      hashes <- vapply(values, function(value) {
        pd_hash_object(pd_canonical_snapshot_value(value))
      }, character(1))
      data.table::set(dt, j = column, value = hashes)
    }
  }
  if (nrow(dt)) {
    data.table::setorderv(dt, names(dt), na.last = TRUE)
  }
  return(list(columns = names(dt), data = as.list(dt)))
}

pd_snapshot_manifest_identity <- function(manifest, context) {
  if (inherits(manifest, "pipdata_manifest_absent")) {
    return(list(absent = TRUE, scope_id = context$scope_id))
  }
  identity <- attr(manifest, "manifest_identity")
  if (!is.null(identity)) {
    return(pd_canonical_snapshot_value(identity))
  }
  tables <- lapply(manifest, function(value) {
    if (is.data.frame(value)) {
      return(pd_canonical_snapshot_table(value))
    }
    pd_canonical_snapshot_value(value)
  })
  return(list(in_memory_manifest = pd_hash_object(tables)))
}

pd_snapshot_identity <- function(snapshot, manifest) {
  current <- data.table::as.data.table(
    data.table::copy(snapshot$current %||% data.table::data.table())
  )
  aux_columns <- intersect(c("stage", "entity_id", "aux_hashes"), names(current))
  aux_hashes <- if (length(aux_columns)) {
    current[, ..aux_columns]
  } else {
    data.table::data.table()
  }
  current_columns <- names(current)[!vapply(current, is.list, logical(1))]
  current_facts <- current[, ..current_columns]
  catalogs <- snapshot$catalogs %||% list()
  if (length(catalogs)) {
    catalog_names <- names(catalogs) %||% as.character(seq_along(catalogs))
    catalogs <- catalogs[order(catalog_names)]
  }
  canonical_catalogs <- lapply(catalogs, pd_canonical_snapshot_table)
  fingerprints <- pd_canonical_snapshot_value(
    snapshot$fingerprints %||% list()
  )
  payload <- list(
    manifest = pd_snapshot_manifest_identity(manifest, snapshot$context),
    aux_catalog = pd_canonical_snapshot_table(
      snapshot$aux$catalog %||% data.table::data.table()
    ),
    catalogs = canonical_catalogs,
    inventory = pd_canonical_snapshot_table(snapshot$inventory),
    master = pd_canonical_snapshot_table(snapshot$master),
    auxiliary_components = pd_canonical_snapshot_table(aux_hashes),
    current = pd_canonical_snapshot_table(current_facts),
    facts = pd_canonical_snapshot_table(
      snapshot$facts %||% data.table::data.table()
    ),
    fingerprints = fingerprints
  )
  return(pd_hash_object(payload))
}

pd_assert_no_removed_surveys <- function(inv, master) {
  inv <- data.table::as.data.table(data.table::copy(inv))
  master <- data.table::as.data.table(data.table::copy(master))
  if (!nrow(master)) {
    return(invisible(NULL))
  }
  if (!"survey_id" %in% names(inv) || !"survey_id" %in% names(master)) {
    rlang::abort(
      "Survey removal detection requires survey IDs in both inventories.",
      class = "pipdata_dependency_facts_invalid"
    )
  }
  completed_surveys <- sort(unique(inv$survey_id[!is.na(inv$survey_id)]))
  prior_surveys <- sort(unique(master$survey_id[!is.na(master$survey_id)]))
  removed_surveys <- setdiff(prior_surveys, completed_surveys)
  if (length(removed_surveys)) {
    rlang::abort(
      paste(
        "Completed validation no longer contains whole surveys:",
        paste(removed_surveys, collapse = ", ")
      ),
      class = "pipdata_upstream_survey_removed",
      removed_surveys = removed_surveys
    )
  }
  return(invisible(NULL))
}

pd_prepare_dependency_facts <- function(
  inv,
  master,
  context = pd_dependency_context(),
  manifest = NULL,
  measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  metadata_measures = c("cpi", "ppp", "pop"),
  verbose = FALSE,
  aux = NULL,
  catalogs = NULL,
  fingerprints = NULL,
  check_removed_surveys = TRUE
) {
  if (!is.list(context) || !is.character(context$scope_id) ||
      length(context$scope_id) != 1L || is.na(context$scope_id) ||
      !nzchar(context$scope_id)) {
    rlang::abort(
      "A valid dependency context is required.",
      class = "pipdata_dependency_context_error"
    )
  }
  if (!is.data.frame(master)) {
    rlang::abort(
      "The master inventory must be tabular.",
      class = "pipdata_dependency_facts_invalid"
    )
  }
  inv <- .filter_completed_dlw_validation_inventory(inv)
  master <- data.table::as.data.table(data.table::copy(master))
  if (isTRUE(check_removed_surveys)) {
    pd_assert_no_removed_surveys(inv, master)
  }
  if (is.null(manifest)) {
    manifest <- pd_manifest_read(context, allow_absent = TRUE)
  }
  if (!inherits(manifest, "pipdata_manifest_absent")) {
    pd_validate_manifest(manifest, context)
  }
  snapshot <- pd_build_dependency_snapshot(
    inv = inv,
    master = master,
    context = context,
    measures = measures,
    metadata_measures = metadata_measures,
    verbose = verbose,
    aux = aux,
    catalogs = catalogs,
    fingerprints = fingerprints
  )
  snapshot$facts <- pd_snapshot_facts(snapshot, manifest)
  snapshot$snapshot_identity <- pd_snapshot_identity(snapshot, manifest)
  return(list(context = context, manifest = manifest, snapshot = snapshot))
}

pd_refresh_execution_facts <- function(
  execution,
  master,
  force = FALSE,
  force_surveys = NULL,
  bootstrap = FALSE,
  bootstrap_entities = NULL,
  verbose = FALSE,
  strict_bootstrap_selectors = FALSE
) {
  pd_assert_execution_refresh_fence(execution)
  snapshot <- pd_build_dependency_snapshot(
    inv = execution$snapshot$inventory,
    master = master,
    context = execution$context,
    measures = execution$snapshot$measures,
    metadata_measures = execution$snapshot$metadata_measures,
    verbose = verbose,
    aux = execution$snapshot$aux,
    fingerprints = execution$snapshot$fingerprints
  )
  snapshot$facts <- pd_snapshot_facts(snapshot, execution$manifest)
  snapshot$snapshot_identity <- pd_snapshot_identity(
    snapshot, execution$manifest
  )
  plan <- pd_dependency_plan(
    snapshot$inventory,
    snapshot$master,
    execution$manifest,
    execution$context,
    snapshot$fingerprints,
    force,
    force_surveys,
    snapshot = snapshot
  )
  execution$snapshot <- snapshot
  execution$plan <- pd_assert_bootstrap(
    plan,
    bootstrap,
    bootstrap_entities,
    strict_selectors = strict_bootstrap_selectors
  )
  return(execution)
}

pd_catalog_receipt <- function(catalog, artifact) {
  catalog <- data.table::as.data.table(catalog)
  if (!nrow(catalog) || !all(c("path", "version_id", "content_hash") %in% names(catalog))) {
    return(list(version_id = NA_character_, content_hash = NA_character_, path = NA_character_))
  }
  rows <- catalog[toupper(fs::path_ext_remove(fs::path_file(path))) == toupper(artifact)]
  if (!nrow(rows)) return(list(version_id = NA_character_, content_hash = NA_character_, path = NA_character_))
  if ("created_at" %in% names(rows)) data.table::setorder(rows, -created_at, path)
  as.list(rows[1L, .(version_id, content_hash, path)])
}

pd_deflate_current_receipt <- function(receipt, master_row) {
  current <- isTRUE(master_row$deflated) && pd_receipt_matches_pointer(
    receipt, master_row, "version_id_deflated", "content_hash_deflated"
  )
  if (current) return(receipt)
  pd_missing_catalog_receipt()
}

pd_missing_catalog_receipt <- function() {
  list(
    version_id = NA_character_, content_hash = NA_character_,
    path = NA_character_
  )
}

pd_receipt_matches_pointer <- function(receipt, master_row, version_field,
                                       hash_field) {
  row <- as.list(master_row)
  required <- c(version_field, hash_field)
  all(required %in% names(row)) &&
    all(vapply(row[required], function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) &&
        nzchar(value)
    }, logical(1L))) &&
    identical(receipt$version_id, row[[version_field]]) &&
    identical(receipt$content_hash, row[[hash_field]])
}

pd_metadata_current_receipt <- function(receipt, master_row) {
  if (pd_receipt_matches_pointer(
    receipt, master_row, "version_id_metadata", "content_hash_metadata"
  )) {
    return(receipt)
  }
  pd_missing_catalog_receipt()
}

pd_clean_current_receipts <- function(receipts, master, survey_id,
                                      expected_pip_ids) {
  receipts <- data.table::as.data.table(data.table::copy(receipts))
  master <- data.table::as.data.table(data.table::copy(master))
  expected <- sort(as.character(expected_pip_ids))
  selected_survey <- survey_id
  rows <- master[
    master$survey_id == selected_survey & master$pip_id %in% expected
  ]
  if (!length(expected) || nrow(rows) != length(expected) ||
      anyDuplicated(rows$pip_id) ||
      !identical(sort(rows$pip_id), expected) ||
      nrow(receipts) != length(expected) || anyDuplicated(receipts$pip_id) ||
      !identical(sort(receipts$pip_id), expected)) {
    return(FALSE)
  }
  rows <- rows[match(expected, pip_id)]
  receipts <- receipts[match(expected, pip_id)]
  return(all(vapply(seq_along(expected), function(i) {
    pd_receipt_matches_pointer(
      as.list(receipts[i]), as.list(rows[i]),
      "version_id_data", "content_hash_data"
    )
  }, logical(1L))))
}

pd_entity_aux_hash <- function(snapshot, row, measures) {
  projections <- lapply(measures, function(measure) {
    aux <- snapshot$aux$objects[[measure]]
    country <- row$country_code %||% row$country %||% substr(row$pip_id %||% "", 1L, 3L)
    year <- row$year %||% suppressWarnings(as.integer(strsplit(row$pip_id %||% "", "_")[[1L]][2L]))
    tryCatch(pd_select_aux(aux, measure, country, year,
                           row$survey_acronym %||% NULL,
                           row$reporting_level %||% NULL)$hash,
             error = function(e) pd_hash_object(aux))
  })
  stats::setNames(unlist(projections), measures)
}

pd_entity_aux_projection <- function(snapshot, row, measures) {
  projections <- lapply(measures, function(measure) {
    aux <- snapshot$aux$objects[[measure]]
    country <- row$country_code %||% row$country %||%
      substr(row$pip_id %||% "", 1L, 3L)
    year <- row$year %||%
      suppressWarnings(as.integer(strsplit(row$pip_id %||% "", "_")[[1L]][2L]))
    pd_select_aux(aux, measure, country, year,
                  row$survey_acronym %||% NULL,
                  row$reporting_level %||% NULL)$data
  })
  stats::setNames(projections, measures)
}

pd_snapshot_current <- function(snapshot) {
  rows <- list()
  fingerprints <- snapshot$fingerprints$summary
  code_hash <- function(stage) {
    fingerprints$hash[match(stage, fingerprints$stage)]
  }
  requested <- snapshot$measures %||%
    c("pfw", "cpi", "ppp", "pop", "gdp", "pce")
  inv <- snapshot$inventory
  for (i in seq_len(nrow(inv))) {
    row <- inv[i]
    entity <- row$survey_id[[1L]]
    state <- pd_entity_input_state(snapshot, row, "clean", "pfw")
    expected <- state$expected_pip_ids
    receipts <- data.table::rbindlist(lapply(expected, function(id) {
      receipt <- pd_catalog_receipt(snapshot$catalogs$pip, id)
      data.table::data.table(
        pip_id = id, alias = "pip", artifact = id, path = receipt$path,
        version_id = receipt$version_id, content_hash = receipt$content_hash,
        success = !anyNA(unlist(receipt)) &&
          all(nzchar(unlist(receipt)))
      )
    }), fill = TRUE)
    receipt_set <- tryCatch(
      pd_clean_receipt_set(receipts, expected),
      error = function(e) list(
        receipts = receipts, output_version_id = NA_character_,
        output_hash = NA_character_
      )
    )
    if (!pd_clean_current_receipts(
      receipts, snapshot$master, entity, expected
    )) {
      receipt_set$output_version_id <- NA_character_
      receipt_set$output_hash <- NA_character_
    }
    rows[[length(rows) + 1L]] <- data.table::data.table(
      stage = "clean", entity_id = entity, survey_id = entity,
      pip_id = NA_character_, input_hash = state$input_hash,
      legacy_input_hash = state$legacy_input_hash,
      legacy_input_version = state$legacy_input_version,
      code_hash = code_hash("clean"),
      output_version_id = receipt_set$output_version_id,
      output_hash = receipt_set$output_hash,
      expected_outputs = list(expected), expected_pip_ids = list(expected),
      output_receipts = list(receipt_set$receipts),
      input_rows = list(state$input_rows)
    )
  }
  clean_current <- if (length(rows)) {
    data.table::rbindlist(rows, fill = TRUE)
  } else {
    data.table::data.table()
  }
  master <- snapshot$master
  for (i in seq_len(nrow(master))) {
    row <- master[i]
    for (stage in c("metadata", "deflate")) {
      selected_stage <- stage
      row_list <- as.list(row)
      measures <- if (selected_stage == "metadata") {
        intersect(c("cpi", "ppp", "pop", "gdp", "pce"), requested)
      } else {
        intersect(c("cpi", "ppp", "pop"), requested)
      }
      upstream_fields <- if (selected_stage == "metadata") {
        c("version_id_data", "content_hash_data")
      } else {
        c(
          "version_id_data", "content_hash_data",
          "version_id_metadata", "content_hash_metadata"
        )
      }
      exact_upstream <- all(upstream_fields %in% names(row)) &&
        all(vapply(upstream_fields, function(field) {
          value <- row[[field]][[1L]]
          is.character(value) && !is.na(value) && nzchar(value)
        }, logical(1)))
      accepted_outputs <- if (nrow(clean_current)) {
        clean_current[
          stage == "clean" & survey_id == row_list$survey_id,
          expected_pip_ids
        ]
      } else {
        list()
      }
      removed_forecast <- length(accepted_outputs) == 1L &&
        !row_list$pip_id %in% accepted_outputs[[1L]]
      state <- if (exact_upstream && !removed_forecast) {
        pd_entity_input_state(snapshot, row, selected_stage, measures)
      } else {
        list(
          input_hash = NA_character_, legacy_input_hash = NA_character_,
          legacy_input_version = NA_character_, aux_projection = list(),
          input_rows = data.table::data.table(
            stage = character(), entity_id = character(), name = character(),
            version_id = character(), content_hash = character()
          )
        )
      }
      receipt <- pd_catalog_receipt(
        snapshot$catalogs[[
          if (selected_stage == "metadata") "pip_meta" else "pip_deflated"
        ]],
        row_list$pip_id
      )
      if (identical(selected_stage, "deflate")) {
        receipt <- pd_deflate_current_receipt(receipt, row_list)
      } else {
        receipt <- pd_metadata_current_receipt(receipt, row_list)
      }
      rows[[length(rows) + 1L]] <- data.table::data.table(
        stage = selected_stage,
        entity_id = row_list$pip_id, survey_id = row_list$survey_id,
        pip_id = row_list$pip_id, input_hash = state$input_hash,
        legacy_input_hash = state$legacy_input_hash,
        legacy_input_version = state$legacy_input_version,
        code_hash = code_hash(selected_stage),
        output_version_id = receipt$version_id,
        output_hash = receipt$content_hash,
        expected_outputs = list(row_list$pip_id),
        output_receipts = list(list(receipt)),
        aux_projection = list(state$aux_projection),
        data_version_id = row_list$version_id_data %||% NA_character_,
        data_hash = row_list$content_hash_data %||% NA_character_,
        metadata_version_id = row_list$version_id_metadata %||% NA_character_,
        metadata_hash = row_list$content_hash_metadata %||% NA_character_,
        input_rows = list(state$input_rows)
      )
    }
  }
  return(data.table::rbindlist(rows, fill = TRUE))
}

pd_snapshot_facts <- function(snapshot, manifest) {
  current <- snapshot$current
  if (!nrow(current) || inherits(manifest, "pipdata_manifest_absent")) {
    return(data.table::data.table())
  }
  records <- manifest$records
  facts <- pd_empty_reasons()
  facts[, `:=`(survey_id = character(), pip_id = character())]
  data.table::setcolorder(
    facts,
    c("stage", "entity_id", "survey_id", "pip_id", "reason", "input",
      "old", "new")
  )
  fact_rows <- list()
  add_fact <- function(row, reason, input, old, new) {
    fact_rows[[length(fact_rows) + 1L]] <<- data.table::data.table(
      stage = row$stage[[1L]], entity_id = row$entity_id[[1L]],
      survey_id = row$survey_id[[1L]], pip_id = row$pip_id[[1L]],
      reason = reason, input = input,
      old = as.character(old), new = as.character(new)
    )
  }
  stage_code_reason <- function(stage) {
    paste0(stage, "_code_changed")
  }
  for (i in seq_len(nrow(current))) {
    row <- current[i]
    prior_record <- records[
      stage == row$stage[[1L]] & entity_id == row$entity_id[[1L]]
    ]
    if (nrow(prior_record) != 1L) {
      has_output <- !is.na(row$output_version_id[[1L]]) &&
        nzchar(row$output_version_id[[1L]]) &&
        !is.na(row$output_hash[[1L]]) && nzchar(row$output_hash[[1L]])
      add_fact(
        row,
        if (has_output) "unknown_provenance" else "new_entity",
        "manifest", NA_character_, row$input_hash[[1L]]
      )
      next
    }
    if (is.na(row$output_hash[[1L]]) ||
        is.na(row$output_version_id[[1L]])) {
      add_fact(
        row, "output_missing", "output", prior_record$output_hash[[1L]],
        row$output_hash[[1L]]
      )
    } else if (!identical(
      prior_record$output_hash[[1L]], row$output_hash[[1L]]
    ) || !identical(
      prior_record$output_version_id[[1L]], row$output_version_id[[1L]]
    )) {
      add_fact(
        row, "output_drift", "output", prior_record$output_hash[[1L]],
        row$output_hash[[1L]]
      )
    }

    prior_inputs <- manifest$inputs[
      stage == row$stage[[1L]] & entity_id == row$entity_id[[1L]]
    ]
    named_prior <- prior_inputs[name != "canonical"]
    if (!nrow(named_prior)) {
      old_hash <- prior_inputs[name == "canonical", content_hash][1L]
      old_version <- prior_inputs[name == "canonical", version_id][1L]
      new_hash <- row$legacy_input_hash[[1L]]
      new_version <- if ("legacy_input_version" %in% names(row)) {
        row$legacy_input_version[[1L]]
      } else {
        old_version
      }
      if (!identical(prior_record$input_hash[[1L]], new_hash) ||
          !identical(old_hash, new_hash) ||
          !identical(old_version, new_version)) {
        add_fact(
          row, "legacy_input_changed", "canonical",
          paste(old_version, prior_record$input_hash[[1L]], sep = ":"),
          paste(new_version, new_hash, sep = ":")
        )
      }
    } else {
      current_inputs <- row$input_rows[[1L]][name != "canonical"]
      comparison <- merge(
        named_prior[, .(name, version_id_old = version_id,
                        content_hash_old = content_hash)],
        current_inputs[, .(name, version_id_new = version_id,
                           content_hash_new = content_hash)],
        by = "name", all = TRUE
      )
      semantic_component <- comparison$name == "pfw" |
        grepl("^aux_", comparison$name)
      changed <- comparison[
        is.na(content_hash_old) | is.na(content_hash_new) |
          content_hash_old != content_hash_new |
          (!semantic_component & (
            is.na(version_id_old) | is.na(version_id_new) |
              version_id_old != version_id_new
          ))
      ]
      for (j in seq_len(nrow(changed))) {
        old <- if (!is.na(changed$version_id_old[[j]]) &&
                   !is.na(changed$version_id_new[[j]]) &&
                   identical(changed$version_id_old[[j]],
                             changed$version_id_new[[j]])) {
          changed$content_hash_old[[j]]
        } else {
          paste(changed$version_id_old[[j]],
                changed$content_hash_old[[j]], sep = ":")
        }
        new <- if (!is.na(changed$version_id_old[[j]]) &&
                   !is.na(changed$version_id_new[[j]]) &&
                   identical(changed$version_id_old[[j]],
                             changed$version_id_new[[j]])) {
          changed$content_hash_new[[j]]
        } else {
          paste(changed$version_id_new[[j]],
                changed$content_hash_new[[j]], sep = ":")
        }
        add_fact(
          row,
          pd_input_change_reason(row$stage[[1L]], changed$name[[j]]),
          changed$name[[j]], old, new
        )
      }
    }

    old_components <- manifest$fingerprints[stage == row$stage[[1L]]]
    new_components <- snapshot$fingerprints$components[
      stage == row$stage[[1L]]
    ]
    if (nrow(old_components)) {
      component_compare <- merge(
        old_components[, .(component, hash_old = hash)],
        new_components[, .(component, hash_new = hash)],
        by = "component", all = TRUE
      )
      changed_components <- component_compare[
        is.na(hash_old) | is.na(hash_new) | hash_old != hash_new
      ]
      for (j in seq_len(nrow(changed_components))) {
        reason <- if (identical(
          changed_components$component[[j]], "recode_spec.yml"
        )) {
          "recode_spec_changed"
        } else {
          stage_code_reason(row$stage[[1L]])
        }
        add_fact(
          row, reason, changed_components$component[[j]],
          changed_components$hash_old[[j]], changed_components$hash_new[[j]]
        )
      }
    } else if (!identical(
      prior_record$code_hash[[1L]], row$code_hash[[1L]]
    )) {
      add_fact(
        row, stage_code_reason(row$stage[[1L]]), "code",
        prior_record$code_hash[[1L]], row$code_hash[[1L]]
      )
    }
  }
  if (length(fact_rows)) {
    facts <- data.table::rbindlist(
      c(list(facts), fact_rows), use.names = TRUE, fill = TRUE
    )
  }
  facts <- unique(facts)
  data.table::setorder(facts, stage, entity_id, reason, input)
  return(facts)
}

pd_revalidate_snapshot <- function(snapshot, advanced_receipts = NULL) {
  if (!identical(pd_dependency_context(), snapshot$context) ||
      !identical(pd_code_fingerprints(), snapshot$fingerprints)) {
    rlang::abort("The execution context or code fingerprint changed.",
                 class = "pipdata_dependency_snapshot_stale")
  }
  expected <- c(list(aux = snapshot$aux$catalog), snapshot$catalogs)
  advanced <- data.table::as.data.table(data.table::copy(
    advanced_receipts %||% data.table::data.table()
  ))
  has_advanced_paths <- all(c("alias", "path") %in% names(advanced))
  for (alias in names(expected)) {
    rows <- data.table::as.data.table(expected[[alias]])
    current <- data.table::as.data.table(stamp::st_catalog_query(alias = alias))
    if (!nrow(rows)) next
    for (i in seq_len(nrow(rows))) {
      row <- rows[i]
      exact <- current[
        path == row$path & version_id == row$version_id &
          content_hash == row$content_hash
      ]
      advanced_path <- has_advanced_paths && nrow(advanced[
        advanced$alias == alias & advanced$path == row$path
      ]) > 0L
      if (nrow(exact) != 1L && advanced_path) {
        versions <- tryCatch(
          stamp::st_versions(row$path, alias = alias),
          error = function(cnd) NULL
        )
        if (!is.null(versions)) {
          exact <- versions[
            version_id == row$version_id & content_hash == row$content_hash
          ]
        }
      }
      if (nrow(exact) != 1L) {
        rlang::abort("A frozen catalog row is no longer exact.",
                     class = "pipdata_dependency_snapshot_stale")
      }
    }
  }
  invisible(snapshot)
}

pd_advance_execution_state <- function(execution, master, receipts) {
  if (is.null(execution$snapshot)) {
    return(execution)
  }
  snapshot <- execution$snapshot
  snapshot$master <- data.table::copy(data.table::as.data.table(master))
  catalogs <- snapshot$catalogs %||% list()
  receipts <- data.table::as.data.table(data.table::copy(receipts))
  required <- c("alias", "path", "version_id", "content_hash", "success")
  if (all(required %in% names(receipts)) && nrow(receipts)) {
    receipts <- receipts[
      success %in% TRUE & !is.na(alias) & nzchar(alias) &
        !is.na(path) & nzchar(path)
    ]
    for (alias in intersect(unique(receipts$alias), names(catalogs))) {
      selected_alias <- alias
      incoming <- unique(receipts[
        receipts$alias == selected_alias,
        .(path, version_id, content_hash)
      ], by = "path")
      catalog <- data.table::as.data.table(data.table::copy(
        catalogs[[selected_alias]]
      ))
      if ("path" %in% names(catalog)) {
        catalog <- catalog[!path %in% incoming$path]
      } else {
        catalog <- data.table::data.table()
      }
      catalogs[[selected_alias]] <- data.table::rbindlist(
        list(catalog, incoming), use.names = TRUE, fill = TRUE
      )
      data.table::setorder(catalogs[[selected_alias]], path)
    }
  }
  snapshot$catalogs <- catalogs
  execution$snapshot <- snapshot
  return(execution)
}

pd_prepare_execution <- function(inv, master, context = pd_dependency_context(),
                                 advisory_plan = NULL, bootstrap = FALSE,
                                  bootstrap_entities = NULL, force = FALSE,
                                   force_surveys = NULL, verbose = FALSE,
                                   measures = c(
                                     "pfw", "cpi", "ppp", "pop", "gdp", "pce"
                                   ),
                                   metadata_measures = setdiff(
                                     measures, "pfw"
                                   )) {
  check_removed_surveys <- is.data.frame(inv) && ncol(inv) > 0L
  prepare_plan <- function() {
    prepared <- pd_prepare_dependency_facts(
      inv = inv,
      master = master,
      context = context,
      measures = measures,
      metadata_measures = metadata_measures,
      verbose = verbose,
      check_removed_surveys = check_removed_surveys
    )
    snapshot <- prepared$snapshot
    plan <- pd_dependency_plan(
      snapshot$inventory,
      snapshot$master,
      prepared$manifest,
      prepared$context,
      snapshot$fingerprints,
      force,
      force_surveys,
      snapshot = snapshot
    )
    plan <- pd_assert_bootstrap(plan, bootstrap, bootstrap_entities)
    list(prepared = prepared, plan = plan)
  }
  prepare_plan()
  if (!is.null(advisory_plan)) {
    pd_validate_plan(advisory_plan)
  }
  lease <- pd_lease_acquire(context)
  execution <- tryCatch(
    pd_prepare_execution_locked(
      inv = inv,
      master = master,
      context = context,
      lease = lease,
      bootstrap = bootstrap,
      bootstrap_entities = bootstrap_entities,
      force = force,
      force_surveys = force_surveys,
      verbose = verbose,
      measures = measures,
      metadata_measures = metadata_measures
    ),
    error = function(cnd) {
      try(pd_lease_release(lease), silent = TRUE)
      rlang::cnd_signal(cnd)
    }
  )
  return(execution)
}

pd_assert_execution_refresh_fence <- function(execution) {
  if (is.null(execution$snapshot$context)) {
    pd_assert_execution_fence(execution)
    return(invisible(execution))
  }
  pd_lease_assert(execution$lease)
  if (!identical(pd_dependency_context(), execution$snapshot$context) ||
      !identical(pd_code_fingerprints(), execution$snapshot$fingerprints)) {
    rlang::abort(
      "The execution context or code fingerprint changed.",
      class = "pipdata_dependency_snapshot_stale"
    )
  }
  current <- pd_manifest_read(execution$context, allow_absent = TRUE)
  identity <- if (inherits(current, "pipdata_manifest_absent")) {
    NULL
  } else {
    attr(current, "manifest_identity")
  }
  if (!identical(identity, execution$manifest_identity)) {
    rlang::abort(
      "Manifest parent changed before fact refresh.",
      class = "pipdata_manifest_parent_changed"
    )
  }
  invisible(execution)
}

pd_prepare_execution_locked <- function(
  inv,
  master,
  context,
  lease,
  bootstrap = FALSE,
  bootstrap_entities = NULL,
  force = FALSE,
  force_surveys = NULL,
  verbose = FALSE,
  measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  metadata_measures = c("cpi", "ppp", "pop"),
  strict_bootstrap_selectors = FALSE
) {
  prepared <- pd_prepare_dependency_facts(
    inv = inv,
    master = master,
    context = context,
    measures = measures,
    metadata_measures = metadata_measures,
    verbose = verbose,
    check_removed_surveys = TRUE
  )
  snapshot <- prepared$snapshot
  plan <- pd_dependency_plan(
    snapshot$inventory,
    snapshot$master,
    prepared$manifest,
    prepared$context,
    snapshot$fingerprints,
    force,
    force_surveys,
    snapshot = snapshot
  )
  plan <- pd_assert_bootstrap(
    plan,
    bootstrap,
    bootstrap_entities,
    strict_selectors = strict_bootstrap_selectors
  )
  manifest <- prepared$manifest
  if (inherits(manifest, "pipdata_manifest_absent")) {
    manifest <- pd_empty_manifest(context)
  }
  return(list(context = context, snapshot = prepared$snapshot,
       plan = plan,
       manifest = manifest,
       manifest_identity = if (inherits(manifest, "pipdata_manifest_absent")) {
         NULL
        } else attr(manifest, "manifest_identity"),
       lease = lease))
}

pd_assert_execution_fence <- function(execution, advanced_receipts = NULL) {
  pd_lease_assert(execution$lease)
  pd_revalidate_snapshot(execution$snapshot, advanced_receipts)
  current <- pd_manifest_read(execution$context, allow_absent = TRUE)
  identity <- if (inherits(current, "pipdata_manifest_absent")) NULL else
    attr(current, "manifest_identity")
  if (!identical(identity, execution$manifest_identity)) {
    rlang::abort("Manifest parent changed at the write boundary.",
                 class = "pipdata_manifest_parent_changed")
  }
  invisible(execution)
}

pd_run_checkpoint_batches <- function(units, worker, checkpoint,
                                      checkpoint_n = getOption("pipdata.manifest_checkpoint_n", 25L),
                                      checkpoint_seconds = getOption("pipdata.manifest_checkpoint_seconds", 60),
                                      clock = Sys.time) {
  has_current_action <- vapply(units, function(unit) {
    action <- if (is.data.frame(unit) && "action" %in% names(unit)) {
      unit$action
    } else if (is.list(unit) && "action" %in% names(unit)) {
      unit[["action"]]
    } else {
      character()
    }
    any(as.character(action) == "none", na.rm = TRUE)
  }, logical(1L))
  if (any(has_current_action)) {
    rlang::abort(
      "Current dependency nodes cannot be dispatched to workers.",
      class = "pipdata_dependency_action_not_runnable"
    )
  }
  pending <- list()
  last <- clock()
  for (i in seq_along(units)) {
    result <- worker(units[[i]])
    if (!is.null(result) && isTRUE(result$success)) pending[[length(pending) + 1L]] <- result
    elapsed <- as.numeric(difftime(clock(), last, units = "secs"))
    if (length(pending) &&
        (length(pending) >= checkpoint_n || elapsed >= checkpoint_seconds)) {
      checkpoint(pending)
      pending <- list()
      last <- clock()
    }
  }
  if (length(pending)) checkpoint(pending)
  invisible(NULL)
}
