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

pd_build_dependency_snapshot <- function(inv, master, context,
                                         measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
                                         verbose = FALSE) {
  aux <- pd_freeze_aux_snapshot(measures, verbose)
  catalogs <- lapply(c("pip", "pip_meta", "pip_deflated", "pip_inv"), function(alias) {
    data.table::as.data.table(stamp::st_catalog_query(alias = alias))
  })
  names(catalogs) <- c("pip", "pip_meta", "pip_deflated", "pip_inv")
  snapshot <- list(
    context = context,
    inventory = data.table::copy(data.table::as.data.table(inv)),
    master = data.table::copy(data.table::as.data.table(master)),
    aux = aux,
    catalogs = catalogs,
    fingerprints = pd_code_fingerprints(),
    captured_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  snapshot$current <- pd_snapshot_current(snapshot)
  snapshot
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
  current <- isTRUE(master_row$deflated) &&
    !is.null(master_row$version_id_deflated) &&
    !is.null(master_row$content_hash_deflated) &&
    !is.na(master_row$version_id_deflated) &&
    !is.na(master_row$content_hash_deflated) &&
    identical(receipt$version_id, master_row$version_id_deflated) &&
    identical(receipt$content_hash, master_row$content_hash_deflated)
  if (current) return(receipt)
  list(version_id = NA_character_, content_hash = NA_character_,
       path = NA_character_)
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
  code_hash <- function(stage) fingerprints[stage == ..stage, hash][1L]
  inv <- snapshot$inventory
  for (i in seq_len(nrow(inv))) {
    row <- as.list(inv[i])
    entity <- row$survey_id
    aux_hash <- pd_entity_aux_hash(snapshot, row, "pfw")
    expected <- tryCatch(expected_pip_ids(row, pd_select_aux(
      snapshot$aux$objects$pfw, "pfw", row$country_code %||% row$country,
      row$year, row$survey_acronym %||% NULL,
      row$reporting_level %||% NULL)$data), error = function(e) character())
    receipts <- lapply(expected, function(id) pd_catalog_receipt(snapshot$catalogs$pip, id))
    rows[[length(rows) + 1L]] <- data.table::data.table(
      stage = "clean", entity_id = entity, survey_id = entity,
      pip_id = NA_character_, input_hash = pd_hash_object(list(row, aux_hash)),
      code_hash = code_hash("clean"), output_version_id = pd_hash_object(lapply(receipts, `[[`, "version_id")),
      output_hash = pd_hash_object(lapply(receipts, `[[`, "content_hash")),
      expected_outputs = list(expected), output_receipts = list(receipts)
    )
  }
  master <- snapshot$master
  for (i in seq_len(nrow(master))) {
    row <- as.list(master[i])
    for (stage in c("metadata", "deflate")) {
      measures <- if (stage == "metadata") c("cpi", "ppp", "pop", "gdp", "pce") else c("cpi", "ppp", "pop")
      aux_hash <- pd_entity_aux_hash(snapshot, row, measures)
      aux_projection <- tryCatch(
        pd_entity_aux_projection(snapshot, row, measures),
        error = function(e) list()
      )
      input <- if (stage == "metadata") {
        list(row$version_id_data, row$content_hash_data, aux_hash)
      } else list(row$version_id_data, row$content_hash_data,
                  row$version_id_metadata, row$content_hash_metadata, aux_hash)
      receipt <- pd_catalog_receipt(snapshot$catalogs[[if (stage == "metadata") "pip_meta" else "pip_deflated"]], row$pip_id)
      if (identical(stage, "deflate")) {
        receipt <- pd_deflate_current_receipt(receipt, row)
      }
      rows[[length(rows) + 1L]] <- data.table::data.table(
        stage, entity_id = row$pip_id, survey_id = row$survey_id,
        pip_id = row$pip_id, input_hash = pd_hash_object(input),
        code_hash = code_hash(stage), output_version_id = receipt$version_id,
        output_hash = receipt$content_hash, expected_outputs = list(row$pip_id),
        output_receipts = list(list(receipt)), aux_projection = list(aux_projection),
        data_version_id = row$version_id_data %||% NA_character_,
        data_hash = row$content_hash_data %||% NA_character_,
        metadata_version_id = row$version_id_metadata %||% NA_character_,
        metadata_hash = row$content_hash_metadata %||% NA_character_
      )
    }
  }
  data.table::rbindlist(rows, fill = TRUE)
}

pd_snapshot_facts <- function(snapshot, manifest) {
  current <- snapshot$current
  if (!nrow(current) || inherits(manifest, "pipdata_manifest_absent")) return(data.table::data.table())
  records <- manifest$records
  joined <- records[current, on = c("stage", "entity_id")]
  prior_inputs <- manifest$inputs[name == "canonical",
                                  .(stage, entity_id,
                                    manifest_input_hash = content_hash)]
  joined <- prior_inputs[joined, on = c("stage", "entity_id")]
  current_fingerprints <- snapshot$fingerprints$components
  fingerprint_compare <- merge(
    manifest$fingerprints, current_fingerprints,
    by = c("stage", "component"), all = TRUE,
    suffixes = c("_old", "_new")
  )
  changed_fingerprint_stages <- fingerprint_compare[
    is.na(hash_old) | is.na(hash_new) | hash_old != hash_new, unique(stage)
  ]
  joined[, reason := data.table::fcase(
    is.na(i.output_hash), "output_missing",
    output_hash != i.output_hash | output_version_id != i.output_version_id, "output_drift",
    input_hash != i.input_hash | manifest_input_hash != i.input_hash,
      "upstream_output_changed",
    code_hash != i.code_hash | stage %in% changed_fingerprint_stages,
      ifelse(stage == "clean", "clean_code_changed",
             ifelse(stage == "metadata", "metadata_code_changed", "deflate_code_changed")),
    default = NA_character_
  )]
  joined[!is.na(reason), .(
    stage, entity_id, survey_id = i.survey_id, pip_id = i.pip_id, reason,
    input = "canonical", old = input_hash, new = i.input_hash
  )]
}

pd_revalidate_snapshot <- function(snapshot) {
  if (!identical(pd_dependency_context(), snapshot$context) ||
      !identical(pd_code_fingerprints(), snapshot$fingerprints)) {
    rlang::abort("The execution context or code fingerprint changed.",
                 class = "pipdata_dependency_snapshot_stale")
  }
  expected <- c(list(aux = snapshot$aux$catalog), snapshot$catalogs)
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
      if (nrow(exact) != 1L) {
        rlang::abort("A frozen catalog row is no longer exact.",
                     class = "pipdata_dependency_snapshot_stale")
      }
    }
  }
  invisible(snapshot)
}

pd_prepare_execution <- function(inv, master, context = pd_dependency_context(),
                                 advisory_plan = NULL, bootstrap = FALSE,
                                 bootstrap_entities = NULL, force = FALSE,
                                 force_surveys = NULL, verbose = FALSE) {
  manifest <- pd_manifest_read(context, allow_absent = TRUE)
  snapshot <- pd_build_dependency_snapshot(inv, master, context,
                                           verbose = verbose)
  snapshot$facts <- pd_snapshot_facts(snapshot, manifest)
  plan <- pd_dependency_plan(
    snapshot$inventory, snapshot$master, manifest, context,
    snapshot$fingerprints, force, force_surveys, snapshot
  )
  plan <- pd_assert_bootstrap(plan, bootstrap, bootstrap_entities)
  if (!is.null(advisory_plan)) {
    pd_validate_plan(advisory_plan)
  }
  lease <- pd_lease_acquire(context)
  if (inherits(manifest, "pipdata_manifest_absent")) {
    manifest <- pd_empty_manifest(context)
  }
  list(context = context, snapshot = snapshot, plan = plan,
       manifest = manifest,
       manifest_identity = if (inherits(manifest, "pipdata_manifest_absent")) {
         NULL
       } else attr(manifest, "manifest_identity"),
       lease = lease)
}

pd_assert_execution_fence <- function(execution) {
  pd_lease_assert(execution$lease)
  pd_revalidate_snapshot(execution$snapshot)
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
