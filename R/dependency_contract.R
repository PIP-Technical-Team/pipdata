.PD_DEPENDENCY_SCHEMA <- 1L
.PD_STAGES <- c("clean", "metadata", "deflate")
.PD_ACTIONS <- c("create", "refresh", "rebuild", "none")
.PD_REASON_CODES <- c(
  "new_entity", "dlw_changed", "pfw_changed", "recode_spec_changed",
  "clean_code_changed", "metadata_code_changed", "deflate_code_changed",
  "aux_cpi_changed", "aux_ppp_changed", "aux_pop_changed",
  "aux_gdp_changed", "aux_pce_changed", "upstream_output_changed",
  "output_missing", "output_drift", "forced", "unknown_provenance",
  "legacy_input_changed"
)

pd_empty_actions <- function() {
  data.table::data.table(
    stage = character(), entity_id = character(), survey_id = character(),
    pip_id = character(), action = character()
  )
}

pd_empty_reasons <- function() {
  data.table::data.table(
    stage = character(), entity_id = character(), reason = character(),
    input = character(), old = character(), new = character()
  )
}

pd_empty_plan_node_states <- function() {
  actions <- pd_empty_actions()
  actions[, `:=`(
    state = character(), scheduling_state = character(),
    wave_state = character()
  )]
  return(actions)
}

pd_empty_manifest <- function(context) {
  list(
    header = data.table::data.table(
      schema_version = .PD_DEPENDENCY_SCHEMA,
      scope_id = context$scope_id,
      context_hash = pd_context_hash(context),
      created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
    ),
    records = data.table::data.table(
      stage = character(), entity_id = character(), output_version_id = character(),
      output_hash = character(), input_hash = character(), code_hash = character(),
      output_receipts = list()
    ),
    inputs = data.table::data.table(
      stage = character(), entity_id = character(), name = character(),
      version_id = character(), content_hash = character()
    ),
    fingerprints = data.table::data.table(
      stage = character(), component = character(), hash = character()
    ),
    tombstones = data.table::data.table(
      survey_id = character(), pip_id = character(), reason = character()
    )
  )
}

pd_validate_plan <- function(x) {
  expected <- c("context", "actions", "reasons", "snapshot")
  action_fields <- c("stage", "entity_id", "survey_id", "pip_id", "action")
  reason_fields <- c("stage", "entity_id", "reason", "input", "old", "new")
  if (!is.list(x) || !identical(names(x), expected) ||
      !data.table::is.data.table(x$actions) ||
      !data.table::is.data.table(x$reasons) ||
      !all(action_fields %in% names(x$actions)) ||
      !all(reason_fields %in% names(x$reasons))) {
    rlang::abort(
      "Invalid dependency plan shape.",
      class = "pipdata_dependency_plan_invalid"
    )
  }
  actions <- x$actions
  reasons <- x$reasons
  invalid_mapping <- actions[
    stage == "clean",
    any(is.na(survey_id) | !nzchar(survey_id) | entity_id != survey_id |
          !is.na(pip_id))
  ] || actions[
    stage %in% c("metadata", "deflate"),
    any(is.na(survey_id) | !nzchar(survey_id) | is.na(pip_id) |
          !nzchar(pip_id) | entity_id != pip_id)
  ]
  if (anyNA(actions[, .(stage, entity_id, action)]) ||
      any(!nzchar(actions$stage)) || any(!nzchar(actions$entity_id)) ||
      any(!actions$stage %in% .PD_STAGES) ||
      any(!x$actions$action %in% .PD_ACTIONS) ||
      anyDuplicated(actions[, .(stage, entity_id)]) || invalid_mapping) {
    rlang::abort(
      "Invalid dependency plan actions.",
      class = "pipdata_dependency_plan_invalid"
    )
  }
  if (anyNA(reasons[, .(stage, entity_id, reason)]) ||
      any(!reasons$stage %in% .PD_STAGES) ||
      any(!reasons$reason %in% .PD_REASON_CODES) ||
      anyDuplicated(reasons)) {
    rlang::abort(
      "Invalid dependency plan reasons.",
      class = "pipdata_dependency_plan_invalid"
    )
  }
  reason_actions <- actions[reasons, on = c("stage", "entity_id")]
  actionable <- actions[action != "none", .(stage, entity_id)]
  reason_keys <- unique(reasons[, .(stage, entity_id)])
  missing_reasons <- actionable[!reason_keys, on = c("stage", "entity_id")]
  if ((nrow(reasons) &&
       (anyNA(reason_actions$action) || any(reason_actions$action == "none"))) ||
      nrow(missing_reasons)) {
    rlang::abort(
      "Plan actions and reasons are inconsistent.",
      class = "pipdata_dependency_plan_invalid"
    )
  }
  invisible(x)
}

pd_validate_manifest <- function(x, context = NULL) {
  required <- c("header", "records", "inputs", "fingerprints", "tombstones")
  if (!is.list(x) || !identical(names(x), required) || nrow(x$header) != 1L ||
      x$header$schema_version[[1L]] != .PD_DEPENDENCY_SCHEMA ||
      any(!x$records$stage %in% .PD_STAGES) ||
      anyDuplicated(x$records[, .(stage, entity_id)])) {
    rlang::abort("Invalid dependency manifest.", class = "pipdata_dependency_manifest_invalid")
  }
  required_record <- c("stage", "entity_id", "output_version_id", "output_hash",
                       "input_hash", "code_hash", "output_receipts")
  required_input <- c("stage", "entity_id", "name", "version_id", "content_hash")
  required_fingerprint <- c("stage", "component", "hash")
  if (!all(required_record %in% names(x$records)) ||
      !all(required_input %in% names(x$inputs)) ||
      !all(required_fingerprint %in% names(x$fingerprints)) ||
      (nrow(x$records) && anyNA(x$records[, c("stage", "entity_id",
        "output_version_id", "output_hash", "input_hash", "code_hash"), with = FALSE])) ||
      anyDuplicated(x$inputs[, .(stage, entity_id, name)]) ||
      anyDuplicated(x$fingerprints[, .(stage, component)])) {
    rlang::abort("Manifest provenance schema is incomplete.",
                 class = "pipdata_dependency_manifest_invalid")
  }
  nonblank <- function(values) {
    is.character(values) && !anyNA(values) && all(nzchar(trimws(values)))
  }
  if ((nrow(x$inputs) && (!nonblank(x$inputs$stage) ||
      !nonblank(x$inputs$entity_id) || !nonblank(x$inputs$name) ||
      !nonblank(x$inputs$version_id) || !nonblank(x$inputs$content_hash))) ||
      (nrow(x$fingerprints) && (!nonblank(x$fingerprints$stage) ||
      !nonblank(x$fingerprints$component) || !nonblank(x$fingerprints$hash)))) {
    rlang::abort("Manifest inputs and fingerprints must be complete.",
                  class = "pipdata_dependency_manifest_invalid")
  }
  record_keys <- unique(x$records[, .(stage, entity_id)])
  input_keys <- unique(x$inputs[, .(stage, entity_id)])
  data.table::setorder(record_keys, stage, entity_id)
  data.table::setorder(input_keys, stage, entity_id)
  if (!identical(record_keys, input_keys)) {
    rlang::abort(
      "Manifest records and input groups must have identical keys.",
      class = "pipdata_dependency_manifest_invalid"
    )
  }
  if (nrow(x$inputs)) {
    if (any(!x$inputs$stage %in% .PD_STAGES)) {
      rlang::abort(
        "Manifest input stages are invalid.",
        class = "pipdata_dependency_manifest_invalid"
      )
    }
    input_groups <- unique(x$inputs[, .(stage, entity_id)])
    valid_inputs <- vapply(seq_len(nrow(input_groups)), function(i) {
      key <- input_groups[i]
      rows <- x$inputs[
        stage == key$stage[[1L]] & entity_id == key$entity_id[[1L]]
      ]
      canonical <- rows[name == "canonical"]
      components <- rows[name != "canonical"]
      record <- x$records[
        stage == key$stage[[1L]] & entity_id == key$entity_id[[1L]]
      ]
      if (nrow(canonical) != 1L || nrow(record) != 1L ||
          !identical(
            record$input_hash[[1L]], canonical$content_hash[[1L]]
          )) {
        return(FALSE)
      }
      if (!nrow(components)) {
        return(TRUE)
      }
      expected <- tryCatch(
        pd_build_input_rows(
          key$stage[[1L]], key$entity_id[[1L]],
          components[, .(name, version_id, content_hash)]
        ),
        error = function(e) NULL
      )
      !is.null(expected) &&
        identical(
          as.list(canonical[, .(name, version_id, content_hash)]),
          as.list(expected[name == "canonical",
                           .(name, version_id, content_hash)])
        )
    }, logical(1))
    if (!all(valid_inputs)) {
      rlang::abort(
        "Manifest named input composites are invalid.",
        class = "pipdata_dependency_manifest_invalid"
      )
    }
  }
  if (nrow(x$records)) {
    valid_receipts <- vapply(seq_len(nrow(x$records)), function(i) {
      receipts <- x$records$output_receipts[[i]]
      while (is.list(receipts) && length(receipts) == 1L &&
             is.list(receipts[[1L]]) && is.null(names(receipts[[1L]]))) {
        receipts <- receipts[[1L]]
      }
      receipt_fields <- c("alias", "artifact", "path", "version_id",
                          "content_hash")
      if (is.list(receipts) && all(receipt_fields %in% names(receipts))) {
        receipts <- list(receipts)
      }
      expected_n <- if (x$records$stage[[i]] == "clean") 1L else 1L
      is.list(receipts) && length(receipts) >= expected_n &&
        all(vapply(receipts, function(receipt) {
          is.list(receipt) &&
            all(receipt_fields %in% names(receipt)) &&
            all(vapply(receipt[c("alias", "artifact", "path", "version_id",
                                 "content_hash")], function(value) {
              is.character(value) && length(value) == 1L && !is.na(value) &&
                nzchar(trimws(value))
            }, logical(1)))
        }, logical(1)))
    }, logical(1))
    if (!all(valid_receipts)) {
      rlang::abort("Manifest output receipts are incomplete or malformed.",
                   class = "pipdata_dependency_manifest_invalid")
    }
  }
  if (!is.null(context) && (!identical(x$header$scope_id[[1L]], context$scope_id) ||
      !identical(x$header$context_hash[[1L]], pd_context_hash(context)))) {
    rlang::abort("Manifest scope does not match the active context.",
                 class = "pipdata_dependency_scope_mismatch")
  }
  invisible(x)
}

pd_normalize_path <- function(path) {
  path <- enc2utf8(fs::path_norm(path))
  path <- gsub("\\\\", "/", path)
  path <- sub("/+$", "", path)
  if (.Platform$OS.type == "windows") path <- tolower(path)
  path
}

pd_hash_object <- function(x, algo = "xxhash64") {
  digest::digest(x, algo = algo, serialize = TRUE, serializeVersion = 3L)
}

pd_context_hash <- function(context) {
  context$scope_id <- NULL
  pd_hash_object(context)
}

pd_dependency_context <- function(release = getOption("pipfun.working_release"),
                                  identity = getOption("pipfun.identity"),
                                  main_dir = getOption("pipfun.main_dir"),
                                  namespace = getOption("pipdata.dependency_scope")) {
  if (!is.character(release) || length(release) != 1L || !nzchar(release) ||
      !is.character(identity) || length(identity) != 1L || !nzchar(identity)) {
    rlang::abort("A single working release and identity are required.",
                 class = "pipdata_dependency_context_error")
  }
  aliases <- data.table::as.data.table(stamp::st_alias_list())
  wanted <- c("pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv")
  roots <- stats::setNames(rep(NA_character_, length(wanted)), wanted)
  if (nrow(aliases)) roots[intersect(wanted, aliases$alias)] <- aliases[match(intersect(wanted, aliases$alias), alias), root]
  fallback <- fs::path(main_dir, "pip_repository", wanted)
  roots[is.na(roots)] <- fallback[is.na(roots)]
  if (any(!nzchar(roots))) {
    rlang::abort("Repository roots are ambiguous.", class = "pipdata_dependency_context_error")
  }
  context <- list(schema_version = .PD_DEPENDENCY_SCHEMA, release = release,
                  identity = identity, roots = lapply(roots, pd_normalize_path),
                  namespace = namespace %||% "")
  context$scope_id <- pd_context_hash(context)
  context
}

`%||%` <- function(x, y) if (is.null(x)) y else x
