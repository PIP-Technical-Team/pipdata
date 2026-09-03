.PDS_SCHEMA <- 2L
.PDS_LEGACY_SCHEMA <- 1L
.PDS_CONDITION_SCHEMA <- 1L
.PDS_STAGES <- c("acquisition", "validation", .PD_STAGES)
.PDS_STATUSES <- c("success", "partial", "failed", "cached", "skipped")
.PDS_UNIT_STATUSES <- setdiff(.PDS_STATUSES, "partial")
.PDS_REASONS <- c(
  "no_selection", "current", "upstream_failed", "policy_excluded",
  "checkpoint_uncommitted", "fatal_uncommitted", "entity_failed"
)
.PDS_ARTIFACT_ROLES <- c("primary", "metadata", "inventory", "log")

pd_stage_abort <- function(message) {
  rlang::abort(message, class = "pipdata_stage_result_invalid")
}

pd_utc_time <- function(x, allow_na = FALSE) {
  if (!inherits(x, "POSIXct") || length(x) != 1L ||
      (!allow_na && is.na(x))) {
    pd_stage_abort("Expected one UTC POSIXct value.")
  }
  as.POSIXct(x, tz = "UTC")
}

pd_scalar_character <- function(x, name, allow_na = FALSE) {
  valid <- is.character(x) && length(x) == 1L &&
    (allow_na || (!is.na(x) && nzchar(enc2utf8(x))))
  if (!valid) pd_stage_abort(paste(name, "must be a character scalar."))
  enc2utf8(x)
}

pd_empty_stage_units <- function() {
  data.table::data.table(
    stage = character(), entity_id = character(), survey_id = character(),
    pip_id = character(), status = character(), action = character(),
    reason_codes = list(), input_hash = character(), output_hash = character(),
    started_at = as.POSIXct(character(), tz = "UTC"),
    completed_at = as.POSIXct(character(), tz = "UTC")
  )
}

pd_empty_artifact_references <- function() {
  data.table::data.table(
    entity_id = character(), alias = character(), artifact = character(),
    path = character(), version_id = character(), content_hash = character(),
    role = character(), manifest_generation = numeric()
  )
}

validate_stage_units <- function(x) {
  expected <- names(pd_empty_stage_units())
  if (!data.table::is.data.table(x) || !identical(names(x), expected)) {
    pd_stage_abort("Unit outcomes have an invalid schema.")
  }
  if (anyNA(x$stage) || anyNA(x$entity_id) || anyNA(x$status) ||
      any(!nzchar(x$entity_id)) || any(!x$stage %in% .PDS_STAGES) ||
      any(!x$status %in% .PDS_UNIT_STATUSES) ||
      anyDuplicated(x[, .(stage, entity_id)])) {
    pd_stage_abort("Unit outcomes contain invalid keys or controlled values.")
  }
  if (!is.list(x$reason_codes) || any(vapply(x$reason_codes, function(value) {
    !is.character(value) || anyNA(value) ||
      any(!value %in% c(.PD_REASON_CODES, .PDS_REASONS)) ||
      (any(value %in% .PD_REASON_CODES) && any(value %in% .PDS_REASONS))
  }, logical(1L)))) {
    pd_stage_abort("Unit reason codes are malformed or mix registries.")
  }
  invisible(x)
}

validate_artifact_references <- function(x) {
  if (!data.table::is.data.table(x) ||
      !identical(names(x), names(pd_empty_artifact_references())) ||
      anyNA(x) || any(!x$role %in% .PDS_ARTIFACT_ROLES) ||
      any(x$manifest_generation <= 0 | x$manifest_generation %% 1 != 0)) {
    pd_stage_abort("Artifact references have an invalid schema or values.")
  }
  invisible(x)
}

pd_condition_code <- function(condition) {
  generic <- c("piperr", "pipwrn", "rlang_error", "rlang_warning", "error",
               "warning", "condition")
  code <- setdiff(class(condition), generic)
  if (length(code)) code[[1L]] else "unknown_condition"
}

pd_bounded_text <- function(x, name, allow_na = FALSE) {
  x <- pd_scalar_character(x, name, allow_na)
  if (!is.na(x) && nchar(x, type = "chars") > 4096L) {
    pd_stage_abort(paste(name, "exceeds 4096 characters."))
  }
  x
}

pd_condition_details <- function(x) {
  if (identical(x, list())) return(x)
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x))) ||
      anyDuplicated(names(x)) || length(x) > 32L ||
      any(!vapply(x, is.atomic, logical(1L))) ||
      any(lengths(x) > 100L)) {
    pd_stage_abort("Condition details must be a bounded named atomic list.")
  }
  for (value in x) {
    if (is.character(value) && any(nchar(value, type = "chars") > 4096L)) {
      pd_stage_abort("Condition detail text exceeds 4096 characters.")
    }
  }
  x[order(names(x))]
}

new_stage_condition_record <- function(
  condition = NULL, severity, code = NULL, message = NULL, classes = NULL,
  stage, entity_id = NA_character_, survey_id = NA_character_,
  pip_id = NA_character_, operation, recoverable, timestamp = Sys.time(),
  details = list()
) {
  if (!severity %in% c("warning", "error")) {
    pd_stage_abort("Condition severity is invalid.")
  }
  if (!stage %in% .PDS_STAGES || !is.logical(recoverable) ||
      length(recoverable) != 1L || is.na(recoverable)) {
    pd_stage_abort("Condition stage or recoverability is invalid.")
  }
  if (!is.null(condition) && !inherits(condition, "condition")) {
    pd_stage_abort("condition must inherit from condition.")
  }
  if (!is.null(condition) && !is.null(classes)) {
    pd_stage_abort("classes must be NULL when condition is supplied.")
  }
  code <- code %||% if (!is.null(condition)) pd_condition_code(condition) else NULL
  message <- message %||% if (!is.null(condition)) conditionMessage(condition) else NULL
  if (is.null(code) || is.null(message)) {
    pd_stage_abort("Sentinel conditions require code and message.")
  }
  classes <- if (!is.null(condition)) class(condition) else
    classes %||% unique(c(code, paste0("pipdata_stage_", severity), "condition"))
  if (!is.character(classes) || !length(classes) || length(classes) > 32L ||
      anyNA(classes) || any(!nzchar(classes))) {
    pd_stage_abort("Condition classes are invalid.")
  }
  parent <- if (!is.null(condition)) condition$parent else NULL
  record <- list(
    schema_version = .PDS_CONDITION_SCHEMA,
    condition_id = pd_random_id(),
    severity = severity,
    code = pd_bounded_text(code, "code"),
    classes = enc2utf8(classes),
    message = pd_bounded_text(message, "message"),
    stage = stage,
    entity_id = pd_scalar_character(entity_id, "entity_id", TRUE),
    survey_id = pd_scalar_character(survey_id, "survey_id", TRUE),
    pip_id = pd_scalar_character(pip_id, "pip_id", TRUE),
    operation = pd_scalar_character(operation, "operation"),
    recoverable = recoverable,
    timestamp = pd_utc_time(timestamp),
    parent_code = if (is.null(parent)) NA_character_ else pd_condition_code(parent),
    parent_message = if (is.null(parent)) NA_character_ else
      pd_bounded_text(conditionMessage(parent), "parent_message"),
    details = pd_condition_details(details)
  )
  validate_stage_condition_record(record)
  record
}

validate_stage_condition_record <- function(x, portable = FALSE) {
  expected <- c(
    "schema_version", "condition_id", "severity", "code", "classes", "message",
    "stage", "entity_id", "survey_id", "pip_id", "operation", "recoverable",
    "timestamp", "parent_code", "parent_message", "details"
  )
  if (!is.list(x) || !identical(names(x), expected) ||
      !identical(x$schema_version, .PDS_CONDITION_SCHEMA) ||
      !x$severity %in% c("warning", "error") || !x$stage %in% .PDS_STAGES ||
      !is.logical(x$recoverable) || length(x$recoverable) != 1L ||
      is.na(x$recoverable)) {
    pd_stage_abort("Condition record has an invalid schema.")
  }
  pd_scalar_character(x$condition_id, "condition_id")
  pd_bounded_text(x$code, "code")
  pd_bounded_text(x$message, "message")
  pd_scalar_character(x$operation, "operation")
  for (field in c("entity_id", "survey_id", "pip_id", "parent_code",
                  "parent_message")) {
    pd_bounded_text(x[[field]], field, TRUE)
  }
  if (!is.character(x$classes) || !length(x$classes) ||
      length(x$classes) > 32L || anyNA(x$classes) || any(!nzchar(x$classes))) {
    pd_stage_abort("Condition classes are invalid.")
  }
  if (portable) {
    pd_scalar_character(x$timestamp, "timestamp")
  } else {
    pd_utc_time(x$timestamp)
  }
  pd_condition_details(x$details)
  invisible(x)
}

pd_manifest_identity_valid <- function(x, allow_null = TRUE) {
  if (is.null(x)) return(isTRUE(allow_null))
  identical(names(x), c("filename", "uuid", "checksum", "generation")) &&
    all(vapply(x[1:3], function(value) is.character(value) &&
      length(value) == 1L && !is.na(value) && nzchar(value), logical(1L))) &&
    is.numeric(x$generation) && length(x$generation) == 1L &&
    is.finite(x$generation) && x$generation > 0 && x$generation %% 1 == 0
}

new_artifact_reference <- function(receipt, finalized, stage, entity_id,
                                   role = "primary") {
  identity <- finalized$execution$manifest_identity
  if (!pd_manifest_identity_valid(identity, FALSE) || !stage %in% .PDS_STAGES ||
      !role %in% .PDS_ARTIFACT_ROLES || !isTRUE(receipt$success)) {
    pd_stage_abort("Artifact reference lacks finalized manifest evidence.")
  }
  target_stage <- stage
  target_entity <- entity_id
  fields <- c("alias", "artifact", "path", "version_id", "content_hash")
  candidate <- pd_committed_output_receipt(
    finalized$execution$manifest,
    target_stage,
    target_entity,
    receipt$artifact
  )
  matched <- is.list(candidate) && all(fields %in% names(candidate)) &&
    all(vapply(fields, function(field) {
      identical(candidate[[field]], receipt[[field]])
    }, logical(1L)))
  if (!matched) pd_stage_abort("Receipt is absent from the finalized manifest.")
  data.table::data.table(
    entity_id = entity_id, alias = receipt$alias, artifact = receipt$artifact,
    path = receipt$path, version_id = receipt$version_id,
    content_hash = receipt$content_hash, role = role,
    manifest_generation = as.numeric(identity$generation)
  )
}

pd_stage_counts <- function(units, warnings, errors) {
  attempted <- sum(units$status %in% c("success", "failed"))
  c(
    selected = nrow(units), attempted = attempted,
    succeeded = sum(units$status == "success"),
    failed = sum(units$status == "failed"),
    skipped = sum(units$status == "skipped"),
    cached = sum(units$status == "cached"),
    warnings = length(warnings), errors = length(errors)
  ) |> as.integer() |> stats::setNames(c(
    "selected", "attempted", "succeeded", "failed", "skipped", "cached",
    "warnings", "errors"
  ))
}

pd_stage_status <- function(counts, terminal) {
  if (terminal) return(if (counts[["succeeded"]] > 0L) "partial" else "failed")
  if (counts[["succeeded"]] > 0L && counts[["failed"]] > 0L) return("partial")
  if (counts[["succeeded"]] > 0L) return("success")
  if (counts[["attempted"]] > 0L) return("failed")
  if (counts[["selected"]] > 0L && counts[["cached"]] == counts[["selected"]]) {
    return("cached")
  }
  "skipped"
}

new_pipdata_stage_result <- function(
  context, stage, terminal, units, artifacts, warnings = list(), errors = list(),
  log_ref, provenance, started_at, completed_at, data = NULL
) {
  validate_stage_units(units)
  validate_artifact_references(artifacts)
  counts <- pd_stage_counts(units, warnings, errors)
  result <- structure(list(
    schema_version = .PDS_SCHEMA, stage = stage,
    status = pd_stage_status(counts, terminal), terminal = terminal,
    run_id = context$run_id, data = data, artifacts = artifacts, units = units,
    counts = counts, log_ref = log_ref, warnings = warnings, errors = errors,
    provenance = provenance,
    input_hashes = stats::setNames(units$input_hash[!is.na(units$input_hash)],
                                   units$entity_id[!is.na(units$input_hash)]),
    output_hashes = stats::setNames(units$output_hash[!is.na(units$output_hash)],
                                    units$entity_id[!is.na(units$output_hash)]),
    started_at = pd_utc_time(started_at), completed_at = pd_utc_time(completed_at)
  ), class = c("pipdata_stage_result", "list"))
  validate_pipdata_stage_result(result, context)
  result
}

validate_pipdata_stage_result <- function(x, context = NULL, portable = FALSE) {
  expected <- c(
    "schema_version", "stage", "status", "terminal", "run_id", "data",
    "artifacts", "units", "counts", "log_ref", "warnings", "errors",
    "provenance", "input_hashes", "output_hashes", "started_at", "completed_at"
  )
  valid_schema <- is.list(x) && is.integer(x$schema_version) &&
    length(x$schema_version) == 1L &&
    x$schema_version %in% c(.PDS_LEGACY_SCHEMA, .PDS_SCHEMA)
  if (!is.list(x) || !identical(names(x), expected) ||
      !valid_schema || !x$stage %in% .PDS_STAGES ||
      !x$status %in% .PDS_STATUSES || !is.logical(x$terminal) ||
      length(x$terminal) != 1L || is.na(x$terminal) || !is.null(x$data)) {
    pd_stage_abort("Stage result has an invalid top-level schema.")
  }
  if (!is.null(context) && !identical(x$run_id, context$run_id)) {
    pd_stage_abort("Stage result run ID does not match its context.")
  }
  if (!is.list(x$warnings) || !is.list(x$errors)) {
    pd_stage_abort("Stage result conditions must be lists.")
  }
  invisible(lapply(x$warnings, validate_stage_condition_record,
                   portable = portable))
  invisible(lapply(x$errors, validate_stage_condition_record,
                   portable = portable))
  log_names <- c("name", "run_id", "summary_discriminator", "log_checkpoint")
  provenance_names_v1 <- c(
    "release", "identity", "scope_id", "context_hash", "plan_hash",
    "manifest_before", "manifest_after", "checkpoint_generations",
    "stage_reason_codes"
  )
  provenance_names <- if (identical(x$schema_version, .PDS_SCHEMA)) {
    append(
      provenance_names_v1,
      "final_evidence_manifest",
      after = match("checkpoint_generations", provenance_names_v1)
    )
  } else {
    provenance_names_v1
  }
  if (!is.list(x$log_ref) || !identical(names(x$log_ref), log_names) ||
      !identical(x$log_ref$name, "pipdata_log") ||
      !identical(x$log_ref$run_id, x$run_id) ||
      !is.null(x$log_ref$log_checkpoint) ||
      !is.list(x$provenance) ||
      !identical(names(x$provenance), provenance_names) ||
      !x$provenance$identity %in% c("PROD", "INT", "TEST") ||
      !pd_manifest_identity_valid(x$provenance$manifest_before) ||
      !pd_manifest_identity_valid(x$provenance$manifest_after) ||
      any(x$provenance$checkpoint_generations <= 0) ||
      any(x$provenance$checkpoint_generations %% 1 != 0)) {
    pd_stage_abort("Stage result references or provenance are invalid.")
  }
  final_evidence <- if (identical(x$schema_version, .PDS_SCHEMA)) {
    x$provenance$final_evidence_manifest
  } else {
    x$provenance$manifest_after
  }
  if (!pd_manifest_identity_valid(final_evidence) ||
      !is.character(x$provenance$stage_reason_codes) ||
      anyNA(x$provenance$stage_reason_codes)) {
    pd_stage_abort("Stage result final evidence is invalid.")
  }
  if (identical(x$schema_version, .PDS_SCHEMA)) {
    manifest_before <- x$provenance$manifest_before
    manifest_after <- x$provenance$manifest_after
    checkpoints <- sort(unique(x$provenance$checkpoint_generations))
    if (is.null(final_evidence) &&
        (!is.null(manifest_before) || !is.null(manifest_after))) {
      pd_stage_abort("A valid stage manifest requires final evidence.")
    }
    before_generation <- if (is.null(manifest_before)) 0 else
      manifest_before$generation
    after_generation <- if (is.null(manifest_after)) 0 else
      manifest_after$generation
    invalid_wave <- if (length(checkpoints)) {
      is.null(manifest_after) || any(checkpoints <= before_generation) ||
        any(checkpoints > after_generation) ||
        max(checkpoints) != after_generation
    } else {
      !identical(manifest_before, manifest_after)
    }
    if (invalid_wave || (!is.null(final_evidence) &&
        final_evidence$generation < after_generation)) {
      pd_stage_abort("Stage checkpoint generations do not match the wave.")
    }
  }
  if (!is.null(context) && (!identical(x$provenance$release, context$release) ||
      !identical(x$provenance$identity, context$identity) ||
      !identical(x$provenance$scope_id, context$dependency$scope_id) ||
      !identical(x$provenance$context_hash, context$dependency$context_hash) ||
      !identical(x$provenance$plan_hash, context$dependency$plan_hash))) {
    pd_stage_abort("Stage result provenance does not match its context.")
  }
  if (portable) {
    pd_scalar_character(x$started_at, "started_at")
    pd_scalar_character(x$completed_at, "completed_at")
    units <- data.table::as.data.table(x$units)
    artifacts <- data.table::as.data.table(x$artifacts)
    for (field in c("started_at", "completed_at")) {
      units[[field]] <- as.POSIXct(units[[field]], tz = "UTC")
    }
    validate_stage_units(units)
    validate_artifact_references(artifacts)
    if (!identical(x$counts, pd_stage_counts(units, x$warnings, x$errors)) ||
        !identical(x$status, pd_stage_status(x$counts, x$terminal))) {
      pd_stage_abort("Portable result counts or status are contradictory.")
    }
  } else {
    validate_stage_units(x$units)
    validate_artifact_references(x$artifacts)
    if (x$completed_at < x$started_at) pd_stage_abort("Result timestamps reverse.")
    if (!identical(x$counts, pd_stage_counts(x$units, x$warnings, x$errors)) ||
        !identical(x$status, pd_stage_status(x$counts, x$terminal))) {
      pd_stage_abort("Stage result counts or status are contradictory.")
    }
    successful <- x$units[status == "success"]
    artifact_entities <- unique(x$artifacts$entity_id)
    artifact_keys <- intersect(
      c("entity_id", "alias", "artifact", "version_id"), names(x$artifacts)
    )
    invalid_artifact_count <- if (identical(x$stage, "clean")) {
      (nrow(successful) > 0L &&
       !setequal(artifact_entities, successful$entity_id)) ||
        (nrow(successful) == 0L && nrow(x$artifacts) > 0L)
    } else {
      nrow(x$artifacts) != nrow(successful) ||
        !setequal(artifact_entities, successful$entity_id)
    }
    expected_generations <- if (identical(x$schema_version, .PDS_SCHEMA)) {
      if (is.null(final_evidence)) numeric() else final_evidence$generation
    } else {
      x$provenance$checkpoint_generations
    }
    if (invalid_artifact_count ||
        (nrow(x$artifacts) && anyDuplicated(x$artifacts[, ..artifact_keys])) ||
        !identical(sort(names(x$input_hashes)),
                   sort(x$units[!is.na(input_hash), entity_id])) ||
        !identical(sort(names(x$output_hashes)),
                   sort(successful[!is.na(output_hash), entity_id])) ||
        any(!x$artifacts$manifest_generation %in%
              expected_generations)) {
      pd_stage_abort("Stage result artifacts and hashes contradict its units.")
    }
  }
  invisible(x)
}

pd_portable_table <- function(x) {
  x <- data.table::copy(x)
  if (nrow(x)) data.table::setorderv(x, intersect(c("stage", "entity_id"), names(x)))
  out <- unclass(as.list(x))
  if ("reason_codes" %in% names(out)) {
    out$reason_codes <- lapply(out$reason_codes, function(value) {
      sort(unique(enc2utf8(value)))
    })
  }
  for (field in intersect(c("started_at", "completed_at"), names(out))) {
    out[[field]] <- format(out[[field]], "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  }
  out
}

pd_portable_condition <- function(record) {
  validate_stage_condition_record(record)
  record$classes <- enc2utf8(record$classes)
  record$details <- pd_condition_details(record$details)
  record$timestamp <- format(record$timestamp, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  record
}

pd_stage_result_portable <- function(x) {
  validate_pipdata_stage_result(x)
  out <- unclass(x)
  out$units <- pd_portable_table(x$units)
  out$artifacts <- pd_portable_table(x$artifacts)
  out$warnings <- lapply(x$warnings, pd_portable_condition)
  out$errors <- lapply(x$errors, pd_portable_condition)
  condition_order <- function(records) {
    if (length(records)) records[order(vapply(records, `[[`, "", "condition_id"))]
    else records
  }
  out$warnings <- condition_order(out$warnings)
  out$errors <- condition_order(out$errors)
  out$provenance$checkpoint_generations <-
    sort(unique(out$provenance$checkpoint_generations))
  out$provenance$stage_reason_codes <-
    sort(unique(enc2utf8(out$provenance$stage_reason_codes)))
  out$input_hashes <- out$input_hashes[order(names(out$input_hashes))]
  out$output_hashes <- out$output_hashes[order(names(out$output_hashes))]
  out$started_at <- format(x$started_at, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  out$completed_at <- format(x$completed_at, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  validate_pipdata_stage_result(out, portable = TRUE)
  out
}

pd_new_stage_outcome <- function(stage, manifest_before = NULL,
                                 started_at = Sys.time()) {
  if (!stage %in% .PD_STAGES) {
    pd_stage_abort("Stage outcome uses an invalid stage.")
  }
  list(
    stage = stage,
    units = pd_empty_stage_units(),
    receipts = list(),
    warnings = list(),
    errors = list(),
    checkpoint_generations = numeric(),
    manifest_before = manifest_before,
    manifest_after = manifest_before,
    started_at = pd_utc_time(started_at),
    completed_at = pd_utc_time(started_at)
  )
}

pd_stage_unit_row <- function(action, stage, status, reasons,
                              started_at = as.POSIXct(NA, tz = "UTC"),
                              completed_at = as.POSIXct(NA, tz = "UTC"),
                              output_hash = NA_character_) {
  if (!data.table::is.data.table(action)) {
    action <- data.table::as.data.table(action)
  }
  data.table::data.table(
    stage = stage,
    entity_id = action$entity_id[[1L]],
    survey_id = action$survey_id[[1L]] %||% NA_character_,
    pip_id = action$pip_id[[1L]] %||% NA_character_,
    status = status,
    action = action$action[[1L]] %||% NA_character_,
    reason_codes = list(sort(unique(as.character(reasons)))),
    input_hash = action$input_hash[[1L]] %||% NA_character_,
    output_hash = output_hash,
    started_at = started_at,
    completed_at = completed_at
  )
}

pd_stage_outcome_result <- function(outcome, context, execution,
                                    terminal = FALSE, log_ref) {
  stage <- outcome$stage
  final_identity <- execution$manifest_identity
  artifacts <- pd_empty_artifact_references()
  successful <- outcome$units[status == "success", entity_id]
  if (length(successful)) {
    if (!pd_manifest_identity_valid(final_identity, FALSE)) {
      pd_stage_abort("Committed stage work lacks final manifest evidence.")
    }
    finalized <- list(execution = execution)
    for (entity_id in successful) {
      receipts <- outcome$receipts[[entity_id]]
      if (is.null(receipts)) {
        pd_stage_abort("Committed stage work lacks retained receipts.")
      }
      receipts <- data.table::as.data.table(receipts)
      for (i in seq_len(nrow(receipts))) {
        receipt <- as.list(receipts[i])
        if (!"success" %in% names(receipt)) {
          receipt$success <- TRUE
        }
        artifacts <- rbind(
          artifacts,
          new_artifact_reference(
            receipt, finalized, stage, entity_id
          )
        )
      }
    }
  }
  reasons <- sort(unique(unlist(outcome$units$reason_codes)))
  if (!length(reasons)) reasons <- "no_selection"
  provenance <- list(
    release = context$release,
    identity = context$identity,
    scope_id = context$dependency$scope_id,
    context_hash = context$dependency$context_hash,
    plan_hash = context$dependency$plan_hash,
    manifest_before = outcome$manifest_before,
    manifest_after = outcome$manifest_after,
    checkpoint_generations = sort(unique(outcome$checkpoint_generations)),
    final_evidence_manifest = final_identity,
    stage_reason_codes = reasons
  )
  new_pipdata_stage_result(
    context = context,
    stage = stage,
    terminal = terminal,
    units = outcome$units,
    artifacts = artifacts,
    warnings = outcome$warnings,
    errors = outcome$errors,
    log_ref = log_ref,
    provenance = provenance,
    started_at = outcome$started_at,
    completed_at = outcome$completed_at
  )
}

#' Print a compact pipeline stage result
#' @param x A `pipdata_stage_result`.
#' @param ... Unused.
#' @export
print.pipdata_stage_result <- function(x, ...) {
  cat(sprintf("<pipdata_stage_result v%d> %s: %s%s\n", x$schema_version,
              x$stage, x$status, if (x$terminal) " (terminal)" else ""))
  cat(paste(names(x$counts), x$counts, sep = "=", collapse = ", "), "\n")
  invisible(x)
}
