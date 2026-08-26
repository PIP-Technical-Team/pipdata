.PD_CONTEXT_SCHEMA <- 1L

pd_plan_hash <- function(execution) {
  action_fields <- c(
    "stage", "entity_id", "survey_id", "pip_id", "action", "input_hash",
    "code_hash", "output_version_id", "output_hash", "data_version_id",
    "data_hash", "metadata_version_id", "metadata_hash"
  )
  actions <- data.table::copy(execution$plan$actions)
  for (field in setdiff(action_fields, names(actions))) actions[, (field) := NA_character_]
  actions <- actions[, action_fields, with = FALSE]
  data.table::setorderv(actions, c("stage", "entity_id"), na.last = TRUE)
  reason_fields <- c("stage", "entity_id", "reason", "input", "old", "new")
  reasons <- data.table::copy(execution$plan$reasons)[,
    reason_fields, with = FALSE]
  data.table::setorderv(reasons, names(reasons), na.last = TRUE)
  pd_hash_object(list(
    dependency_context = execution$context,
    actions = as.list(actions), reasons = as.list(reasons)
  ))
}

new_pipeline_context <- function(execution, run_id, options, selection, storage,
                                 created_at = Sys.time(), runtime = NULL) {
  context <- execution$context
  stage_hashes <- execution$snapshot$fingerprints$summary
  if (data.table::is.data.table(stage_hashes)) {
    stage_hashes <- stats::setNames(stage_hashes$hash, stage_hashes$stage)
  }
  result <- structure(list(
    schema_version = .PD_CONTEXT_SCHEMA,
    run_id = enc2utf8(run_id), release = enc2utf8(context$release),
    identity = context$identity, dependency_context = context,
    storage = storage, options = options, selection = selection,
    dependency = list(
      scope_id = context$scope_id,
      context_hash = pd_context_hash(context),
      manifest_before = execution$manifest_identity,
      plan_hash = pd_plan_hash(execution),
      stage_code_hashes = stage_hashes,
      snapshot_captured_at = as.POSIXct(
        execution$snapshot$captured_at %||% Sys.time(), tz = "UTC"
      ),
      bootstrap = isTRUE(options$bootstrap)
    ),
    logging = list(name = "pipdata_log", run_id = enc2utf8(run_id)),
    created_at = as.POSIXct(created_at, tz = "UTC"), runtime = runtime
  ), class = c("pipeline_context", "list"))
  validate_pipeline_context(result)
  result
}

validate_pipeline_context <- function(x, portable = FALSE) {
  expected <- c(
    "schema_version", "run_id", "release", "identity", "dependency_context",
    "storage", "options", "selection", "dependency", "logging", "created_at",
    "runtime"
  )
  storage_names <- c("aliases", "roots", "log_name")
  option_names <- c(
    "verbose", "force", "force_surveys", "bootstrap", "bootstrap_entities",
    "checkpoint_size", "checkpoint_seconds", "entity_error_policy",
    "fatal_error_policy"
  )
  selection_names <- c("survey_id", "pip_id", "force_requested")
  dependency_names <- c(
    "scope_id", "context_hash", "manifest_before", "plan_hash",
    "stage_code_hashes", "snapshot_captured_at", "bootstrap"
  )
  valid <- is.list(x) && identical(names(x), expected) &&
    identical(x$schema_version, .PD_CONTEXT_SCHEMA) &&
    x$identity %in% c("PROD", "INT", "TEST") &&
    identical(names(x$storage), storage_names) &&
    identical(names(x$options), option_names) &&
    identical(names(x$selection), selection_names) &&
    identical(names(x$dependency), dependency_names) &&
    identical(names(x$logging), c("name", "run_id")) &&
    identical(x$logging$name, "pipdata_log") &&
    identical(x$logging$run_id, x$run_id) &&
    identical(x$dependency$scope_id, x$dependency_context$scope_id) &&
    identical(x$dependency$context_hash, pd_context_hash(x$dependency_context)) &&
    identical(x$release, x$dependency_context$release) &&
    identical(x$identity, x$dependency_context$identity) &&
    pd_manifest_identity_valid(x$dependency$manifest_before)
  if (!valid) pd_stage_abort("Pipeline context has an invalid schema or identity.")
  if (isTRUE(x$options$force) && length(x$options$force_surveys)) {
    pd_stage_abort("force and force_surveys are mutually exclusive.")
  }
  if (!isTRUE(x$options$bootstrap) && length(x$options$bootstrap_entities)) {
    pd_stage_abort("bootstrap_entities requires bootstrap.")
  }
  if (!x$options$entity_error_policy %in% c("continue", "abort") ||
      !x$options$fatal_error_policy %in% c("abort", "capture_at_run_boundary") ||
      length(x$options$checkpoint_size) != 1L ||
      x$options$checkpoint_size <= 0L ||
      length(x$options$checkpoint_seconds) != 1L ||
      is.na(x$options$checkpoint_seconds) || x$options$checkpoint_seconds <= 0) {
    pd_stage_abort("Pipeline context options are invalid.")
  }
  if (portable && !is.null(x$runtime)) {
    pd_stage_abort("Portable contexts cannot contain runtime state.")
  }
  if (!portable && !is.null(x$runtime) && !is.environment(x$runtime)) {
    pd_stage_abort("Context runtime must be an environment or NULL.")
  }
  invisible(x)
}

pd_pipeline_storage <- function(dependency_context) {
  aliases <- c(
    pip = "pip", pip_meta = "pip_meta", pip_deflated = "pip_deflated",
    pip_master = "pip_master", pip_inv = "pip_inv"
  )
  roots <- vapply(dependency_context$roots[names(aliases)], identity, character(1L))
  list(aliases = aliases, roots = enc2utf8(roots), log_name = "pipdata_log")
}

pd_pipeline_options <- function(
  verbose = FALSE, force = FALSE, force_surveys = character(), bootstrap = FALSE,
  bootstrap_entities = character(), checkpoint_size = 25L,
  checkpoint_seconds = 60, entity_error_policy = "continue",
  fatal_error_policy = "abort"
) {
  list(
    verbose = isTRUE(verbose), force = isTRUE(force),
    force_surveys = sort(unique(enc2utf8(force_surveys))),
    bootstrap = isTRUE(bootstrap),
    bootstrap_entities = sort(unique(enc2utf8(bootstrap_entities))),
    checkpoint_size = as.integer(checkpoint_size),
    checkpoint_seconds = as.numeric(checkpoint_seconds),
    entity_error_policy = entity_error_policy,
    fatal_error_policy = fatal_error_policy
  )
}

#' Print compact pipeline context identity
#' @param x A `pipeline_context`.
#' @param ... Unused.
#' @export
print.pipeline_context <- function(x, ...) {
  cat(sprintf("<pipeline_context v%d> run=%s release=%s identity=%s\n",
              x$schema_version, x$run_id, x$release, x$identity))
  invisible(x)
}
