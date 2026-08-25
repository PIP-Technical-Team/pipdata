.PD_DEPENDENCY_SCHEMA <- 1L
.PD_STAGES <- c("clean", "metadata", "deflate")
.PD_ACTIONS <- c("create", "refresh", "rebuild", "none")
.PD_REASON_CODES <- c(
  "new_entity", "dlw_changed", "pfw_changed", "recode_spec_changed",
  "clean_code_changed", "metadata_code_changed", "deflate_code_changed",
  "aux_cpi_changed", "aux_ppp_changed", "aux_pop_changed",
  "aux_gdp_changed", "aux_pce_changed", "upstream_output_changed",
  "output_missing", "output_drift", "forced", "unknown_provenance"
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
  if (!is.list(x) || !identical(names(x), c("context", "actions", "reasons", "snapshot"))) {
    rlang::abort("Invalid dependency plan shape.", class = "pipdata_dependency_plan_invalid")
  }
  if (any(!x$actions$stage %in% .PD_STAGES) ||
      any(!x$actions$action %in% .PD_ACTIONS) ||
      anyDuplicated(x$actions[, .(stage, entity_id)])) {
    rlang::abort("Invalid dependency plan actions.", class = "pipdata_dependency_plan_invalid")
  }
  if (any(!x$reasons$reason %in% .PD_REASON_CODES)) {
    rlang::abort("Unknown dependency reason code.", class = "pipdata_dependency_plan_invalid")
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
