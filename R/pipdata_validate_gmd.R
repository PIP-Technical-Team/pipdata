.abort_dlw_validation_contract <- function(message) {
  rlang::abort(
    message,
    class = c("pipdata_dlw_contract_error", "piperr")
  )
}

.validation_has_raw_condition <- function(x) {
  if (inherits(x, "condition")) {
    return(TRUE)
  }
  if (!is.list(x)) {
    return(FALSE)
  }
  any(vapply(x, .validation_has_raw_condition, logical(1)))
}

.new_dlw_validation_failure <- function(
    survey_id = NA_character_,
    phase = NULL,
    condition = NULL,
    error_type = NULL,
    condition_msg = NULL
) {
  is_empty <- is.character(survey_id) && length(survey_id) == 1L &&
    is.na(survey_id) && is.null(phase) && is.null(condition) &&
    is.null(error_type) && is.null(condition_msg)
  if (is_empty) {
    return(data.table::data.table(
      survey_id = character(),
      phase = character(),
      error_type = character(),
      condition_msg = character()
    ))
  }

  if (length(survey_id) != 1L || !is.character(survey_id) ||
      (!is.na(survey_id) && !nzchar(survey_id))) {
    .abort_dlw_validation_contract(
      "Validation failure `survey_id` must be one character value or `NA`."
    )
  }
  if (!is.character(phase) || length(phase) != 1L ||
      is.na(phase) || !nzchar(phase)) {
    .abort_dlw_validation_contract(
      "Validation failure `phase` must be one nonempty character value."
    )
  }

  if (!is.null(condition)) {
    if (!inherits(condition, "condition") || !is.null(error_type) ||
        !is.null(condition_msg)) {
      .abort_dlw_validation_contract(
        "Validation failures require either a condition or compact fields."
      )
    }
    condition_classes <- setdiff(
      class(condition),
      c("rlang_error", "error", "condition")
    )
    error_type <- if (length(condition_classes) > 0L) {
      condition_classes[[1L]]
    } else {
      "unknown_error"
    }
    condition_msg <- conditionMessage(condition)
  }

  if (!is.character(error_type) || length(error_type) != 1L ||
      is.na(error_type) || !nzchar(error_type) ||
      !is.character(condition_msg) || length(condition_msg) != 1L ||
      is.na(condition_msg) || !nzchar(condition_msg)) {
    .abort_dlw_validation_contract(
      "Validation failure compact fields must be nonempty character scalars."
    )
  }

  data.table::data.table(
    survey_id = survey_id,
    phase = phase,
    error_type = error_type,
    condition_msg = condition_msg
  )
}

.new_dlw_validation_artifact_fact <- function(
    id,
    alias,
    attempted,
    success,
    trustworthy,
    version_id = NA_character_,
    skipped,
    reconciled
) {
  scalar_logical <- function(x, allow_na = FALSE) {
    is.logical(x) && length(x) == 1L && (allow_na || !is.na(x))
  }
  scalar_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }

  if (!scalar_string(id) || !scalar_string(alias) ||
      !scalar_logical(attempted) ||
      !scalar_logical(success, allow_na = TRUE) ||
      !scalar_logical(trustworthy) ||
      !scalar_logical(skipped, allow_na = TRUE) ||
      !scalar_logical(reconciled)) {
    .abort_dlw_validation_contract(
      "Validation artifact fact fields have invalid scalar types."
    )
  }
  if (is.null(version_id)) {
    version_id <- NA_character_
  }
  valid_version <- is.character(version_id) && length(version_id) == 1L &&
    (is.na(version_id) || nzchar(version_id))
  if (!valid_version) {
    .abort_dlw_validation_contract(
      "Validation artifact `version_id` must be nonempty or `NA`."
    )
  }

  no_write_loaded <- !attempted && is.na(success) && trustworthy &&
    is.na(skipped) && !reconciled
  no_write_not_reached <- !attempted && identical(success, FALSE) &&
    is.na(skipped) && !reconciled
  write_returned <- attempted && identical(success, TRUE) && trustworthy &&
    identical(skipped, FALSE) && !reconciled && !is.na(version_id)
  write_skipped <- attempted && identical(success, TRUE) && trustworthy &&
    identical(skipped, TRUE) && !reconciled
  write_recovered <- attempted && identical(success, TRUE) && trustworthy &&
    identical(skipped, FALSE) && reconciled
  write_failed <- attempted && identical(success, FALSE) &&
    identical(skipped, FALSE) && reconciled

  if (!any(c(
    no_write_loaded,
    no_write_not_reached,
    write_returned,
    write_skipped,
    write_recovered,
    write_failed
  ))) {
    .abort_dlw_validation_contract(
      "Validation artifact fact does not match the DLW write truth table."
    )
  }

  list(
    id = id,
    alias = alias,
    attempted = attempted,
    success = success,
    trustworthy = trustworthy,
    version_id = version_id,
    skipped = skipped,
    reconciled = reconciled
  )
}

.canonicalize_dlw_validation_inventory <- function(x) {
  canonical <- .normalize_dlw_validation_inventory(x)
  remaining <- sort(.dlw_validation_parsed_columns)
  data.table::setcolorder(
    canonical,
    c(.dlw_validation_core_columns, remaining)
  )
  data.table::setorderv(canonical, "survey_id", na.last = TRUE)

  output <- lapply(canonical, data.table::copy)
  attr(output, "stamp_pk") <- list(keys = "survey_id")
  output
}

.canonicalize_dlw_validation_report <- function(x) {
  canonical <- .normalize_dlw_validation_report(
    x,
    deduplicate = FALSE
  )
  lapply(canonical, data.table::copy)
}

.new_dlw_validation_result <- function(
    outcome,
    inventory,
    summary,
    failures,
    artifacts
) {
  allowed_outcomes <- c("success", "partial", "failed", "no_work")
  if (!is.character(outcome) || length(outcome) != 1L ||
      is.na(outcome) || !outcome %in% allowed_outcomes) {
    .abort_dlw_validation_contract("Invalid validation stage outcome.")
  }
  if (!is.null(inventory) && !data.table::is.data.table(inventory)) {
    .abort_dlw_validation_contract(
      "Validation result inventory must be a data.table or `NULL`."
    )
  }

  summary_names <- c(
    "n_total", "n_valid", "n_invalid", "n_failed",
    "surveys_valid", "surveys_invalid", "surveys_failed"
  )
  if (!is.list(summary) || !identical(names(summary), summary_names)) {
    .abort_dlw_validation_contract(
      "Validation summary names do not match the pinned contract."
    )
  }
  count_names <- c("n_total", "n_valid", "n_invalid", "n_failed")
  valid_counts <- vapply(summary[count_names], function(x) {
    is.integer(x) && length(x) == 1L && !is.na(x) && x >= 0L
  }, logical(1))
  survey_names <- c(
    "surveys_valid", "surveys_invalid", "surveys_failed"
  )
  valid_surveys <- vapply(summary[survey_names], function(x) {
    is.character(x) && !anyNA(x) && all(nzchar(x)) && !anyDuplicated(x)
  }, logical(1))
  arithmetic_valid <- identical(
    summary$n_total,
    summary$n_valid + summary$n_invalid + summary$n_failed
  ) && length(summary$surveys_valid) == summary$n_valid &&
    length(summary$surveys_invalid) == summary$n_invalid &&
    length(summary$surveys_failed) == summary$n_failed
  all_surveys <- unlist(summary[survey_names], use.names = FALSE)
  disjoint <- !anyDuplicated(all_surveys)
  if (!all(valid_counts) || !all(valid_surveys) ||
      !arithmetic_valid || !disjoint) {
    .abort_dlw_validation_contract("Invalid validation summary values.")
  }

  failure_names <- c(
    "survey_id", "phase", "error_type", "condition_msg"
  )
  required_failure_values <- c("phase", "error_type", "condition_msg")
  valid_failures <- data.table::is.data.table(failures) &&
    identical(names(failures), failure_names) &&
    all(vapply(failures, is.character, logical(1))) &&
    all(is.na(failures$survey_id) | nzchar(failures$survey_id)) &&
    !anyNA(failures[, required_failure_values, with = FALSE]) &&
    all(nzchar(failures$phase)) && all(nzchar(failures$error_type)) &&
    all(nzchar(failures$condition_msg))
  if (!valid_failures) {
    .abort_dlw_validation_contract(
      "Invalid validation compact failure table."
    )
  }

  if (.validation_has_raw_condition(artifacts) ||
      !is.list(artifacts) ||
      !identical(names(artifacts), c("report", "inventory"))) {
    .abort_dlw_validation_contract(
      "Validation artifacts do not match the pinned contract."
    )
  }
  normalized_artifacts <- lapply(artifacts, function(artifact) {
    if (!is.list(artifact)) {
      return(NULL)
    }
    tryCatch(
      do.call(.new_dlw_validation_artifact_fact, artifact),
      error = function(e) NULL
    )
  })
  valid_artifacts <- all(vapply(normalized_artifacts, Negate(is.null), logical(1))) &&
    identical(normalized_artifacts, artifacts)
  if (!valid_artifacts ||
      (!is.null(inventory) &&
        !isTRUE(normalized_artifacts$inventory$trustworthy))) {
    .abort_dlw_validation_contract("Invalid validation artifact facts.")
  }

  completed <- summary$n_valid + summary$n_invalid
  has_failure <- summary$n_failed > 0L || nrow(failures) > 0L
  commits_verified <- all(vapply(
    normalized_artifacts,
    function(x) {
      isTRUE(x$trustworthy) && !identical(x$success, FALSE)
    },
    logical(1)
  ))
  no_failed_commit <- all(vapply(
    normalized_artifacts,
    function(x) isTRUE(x$trustworthy) && !identical(x$success, FALSE),
    logical(1)
  ))
  outcome_valid <- switch(outcome,
    success = completed > 0L && !has_failure && commits_verified,
    partial = completed > 0L && has_failure && commits_verified,
    failed = !commits_verified || (completed == 0L && has_failure),
    no_work = summary$n_total == 0L && !has_failure && no_failed_commit
  )
  if (!isTRUE(outcome_valid)) {
    .abort_dlw_validation_contract(
      "Validation outcome is inconsistent with its summary and write facts."
    )
  }

  result <- list(
    stage = "validation",
    outcome = outcome,
    inventory = if (is.null(inventory)) NULL else data.table::copy(inventory),
    summary = summary,
    failures = data.table::copy(failures),
    artifacts = normalized_artifacts
  )
  if (.validation_has_raw_condition(result)) {
    .abort_dlw_validation_contract(
      "Validation results cannot retain raw conditions."
    )
  }
  return(result)
}

.dlw_validation_report_columns <- c(
  "table_name", "message", "type", "description", "module_type",
  "vermast", "veralt", "country_code", "rf_year"
)

.abort_dlw_validation_report_schema <- function(message) {
  rlang::abort(
    message,
    class = c("pipdata_dlw_report_schema_error", "piperr")
  )
}

.abort_dlw_validation_report_consistency <- function(message) {
  rlang::abort(
    message,
    class = c("pipdata_dlw_report_consistency_error", "piperr")
  )
}

.empty_dlw_validation_report <- function() {
  data.table::data.table(
    table_name = character(),
    message = character(),
    type = character(),
    description = character(),
    module_type = character(),
    vermast = character(),
    veralt = character(),
    country_code = character(),
    rf_year = character()
  )
}

.normalize_dlw_validation_report <- function(x, deduplicate = TRUE) {
  if (!is.data.frame(x)) {
    .abort_dlw_validation_report_schema(
      "The validation report must be tabular."
    )
  }
  report <- data.table::as.data.table(data.table::copy(x))
  missing_columns <- setdiff(.dlw_validation_report_columns, names(report))
  if (length(missing_columns) > 0L) {
    .abort_dlw_validation_report_schema(paste0(
      "Validation report is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    ))
  }
  for (column in .dlw_validation_report_columns) {
    value <- report[[column]]
    compatible <- is.atomic(value) && !is.list(value)
    if (!compatible) {
      .abort_dlw_validation_report_schema(paste0(
        "Validation report column `", column,
        "` is not character-compatible."
      ))
    }
    data.table::set(report, j = column, value = as.character(value))
  }
  invalid_table <- anyNA(report$table_name) || any(!nzchar(report$table_name))
  if (invalid_table) {
    .abort_dlw_validation_report_schema(
      "Validation report table names must be nonempty."
    )
  }
  remaining <- sort(setdiff(names(report), .dlw_validation_report_columns))
  invalid_optional <- vapply(remaining, function(column) {
    !is.atomic(report[[column]]) || is.list(report[[column]])
  }, logical(1))
  if (any(invalid_optional)) {
    .abort_dlw_validation_report_schema(paste0(
      "Validation report optional columns must be atomic: ",
      paste(remaining[invalid_optional], collapse = ", "),
      "."
    ))
  }
  data.table::setcolorder(
    report,
    c(.dlw_validation_report_columns, remaining)
  )
  if (deduplicate) {
    report <- unique(report)
  }
  leading_keys <- c("table_name", "type", "message", "description")
  sort_columns <- c(leading_keys, setdiff(names(report), leading_keys))
  if (nrow(report) > 0L) {
    sort_values <- lapply(report[, sort_columns, with = FALSE], function(value) {
      if (typeof(value) == "raw") as.integer(value) else value
    })
    row_keys <- vapply(seq_len(nrow(report)), function(index) {
      row <- lapply(report, function(value) value[index])
      paste(as.integer(serialize(row, NULL, version = 2L)), collapse = ".")
    }, character(1))
    row_order <- tryCatch(
      do.call(order, c(sort_values, list(row_keys), list(na.last = TRUE))),
      error = function(e) e
    )
    if (inherits(row_order, "condition")) {
      .abort_dlw_validation_report_schema(
        "Validation report columns cannot be ordered deterministically."
      )
    }
    report <- report[row_order]
  }
  report[]
}

.normalize_dlw_validation_report_durable <- function(x) {
  .normalize_dlw_validation_report(x, deduplicate = FALSE)
}

.dlw_optional_report_signature <- function(x) {
  attributes <- attributes(x)
  attributes$names <- NULL
  list(typeof = typeof(x), attributes = attributes)
}

.typed_missing_dlw_report_column <- function(prototype, n) {
  if (n == 0L) {
    return(prototype[0L])
  }
  if (typeof(prototype) == "raw") {
    .abort_dlw_validation_report_schema(
      "Additive raw validation report columns cannot represent typed missing values."
    )
  }
  prototype[rep(NA_integer_, n)]
}

.merge_dlw_validation_report_list <- function(reports) {
  if (!is.list(reports) || length(reports) == 0L) {
    .abort_dlw_validation_report_schema(
      "Validation report assembly requires at least one report."
    )
  }
  reports <- lapply(reports, .normalize_dlw_validation_report)
  optional <- sort(unique(unlist(lapply(
    reports,
    function(report) {
      setdiff(names(report), .dlw_validation_report_columns)
    }
  ), use.names = FALSE)))
  for (column in optional) {
    present <- vapply(reports, function(report) column %in% names(report), logical(1))
    signatures <- lapply(
      reports[present],
      function(report) .dlw_optional_report_signature(report[[column]])
    )
    compatible <- vapply(
      signatures,
      identical,
      logical(1),
      y = signatures[[1L]]
    )
    if (!all(compatible)) {
      .abort_dlw_validation_report_schema(paste0(
        "Validation report optional column `", column,
        "` has incompatible coercion attributes."
      ))
    }
    prototype <- reports[[which(present)[[1L]]]][[column]]
    for (index in which(!present)) {
      data.table::set(
        reports[[index]],
        j = column,
        value = .typed_missing_dlw_report_column(
          prototype,
          nrow(reports[[index]])
        )
      )
    }
  }
  columns <- c(.dlw_validation_report_columns, optional)
  reports <- lapply(reports, function(report) {
    data.table::setcolorder(report, columns)
    report
  })
  .normalize_dlw_validation_report(data.table::rbindlist(
    reports,
    use.names = TRUE
  ))
}

.merge_dlw_validation_reports <- function(prior, current) {
  .merge_dlw_validation_report_list(list(prior, current))
}

.assert_dlw_validation_report_consistency <- function(inventory, report) {
  inventory <- .normalize_dlw_validation_inventory(inventory)
  report <- .normalize_dlw_validation_report(report)
  inventory_ids <- sort(unique(inventory$survey_id))
  report_ids <- sort(unique(report$table_name))
  if (!identical(inventory_ids, report_ids)) {
    .abort_dlw_validation_report_consistency(
      "Validation report IDs must exactly cover completed inventory IDs."
    )
  }
  invisible(TRUE)
}

.validation_report_for_survey <- function(survey_id) {
  report <- pd_env_get("validation_report")
  if (!is.data.frame(report) || !"table_name" %in% names(report)) {
    .abort_dlw_validation_report_consistency(
      "Validation completed without an available report accumulator."
    )
  }
  report <- data.table::as.data.table(data.table::copy(report))
  rows <- report[!is.na(table_name) & table_name == survey_id]
  if (nrow(rows) == 0L) {
    .abort_dlw_validation_report_consistency(paste0(
      "Validation completed without report rows for `", survey_id, "`."
    ))
  }
  transient <- intersect(c("assertion.id", "call", "error_df"), names(rows))
  if (length(transient) > 0L) {
    rows[, (transient) := NULL]
  }
  rows[, module_type := sub(".*_(.*)", "\\1", table_name)]
  rows[, module_type := data.table::fifelse(
    module_type %in% .dlw_acquisition_modules,
    module_type,
    "OTHER"
  )]
  rows[, vermast := sub(".*_([^_]+)_M.*", "\\1", table_name)]
  rows[, veralt := sub(".*_M_([^_]+)_A.*", "\\1", table_name)]
  rows[, country_code := data.table::fifelse(
    module_type %in% .dlw_acquisition_modules,
    sub("^(.{3}).*", "\\1", table_name),
    NA_character_
  )]
  rows[, rf_year := data.table::fifelse(
    module_type %in% .dlw_acquisition_modules,
    sub("^[^_]*_([^_]*)_.*", "\\1", table_name),
    NA_character_
  )]
  .normalize_dlw_validation_report(rows)
}

.drop_validation_report_survey <- function(survey_id) {
  report <- pd_env_get("validation_report")
  if (!is.data.frame(report) || !"table_name" %in% names(report)) {
    return(invisible(NULL))
  }
  report <- data.table::as.data.table(data.table::copy(report))
  pd_env_set(
    "validation_report",
    report[is.na(table_name) | table_name != survey_id]
  )
  invisible(NULL)
}

.validate_one_gmd <- function(candidate, next_pipeline_version, verbose) {
  candidate <- .normalize_dlw_acquisition_catalog(candidate, source = "local")
  if (nrow(candidate) != 1L || candidate$data_available[[1L]] != "Yes" ||
      !is.integer(next_pipeline_version) ||
      length(next_pipeline_version) != 1L ||
      is.na(next_pipeline_version) || next_pipeline_version < 1L) {
    .abort_dlw_validation_contract(
      "The validation worker requires one available normalized candidate."
    )
  }
  survey_id <- .dlw_survey_id(candidate$FileName[[1L]])
  failure_result <- function(phase, condition) {
    list(
      survey_id = survey_id,
      status = "failed",
      inventory_row = NULL,
      report_rows = NULL,
      failure = .new_dlw_validation_failure(
        survey_id = survey_id,
        phase = phase,
        condition = condition
      )
    )
  }
  file_id <- tolower(fs::path_ext_remove(candidate$FileName[[1L]]))
  data_result <- tryCatch(
    pipload::load_dlw_data(id_name = file_id, verbose = verbose),
    error = function(e) e
  )
  if (inherits(data_result, "condition")) {
    return(failure_result("load", data_result))
  }

  artifact_id <- fs::path_ext_set(file_id, "qs2")
  info_result <- tryCatch(
    stamp::st_info(artifact_id, alias = "dlw"),
    error = function(e) e
  )
  if (inherits(info_result, "condition")) {
    return(failure_result("artifact_info_fail", info_result))
  }

  modules <- c(
    GPWG = "gpwg", GROUP = "group", BIN = "bin", HIST = "hist",
    ALL = "all", ASPIRE = "aspire", L = "l"
  )
  module <- modules[[candidate$Module[[1L]]]]
  if (is.null(module)) {
    module <- "skip"
  }
  engine_result <- tryCatch(
    dlw_validation_engine(data_result, survey_id, module),
    error = function(e) e
  )
  rm(data_result)
  if (inherits(engine_result, "condition")) {
    .drop_validation_report_survey(survey_id)
    return(failure_result("validation_engine", engine_result))
  }
  valid_engine_types <- is.data.frame(engine_result) &&
    nrow(engine_result) > 0L && "type" %in% names(engine_result) &&
    is.character(engine_result$type) && !anyNA(engine_result$type) &&
    all(nzchar(engine_result$type)) &&
    all(engine_result$type %in% c("success", "warning", "error"))
  if (!valid_engine_types) {
    .drop_validation_report_survey(survey_id)
    return(failure_result(
      "validation_engine",
      rlang::error_cnd(
        "validation_engine_result_error",
        message = "The validation engine returned an invalid result."
      )
    ))
  }
  status <- if (any(engine_result$type == "error", na.rm = TRUE)) {
    "invalid"
  } else {
    "valid"
  }

  report_result <- tryCatch(
    .validation_report_for_survey(survey_id),
    error = function(e) e
  )
  if (inherits(report_result, "condition")) {
    .drop_validation_report_survey(survey_id)
    return(failure_result("report_unavailable", report_result))
  }
  if (!identical(sort(report_result$type), sort(engine_result$type))) {
    .drop_validation_report_survey(survey_id)
    return(failure_result(
      "validation_engine",
      rlang::error_cnd(
        "validation_engine_result_error",
        message = "Validation engine and report type values do not agree."
      )
    ))
  }
  row_result <- tryCatch({
    latest_version_id <- info_result$catalog$latest_version_id
    content_hash <- info_result$sidecar$content_hash
    file_path <- info_result$sidecar$path
    metadata <- c(latest_version_id, content_hash, file_path)
    if (!is.character(metadata) || length(metadata) != 3L ||
        anyNA(metadata) || any(!nzchar(metadata))) {
      rlang::abort(
        "GMD artifact metadata is incomplete.",
        class = "validation_inventory_row_error"
      )
    }
    row <- data.table::data.table(
      survey_id = survey_id,
      pipeline_version = next_pipeline_version,
      latest_version_id = latest_version_id,
      content_hash = content_hash,
      file_path = file_path,
      status = status,
      data_available = "Yes",
      date_validated = Sys.time(),
      Checksum = candidate$Checksum[[1L]]
    )
    row <- pipload::survey_id_to_vars(row)
    .normalize_dlw_validation_inventory(row)
  }, error = function(e) e)
  if (inherits(row_result, "condition")) {
    .drop_validation_report_survey(survey_id)
    return(failure_result("inventory_row", row_result))
  }

  list(
    survey_id = survey_id,
    status = status,
    inventory_row = row_result,
    report_rows = report_result,
    failure = .new_dlw_validation_failure()
  )
}

.normalize_dlw_validation_worker_result <- function(x, candidate) {
  expected_id <- candidate$survey_id[[1L]]
  valid_failure <- is.list(x) && data.table::is.data.table(x$failure) &&
    identical(
      names(x$failure),
      c("survey_id", "phase", "error_type", "condition_msg")
    )
  valid_common <- is.list(x) &&
    is.character(x$survey_id) && length(x$survey_id) == 1L &&
    identical(x$survey_id, expected_id) &&
    is.character(x$status) && length(x$status) == 1L &&
    !is.na(x$status) && x$status %in% c("valid", "invalid", "failed") &&
    valid_failure
  completed <- isTRUE(valid_common) && x$status %in% c("valid", "invalid") &&
    data.table::is.data.table(x$inventory_row) &&
    nrow(x$inventory_row) == 1L &&
    is.data.frame(x$report_rows) && nrow(x$report_rows) > 0L &&
    nrow(x$failure) == 0L
  failed <- isTRUE(valid_common) && identical(x$status, "failed") &&
    is.null(x$inventory_row) && is.null(x$report_rows) &&
    nrow(x$failure) == 1L
  if (completed) {
    normalized <- tryCatch(list(
      inventory = .normalize_dlw_validation_inventory(x$inventory_row),
      report = .normalize_dlw_validation_report(x$report_rows)
    ), error = function(e) NULL)
    completed <- !is.null(normalized) &&
      identical(normalized$inventory$survey_id, expected_id) &&
      all(normalized$report$table_name == expected_id) &&
      identical(normalized$inventory$status, x$status) &&
      identical(normalized$inventory$Checksum, candidate$Checksum[[1L]]) &&
      identical(
        normalized$inventory$pipeline_version,
        candidate$next_pipeline_version[[1L]]
      ) &&
      all(normalized$report$type %in% c("success", "warning", "error")) &&
      identical(
        x$status,
        if (any(normalized$report$type == "error")) "invalid" else "valid"
      )
    if (completed) {
      x$inventory_row <- normalized$inventory
      x$report_rows <- normalized$report
      return(x)
    }
  }
  if (failed) {
    return(x)
  }
  list(
    survey_id = expected_id,
    status = "failed",
    inventory_row = NULL,
    report_rows = NULL,
    failure = .new_dlw_validation_failure(
      survey_id = expected_id,
      phase = "inventory_row",
      error_type = "validation_worker_result_error",
      condition_msg = "The validation worker returned an invalid result."
    )
  )
}

.validate_dlw_validation_completion_logmeta <- function(x) {
  expected <- c(
    "info", "phase", "outcome", "n_total", "n_valid", "n_invalid",
    "n_failed", "surveys_valid", "surveys_invalid", "surveys_failed"
  )
  valid_count <- function(value) {
    is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
  }
  valid_ids <- function(value) {
    is.character(value) && !anyNA(value) && all(nzchar(value)) &&
      !anyDuplicated(value)
  }
  counts <- c("n_total", "n_valid", "n_invalid", "n_failed")
  id_fields <- c("surveys_valid", "surveys_invalid", "surveys_failed")
  all_ids <- if (is.list(x)) unlist(x[id_fields], use.names = FALSE) else NULL
  valid <- is.list(x) && identical(names(x), expected) &&
    identical(x$info, .logtype_dlw_validation) &&
    identical(x$phase, "complete") &&
    is.character(x$outcome) && length(x$outcome) == 1L &&
    !is.na(x$outcome) &&
    x$outcome %in% c("success", "partial", "failed", "no_work") &&
    all(vapply(x[counts], valid_count, logical(1))) &&
    all(vapply(x[id_fields], valid_ids, logical(1))) &&
    identical(x$n_total, x$n_valid + x$n_invalid + x$n_failed) &&
    length(x$surveys_valid) == x$n_valid &&
    length(x$surveys_invalid) == x$n_invalid &&
    length(x$surveys_failed) == x$n_failed &&
    !anyDuplicated(all_ids)
  if (!isTRUE(valid)) {
    .abort_dlw_validation_contract(
      "Validation completion metadata does not match the pinned schema."
    )
  }
  invisible(x)
}

.new_dlw_validation_completion_logmeta <- function(outcome, summary) {
  value <- list(
    info = .logtype_dlw_validation,
    phase = "complete",
    outcome = outcome,
    n_total = summary$n_total,
    n_valid = summary$n_valid,
    n_invalid = summary$n_invalid,
    n_failed = summary$n_failed,
    surveys_valid = summary$surveys_valid,
    surveys_invalid = summary$surveys_invalid,
    surveys_failed = summary$surveys_failed
  )
  .validate_dlw_validation_completion_logmeta(value)
  value
}

.dlw_validation_empty_summary <- function() {
  list(
    n_total = 0L,
    n_valid = 0L,
    n_invalid = 0L,
    n_failed = 0L,
    surveys_valid = character(),
    surveys_invalid = character(),
    surveys_failed = character()
  )
}

.dlw_validation_no_write_fact <- function(
    id,
    success = NA,
    trustworthy = TRUE,
    version_id = NA_character_
) {
  .new_dlw_validation_artifact_fact(
    id = id,
    alias = "dlw_meta",
    attempted = FALSE,
    success = success,
    trustworthy = trustworthy,
    version_id = version_id,
    skipped = NA,
    reconciled = FALSE
  )
}

.dlw_validation_write_unknown_fact <- function(id) {
  .new_dlw_validation_artifact_fact(
    id = id,
    alias = "dlw_meta",
    attempted = TRUE,
    success = FALSE,
    trustworthy = FALSE,
    version_id = NA_character_,
    skipped = FALSE,
    reconciled = TRUE
  )
}

.dlw_validation_latest_version <- function(id) {
  version <- stamp::st_latest(
    .dlw_validation_file_id(id),
    alias = "dlw_meta"
  )
  if (is.null(version) || length(version) == 0L || is.na(version[[1L]])) {
    return(NA_character_)
  }
  as.character(version[[1L]])
}

.load_dlw_validation_artifact_state <- function(id, normalize, verbose) {
  value <- tryCatch(
    pipload::pip_read(
      id = id,
      alias = "dlw_meta",
      verbose = verbose
    ),
    error = function(e) e
  )
  if (!inherits(value, "condition")) {
    return(list(
      state = "present",
      value = normalize(value),
      version_id = tryCatch(
        .dlw_validation_latest_version(id),
        error = function(e) NA_character_
      )
    ))
  }
  versions <- tryCatch(
    .strict_dlw_versions(
      .dlw_validation_file_id(id),
      "dlw_meta"
    ),
    error = function(e) NULL
  )
  if (.is_valid_dlw_version_catalog(versions) && nrow(versions) == 0L) {
    return(list(
      state = "absent",
      value = NULL,
      version_id = NA_character_
    ))
  }
  rlang::cnd_signal(value)
}

.persist_dlw_validation_artifact <- function(
    intended,
    prior,
    id,
    pk = NULL,
    canonicalize,
    normalize,
    verbose,
    prior_version_id = NA_character_
) {
  write_result <- tryCatch(
    pipload::pip_write(
      x = intended,
      id = id,
      pk = pk,
      alias = "dlw_meta",
      verbose = verbose
    ),
    error = function(e) e
  )
  persisted <- .reconcile_dlw_persistence(
    id = id,
    alias = "dlw_meta",
    write_result = write_result,
    intended = intended,
    prior = prior,
    reload = function() {
      .load_dlw_validation_artifact_state(id, normalize, verbose)
    },
    canonicalize = canonicalize,
    prior_version_id = prior_version_id
  )
  persisted$fact <- do.call(
    .new_dlw_validation_artifact_fact,
    persisted$fact
  )
  persisted
}

.dlw_validation_artifact_unchanged <- function(x, y, canonicalize) {
  if (is.null(x) || is.null(y)) {
    return(identical(x, y))
  }
  identical(canonicalize(x), canonicalize(y))
}

.validate_dlw_validation_arguments <- function(
    verbose,
    acquisition_inventory_id = "dlw_gmd_inv"
) {
  valid_verbose <- is.logical(verbose) && length(verbose) == 1L &&
    !is.na(verbose)
  valid_id <- is.character(acquisition_inventory_id) &&
    length(acquisition_inventory_id) == 1L &&
    !is.na(acquisition_inventory_id) &&
    nzchar(trimws(acquisition_inventory_id))
  if (!valid_verbose || !valid_id) {
    rlang::abort(
      paste0(
        "`verbose` must be a non-missing logical scalar and the acquisition ",
        "inventory ID must be a nonempty character scalar."
      ),
      class = c("pipdata_dlw_argument_error", "piperr")
    )
  }
  invisible(TRUE)
}

.pipdata_validate_gmd_core <- function(
    acquisition_inventory_id = "dlw_gmd_inv",
    verbose = getOption("pipdata.verbose", default = TRUE)
) {
  .validate_dlw_validation_arguments(verbose, acquisition_inventory_id)
  pipfun::get_wrk_release()

  failures <- .new_dlw_validation_failure()
  logging_enabled <- TRUE
  append_failure <- function(failure) {
    failures <<- data.table::rbindlist(
      list(failures, failure),
      use.names = TRUE
    )
  }
  append_log_failure <- function(condition) {
    append_failure(.new_dlw_validation_failure(
      phase = "log_emit",
      condition = condition
    ))
  }
  emit_info <- function(message, logmeta) {
    if (!logging_enabled) {
      return(invisible(FALSE))
    }
    error <- tryCatch({
      pipfun::log_info(
        message,
        name = "pipdata_log",
        logmeta = logmeta
      )
      NULL
    }, error = function(e) e)
    if (!is.null(error)) {
      append_log_failure(error)
      return(invisible(FALSE))
    }
    invisible(TRUE)
  }
  emit_error <- function(message, logmeta) {
    if (!logging_enabled) {
      return(invisible(FALSE))
    }
    error <- tryCatch({
      pipfun::log_error(
        message,
        name = "pipdata_log",
        logmeta = logmeta
      )
      NULL
    }, error = function(e) e)
    if (!is.null(error)) {
      append_log_failure(error)
      return(invisible(FALSE))
    }
    invisible(TRUE)
  }
  derive_outcome <- function(summary, artifacts) {
    completed <- summary$n_valid + summary$n_invalid
    has_failure <- summary$n_failed > 0L || nrow(failures) > 0L
    commits_verified <- all(vapply(
      artifacts,
      function(fact) {
        isTRUE(fact$trustworthy) && !identical(fact$success, FALSE)
      },
      logical(1)
    ))
    no_failed_commit <- all(vapply(
      artifacts,
      function(fact) {
        isTRUE(fact$trustworthy) && !identical(fact$success, FALSE)
      },
      logical(1)
    ))
    if (summary$n_total == 0L && !has_failure && no_failed_commit) {
      return("no_work")
    }
    if (!commits_verified && any(vapply(
      artifacts,
      function(fact) identical(fact$success, FALSE),
      logical(1)
    ))) {
      return("failed")
    }
    if (completed == 0L && has_failure) {
      return("failed")
    }
    if (completed > 0L && has_failure && commits_verified) {
      return("partial")
    }
    if (completed > 0L && !has_failure && commits_verified) {
      return("success")
    }
    "failed"
  }
  finish <- function(inventory, summary, artifacts) {
    outcome <- derive_outcome(summary, artifacts)
    if (logging_enabled) {
      emit_info(
        "DLW validation complete.",
        .new_dlw_validation_completion_logmeta(outcome, summary)
      )
      outcome <- derive_outcome(summary, artifacts)
    }
    .new_dlw_validation_result(
      outcome = outcome,
      inventory = inventory,
      summary = summary,
      failures = failures,
      artifacts = artifacts
    )
  }
  fail_workflow <- function(
      phase,
      condition,
      artifacts,
      inventory = NULL,
      error_type = NULL
  ) {
    failure <- if (is.null(error_type)) {
      .new_dlw_validation_failure(
        phase = phase,
        condition = condition
      )
    } else {
      .new_dlw_validation_failure(
        phase = phase,
        error_type = error_type,
        condition_msg = conditionMessage(condition)
      )
    }
    append_failure(failure)
    emit_error(
      "DLW validation workflow failed.",
      list(
        error = .logtype_dlw_validation,
        phase = phase,
        inventory = acquisition_inventory_id,
        condition_msg = failure$condition_msg[[1L]]
      )
    )
    finish(inventory, .dlw_validation_empty_summary(), artifacts)
  }

  boundary_error <- tryCatch({
    pipfun::log_info(
      "DLW validation attempt started.",
      name = "pipdata_log",
      logmeta = list(
        info = .logtype_dlw_validation,
        phase = "attempt_start",
        inventory = acquisition_inventory_id
      )
    )
    NULL
  }, error = function(e) e)
  if (!is.null(boundary_error)) {
    append_log_failure(boundary_error)
    logging_enabled <- FALSE
  }

  unknown_artifacts <- list(
    report = .dlw_validation_no_write_fact(
      "validation_report", FALSE, FALSE
    ),
    inventory = .dlw_validation_no_write_fact(
      "gmd_valid_inv", FALSE, FALSE
    )
  )
  folders_result <- tryCatch(
    pipfun::get_pip_folders(),
    error = function(e) e
  )
  if (inherits(folders_result, "condition")) {
    return(invisible(fail_workflow(
      "folder_resolve",
      folders_result,
      unknown_artifacts
    )))
  }
  pip_folders <- folders_result
  for (folder in c("dlw_data", "dlw_inventory", "dlw_metadata")) {
    directory_error <- tryCatch({
      check_directory(pip_folders[[folder]])
      NULL
    }, error = function(e) e)
    if (!is.null(directory_error)) {
      return(invisible(fail_workflow(
        "directory_check",
        directory_error,
        unknown_artifacts
      )))
    }
  }

  acquisition_result <- tryCatch(
    .load_dlw_acquisition_inventory(
      acquisition_inventory_id,
      verbose = verbose
    ),
    error = function(e) e
  )
  if (inherits(acquisition_result, "condition")) {
    schema_error <- inherits(
      acquisition_result,
      "pipdata_dlw_catalog_schema_error"
    )
    missing_error <- .is_missing_dlw_acquisition_error(
      acquisition_result,
      acquisition_inventory_id
    )
    phase <- if (schema_error) {
      "catalog_schema"
    } else if (missing_error) {
      "inventory_missing"
    } else {
      "catalog_load"
    }
    return(invisible(fail_workflow(
      phase,
      acquisition_result,
      unknown_artifacts,
      error_type = if (missing_error) "inventory_missing_error" else NULL
    )))
  }
  acquisition <- data.table::copy(acquisition_result)

  inventory_state_result <- tryCatch(
    .load_dlw_validation_artifact_state(
      "gmd_valid_inv",
      .normalize_dlw_validation_inventory,
      verbose
    ),
    error = function(e) e
  )
  if (inherits(inventory_state_result, "condition")) {
    phase <- if (inherits(
      inventory_state_result,
      "pipdata_dlw_inventory_schema_error"
    )) "inventory_schema" else "inv_load_fail"
    return(invisible(fail_workflow(
      phase,
      inventory_state_result,
      unknown_artifacts
    )))
  }
  inventory_state <- inventory_state_result
  prior_inventory <- inventory_state$value

  report_state_result <- tryCatch(
    .load_dlw_validation_artifact_state(
      "validation_report",
      .normalize_dlw_validation_report_durable,
      verbose
    ),
    error = function(e) e
  )
  if (inherits(report_state_result, "condition")) {
    phase <- if (inherits(
      report_state_result,
      "pipdata_dlw_report_schema_error"
    )) "report_schema" else "report_load_fail"
    known_inventory_artifacts <- list(
      report = .dlw_validation_no_write_fact(
        "validation_report", FALSE, FALSE
      ),
      inventory = .dlw_validation_no_write_fact(
        "gmd_valid_inv",
        FALSE,
        TRUE,
        inventory_state$version_id
      )
    )
    return(invisible(fail_workflow(
      phase,
      report_state_result,
      known_inventory_artifacts,
      inventory = prior_inventory
    )))
  }
  report_state <- report_state_result

  history_result <- tryCatch(
    .scan_dlw_validation_history(verbose = verbose),
    error = function(e) e
  )
  if (inherits(history_result, "condition")) {
    phase <- if (inherits(
      history_result,
      "pipdata_dlw_inventory_schema_error"
    )) "inventory_schema" else "inv_load_fail"
    blocked <- list(
      report = .dlw_validation_no_write_fact(
        "validation_report", FALSE, TRUE, report_state$version_id
      ),
      inventory = .dlw_validation_no_write_fact(
        "gmd_valid_inv", FALSE, TRUE, inventory_state$version_id
      )
    )
    return(invisible(fail_workflow(
      phase,
      history_result,
      blocked,
      inventory = prior_inventory
    )))
  }
  history <- history_result
  if (!is.null(prior_inventory) && nrow(prior_inventory) > 0L) {
    history <- data.table::rbindlist(list(
      history,
      prior_inventory[, .(
        pipeline_version = max(pipeline_version)
      ), by = survey_id]
    ))[, .(
      pipeline_version = max(pipeline_version)
    ), by = survey_id]
  }
  state_result <- tryCatch(
    .reconcile_dlw_validation_inventory(
      acquisition,
      prior_inventory,
      history
    ),
    error = function(e) e
  )
  if (inherits(state_result, "condition")) {
    phase <- if (inherits(
      state_result,
      "pipdata_dlw_catalog_schema_error"
    )) "catalog_schema" else "inventory_schema"
    blocked <- list(
      report = .dlw_validation_no_write_fact(
        "validation_report", FALSE, TRUE, report_state$version_id
      ),
      inventory = .dlw_validation_no_write_fact(
        "gmd_valid_inv", FALSE, TRUE, inventory_state$version_id
      )
    )
    return(invisible(fail_workflow(
      phase,
      state_result,
      blocked,
      inventory = prior_inventory
    )))
  }
  state <- state_result

  prior_report <- if (identical(report_state$state, "absent")) {
    .empty_dlw_validation_report()
  } else {
    data.table::copy(report_state$value)
  }
  intended_report <- .normalize_dlw_validation_report(
    prior_report[table_name %in% state$inventory$survey_id]
  )
  consistency_error <- tryCatch({
    .assert_dlw_validation_report_consistency(
      state$inventory,
      intended_report
    )
    NULL
  }, error = function(e) e)
  if (!is.null(consistency_error)) {
    blocked <- list(
      report = .dlw_validation_no_write_fact(
        "validation_report", FALSE, TRUE, report_state$version_id
      ),
      inventory = .dlw_validation_no_write_fact(
        "gmd_valid_inv", FALSE, TRUE, inventory_state$version_id
      )
    )
    return(invisible(fail_workflow(
      "report_consistency",
      consistency_error,
      blocked,
      inventory = prior_inventory
    )))
  }

  commit <- function(intended_inventory, intended_report, summary) {
    consistency_error <- tryCatch({
      .assert_dlw_validation_report_consistency(
        intended_inventory,
        intended_report
      )
      NULL
    }, error = function(e) e)
    if (!is.null(consistency_error)) {
      blocked <- list(
        report = .dlw_validation_no_write_fact(
          "validation_report", FALSE, TRUE, report_state$version_id
        ),
        inventory = .dlw_validation_no_write_fact(
          "gmd_valid_inv", FALSE, TRUE, inventory_state$version_id
        )
      )
      append_failure(.new_dlw_validation_failure(
        phase = "report_consistency",
        condition = consistency_error
      ))
      emit_error(
        "Validation report coverage is inconsistent.",
        list(
          error = .logtype_dlw_validation,
          phase = "report_consistency",
          condition_msg = conditionMessage(consistency_error)
        )
      )
      return(finish(prior_inventory, summary, blocked))
    }

    report_changed <- if (identical(report_state$state, "absent")) {
      nrow(intended_report) > 0L
    } else {
      !.dlw_validation_artifact_unchanged(
        intended_report,
        report_state$value,
        .canonicalize_dlw_validation_report
      )
    }
    if (report_changed) {
      report_persisted <- tryCatch(
        .persist_dlw_validation_artifact(
          intended = intended_report,
          prior = report_state$value,
          id = "validation_report",
          canonicalize = .canonicalize_dlw_validation_report,
          normalize = .normalize_dlw_validation_report_durable,
          verbose = verbose,
          prior_version_id = report_state$version_id
        ),
        error = function(e) e
      )
      if (inherits(report_persisted, "condition")) {
        report_persisted <- list(
          value = NULL,
          fact = .dlw_validation_write_unknown_fact("validation_report")
        )
      }
      report_fact <- report_persisted$fact
      if (!isTRUE(report_fact$success)) {
        failure <- .new_dlw_validation_failure(
          phase = "report_save",
          error_type = "report_save_error",
          condition_msg = "The intended validation report is not durably active."
        )
        append_failure(failure)
        emit_error(
          "Failed to save the validation report.",
          list(
            error = .logtype_dlw_validation,
            phase = "report_save",
            artifact = "validation_report",
            condition_msg = failure$condition_msg[[1L]]
          )
        )
        blocked_inventory <- .dlw_validation_no_write_fact(
          "gmd_valid_inv",
          FALSE,
          TRUE,
          inventory_state$version_id
        )
        return(finish(
          prior_inventory,
          summary,
          list(report = report_fact, inventory = blocked_inventory)
        ))
      }
      emit_info(
        "Validation report saved.",
        list(
          info = .logtype_dlw_validation,
          phase = "report_save",
          artifact = "validation_report"
        )
      )
    } else {
      report_fact <- .dlw_validation_no_write_fact(
        "validation_report",
        version_id = report_state$version_id
      )
    }

    inventory_changed <- if (identical(inventory_state$state, "absent")) {
      nrow(intended_inventory) > 0L
    } else {
      !.dlw_validation_artifact_unchanged(
        intended_inventory,
        inventory_state$value,
        .canonicalize_dlw_validation_inventory
      )
    }
    if (inventory_changed) {
      inventory_persisted <- tryCatch(
        .persist_dlw_validation_artifact(
          intended = intended_inventory,
          prior = inventory_state$value,
          id = "gmd_valid_inv",
          pk = "survey_id",
          canonicalize = .canonicalize_dlw_validation_inventory,
          normalize = .normalize_dlw_validation_inventory,
          verbose = verbose,
          prior_version_id = inventory_state$version_id
        ),
        error = function(e) e
      )
      if (inherits(inventory_persisted, "condition")) {
        inventory_persisted <- list(
          value = NULL,
          fact = .dlw_validation_write_unknown_fact("gmd_valid_inv")
        )
      }
      inventory_fact <- inventory_persisted$fact
      result_inventory <- inventory_persisted$value
      if (!isTRUE(inventory_fact$success)) {
        failure <- .new_dlw_validation_failure(
          phase = "inventory_save",
          error_type = "inventory_save_error",
          condition_msg = paste0(
            "The intended completed validation inventory is not durably active."
          )
        )
        append_failure(failure)
        emit_error(
          "Failed to save the validation inventory.",
          list(
            error = .logtype_dlw_validation,
            phase = "inventory_save",
            artifact = "gmd_valid_inv",
            condition_msg = failure$condition_msg[[1L]]
          )
        )
      } else {
        emit_info(
          "Validation inventory saved.",
          list(
            info = .logtype_dlw_validation,
            phase = "inventory_save",
            artifact = "gmd_valid_inv"
          )
        )
      }
    } else {
      inventory_fact <- .dlw_validation_no_write_fact(
        "gmd_valid_inv",
        version_id = inventory_state$version_id
      )
      result_inventory <- inventory_state$value
    }
    finish(
      result_inventory,
      summary,
      list(report = report_fact, inventory = inventory_fact)
    )
  }

  candidates <- state$candidates
  if (nrow(candidates) == 0L) {
    emit_info(
      "No new GMD data was available for validation.",
      list(
        info = .logtype_dlw_validation,
        phase = "no_new_data",
        n_surveys = 0L
      )
    )
    return(invisible(commit(
      state$inventory,
      intended_report,
      .dlw_validation_empty_summary()
    )))
  }

  reset_error <- tryCatch({
    pd_env_rm("validation_report")
    NULL
  }, error = function(e) e)
  if (!is.null(reset_error)) {
    blocked <- list(
      report = .dlw_validation_no_write_fact(
        "validation_report", FALSE, TRUE, report_state$version_id
      ),
      inventory = .dlw_validation_no_write_fact(
        "gmd_valid_inv", FALSE, TRUE, inventory_state$version_id
      )
    )
    return(invisible(fail_workflow(
      "report_unavailable",
      reset_error,
      blocked,
      inventory = prior_inventory
    )))
  }
  emit_info(
    "DLW validation started.",
    list(
      info = .logtype_dlw_validation,
      phase = "start",
      n_surveys = as.integer(nrow(candidates))
    )
  )

  worker_results <- lapply(seq_len(nrow(candidates)), function(index) {
    candidate <- candidates[index]
    result <- tryCatch(
      .validate_one_gmd(
        candidate,
        candidate$next_pipeline_version[[1L]],
        verbose
      ),
      error = function(e) e
    )
    if (inherits(result, "condition")) {
      survey_id <- candidate$survey_id[[1L]]
      return(list(
        survey_id = survey_id,
        status = "failed",
        inventory_row = NULL,
        report_rows = NULL,
        failure = .new_dlw_validation_failure(
          survey_id = survey_id,
          phase = "inventory_row",
          condition = result
        )
      ))
    }
    .normalize_dlw_validation_worker_result(result, candidate)
  })
  statuses <- vapply(worker_results, `[[`, character(1), "status")
  survey_ids <- vapply(worker_results, `[[`, character(1), "survey_id")
  summary <- list(
    n_total = as.integer(length(worker_results)),
    n_valid = as.integer(sum(statuses == "valid")),
    n_invalid = as.integer(sum(statuses == "invalid")),
    n_failed = as.integer(sum(statuses == "failed")),
    surveys_valid = survey_ids[statuses == "valid"],
    surveys_invalid = survey_ids[statuses == "invalid"],
    surveys_failed = survey_ids[statuses == "failed"]
  )
  worker_failures <- list()
  for (result in worker_results) {
    if (nrow(result$failure) > 0L) {
      worker_failures[[length(worker_failures) + 1L]] <- result$failure
      emit_error(
        "GMD validation execution failed.",
        list(
          error = .logtype_dlw_validation,
          phase = result$failure$phase[[1L]],
          survey = result$survey_id,
          condition_msg = result$failure$condition_msg[[1L]]
        )
      )
    } else if (identical(result$status, "invalid")) {
      emit_error(
        "GMD validation classified data as invalid.",
        list(
          error = .logtype_dlw_validation,
          phase = "validation",
          survey = result$survey_id
        )
      )
    }
  }
  if (length(worker_failures) > 0L) {
    append_failure(data.table::rbindlist(worker_failures, use.names = TRUE))
  }

  completed <- worker_results[statuses %in% c("valid", "invalid")]
  intended_inventory <- state$inventory
  worker_inventory <- lapply(completed, `[[`, "inventory_row")
  if (length(worker_inventory) > 0L) {
    inventory_result <- tryCatch(.normalize_dlw_validation_inventory(
      data.table::rbindlist(
        c(list(intended_inventory), worker_inventory),
        use.names = TRUE
      )
    ), error = function(e) e)
    if (inherits(inventory_result, "condition")) {
      append_failure(.new_dlw_validation_failure(
        phase = "inventory_schema",
        condition = inventory_result
      ))
      emit_error(
        "Completed validation inventory assembly failed.",
        list(
          error = .logtype_dlw_validation,
          phase = "inventory_schema",
          condition_msg = conditionMessage(inventory_result)
        )
      )
      blocked <- list(
        report = .dlw_validation_no_write_fact(
          "validation_report", FALSE, TRUE, report_state$version_id
        ),
        inventory = .dlw_validation_no_write_fact(
          "gmd_valid_inv", FALSE, TRUE, inventory_state$version_id
        )
      )
      return(invisible(finish(prior_inventory, summary, blocked)))
    }
    intended_inventory <- inventory_result
  }
  worker_reports <- lapply(completed, `[[`, "report_rows")
  if (length(worker_reports) > 0L) {
    report_result <- tryCatch(
      .merge_dlw_validation_report_list(c(list(intended_report), worker_reports)),
      error = function(e) e
    )
    if (inherits(report_result, "condition")) {
      phase <- if (inherits(
        report_result,
        "pipdata_dlw_report_consistency_error"
      )) "report_consistency" else "report_schema"
      append_failure(.new_dlw_validation_failure(
        phase = phase,
        condition = report_result
      ))
      emit_error(
        "Validation report assembly failed.",
        list(
          error = .logtype_dlw_validation,
          phase = phase,
          condition_msg = conditionMessage(report_result)
        )
      )
      blocked <- list(
        report = .dlw_validation_no_write_fact(
          "validation_report", FALSE, TRUE, report_state$version_id
        ),
        inventory = .dlw_validation_no_write_fact(
          "gmd_valid_inv", FALSE, TRUE, inventory_state$version_id
        )
      )
      return(invisible(finish(prior_inventory, summary, blocked)))
    }
    intended_report <- report_result
  }
  return(invisible(commit(
    intended_inventory,
    intended_report,
    summary
  )))
}

#' Validate available GMD data and commit completed validation state
#'
#' Validates current available acquisition rows with [dlw_validation_engine()].
#' The engine receives mappings for all seven recognized modules: `"ALL"`,
#' `"GROUP"`, `"HIST"`, `"GPWG"`, `"BIN"`, `"ASPIRE"`, and `"L"`. A completed
#' engine result is classified as `"valid"` or `"invalid"`; invalid data is a
#' completed validation, not an execution failure.
#'
#' `gmd_valid_inv` is authoritative completed-data state. It contains only
#' `data_available = "Yes"` rows with status `"valid"` or `"invalid"`. Load,
#' artifact-info, engine, and inventory-row failures produce no inventory or
#' report row and therefore retry because their current acquisition key remains
#' absent. Before pruning stale checksums, historical inventory versions are
#' scanned by `survey_id`; the next completed `pipeline_version` is one plus the
#' historical maximum, or `1L` with no history. Failed attempts consume no
#' version. Every catalog-listed historical version must be readable and
#' schema-valid; otherwise validation blocks rather than understating history.
#'
#' Completed inventory rows have exactly the core fields `survey_id`,
#' `pipeline_version`, `latest_version_id`, `content_hash`, `file_path`,
#' `status`, `data_available`, `date_validated`, and `Checksum`, plus parsed
#' identity fields `country_code`, `surveyid_year`, `survey_acronym`, `vermast`,
#' `veralt`, `collection`, `module`, and `tool`. Character identity and artifact
#' fields are nonmissing and nonempty; `pipeline_version` is a positive integer;
#' `surveyid_year` is a nonnegative integer; `date_validated` is nonmissing
#' Date/POSIX time; `status` is `"valid"` or `"invalid"`; and
#' `data_available = "Yes"`.
#'
#' Validation inventory and report state are reconciled to current available
#' acquisition `survey_id`/`Checksum` keys on every call, including no-work
#' calls. The normalized report must exactly cover completed inventory IDs and
#' exact full-row duplicates are removed. Completed worker rows are assembled in
#' memory, then `validation_report` is verified first and `gmd_valid_inv` is
#' committed last. Every uncertain write is reloaded and compared with canonical
#' prior and intended content; unreadable or ambiguous durable state is not
#' overwritten as though it were trustworthy.
#'
#' Persisted reports require character-compatible `table_name`, `message`,
#' `type`, `description`, `module_type`, `vermast`, `veralt`, `country_code`, and
#' `rf_year`. Optional columns may be added only when same-name columns have
#' identical coercion-relevant attributes, including class, factor levels and
#' ordering, units, and time zone. Exact rows are deduplicated and canonical row
#' order uses every persisted column. Engine `type` values must be nonempty
#' character values in `success`, `warning`, or `error` and agree with the
#' report. If the engine completes but its report rows are unavailable, the
#' survey records `phase = "report_unavailable"`, persists no completed row or
#' report row, and is selected again on the next call.
#'
#' Logging is unconditional. `dlw_validation_inf` entries include an attempt
#' boundary, lifecycle and failure entries, and an exact completion summary that
#' separates valid, invalid, and execution-failed surveys.
#'
#' @note This function expects a working release to be configured via
#'   [pipfun::setup_working_release()]. When called from
#'   [pipdata_dlw_process()], the release is already set. When called
#'   standalone, configure it first. This exported function reads the default
#'   `"dlw_gmd_inv"`; [pipdata_dlw_process()] routes its `inv_gmd_list` value to
#'   the same validation implementation. Invalid arguments and a missing working
#'   release escape as caller/precondition errors. Runtime folder, artifact,
#'   schema, worker, logging, and persistence failures return inspectable
#'   results; interrupts are not converted.
#'
#' @param verbose Logical scalar. Controls verbosity of downstream I/O calls
#'   (including [pipload::pip_write()]). Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#' @return Invisibly, a plain unclassed list with names `stage`, `outcome`,
#'   `inventory`, `summary`, `failures`, and `artifacts`. `stage` is
#'   `"validation"`. `outcome` is `"success"` when one or more validations
#'   complete and required commits are verified; `"partial"` when a valid or
#'   invalid result completes alongside an execution or non-commit workflow
#'   failure; `"failed"` when a required commit is unverified or no validation
#'   completes while failures occur; or `"no_work"` when trustworthy discovery
#'   selects no validation workers. Invalid classifications alone do not make an
#'   outcome partial or failed.
#'
#'   `inventory` is a copy of trustworthy durable completed state, or `NULL`
#'   when that state is absent or unknown. `summary` has exactly `n_total`,
#'   `n_valid`, `n_invalid`, `n_failed`, `surveys_valid`, `surveys_invalid`, and
#'   `surveys_failed`; totals count terminal worker outcomes. `failures` is a
#'   `data.table` with `survey_id`, `phase`, `error_type`, and `condition_msg`.
#'   `artifacts` contains `report` and `inventory` facts, each with `id`, `alias`,
#'   `attempted`, `success`, `trustworthy`, `version_id`, `skipped`, and
#'   `reconciled`. The full validation report is not returned.
#' @export
#'
#' @examples
#' \dontrun{
#' pipfun::setup_working_release("20260206", "TEST")
#' pipdata_validate_gmd()
#' }
pipdata_validate_gmd <- function(
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  .validate_dlw_validation_arguments(verbose)
  invisible(.pipdata_validate_gmd_core(
    acquisition_inventory_id = "dlw_gmd_inv",
    verbose = verbose
  ))
}


#' Check whether the working folder exists and abort if it does not
#'
#' @param wrk_folder A working folder path
#'
#' @returns Message if working folder is not available
#' @export
#'
#' @examples
#' \dontrun{
#' check_directory(dlw_data)
#' }
check_directory <- function(wrk_folder) {
  if (!dir.exists(wrk_folder)) {
    cli::cli_abort(
      "Folder {.dir {wrk_folder}} is not available"
    )
  }
}

#' Get datasets list that needs to be validated
#'
#' This function filters and returns the subset of new GMD records that match the validated inventory.
#'
#' @param gmd_new A data.table containing the new GMD records. Must include
#' columns `FileName` and `Checksum`.
#' @param inv_validated A data.table of validated inventory records
#' with `survey_id` and `Checksum` columns.
#'
#' @return A data.table containing only GMD records that match the validated inventory.
#' Returns all of `gmd_new` if `inv_validated` is NULL or empty. Result is returned invisibly.
#'
#' @examples
#' \dontrun{
#' validated_records <- gmd_to_validate(gmd_new, inv_validated)
#' }
gmd_to_validate <- function(gmd_new, inv_validated) {

  stopifnot("GMD new dataset is not loaded" = !is.null(gmd_new))

  # generate survey id to be used to merge the data with inventory file
  gmd_new0 <- gmd_new[
    , survey_id := as.character(fs::path_ext_remove(FileName))
  ][
    , .(survey_id, Checksum)
  ]

  if (is.null(inv_validated) || nrow(inv_validated) == 0) return(gmd_new)

  # keep records that are going to be validated
  new_gmd <- joyn::right_join(
    inv_validated,
    gmd_new0,
    by = c("survey_id", "Checksum"),
    reportvar = FALSE,
    verbose = FALSE
  )

  return(invisible(new_gmd))
}


#' Return Validated GMD Records
#'
#' This function filters the GMD dataset to return only the records that match entries in the validated inventory.
#'
#' @param gmd_new A data.table containing new GMD records. Must include
#' columns `FileName` and `Checksum`.
#' @param inv_validated A data.table of validated inventory records
#' with `survey_id` and `Checksum` columns.
#'
#' @return A data.table with only validated GMD records that exist in both `gmd_new` and `inv_validated`.
#'         Returns `NULL` if `inv_validated` is NULL or empty. Result is returned invisibly.
#'
#' @examples
#' \dontrun{
#' validated_gmd <- gmd_validated(gmd_new, inv_validated)
#' }
gmd_validated <- function(gmd_new, inv_validated) {

  stopifnot("GMD new dataset is not loaded" = !is.null(gmd_new))

  # generate survey id to be used to merge the data with inventory file
  gmd_new0 <- gmd_new[
    , survey_id := as.character(fs::path_ext_remove(FileName))
  ][
    , .(survey_id, Checksum)
  ]

  if (is.null(inv_validated) || nrow(inv_validated) == 0) return(NULL)

  # keep only validated GMD entries
  # reportvar left TRUE (default) intentionally: .joyn == "x" filters to
  # rows present in inv_validated only (right_join semantics via full_join).
  gmd_validated_records <- joyn::full_join(
    inv_validated,
    gmd_new0,
    by = c("survey_id", "Checksum"),
    verbose = FALSE
  )
  gmd_validated_records <- gmd_validated_records[`.joyn` == "x", !c(".joyn")]

  return(invisible(gmd_validated_records))
}
