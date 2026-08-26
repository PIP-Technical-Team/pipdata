.abort_dlw_acquisition_contract <- function(message) {
  rlang::abort(
    message,
    class = c("pipdata_dlw_contract_error", "piperr")
  )
}

.acquisition_has_raw_condition <- function(x) {
  if (inherits(x, "condition")) {
    return(TRUE)
  }
  if (!is.list(x)) {
    return(FALSE)
  }
  any(vapply(x, .acquisition_has_raw_condition, logical(1)))
}

.new_dlw_acquisition_failure <- function(
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
    .abort_dlw_acquisition_contract(
      "Acquisition failure `survey_id` must be one character value or `NA`."
    )
  }
  if (!is.character(phase) || length(phase) != 1L ||
      is.na(phase) || !nzchar(phase)) {
    .abort_dlw_acquisition_contract(
      "Acquisition failure `phase` must be one nonempty character value."
    )
  }

  if (!is.null(condition)) {
    if (!inherits(condition, "condition") || !is.null(error_type) ||
        !is.null(condition_msg)) {
      .abort_dlw_acquisition_contract(
        "Acquisition failures require either a condition or compact fields."
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
    .abort_dlw_acquisition_contract(
      "Acquisition failure compact fields must be nonempty character scalars."
    )
  }

  data.table::data.table(
    survey_id = survey_id,
    phase = phase,
    error_type = error_type,
    condition_msg = condition_msg
  )
}

.new_dlw_acquisition_artifact_fact <- function(
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
    .abort_dlw_acquisition_contract(
      "Acquisition artifact fact fields have invalid scalar types."
    )
  }
  if (is.null(version_id)) {
    version_id <- NA_character_
  }
  valid_version <- is.character(version_id) && length(version_id) == 1L &&
    (is.na(version_id) || nzchar(version_id))
  if (!valid_version) {
    .abort_dlw_acquisition_contract(
      "Acquisition artifact `version_id` must be nonempty or `NA`."
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
    .abort_dlw_acquisition_contract(
      "Acquisition artifact fact does not match the DLW write truth table."
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

.canonicalize_dlw_acquisition_inventory <- function(x) {
  if (!is.data.frame(x) ||
      !all(c("FileName", "Checksum") %in% names(x))) {
    .abort_dlw_acquisition_contract(
      "Acquisition inventory canonicalization requires filename/checksum keys."
    )
  }

  canonical <- data.table::as.data.table(data.table::copy(x))
  required <- c(
    "Country", "Year", "Survey_acronym", "Vermast", "Veralt", "Module",
    "Collection", "FileName", "Checksum", "Ext"
  )
  first <- intersect(required, names(canonical))
  remaining <- sort(setdiff(
    names(canonical),
    c(required, "data_available")
  ))
  last <- intersect("data_available", names(canonical))
  data.table::setcolorder(canonical, c(first, remaining, last))
  data.table::setorderv(canonical, c("FileName", "Checksum"), na.last = TRUE)

  output <- lapply(canonical, data.table::copy)
  attr(output, "stamp_pk") <- list(keys = c("Checksum", "FileName"))
  output
}

.new_dlw_acquisition_result <- function(
    outcome,
    inventory,
    summary,
    failures,
    artifacts
) {
  allowed_outcomes <- c("success", "partial", "failed", "no_work")
  if (!is.character(outcome) || length(outcome) != 1L ||
      is.na(outcome) || !outcome %in% allowed_outcomes) {
    .abort_dlw_acquisition_contract("Invalid acquisition stage outcome.")
  }
  if (!is.null(inventory) && !data.table::is.data.table(inventory)) {
    .abort_dlw_acquisition_contract(
      "Acquisition result inventory must be a data.table or `NULL`."
    )
  }

  summary_names <- c(
    "n_total", "n_success", "n_failed",
    "surveys_success", "surveys_failed"
  )
  if (!is.list(summary) || !identical(names(summary), summary_names)) {
    .abort_dlw_acquisition_contract(
      "Acquisition summary names do not match the pinned contract."
    )
  }
  count_names <- c("n_total", "n_success", "n_failed")
  valid_counts <- vapply(summary[count_names], function(x) {
    is.integer(x) && length(x) == 1L && !is.na(x) && x >= 0L
  }, logical(1))
  survey_names <- c("surveys_success", "surveys_failed")
  valid_surveys <- vapply(summary[survey_names], function(x) {
    is.character(x) && !anyNA(x) && all(nzchar(x)) && !anyDuplicated(x)
  }, logical(1))
  arithmetic_valid <- identical(
    summary$n_total,
    summary$n_success + summary$n_failed
  ) && length(summary$surveys_success) == summary$n_success &&
    length(summary$surveys_failed) == summary$n_failed
  disjoint <- length(intersect(
    summary$surveys_success,
    summary$surveys_failed
  )) == 0L
  if (!all(valid_counts) || !all(valid_surveys) ||
      !arithmetic_valid || !disjoint) {
    .abort_dlw_acquisition_contract("Invalid acquisition summary values.")
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
    .abort_dlw_acquisition_contract(
      "Invalid acquisition compact failure table."
    )
  }

  if (.acquisition_has_raw_condition(artifacts) ||
      !is.list(artifacts) || !identical(names(artifacts), "inventory") ||
      !is.list(artifacts$inventory)) {
    .abort_dlw_acquisition_contract(
      "Acquisition artifacts do not match the pinned contract."
    )
  }
  normalized_fact <- tryCatch(
    do.call(.new_dlw_acquisition_artifact_fact, artifacts$inventory),
    error = function(e) NULL
  )
  if (is.null(normalized_fact) ||
      !identical(normalized_fact, artifacts$inventory) ||
      (!is.null(inventory) && !isTRUE(normalized_fact$trustworthy))) {
    .abort_dlw_acquisition_contract("Invalid acquisition inventory fact.")
  }

  has_failure <- summary$n_failed > 0L || nrow(failures) > 0L
  commit_verified <- isTRUE(normalized_fact$success)
  no_failed_commit <- isTRUE(normalized_fact$trustworthy) &&
    !identical(normalized_fact$success, FALSE)
  outcome_valid <- switch(outcome,
    success = summary$n_success > 0L && !has_failure && commit_verified,
    partial = summary$n_success > 0L && has_failure && commit_verified,
    failed = !commit_verified || (summary$n_success == 0L && has_failure),
    no_work = summary$n_total == 0L && !has_failure && no_failed_commit
  )
  if (!isTRUE(outcome_valid)) {
    .abort_dlw_acquisition_contract(
      "Acquisition outcome is inconsistent with its summary and write fact."
    )
  }

  result <- list(
    stage = "acquisition",
    outcome = outcome,
    inventory = if (is.null(inventory)) NULL else data.table::copy(inventory),
    summary = summary,
    failures = data.table::copy(failures),
    artifacts = list(inventory = normalized_fact)
  )
  if (.acquisition_has_raw_condition(result)) {
    .abort_dlw_acquisition_contract(
      "Acquisition results cannot retain raw conditions."
    )
  }
  return(result)
}

.reconcile_dlw_persistence <- function(
    id,
    alias,
    write_result,
    intended,
    prior,
    reload,
    canonicalize,
    compare = identical,
    prior_version_id = NA_character_
) {
  scalar_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  normalize_version <- function(x) {
    if (is.null(x) || (is.character(x) && length(x) == 1L && is.na(x))) {
      return(NA_character_)
    }
    if (scalar_string(x)) {
      return(x)
    }
    NULL
  }
  copy_value <- function(x) {
    if (is.null(x)) NULL else data.table::copy(x)
  }
  fact <- function(success, trustworthy, version_id, skipped, reconciled) {
    list(
      id = id,
      alias = alias,
      attempted = TRUE,
      success = success,
      trustworthy = trustworthy,
      version_id = version_id,
      skipped = skipped,
      reconciled = reconciled
    )
  }
  unknown <- function() {
    list(
      value = NULL,
      fact = fact(FALSE, FALSE, NA_character_, FALSE, TRUE)
    )
  }
  has_condition <- function(x) {
    if (inherits(x, "condition")) {
      return(TRUE)
    }
    is.list(x) && any(vapply(x, has_condition, logical(1)))
  }

  normalized_prior_version <- normalize_version(prior_version_id)
  if (!scalar_string(id) || !scalar_string(alias) ||
      !is.function(reload) || !is.function(canonicalize) ||
      !is.function(compare) || is.null(normalized_prior_version) ||
      has_condition(intended) || has_condition(prior)) {
    rlang::abort(
      "Invalid DLW persistence reconciliation inputs.",
      class = c("pipdata_dlw_contract_error", "piperr")
    )
  }

  uncertain <- has_condition(write_result) || !is.list(write_result)
  if (!uncertain) {
    result_version <- normalize_version(write_result$version_id)
    skipped_value <- write_result$skipped
    valid_skipped <- is.null(skipped_value) ||
      (is.logical(skipped_value) && length(skipped_value) == 1L &&
        !is.na(skipped_value))
    if (is.null(result_version) || !valid_skipped) {
      uncertain <- TRUE
    } else {
      was_skipped <- isTRUE(skipped_value)
      has_version <- !is.na(result_version)
      skipped_matches_prior <- if (was_skipped && !is.null(prior)) {
        tryCatch(
          isTRUE(compare(
            canonicalize(prior),
            canonicalize(intended)
          )),
          error = function(e) FALSE
        )
      } else {
        FALSE
      }
      uncertain <- if (was_skipped) {
        !skipped_matches_prior
      } else {
        !has_version
      }
    }
  }

  if (!uncertain) {
    return(list(
      value = copy_value(intended),
      fact = fact(
        TRUE,
        TRUE,
        result_version,
        was_skipped,
        FALSE
      )
    ))
  }

  reload_failed <- FALSE
  active <- tryCatch(
    reload(),
    error = function(e) {
      reload_failed <<- TRUE
      NULL
    }
  )
  if (reload_failed || !is.list(active) ||
      !identical(names(active), c("state", "value", "version_id")) ||
      !is.character(active$state) || length(active$state) != 1L ||
      is.na(active$state) ||
      !active$state %in% c("present", "absent") ||
      has_condition(active$value)) {
    return(unknown())
  }
  active_version <- normalize_version(active$version_id)
  if (is.null(active_version)) {
    return(unknown())
  }

  if (identical(active$state, "absent")) {
    if (!is.null(active$value) || !is.null(prior) || !is.na(active_version)) {
      return(unknown())
    }
    return(list(
      value = NULL,
      fact = fact(FALSE, TRUE, NA_character_, FALSE, TRUE)
    ))
  }
  if (is.null(active$value)) {
    return(unknown())
  }

  equivalent <- function(x, y) {
    if (is.null(y)) {
      return(FALSE)
    }
    isTRUE(compare(canonicalize(x), canonicalize(y)))
  }
  intended_active <- tryCatch(
    equivalent(active$value, intended),
    error = function(e) FALSE
  )
  if (intended_active) {
    return(list(
      value = copy_value(active$value),
      fact = fact(TRUE, TRUE, active_version, FALSE, TRUE)
    ))
  }
  prior_active <- tryCatch(
    equivalent(active$value, prior),
    error = function(e) FALSE
  )
  if (prior_active) {
    recovered_version <- if (!is.na(active_version)) {
      active_version
    } else {
      normalized_prior_version
    }
    return(list(
      value = copy_value(prior),
      fact = fact(FALSE, TRUE, recovered_version, FALSE, TRUE)
    ))
  }

  unknown()
}

.validate_dlw_acquisition_arguments <- function(
    inv_gmd_list,
    check_missing,
    verbose
) {
  scalar_string <- is.character(inv_gmd_list) &&
    length(inv_gmd_list) == 1L && !is.na(inv_gmd_list) &&
    nzchar(trimws(inv_gmd_list))
  scalar_logical <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }
  if (!scalar_string || !scalar_logical(check_missing) ||
      !scalar_logical(verbose)) {
    rlang::abort(
      paste0(
        "`inv_gmd_list` must be a nonempty character scalar; ",
        "`check_missing` and `verbose` must be non-missing logical scalars."
      ),
      class = c("pipdata_dlw_argument_error", "piperr")
    )
  }
  invisible(TRUE)
}

.acquire_one_gmd <- function(candidate, local_dir, verbose) {
  candidate <- data.table::as.data.table(data.table::copy(candidate))
  if (nrow(candidate) != 1L ||
      !all(.dlw_acquisition_catalog_columns %in% names(candidate))) {
    .abort_dlw_acquisition_contract(
      "The acquisition worker requires exactly one normalized catalog row."
    )
  }
  survey_id <- .dlw_survey_id(candidate$FileName[[1L]])
  download <- tryCatch(
    dlw::dlw_get_gmd(
      country_code = candidate$Country[[1L]],
      year = candidate$Year[[1L]],
      survey = candidate$Survey_acronym[[1L]],
      module = candidate$Module[[1L]],
      vermast = candidate$Vermast[[1L]],
      veralt = candidate$Veralt[[1L]],
      filename = candidate$FileName[[1L]],
      local_dir = local_dir,
      local_overwrite = TRUE,
      verbose = verbose
    ),
    error = function(e) e
  )

  if (inherits(download, "condition")) {
    return(list(
      survey_id = survey_id,
      FileName = candidate$FileName[[1L]],
      success = FALSE,
      data_available = "No",
      failure = .new_dlw_acquisition_failure(
        survey_id = survey_id,
        phase = "download",
        condition = download
      )
    ))
  }
  if (inherits(download, "dlw_call_list")) {
    return(list(
      survey_id = survey_id,
      FileName = candidate$FileName[[1L]],
      success = FALSE,
      data_available = "No",
      failure = .new_dlw_acquisition_failure(
        survey_id = survey_id,
        phase = "download",
        error_type = "dlw_ambiguous_download_error",
        condition_msg = paste0(
          "DLW returned multiple acquisition calls for `",
          candidate$FileName[[1L]],
          "`."
        )
      )
    ))
  }

  list(
    survey_id = survey_id,
    FileName = candidate$FileName[[1L]],
    success = TRUE,
    data_available = "Yes",
    failure = .new_dlw_acquisition_failure()
  )
}

.normalize_dlw_acquisition_worker_result <- function(x, candidate) {
  candidate <- data.table::as.data.table(data.table::copy(candidate))
  expected_survey <- .dlw_survey_id(candidate$FileName[[1L]])
  expected_file <- candidate$FileName[[1L]]
  if (!is.list(x)) {
    return(list(
      survey_id = expected_survey,
      FileName = expected_file,
      success = FALSE,
      data_available = "No",
      failure = .new_dlw_acquisition_failure(
        survey_id = expected_survey,
        phase = "download",
        error_type = "download_result_error",
        condition_msg = "The acquisition worker returned an invalid result."
      )
    ))
  }
  valid_failure <- data.table::is.data.table(x$failure) &&
    identical(
      names(x$failure),
      c("survey_id", "phase", "error_type", "condition_msg")
    )
  valid <- is.list(x) &&
    is.character(x$survey_id) && length(x$survey_id) == 1L &&
    identical(x$survey_id, expected_survey) &&
    is.character(x$FileName) && length(x$FileName) == 1L &&
    identical(x$FileName, expected_file) &&
    is.logical(x$success) && length(x$success) == 1L && !is.na(x$success) &&
    is.character(x$data_available) && length(x$data_available) == 1L &&
    x$data_available %in% c("Yes", "No") && valid_failure &&
    identical(x$data_available, if (x$success) "Yes" else "No") &&
    if (x$success) nrow(x$failure) == 0L else nrow(x$failure) == 1L
  if (isTRUE(valid)) {
    return(x)
  }
  list(
    survey_id = expected_survey,
    FileName = expected_file,
    success = FALSE,
    data_available = "No",
    failure = .new_dlw_acquisition_failure(
      survey_id = expected_survey,
      phase = "download",
      error_type = "download_result_error",
      condition_msg = "The acquisition worker returned an invalid result."
    )
  )
}

.validate_dlw_acquisition_completion_logmeta <- function(x) {
  expected_names <- c(
    "info", "phase", "outcome", "n_total", "n_success", "n_failed",
    "surveys_success", "surveys_failed"
  )
  valid_count <- function(value) {
    is.integer(value) && length(value) == 1L &&
      !is.na(value) && value >= 0L
  }
  valid_ids <- function(value) {
    is.character(value) && !anyNA(value) &&
      all(nzchar(value)) && !anyDuplicated(value)
  }
  valid <- is.list(x) && identical(names(x), expected_names) &&
    identical(x$info, .logtype_dlw_acquisition) &&
    identical(x$phase, "complete") &&
    is.character(x$outcome) && length(x$outcome) == 1L &&
    !is.na(x$outcome) &&
    x$outcome %in% c("success", "partial", "failed", "no_work") &&
    all(vapply(x[c("n_total", "n_success", "n_failed")],
      valid_count,
      logical(1)
    )) &&
    valid_ids(x$surveys_success) && valid_ids(x$surveys_failed) &&
    identical(x$n_total, x$n_success + x$n_failed) &&
    length(x$surveys_success) == x$n_success &&
    length(x$surveys_failed) == x$n_failed &&
    length(intersect(x$surveys_success, x$surveys_failed)) == 0L
  if (!isTRUE(valid)) {
    .abort_dlw_acquisition_contract(
      "Acquisition completion metadata does not match the pinned schema."
    )
  }
  invisible(x)
}

.new_dlw_acquisition_completion_logmeta <- function(outcome, summary) {
  logmeta <- list(
    info = .logtype_dlw_acquisition,
    phase = "complete",
    outcome = outcome,
    n_total = summary$n_total,
    n_success = summary$n_success,
    n_failed = summary$n_failed,
    surveys_success = summary$surveys_success,
    surveys_failed = summary$surveys_failed
  )
  .validate_dlw_acquisition_completion_logmeta(logmeta)
  logmeta
}

.dlw_acquisition_empty_summary <- function() {
  list(
    n_total = 0L,
    n_success = 0L,
    n_failed = 0L,
    surveys_success = character(),
    surveys_failed = character()
  )
}

.dlw_acquisition_no_write_fact <- function(
    id,
    success = NA,
    trustworthy = TRUE,
    version_id = NA_character_
) {
  .new_dlw_acquisition_artifact_fact(
    id = id,
    alias = "dlw_inv",
    attempted = FALSE,
    success = success,
    trustworthy = trustworthy,
    version_id = version_id,
    skipped = NA,
    reconciled = FALSE
  )
}

.dlw_acquisition_write_unknown_fact <- function(id) {
  .new_dlw_acquisition_artifact_fact(
    id = id,
    alias = "dlw_inv",
    attempted = TRUE,
    success = FALSE,
    trustworthy = FALSE,
    version_id = NA_character_,
    skipped = FALSE,
    reconciled = TRUE
  )
}

.is_missing_dlw_acquisition_error <- function(x, id) {
  if (inherits(x, "pipdata_dlw_inventory_missing_error")) {
    return(TRUE)
  }
  versions <- tryCatch(
    .strict_dlw_versions(
      .dlw_acquisition_file_id(id),
      "dlw_inv"
    ),
    error = function(e) NULL
  )
  .is_valid_dlw_version_catalog(versions) && nrow(versions) == 0L
}

#' Acquire GMD catalog datasets and reconcile the local inventory
#'
#' Compares the current server catalog with `inv_gmd_list`, downloads selected
#' files, and reconciles the complete local acquisition inventory to the
#' authoritative current catalog. Acquisition actively downloads only the five
#' modules `"ALL"`, `"GROUP"`, `"HIST"`, `"GPWG"`, and `"BIN"`; the catalog and
#' validation layer also recognize `"ASPIRE"` and `"L"`.
#'
#' Every selected download is pinned to the catalog's exact `FileName` and uses
#' `local_overwrite = TRUE`. Cached or ambiguous multi-file DLW responses are
#' failures and cannot mark a row available. When `check_missing = TRUE`, current
#' five-module rows whose durable state is `data_available = "No"` are selected
#' again. Retry is inventory-driven and at least once.
#'
#' The intended inventory is assembled even when no download is selected. It
#' drops obsolete checksums and catalog-deleted rows, retains current successful
#' rows, and retains current `"ASPIRE"`/`"L"` rows only when they were already
#' available. A changed durable inventory is written once per completed attempt.
#' A thrown or malformed write result is uncertain: active storage is reloaded
#' and compared with canonical prior and intended content. The result never
#' assumes that a reported write failure rolled back.
#'
#' The persisted acquisition inventory has required columns `Country`
#' (nonempty character), `Year` (nonmissing whole-number integer),
#' `Survey_acronym`, `Vermast`, `Veralt`, `Collection`, `FileName`, and
#' `Checksum` (nonempty character), `Module` (one of the seven recognized
#' modules), `Ext = "dta"`, and `data_available` (`"Yes"` or `"No"`). Server
#' columns beyond this schema are retained in deterministic name order. A
#' normalized server catalog with zero rows is a load failure, not authoritative
#' evidence that all durable acquisition state should be deleted.
#'
#' Logging is unconditional. `dlw_acquisition_inf` entries include an attempt
#' boundary, lifecycle and failure entries, and an exact completion summary.
#'
#' @param inv_gmd_list Character scalar. Acquisition inventory artifact ID.
#'   This ID controls loading, comparison, and persistence.
#' @param check_missing Logical scalar. Retry current unresolved five-module
#'   rows when `TRUE`. Default `TRUE`.
#' @param verbose Logical. Controls verbosity of downstream
#'   [pipload::pip_write()] calls. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#'
#' @note This function expects a working release to be configured via
#'   [pipfun::setup_working_release()]. When called from
#'   [pipdata_dlw_process()], the release is already set. When called
#'   standalone, configure it first. Invalid arguments and a missing working
#'   release are caller/precondition errors and escape. Runtime folder, catalog,
#'   download, logging, and persistence failures are returned in an inspectable
#'   failed or partial result; interrupts are not converted.
#'
#' @return Invisibly, a plain unclassed list with names `stage`, `outcome`,
#'   `inventory`, `summary`, `failures`, and `artifacts`. `stage` is
#'   `"acquisition"`. `outcome` is `"success"` when one or more downloads
#'   complete without failure and the intended write is verified; `"partial"`
#'   when useful downloads complete but a worker or non-commit workflow failure
#'   occurs; `"failed"` when a required commit is unverified or no download
#'   completes while failures occur; or `"no_work"` when trustworthy discovery
#'   selects no downloads.
#'
#'   `inventory` is a copy of the trustworthy durable `data.table`, or `NULL`
#'   when durable state is absent or unknown. `summary` has exactly `n_total`,
#'   `n_success`, `n_failed`, `surveys_success`, and `surveys_failed`.
#'   `failures` is a `data.table` with `survey_id`, `phase`, `error_type`, and
#'   `condition_msg`, and never contains condition objects. `artifacts$inventory`
#'   records `id`, `alias`, `attempted`, `success`, `trustworthy`, `version_id`,
#'   `skipped`, and `reconciled` for the durable inventory write.
#' @export
#'
#' @examples
#' \dontrun{
#' pipfun::setup_working_release("20260206", "TEST")
#' pipdata_get_gmd(
#'   inv_gmd_list = "dlw_gmd_inv",
#'   check_missing = TRUE
#' )
#' }
pipdata_get_gmd <- function(
  inv_gmd_list = "dlw_gmd_inv",
  check_missing = TRUE,
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  .validate_dlw_acquisition_arguments(
    inv_gmd_list,
    check_missing,
    verbose
  )
  pipfun::get_wrk_release()

  failures <- .new_dlw_acquisition_failure()
  logging_enabled <- TRUE
  append_failure <- function(failure) {
    failures <<- data.table::rbindlist(
      list(failures, failure),
      use.names = TRUE
    )
  }
  append_log_failure <- function(condition) {
    append_failure(.new_dlw_acquisition_failure(
      phase = "log_emit",
      condition = condition
    ))
  }
  emit_info <- function(message, logmeta) {
    if (!logging_enabled) {
      return(invisible(FALSE))
    }
    log_error <- tryCatch(
      {
        pipfun::log_info(
          message,
          name = "pipdata_log",
          logmeta = logmeta
        )
        NULL
      },
      error = function(e) e
    )
    if (!is.null(log_error)) {
      append_log_failure(log_error)
      return(invisible(FALSE))
    }
    invisible(TRUE)
  }
  emit_error <- function(message, logmeta) {
    if (!logging_enabled) {
      return(invisible(FALSE))
    }
    logger_error <- tryCatch(
      {
        pipfun::log_error(
          message,
          name = "pipdata_log",
          logmeta = logmeta
        )
        NULL
      },
      error = function(e) e
    )
    if (!is.null(logger_error)) {
      append_log_failure(logger_error)
      return(invisible(FALSE))
    }
    invisible(TRUE)
  }
  derive_outcome <- function(summary, fact) {
    has_failure <- summary$n_failed > 0L || nrow(failures) > 0L
    if (summary$n_total == 0L && !has_failure &&
        isTRUE(fact$trustworthy) && !identical(fact$success, FALSE)) {
      return("no_work")
    }
    if (isTRUE(fact$attempted) && !isTRUE(fact$success)) {
      return("failed")
    }
    if (summary$n_success == 0L && has_failure) {
      return("failed")
    }
    if (summary$n_success > 0L && has_failure && isTRUE(fact$success)) {
      return("partial")
    }
    if (summary$n_success > 0L && !has_failure && isTRUE(fact$success)) {
      return("success")
    }
    "failed"
  }
  finish <- function(inventory, summary, fact) {
    outcome <- derive_outcome(summary, fact)
    if (logging_enabled) {
      completion <- .new_dlw_acquisition_completion_logmeta(
        outcome = outcome,
        summary = summary
      )
      emit_info("DLW acquisition complete.", completion)
      outcome <- derive_outcome(summary, fact)
    }
    .new_dlw_acquisition_result(
      outcome = outcome,
      inventory = inventory,
      summary = summary,
      failures = failures,
      artifacts = list(inventory = fact)
    )
  }
  fail_workflow <- function(
      phase,
      condition,
      inventory,
      fact,
      message,
      error_type = NULL
  ) {
    failure <- if (is.null(error_type)) {
      .new_dlw_acquisition_failure(
        phase = phase,
        condition = condition
      )
    } else {
      .new_dlw_acquisition_failure(
        phase = phase,
        error_type = error_type,
        condition_msg = conditionMessage(condition)
      )
    }
    append_failure(failure)
    emit_error(
      message,
      list(
        error = .logtype_dlw_acquisition,
        phase = phase,
        inventory = inv_gmd_list,
        condition_msg = failure$condition_msg[[1L]]
      )
    )
    finish(inventory, .dlw_acquisition_empty_summary(), fact)
  }

  boundary_error <- tryCatch(
    {
      pipfun::log_info(
        "DLW acquisition attempt started.",
        name = "pipdata_log",
        logmeta = list(
          info = .logtype_dlw_acquisition,
          phase = "attempt_start",
          inventory = inv_gmd_list
        )
      )
      NULL
    },
    error = function(e) e
  )
  if (!is.null(boundary_error)) {
    append_log_failure(boundary_error)
    logging_enabled <- FALSE
  }

  folder_result <- tryCatch(
    pipfun::get_pip_folders(),
    error = function(e) e
  )
  if (inherits(folder_result, "condition")) {
    fact <- .dlw_acquisition_no_write_fact(
      inv_gmd_list,
      success = FALSE,
      trustworthy = FALSE
    )
    return(invisible(fail_workflow(
      phase = "folder_resolve",
      condition = folder_result,
      inventory = NULL,
      fact = fact,
      message = "Failed to resolve DLW acquisition folders."
    )))
  }
  pip_folders <- folder_result

  for (folder in c("dlw_data", "dlw_inventory")) {
    directory_error <- tryCatch(
      {
        check_directory(pip_folders[[folder]])
        NULL
      },
      error = function(e) e
    )
    if (!is.null(directory_error)) {
      fact <- .dlw_acquisition_no_write_fact(
        inv_gmd_list,
        success = FALSE,
        trustworthy = FALSE
      )
      return(invisible(fail_workflow(
        phase = "directory_check",
        condition = directory_error,
        inventory = NULL,
        fact = fact,
        message = paste0("Failed to verify DLW folder `", folder, "`.")
      )))
    }
  }

  prior_result <- tryCatch(
    .load_dlw_acquisition_inventory(inv_gmd_list, verbose = verbose),
    error = function(e) e
  )
  if (inherits(prior_result, "condition")) {
    schema_error <- inherits(
      prior_result,
      "pipdata_dlw_catalog_schema_error"
    )
    missing_error <- .is_missing_dlw_acquisition_error(
      prior_result,
      inv_gmd_list
    )
    phase <- if (schema_error) {
      "catalog_schema"
    } else if (missing_error) {
      "inventory_missing"
    } else {
      "catalog_load"
    }
    fact <- .dlw_acquisition_no_write_fact(
      inv_gmd_list,
      success = FALSE,
      trustworthy = missing_error
    )
    error_type <- if (missing_error) "inventory_missing_error" else NULL
    return(invisible(fail_workflow(
      phase = phase,
      condition = prior_result,
      inventory = NULL,
      fact = fact,
      message = "Failed to load the local GMD acquisition inventory.",
      error_type = error_type
    )))
  }
  prior <- data.table::copy(prior_result)
  prior_version <- tryCatch(
    .dlw_acquisition_latest_version(inv_gmd_list),
    error = function(e) NA_character_
  )
  prior_fact <- .dlw_acquisition_no_write_fact(
    inv_gmd_list,
    version_id = prior_version
  )
  blocked_fact <- .dlw_acquisition_no_write_fact(
    inv_gmd_list,
    success = FALSE,
    trustworthy = TRUE,
    version_id = prior_version
  )

  server_result <- tryCatch(
    .load_dlw_acquisition_server_catalog(),
    error = function(e) e
  )
  if (inherits(server_result, "condition")) {
    phase <- if (inherits(
      server_result,
      "pipdata_dlw_catalog_schema_error"
    )) {
      "catalog_schema"
    } else {
      "catalog_load"
    }
    return(invisible(fail_workflow(
      phase = phase,
      condition = server_result,
      inventory = prior,
      fact = blocked_fact,
      message = "Failed to load the current GMD catalog."
    )))
  }
  server <- data.table::copy(server_result)

  candidates_result <- tryCatch(
    .select_dlw_acquisition_candidates(
      server,
      prior,
      check_missing = check_missing
    ),
    error = function(e) e
  )
  if (inherits(candidates_result, "condition")) {
    phase <- if (inherits(
      candidates_result,
      "pipdata_dlw_catalog_schema_error"
    )) {
      "catalog_schema"
    } else {
      "inventory_match"
    }
    return(invisible(fail_workflow(
      phase = phase,
      condition = candidates_result,
      inventory = prior,
      fact = blocked_fact,
      message = "Failed to select GMD acquisition candidates."
    )))
  }
  candidates <- data.table::copy(candidates_result)

  persist_intended <- function(intended) {
    persisted <- tryCatch(
      .persist_dlw_acquisition_inventory(
        intended = intended,
        prior = prior,
        id = inv_gmd_list,
        verbose = verbose,
        prior_version_id = prior_version
      ),
      error = function(e) e
    )
    valid_persisted <- !inherits(persisted, "condition") &&
      is.list(persisted) &&
      identical(names(persisted), c("value", "fact")) &&
      (is.null(persisted$value) || data.table::is.data.table(persisted$value)) &&
      is.list(persisted$fact)
    if (valid_persisted) {
      normalized_fact <- tryCatch(
        do.call(.new_dlw_acquisition_artifact_fact, persisted$fact),
        error = function(e) NULL
      )
      valid_persisted <- !is.null(normalized_fact) &&
        identical(normalized_fact, persisted$fact)
    }
    if (!valid_persisted && !inherits(persisted, "condition")) {
      persisted <- rlang::error_cnd(
        "pipdata_dlw_inventory_save_error",
        message = "Acquisition persistence returned an invalid result."
      )
    }
    if (inherits(persisted, "condition")) {
      append_failure(.new_dlw_acquisition_failure(
        phase = "inventory_save",
        condition = persisted
      ))
      emit_error(
        "Failed to save the GMD inventory.",
        list(
          error = .logtype_dlw_acquisition,
          phase = "inventory_save",
          artifact = inv_gmd_list,
          path = pip_folders$dlw_inventory,
          condition_msg = conditionMessage(persisted)
        )
      )
      return(list(
        value = NULL,
        fact = .dlw_acquisition_write_unknown_fact(inv_gmd_list)
      ))
    }
    if (!isTRUE(persisted$fact$success)) {
      failure <- .new_dlw_acquisition_failure(
        phase = "inventory_save",
        error_type = "inventory_save_error",
        condition_msg = "The intended GMD inventory is not durably active."
      )
      append_failure(failure)
      emit_error(
        "Failed to save the GMD inventory.",
        list(
          error = .logtype_dlw_acquisition,
          phase = "inventory_save",
          artifact = inv_gmd_list,
          path = pip_folders$dlw_inventory,
          condition_msg = failure$condition_msg[[1L]]
        )
      )
    }
    persisted
  }

  if (nrow(candidates) == 0L) {
    emit_info(
      "No new GMD data was found.",
      list(
        info = .logtype_dlw_acquisition,
        phase = "no_new_data",
        n_surveys = 0L,
        inventory = inv_gmd_list
      )
    )
    intended_result <- tryCatch(
      .merge_dlw_acquisition_inventory(
        server,
        prior,
        data.table::data.table(
          FileName = character(),
          data_available = character()
        )
      ),
      error = function(e) e
    )
    if (inherits(intended_result, "condition")) {
      return(invisible(fail_workflow(
        phase = "inventory_match",
        condition = intended_result,
        inventory = prior,
        fact = blocked_fact,
        message = "Failed to reconcile the current GMD inventory."
      )))
    }
    intended <- data.table::copy(intended_result)
    unchanged <- tryCatch(
      identical(
        .canonicalize_dlw_acquisition_inventory(intended),
        .canonicalize_dlw_acquisition_inventory(prior)
      ),
      error = function(e) e
    )
    if (inherits(unchanged, "condition")) {
      return(invisible(fail_workflow(
        phase = "inventory_match",
        condition = unchanged,
        inventory = prior,
        fact = blocked_fact,
        message = "Failed to compare the current GMD inventory."
      )))
    }
    if (isTRUE(unchanged)) {
      return(invisible(finish(
        prior,
        .dlw_acquisition_empty_summary(),
        prior_fact
      )))
    }
    persisted <- persist_intended(intended)
    return(invisible(finish(
      persisted$value,
      .dlw_acquisition_empty_summary(),
      persisted$fact
    )))
  }

  emit_info(
    "DLW acquisition started.",
    list(
      info = .logtype_dlw_acquisition,
      phase = "start",
      n_surveys = as.integer(nrow(candidates)),
      inventory = inv_gmd_list
    )
  )
  worker_results <- lapply(seq_len(nrow(candidates)), function(index) {
    candidate <- candidates[index]
    result <- tryCatch(
      .acquire_one_gmd(candidate, pip_folders$dlw_data, verbose),
      error = function(e) {
        survey_id <- .dlw_survey_id(candidate$FileName[[1L]])
        list(
          survey_id = survey_id,
          FileName = candidate$FileName[[1L]],
          success = FALSE,
          data_available = "No",
          failure = .new_dlw_acquisition_failure(
            survey_id = survey_id,
            phase = "download",
            condition = e
          )
        )
      }
    )
    result <- .normalize_dlw_acquisition_worker_result(result, candidate)
    if (!isTRUE(result$success)) {
      emit_error(
        paste0("Could not download `", result$FileName, "`."),
        list(
          error = .logtype_dlw_acquisition,
          phase = "download",
          survey = result$survey_id,
          country = candidate$Country[[1L]],
          year = candidate$Year[[1L]],
          module = candidate$Module[[1L]],
          file_name = candidate$FileName[[1L]],
          vermast = candidate$Vermast[[1L]],
          veralt = candidate$Veralt[[1L]],
          condition_msg = result$failure$condition_msg[[1L]]
        )
      )
    }
    result
  })
  worker_failures <- lapply(
    worker_results[!vapply(worker_results, `[[`, logical(1), "success")],
    `[[`,
    "failure"
  )
  if (length(worker_failures) > 0L) {
    append_failure(data.table::rbindlist(worker_failures, use.names = TRUE))
  }

  success <- vapply(worker_results, `[[`, logical(1), "success")
  survey_ids <- vapply(worker_results, `[[`, character(1), "survey_id")
  summary <- list(
    n_total = as.integer(length(worker_results)),
    n_success = as.integer(sum(success)),
    n_failed = as.integer(sum(!success)),
    surveys_success = survey_ids[success],
    surveys_failed = survey_ids[!success]
  )
  worker_status <- data.table::data.table(
    FileName = vapply(worker_results, `[[`, character(1), "FileName"),
    data_available = vapply(
      worker_results,
      `[[`,
      character(1),
      "data_available"
    )
  )
  intended_result <- tryCatch(
    .merge_dlw_acquisition_inventory(server, prior, worker_status),
    error = function(e) e
  )
  if (inherits(intended_result, "condition")) {
    append_failure(.new_dlw_acquisition_failure(
      phase = "inventory_match",
      condition = intended_result
    ))
    emit_error(
      "Failed to assemble the GMD acquisition inventory.",
      list(
        error = .logtype_dlw_acquisition,
        phase = "inventory_match",
        inventory = inv_gmd_list,
        condition_msg = conditionMessage(intended_result)
      )
    )
    return(invisible(finish(prior, summary, blocked_fact)))
  }

  persisted <- persist_intended(intended_result)
  return(invisible(finish(
    persisted$value,
    summary,
    persisted$fact
  )))
}
