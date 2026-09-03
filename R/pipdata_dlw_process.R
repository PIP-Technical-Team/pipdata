.abort_dlw_wrapper_contract <- function(message) {
  rlang::abort(
    message,
    class = c("pipdata_dlw_contract_error", "piperr")
  )
}

.validate_dlw_wrapper_arguments <- function(
    inv_gmd_list,
    get_dlw_data,
    validate_dlw_data,
    check_missing,
    release,
    identity,
    verbose
) {
  scalar_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  scalar_logical <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }
  valid <- scalar_string(inv_gmd_list) && scalar_string(release) &&
    scalar_string(identity) && identity %in% c("PROD", "INT", "TEST") &&
    all(vapply(
      list(get_dlw_data, validate_dlw_data, check_missing, verbose),
      scalar_logical,
      logical(1)
    ))
  if (!valid) {
    rlang::abort(
      paste0(
        "DLW wrapper IDs must be nonempty character scalars; `identity` ",
        "must be PROD, INT, or TEST; stage flags, `check_missing`, and ",
        "`verbose` must be non-missing logical scalars."
      ),
      class = c("pipdata_dlw_argument_error", "piperr")
    )
  }
  invisible(TRUE)
}

.new_dlw_wrapper_failure <- function(
    phase = NULL,
    condition = NULL,
    error_type = NULL,
    condition_msg = NULL
) {
  if (is.null(phase) && is.null(condition) && is.null(error_type) &&
      is.null(condition_msg)) {
    return(data.table::data.table(
      survey_id = character(),
      phase = character(),
      error_type = character(),
      condition_msg = character()
    ))
  }
  if (!is.character(phase) || length(phase) != 1L || is.na(phase) ||
      !nzchar(phase)) {
    .abort_dlw_wrapper_contract(
      "Wrapper failure `phase` must be one nonempty character value."
    )
  }
  if (!is.null(condition)) {
    if (!inherits(condition, "condition") || !is.null(error_type) ||
        !is.null(condition_msg)) {
      .abort_dlw_wrapper_contract(
        "Wrapper failures require either a condition or compact fields."
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
  valid_compact <- is.character(error_type) && length(error_type) == 1L &&
    !is.na(error_type) && nzchar(error_type) &&
    is.character(condition_msg) && length(condition_msg) == 1L &&
    !is.na(condition_msg) && nzchar(condition_msg)
  if (!valid_compact) {
    .abort_dlw_wrapper_contract(
      "Wrapper failure compact fields must be nonempty character scalars."
    )
  }
  data.table::data.table(
    survey_id = NA_character_,
    phase = phase,
    error_type = error_type,
    condition_msg = condition_msg
  )
}

.new_dlw_wrapper_not_run <- function(stage, reason) {
  if (!is.character(stage) || length(stage) != 1L ||
      !stage %in% c("acquisition", "validation") ||
      !is.character(reason) || length(reason) != 1L ||
      !reason %in% c("disabled", "dependency_failed")) {
    .abort_dlw_wrapper_contract("Invalid wrapper not-run stage or reason.")
  }
  list(
    stage = stage,
    outcome = "not_run",
    inventory = NULL,
    summary = list(reason = reason),
    failures = .new_dlw_wrapper_failure(),
    artifacts = list()
  )
}

.dlw_wrapper_not_run_reason <- function(result) {
  if (!identical(result$outcome, "not_run") || !is.list(result$summary)) {
    return(NA_character_)
  }
  reason <- result$summary$reason
  if (!is.character(reason) || length(reason) != 1L || is.na(reason)) {
    return(NA_character_)
  }
  reason
}

.derive_dlw_wrapper_outcome <- function(acquisition, validation, failures) {
  if (!data.table::is.data.table(failures) || nrow(failures) > 0L) {
    return("failed")
  }
  acquisition_outcome <- acquisition$outcome
  validation_outcome <- validation$outcome
  acquisition_reason <- .dlw_wrapper_not_run_reason(acquisition)
  validation_reason <- .dlw_wrapper_not_run_reason(validation)

  if (identical(acquisition_outcome, "not_run") &&
      identical(acquisition_reason, "disabled")) {
    if (identical(validation_outcome, "not_run") &&
        identical(validation_reason, "disabled")) {
      return("no_work")
    }
    return(validation_outcome)
  }
  if (identical(validation_outcome, "not_run") &&
      identical(validation_reason, "disabled")) {
    return(acquisition_outcome)
  }
  if (identical(validation_outcome, "not_run") &&
      identical(validation_reason, "dependency_failed")) {
    return("failed")
  }
  if (identical(acquisition_outcome, "partial")) {
    return("partial")
  }
  if (identical(acquisition_outcome, "failed")) {
    if (identical(validation_outcome, "failed")) {
      return("failed")
    }
    return("partial")
  }
  if (identical(acquisition_outcome, "success")) {
    if (validation_outcome %in% c("success", "no_work")) {
      return("success")
    }
    return("partial")
  }
  if (identical(acquisition_outcome, "no_work")) {
    if (identical(validation_outcome, "success")) {
      return("success")
    }
    if (identical(validation_outcome, "no_work")) {
      return("no_work")
    }
    return("partial")
  }
  .abort_dlw_wrapper_contract("Illegal DLW aggregate outcome pairing.")
}

.dlw_wrapper_summary_counts <- function(result, stage) {
  if (identical(result$outcome, "not_run")) {
    return(if (identical(stage, "acquisition")) {
      list(n_total = 0L, n_success = 0L, n_failed = 0L)
    } else {
      list(n_total = 0L, n_valid = 0L, n_invalid = 0L, n_failed = 0L)
    })
  }
  if (!is.list(result$summary)) {
    .abort_dlw_wrapper_contract("A nested DLW stage summary is unavailable.")
  }
  fields <- if (identical(stage, "acquisition")) {
    c("n_total", "n_success", "n_failed")
  } else {
    c("n_total", "n_valid", "n_invalid", "n_failed")
  }
  values <- result$summary[fields]
  valid <- identical(names(values), fields) && all(vapply(values, function(x) {
    is.integer(x) && length(x) == 1L && !is.na(x) && x >= 0L
  }, logical(1)))
  if (!valid) {
    .abort_dlw_wrapper_contract("A nested DLW stage summary is malformed.")
  }
  values
}

.validate_dlw_wrapper_summary_logmeta <- function(x) {
  expected <- c(
    "info", "phase", "get_dlw_data", "validate_dlw_data", "outcome",
    "acquisition_outcome", "validation_outcome",
    "acquisition_n_total", "acquisition_n_success", "acquisition_n_failed",
    "validation_n_total", "validation_n_valid", "validation_n_invalid",
    "validation_n_failed"
  )
  count_fields <- c(
    "acquisition_n_total", "acquisition_n_success", "acquisition_n_failed",
    "validation_n_total", "validation_n_valid", "validation_n_invalid",
    "validation_n_failed"
  )
  valid_count <- function(value) {
    is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
  }
  aggregate_outcomes <- c("success", "partial", "failed", "no_work")
  stage_outcomes <- c(aggregate_outcomes, "not_run")
  valid <- is.list(x) && identical(names(x), expected) &&
    identical(x$info, .logtype_dlw_summary) &&
    identical(x$phase, "complete") &&
    is.logical(x$get_dlw_data) && length(x$get_dlw_data) == 1L &&
    !is.na(x$get_dlw_data) &&
    is.logical(x$validate_dlw_data) && length(x$validate_dlw_data) == 1L &&
    !is.na(x$validate_dlw_data) &&
    is.character(x$outcome) && length(x$outcome) == 1L &&
    !is.na(x$outcome) && x$outcome %in% aggregate_outcomes &&
    is.character(x$acquisition_outcome) &&
    length(x$acquisition_outcome) == 1L &&
    !is.na(x$acquisition_outcome) &&
    x$acquisition_outcome %in% stage_outcomes &&
    is.character(x$validation_outcome) &&
    length(x$validation_outcome) == 1L &&
    !is.na(x$validation_outcome) &&
    x$validation_outcome %in% stage_outcomes &&
    all(vapply(x[count_fields], valid_count, logical(1)))
  if (isTRUE(valid)) {
    valid <- identical(
      x$acquisition_n_total,
      x$acquisition_n_success + x$acquisition_n_failed
    ) && identical(
      x$validation_n_total,
      x$validation_n_valid + x$validation_n_invalid + x$validation_n_failed
    ) && (!identical(x$acquisition_outcome, "not_run") ||
      x$acquisition_n_total == 0L) &&
      (!identical(x$validation_outcome, "not_run") ||
        x$validation_n_total == 0L)
  }
  if (!isTRUE(valid)) {
    .abort_dlw_wrapper_contract(
      "DLW wrapper summary metadata does not match the pinned schema."
    )
  }
  invisible(x)
}

.new_dlw_wrapper_summary_logmeta <- function(
    get_dlw_data,
    validate_dlw_data,
    outcome,
    acquisition,
    validation
) {
  acquisition_counts <- .dlw_wrapper_summary_counts(
    acquisition,
    "acquisition"
  )
  validation_counts <- .dlw_wrapper_summary_counts(validation, "validation")
  value <- list(
    info = .logtype_dlw_summary,
    phase = "complete",
    get_dlw_data = get_dlw_data,
    validate_dlw_data = validate_dlw_data,
    outcome = outcome,
    acquisition_outcome = acquisition$outcome,
    validation_outcome = validation$outcome,
    acquisition_n_total = acquisition_counts$n_total,
    acquisition_n_success = acquisition_counts$n_success,
    acquisition_n_failed = acquisition_counts$n_failed,
    validation_n_total = validation_counts$n_total,
    validation_n_valid = validation_counts$n_valid,
    validation_n_invalid = validation_counts$n_invalid,
    validation_n_failed = validation_counts$n_failed
  )
  .validate_dlw_wrapper_summary_logmeta(value)
  value
}

.dlw_wrapper_is_interactive <- function() {
  interactive()
}

.is_dlw_wrapper_cancellation <- function(x) {
  inherits(x, c(
    "pipdata_dlw_cancellation",
    "pipdata_dlw_cancelled",
    "pipdata_user_cancelled"
  ))
}

.capture_dlw_wrapper_error <- function(condition) {
  if (.is_dlw_wrapper_cancellation(condition)) {
    rlang::cnd_signal(condition)
  }
  condition
}

.validate_dlw_wrapper_inventory_state <- function(x) {
  valid <- is.list(x) &&
    identical(names(x), c("state", "value", "version_id")) &&
    is.character(x$state) && length(x$state) == 1L && !is.na(x$state) &&
    x$state %in% c("present", "absent") &&
    is.character(x$version_id) && length(x$version_id) == 1L &&
    (is.na(x$version_id) || nzchar(x$version_id))
  if (isTRUE(valid) && identical(x$state, "present")) {
    valid <- data.table::is.data.table(x$value)
  }
  if (isTRUE(valid) && identical(x$state, "absent")) {
    valid <- is.null(x$value) && is.na(x$version_id)
  }
  if (!isTRUE(valid)) {
    rlang::abort(
      "Acquisition inventory inspection returned an invalid state.",
      class = "pipdata_dlw_bootstrap_inventory_error"
    )
  }
  x
}

.normalize_dlw_wrapper_stage_result <- function(result, stage) {
  expected_names <- c(
    "stage", "outcome", "inventory", "summary", "failures", "artifacts"
  )
  if (!is.list(result) || !identical(names(result), expected_names) ||
      !identical(result$stage, stage)) {
    rlang::abort(
      paste0("The ", stage, " delegate returned an invalid stage result."),
      class = "pipdata_dlw_delegate_result_error"
    )
  }
  args <- result[setdiff(expected_names, "stage")]
  if (identical(stage, "acquisition")) {
    return(do.call(.new_dlw_acquisition_result, args))
  }
  do.call(.new_dlw_validation_result, args)
}

.new_dlw_wrapper_acquisition_delegate_error <- function(
    condition,
    inventory_state,
    inv_gmd_list
) {
  known <- is.list(inventory_state) &&
    is.character(inventory_state$state) &&
    length(inventory_state$state) == 1L &&
    !is.na(inventory_state$state) &&
    inventory_state$state %in% c("present", "absent")
  present <- known && identical(inventory_state$state, "present")
  inventory <- if (present) data.table::copy(inventory_state$value) else NULL
  version_id <- if (present) inventory_state$version_id else NA_character_
  .new_dlw_acquisition_result(
    outcome = "failed",
    inventory = inventory,
    summary = .dlw_acquisition_empty_summary(),
    failures = .new_dlw_acquisition_failure(
      phase = "delegate_error",
      condition = condition
    ),
    artifacts = list(inventory = .new_dlw_acquisition_artifact_fact(
      id = inv_gmd_list,
      alias = "dlw_inv",
      attempted = FALSE,
      success = FALSE,
      trustworthy = known,
      version_id = version_id,
      skipped = NA,
      reconciled = FALSE
    ))
  )
}

.new_dlw_wrapper_validation_delegate_error <- function(condition) {
  .new_dlw_validation_result(
    outcome = "failed",
    inventory = NULL,
    summary = .dlw_validation_empty_summary(),
    failures = .new_dlw_validation_failure(
      phase = "delegate_error",
      condition = condition
    ),
    artifacts = list(
      report = .dlw_validation_no_write_fact(
        "validation_report", FALSE, FALSE
      ),
      inventory = .dlw_validation_no_write_fact(
        "gmd_valid_inv", FALSE, FALSE
      )
    )
  )
}

.dlw_wrapper_can_validate <- function(acquisition) {
  fact <- acquisition$artifacts$inventory
  is.list(fact) && isTRUE(fact$trustworthy) &&
    !is.null(acquisition$inventory)
}

.inspect_dlw_wrapper_checkpoint <- function() {
  checkpoint_file <- "pipdata_log_checkpoint_dlw.qs2"
  versions <- tryCatch(
    .strict_dlw_versions(checkpoint_file, "dlw_meta"),
    error = function(e) e
  )
  unknown <- list(
    state = "unknown",
    version_id = NA_character_
  )
  if (inherits(versions, "condition") ||
      !.is_valid_dlw_version_catalog(versions)) {
    return(unknown)
  }
  if (nrow(versions) == 0L) {
    return(list(state = "absent", version_id = NA_character_))
  }
  version_id <- versions$version_id[[1L]]
  if (!is.character(version_id) || length(version_id) != 1L ||
      is.na(version_id) || !nzchar(version_id)) {
    return(unknown)
  }
  checkpoint <- tryCatch(
    stamp::st_load(
      checkpoint_file,
      alias = "dlw_meta",
      verbose = FALSE
    ),
    error = function(e) e
  )
  if (inherits(checkpoint, "condition") || !inherits(checkpoint, "piplog")) {
    return(unknown)
  }
  list(state = "present", version_id = version_id)
}

.new_dlw_wrapper_checkpoint_fact <- function(
    summary_logged,
    summary_condition_msg,
    success,
    trustworthy,
    version_id,
    skipped,
    reconciled,
    condition_msg
) {
  list(
    summary_logged = summary_logged,
    summary_condition_msg = summary_condition_msg,
    attempted = TRUE,
    success = success,
    trustworthy = trustworthy,
    alias = "dlw_meta",
    stage = "dlw",
    version_id = version_id,
    skipped = skipped,
    reconciled = reconciled,
    condition_msg = condition_msg
  )
}

.save_dlw_wrapper_checkpoint <- function(
    summary_logged,
    summary_condition_msg
) {
  prior <- .inspect_dlw_wrapper_checkpoint()
  write_result <- tryCatch(
    pipfun::log_save_checkpoint(
      name = "pipdata_log",
      stage = "dlw",
      alias = "dlw_meta"
    ),
    error = function(e) .capture_dlw_wrapper_error(e)
  )
  write_condition <- if (inherits(write_result, "condition")) {
    write_result
  } else {
    NULL
  }
  valid_version <- is.list(write_result) &&
    is.character(write_result$version_id) &&
    length(write_result$version_id) == 1L &&
    !is.na(write_result$version_id) && nzchar(write_result$version_id)
  valid_direct_skipped <- is.list(write_result) &&
    (is.null(write_result$skipped) ||
      (is.logical(write_result$skipped) &&
        length(write_result$skipped) == 1L &&
        !is.na(write_result$skipped) && !write_result$skipped))
  valid_skipped <- is.list(write_result) &&
    is.logical(write_result$skipped) &&
    length(write_result$skipped) == 1L && !is.na(write_result$skipped) &&
    isTRUE(write_result$skipped) &&
    (is.null(write_result$version_id) || valid_version ||
      (is.character(write_result$version_id) &&
        length(write_result$version_id) == 1L &&
        is.na(write_result$version_id)))

  if (valid_version && valid_direct_skipped) {
    return(.new_dlw_wrapper_checkpoint_fact(
      summary_logged, summary_condition_msg,
      TRUE, TRUE, write_result$version_id, FALSE, FALSE, NA_character_
    ))
  }
  if (valid_skipped) {
    post <- .inspect_dlw_wrapper_checkpoint()
    same_active <- identical(prior$state, "present") &&
      identical(post$state, "present") &&
      identical(prior$version_id, post$version_id) &&
      (!valid_version || identical(write_result$version_id, post$version_id))
    if (same_active) {
      version_id <- if (valid_version) {
        write_result$version_id
      } else {
        NA_character_
      }
      return(.new_dlw_wrapper_checkpoint_fact(
        summary_logged, summary_condition_msg,
        TRUE, TRUE, version_id, TRUE, FALSE, NA_character_
      ))
    }
    advanced <- identical(post$state, "present") &&
      (identical(prior$state, "absent") ||
        (identical(prior$state, "present") &&
          !identical(prior$version_id, post$version_id)))
    if (advanced) {
      return(.new_dlw_wrapper_checkpoint_fact(
        summary_logged, summary_condition_msg,
        TRUE, TRUE, post$version_id, FALSE, TRUE, NA_character_
      ))
    }
    trustworthy <- post$state %in% c("present", "absent") &&
      !identical(prior$state, "unknown")
    version_id <- if (identical(post$state, "present")) {
      post$version_id
    } else {
      NA_character_
    }
    return(.new_dlw_wrapper_checkpoint_fact(
      summary_logged, summary_condition_msg,
      FALSE, trustworthy, version_id, FALSE, TRUE,
      "DLW checkpoint skip could not be verified against active state."
    ))
  }

  post <- .inspect_dlw_wrapper_checkpoint()
  advanced <- identical(post$state, "present") &&
    (identical(prior$state, "absent") ||
      (identical(prior$state, "present") &&
        !identical(prior$version_id, post$version_id)))
  if (advanced) {
    return(.new_dlw_wrapper_checkpoint_fact(
      summary_logged, summary_condition_msg,
      TRUE, TRUE, post$version_id, FALSE, TRUE, NA_character_
    ))
  }
  trustworthy <- post$state %in% c("present", "absent") &&
    !identical(prior$state, "unknown")
  version_id <- if (identical(post$state, "present")) {
    post$version_id
  } else {
    NA_character_
  }
  failure_message <- if (!is.null(write_condition)) {
    conditionMessage(write_condition)
  } else {
    "DLW checkpoint save returned an invalid result."
  }
  .new_dlw_wrapper_checkpoint_fact(
    summary_logged, summary_condition_msg,
    FALSE, trustworthy, version_id, FALSE, TRUE, failure_message
  )
}

#' Acquire and validate DLW data
#'
#' `pipdata_dlw_process()` is the current supported DLW entry point. It runs the
#' explicit acquisition and validation stages in order, emits a scalar
#' `dlw_summary_inf` entry from their returned facts, and attempts the existing
#' `"dlw"` checkpoint. After validation, use [pd_run_pipeline()] for incremental
#' clean, metadata, and deflate execution.
#'
#' Validation continues after acquisition `"no_work"`, `"partial"`, or even
#' `"failed"` when acquisition still returns a trustworthy non-`NULL` durable
#' inventory. It is dependency-blocked only when that prerequisite is
#' unavailable. Stage outcomes are aggregated as `"success"`, `"partial"`,
#' `"failed"`, or `"no_work"`; disabled or dependency-blocked nested stages use
#' `"not_run"` with `summary$reason` equal to `"disabled"` or
#' `"dependency_failed"`.
#'
#' With both stages requested, acquisition `"success"` followed by validation
#' `"success"`/`"no_work"` is aggregate `"success"`; acquisition `"no_work"`
#' followed by validation `"success"` is also `"success"`, while two no-work
#' stages are `"no_work"`. Any acquisition `"partial"` is aggregate `"partial"`.
#' Acquisition `"success"`/`"no_work"` plus validation `"partial"`/`"failed"` is
#' `"partial"`. Acquisition `"failed"` with trustworthy state plus validation
#' `"success"`/`"no_work"`/`"partial"` is `"partial"`, but two failed stages are
#' `"failed"`. An untrustworthy acquisition failure that dependency-blocks
#' validation is `"failed"`. When only one stage is requested, its outcome is
#' the aggregate; both disabled is `"no_work"` unless a wrapper failure occurs.
#' Checkpoint or summary-log failure is reported in the checkpoint facts and
#' does not rewrite the completed business outcome.
#'
#' The wrapper routes `inv_gmd_list` through acquisition, bootstrap, and the
#' internal validation path, so custom inventory IDs are honored end to end.
#' Validate-only calls never display a menu. A missing inventory in validate-only
#' or noninteractive execution is an inspectable validation/acquisition failure,
#' respectively. The Download/Abort menu is reserved for interactive calls that
#' request acquisition.
#'
#' @inheritParams pipdata_get_gmd
#' @param get_dlw_data Logical scalar. Run acquisition. Default `TRUE`.
#' @param validate_dlw_data Logical scalar. Run validation. Default `TRUE`.
#' @param release Required nonempty character scalar. Data release identifier or
#'   date. The formal default `NULL` is a missing-required-value sentinel, not an
#'   operational default, and aborts before setup.
#' @param identity Required character scalar. One of `"PROD"`, `"INT"`, or
#'   `"TEST"`. The formal default `NULL` is a missing-required-value sentinel,
#'   not an operational default, and aborts before setup.
#'
#' @note Invalid arguments, working-release setup failures, interactive user
#'   cancellation, and interrupts escape. Runtime failures after setup are
#'   converted to compact wrapper or stage facts so callers can inspect the
#'   returned result. Normal unassigned calls remain quiet because the result is
#'   invisible.
#'
#' @return Invisibly, a plain unclassed list with names `stage`, `outcome`,
#'   `acquisition`, `validation`, `failures`, and `checkpoint`. `stage` is
#'   `"dlw"`; `outcome` is the aggregate `"success"`, `"partial"`, `"failed"`, or
#'   `"no_work"`. `acquisition` and `validation` are the six-field stage results
#'   documented by [pipdata_get_gmd()] and [pipdata_validate_gmd()]. A wrapper-only
#'   not-run stage still has names `stage`, `outcome`, `inventory`, `summary`,
#'   `failures`, and `artifacts`, with `inventory = NULL`,
#'   `summary = list(reason = ...)`, an empty failure table, and empty artifacts.
#'
#'   `failures` is the compact wrapper `data.table` with `survey_id`, `phase`,
#'   `error_type`, and `condition_msg`. `checkpoint` records
#'   `summary_logged`, `summary_condition_msg`, `attempted`, `success`,
#'   `trustworthy`, `alias`, `stage`, `version_id`, `skipped`, `reconciled`, and
#'   `condition_msg`.
#' @export
#'
#' @examples
#' \dontrun{
#' pipdata_dlw_process(inv_gmd_list = "dlw_gmd_inv",
#'             get_dlw_data = TRUE,
#'             validate_dlw_data = TRUE,
#'             check_missing   = TRUE,
#'             release         = "20260206",
#'             identity        = "TEST"
#'             )
#' }
pipdata_dlw_process <- function(
    inv_gmd_list = "dlw_gmd_inv",
    get_dlw_data = TRUE,
    validate_dlw_data = TRUE,
    check_missing = TRUE,
    release = NULL,
    identity = NULL,
    verbose = getOption("pipdata.verbose", default = TRUE)
){
  .validate_dlw_wrapper_arguments(
    inv_gmd_list = inv_gmd_list,
    get_dlw_data = get_dlw_data,
    validate_dlw_data = validate_dlw_data,
    check_missing = check_missing,
    release = release,
    identity = identity,
    verbose = verbose
  )
  pipfun::setup_working_release(
    release = release,
    identity = identity,
    verbose = FALSE
  )
  pipfun::get_wrk_release()

  failures <- .new_dlw_wrapper_failure()
  append_failure <- function(phase, condition) {
    failures <<- data.table::rbindlist(
      list(
        failures,
        .new_dlw_wrapper_failure(phase = phase, condition = condition)
      ),
      use.names = TRUE
    )
  }

  alias_error <- tryCatch({
    stamp::st_init(
      root = fs::path(
        getOption("pipfun.main_dir"),
        "pip_repository",
        "pip_deflated"
      ),
      alias = "pip_deflated"
    )
    stamp::st_init(
      root = fs::path(
        getOption("pipfun.main_dir"),
        "pip_repository",
        "pip_logs"
      ),
      alias = "piplog"
    )
    NULL
  }, error = function(e) .capture_dlw_wrapper_error(e))
  if (!is.null(alias_error)) {
    append_failure("alias_init", alias_error)
  }

  acquisition_state <- NULL
  interactive_acquisition <- get_dlw_data && .dlw_wrapper_is_interactive()
  if (nrow(failures) == 0L && interactive_acquisition) {
    folders_result <- tryCatch(
      pipfun::get_pip_folders(),
      error = function(e) .capture_dlw_wrapper_error(e)
    )
    if (inherits(folders_result, "condition")) {
      append_failure("folder_resolve", folders_result)
    } else {
      directory_error <- tryCatch({
        check_directory(folders_result$dlw_inventory)
        NULL
      }, error = function(e) .capture_dlw_wrapper_error(e))
      if (!is.null(directory_error)) {
        append_failure("directory_check", directory_error)
      }
    }

    if (nrow(failures) == 0L) {
      state_result <- tryCatch(
        .validate_dlw_wrapper_inventory_state(
          .reload_dlw_acquisition_inventory_state(
            inv_gmd_list,
            verbose = verbose
          )
        ),
        error = function(e) .capture_dlw_wrapper_error(e)
      )
      if (inherits(state_result, "condition")) {
        append_failure("bootstrap_inventory", state_result)
      } else {
        acquisition_state <- state_result
      }
    }

    missing_interactive <- nrow(failures) == 0L &&
      identical(acquisition_state$state, "absent")
    if (missing_interactive) {
      cli::cli_text(
        "Local GMD list is not available.\n",
        "Expected location: {.path {folders_result$dlw_inventory}}\n",
        "What would you like to do?"
      )
      choice <- tryCatch(
        utils::menu(
          choices = c("Download GMD list", "Abort"),
          title = "Select an option"
        ),
        error = function(e) .capture_dlw_wrapper_error(e)
      )
      if (inherits(choice, "condition")) {
        append_failure("bootstrap_inventory", choice)
      } else if (!is.numeric(choice) || length(choice) != 1L ||
          is.na(choice) || !choice %in% c(0L, 1L, 2L)) {
        append_failure(
          "bootstrap_inventory",
          rlang::error_cnd(
            "pipdata_dlw_bootstrap_inventory_error",
            message = "The acquisition bootstrap menu returned no valid choice."
          )
        )
      } else if (choice %in% c(0L, 2L)) {
        rlang::abort(
          "Process aborted by user.",
          class = c("pipdata_dlw_cancellation", "piperr")
        )
      }
      if (nrow(failures) == 0L) {
        bootstrap_error <- tryCatch({
          dlw_gmd_list(inv_gmd_list = inv_gmd_list)
          acquisition_state <- .validate_dlw_wrapper_inventory_state(
            .reload_dlw_acquisition_inventory_state(
              inv_gmd_list,
              verbose = verbose
            )
          )
          if (!identical(acquisition_state$state, "present")) {
            rlang::abort(
              "The requested acquisition inventory was not durably created.",
              class = "pipdata_dlw_bootstrap_inventory_error"
            )
          }
          NULL
        }, error = function(e) .capture_dlw_wrapper_error(e))
        if (!is.null(bootstrap_error)) {
          append_failure("bootstrap_inventory", bootstrap_error)
        }
      }
    }
  }

  if (nrow(failures) > 0L) {
    acquisition <- .new_dlw_wrapper_not_run(
      "acquisition",
      if (get_dlw_data) "dependency_failed" else "disabled"
    )
  } else if (!get_dlw_data) {
    acquisition <- .new_dlw_wrapper_not_run("acquisition", "disabled")
  } else {
    acquisition_result <- tryCatch({
      value <- pipdata_get_gmd(
        inv_gmd_list = inv_gmd_list,
        check_missing = check_missing,
        verbose = verbose
      )
      .normalize_dlw_wrapper_stage_result(value, "acquisition")
    }, error = function(e) .capture_dlw_wrapper_error(e))
    acquisition <- if (inherits(acquisition_result, "condition")) {
      acquisition_state <- tryCatch(
        .validate_dlw_wrapper_inventory_state(
          .reload_dlw_acquisition_inventory_state(
            inv_gmd_list,
            verbose = verbose
          )
        ),
        error = function(e) NULL
      )
      .new_dlw_wrapper_acquisition_delegate_error(
        acquisition_result,
        acquisition_state,
        inv_gmd_list
      )
    } else {
      acquisition_result
    }
  }

  validation_blocked <- get_dlw_data &&
    !.dlw_wrapper_can_validate(acquisition)
  if (nrow(failures) > 0L) {
    validation <- .new_dlw_wrapper_not_run(
      "validation",
      if (validate_dlw_data) "dependency_failed" else "disabled"
    )
  } else if (!validate_dlw_data) {
    validation <- .new_dlw_wrapper_not_run("validation", "disabled")
  } else if (validation_blocked) {
    validation <- .new_dlw_wrapper_not_run(
      "validation",
      "dependency_failed"
    )
  } else {
    validation_result <- tryCatch({
      value <- .pipdata_validate_gmd_core(
        acquisition_inventory_id = inv_gmd_list,
        verbose = verbose
      )
      .normalize_dlw_wrapper_stage_result(value, "validation")
    }, error = function(e) .capture_dlw_wrapper_error(e))
    validation <- if (inherits(validation_result, "condition")) {
      .new_dlw_wrapper_validation_delegate_error(validation_result)
    } else {
      validation_result
    }
  }

  outcome <- .derive_dlw_wrapper_outcome(
    acquisition,
    validation,
    failures
  )
  summary_error <- tryCatch({
    summary <- .new_dlw_wrapper_summary_logmeta(
      get_dlw_data = get_dlw_data,
      validate_dlw_data = validate_dlw_data,
      outcome = outcome,
      acquisition = acquisition,
      validation = validation
    )
    pipfun::log_info(
      "DLW processing complete.",
      name = "pipdata_log",
      logmeta = summary
    )
    NULL
  }, error = function(e) .capture_dlw_wrapper_error(e))
  summary_logged <- is.null(summary_error)
  summary_condition_msg <- if (summary_logged) {
    NA_character_
  } else {
    conditionMessage(summary_error)
  }
  checkpoint <- .save_dlw_wrapper_checkpoint(
    summary_logged = summary_logged,
    summary_condition_msg = summary_condition_msg
  )

  invisible(list(
    stage = "dlw",
    outcome = outcome,
    acquisition = acquisition,
    validation = validation,
    failures = data.table::copy(failures),
    checkpoint = checkpoint
  ))
}

