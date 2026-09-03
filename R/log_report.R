#' Generate a markdown report from a pipeline log.
#'
#' Parses a `piplog` object produced by `pipfun::log_filter()` and writes a
#' structured markdown document summarising errors, informational messages, and
#' affected surveys.
#'
#' @param log A `piplog` object (inherits from `data.table`). Default `NULL`,
#'   in which case it is loaded internally via
#'   `pipfun::log_filter(name = "pipdata_log")`.
#' @param path Character scalar. File path for the output `.md` file.
#'   If `NULL` (default), the report is returned as a character vector and
#'   not written to disk.
#' @param title Character scalar. Title for the report
#'   (default: `"Pipeline Log Report"`).
#' @param overwrite Logical. Overwrite `path` if it already exists
#'   (default: `FALSE`).
#'
#' @return Invisibly, the report as a character vector (one element per line).
#'   If `path` is non-`NULL`, the file is written as a side-effect.
#'
#' @details
#' The report contains:
#' \itemize{
#'   \item Running metadata (time window, total entries, success/fail counts).
#'   \item Stage-aware warnings for DLW-only, pipeline-only, no-op, and
#'     incomplete runs.
#'   \item DLW acquisition summary, including attempted, successful, and
#'     failed survey counts and failure details from the latest attempt.
#'   \item DLW validation summary, separating valid, invalid, execution-failed,
#'     and workflow outcomes from the latest attempt.
#'   \item Processing summary: total, cleaned, and failed counts
#'     (from `process_summary_inf` log entry).
#'   \item Deflation summary: candidates, successes, failures, and failing
#'     surveys (from `deflate_summary_inf` log entry).
#'   \item Auxiliary file changes: which measures changed and how many
#'     surveys were affected (from `aux_changes_inf` log entry).
#'   \item Summary table by error / info type.
#'   \item Country-level breakdown of errors.
#'   \item Inventory verification: confirmed vs missing surveys
#'     (from `inv_update_inf` log entry).
#'   \item Surveys skipped during data processing or metadata creation
#'     (`skipped_svys_data` / `skipped_svys_metadata` entries), with reasons.
#'   \item List of surveys that failed processing (`null_svys_inf` entry).
#' }
#' Acquisition and validation are segmented independently from their latest
#' `attempt_start` entry. An exact completion entry is preferred; logs produced
#' before completion entries existed use a fallback confined to that latest
#' segment. All DLW acquisition, validation, and wrapper discriminators are
#' excluded from generic type and country sections, so dedicated DLW sections
#' own those entries without historical leakage or double counting. Other
#' sections that rely on a specific logmeta entry are silently omitted when that
#' entry is absent.
#'
#' @family pd_process_data pipeline
#' @export
#'
#' @examples
#' \dontrun{
#' # Return as character vector (log defaults to the "pipdata_log")
#' report <- log_report()
#' # Write to file
#' log_report(path = "log_report.md", overwrite = TRUE)
#' }
log_report <- function(
  log = NULL,
  path = NULL,
  title = "Pipeline Log Report",
  overwrite = FALSE
) {
  # --- Validation ----------------------------------------------------------
  if (is.null(log)) {
    log <- pipfun::log_filter(name = "pipdata_log")
  }
  if (!inherits(log, "piplog")) {
    cli::cli_abort("{.arg log} must be a {.cls piplog} object.")
  }
  if (nrow(log) == 0L) {
    cli::cli_abort("{.arg log} contains no entries.")
  }
  if (!is.null(path) && file.exists(path) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "File {.path {path}} already exists. Use {.code overwrite = TRUE}."
    )
  }

  # --- Parse logmeta -------------------------------------------------------
  dt <- parse_log_meta(log)

  # --- Build report sections -----------------------------------------------
  # Filter out empty sections to avoid orphan blank lines when optional
  # entries are absent from the log.
  sections <- Filter(
    length,
    list(
      build_header(dt, title),
      build_stage_warning(dt),
      build_dlw_acquisition_summary(dt),
      build_dlw_validation_summary(dt),
      build_pipeline_run_summary(dt),
      build_processing_summary(dt),
      build_deflation_summary(dt),
      build_aux_changes(dt),
      build_type_summary(dt),
      build_country_table(dt),
      build_inventory_additions(dt),
      build_skipped_surveys(dt),
      build_null_surveys(dt)
    )
  )
  lines <- unlist(lapply(sections, \(s) c(s, "")))

  # --- Write or return -----------------------------------------------------
  if (!is.null(path)) {
    writeLines(lines, con = path)
    cli::cli_alert_success("Report written to {.path {path}}")
  }

  return(invisible(lines))
}


#' Build a stage-awareness warning for a parsed pipeline log
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#' @return Character vector of markdown lines, or an empty vector.
#' @keywords internal
build_stage_warning <- function(dt) {
  dlw_idx <- which(dt$error_type == .logtype_dlw_summary)
  pipeline_ran <- any(
    dt$error_type %in% c(
      "process_summary_inf", "pipeline_run_summary_inf"
    ),
    na.rm = TRUE
  )

  if (length(dlw_idx) > 0L) {
    summary <- dt$logmeta[[dlw_idx[length(dlw_idx)]]]
    no_op <- identical(summary$get_dlw_data, FALSE) &&
      identical(summary$validate_dlw_data, FALSE)
    if (no_op) {
      return(c(
        "> **DLW no-op:** DLW stage ran but neither acquisition nor",
        "> validation was performed (both `get_dlw_data` and",
        "> `validate_dlw_data` were FALSE)."
      ))
    }
  }

  dlw_ran <- length(dlw_idx) > 0L
  if (dlw_ran && !pipeline_ran) {
    return(c(
      "> **Partial run:** Only DLW acquisition/validation completed.",
      "> Survey cleaning (`pd_process_data`) was not executed."
    ))
  }

  if (!dlw_ran && pipeline_ran) {
    return(c(
      "> **Note:** DLW acquisition was not part of this run."
    ))
  }

  if (!dlw_ran && !pipeline_ran) {
    return(c(
      "> **Warning:** This log does not contain a completed DLW or pipeline",
      "> stage marker; the run may be incomplete."
    ))
  }

  character(0)
}


.valid_pipeline_run_summary <- function(meta) {
  expected <- c(
    "info", "run_id", "status", "terminal", "n_selected",
    "n_attempted", "n_success", "n_failed", "n_cached", "n_blocked",
    "clean_status", "metadata_status", "deflate_status",
    "manifest_before_generation", "manifest_after_generation",
    "started_at", "completed_at"
  )
  count_fields <- c(
    "n_selected", "n_attempted", "n_success", "n_failed", "n_cached",
    "n_blocked"
  )
  generation_fields <- c(
    "manifest_before_generation", "manifest_after_generation"
  )
  stage_statuses <- c("success", "partial", "failed", "cached", "skipped")
  valid_stage <- function(value) {
    is.character(value) && length(value) == 1L &&
      (is.na(value) || value %in% stage_statuses)
  }
  is.list(meta) && identical(names(meta), expected) &&
    identical(meta$info, "pipeline_run_summary_inf") &&
    is.character(meta$run_id) && length(meta$run_id) == 1L &&
    !is.na(meta$run_id) && nzchar(meta$run_id) &&
    is.character(meta$status) && length(meta$status) == 1L &&
    meta$status %in% stage_statuses &&
    is.logical(meta$terminal) && length(meta$terminal) == 1L &&
    !is.na(meta$terminal) &&
    all(vapply(meta[count_fields], function(value) {
      is.integer(value) && length(value) == 1L &&
        !is.na(value) && value >= 0L
    }, logical(1L))) &&
    all(vapply(meta[generation_fields], function(value) {
      is.integer(value) && length(value) == 1L &&
        (is.na(value) || value > 0L)
    }, logical(1L))) &&
    all(vapply(
      meta[c("clean_status", "metadata_status", "deflate_status")],
      valid_stage,
      logical(1L)
    )) &&
    inherits(meta$started_at, "POSIXct") && length(meta$started_at) == 1L &&
    !is.na(meta$started_at) &&
    inherits(meta$completed_at, "POSIXct") &&
    length(meta$completed_at) == 1L && !is.na(meta$completed_at) &&
    meta$completed_at >= meta$started_at
}


.latest_pipeline_run_index <- function(dt) {
  indices <- which(dt$error_type == "pipeline_run_summary_inf")
  if (!length(indices)) {
    return(NA_integer_)
  }
  valid <- indices[vapply(
    dt$logmeta[indices], .valid_pipeline_run_summary, logical(1L)
  )]
  if (!length(valid)) {
    return(NA_integer_)
  }
  completed <- as.POSIXct(vapply(
    dt$logmeta[valid],
    function(meta) as.numeric(meta$completed_at),
    numeric(1L)
  ), origin = "1970-01-01", tz = "UTC")
  run_ids <- vapply(dt$logmeta[valid], `[[`, character(1L), "run_id")
  return(valid[order(completed, run_ids, decreasing = TRUE)][[1L]])
}


.latest_pipeline_stage_summary <- function(dt, discriminator, legacy = "first") {
  indices <- which(vapply(
    dt$logmeta,
    function(meta) identical(meta$info, discriminator),
    logical(1L)
  ))
  if (!length(indices)) {
    return(NULL)
  }
  pipeline_index <- .latest_pipeline_run_index(dt)
  if (!is.na(pipeline_index)) {
    run_id <- dt$logmeta[[pipeline_index]]$run_id
    indices <- indices[vapply(dt$logmeta[indices], function(meta) {
      identical(meta$run_id, run_id)
    }, logical(1L))]
    if (!length(indices)) {
      return(NULL)
    }
  }
  selected <- if (identical(legacy, "last")) {
    indices[[length(indices)]]
  } else {
    indices[[1L]]
  }
  return(dt$logmeta[[selected]])
}


#' Build the latest top-level pipeline run summary
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#' @return Character vector of markdown lines, or an empty vector.
#' @keywords internal
build_pipeline_run_summary <- function(dt) {
  selected <- .latest_pipeline_run_index(dt)
  if (is.na(selected)) {
    return(character())
  }
  meta <- dt$logmeta[[selected]]
  stage_value <- function(value) {
    if (is.na(value)) "not accepted" else value
  }
  generation_value <- function(value) {
    if (is.na(value)) "none" else as.character(value)
  }
  return(c(
    "## Pipeline Run Summary",
    "",
    sprintf("**Run ID:** `%s`", meta$run_id),
    sprintf(
      "**Status:** `%s`%s",
      meta$status,
      if (meta$terminal) " (terminal)" else ""
    ),
    sprintf(
      "**Manifest generations:** %s -> %s",
      generation_value(meta$manifest_before_generation),
      generation_value(meta$manifest_after_generation)
    ),
    "",
    "| Metric | Count |",
    "|--------|------:|",
    sprintf("| Selected | %d |", meta$n_selected),
    sprintf("| Attempted | %d |", meta$n_attempted),
    sprintf("| Succeeded | %d |", meta$n_success),
    sprintf("| Failed | %d |", meta$n_failed),
    sprintf("| Cached | %d |", meta$n_cached),
    sprintf("| Blocked | %d |", meta$n_blocked),
    "",
    "| Stage | Status |",
    "|-------|--------|",
    sprintf("| Clean | `%s` |", stage_value(meta$clean_status)),
    sprintf("| Metadata | `%s` |", stage_value(meta$metadata_status)),
    sprintf("| Deflate | `%s` |", stage_value(meta$deflate_status))
  ))
}


.dlw_log_phase <- function(meta) {
  phase <- meta$phase
  if (!is.character(phase) || length(phase) != 1L || is.na(phase) ||
      !nzchar(phase)) {
    return("unknown")
  }
  phase
}

.latest_dlw_attempt_segment <- function(dt, discriminator) {
  idx <- which(!is.na(dt$error_type) & dt$error_type == discriminator)
  if (length(idx) == 0L) {
    return(integer())
  }
  phases <- vapply(dt$logmeta[idx], .dlw_log_phase, character(1))
  boundaries <- idx[phases == "attempt_start" & dt$event[idx] == "info"]
  if (length(boundaries) > 0L) {
    return(idx[idx >= boundaries[[length(boundaries)]]])
  }
  legacy_boundaries <- idx[
    phases %in% c("start", "no_new_data") & dt$event[idx] == "info"
  ]
  if (length(legacy_boundaries) > 0L) {
    return(idx[idx >= legacy_boundaries[[length(legacy_boundaries)]]])
  }
  idx
}

.latest_valid_dlw_completion <- function(dt, idx, validate) {
  phases <- vapply(dt$logmeta[idx], .dlw_log_phase, character(1))
  completion_idx <- rev(idx[phases == "complete"])
  if (length(completion_idx) == 0L) {
    return(NULL)
  }
  for (i in completion_idx) {
    valid <- tryCatch({
      validate(dt$logmeta[[i]])
      TRUE
    }, error = function(e) FALSE)
    if (valid) {
      return(dt$logmeta[[i]])
    }
  }
  NULL
}

.latest_dlw_start_count <- function(dt, idx) {
  phases <- vapply(dt$logmeta[idx], .dlw_log_phase, character(1))
  starts <- rev(idx[phases == "start"])
  if (length(starts) == 0L) {
    return(NA_integer_)
  }
  value <- dt$logmeta[[starts[[1L]]]]$n_surveys
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value >= 0 && value == floor(value)
  if (!valid) {
    return(NA_integer_)
  }
  as.integer(value)
}

.dlw_workflow_failure_lines <- function(dt, idx) {
  if (length(idx) == 0L) {
    return(character())
  }
  vapply(idx, function(i) {
    meta <- dt$logmeta[[i]]
    detail <- if (!is.null(meta$condition_msg)) {
      meta$condition_msg
    } else {
      dt$message[[i]]
    }
    sprintf(
      "- Workflow phase `%s` - %s",
      .dlw_log_phase(meta),
      detail
    )
  }, character(1))
}

#' Build the DLW acquisition summary section
#'
#' Successful acquisitions are inferred from the start-entry denominator
#' minus per-survey download failures. Phase markers are not outcomes.
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#' @return Character vector of markdown lines, or an empty vector.
#' @keywords internal
build_dlw_acquisition_summary <- function(dt) {
  idx <- .latest_dlw_attempt_segment(dt, .logtype_dlw_acquisition)
  if (length(idx) == 0L) {
    return(character())
  }
  completion <- .latest_valid_dlw_completion(
    dt,
    idx,
    .validate_dlw_acquisition_completion_logmeta
  )
  phases <- vapply(dt$logmeta[idx], .dlw_log_phase, character(1))
  survey_failure_idx <- idx[
    dt$event[idx] == "error" & !is.na(dt$survey[idx])
  ]
  workflow_failure_idx <- idx[
    dt$event[idx] == "error" & is.na(dt$survey[idx])
  ]

  if (!is.null(completion)) {
    n_total <- completion$n_total
    n_success <- completion$n_success
    n_failed <- completion$n_failed
    failed_ids <- completion$surveys_failed
    no_work <- identical(completion$outcome, "no_work")
  } else {
    n_total <- .latest_dlw_start_count(dt, idx)
    failed_ids <- unique(dt$survey[survey_failure_idx])
    n_failed <- as.integer(length(failed_ids))
    no_work <- any(phases == "no_new_data") &&
      length(survey_failure_idx) == 0L &&
      length(workflow_failure_idx) == 0L
    if (no_work) {
      n_total <- 0L
    }
    n_success <- if (is.na(n_total)) {
      NA_integer_
    } else {
      as.integer(max(0L, n_total - n_failed))
    }
  }
  if (is.na(n_total) && length(workflow_failure_idx) == 0L &&
      length(survey_failure_idx) == 0L) {
    return(character())
  }

  attempted_line <- if (is.na(n_total)) {
    "**Surveys:** no valid start-entry denominator was recorded."
  } else {
    sprintf(
      "**Surveys:** %d attempted, %d succeeded, %d failed.",
      n_total,
      n_success,
      n_failed
    )
  }
  lines <- c("## DLW Acquisition Summary", "", attempted_line)
  if (no_work) {
    lines <- c(
      lines,
      "",
      "No new GMD data was available for acquisition."
    )
  }

  failure_lines <- vapply(failed_ids, function(survey_id) {
    matches <- survey_failure_idx[dt$survey[survey_failure_idx] == survey_id]
    if (length(matches) == 0L) {
      return(sprintf("- `%s`", survey_id))
    }
    i <- matches[[length(matches)]]
    meta <- dt$logmeta[[i]]
    country <- if (is.null(meta$country)) dt$country[[i]] else meta$country
    if (is.na(country)) {
      country <- "unknown"
    }
    year <- if (is.null(meta$year)) "unknown" else meta$year
    module <- if (is.null(meta$module)) "unknown" else meta$module
    detail <- if (is.null(meta$condition_msg)) {
      dt$message[[i]]
    } else {
      meta$condition_msg
    }
    sprintf(
      "- `%s` (%s, %s, %s) - %s",
      survey_id,
      country,
      year,
      module,
      detail
    )
  }, character(1))
  workflow_lines <- .dlw_workflow_failure_lines(dt, workflow_failure_idx)

  c(
    lines,
    if (length(failure_lines) > 0L) {
      c("", "**Failed acquisitions:**", "", failure_lines)
    } else {
      character()
    },
    if (length(workflow_lines) > 0L) {
      c("", "**Acquisition workflow failures:**", "", workflow_lines)
    } else {
      character()
    }
  )
}


#' Build the DLW validation summary section
#'
#' Workflow phase markers are reported separately from per-survey failures so
#' inventory and report persistence are not counted as survey validations.
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#' @return Character vector of markdown lines, or an empty vector.
#' @keywords internal
build_dlw_validation_summary <- function(dt) {
  idx <- .latest_dlw_attempt_segment(dt, .logtype_dlw_validation)
  if (length(idx) == 0L) {
    return(character())
  }
  completion <- .latest_valid_dlw_completion(
    dt,
    idx,
    .validate_dlw_validation_completion_logmeta
  )
  phases <- vapply(dt$logmeta[idx], .dlw_log_phase, character(1))
  error_idx <- idx[dt$event[idx] == "error"]
  invalid_idx <- error_idx[
    !is.na(dt$survey[error_idx]) &
      phases[match(error_idx, idx)] == "validation"
  ]
  execution_idx <- error_idx[
    !is.na(dt$survey[error_idx]) &
      phases[match(error_idx, idx)] != "validation"
  ]
  workflow_idx <- error_idx[is.na(dt$survey[error_idx])]

  if (!is.null(completion)) {
    n_total <- completion$n_total
    n_valid <- completion$n_valid
    n_invalid <- completion$n_invalid
    n_failed <- completion$n_failed
    invalid_ids <- completion$surveys_invalid
    failed_ids <- completion$surveys_failed
  } else {
    n_total <- .latest_dlw_start_count(dt, idx)
    invalid_ids <- unique(dt$survey[invalid_idx])
    failed_ids <- unique(dt$survey[execution_idx])
    invalid_ids <- setdiff(invalid_ids, failed_ids)
    n_invalid <- as.integer(length(invalid_ids))
    n_failed <- as.integer(length(failed_ids))
    no_work <- any(phases == "no_new_data") &&
      length(error_idx) == 0L
    if (no_work) {
      n_total <- 0L
    }
    n_valid <- if (is.na(n_total)) {
      NA_integer_
    } else {
      as.integer(max(0L, n_total - n_invalid - n_failed))
    }
  }
  attempted_line <- if (is.na(n_total)) {
    "**Surveys:** no valid start-entry denominator was recorded."
  } else {
    sprintf(
      paste0(
        "**Surveys:** %d attempted, %d valid, %d invalid, ",
        "%d execution failed."
      ),
      n_total,
      n_valid,
      n_invalid,
      n_failed
    )
  }

  phase_counts <- table(phases)
  phase_lines <- c(
    "### Workflow Phases",
    "",
    "| Phase | Entries |",
    "|-------|--------:|",
    vapply(names(phase_counts), function(phase) {
      sprintf("| `%s` | %d |", phase, phase_counts[[phase]])
    }, character(1))
  )
  lines <- c(
    "## DLW Validation Summary",
    "",
    attempted_line,
    "",
    phase_lines
  )

  detail_line <- function(survey_id, candidate_idx, category_phase = NULL) {
    matches <- candidate_idx[dt$survey[candidate_idx] == survey_id]
    if (length(matches) == 0L) {
      return(sprintf("- `%s`", survey_id))
    }
    i <- matches[[length(matches)]]
    meta <- dt$logmeta[[i]]
    detail <- if (!is.null(meta$condition_msg)) {
      meta$condition_msg
    } else if (!is.null(meta$validation_messages)) {
      paste(meta$validation_messages, collapse = "; ")
    } else {
      dt$message[[i]]
    }
    phase <- if (is.null(category_phase)) .dlw_log_phase(meta) else category_phase
    sprintf("- `%s` (`%s`) - %s", survey_id, phase, detail)
  }
  invalid_lines <- vapply(
    invalid_ids,
    detail_line,
    character(1),
    candidate_idx = invalid_idx,
    category_phase = "validation"
  )
  execution_lines <- vapply(
    failed_ids,
    detail_line,
    character(1),
    candidate_idx = execution_idx
  )
  workflow_lines <- .dlw_workflow_failure_lines(dt, workflow_idx)

  c(
    lines,
    if (length(invalid_lines) > 0L) {
      c("", "**Invalid classifications:**", "", invalid_lines)
    } else {
      character()
    },
    if (length(execution_lines) > 0L) {
      c("", "**Execution failures:**", "", execution_lines)
    } else {
      character()
    },
    if (length(workflow_lines) > 0L) {
      c("", "**Workflow failures:**", "", workflow_lines)
    } else {
      character()
    }
  )
}


# -- Internal helpers -------------------------------------------------------

#' Parse logmeta into a flat data.table
#'
#' Extracts `error_type`, `survey`, and `country` from the nested `logmeta`
#' list-column of a `piplog` object.
#'
#' @param log A `piplog` / `data.table`.
#'
#' @return A `data.table` with columns from `log` plus `error_type`, `survey`,
#'   and `country`.
#' @keywords internal
parse_log_meta <- function(log) {
  dt <- data.table::copy(log)
  data.table::setDT(dt)

  dt[,
    error_type := vapply(
      logmeta,
      \(x) {
        discriminator <- if (!is.null(x$error)) x$error else x$info
        if (is.null(discriminator) || length(discriminator) == 0L) {
          return(NA_character_)
        }
        if (inherits(discriminator, "condition")) {
          return(paste0("legacy_", class(discriminator)[1L]))
        }
        if (!is.character(discriminator)) {
          discriminator <- as.character(discriminator)
        }
        if (length(discriminator) != 1L) {
          return(discriminator[1L])
        }
        return(discriminator)
      },
      character(1)
    )
  ]

  dt[,
    survey := vapply(
      logmeta,
      \(x) {
        s <- x$survey
        if (!is.null(s) && length(s) == 1L) {
          return(s)
        }
        return(NA_character_)
      },
      character(1)
    )
  ]

  dt[,
    country := fifelse(
      is.na(survey),
      NA_character_,
      sub("_.*", "", survey)
    )
  ]

  return(dt)
}


#' Build the header / metadata section of the report
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#' @param title Report title.
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_header <- function(dt, title) {
  time_range <- range(dt$time, na.rm = TRUE)
  duration <- difftime(time_range[2], time_range[1], units = "mins")
  n_errors <- dt[event == "error", .N]
  n_info <- dt[event == "info", .N]

  # Enrich the header only with the selected pipeline run.
  ps <- .latest_pipeline_stage_summary(dt, "process_summary_inf")
  ps_line <- if (!is.null(ps)) {
    sprintf(
      "**Surveys processed:** %d total \u2014 %d cleaned, %d failed",
      ps$n_total,
      ps$n_success,
      ps$n_failed
    )
  } else {
    NULL
  }

  # Build character vector: NULL ps_line is automatically dropped by c() when
  # process_summary_inf is absent from the log. This provides clean formatting
  # for reports that lack process-level metadata. If explicit handling is
  # preferred, refactor to: Filter(Negate(is.null), c(...))
  c(
    sprintf("# %s", title),
    "",
    sprintf(
      "**Run window:** %s \u2192 %s (~%.0f min)",
      format(time_range[1], "%Y-%m-%d %H:%M:%S"),
      format(time_range[2], "%H:%M:%S"),
      as.numeric(duration)
    ),
    sprintf(
      "**Log entries:** %d total (%d errors, %d info)",
      nrow(dt),
      n_errors,
      n_info
    ),
    ps_line
  )
}


#' Build the type-summary table
#'
#' @inheritParams build_header
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_type_summary <- function(dt) {
  dlw_types <- c(
    .logtype_dlw_acquisition,
    .logtype_dlw_validation,
    .logtype_dlw_summary
  )
  tbl <- dt[
    !error_type %in% union(.log_internal_types, dlw_types),
    .N,
    by = .(event, error_type, message)
  ][
    order(event, -N)
  ]

  lines <- c(
    "## Summary by Type",
    "",
    "| Type | Level | Count | Message |",
    "|------|-------|------:|---------|"
  )

  for (i in seq_len(nrow(tbl))) {
    msg <- tbl$message[i]
    # Truncate long messages and strip cli markup
    msg <- gsub("\\{[^}]*\\}", "", msg)
    msg <- gsub("\\\\n", " ", msg)
    msg <- trimws(msg)
    if (nchar(msg) > 80L) {
      msg <- paste0(substr(msg, 1L, 77L), "...")
    }

    lines <- c(
      lines,
      sprintf(
        "| `%s` | %s | %d | %s |",
        tbl$error_type[i],
        toupper(tbl$event[i]),
        tbl$N[i],
        msg
      )
    )
  }

  return(lines)
}


#' Build the country x error_type table
#'
#' @inheritParams build_header
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_country_table <- function(dt) {
  dlw_types <- c(
    .logtype_dlw_acquisition,
    .logtype_dlw_validation,
    .logtype_dlw_summary
  )
  ct <- dt[
    !is.na(country) & !error_type %in% dlw_types,
    .N,
    by = .(country, error_type)
  ][
    order(error_type, country)
  ]

  if (nrow(ct) == 0L) {
    return(c("## Breakdown by Country", "", "No country-level entries found."))
  }

  # Pivot to wide format
  err_types <- sort(unique(ct$error_type))
  countries <- sort(unique(ct$country))

  header <- c(
    "## Breakdown by Country",
    "",
    paste0(
      "| Country | ",
      paste0(sprintf("`%s`", err_types), collapse = " | "),
      " |"
    ),
    paste0(
      "|---------|",
      paste0(rep(":-:", length(err_types)), collapse = "|"),
      "|"
    )
  )

  rows <- vapply(
    countries,
    \(cty) {
      vals <- vapply(
        err_types,
        \(et) {
          n <- ct[country == cty & error_type == et, N]
          if (length(n) == 0L) "\u2014" else as.character(n)
        },
        character(1)
      )
      paste0("| ", cty, " | ", paste(vals, collapse = " | "), " |")
    },
    character(1)
  )

  c(header, rows)
}


#' Build the null-surveys section
#'
#' Extracts the `null_svys_inf` entry (if present) which lists all surveys
#' that were not cleaned.
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_null_surveys <- function(dt) {
  null_idx <- which(vapply(
    dt$logmeta,
    \(x) identical(x$info, "null_svys_inf"),
    logical(1)
  ))

  if (length(null_idx) == 0L) {
    return(character(0))
  }

  surveys <- dt$logmeta[[null_idx[1L]]]$surveys

  if (is.null(surveys) || length(surveys) == 0L) {
    return(character(0))
  }

  c(
    sprintf("## Surveys Not Cleaned (%d)", length(surveys)),
    "",
    vapply(surveys, \(s) sprintf("- `%s`", s), character(1))
  )
}


#' Build the processing summary section
#'
#' Renders counts from the `process_summary_inf` log entry written by
#' [pd_process_data()]. Returns an empty character vector when the entry
#' is absent.
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_processing_summary <- function(dt) {
  ps <- .latest_pipeline_stage_summary(dt, "process_summary_inf")
  if (is.null(ps)) {
    return(character(0))
  }

  c(
    "## Processing Summary",
    "",
    "| Metric | Count |",
    "|--------|------:|",
    sprintf("| Surveys sent for processing | %d |", ps$n_total),
    sprintf("| Successfully cleaned | %d |", ps$n_success),
    sprintf("| Failed | %d |", ps$n_failed)
  )
}


#' Build the deflation summary section
#'
#' Renders counts from the `deflate_summary_inf` log entry written by
#' [pd_deflate_pipeline()]. Returns an empty character vector when the entry
#' is absent.
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_deflation_summary <- function(dt) {
  ds <- .latest_pipeline_stage_summary(
    dt, "deflate_summary_inf", legacy = "last"
  )
  if (is.null(ds)) {
    return(character(0))
  }

  lines <- c(
    "## Deflation Summary",
    "",
    "| Metric | Count |",
    "|--------|------:|",
    sprintf("| Survey candidates | %d |", ds$n_total),
    sprintf("| Successfully deflated | %d |", ds$n_success),
    sprintf("| Failed | %d |", ds$n_failed)
  )

  if (!is.null(ds$surveys_failed) && length(ds$surveys_failed) > 0L) {
    lines <- c(
      lines,
      "",
      "**Surveys that failed deflation:**",
      "",
      vapply(ds$surveys_failed, \(s) sprintf("- `%s`", s), character(1))
    )
  }

  lines
}


#' Build the auxiliary file changes section
#'
#' Renders changed measures and affected survey counts from the
#' `aux_changes_inf` log entry written by [valid_dlw_load()]. Returns an
#' empty character vector when the entry is absent (no aux changes).
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_aux_changes <- function(dt) {
  ac_idx <- which(vapply(
    dt$logmeta,
    \(x) identical(x$info, "aux_changes_inf"),
    logical(1)
  ))

  if (length(ac_idx) == 0L) {
    return(character(0))
  }

  # Aggregate across all aux_changes_inf entries (in case of multiple runs)
  measures_all  <- unique(unlist(lapply(ac_idx, \(i) dt$logmeta[[i]]$measures)))
  n_affected    <- sum(vapply(
    ac_idx,
    \(i) {
      n <- dt$logmeta[[i]]$n_surveys_affected
      if (is.null(n)) 0L else as.integer(n)
    },
    integer(1)
  ))
  surveys_all <- unique(unlist(lapply(
    ac_idx,
    \(i) dt$logmeta[[i]]$surveys_affected
  )))

  survey_lines <- if (length(surveys_all) > 0L) {
    c(
      "",
      "**Surveys affected by aux changes:**",
      "",
      vapply(surveys_all, \(s) sprintf("- `%s`", s), character(1))
    )
  } else {
    character(0)
  }

  c(
    "## Auxiliary File Changes",
    "",
    sprintf(
      "**Measures changed (%d):** %s",
      length(measures_all),
      paste(sprintf("`%s`", measures_all), collapse = ", ")
    ),
    sprintf("**Surveys affected:** %d", n_affected),
    survey_lines
  )
}


#' Build the inventory verification section
#'
#' Renders the cross-check between successfully cleaned surveys and the master
#' inventory, from the `inv_update_inf` log entry written by
#' [build_pip_inventory()]. Lists any surveys confirmed missing.
#' Returns an empty character vector when the entry is absent.
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_inventory_additions <- function(dt) {
  # Entry is info when all surveys confirmed, error when some are missing
  iv_idx <- which(vapply(
    dt$logmeta,
    \(x) identical(x$info, "inv_update_inf") ||
         identical(x$error, "inv_update_inf"),
    logical(1)
  ))

  if (length(iv_idx) == 0L) {
    return(character(0))
  }

  iv <- dt$logmeta[[iv_idx[1L]]]

  # Guard against malformed logmeta entries
  if (is.null(iv$n_expected) || is.null(iv$n_confirmed) || is.null(iv$n_missing)) {
    return(character(0))
  }

  lines <- c(
    "## Inventory Verification",
    "",
    "| Metric | Count |",
    "|--------|------:|",
    sprintf("| Expected (successfully cleaned) | %d |", iv$n_expected),
    sprintf("| Confirmed in master inventory | %d |", iv$n_confirmed),
    sprintf("| Missing from master inventory | %d |", iv$n_missing)
  )

  if (!is.null(iv$surveys_missing) && length(iv$surveys_missing) > 0L) {
    lines <- c(
      lines,
      "",
      "**Surveys missing from inventory:**",
      "",
      vapply(iv$surveys_missing, \(s) sprintf("- `%s`", s), character(1))
    )
  }

  return(lines)
}


#' Build the skipped-surveys section
#'
#' Reads `skipped_svys_data` and `skipped_svys_metadata` log entries written
#' by [build_pip_inventory()] and renders each group with its skip reasons.
#' Returns an empty character vector when no skipped-survey entries exist.
#'
#' @param dt Parsed log `data.table` (output of [parse_log_meta()]).
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_skipped_surveys <- function(dt) {
  data_idx <- which(vapply(
    dt$logmeta,
    \(x) identical(x$info, "skipped_svys_data"),
    logical(1)
  ))
  meta_idx <- which(vapply(
    dt$logmeta,
    \(x) identical(x$info, "skipped_svys_metadata"),
    logical(1)
  ))

  if (length(data_idx) == 0L && length(meta_idx) == 0L) {
    return(character(0))
  }

  collect_rows <- function(indices, stage_label) {
    surveys <- unique(unlist(lapply(indices, \(i) dt$logmeta[[i]]$surveys)))
    reasons <- unique(unlist(lapply(indices, \(i) dt$logmeta[[i]]$reasons)))
    if (length(surveys) == 0L) return(character(0))
    rows <- vapply(
      seq_along(surveys),
      \(i) {
        reason <- if (!is.null(reasons) && i <= length(reasons) && !is.na(reasons[i]))
          reasons[i] else "unknown"
        sprintf("- `%s` \u2014 %s", surveys[i], reason)
      },
      character(1)
    )
    c(
      sprintf("**Skipped during %s (%d):**", stage_label, length(surveys)),
      "",
      rows
    )
  }

  data_rows <- collect_rows(data_idx, "data processing")
  meta_rows <- collect_rows(meta_idx, "metadata creation")

  n_total <- length(unique(unlist(lapply(
    c(data_idx, meta_idx), \(i) dt$logmeta[[i]]$surveys
  ))))

  sep <- if (length(data_rows) > 0L && length(meta_rows) > 0L) "" else character(0)

  c(
    sprintf("## Skipped Surveys (%d)", n_total),
    "",
    data_rows,
    sep,
    meta_rows
  )
}
