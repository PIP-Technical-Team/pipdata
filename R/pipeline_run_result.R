.PDP_SCHEMA <- 1L
.PDP_STAGES <- c("clean", "metadata", "deflate")
.PDP_STATUSES <- c("success", "partial", "failed", "cached", "skipped")
.PDP_COUNT_NAMES <- c(
  "selected", "attempted", "succeeded", "failed", "skipped", "cached",
  "blocked", "warnings", "errors"
)

pd_pipeline_abort <- function(message) {
  rlang::abort(message, class = "pipdata_pipeline_result_invalid")
}

pd_pipeline_utc_string <- function(x, name) {
  x <- pd_scalar_character(x, name)
  canonical <- grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[.][0-9]{6}Z$",
    x
  )
  if (!canonical) {
    pd_pipeline_abort(paste(name, "must be a canonical UTC timestamp."))
  }
  x
}

pd_pipeline_counts <- function(stage_results, warnings, errors) {
  accepted <- Filter(Negate(is.null), stage_results)
  sum_count <- function(name) {
    sum(vapply(accepted, function(result) result$counts[[name]], integer(1L)))
  }
  blocked <- sum(vapply(accepted, function(result) {
    units <- data.table::as.data.table(result$units)
    sum(vapply(seq_len(nrow(units)), function(i) {
      identical(units$status[[i]], "skipped") &&
        "upstream_failed" %in% units$reason_codes[[i]]
    }, logical(1L)))
  }, integer(1L)))
  values <- c(
    selected = sum_count("selected"),
    attempted = sum_count("attempted"),
    succeeded = sum_count("succeeded"),
    failed = sum_count("failed"),
    skipped = sum_count("skipped"),
    cached = sum_count("cached"),
    blocked = blocked,
    warnings = sum_count("warnings") + length(warnings),
    errors = sum_count("errors") + length(errors)
  )
  as.list(stats::setNames(as.integer(values), .PDP_COUNT_NAMES))
}

pd_pipeline_status <- function(counts, terminal) {
  if (isTRUE(terminal)) {
    return(if (counts$succeeded > 0L) "partial" else "failed")
  }
  if (counts$attempted > 0L && counts$succeeded > 0L && counts$failed > 0L) {
    return("partial")
  }
  if (counts$attempted > 0L && counts$failed == counts$attempted) {
    return("failed")
  }
  if (counts$attempted > 0L && counts$succeeded == counts$attempted) {
    return("success")
  }
  if (counts$selected == 0L) return("skipped")
  if (counts$cached == counts$selected) return("cached")
  "skipped"
}

pd_pipeline_manifest_chain <- function(stage_results, manifest_before,
                                       manifest_after) {
  accepted <- Filter(Negate(is.null), stage_results)
  if (!length(accepted)) return(invisible(NULL))
  if (!identical(accepted[[1L]]$provenance$manifest_before, manifest_before)) {
    pd_pipeline_abort("The first accepted wave has a different manifest parent.")
  }
  if (length(accepted) > 1L) {
    for (i in 2:length(accepted)) {
      if (!identical(
        accepted[[i - 1L]]$provenance$manifest_after,
        accepted[[i]]$provenance$manifest_before
      )) {
        pd_pipeline_abort("Accepted stage manifests do not form one chain.")
      }
    }
  }
  final_evidence <- lapply(accepted, function(result) {
    if (identical(result$schema_version, .PDS_SCHEMA)) {
      result$provenance$final_evidence_manifest
    } else {
      result$provenance$manifest_after
    }
  })
  if (any(!vapply(final_evidence, identical, logical(1L), manifest_after))) {
    pd_pipeline_abort("Stage results do not use the final run evidence manifest.")
  }
  invisible(NULL)
}

new_pipdata_pipeline_result <- function(
  run_id,
  stage_results,
  warnings,
  errors,
  plan_hashes,
  manifest_before,
  manifest_after,
  log_ref,
  started_at,
  completed_at,
  terminal = FALSE
) {
  if (!is.list(stage_results) ||
      !identical(names(stage_results), .PDP_STAGES)) {
    pd_pipeline_abort("Stage results must use the frozen stage slots.")
  }
  invisible(lapply(stage_results, function(result) {
    if (!is.null(result)) validate_pipdata_stage_result(result)
  }))
  counts <- pd_pipeline_counts(stage_results, warnings, errors)
  result <- structure(list(
    schema_version = .PDP_SCHEMA,
    run_id = pd_scalar_character(run_id, "run_id"),
    status = pd_pipeline_status(counts, terminal),
    terminal = terminal,
    stage_results = stage_results,
    counts = counts,
    warnings = warnings,
    errors = errors,
    plan_hashes = plan_hashes,
    manifest_before = manifest_before,
    manifest_after = manifest_after,
    log_ref = log_ref,
    started_at = pd_utc_time(started_at),
    completed_at = pd_utc_time(completed_at)
  ), class = c("pipdata_pipeline_result", "list"))
  validate_pipdata_pipeline_result(result)
  result
}

validate_pipdata_pipeline_result <- function(x, portable = FALSE) {
  expected <- c(
    "schema_version", "run_id", "status", "terminal", "stage_results",
    "counts", "warnings", "errors", "plan_hashes", "manifest_before",
    "manifest_after", "log_ref", "started_at", "completed_at"
  )
  if (!is.list(x) || !identical(names(x), expected) ||
      !identical(x$schema_version, .PDP_SCHEMA) ||
      !x$status %in% .PDP_STATUSES || !is.logical(x$terminal) ||
      length(x$terminal) != 1L || is.na(x$terminal) ||
      !is.list(x$stage_results) ||
      !identical(names(x$stage_results), .PDP_STAGES) ||
      !is.list(x$warnings) || !is.list(x$errors)) {
    pd_pipeline_abort("Pipeline result has an invalid top-level schema.")
  }
  pd_scalar_character(x$run_id, "run_id")
  if (!identical(names(x$counts), .PDP_COUNT_NAMES) ||
      any(!vapply(x$counts, function(value) {
        is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L
      }, logical(1L)))) {
    pd_pipeline_abort("Pipeline result counts have an invalid schema.")
  }
  if (!is.character(x$plan_hashes) ||
      !identical(names(x$plan_hashes), c("initial", .PDP_STAGES)) ||
      length(x$plan_hashes) != 4L || is.na(x$plan_hashes[["initial"]]) ||
      !nzchar(x$plan_hashes[["initial"]])) {
    pd_pipeline_abort("Pipeline result plan hashes are invalid.")
  }
  if (!pd_manifest_identity_valid(x$manifest_before) ||
      !pd_manifest_identity_valid(x$manifest_after)) {
    pd_pipeline_abort("Pipeline result manifest identities are invalid.")
  }
  if (is.null(x$manifest_after) && !is.null(x$manifest_before)) {
    pd_pipeline_abort("A run cannot discard an existing valid manifest.")
  }
  log_names <- c("name", "run_id", "summary_discriminator", "log_checkpoint")
  if (!is.list(x$log_ref) || !identical(names(x$log_ref), log_names) ||
      !identical(x$log_ref$name, "pipdata_log") ||
      !identical(x$log_ref$run_id, x$run_id) ||
      !identical(
        x$log_ref$summary_discriminator, "pipeline_run_summary_inf"
      ) || !is.null(x$log_ref$log_checkpoint)) {
    pd_pipeline_abort("Pipeline result log reference is invalid.")
  }
  for (i in seq_along(.PDP_STAGES)) {
    stage <- .PDP_STAGES[[i]]
    stage_result <- x$stage_results[[stage]]
    plan_hash <- x$plan_hashes[[stage]]
    if (is.null(stage_result)) {
      if (!is.na(plan_hash)) {
        pd_pipeline_abort("An unavailable wave has a fabricated plan hash.")
      }
      next
    }
    validate_pipdata_stage_result(stage_result, portable = portable)
    if (!identical(stage_result$stage, stage) ||
        !identical(stage_result$run_id, x$run_id) ||
        is.na(plan_hash) || !nzchar(plan_hash) ||
        !identical(stage_result$provenance$plan_hash, plan_hash)) {
      pd_pipeline_abort("A stage result does not match its aggregate slot.")
    }
  }
  if (any(vapply(x$stage_results, function(result) {
    !is.null(result) && isTRUE(result$terminal)
  }, logical(1L))) && !isTRUE(x$terminal)) {
    pd_pipeline_abort("A terminal stage result requires a terminal run.")
  }
  invisible(lapply(x$warnings, validate_stage_condition_record,
                   portable = portable))
  invisible(lapply(x$errors, validate_stage_condition_record,
                   portable = portable))
  run_conditions <- c(x$warnings, x$errors)
  if (length(run_conditions) && any(vapply(run_conditions, function(record) {
    !identical(record$operation, "run_boundary") ||
      !record$stage %in% .PDP_STAGES
  }, logical(1L)))) {
    pd_pipeline_abort("Run-boundary conditions are invalid.")
  }
  pd_pipeline_manifest_chain(
    x$stage_results, x$manifest_before, x$manifest_after
  )
  expected_counts <- pd_pipeline_counts(
    x$stage_results, x$warnings, x$errors
  )
  if (!identical(x$counts, expected_counts) ||
      !identical(x$status, pd_pipeline_status(x$counts, x$terminal)) ||
      x$counts$blocked > x$counts$skipped) {
    pd_pipeline_abort("Pipeline result counts or status are contradictory.")
  }
  if (portable) {
    pd_pipeline_utc_string(x$started_at, "started_at")
    pd_pipeline_utc_string(x$completed_at, "completed_at")
  } else {
    pd_utc_time(x$started_at)
    pd_utc_time(x$completed_at)
    if (x$completed_at < x$started_at) {
      pd_pipeline_abort("Pipeline result timestamps reverse.")
    }
  }
  invisible(x)
}

pd_pipeline_result_portable <- function(x) {
  validate_pipdata_pipeline_result(x)
  out <- unclass(x)
  out$stage_results <- lapply(x$stage_results, function(result) {
    if (is.null(result)) NULL else pd_stage_result_portable(result)
  })
  condition_order <- function(records) {
    records <- lapply(records, pd_portable_condition)
    if (length(records)) {
      records[order(vapply(records, `[[`, "", "condition_id"))]
    } else {
      records
    }
  }
  out$warnings <- condition_order(x$warnings)
  out$errors <- condition_order(x$errors)
  out$started_at <- format(
    x$started_at, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"
  )
  out$completed_at <- format(
    x$completed_at, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"
  )
  validate_pipdata_pipeline_result(out, portable = TRUE)
  out
}

#' Print a compact pipeline run result
#' @param x A `pipdata_pipeline_result`.
#' @param ... Unused.
#' @export
print.pipdata_pipeline_result <- function(x, ...) {
  cat(sprintf(
    "<pipdata_pipeline_result v%d> run=%s: %s%s\n",
    x$schema_version, x$run_id, x$status,
    if (x$terminal) " (terminal)" else ""
  ))
  cat(
    paste(names(x$counts), unlist(x$counts), sep = "=", collapse = ", "),
    "\n"
  )
  invisible(x)
}
