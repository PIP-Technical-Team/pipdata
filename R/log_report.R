#' Generate a markdown report from a pipeline log
#'
#' Parses a `piplog` object produced by `pipfun::log_filter()` and writes a
#' structured markdown document summarising errors, informational messages, and
#' affected surveys.
#'
#' @param log A `piplog` object (inherits from `data.table`).
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
#'   \item Run metadata (time window, total entries, success/fail counts).
#'   \item Processing summary: total, cleaned, and failed counts
#'     (from `process_summary_inf` log entry).
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
#' Sections that rely on a specific logmeta entry are silently omitted when
#' that entry is absent from the log.
#'
#' @family pd_process_data pipeline
#' @export
#'
#' @examples
#' \dontrun{
#' log <- pipfun::log_filter(name = "pipdata_log")
#' # Return as character vector
#' report <- log_report(log)
#' # Write to file
#' log_report(log, path = "log_report.md", overwrite = TRUE)
#' }
log_report <- function(
  log,
  path = NULL,
  title = "Pipeline Log Report",
  overwrite = FALSE
) {
  # --- Validation ----------------------------------------------------------
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
      build_processing_summary(dt),
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


# ── Internal helpers ─────────────────────────────────────────────────────────

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
        if (!is.null(x$error)) {
          return(x$error)
        }
        if (!is.null(x$info)) {
          return(x$info)
        }
        return(NA_character_)
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

  # Enrich header with survey counts when process_summary_inf is present
  ps_idx <- which(vapply(
    dt$logmeta,
    \(x) identical(x$info, "process_summary_inf"),
    logical(1)
  ))
  ps_line <- if (length(ps_idx) > 0L) {
    ps <- dt$logmeta[[ps_idx[1L]]]
    sprintf(
      "**Surveys processed:** %d total \u2014 %d cleaned, %d failed",
      ps$n_total,
      ps$n_success,
      ps$n_failed
    )
  } else {
    NULL
  }

  # NULL ps_line is silently dropped by c() when process_summary_inf is absent.
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
  tbl <- dt[
    !error_type %in% .log_internal_types,
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


#' Build the country × error_type table
#'
#' @inheritParams build_header
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_country_table <- function(dt) {
  ct <- dt[!is.na(country), .N, by = .(country, error_type)][
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
  ps_idx <- which(vapply(
    dt$logmeta,
    \(x) identical(x$info, "process_summary_inf"),
    logical(1)
  ))

  if (length(ps_idx) == 0L) {
    return(character(0))
  }

  ps <- dt$logmeta[[ps_idx[1L]]]

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
#' [update_pip_inventory()]. Lists any surveys confirmed missing.
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
#' by [update_pip_inventory()] and renders each group with its skip reasons.
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
        sprintf("- `%s` — %s", surveys[i], reason)
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
