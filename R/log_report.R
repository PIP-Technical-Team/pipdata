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
#'   \item Run metadata (time window, total entries).
#'   \item Summary table by error / info type.
#'   \item Country-level breakdown of errors.
#'   \item List of surveys that failed processing (`null_svys_inf` entry).
#' }
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
  lines <- c(
    build_header(dt, title),
    "",
    build_type_summary(dt),
    "",
    build_country_table(dt),
    "",
    build_null_surveys(log),
    ""
  )

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
      "**Total entries:** %d (%d errors, %d info)",
      nrow(dt),
      n_errors,
      n_info
    )
  )
}


#' Build the type-summary table
#'
#' @inheritParams build_header
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_type_summary <- function(dt) {
  tbl <- dt[, .N, by = .(event, error_type, message)][
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
    return("## Errors by Country\n\nNo country-level entries found.")
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
#' @param log The original `piplog` object.
#'
#' @return Character vector of markdown lines.
#' @keywords internal
build_null_surveys <- function(log) {
  dt <- data.table::as.data.table(log)

  null_idx <- which(vapply(
    dt$logmeta,
    \(x) {
      identical(x$info, "null_svys_inf")
    },
    logical(1)
  ))

  if (length(null_idx) == 0L) {
    return(character(0))
  }

  surveys <- dt$logmeta[[null_idx[1]]]$surveys

  if (is.null(surveys) || length(surveys) == 0L) {
    return(character(0))
  }

  c(
    sprintf("## Surveys Not Cleaned (%d)", length(surveys)),
    "",
    vapply(surveys, \(s) sprintf("- `%s`", s), character(1))
  )
}
