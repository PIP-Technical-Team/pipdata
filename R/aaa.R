
# Unified package-level environment. Stores mutable state for both pipeline
# wrappers using namespaced keys:
#   save_*      — save_pip.R / pd_process_data.R save context
#   process_*   — pd_process_data.R survey-loop context
#   validation_ — DLW validation accumulator (was .pipdata)
#   log_*       — deflation error log (was .logenv)
.pipdataenv <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Package environment accessors — use these instead of raw assign()/rm()/get()
# ---------------------------------------------------------------------------

#' Set a value in the unified package environment
#'
#' @param key Character key name. Use namespaced prefixes (`save_`, `process_`,
#'   `validation_`, `log_`) to avoid collisions.
#' @param value Value to store.
#' @return `value` invisibly.
#' @noRd
pd_env_set <- function(key, value) {
  assign(key, value, envir = .pipdataenv)
  invisible(value)
}

#' Get a value from the unified package environment
#'
#' @param key Character key name.
#' @param default Value to return when `key` is absent (default: `NULL`).
#' @return The stored value, or `default`.
#' @noRd
pd_env_get <- function(key, default = NULL) {
  if (rlang::env_has(.pipdataenv, key)) {
    return(get(key, envir = .pipdataenv))
  }
  default
}

#' Remove a key from the unified package environment
#'
#' @param key Character key name to remove. No-op if absent.
#' @return `NULL` invisibly.
#' @noRd
pd_env_rm <- function(key) {
  if (rlang::env_has(.pipdataenv, key)) {
    rm(list = key, envir = .pipdataenv)
  }
  invisible(NULL)
}

#' Reset the unified package environment (remove all keys)
#'
#' @return `NULL` invisibly.
#' @noRd
pd_env_reset <- function() {
  rm(list = ls(.pipdataenv), envir = .pipdataenv)
  invisible(NULL)
}

#' Append rows to a data.table stored in the package environment
#'
#' If `key` does not yet exist, stores `new_rows` directly.
#' If it exists, `rbind`s `new_rows` onto the existing value.
#'
#' @param key Character key name.
#' @param new_rows A data.table to append.
#' @return The updated value invisibly.
#' @noRd
pd_env_append <- function(key, new_rows) {
  existing <- pd_env_get(key)
  if (is.null(existing)) {
    pd_env_set(key, new_rows)
  } else {
    pd_env_set(key, rbind(existing, new_rows, ignore.attr = TRUE))
  }
}

# Internal logmeta type markers -- excluded from the summary-by-type table
# in log_report() so it only shows genuine pipeline errors/warnings.
.log_internal_types <- c(
  "process_summary_inf",
  "aux_changes_inf",
  "inv_update_inf",
  "null_svys_inf",
  "skipped_svys_data",
  "skipped_svys_metadata",
  "release_write_err"
)

# Suppress R CMD check notes for unquoted data.table column names and other
# symbols used in non-standard evaluation throughout the package.
utils::globalVariables(c(
  # data.table NSE column names
  "..key",
  "..selected_vars",
  ".data",
  ".joyn",
  "Checksum",
  "Checksum_dlw",
  "Ext",
  "FileName",
  "Module",
  "N",
  "age",
  "code_hash",
  "content_hash_data",
  "content_hash_dlw",
  "content_hash_metadata",
  "count_valid",
  "country",
  "country_code",
  "created_at_data",
  "created_at_metadata",
  "data_available",
  "data_status",
  "description",
  "first_release_version_id",
  "latest_release_version_id",
  "latest_version_id_dlw",
  "dlw_meta",
  "educat7",
  "educy",
  "error_type",
  "event",
  "ext",
  "hhid",
  "inpovcal",
  "logmeta",
  "maxalt",
  "maxmast",
  "maxpip",
  "module_type",
  "path",
  "path_data",
  "path_dlw",
  "path_metadata",
  "pid",
  "pin_version",
  "pip_id",
  "pipeline_version_dlw",
  "rf_year",
  "school",
  "size_bytes_data",
  "size_bytes_metadata",
  "status",
  "status_count",
  "survey",
  "survey_acronym",
  "survey_id",
  "surveyid_year",
  "table_name",
  "tool",
  "type",
  "version_dlw",
  "version_id_data",
  "version_id_metadata",
  "welfare_type"
))
