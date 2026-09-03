
# Unified package-level environment. Stores mutable state for both pipeline
# wrappers using namespaced keys:
#   save_*      — save_pip.R / pd_process_data.R save context
#   process_*   — pd_process_data.R survey-loop context
#   validation_ — DLW validation accumulator (was .pipdata)
#   log_*       — deflation error log (was .logenv)
.pipdataenv <- new.env(parent = emptyenv())

utils::globalVariables(c(
  "i.code_hash", "i.input_hash", "i.output_hash", "i.output_version_id",
  "i.pip_id", "i.survey_id", "name", "output_hash", "output_version_id"
  , "aux_projection", "hash_new", "hash_old", "manifest_input_hash"
))

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

# Canonical DLW logmeta discriminators.
.logtype_dlw_acquisition <- "dlw_acquisition_inf"
.logtype_dlw_validation <- "dlw_validation_inf"
.logtype_dlw_summary <- "dlw_summary_inf"

# Internal logmeta type markers -- excluded from the summary-by-type table
# in log_report() so it only shows genuine pipeline errors/warnings.
.log_internal_types <- c(
  "process_summary_inf",
  "aux_changes_inf",
  "inv_update_inf",
  "null_svys_inf",
  "skipped_svys_data",
  "skipped_svys_metadata",
  "release_write_err",
  "deflate_summary_inf",
  "pipeline_run_summary_inf",
  .logtype_dlw_acquisition,
  .logtype_dlw_validation,
  .logtype_dlw_summary
)

.validate_pip_write_result <- function(result, artifact) {
  has_version <- is.list(result) &&
    !is.null(result$version_id) &&
    length(result$version_id) == 1L &&
    !is.na(result$version_id) &&
    nzchar(as.character(result$version_id))
  was_skipped <- is.list(result) && isTRUE(result$skipped)

  if (!has_version && !was_skipped) {
    rlang::abort(
      paste0("Persistence did not return a version for `", artifact, "`."),
      class = c("pipdata_persistence_error", "piperr")
    )
  }

  invisible(result)
}

# Column-pointer sentinel registry for *_data_level attributes.
# Keys are sentinel strings stored in the attribute; values are the
# column names they point to. Anything not in this registry is treated
# as a literal level value (e.g. "national") and broadcast as a scalar.
# This registry governs dispatch only: adjust_population() and
# finalize_deflation_output() still intentionally hard-code "area".
# Update those consumers before registering another sentinel.
.data_level_columns <- list(area = "area")

#' Resolve a data_level attribute to a column name
#'
#' Returns the column name when `lvl` is a registered column-pointer
#' sentinel, or `NA_character_` when `lvl` is a literal level value
#' (e.g. `"national"`) that should be broadcast as a scalar. Also
#' returns `NA_character_` for degenerate inputs (`NULL`,
#' `character(0)`, `NA_character_`, multi-element vectors, or
#' non-character scalars) so that a missing or empty `*_data_level`
#' attribute safely falls through to the scalar-broadcast branch rather
#' than crashing.
#'
#' @param lvl Character scalar (or `NULL`/`character(0)`/`NA`/other
#'   degenerate shape). Value of a `*_data_level` attribute.
#' @return Character scalar column name, or `NA_character_`.
#' @noRd
data_level_column <- function(lvl) {
  if (is.null(lvl) || !is.character(lvl) || length(lvl) != 1L || is.na(lvl)) {
    return(NA_character_)
  }
  col <- .data_level_columns[[lvl]]
  if (is.null(col)) NA_character_ else col
}

# Suppress R CMD check notes for unquoted data.table column names and other
# symbols used in non-standard evaluation throughout the package.
utils::globalVariables(c(
  # data.table NSE column names
  "..key",
  "..keys",
  "..artifact_keys",
  "..aux_columns",
  "..current_columns",
  "..dlw_order",
  "..domain_vars",
  "..domains",
  "..duplicate_fields",
  "..entity_id",
  "..metadata_fields",
  "..pip_id",
  "..reporting_level",
  "..required",
  "..survey_acronym",
  "..survey_id",
  "..stage",
  "..year",
  "..selected_vars",
  ".data",
  ".joyn",
  ".pd_country_code",
  ".pd_survey_acronym",
  ".pd_year",
  "Checksum",
  "Checksum_dlw",
  "Ext",
  "FileName",
  "Module",
  "N",
  "Year",
  "age",
  "action",
  "action_rank",
  "alias",
  "artifact",
  "code_hash",
  "content_hash_data",
  "content_hash_dlw",
  "content_hash_metadata",
  "content_hash",
  "content_hash_new",
  "content_hash_old",
  "component",
  "count_valid",
  "country",
  "country_code",
  "created_at",
  "created_at_data",
  "created_at_metadata",
  "data_available",
  "data_hash",
  "data_status",
  "data_version_id",
  "date_validated",
  "aux_cpi_hash_at_deflation",
  "aux_pop_hash_at_deflation",
  "aux_ppp_hash_at_deflation",
  "content_hash_deflated",
  "i.content_hash_deflated",
  "i.action",
  "i.entity_id",
  "i.stage",
  "deflated",
  "description",
  "entity_id",
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
  "hash",
  "input",
  "input_hash",
  "inpovcal",
  "logmeta",
  "maxalt",
  "maxmast",
  "maxpip",
  "module_type",
  "metadata_hash",
  "metadata_version_id",
  "next_pipeline_version",
  "path",
  "path_data",
  "path_dlw",
  "path_metadata",
  "pid",
  "pin_version",
  "pip_id",
  "pipeline_version_dlw",
  "rf_year",
  "reason",
  "reconstruct_base_metadata",
  "root",
  "school",
  "scheduling_state",
  "size_bytes_data",
  "size_bytes_metadata",
  "status",
  "status_count",
  "stage",
  "state",
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
  "version_id_deflated",
  "version_id",
  "version_id_new",
  "version_id_old",
  "i.version_id",
  "i.content_hash",
  "version_id_recode_spec",
  "wave_state",
  "welfare_type"
))
