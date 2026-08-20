# ── Validation Spec Loader & Schema Validator ──────────────────────────────────

.known_validation_types <- c(
  "variable_availability",
  "numeric_validation",
  "character_validation",
  "validation_group",
  "single_variable",
  "categorical_check",
  "not_missing",
  "uniqueness",
  "value_constraint",
  "data_presence"
)

.helper_fixed_checks <- c(
  "is_numeric",
  "is_positive",
  "is_positive_or_zero",
  "is_character",
  "is_greaterthanzero",
  "is_greaterequale0",
  "is_var_avail",
  "is_var_startwith_avail",
  "value_range",
  "check_urban",
  "check_gender",
  "is_valuebtwn0and110"
)

.valid_severities <- c("critical", "warning", "helper")

# Authoritative dispatch table for check names the engine can execute.
# Any check name not in this table is silently inert — validate against it.
.known_check_dispatch <- c(
  "is_numeric",
  "is_positive",
  "is_positive_or_zero",
  "is_character",
  "is_greaterthanzero",
  "is_greaterequale0",
  "is_var_avail",
  "is_var_startwith_avail",
  "check_urban",
  "check_gender",
  "is_valuebtwn0and110",
  "not_missing",
  "na_threshold"
)

.known_secondary_checks <- c("is_valuebtwn0and110")

#'.validation_spec_cache is stored in .pipdataenv to avoid locked-binding issues

#' Load the package validation spec
#'
#' @return A parsed validation spec list.
#' @keywords internal
load_package_validation_spec <- function() {
  spec_path <- system.file("extdata", "validation_spec.yml", package = "pipdata")
  if (!file.exists(spec_path)) {
    cli::cli_abort(
      c(
        "validation_spec.yml not found in inst/extdata/",
        "i" = "Expected path: {.path {spec_path}}"
      ),
      class = c("validation_spec_missing", "piperr")
    )
  }
  spec <- yaml::read_yaml(spec_path)
  validate_validation_spec(spec)
  spec
}

#' Validate validation spec schema
#'
#' @param spec A parsed validation spec list.
#' @return `TRUE` if valid.
#' @keywords internal
validate_validation_spec <- function(spec) {
  if (is.null(spec$schema_version)) {
    cli::cli_abort(
      "validation_spec missing {.field schema_version}",
      class = c("validation_spec_invalid", "piperr")
    )
  }

  for (module_name in names(spec$modules)) {
    module <- spec$modules[[module_name]]

    if (is.null(module$validations)) {
      cli::cli_abort(
        "Module {.field {module_name}} missing {.field validations}",
        class = c("validation_spec_invalid", "piperr")
      )
    }

    for (val_name in names(module$validations)) {
      entry <- module$validations[[val_name]]

      if (is.null(entry$type)) {
        cli::cli_abort(
          "Validation {.field {val_name}} in module {.field {module_name}} missing {.field type}",
          class = c("validation_spec_invalid", "piperr")
        )
      }

      if (!entry$type %in% .known_validation_types) {
        cli::cli_abort(
          c(
            "Validation {.field {val_name}} in module {.field {module_name}} has unknown type {.val {entry$type}}",
            "i" = "Known types: {.val {(.known_validation_types)}}"
          ),
          class = c("validation_spec_invalid", "piperr")
        )
      }

      switch(entry$type,
        variable_availability = {
          if (is.null(entry$prefix)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (variable_availability) missing {.field prefix}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (!is.null(entry$pattern)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (variable_availability) must not have {.field pattern}; use {.field prefix} instead",
              class = c("validation_spec_invalid", "piperr")
            )
          }
        },
        numeric_validation = , # empty cases fall through to validation_group handling
        character_validation = ,
        validation_group = {
          if (is.null(entry$pattern)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} ({entry$type}) missing {.field pattern}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (!is.null(entry$checks) && length(entry$checks) == 0) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} ({entry$type}) has empty {.field checks}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
        },
        single_variable = {
          if (is.null(entry$variable)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (single_variable) missing {.field variable}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (is.null(entry$check)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (single_variable) missing {.field check}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (!entry$check %in% .known_check_dispatch) {
            cli::cli_abort(
              c(
                "Validation {.field {val_name}} in module {.field {module_name}} (single_variable) unknown check {.val {entry$check}}",
                "i" = "Known checks: {.val {(.known_check_dispatch)}}"
              ),
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (!is.null(entry$secondary_check) && !entry$secondary_check %in% .known_secondary_checks) {
            cli::cli_abort(
              c(
                "Validation {.field {val_name}} in module {.field {module_name}} (single_variable) unknown secondary_check {.val {entry$secondary_check}}",
                "i" = "Known secondary checks: {.val {(.known_secondary_checks)}}"
              ),
              class = c("validation_spec_invalid", "piperr")
            )
          }
        },
        categorical_check = {
          if (is.null(entry$variable)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (categorical_check) missing {.field variable}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (is.null(entry$check)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (categorical_check) missing {.field check}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (!entry$check %in% .known_check_dispatch) {
            cli::cli_abort(
              c(
                "Validation {.field {val_name}} in module {.field {module_name}} (categorical_check) unknown check {.val {entry$check}}",
                "i" = "Known checks: {.val {(.known_check_dispatch)}}"
              ),
              class = c("validation_spec_invalid", "piperr")
            )
          }
        },
        not_missing = {
          if (is.null(entry$variable)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (not_missing) missing {.field variable}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (is.null(entry$severity)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (not_missing) missing {.field severity}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (!is.null(entry$condition) && !entry$condition %in% c("hhid_present", "hhid_and_pid_present")) {
            cli::cli_abort(
              c(
                "Validation {.field {val_name}} in module {.field {module_name}} (not_missing) unknown condition {.val {entry$condition}}",
                "i" = "Known conditions: {.val {c('hhid_present', 'hhid_and_pid_present')}}"
              ),
              class = c("validation_spec_invalid", "piperr")
            )
          }
        },
        value_constraint = {
          if (is.null(entry$variable)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (value_constraint) missing {.field variable}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (is.null(entry$valid_values)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (value_constraint) missing {.field valid_values}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
        },
        uniqueness = {
          if (is.null(entry$key_variables)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (uniqueness) missing {.field key_variables}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (is.null(entry$severity)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (uniqueness) missing {.field severity}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (!is.null(entry$condition) && !entry$condition %in% c("hhid_and_pid_present")) {
            cli::cli_abort(
              c(
                "Validation {.field {val_name}} in module {.field {module_name}} (uniqueness) unknown condition {.val {entry$condition}}",
                "i" = "Known conditions: {.val {c('hhid_and_pid_present')}}"
              ),
              class = c("validation_spec_invalid", "piperr")
            )
          }
        },
        data_presence = {
          if (is.null(entry$severity)) {
            cli::cli_abort(
              "Validation {.field {val_name}} in module {.field {module_name}} (data_presence) missing {.field severity}",
              class = c("validation_spec_invalid", "piperr")
            )
          }
        }
      )

      # Validate severity value if present on any entry type
      if (!is.null(entry$severity) && !(entry$severity %in% .valid_severities)) {
        cli::cli_abort(
          c(
            "Validation {.field {val_name}} in module {.field {module_name}} has invalid severity {.val {entry$severity}}",
            "i" = "Valid severity values: {.val {(.valid_severities)}}"
          ),
          class = c("validation_spec_invalid", "piperr")
        )
      }

      if (!is.null(entry$checks)) {
        for (chk in entry$checks) {
          chk_name <- if (is.list(chk)) chk$name else chk
          if (!chk_name %in% .known_check_dispatch) {
            cli::cli_abort(
              c(
                "Validation {.field {val_name}} in module {.field {module_name}} has unknown check {.val {chk_name}}",
                "i" = "Known checks: {.val {(.known_check_dispatch)}}"
              ),
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (is.list(chk) && chk_name %in% .helper_fixed_checks &&
              !is.null(chk$severity) && chk$severity != "helper") {
            cli::cli_abort(
              c(
                "Validation {.field {val_name}} in module {.field {module_name}}: severity on helper-fixed check {.field {chk_name}} is inert",
                "i" = "Helper-fixed checks ignore spec severity; remove it or set severity to 'helper'"
              ),
              class = c("validation_spec_invalid", "piperr")
            )
          }
          if (is.list(chk) && !is.null(chk$severity) && !chk$severity %in% .valid_severities) {
            cli::cli_abort(
              c(
                "Validation {.field {val_name}} in module {.field {module_name}} has invalid check severity {.val {chk$severity}}",
                "i" = "Valid severity values: {.val {(.valid_severities)}}"
              ),
              class = c("validation_spec_invalid", "piperr")
            )
          }
        }
      }
    }
  }

  TRUE
}

#' Memoized accessor for the validation spec
#'
#' @return A parsed validation spec list.
#' @keywords internal
dlw_validation_spec <- function() {
  cached <- pd_env_get(".validation_spec_cache")
  if (is.null(cached)) {
    cached <- load_package_validation_spec()
    pd_env_set(".validation_spec_cache", cached)
  }
  cached
}

#' Reset the validation spec cache (for testing)
#' @keywords internal
dlw_validation_spec_reset <- function() {
  pd_env_rm(".validation_spec_cache")
  invisible(NULL)
}


# ── Data-Driven Validation Engine ─────────────────────────────────────────────

#' DLW Validation Engine
#'
#' A single data-driven engine that replaces the 7 per-module validation
#' functions. Reads `inst/extdata/validation_spec.yml` and dispatches
#' validation checks accordingly.
#'
#' @param dlw_data A DLW dataset (data.table).
#' @param svy_id Survey identifier string.
#' @param module Module id (one of: gpwg, group, bin, hist, all, aspire, l, skip).
#' @return A data.table with columns `table_name`, `message`, `type` (invisibly).
#'   Also appends the full validation record to `pd_env_get("validation_report")`.
#'
#' @export
dlw_validation_engine <- function(dlw_data, svy_id, module) {

  stopifnot("Data is not loaded" = !is.null(dlw_data))

  df_var_list <- colnames(dlw_data)

  spec <- dlw_validation_spec()
  mod <- spec$modules[[module]]
  if (is.null(mod)) {
    mod <- spec$modules[["skip"]]
  }

  na_threshold <- round(nrow(dlw_data) * 0.10)
  if (!is.null(mod$na_threshold_min) && na_threshold < mod$na_threshold_min) {
    na_threshold <- mod$na_threshold_min
  }

  report <- data_validation_report()

  .uniqueness_checks <- list()

  for (val_name in names(mod$validations)) {
    entry <- mod$validations[[val_name]]

    switch(entry$type,
      variable_availability = {
        validate(dlw_data, name = svy_id) |>
          is_var_startwith_avail(entry$prefix) |>
          add_results(report)
      },

      numeric_validation = , # empty cases fall through to validation_group handling
      character_validation = ,
      validation_group = {
        matched_vars <- df_var_list[grep(entry$pattern, df_var_list)]
        if (length(matched_vars) == 0) next

        for (var in matched_vars) {
          chain <- validate(dlw_data, name = svy_id)

          for (chk in entry$checks) {
            chk_name <- if (is.list(chk)) chk$name else chk
            chk_severity <- if (is.list(chk)) chk$severity else "helper"
            chk_desc <- if (is.list(chk) && !is.null(chk$description)) {
              gsub("\\{var\\}", var, chk$description)
            } else NULL

            chain <- switch(chk_name,
              is_numeric = chain |> is_numeric(var),
              is_positive = chain |> is_greaterthanzero(var),
              is_positive_or_zero = chain |> is_greaterequale0(var),
              is_character = chain |> is_character(var),
              not_missing = {
                error_fn <- if (chk_severity == "critical") error_append else warning_append
                validate_cols(
                  d = chain,
                  description = chk_desc %||% glue::glue("{var} should not be missing"),
                  skip_chain_opts = TRUE,
                  error_fun = error_fn,
                  not_na, var
                )
              },
              na_threshold = {
                error_fn <- if (chk_severity == "critical") error_append else warning_append
                validate_rows(
                  d = chain,
                  description = chk_desc %||% glue::glue("{var} NAs within %10"),
                  skip_chain_opts = TRUE,
                  error_fun = error_fn,
                  num_row_NAs, within_bounds(0, na_threshold), var
                )
              },
              chain
            )
          }

          if (!is.null(entry$labelled_clear) && entry$labelled_clear) {
            labelled::var_label(dlw_data[[var]]) <- NULL
          }

          chain |> add_results(report)
        }
      },

      single_variable = {
        var <- entry$variable
        if (!(var %in% df_var_list)) next
        chain <- validate(dlw_data, name = svy_id)
        chain <- switch(entry$check,
          is_character = chain |> is_character(var),
          is_greaterequale0 = chain |> is_greaterequale0(var),
          chain
        )
        if (!is.null(entry$secondary_check)) {
          chain <- switch(entry$secondary_check,
            is_valuebtwn0and110 = chain |> is_valuebtwn0and110(var),
            chain
          )
        }
        chain |> add_results(report)
      },

      categorical_check = {
        var <- entry$variable
        if (!(var %in% df_var_list)) next
        chain <- validate(dlw_data, name = svy_id)
        chain <- switch(entry$check,
          check_urban = chain |> check_urban(var),
          check_gender = chain |> check_gender(var),
          chain
        )
        chain |> add_results(report)
      },

      not_missing = {
        var <- entry$variable
        if (is.null(entry$condition) || entry$condition == "hhid_present") {
          if (!(var %in% df_var_list)) next
        } else if (entry$condition == "hhid_and_pid_present") {
          if (!("hhid" %in% df_var_list) || !(var %in% df_var_list)) next
        }
        error_fn <- if (entry$severity == "critical") error_append else warning_append
        desc <- if (!is.null(entry$description)) {
          gsub("\\{var\\}", var, entry$description)
        } else {
          glue::glue("{var} should not be missing")
        }
        validate(dlw_data, name = svy_id) |>
          validate_cols(
            description = desc,
            skip_chain_opts = TRUE,
            error_fun = error_fn,
            not_na, var
          ) |>
          add_results(report)
      },

      uniqueness = {
        vars <- entry$key_variables
        all_present <- all(vars %in% df_var_list)
        if (!all_present) next
        error_fn <- if (entry$severity == "critical") error_append else warning_append
        .uniqueness_checks <- c(.uniqueness_checks, list(list(
          vars = vars,
          severity = entry$severity,
          description = entry$description %||% "No duplicate records in key variables"
        )))
      },

      value_constraint = {
        var <- entry$variable
        if (!(var %in% df_var_list)) next
        inset_expr <- bquote(in_set(.(entry$valid_values)))
        vc_expr <- bquote(validate_cols(
          d = validate(dlw_data, name = svy_id),
          description = .(glue::glue("{var} should not contain out of range values")),
          skip_chain_opts = TRUE,
          error_fun = warning_append,
          .(inset_expr), .(var)
        ))
        eval(vc_expr) |>
          add_results(report)
      },

      data_presence = {
        error_fn <- if (entry$severity == "critical") error_append else warning_append
        validate(dlw_data, name = svy_id) |>
          verify(
            nrow(dlw_data) > 0,
            description = entry$description %||% "Data should not blank",
            error_fun = error_fn
          ) |>
          add_results(report)
      }
    )
  }

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  for (uc in .uniqueness_checks) {
    has_dup <- any(duplicated(dlw_data[, uc$vars, with = FALSE]))
    dup_msg <- uc$description
    dup_type <- if (has_dup) {
      if (uc$severity == "critical") "error" else "warning"
    } else {
      "success"
    }
    dup_display <- if (has_dup) {
      paste0("verification [is_uniq(", paste(uc$vars, collapse = ", "), ")] failed! (1 failure)")
    } else {
      paste0("verification [is_uniq(", paste(uc$vars, collapse = ", "), ")] passed!")
    }
    dup_row <- data.table::data.table(
      table_name = svy_id,
      assertion.id = NA_character_,
      description = dup_msg,
      num.violations = if (has_dup) 1L else NA_integer_,
      call = paste0("is_uniq(", paste(uc$vars, collapse = ", "), ")"),
      message = dup_display,
      type = dup_type,
      error_df = list(NULL)
    )
    validation_record <- data.table::rbindlist(list(validation_record, dup_row), fill = TRUE, ignore.attr = TRUE)
  }

  err_t <- validation_record[, .(table_name, message, type)]

  pd_env_append("validation_report", validation_record)

  return(invisible(err_t))
}


#' Validate DLW data (Generic Documentation)
#'
#' This is a generic validation interface for DLW datasets across different module types.
#' Specific functions handle validation logic for GPWG, GROUP, BIN, HIST, ALL, ASPIRE, and L module types.
#'
#' @param dlw_data A DLW dataset in `qs` format.
#' @param svy_id A survey identifier extracted from the dataset.
#'
#' @return A data.frame containing validation results.
#'
#' @keywords internal
#' @export
dlw_validation <- function(dlw_data, svy_id) {
  stop("This is a documentation anchor. Use a method like dlw_validation_gpwg(), dlw_validation_group(), dlw_validation_bin(), dlw_validation_hist(), dlw_validation_all(), dlw_validation_aspire(), or dlw_validation_l().")
}

#' @describeIn dlw_validation Validate GPWG data
#'
#' Performs variable and structural checks on GPWG data, such as availability of core variables,
#' non-missingness, valid value ranges, and duplication checks.
#'
#' @import data.validator assertr
#' @details
#' Deprecated: use `dlw_validation_engine()` with `module = "gpwg"` instead.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_gpwg(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_gpwg <- function(dlw_data, svy_id) {
  # Deprecated wrapper: the data-driven engine is the canonical implementation.
  dlw_validation_engine(dlw_data, svy_id, "gpwg")
}


#' @describeIn dlw_validation Validate GROUP data
#'
#' Checks for missing values, type mismatches, and invalid entries in GROUP datasets.
#'
#' @import data.validator assertr
#' @details
#' Deprecated: use `dlw_validation_engine()` with `module = "group"` instead.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_group(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_group <- function(dlw_data, svy_id) {
  # Deprecated wrapper: the data-driven engine is the canonical implementation.
  dlw_validation_engine(dlw_data, svy_id, "group")
}


#' @describeIn dlw_validation Validate BIN data
#'
#' Performs structural and value-based validation for BIN datasets,
#' checking numeric, character, and key variable consistency.
#'
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq has_all_names has_only_names verify warning_append within_bounds
#' @details
#' Deprecated: use `dlw_validation_engine()` with `module = "bin"` instead.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_bin(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_bin <- function(dlw_data, svy_id) {
  # Deprecated wrapper: the data-driven engine is the canonical implementation.
  dlw_validation_engine(dlw_data, svy_id, "bin")
}


#' @describeIn dlw_validation Validate HIST data
#'
#' Conducts data validation for HIST datasets, including checks for key variables like
#' `urban`, `weight`, and `welfare`, as well as common structural validations.
#'
#' @import data.validator assertr
#' @details
#' Deprecated: use `dlw_validation_engine()` with `module = "hist"` instead.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_hist(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_hist <- function(dlw_data, svy_id) {
  # Deprecated wrapper: the data-driven engine is the canonical implementation.
  dlw_validation_engine(dlw_data, svy_id, "hist")
}

#' @describeIn dlw_validation Validate ALL data
#'
#' Validates general ALL module type data containing core variables such as `welfare`, `weight`, and optionally `urban`.
#' Ensures basic structure and NA thresholds.
#'
#' @import data.validator assertr
#' @details
#' Deprecated: use `dlw_validation_engine()` with `module = "all"` instead.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_all(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_all <- function(dlw_data, svy_id) {
  # Deprecated wrapper: the data-driven engine is the canonical implementation.
  dlw_validation_engine(dlw_data, svy_id, "all")
}


#' @describeIn dlw_validation Validate ASPIRE data
#'
#' Handles validation for ASPIRE DLW datasets by checking structure and numeric variable consistency.
#' Special attention is paid to `hhweight`, `urban`, and household size.
#'
#' @import data.validator assertr
#' @details
#' Deprecated: use `dlw_validation_engine()` with `module = "aspire"` instead.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_aspire(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_aspire <- function(dlw_data, svy_id) {
  # Deprecated wrapper: the data-driven engine is the canonical implementation.
  dlw_validation_engine(dlw_data, svy_id, "aspire")
}

#' @describeIn dlw_validation Validate Labor (L) DLW data
#'
#' Validates DLW datasets containing labor-specific data, such as employment status (`lstatus`, `empstat`),
#' person-level identifiers (`hhid`, `pid`), and working hours (`whours`).
#'
#' @import data.validator assertr
#' @details
#' Deprecated: use `dlw_validation_engine()` with `module = "?"` instead.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_l(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = "survey_id",
#' )
#' }
dlw_validation_l <- function(dlw_data, svy_id) {
  # Deprecated wrapper: the data-driven engine is the canonical implementation.
  dlw_validation_engine(dlw_data, svy_id, "l")
}

#' @describeIn dlw_validation Skip Validation
#'
#' Used for DLW modules that require no validation. Ensures only that the dataset is not blank.
#'
#' @details
#' Deprecated: use `dlw_validation_engine()` with `module = "skip"` instead.
#' @keywords internal
#' @return An empty data.frame with minimal checks applied.
#' @export
#'
#' @examples
#' \dontrun{
#' dlw_validation_skip(
#'   dlw_data = "data/dlw_qs",
#'   svy_id = survey_id
#' )
#' }
dlw_validation_skip <- function(dlw_data, svy_id) {
  # Deprecated wrapper: the data-driven engine is the canonical implementation.
  dlw_validation_engine(dlw_data, svy_id, "skip")
}


#' Validating Specific Conditions of a Variable (Generic Documentation)
#'
#' This interface serves as a generic check for variables in DLW datasets across various scenarios.
#' It includes specific functions designed to assess different conditions, such as determining if a variable is of character or numeric type,
#' checking the number of reporting levels for urban/rural variables, verifying if values are greater than zero,
#' and confirming the availability of a variable within the dataset.
#'
#'
#' @param val variable name
#' @param col_name data
#'
#' @returns a validation report as text
#' @export
#'
#' @keywords internal
#' @export
dlw_var_check <- function(val, col_name) {
  stop(
    "This is a documentation anchor. Use a method like is_character(), is_numeric(), 
    check_urban(), check_gender(), is_greaterthanzero(), is_var_avail(), is_var_startwith_avail(), 
    is_valuebtwn0and120() or is_greaterequale0."
  )
}

#' @describeIn dlw_var_check Check a variable is character
#'
#' @examples
#' \dontrun{
#' is_character(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_character <-  function(val, col_name){

  expr = bquote(is.character(.(val)[[.(col_name)]]))
  validate_if(val,
              eval(expr),
              description = glue::glue("{col_name} is character"),
              skip_chain_opts = TRUE,
              error_fun = warning_append)
}

#' @describeIn dlw_var_check Check a variable is numeric
#'
#' @examples
#' \dontrun{
#' is_numeric(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_numeric <- function(val, col_name){

  expr = bquote(is.numeric(.(val)[[.(col_name)]]))
  validate_if(val,
              eval(expr),
              description = glue::glue("{col_name} is numeric"),
              skip_chain_opts = TRUE,
              error_fun = warning_append)
}


#' @describeIn dlw_var_check Check residential variable (urban/rural) has more than one reporting level in group data
#'
#' @examples
#' \dontrun{
#' check_urban(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
check_urban <- function(val, col_name){

  # extract unique URBAN values
  urban_info <- unique(val[[col_name]])

  # Logical vector
  expr = bquote(urban_info == 1 | is.na(urban_info))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("Urban - more than one reporting level"),
    skip_chain_opts = TRUE,
    error_fun = warning_append
  )
}


#' @describeIn dlw_var_check Check gender (male - variable) has more than two categories in ALL data
#'
#' @examples
#' \dontrun{
#' check_gender(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
check_gender <- function(val, col_name) {
  # extract unique gender values
  gender_info <- unique(val[[col_name]])

  # Logical vector
  expr = bquote(gender_info == 2 | is.na(gender_info))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("Gender values are more than two categories"),
    skip_chain_opts = TRUE,
    error_fun = warning_append
  )
}

#' @describeIn dlw_var_check Check a numeric variable is greater than 0
#'
#' @examples
#' \dontrun{
#' is_greaterthanzero(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_greaterthanzero <- function(val, col_name){

  # Logical vector
  expr = bquote(any(val[[col_name]] > 0) |
                  any(is.na(.(val)[[.(col_name)]])))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} > 0"),
    skip_chain_opts = TRUE,
    error_fun = warning_append
  )
}

#' @describeIn dlw_var_check Check a numeric variable is greater than or equal to 0
#'
#' @examples
#' \dontrun{
#' is_greaterequale0(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_greaterequale0 <- function(val, col_name) {
  # Logical vector
  expr = bquote(
    any(val[[col_name]] >= 0) |
      any(is.na(.(val)[[.(col_name)]]))
  )

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} >= 0"),
    skip_chain_opts = TRUE,
    error_fun = error_append
  )
}

#' @describeIn dlw_var_check Check a variable is available in a dataset with specified variable name
#'
#' @examples
#' \dontrun{
#' is_var_avail(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_var_avail <- function(val, col_name){

  # Logical vector
  expr = bquote(col_name %in% names(val))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} variable should be in the data"),
    skip_chain_opts = TRUE,
    error_fun = error_append
  )
}

#' @describeIn dlw_var_check Check a variable is available in a dataset with variable name starting with a specified text
#'
#' @examples
#' \dontrun{
#' is_var_startwith_avail(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_var_startwith_avail <- function(val, col_name){

  # Logical vector
  expr = bquote(any(startsWith(names(val), col_name)))

  # Validate
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} variable should be in the data"),
    skip_chain_opts = TRUE,
    error_fun = error_append
  )
}

#' @describeIn dlw_var_check Check age is available in a dataset with value between 0 and 110
#'
#' @examples
#' \dontrun{
#' is_valuebtwn0and110(
#'   val = data,
#'   col_name = variable_name,
#' )
#' }
is_valuebtwn0and110 <- function(val, col_name) { 
  
  expr <- bquote(
    all((.(val)[[.(col_name)]] >= 0 & .(val)[[.(col_name)]] <= 110) |
          is.na(.(val)[[.(col_name)]]))
  )
  
  validate_if(
    val,
    eval(expr),
    description = glue::glue("{col_name} btwn 0 and 110 or NA"),
    skip_chain_opts = TRUE,
    error_fun = warning_append
  )
}

