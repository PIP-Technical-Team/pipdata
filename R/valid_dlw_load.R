#' Determine which DLW surveys need processing
#'
#' Compares the current DLW inventory against previously cleaned data
#' and auxiliary-file changes to identify surveys that require
#' (re-)processing. Returns the filtered inventory of surveys to clean.
#'
#' The function:
#' 1. Detects changes in auxiliary files (PFW, CPI, PPP, etc.) and
#'    identifies affected surveys.
#' 2. Filters the inventory to requested modules.
#' 3. Selects the latest version of each survey via `last_ver_inv()`.
#' 4. Unless `force = TRUE`, removes surveys already cleaned in the
#'    master inventory via `inv_to_process()`.
#' 5. Combines DLW-new and aux-changed surveys into a single inventory.
#'
#' @param inv A `data.table` of the full DLW inventory.
#' @param aux_measures Character vector of auxiliary measures to check
#'   for changes. Default: `c("pfw", "cpi", "ppp", "pop", "gdp", "pce")`.
#' @param modules Character vector of survey modules to include.
#'   Default: `c("ALL", "GROUP", "HIST", "GPWG", "BIN")`.
#' @param force Logical. If `TRUE`, skip the comparison against the
#'   master inventory and process all surveys.
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#'
#' @return A `data.table` of surveys to process, or `NULL` if none.
#'
#' @details
#' **Logging**: This function writes an `aux_changes_inf` entry to the `"pipdata_log"`
#' when changes are detected in any of the requested auxiliary measures. The logmeta
#' entry includes the measures that changed and the number of affected surveys.
#'
#' @family pd_process_data pipeline
#' @export
valid_dlw_load <- function(
  inv,
  aux_measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  modules = c("ALL", "GROUP", "HIST", "GPWG", "BIN"),
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.data.table(inv)) {
    inv <- data.table::as.data.table(inv)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Load changes in aux files
  all_changes_aux <- valid_aux_load(measure = aux_measures, compare = "all", verbose = verbose)
  ls_inv_aux <- lapply(all_changes_aux, filter_aux_inv, inv = inv)

  # Join release and vintage changes and select unique surveys
  if (
    is.null(ls_inv_aux) ||
      length(ls_inv_aux) == 0 ||
      all(sapply(ls_inv_aux, is.null))
  ) {
    cli::cli_alert_info("No changes in auxiliary files.")
    inv_aux <- NULL
  } else {
    inv_aux <- ls_inv_aux |>
      data.table::rbindlist() |>
      collapse::funique()
  }

  # Log aux changes if any were detected
  if (!is.null(all_changes_aux)) {
    changed_measures <- unique(unlist(lapply(all_changes_aux, names)))
    n_affected <- if (is.null(inv_aux)) 0L else nrow(inv_aux)
    survey_ids_aux <- if (is.null(inv_aux)) character(0) else inv_aux$survey_id
    pipfun::log_info(
      "Auxiliary file changes detected.",
      name = "pipdata_log",
      logmeta = list(
        info = "aux_changes_inf",
        measures = changed_measures,
        n_surveys_affected = n_affected,
        surveys_affected = survey_ids_aux
      )
    )
  }

  # Alert when surveys need re-cleaning due to aux changes.
  # Console message is gated by verbose; the log entry above always fires.
  if (!is.null(inv_aux) && nrow(inv_aux) > 0L) {
    if (verbose) {
      cli::cli_alert_warning(
        "{nrow(inv_aux)} survey{?s} will be re-cleaned because auxiliary data changed."
      )
    }
  }

  # Filter inventory for specific modules and select last version of each survey (and random sample if needed)
  # inv <- m_inv_valid(inv, seed = seed) # Mock function to select 20 random surveys from valid inventory
  inv <- inv[module %in% modules]
  inv_svy <- last_ver_inv(inv)

  # Select valid surveys and compare to previous cleaning
  if (!force) {
    inv_svy <- inv_to_process(inv_svy, verbose = verbose)
  } else {
    # Explicitly pass verbose even when force=TRUE (for consistency in call trace)
  }

  if (
    (is.null(inv_svy) || nrow(inv_svy) == 0) &&
      (is.null(inv_aux) || nrow(inv_aux) == 0)
  ) {
    return(NULL)
  }

  # Bind with inventory from aux changes
  inv_to_clean <- rbind(inv_svy, inv_aux, fill = TRUE)

  # Choose only unique
  inv_to_clean <- unique(inv_to_clean)

  # Order alphabetically
  setorder(inv_to_clean, survey_id)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_to_clean)
}


#' Filter DLW inventory by auxiliary-data changes
#'
#' For each auxiliary dataset that has changed, normalises the year
#' variable and merges the changes against the DLW inventory to
#' identify surveys affected by those changes.
#'
#' @param inv A `data.table` of the DLW inventory.
#' @param changes_aux A list of `data.table` objects representing changed
#'   rows in an auxiliary dataset, as returned by [valid_aux_load()].
#'
#' @return A `data.table` of affected surveys (latest version only),
#'   or `NULL` if no changes apply.
#'
#' @family pd_process_data pipeline
#' @keywords internal
filter_aux_inv <- function(inv, changes_aux) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Defense
  if (is.null(changes_aux) || length(changes_aux) == 0) {
    return(NULL)
  }

  # Fix year variable

  changes <- lapply(changes_aux, fix_year_var)

  # Row bind and select unique values from all aux files

  changes <- unique(rbindlist(changes, fill = TRUE))

  # Temporary fix to test data from Rossana

  max_year <- max(inv[!is.na(inv$surveyid_year), ]$surveyid_year)

  changes <- changes[changes$surveyid_year <= max_year, ]

  # Merge inventory with aux changes

  inv_aux <- joyn::inner_join(
    inv,
    changes,
    relationship = "many-to-one",
    verbose = FALSE,
    by = c("country_code", "surveyid_year"),
    reportvar = FALSE
  )

  # Return if empty
  if (nrow(inv_aux) == 0) {
    return(NULL)
  }

  # Choose last version if not empty
  inv_aux <- last_ver_inv(inv_aux)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_aux)
}

#' Normalise the year variable name in an auxiliary change table
#'
#' Finds the column whose name contains `"year"`, renames it to
#' `"surveyid_year"` if needed, and returns a unique two-column
#' `data.table` of `country_code` and `surveyid_year`.
#'
#' @param dt A `data.table` from an auxiliary-change comparison.
#'
#' @return A `data.table` with columns `country_code` and
#'   `surveyid_year`.
#'
#' @family pd_process_data pipeline
#' @keywords internal
fix_year_var <- function(dt) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Select variable names that contain the word "year"

  year_var <- grep("year", attributes(dt)$names, value = TRUE)

  if (length(year_var) > 1) {
    if (any(year_var %in% c("year"))) {
      year_var <- "year"
    } else if (any(year_var %in% c("surveyid_year"))) {
      year_var <- "surveyid_year"
    } else {
      cli::cli_abort(
        "The auxiliary keys has more than one variable related to `year` and none are `surveyid_year`"
      )
    }
  }

  # Subset the data.table with the selected variables and make them unique

  selected_vars <- c("country_code", year_var)

  dt_selected <- unique(dt[, ..selected_vars])

  # Change name of year variable to match

  if (year_var != "surveyid_year") {
    names(dt_selected)[names(dt_selected) == year_var] <- "surveyid_year"
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt_selected)
}

#' Remove surveys already cleaned from the processing inventory
#'
#' Anti-joins the current DLW inventory against the PIP master inventory
#' to keep only surveys that have not yet been cleaned. If the master
#' inventory cannot be loaded, all surveys are returned.
#'
#' @param inv A `data.table` of DLW surveys (latest versions).
#' @param verbose Logical. Print progress messages.
#'
#' @return A `data.table` of surveys still needing processing, or
#'   `NULL` if all surveys have already been cleaned.
#'
#' @family pd_process_data pipeline
#' @keywords internal
inv_to_process <- function(
  inv,
  verbose = TRUE
) {
  # Select valid surveys and compare to previous cleaning
  inv_svy <- tryCatch(
    expr = {
      # Load master inventory to compare with previous cleaning
      dt_master <- pipload::load_pip_master_inventory(verbose = verbose)

      # Remove _dlw suffix from master inventory to be able to compare with current inventory
      dlw_cols <- grep("_dlw$", names(dt_master), value = TRUE)
      if (length(dlw_cols) > 0) {
        new_names <- sub("_dlw$", "", dlw_cols)
        data.table::setnames(dt_master, dlw_cols, new_names)
      }

      # keep only surveys not cleaned in previous version
      key_inventory <- c("country_code", "surveyid_year", "survey_acronym") # Temporary fix until we have create keys in the inventory

      # if (!all(key_inventory %in% names(inv))) {
      #   cli::cli_abort(
      #     "The inventory should contain the following variables: country_code, surveyid_year and survey_acronym"
      #   )
      # }

      inv_svy <- inv |>
        joyn::anti_join(
          dt_master,
          by = key_inventory,
          verbose = FALSE,
          reportvar = FALSE
        )

      inv_svy
    },
    error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(
          "Could not load PIP master inventory. Returning all valid surveys without comparing to previous cleaning."
        )
      }
      return(inv)
    }
  )

  if (inv_svy[, .N] == 0) {
    if (verbose) {
      cli::cli_alert_warning(
        "All surveys in the inventory have been cleaned in previous versions. No surveys to process."
      )
    }
    return(NULL)
  }

  return(inv_svy)
}
