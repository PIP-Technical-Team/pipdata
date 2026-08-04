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
#' **Logging**: This function writes the following entries to the `"pipdata_log"`:
#' - `aux_changes_inf` — changes were detected in any of the requested auxiliary
#'   measures and at least one survey is affected. Includes the measures that
#'   changed and the number/list of affected surveys.
#' - `aux_no_changes_inf` — no auxiliary file changes were detected at all.
#' - `aux_changes_no_surveys_inf` — auxiliary files changed but no surveys in
#'   the inventory were affected by those changes.
#' - `surveys_to_clean_inf` — emitted once after the DLW-new and aux-changed
#'   inventories are combined and deduplicated; includes counts of new,
#'   aux-changed, and total unique surveys, plus the aux measures that
#'   triggered re-cleaning.
#'
#' When neither new DLW surveys nor auxiliary changes leave anything to
#' process, the function aborts with `cli::cli_abort(class = "piperr")` rather
#' than returning `NULL` silently.
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
  if (is.null(all_changes_aux) || length(all_changes_aux) == 0) {
    # Check 1: were any aux changes detected at all?
    pipfun::log_info(
      "No auxiliary file changes detected for survey cleaning.",
      name = "pipdata_log",
      logmeta = list(info = "aux_no_changes_inf")
    )
    inv_aux <- NULL
  } else if (all(vapply(ls_inv_aux, is.null, logical(1)))) {
    # Check 2: aux changed but no surveys match
    pipfun::log_info(
      "Auxiliary files changed but no surveys affected.",
      name = "pipdata_log",
      logmeta = list(
        info = "aux_changes_no_surveys_inf",
        measures = unique(unlist(lapply(all_changes_aux, names)))
      )
    )
    inv_aux <- NULL
  } else {
    inv_aux <- ls_inv_aux |>
      data.table::rbindlist() |>
      collapse::funique()

    # aux_changes_inf fires here, inside the branch where inv_aux is
    # actually non-empty -- not on the mere non-NULL-ness of all_changes_aux.
    changed_measures <- unique(unlist(lapply(all_changes_aux, names)))
    pipfun::log_info(
      "Auxiliary file changes detected.",
      name = "pipdata_log",
      logmeta = list(
        info = "aux_changes_inf",
        measures = changed_measures,
        n_surveys_affected = nrow(inv_aux),
        surveys_affected = inv_aux$survey_id
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
    cli::cli_abort(
      "No surveys to process: all surveys are up to date and no auxiliary changes affect any survey.",
      class = "piperr"
    )
  }

  # Bind with inventory from aux changes
  inv_to_clean <- rbind(inv_svy, inv_aux, fill = TRUE)

  # Choose only unique
  inv_to_clean <- unique(inv_to_clean)

  # Log summary of surveys identified for cleaning
  pipfun::log_info(
    "Surveys identified for cleaning.",
    name = "pipdata_log",
    logmeta = list(
      info = "surveys_to_clean_inf",
      n_dlw_new      = if (is.null(inv_svy)) 0L else nrow(inv_svy),
      n_aux_changed  = if (is.null(inv_aux)) 0L else nrow(inv_aux),
      n_total_unique = nrow(inv_to_clean),
      aux_measures_triggered = if (is.null(all_changes_aux)) character(0)
                               else unique(unlist(lapply(all_changes_aux, names)))
    )
  )

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
#' Compares the current DLW inventory against the PIP master inventory by
#' joining on `survey_id` and comparing `content_hash` (DLW) against
#' `content_hash_dlw` (master). Surveys are kept when they are new to the
#' master (no `content_hash_dlw`) or when their DLW content hash differs
#' from the previously cleaned value. If the master inventory cannot be
#' loaded, all surveys are returned.
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
  # Load master inventory to compare with previous cleaning
  dt_master <- tryCatch(
    pipload::load_pip_master_inventory(verbose = verbose),
    error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(
          "Could not load PIP master inventory. Processing all surveys."
        )
      }
      return(NULL)
    }
  )

  if (is.null(dt_master)) return(inv)

  # Deduplicate by survey_id: content_hash_dlw is expected to be identical
  # across pip_id splits of the same survey_id, but the join must not rely
  # on that being true for row cardinality -- dedup first, and let
  # relationship = "many-to-one" raise if it is ever violated.
  dt_master_hash <- collapse::funique(dt_master[, .(survey_id, content_hash_dlw)])

  # Join on survey_id to compare content hashes
  inv_compare <- joyn::left_join(
    inv,
    dt_master_hash,
    by = "survey_id",
    relationship = "many-to-one",
    verbose = FALSE,
    reportvar = FALSE
  )

  # Keep: new surveys (NA hash in master) or surveys whose DLW content changed
  inv_changed <- inv_compare[
    is.na(content_hash_dlw) | content_hash != content_hash_dlw
  ]
  inv_changed[, content_hash_dlw := NULL]

  if (nrow(inv_changed) == 0) {
    if (verbose) {
      cli::cli_alert_warning(
        "All surveys in the inventory have been cleaned in previous versions. No surveys to process."
      )
    }
    return(NULL)
  }

  return(inv_changed)
}
