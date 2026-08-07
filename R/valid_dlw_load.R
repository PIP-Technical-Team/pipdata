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
#' @param aux_hashes A named character vector of current aux `content_hash`
#'   values, one per requested auxiliary measure. Resolved once per run by
#'   [get_aux_hashes()] and used to gate aux-change detection. Default `NULL`
#'   (no aux hashes available — aux-change detection is skipped).
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#'
#' @return A `data.table` of surveys to process, or `NULL` if none.
#'
#' @details
#' **Aux-change gating (two-stage)**: aux-change detection is gated on the
#' current aux `content_hash` values passed via `aux_hashes`.
#' - Stage 1 (cheap): for each previously-cleaned survey, compare its stored
#'   per-survey aux hash (from the master inventory's `aux_<measure>_hash`
#'   columns) against the current hash for that measure. A mismatch or a
#'   missing historical hash makes the survey a candidate. New surveys and
#'   DLW-content-changed surveys are always processed via [inv_to_process()].
#' - Stage 2 (detailed): for the changed measures only, [valid_aux_load()] /
#'   `compare_aux_*` identifies which requested surveys actually have changed
#'   rows inside the aux file. The affected surveys are intersected with the
#'   candidate set, so a globally changed aux table that only affects
#'   non-requested countries does not re-clean requested surveys.
#'
#' The master inventory is loaded at most once and shared between the DLW
#' comparison and the aux-hash comparison. When `force = TRUE`, no master or
#' aux comparison runs and all filtered/latest surveys are processed.
#'
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
  aux_hashes = NULL,
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

  # Filter inventory for specific modules and select last version of each
  # survey before any comparison.
  inv <- inv[module %in% modules]
  inv_svy_full <- last_ver_inv(inv)

  # Load the master inventory once and share it between the DLW comparison
  # and the aux-hash comparison. When force = TRUE, no master is loaded and
  # all filtered/latest surveys are processed.
  dt_master <- NULL
  if (!force) {
    dt_master <- tryCatch(
      pipload::load_pip_master_inventory(verbose = verbose),
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(
            "Could not load PIP master inventory. Processing all surveys."
          )
        }
        NULL
      }
    )
  }

  # Select valid surveys and compare to previous cleaning (DLW content hash).
  # inv_svy holds the DLW-new / DLW-content-changed surveys (may be NULL).
  inv_svy <- inv_svy_full
  if (!force) {
    inv_svy <- inv_to_process(inv_svy_full, dt_master = dt_master, verbose = verbose)
  }

  # Stage 1: build the aux candidate set from the per-survey aux hash
  # comparison over the full filtered/latest inventory (all previously-cleaned
  # surveys). Only runs when aux_hashes are supplied and the master is
  # available (i.e. not force mode).
  inv_aux <- NULL
  changed_measures <- character(0)

  if (!force && !is.null(aux_hashes) && length(aux_hashes) > 0L && !is.null(dt_master) && nrow(inv_svy_full) > 0L) {
    candidates <- aux_hash_candidates(
      inv = inv_svy_full,
      dt_master = dt_master,
      aux_hashes = aux_hashes,
      verbose = verbose
    )

    if (is.null(candidates) || nrow(candidates) == 0L) {
      # No requested measure's aux hash changed for any previously-cleaned
      # survey.
      pipfun::log_info(
        "No auxiliary file changes detected for survey cleaning.",
        name = "pipdata_log",
        logmeta = list(info = "aux_no_changes_inf")
      )
    } else {
      # Stage 2: for the changed measures only, run valid_aux_load() and
      # intersect the affected surveys with the candidate set.
      changed_measures <- attr(candidates, "changed_measures")
      all_changes_aux <- valid_aux_load(
        measure = changed_measures,
        compare = "all",
        verbose = verbose
      )
      ls_inv_aux <- lapply(all_changes_aux, filter_aux_inv, inv = inv)

      if (all(vapply(ls_inv_aux, is.null, logical(1)))) {
        # Measures changed but no requested survey is actually affected.
        pipfun::log_info(
          "Auxiliary files changed but no surveys affected.",
          name = "pipdata_log",
          logmeta = list(
            info = "aux_changes_no_surveys_inf",
            measures = changed_measures
          )
        )
      } else {
        affected <- ls_inv_aux |>
          data.table::rbindlist() |>
          collapse::funique()

        # Intersect affected surveys with the candidate set.
        inv_aux <- affected[survey_id %in% candidates$survey_id]

        if (nrow(inv_aux) > 0L) {
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
          if (verbose) {
            cli::cli_alert_warning(
              "{nrow(inv_aux)} survey{?s} will be re-cleaned because auxiliary data changed."
            )
          }
        } else {
          pipfun::log_info(
            "Auxiliary files changed but no surveys affected.",
            name = "pipdata_log",
            logmeta = list(
              info = "aux_changes_no_surveys_inf",
              measures = changed_measures
            )
          )
        }
      }
    }
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
      aux_measures_triggered = changed_measures
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
#' @param dt_master A `data.table` of the PIP master inventory, already loaded
#'   by the caller ([valid_dlw_load()]) and shared with the aux-hash
#'   comparison. Default `NULL`, in which case the master is loaded here.
#' @param verbose Logical. Print progress messages.
#'
#' @return A `data.table` of surveys still needing processing, or
#'   `NULL` if all surveys have already been cleaned.
#'
#' @family pd_process_data pipeline
#' @keywords internal
inv_to_process <- function(
  inv,
  dt_master = NULL,
  verbose = TRUE
) {
  # Load master inventory to compare with previous cleaning, unless the
  # caller already loaded it (shared single-load handoff).
  if (is.null(dt_master)) {
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
  }

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

#' Identify surveys whose stored aux hash differs from the current aux hash
#'
#' Stage 1 of the two-stage aux-change gate. For each previously-cleaned
#' survey (present in the master inventory), compares its stored per-survey
#' aux hash (from the master's `aux_<measure>_hash` columns) against the
#' current aux `content_hash` for each requested measure. A survey is a
#' candidate when any requested measure's stored hash differs from the
#' current hash, or when the stored hash is missing (survey cleaned before
#' this feature — treated as changed).
#'
#' @param inv A `data.table` of DLW surveys (latest versions).
#' @param dt_master A `data.table` of the PIP master inventory.
#' @param aux_hashes A named character vector of current aux `content_hash`
#'   values, one per requested measure.
#' @param verbose Logical. Print progress messages.
#'
#' @return A `data.table` of candidate surveys (subset of `inv`), with an
#'   attribute `changed_measures` holding the measures whose hash changed.
#'   Returns `NULL` when no survey is a candidate.
#'
#' @details
#' The master inventory is reduced to one row per `survey_id` for the same
#' `content_hash_dlw`. All rows in that group must have identical aux hashes;
#' a conflict aborts loudly (this protects the invariant that split `pip_id`s
#' for one survey/content version use the same aux versions).
#'
#' @family pd_process_data pipeline
#' @keywords internal
aux_hash_candidates <- function(
  inv,
  dt_master,
  aux_hashes,
  verbose = TRUE
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Reduce master to one row per survey_id for the same content_hash_dlw,
  # carrying the aux hash columns. Abort on conflicting aux hashes within a
  # survey/content-hash group.
  aux_cols <- paste0("aux_", names(aux_hashes), "_hash")
  aux_cols <- intersect(aux_cols, names(dt_master))

  if (length(aux_cols) == 0L) {
    # No stored aux hashes at all — every previously-cleaned survey is a
    # candidate (migration case).
    candidates <- inv
    attr(candidates, "changed_measures") <- names(aux_hashes)
    return(candidates)
  }

  # Build a survey-level master keyed by survey_id + content_hash_dlw.
  key_cols <- c("survey_id", "content_hash_dlw")
  master_svy <- collapse::funique(dt_master[, c(key_cols, aux_cols), with = FALSE])

  # Abort on conflicting aux hashes within a survey/content-hash group.
  n_groups <- nrow(collapse::funique(master_svy[, key_cols, with = FALSE]))
  if (n_groups != nrow(master_svy)) {
    cli::cli_abort(
      "Conflicting aux hashes found for the same survey_id and content_hash_dlw.",
      class = c("aux_hash_candidates_conflict", "piperr")
    )
  }

  # Join the survey-level master hashes onto the DLW inventory.
  inv_compare <- joyn::left_join(
    inv,
    master_svy,
    by = "survey_id",
    relationship = "many-to-one",
    verbose = FALSE,
    reportvar = FALSE
  )

  # Determine which measures changed for each survey.
  changed_measures <- character(0)
  candidate_idx <- rep(FALSE, nrow(inv_compare))

  for (m in names(aux_hashes)) {
    col <- paste0("aux_", m, "_hash")
    if (!col %in% names(inv_compare)) {
      # Measure not stored for this survey — treat as changed.
      candidate_idx <- candidate_idx | TRUE
      changed_measures <- unique(c(changed_measures, m))
      next
    }
    stored <- inv_compare[[col]]
    current <- aux_hashes[[m]]
    is_changed <- is.na(stored) | stored != current
    candidate_idx <- candidate_idx | is_changed
    if (any(is_changed, na.rm = TRUE)) {
      changed_measures <- unique(c(changed_measures, m))
    }
  }

  candidates <- inv_compare[candidate_idx]
  # Drop the joined aux hash columns from the result.
  candidates[, (aux_cols) := NULL]

  if (nrow(candidates) == 0L) {
    return(NULL)
  }

  attr(candidates, "changed_measures") <- changed_measures

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(candidates)
}
