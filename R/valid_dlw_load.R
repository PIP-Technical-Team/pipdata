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
#'   [get_aux_hashes()] and used to gate aux-change detection. When `NULL`
#'   (the default) and `force = FALSE`, the hashes are resolved internally so
#'   that direct callers retain the previous behavior of always running
#'   aux-change detection.
#' @param force_surveys Character vector of `survey_id` and/or `pip_id`
#'   values to re-process surgically, alongside the normal invalidation
#'   candidates. Forced surveys bypass [inv_to_process()] only and are unioned
#'   into the candidate set, deduplicated via `unique()`. Mutually exclusive
#'   with `force = TRUE`. Preserves content-based stamp versioning. Unknown
#'   identifiers are warned about and skipped. Default `NULL`.
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#'
#' @return A `data.table` of surveys to process. If no surveys require
#'   processing, the function aborts with class `piperr`.
#'
#' @details
#' **Force-survey path (`force_surveys`)**: forced surveys are resolved via
#' [resolve_force_surveys()] (lookup-first: `survey_id` membership, then
#' `pip_id` reverse-map through the already-loaded master inventory) and
#' unioned into the candidate set. They bypass [inv_to_process()] only;
#' aux-change detection runs normally and overlaps are deduplicated via
#' `unique()`. Emits `force_surveys_inf` / `force_surveys_unknown_inf`
#' log entries.
#'
#' **Aux-change gating (two-stage)**: aux-change detection is gated on the
#' current aux `content_hash` values passed via `aux_hashes`.
#' - Stage 1 (cheap): for each filtered/latest survey, compare its stored
#'   per-survey aux hash (from the master inventory's `aux_<measure>_hash`
#'   columns) against the current hash for that measure. A mismatch or a
#'   missing historical hash makes the survey a candidate. New surveys and
#'   DLW-content-changed surveys are also retained through
#'   [inv_to_process()] and are deduplicated with the aux candidates.
#' - Stage 2 (detailed): for the changed measures only, [valid_aux_load()] /
#'   `compare_aux_*` identifies which requested surveys actually have changed
#'   rows inside the aux file. The affected surveys are intersected with the
#'   candidate set, so a globally changed aux table that only affects
#'   non-requested countries does not re-clean requested surveys.
#'
#' The master inventory is loaded at most once within this function and shared
#' between the DLW comparison and the aux-hash comparison. This guarantee is
#' scoped to `valid_dlw_load()`; downstream steps such as
#' [build_pip_inventory()] load the master again for their own assembly and
#' verification. When `force = TRUE`, no master or aux comparison runs and all
#' filtered/latest surveys are processed.
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
  verbose = getOption("pipdata.verbose", default = TRUE),
  aux_hashes = NULL,
  force_surveys = NULL
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.data.table(inv)) {
    inv <- data.table::as.data.table(inv)
  }

  # force and force_surveys are mutually exclusive. valid_dlw_load() is
  # exported and can be called directly, so it carries its own guard before
  # any inventory/stamp work.
  if (force && !is.null(force_surveys)) {
    cli::cli_abort(
      .force_exclusive_msg,
      class = "piperr"
    )
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
  #
  # master_available distinguishes "master was not supplied" (NULL) from
  # "master was supplied but could not be loaded" (FALSE). This prevents
  # inv_to_process() from re-loading the master when the load already failed.
  dt_master <- NULL
  master_available <- FALSE
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
    master_available <- !is.null(dt_master)
  }

  # Select valid surveys and compare to previous cleaning (DLW content hash).
  # inv_svy holds the DLW-new / DLW-content-changed surveys (may be NULL).
  inv_svy <- inv_svy_full
  if (!force) {
    inv_svy <- inv_to_process(
      inv_svy_full,
      dt_master = dt_master,
      master_available = master_available,
      verbose = verbose
    )
  }

  # Resolve current aux hashes when the caller did not supply them, so that
  # direct callers of valid_dlw_load() retain the previous behavior of always
  # running aux-change detection (rather than silently skipping it).
  if (!force && is.null(aux_hashes)) {
    aux_hashes <- get_aux_hashes(aux_measures, verbose = verbose)
  }

  # Validate the aux_hashes input when supplied: it must be a non-empty named
  # character vector with unique, non-missing names and non-missing values.
  if (!force && !is.null(aux_hashes)) {
    if (!is.character(aux_hashes) || length(aux_hashes) == 0L) {
      cli::cli_abort(
        "aux_hashes must be a non-empty named character vector.",
        class = c("valid_dlw_load_bad_aux_hashes", "piperr")
      )
    }
    if (is.null(names(aux_hashes)) || any(!nzchar(names(aux_hashes)))) {
      cli::cli_abort(
        "aux_hashes must have non-empty names (one per measure).",
        class = c("valid_dlw_load_bad_aux_hashes", "piperr")
      )
    }
    if (anyDuplicated(names(aux_hashes)) > 0L) {
      cli::cli_abort(
        "aux_hashes names must be unique.",
        class = c("valid_dlw_load_bad_aux_hashes", "piperr")
      )
    }
    if (any(is.na(aux_hashes)) || any(!nzchar(aux_hashes))) {
      cli::cli_abort(
        "aux_hashes values must be non-missing, non-empty content hashes.",
        class = c("valid_dlw_load_bad_aux_hashes", "piperr")
      )
    }
  }

  # Resolve force_surveys: map each identifier to a survey_id present in the
  # filtered/latest inventory (inv_svy_full). This uses inv_svy_full, which is
  # already computed, and reuses dt_master (already loaded when !force) for
  # the pip_id reverse-map; it never loads the master a second time.
  force_res <- resolve_force_surveys(
    force_surveys,
    inv_svy_full = inv_svy_full,
    dt_master = if (!force) dt_master else NULL,
    verbose = verbose
  )
  forced_inv <- NULL
  if (length(force_res$survey_ids) > 0L) {
    forced_inv <- inv_svy_full[survey_id %in% force_res$survey_ids]
  }

  if (length(force_res$survey_ids) > 0L) {
    pipfun::log_info(
      "Surveys forced for reprocessing.",
      name = "pipdata_log",
      logmeta = list(
        info = "force_surveys_inf",
        n_forced = length(force_res$survey_ids),
        surveys_forced = force_res$survey_ids,
        n_from_survey_id = length(force_res$resolved_from_survey_id),
        n_from_pip_id = length(force_res$resolved_from_pip_id)
      )
    )
  }
  if (length(force_res$unknown) > 0L) {
    pipfun::log_info(
      "Force_surveys identifiers matched no known survey.",
      name = "pipdata_log",
      logmeta = list(
        info = "force_surveys_unknown_inf",
        unknown_identifiers = force_res$unknown
      )
    )
  }
  if (length(force_res$survey_ids) > 0L && verbose) {
    cli::cli_alert_info(
      "{length(force_res$survey_ids)} surve{?y/ies} forced for reprocessing."
    )
  }
  if (length(force_res$unknown) > 0L && verbose) {
    cli::cli_alert_info(
      "{length(force_res$unknown)} force_surveys identifier{?s} did not match any known survey and {?was/were} skipped."
    )
  }

  # Stage 1: build the aux candidate set from the per-survey aux hash
  # comparison over the full filtered/latest inventory (all previously-cleaned
  # surveys). Only runs when aux_hashes are supplied and the master is
  # available (i.e. not force mode).
  inv_aux <- NULL
  changed_measures <- character(0)

  if (!force && !is.null(aux_hashes) && length(aux_hashes) > 0L && !is.null(dt_master) && nrow(inv_svy_full) > 0L) {
    # Count surveys with a missing (NA) stored aux hash. These were cleaned
    # before this feature and are ignored for the change comparison. Logged
    # regardless of whether any candidate is found.
    aux_cols <- paste0("aux_", names(aux_hashes), "_hash")
    aux_cols <- intersect(aux_cols, names(dt_master))
    n_na_hash <- 0L
    if (length(aux_cols) > 0L) {
      n_na_hash <- sum(
        Reduce(`|`, lapply(aux_cols, function(col) is.na(dt_master[[col]])))
      )
    }
    if (n_na_hash > 0L) {
      pipfun::log_info(
        "Surveys with no stored aux hash (cleaned before this feature) are ignored for aux-change detection.",
        name = "pipdata_log",
        logmeta = list(
          info = "aux_na_hash_inf",
          n_surveys_na_hash = n_na_hash
        )
      )
    }

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
      # intersect the affected surveys with the candidate set. The inventory
      # is pre-filtered to the candidate survey IDs before the detailed aux
      # comparison, so only candidate rows are materialized.
      changed_measures <- attr(candidates, "changed_measures")

      all_changes_aux <- valid_aux_load(
        measure = changed_measures,
        compare = "all",
        verbose = verbose
      )
      inv_candidates <- inv[survey_id %in% candidates$survey_id]
      ls_inv_aux <- lapply(all_changes_aux, filter_aux_inv, inv = inv_candidates)

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
      (is.null(inv_aux) || nrow(inv_aux) == 0) &&
      (is.null(forced_inv) || nrow(forced_inv) == 0)
  ) {
    cli::cli_abort(
      "No surveys to process: all surveys are up to date and no auxiliary changes affect any survey.",
      class = "piperr"
    )
  }

  # Bind with inventory from aux changes and the forced surveys, then choose
  # only unique (overlaps between forced and normal candidates are deduplicated).
  inv_to_clean <- rbind(inv_svy, inv_aux, forced_inv, fill = TRUE)

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
      n_forced       = if (is.null(forced_inv)) 0L else nrow(forced_inv),
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


# Shared mutual-exclusivity message so both exported guard sites (pd_process_data
# and valid_dlw_load) cannot drift apart in wording.
.force_exclusive_msg <- paste0(
  "force and force_surveys are mutually exclusive: force = TRUE switches ",
  "stamp to timestamp versioning globally while force_surveys preserves ",
  "content versioning. Specify only one."
)

#' Resolve `force_surveys` identifiers to survey_id values
#'
#' Maps a character vector of `survey_id` and/or `pip_id` identifiers to the
#' subset present in the module-filtered, latest-version inventory
#' (`inv_svy_full`). Lookup is first-by-`survey_id` membership, then by
#' `pip_id` reverse-map through the already-loaded master inventory. Unknown
#' identifiers are collected, not aborted.
#'
#' @param force_surveys Character vector of `survey_id` and/or `pip_id`
#'   identifiers, or `NULL`.
#' @param inv_svy_full A `data.table` of the module-filtered, latest-version
#'   DLW inventory (already computed by [valid_dlw_load()]).
#' @param dt_master A `data.table` of the PIP master inventory, already loaded
#'   by the caller, or `NULL` when unavailable.
#' @param verbose Logical. Print progress messages.
#'
#' @return A named list with character vectors:
#'   `survey_ids` (resolved survey_ids present in `inv_svy_full`),
#'   `resolved_from_survey_id`, `resolved_from_pip_id`, and `unknown`.
#'
#' @family pd_process_data pipeline
#' @keywords internal
resolve_force_surveys <- function(
  force_surveys,
  inv_svy_full,
  dt_master,
  verbose = TRUE
) {
  empty <- list(
    survey_ids = character(0),
    resolved_from_survey_id = character(0),
    resolved_from_pip_id = character(0),
    unknown = character(0)
  )

  if (is.null(force_surveys) || length(force_surveys) == 0L) {
    return(empty)
  }

  # Input must be a character vector; a numeric/factor silently matches
  # nothing and no-ops, so abort loudly instead (mirrors aux_hashes pattern).
  if (!is.character(force_surveys)) {
    cli::cli_abort(
      "force_surveys must be a character vector of survey_id and/or pip_id values.",
      class = "piperr"
    )
  }

  # Deduplicate before the resolution loop so log counts reflect the actual
  # number of unique surveys, not redundant caller-supplied entries.
  force_surveys <- unique(force_surveys)

  inv_ids <- inv_svy_full$survey_id

  # Defensive column-existence check: pip_id resolution needs the master and
  # its pip_id column. Without it, pip_id inputs are treated as unknown.
  master_has_pip_id <- !is.null(dt_master) && "pip_id" %in% names(dt_master)

  survey_ids <- character(0)
  resolved_from_survey_id <- character(0)
  resolved_from_pip_id <- character(0)
  unknown <- character(0)

  # Pass 1: direct survey_id membership (lookup-first). Only identifiers that
  # FAIL this check (neither a survey_id member) can ever need pip_id
  # resolution, so the pip_id reverse-map is built lazily over just those.
  is_survey_id <- force_surveys %in% inv_ids
  survey_ids <- force_surveys[is_survey_id]
  resolved_from_survey_id <- survey_ids
  pip_candidates <- force_surveys[!is_survey_id]

  if (length(pip_candidates) > 0L) {
    if (master_has_pip_id) {
      # Build the reverse-map ONLY over the identifiers we actually need to
      # resolve (avoids a full-master scan and avoids aborting on unrelated
      # ambiguous pip_ids elsewhere in the master). Subset the master to the
      # requested pip_ids first.
      up_candidates <- toupper(pip_candidates)
      pip_map <- collapse::funique(
        dt_master[toupper(pip_id) %in% up_candidates, .(pip_id, survey_id)]
      )
      # A pip_id that maps to more than one DISTINCT survey_id is ambiguous:
      # abort on the requested identifier rather than silently picking one.
      if (anyDuplicated(pip_map$pip_id) > 0L) {
        ambiguous <- pip_map[duplicated(pip_map$pip_id), pip_id][1L]
        cli::cli_abort(
          "pip_id '{ambiguous}' maps to multiple distinct survey_ids; cannot resolve force_surveys.",
          class = "piperr"
        )
      }
      master_pip_key <- stats::setNames(
        pip_map$survey_id,
        toupper(pip_map$pip_id)
      )

      for (id in pip_candidates) {
        id_upper <- toupper(id)
        if (id_upper %in% names(master_pip_key)) {
          # pip_id reverse-map, case-insensitive (matches pip_id_map building).
          svy <- master_pip_key[[id_upper]]
          if (svy %in% inv_ids) {
            survey_ids <- c(survey_ids, svy)
            resolved_from_pip_id <- c(resolved_from_pip_id, id)
          } else {
            # Survey resolved via pip_id but outside module/latest filter (R9).
            unknown <- c(unknown, id)
          }
        } else {
          unknown <- c(unknown, id)
        }
      }
    } else {
      unknown <- c(unknown, pip_candidates)
    }
  }

  # Distinguish "your IDs are wrong" from "the master couldn't load".
  if (verbose && length(pip_candidates) > 0L) {
    if (!is.null(dt_master) && !master_has_pip_id) {
      cli::cli_alert_warning(
        "Master inventory lacks pip_id column; pip_id resolution unavailable. All non-survey_id identifiers treated as unknown."
      )
    }
    if (is.null(dt_master)) {
      cli::cli_alert_warning(
        "Master inventory unavailable; pip_id resolution skipped. All non-survey_id identifiers treated as unknown."
      )
    }
  }

  list(
    survey_ids = unique(survey_ids),
    resolved_from_survey_id = unique(resolved_from_survey_id),
    resolved_from_pip_id = resolved_from_pip_id,
    unknown = unknown
  )
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
#' @param dt_master A `data.table` of the PIP master inventory, already loaded
#'   by the caller ([valid_dlw_load()]) and shared with the aux-hash
#'   comparison. Default `NULL`, in which case the master is loaded here.
#' @param master_available Logical. Whether the caller already attempted to
#'   load the master. When `TRUE`, `dt_master` is used as-is. When `FALSE`,
#'   the master was attempted but unavailable, so all surveys are returned
#'   without re-loading. Default `NULL` (unknown — load here if `dt_master`
#'   is `NULL`).
#'
#' @return A `data.table` of surveys still needing processing, or
#'   `NULL` if all surveys have already been cleaned.
#'
#' @family pd_process_data pipeline
#' @keywords internal
inv_to_process <- function(
  inv,
  verbose = TRUE,
  dt_master = NULL,
  master_available = NULL
) {
  # Load master inventory to compare with previous cleaning, unless the
  # caller already loaded it (shared single-load handoff). When the caller
  # explicitly reports the master is unavailable (master_available = FALSE),
  # do not re-load — return all surveys instead.
  if (isFALSE(master_available)) {
    return(inv)
  }

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

  # Deduplicate by survey_id + content_hash_dlw. A survey may have multiple
  # historical DLW content hashes in the master; the join below matches the
  # current DLW content hash so only the corresponding master row is used.
  dt_master_hash <- collapse::funique(dt_master[, .(survey_id, content_hash_dlw)])

  # Rename the current DLW inventory's content_hash to content_hash_dlw so the
  # join can match it against the master's stored content_hash_dlw.
  inv_join <- data.table::copy(inv)
  data.table::setnames(inv_join, "content_hash", "content_hash_dlw")

  # Left-join on survey_id + content_hash_dlw. reportvar = TRUE adds the .joyn
  # column: "matched" when the current DLW content hash matches the master's
  # content_hash_dlw (survey already cleaned), "x" when the survey is new or
  # its DLW content changed.
  inv_compare <- joyn::left_join(
    inv_join,
    dt_master_hash,
    by = c("survey_id", "content_hash_dlw"),
    relationship = "many-to-one",
    verbose = FALSE,
    reportvar = ".joyn"
  )

  # Keep: new surveys or surveys whose DLW content changed (.joyn == "x").
  inv_changed <- inv_compare[.joyn == "x"]
  # Restore the original column name for the DLW content hash, then drop the
  # .joyn report column.
  data.table::setnames(inv_changed, "content_hash_dlw", "content_hash")
  inv_changed[, .joyn := NULL]

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
#' The current DLW inventory is joined to the master on both `survey_id` and
#' the DLW content hash (`inv$content_hash` matched to `master$content_hash_dlw`),
#' so a survey with multiple historical DLW versions is compared against the
#' aux hashes of its current version only.
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
    # No stored aux hashes at all — every survey was cleaned before this
    # feature. These are ignored for aux-change detection (no hash to compare),
    # so there are no candidates.
    return(NULL)
  }

  # Build a survey-level master keyed by survey_id + content_hash_dlw, and
  # detect conflicting aux hashes within a group in a single pass. A group
  # with more than one distinct aux-hash combination for the same key is a
  # conflict (split pip_id rows must share the same aux versions).
  key_cols <- c("survey_id", "content_hash_dlw")
  master_svy <- collapse::funique(dt_master[, c(key_cols, aux_cols), with = FALSE])

  n_groups <- nrow(collapse::funique(master_svy[, key_cols, with = FALSE]))
  if (n_groups != nrow(master_svy)) {
    cli::cli_abort(
      "Conflicting aux hashes found for the same survey_id and content_hash_dlw.",
      class = c("aux_hash_candidates_conflict", "piperr")
    )
  }

  # Rename the current DLW inventory's content_hash to content_hash_dlw so the
  # join can match it against the master's stored content_hash_dlw. This
  # ensures a survey with multiple historical DLW versions is compared against
  # the aux hashes of its current version only.
  inv_join <- data.table::copy(inv)
  data.table::setnames(inv_join, "content_hash", "content_hash_dlw")

  inv_compare <- joyn::left_join(
    inv_join,
    master_svy,
    by = c("survey_id", "content_hash_dlw"),
    relationship = "many-to-one",
    verbose = FALSE,
    reportvar = ".joyn"
  )

  # Determine which measures changed for each survey. Surveys with a missing
  # (NA) stored aux hash are ignored for the change comparison — they were
  # cleaned before this feature and simply have no hash recorded yet. Only
  # surveys with a populated stored hash are compared against the current
  # hash.
  changed_measures <- character(0)
  candidate_idx <- rep(FALSE, nrow(inv_compare))

  for (m in names(aux_hashes)) {
    col <- paste0("aux_", m, "_hash")
    if (!col %in% names(inv_compare)) {
      # Measure column not present at all — nothing to compare for this
      # measure; skip it (do not treat as changed).
      next
    }
    stored <- inv_compare[[col]]
    current <- aux_hashes[[m]]
    is_na <- is.na(stored)
    # Only compare surveys that have a populated stored hash.
    is_changed <- !is_na & stored != current
    candidate_idx <- candidate_idx | is_changed
    if (any(is_changed, na.rm = TRUE)) {
      changed_measures <- unique(c(changed_measures, m))
    }
  }

  candidates <- inv_compare[candidate_idx]
  # Drop the joined aux hash columns and the .joyn report column.
  drop_cols <- intersect(c(aux_cols, ".joyn"), names(candidates))
  if (length(drop_cols) > 0L) {
    candidates[, (drop_cols) := NULL]
  }
  # Restore the original column name for the DLW content hash.
  data.table::setnames(candidates, "content_hash_dlw", "content_hash")

  if (nrow(candidates) == 0L) {
    return(NULL)
  }

  attr(candidates, "changed_measures") <- changed_measures

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(candidates)
}
