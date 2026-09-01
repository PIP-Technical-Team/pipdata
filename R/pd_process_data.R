#' Process DLW inventory and create cleaned pip data
#'
#' Iterate over the datalibweb (DLW) inventory, process each survey by
#' merging auxiliary data (PFW, CPI, PPP, population, GDP, PCE), cleaning
#' main variables, creating metadata, and saving new versions of the cleaned
#' data and metadata into the pip storage. The function returns an updated
#' pip inventory with the new versions recorded.
#'
#' @param inv A data.frame or tibble containing the completed DLW validation
#'   inventory. Default `NULL`, in which case it is loaded internally via
#'   `pipload::load_gmd_valid_inv()`. Before planning or row lookup, input is
#'   normalized to `data_available = "Yes"` rows whose status is `"valid"` or
#'   `"invalid"`. Recognized legacy blank/`"No"` retry rows are excluded;
#'   malformed completed rows abort rather than entering cleaning.
#' @param aux_measures A character vector of auxiliary measures to load and merge
#' with the DLW data. The default is `c("pfw", "cpi", "ppp", "pop", "gdp", "pce")`.
#' @param force Logical. If `TRUE`, forces reprocessing of all surveys by
#'   switching stamp versioning to `"timestamp"` and bypassing the master
#'   inventory comparison. Default `FALSE`. For surgical re-processing
#'   without the global versioning side effect, see `force_surveys`.
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#' @param force_surveys Character vector of `survey_id` and/or `pip_id`
#'   values to re-process surgically, alongside the normal invalidation
#'   candidates. Mutually exclusive with `force = TRUE`. Preserves content-based
#'   stamp versioning (unlike `force = TRUE`, which switches to timestamp
#'   versioning for the entire run). Unknown identifiers are warned about and
#'   skipped. Default `NULL`.
#' @param bootstrap Logical. Explicitly permit rebuilding unknown legacy
#'   provenance. Default `FALSE`.
#' @param bootstrap_entities Optional restrictive survey/pip identifiers for a
#'   bootstrap canary. Unlike `force_surveys`, this never expands selection.
#' @param dependency_plan Optional precomputed advisory plan. Execution validates
#'   and restricts it again before any processing side effect.
#' @return A data.frame: updated pip inventory (`new_pip_inv`) with new
#'   versions for cleaned data and metadata.
#'
#' @details
#' **Validation handoff**: Both valid and invalid completed validation rows keep
#' their existing cleaning eligibility. Execution-failure control rows are not
#' eligible. The guard is applied here and again during dependency execution so
#' legacy inventories cannot create cleaning, metadata, or deflation actions.
#'
#' **Logging**: This function writes `process_summary_inf` and `null_svys_inf` entries
#' to the `"pipdata_log"`, summarizing totals and failed surveys. Additional entries for
#' auxiliary file changes and inventory verification are emitted by [valid_dlw_load()]
#' and [build_pip_inventory()] respectively.
#'
#' **Aux hashes**: the current `content_hash` for every requested auxiliary
#' measure is resolved once from the `"aux"` stamp catalog via
#' [get_aux_hashes()] before aux data is loaded. The run-level hash map is
#' passed to [build_pip_inventory()] and recorded in the master inventory so
#' that [valid_dlw_load()] can gate aux-change detection against the aux data
#' actually used in this run.
#'
#' **Recode spec**: the recode specification is synced to stamp once via
#' [sync_recode_spec()] before the per-survey loop and the resolved spec is
#' threaded into each [process_data()] call, so [apply_recode_spec()] performs
#' no stamp I/O per survey.
#'
#' **Memory management**: surveys are processed one at a time. After each survey
#' is saved, the large intermediates (`df`, `ls_cpfw`, `ls_clean`, `metadata`)
#' are explicitly removed and `gc()` is called inside [process_data()] before
#' the next survey is loaded, keeping peak heap bounded on full-inventory runs.
#'
#' @export
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#' pd_process_data()
#' }
pd_process_data <- function(
  inv = NULL,
  aux_measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = TRUE),
  force_surveys = NULL,
  bootstrap = FALSE,
  bootstrap_entities = NULL,
  dependency_plan = NULL
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Guard force + force_surveys are mutually exclusive, before any stamp
  # versioning side effect runs.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (force && !is.null(force_surveys)) {
    cli::cli_abort(
      .force_exclusive_msg,
      class = "piperr"
    )
  }

  if (!isTRUE(bootstrap) && !is.null(bootstrap_entities)) {
    cli::cli_abort("bootstrap_entities requires bootstrap = TRUE.",
                   class = "pipdata_bootstrap_selector_error")
  }

  aux_measures <- pd_normalize_aux_measures(aux_measures)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Temporarily switch stamp versioning to "timestamp" when force = TRUE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load the validated DLW inventory when the caller does not supply one
  if (is.null(inv)) {
    inv <- pipload::load_gmd_valid_inv(verbose = verbose)
  }
  inv <- .filter_completed_dlw_validation_inventory(inv)

  master <- pipload::load_pip_master_inventory(verbose = verbose)
  execution <- pd_prepare_execution(
    inv = inv, master = master, context = pd_dependency_context(),
    advisory_plan = dependency_plan, bootstrap = bootstrap,
    bootstrap_entities = bootstrap_entities, force = force,
    force_surveys = force_surveys, verbose = verbose, measures = aux_measures,
    metadata_measures = setdiff(aux_measures, "pfw")
  )
  on.exit(pd_lease_release(execution$lease), add = TRUE)
  if (force) {
    old_versioning <- stamp::st_opts("versioning", .get = TRUE)
    on.exit(stamp::st_opts(versioning = old_versioning), add = TRUE)
    stamp::st_opts(versioning = "timestamp")
  }
  selected <- execution$plan$actions
  if (!any(selected$action != "none")) return(master)

  run_id <- pd_random_id()
  options <- pd_pipeline_options(
    verbose = verbose,
    force = force,
    force_surveys = force_surveys %||% character(),
    bootstrap = bootstrap,
    bootstrap_entities = bootstrap_entities %||% character(),
    checkpoint_size = getOption("pipdata.manifest_checkpoint_n", 25L),
    checkpoint_seconds = getOption(
      "pipdata.manifest_checkpoint_seconds", 60
    )
  )
  clean_actions <- selected[stage == "clean"]
  clean_context <- pd_stage_context(
    execution, run_id, options, clean_actions, force_surveys
  )
  recode_spec <- if (any(clean_actions$action != "none")) {
    sync_recode_spec(alias = "pip_inv", verbose = verbose)
  } else {
    NULL
  }
  clean <- pd_run_clean_stage_prepared(
    execution, clean_actions, run_id, clean_context, master, inv, options,
    recode_spec, verbose
  )
  execution <- clean$execution
  master <- clean$master
  if (any(clean_actions$action != "none")) {
    execution <- pd_refresh_execution_facts(
      execution,
      master,
      force = force,
      force_surveys = force_surveys,
      bootstrap = bootstrap,
      bootstrap_entities = bootstrap_entities,
      verbose = verbose
    )
  }
  metadata_actions <- data.table::copy(
    execution$plan$actions[stage == "metadata"]
  )
  failed_surveys <- clean$outcome$units[status == "failed", survey_id]
  if (length(failed_surveys)) {
    metadata_actions[
      survey_id %in% failed_surveys & action != "none",
      scheduling_state := "blocked"
    ]
  }
  metadata_context <- pd_stage_context(
    execution, run_id, options, metadata_actions, force_surveys
  )
  metadata <- pd_run_metadata_stage_prepared(
    execution, metadata_actions, run_id, metadata_context, master, options,
    verbose
  )
  master <- metadata$master
  return(master)

}

pd_invalidate_failed_action <- function(master, action, emit_log = TRUE) {
  out <- data.table::copy(data.table::as.data.table(master))
  stage <- action$stage[[1L]]
  ids <- if (stage == "clean") {
    out[survey_id == action$survey_id[[1L]], which = TRUE]
  } else out[pip_id == action$pip_id[[1L]], which = TRUE]
  columns <- switch(stage,
    clean = c("version_id_data", "content_hash_data", "version_id_metadata",
              "content_hash_metadata", "version_id_deflated",
              "content_hash_deflated", "deflated"),
    metadata = c("version_id_metadata", "content_hash_metadata",
                 "version_id_deflated", "content_hash_deflated", "deflated"),
    deflate = c("version_id_deflated", "content_hash_deflated", "deflated")
  )
  for (column in intersect(columns, names(out))) {
    value <- if (column == "deflated") FALSE else NA
    data.table::set(out, i = ids, j = column, value = value)
  }
  if (isTRUE(emit_log)) {
    pipfun::log_add(
      event = "error", message = "Forced dependency work failed.",
      name = "pipdata_log",
      logmeta = list(error = "forced_work_failed", stage = stage,
                     entity_id = action$entity_id[[1L]])
    )
  }
  out
}

pd_inventory_writer <- function(alias, id, verbose = FALSE) {
  function(candidate, lease) {
    pd_save_receipt(candidate, id, alias, verbose, lease)
  }
}

pd_execute_clean <- function(action, inv_row, execution, recode_spec,
                             verbose = FALSE) {
  survey_id <- action$survey_id[[1L]]
  result <- tryCatch({
    df <- inv_dlw_load(inv_row)
    merged <- pd_cpfw_merge(df, execution$snapshot$aux$objects$pfw)
    clean <- pd_dlw_clean(merged, verbose = verbose, recode_spec = recode_spec)
    metadata <- pd_aux_attr(clean, execution$snapshot$aux$objects)
    expected <- pd_assert_clean_output_set(
      action$expected_pip_ids[[1L]], clean, metadata
    )
    receipts <- lapply(expected, function(pip_id) {
      source_name <- names(clean)[match(pip_id, toupper(names(clean)))]
      pd_assert_execution_fence(execution)
      receipt <- pd_save_receipt(clean[[source_name]], pip_id, "pip", verbose,
                                 execution$lease)
      c(list(stage = "clean", survey_id = survey_id, pip_id = pip_id,
             input_hash = action$input_hash[[1L]],
             code_hash = action$code_hash[[1L]]), receipt)
    })
    receipts <- data.table::rbindlist(receipts, fill = TRUE)
    if (nrow(receipts) != length(expected) || any(!receipts$success)) {
      rlang::abort(
        "Clean artifact receipt was not verified.",
        class = "pipdata_clean_receipt_invalid"
      )
    }
    invariant <- data.table::copy(inv_row)
    invariant[, pip_id := expected[1L]]
    if (length(expected) > 1L) {
      invariant <- invariant[rep(seq_len(.N), each = length(expected))]
      invariant[, pip_id := expected]
    }
    receipts <- invariant[receipts, on = "pip_id"]
    success <- nrow(receipts) == length(expected) && all(receipts$success) &&
      setequal(receipts$pip_id, expected)
    list(stage = "clean", survey_id = survey_id, success = success,
         expected_pip_ids = expected, receipts = receipts,
         metadata = metadata)
  }, error = function(e) {
    if (!pd_condition_allowlisted(e, .PD_CLEAN_RECOVERABLE_CLASSES)) {
      rlang::cnd_signal(e)
    }
    root <- pd_condition_root(e)
    list(stage = "clean", survey_id = survey_id, success = FALSE,
          expected_pip_ids = character(), receipts = data.table::data.table(),
          metadata = list(), condition = new_stage_condition_record(
            root, "error", stage = "clean", entity_id = survey_id,
            survey_id = survey_id, operation = "clean", recoverable = TRUE
          ))
  })
  result
}

#' Process datalibweb data: merge PFW data and clean variables
#'
#' @param inv inventory with survey_id and pins folder
#' @param aux_list Named list of auxiliary data frames; expected keys:
#'   `"pfw"`, `"cpi"`, `"ppp"`, `"pop"`, `"gdp"`, `"pce"`.
#' @param recode_spec Optional pre-resolved recode spec (as returned by
#'   [sync_recode_spec()]) threaded to [pd_dlw_clean()]/[apply_recode_spec()] so
#'   the spec is read once upstream rather than once per survey. Default `NULL`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#' @param ...  other parameters
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' pfw <- pipload::load_aux_data("pfw")
#'
#' gd  <- pipload::load_aux_data("CHN", 2015)
#' gd  <- survey_id_to_attr(gd, unique(gd$survey_id))
#' process_data(gd, pfw)
#'
#' md   <- pipload::load_aux_data(country = "PRY", 2012)
#' md  <- survey_id_to_attr(md, unique(md$survey_id))
#' process_data(md, pfw)
#' }
process_data <- function(inv, aux_list, recode_spec = NULL, verbose = TRUE, ...) {
  # on.exit ------------
  on.exit({
    pd_env_rm("process_survey_id")
  })

  svy <- inv$survey_id

  pd_env_set("process_survey_id", svy)

  # Computations -------
  res <- tryCatch(
    expr = {
      # Load file
      df <- inv_dlw_load(inv)

      # Merge country PFW information
      ls_cpfw <- pd_cpfw_merge(df, aux_list[["pfw"]])

      # Clean main variables
      ls_clean <- pd_dlw_clean(ls_cpfw, verbose = verbose, recode_spec = recode_spec)

      # Validate

      #valid_inv    <- pip_validation(ls_clean)
      #valid_data   <- valid_clean_data(valid_inv)

      # Create Aux Metadata

      metadata <- pd_aux_attr(clean_data = ls_clean, aux_list = aux_list)

      # Save clean data and metadata to stamp (side effect; version facts
      # are read back from the stamp catalog by build_pip_inventory()).
      save_pip_data(ls_clean, alias = "pip", verbose = verbose)
      save_pip_data(metadata, alias = "pip_meta", verbose = verbose)

      # Return only pip_names — version metadata is no longer tracked
      # in-memory; the assembler reads it from stamp catalogs directly.
      # Build the result first (reads names(ls_clean)), then free the
      # survey-sized intermediates and collect so the heap returns to baseline
      # before the next survey is loaded — prevents OOM on full-inventory runs.
      result <- list(pip_names = names(ls_clean))
      rm(df, ls_cpfw, ls_clean, metadata)
      gc(verbose = FALSE)
      result
    },
    piperr = function(cnd) {
      survey_id <- c(pd_env_get("process_survey_id"))

      pipfun::log_add(
        event = "error",
        message = cnd$message,
        name = "pipdata_log",
        logmeta = list(
          error = class(cnd)[2],
          survey = survey_id,
          status = "The survey was skipped"
        )
      )

      NULL
    },

    error = function(cnd) {
      survey_id <- c(pd_env_get("process_survey_id"))

      # lapply() may wrap the original condition; traverse the parent chain
      # to recover the root cause (e.g. a piperr thrown inside lapply())
      original_cnd <- cnd
      while (!is.null(original_cnd$parent)) {
        original_cnd <- original_cnd$parent
      }

      if (inherits(original_cnd, "piperr")) {
        error_class <- class(original_cnd)[2] # e.g. "gd_type_miss"
        err_msg <- original_cnd$message
      } else {
        error_class <- "unknown_error"
        err_msg <- cnd$message
      }

      pipfun::log_add(
        event = "error",
        message = err_msg,
        name = "pipdata_log",
        logmeta = list(
          error = error_class,
          survey = survey_id,
          status = "The survey was skipped"
        )
      )

      NULL
    }
  )

  return(res)
}
