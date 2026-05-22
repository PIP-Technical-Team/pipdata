#' Process DLW inventory and create cleaned pip data
#'
#' Iterate over the datalibweb (DLW) inventory, process each survey by
#' merging auxiliary data (PFW, CPI, PPP, population, GDP, PCE), cleaning
#' main variables, creating metadata, and saving new versions of the cleaned
#' data and metadata into the pip storage. The function returns an updated
#' pip inventory with the new versions recorded.
#'
#' @param inv A data.frame or tibble containing the DLW inventory. It can be
#' downloaded using `pipload::load_gmd_valid_inv()`.
#' @param aux_measures A character vector of auxiliary measures to load and merge
#' with the DLW data. The default is `c("pfw", "cpi", "ppp", "pop", "gdp", "pce")`.
#' @param force Logical. If `TRUE`, forces reprocessing of all surveys by
#'   switching stamp versioning to `"timestamp"` and bypassing the master
#'   inventory comparison. Default `FALSE`.
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("pipdata.verbose", default = FALSE)`.
#' @return A data.frame: updated pip inventory (`new_pip_inv`) with new
#'   versions for cleaned data and metadata.
#'
#' @details
#' **Logging**: This function writes `process_summary_inf` and `null_svys_inf` entries
#' to the `"pipdata_log"`, summarizing totals and failed surveys. Additional entries for
#' auxiliary file changes and inventory verification are emitted by [valid_dlw_load()]
#' and [build_pip_inventory()] respectively.
#'
#' @export
#' @examples
#' \dontrun{
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#' inv <- pipload::load_gmd_valid_inv()
#' pd_process_data(inv)
#' }
pd_process_data <- function(
  inv = inv,
  aux_measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  force = FALSE,
  verbose = getOption("pipdata.verbose", default = FALSE)
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Temporarily switch stamp versioning to "timestamp" when force = TRUE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (force) {
    old_versioning <- stamp::st_opts("versioning", .get = TRUE)
    on.exit(stamp::st_opts(versioning = old_versioning), add = TRUE)
    stamp::st_opts(versioning = "timestamp")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load aux data for metadata attributes and processing
  aux_list <- lapply(aux_measures, pipload::load_aux_data, verbose = FALSE)
  names(aux_list) <- aux_measures

  # Load valid inventory
  inv_to_clean <- valid_dlw_load(
    inv = inv,
    aux_measures = aux_measures,
    force = force,
    verbose = verbose
  )

  if (is.null(inv_to_clean) || nrow(inv_to_clean) == 0) {
    cli::cli_alert_info("No surveys to process.")

    # Load old pip inventory and return
    old_pip_inv <- pipload::load_pip_master_inventory(verbose = FALSE)
    return(old_pip_inv)
  }

  # Process data
  inv_ls <- split(inv_to_clean, seq_len(nrow(inv_to_clean)))
  names(inv_ls) <- inv_to_clean$survey_id
  results <- lapply(inv_ls, process_data, aux_list = aux_list)
  names(results) <- inv_to_clean$survey_id

  # Log processing summary
  n_total   <- length(results)
  n_success <- sum(!vapply(results, is.null, logical(1)))
  n_failed  <- n_total - n_success
  successful <- names(Filter(Negate(is.null), results))

  pipfun::log_info(
    "Processing complete.",
    name    = "pipdata_log",
    logmeta = list(
      info            = "process_summary_inf",
      n_total         = n_total,
      n_success       = n_success,
      n_failed        = n_failed,
      surveys_success = successful
    )
  )

  # Log null (failed) surveys before building the inventory map
  null_ls <- names(Filter(is.null, results))
  if (length(null_ls) > 0L) {
    pipfun::log_add(
      event   = "info",
      message = "Some surveys were not cleaned. Review logmeta to identify which ones.",
      name    = "pipdata_log",
      logmeta = list(info = "null_svys_inf", surveys = null_ls)
    )
  }

  # Build a minimal pip_id → survey_id map from successful results.
  # This is the only per-run data the assembler needs; version metadata is
  # read directly from stamp's persisted catalogs.
  pip_id_map <- data.table::rbindlist(
    lapply(Filter(Negate(is.null), results), \(x) {
      data.table::data.table(pip_id = toupper(unlist(x$pip_names)))
    }),
    idcol = "survey_id"
  )

  # Update inventory via catalog-based assembler
  new_pip_inv <- build_pip_inventory(
    inv_to_clean = inv_to_clean,
    pip_id_map   = pip_id_map
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(new_pip_inv)
}

#' Process datalibweb data: merge PFW data and clean variables
#'
#' @param inv inventory with survey_id and pins folder
#' @param aux_list Named list of auxiliary data frames; expected keys:
#'   `"pfw"`, `"cpi"`, `"ppp"`, `"pop"`, `"gdp"`, `"pce"`.
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
process_data <- function(inv, aux_list, ...) {
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
      ls_clean <- pd_dlw_clean(ls_cpfw)

      # Validate

      #valid_inv    <- pip_validation(ls_clean)
      #valid_data   <- valid_clean_data(valid_inv)

      # Create Aux Metadata

      metadata <- pd_aux_attr(clean_data = ls_clean, aux_list = aux_list)

      # Save clean data and metadata to stamp (side effect; version facts
      # are read back from the stamp catalog by build_pip_inventory()).
      save_pip_data(ls_clean, alias = "pip")
      save_pip_data(metadata, alias = "pip_meta")

      # Return only pip_names — version metadata is no longer tracked
      # in-memory; the assembler reads it from stamp catalogs directly.
      list(pip_names = names(ls_clean))
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
