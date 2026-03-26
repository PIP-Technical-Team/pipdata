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
#' @return A data.frame: updated pip inventory (`new_pip_inv`) with new
#'   versions for cleaned data and metadata.
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
  results <- purrr::map(inv_ls, process_data, aux_list = aux_list)
  names(results) <- inv_to_clean$survey_id

  # Update inventory with new versions of clean data
  new_pip_inv <- update_pip_inventory(
    inv_to_clean = inv_to_clean,
    proc_dta = results
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(new_pip_inv)
}

#' Process datalibweb data: merge PFW data and clean variables
#'
#' @param inv inventory with survey_id and pins folder
#' @param pfw PFW
#' @param ...  other parameters
#'
#' @return data.table
#' @export
#'
#' @examples
#' release <- "20250203"
#' pipfun::setup_working_release(release)
#'
#' pfw <- pipload::load_aux_data("pfw")
#'
#' gd  <- pipload::load_aux_data("CHN", 2015)
#' gd  <- pipdata:::m_svy_id_to_att(gd)
#' process_data(gd, pfw)
#'
#' md   <- pipload::load_aux_data(country = "PRY", 2012)
#' md  <- pipdata:::m_svy_id_to_att(md)
#' process_data(md, pfw)
process_data <- function(inv, aux_list, ...) {
  # on.exit ------------
  on.exit({
    rm(survey_id, envir = .pipdataenv)
  })

  svy <- inv$survey_id

  assign("survey_id", svy, envir = .pipdataenv)

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

      # Save clean data and metadata
      versions_data <- save_pip_data(ls_clean, alias = "pip")

      versions_metadata <- save_pip_data(metadata, alias = "pip_meta")

      # Results
      list(
        pip_names = names(ls_clean),
        versions_data = versions_data,
        versions_metadata = versions_metadata
      )
    },
    piperr = function(cnd) {
      survey_id <- c(.pipdataenv$survey_id)

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
      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(
        event = "error",
        message = cnd$message,
        name = "pipdata_log",
        logmeta = list(
          error = "unknown_error",
          survey = survey_id,
          status = "The survey was skipped"
        )
      )

      NULL
    }
  )

  return(res)
}
