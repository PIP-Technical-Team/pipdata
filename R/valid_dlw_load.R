valid_dlw_load <- function(
  inv,
  aux_measures = c("pfw", "cpi", "ppp", "pop", "gdp", "pce"),
  modules = c("ALL", "GROUP", "HIST", "GPWG", "BIN"),
  # date_valid = .pipdataenv$date_valid,
  verbose = getOption("pipdata.verbose", default = FALSE)
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
  changes_aux <- valid_aux_load(measure = aux_measures, compare = "all")
  ls_inv_aux <- lapply(changes_aux, filter_aux_inv, inv = inv)

  # Join release and vintage changes and select unique surveys
  inv_aux <- ls_inv_aux |>
    data.table::rbindlist() |>
    collapse::funique()

  # Filter inventory for specific modules and select last version of each survey (and random sample if needed)
  # inv <- m_inv_valid(inv, seed = seed) # Mock function to select 20 random surveys from valid inventory
  inv <- inv[module %in% modules]
  inv_svy <- last_ver_inv(inv)

  # Select valid surveys and compare to previous cleaning
  inv_svy <- inv_to_process(inv_svy)

  # Bind with inventory from aux changes
  inv_to_clean <- rbind(inv_svy, inv_aux, fill = TRUE)

  # Only those after specific date validated
  # inv_to_clean <- inv_to_clean[date_validated < date_valid]

  # Choose only unique
  inv_to_clean <- unique(inv_to_clean)

  # Order alphabetically
  setorder(inv_to_clean, survey_id)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_to_clean)
}


filter_aux_inv <- function(inv, changes_aux) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  # Choose last version if not empty

  if (!(nrow(inv_aux) == 0)) {
    inv_aux <- last_ver_inv(inv_aux)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_aux)
}

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

inv_to_process <- function(inv) {
  # Select valid surveys and compare to previous cleaning
  inv_svy <- tryCatch(
    expr = {
      # Load master inventory to compare with previous cleaning
      dt_master <- pipload::load_pip_master_inventory()

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
        joyn::anti_join(dt_master, by = key_inventory, verbose = FALSE)

      inv_svy
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Could not load or master inventory. Returning all valid surveys without comparing to previous cleaning."
      )
      return(inv)
    }
  )

  if (inv_svy[, .N] == 0) {
    cli::cli_abort(
      "All surveys in the inventory have been cleaned in previous versions. No surveys to process."
    )
  }

  return(inv_svy)
}
