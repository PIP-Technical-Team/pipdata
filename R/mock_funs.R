m_inv_load <- function(folder = "DLW-OUTPUT",
                       name_inv = "pip_raw_inventory_20250203_TEST.qs") {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # inv_files <- fs::dir_ls(fs::path(Sys.getenv("PIP_ROOT_DIR"),folder,"/_Inventory/_release"))

  inv   <- qs::qread(fs::path(Sys.getenv("PIP_ROOT_DIR"),
                              folder,
                              "/_Inventory/_release",
                              name_inv))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv)

}

m_inv_valid <- function(inv,
                        filter = "random",
                        seed = 1089,
                        n = 20) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Select only valid surveys
  inv_valid <- inv[inv$status == "valid",]

  if(filter == "all"){

    return(inv_valid)
  }else if(filter == "compare"){

    # Compare to previous cleaning with master file
    master_pip_inv <- pipload::load_pip_master_inventory()|>
      collapse::fselect(survey_id, version_dlw)|>
      collapse::frename(version = "version_dlw")

    inv_clean <- inv_valid[!master_pip_inv, on = .( survey_id, version)]

    return(inv_to_clean)

  }else if(filter == "random"){

    set.seed(seed)

    inv_smp <- inv_valid[module %in% c("ALL","GROUP","HIST",  "GPWG", "BIN")]
    inv_smp <- inv_smp[,.SD[sample(.N, min(floor(n/5), .N))], by = module]

    return(inv_smp)

  }else{

    cli::cli_abort("Need to select a filter")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(NULL)

}

m_inv_filter <- function(inv,
                         seed = 1089,
                         n = 20) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  set.seed(seed)

  # selected   <- sample(1:nrow(inv), n) # Needs to be changed to filter by status
  # inv_smp    <- inv[selected,]

  inv_smp <- inv[module %in% c("ALL","GROUP","HIST",  "GPWG", "BIN")]
  inv_smp <- inv_smp[,.SD[sample(.N, min(floor(n/5), .N))], by = module]

  ## Add Philipines and China
  # inv_phl12 <- inv[inv$country_code == "PHL" & inv$surveyid_year == 2012,]
  #
  # inv_phl94 <- inv[inv$country_code == "PHL" & inv$surveyid_year == 1994,]
  #
  # inv_chn11 <- inv[inv$country_code == "CHN" & inv$surveyid_year == 2011,]
  #
  # inv_othr <- rbind(inv_phl12, inv_phl94, inv_chn11, fill = TRUE)
  #
  # inv_othr <- last_ver_inv(inv_othr)

  # Bind lists

  # inv_smp <- rbind(inv_smp, inv_othr)

  # inv_smp <- unique(inv_smp)

  # Randomly assign names to a new variable

  # inv_smp[, status := sample(options, .N, replace = TRUE)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_smp)

}


m_compare_aux_release <- function() {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  changes <- qs::qread("aux_changes.qs")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(changes)

}


m_svy_id_to_att <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Add on for the mock to work
  survey_id <- unique(dt$survey_id)

  dt <- survey_id_to_attr(dt, survey_id)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}


fix_inv <- function(inv,
                    inv_to_clean) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inv_dlw_vrs <- inv[, .(dlw_version = unlist(pin_version)[[1]]),
                     by = survey_id]

  inv_to_clean <- inv_to_clean |>
    joyn::left_join(inv_dlw_vrs, by = "survey_id",
                    reportvar = FALSE,
                    verbose = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_to_clean)

}
