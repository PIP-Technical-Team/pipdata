valid_dlw_load <- function(inv,
                           measure = c("cpi", "ppp","pfw","pop"),
                           path = fs::path(Sys.getenv("PIP_ROOT_DIR"), "DLW-OUTPUT/")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(!is.data.table(inv)){
    inv <- data.table::as.data.table(inv)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Load changes in aux files

  changes_aux <- valid_aux_load(measure = measure,
                                compare = "all")

  ls_inv_aux <- lapply(changes_aux, filter_aux_inv, inv = inv)

  # Join release and vintage changes and select unique surveys

  inv_aux <- unique(data.table::rbindlist(ls_inv_aux))

  # Create mock changes for the inventory (Temporal)

  inv_svy <- m_inv_filter(inv, seed = 1089) # For now is a mock function

  # Bind with inventory from aux changes

  inv_to_clean <- rbind(inv_svy, inv_aux, fill = TRUE)

  # Order alphabetically

  setorder(inv_to_clean, survey_id)

  # Load survey files

  ls_svy <- lapply(1:length(inv_to_clean$pip_file_path),
                   \(x) qs::qread(inv_to_clean$pip_file_path[x]))

  names(ls_svy) <- inv_to_clean$survey_id

  # Add data from inventory to attributes of data table and add pip class

  ls <- purrr::map2(.x = ls_svy,
                    .y = names(ls_svy),
                    .f = data_to_dt)

  # Filter NULL surveys

  ls <- purrr::discard(ls, is.null)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}

data_to_dt <- function(dt, survey_id) {

  # on.exit ------------
  on.exit({
    rm(survey_id,
       envir = .logenv)
  }) # For now

  assign("survey_id",
         survey_id,
         envir = .pipdataenv)

  assign("survey_id",
         survey_id,
         envir = .logenv) # For now

  res <- tryCatch(
    expr = {

      #--------- defenses --------------------------

      if(!is.data.table(dt)){
        dt <- data.table::as.data.table(dt)
      }

      #--------- leaving just the 'label' attribute ---------

      dt <- only_labels(dt)

      #--------- Survey ID and its components ---------

      dt <- survey_id_to_attr(dt, survey_id)

      #--------- Add class ---------

      dt <- pipload::as_pip(dt)

      # Temporary fix

      dt[,
               module := NULL
      ]

    },

    piperr = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      args = list(error = class(cnd)[2],
                                  survey = survey_id,
                                  status = "The survey was skipped"))

      NULL

    },

    error = function(cnd){

      survey_id <- c(.pipdataenv$survey_id)

      pipfun::log_add(event = "error",
                      message = cnd$message,
                      name = "pipdata_log",
                      .trace = cnd$call,
                      logmeta = list(error = "unknown_error",
                                     survey = survey_id,
                                     status = "The survey was skipped"))

      NULL

    }

  )

  return(res)
}

filter_aux_inv <- function(inv,
                           changes_aux) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Fix year variable

  changes <- lapply(changes_aux, fix_year_var)

  # Row bind and select unique values from all aux files

  changes <- unique(rbindlist(changes, fill = TRUE))

  # Temporary fix to test data from Rossana

  max_year <- max(inv[!is.na(inv$surveyid_year),]$surveyid_year)

  changes <- changes[changes$surveyid_year<=max_year,]

  # Merge inventory with aux changes

  inv_aux  <- joyn::inner_join(inv, changes,
                              relationship = "many-to-one",
                              verbose = FALSE,
                              by = c("country_code", "surveyid_year"))

  # Choose last version (avoid message -> check issue of data.table)

  inv_aux <- suppressWarnings(last_ver_inv(inv_aux))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_aux)

}

only_labels <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nn  <- names(dt)

  for (j in seq_along(nn)) {

    ats       <- attributes(dt[[j]])
    atsn      <- names(ats)
    to_remove <- atsn[!grepl("label", atsn)]

    for (i in seq_along(to_remove)) {
      attr(dt[[j]], to_remove[i]) <- NULL
    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

survey_id_to_attr <- function(dt, survey_id) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Computations -------

  # Variables to attributes (Add them to package later in pipload)
  cnames <-
    c(
      "country_code",
      "surveyid_year",
      "survey_acronym",
      "vermast",
      "M",
      "veralt",
      "A",
      "collection",
      "module"
    )

  svy_id_attr <- tstrsplit(survey_id, "_",
            fixed=TRUE, names = cnames)

  attributes(dt)      <- c(attributes(dt), svy_id_attr)

  attr(dt, "tool")    <- ifelse(attributes(dt)$module == "ALL", "TB", "PC")
  attr(dt, "vermast") <- tolower(attributes(dt)$vermast)
  attr(dt, "veralt")  <- tolower(attributes(dt)$veralt)
  attr(dt, "surveyid_year") <- as.numeric(attributes(dt)$surveyid_year)
  attr(dt, "M")       <- NULL
  attr(dt, "A")       <- NULL

  # Add gd_type

  if("gd_type" %in% names(dt)){

    attr(dt, "gd_type") <- collapse::funique(dt$gd_type)

  }

  # Check year and surveyid_year is the same

  if("year" %in% names(dt)){

    year <- unique(dt$year)
    year <- purrr::discard(year, is.na)
    surveyid_year <- attributes(dt)$surveyid_year

    if(surveyid_year!=year){

      rlang::abort("Year variables in DLW survey different from survey_id year in inventory.
                     {cli::col_blue('Surveyid_year added to attributes')}",
                   class = c("piperr","yr_wrng"),
                   use_cli_format = TRUE)

    }
  }

  # Remove variables

  fnames <- c(grep("^(welf|weight|subnatid|edu)", names(dt), value = TRUE),
              "urban","area","age","male","literacy")

  to_keep <- names(dt)[names(dt) %in% fnames]

  dt <- dt[, ..to_keep]

  # Temporary fix for it to work on pip_class:

  mod <- attributes(dt)$module

  dt <- dt[, module := mod]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

fix_year_var <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Select variable names that contain the word "year"

  year_var <- grep("year", attributes(dt)$names, value = TRUE)

  # Temporal fix

  if(any(year_var %in% c("cpi_year"))){ # NEED TO FIX CPI

    setnames(dt, old = c("reporting_level", "survey_year", "survey_acronym"), new = c("survey_year", "survey_acronym", "reporting_level"))

    dt[, year := floor(as.numeric(survey_year))]

    year_var <- grep("year", attributes(dt)$names, value = TRUE)
  }

  if(length(year_var) > 1){

    if(any(year_var %in% c("year"))){

      year_var <-  "year"

    }else if(any(year_var %in% c("surveyid_year"))){

      year_var <-  "surveyid_year"

    }else{

      cli::cli_abort("The auxiliary keys has more than one variable related to `year` and none are `surveyid_year`")

      }

  }

  # Subset the data.table with the selected variables and make them unique

  selected_vars <- c("country_code",year_var)

  dt_selected <- unique(dt[, ..selected_vars])

  # Change name of year variable to match

  if(year_var != "surveyid_year"){

    names(dt_selected)[names(dt_selected) == year_var] <- "surveyid_year"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt_selected)

}
