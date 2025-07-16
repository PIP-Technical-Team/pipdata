valid_dlw_load <- function(inv,
                           measure = c("cpi", "ppp","pfw","pop"),
                           path = fs::path(Sys.getenv("PIP_ROOT_DIR"), "DLW-OUTPUT/")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if(!is.data.table(inv)){
    inv <- data.table::data.table(inv)
  }

  # Check what aux has changed
  changes_aux <- valid_aux_load(measure = measure, load = "inventory")

  inv_aux <- filter_aux_inv(changes_aux = changes_aux, inv = inv)

  # Create mock changes for the inventory (Temporal)
  inv_svy <- m_inv_filter(inv, seed = 1089) # For now is a mock function

  # Bind with inventory from aux changes
  inv_to_clean <- rbind(inv_svy, inv_aux, fill = TRUE)

  # Order alphabetically
  inv_to_clean <- inv_to_clean |>
    collapse::fmutate(file_qs = fs::path_file(pip_file_path))

  setorder(inv_to_clean, file_qs)

  # Load survey files
  n      <- length(inv_to_clean$file_qs)
  ls_svy <- lapply(1:n, \(x) qs::qread(fs::path(path, inv_to_clean$file_qs[x])))

  # Add data from inventory to data frame

  # poss_data_to_dt <- purrr::possibly(.f = data_to_dt,
  #                                    otherwise = NULL)

  ls <- purrr::map2(.x = ls_svy,
                    .y = as.list(inv_to_clean$survey_id),
                    .f = data_to_dt)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(ls)

}

data_to_dt <- function(dt, survey_id) {

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

  dt <- dt[,
    module := NULL
  ]

  return(dt)
}

filter_aux_inv <- function(inv,
                           changes_aux) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Temporary fix

  names(changes_aux)[names(changes_aux) == "year"] <- "surveyid_year"

  max_year <- max(inv[!is.na(inv$surveyid_year),c("surveyid_year")])

  changes_aux <- changes_aux[changes_aux$surveyid_year<=max_year,]

  # Merge inventory with aux changes

  inv_aux  <- merge(inv,
                    changes_aux[, c("country_code","surveyid_year")],
                    by = c("country_code", "surveyid_year"))

  # Choose last version

  inv_aux <- last_ver_inv(inv_aux)

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

  # Defenses -----------
  stopifnot(exprs = {
    data.table::is.data.table(dt)
  }
  )

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
    surveyid_year <- attributes(dt)$surveyid_year

    if(surveyid_year!=year){
      cli::cli_abort("Year variables in DLW survey different from survey_id year in inventory.
                     {cli::col_blue('Surveyid_year added to attributes')}",
                     class = c("piperr","yr_wrng"))
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
