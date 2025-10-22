inv_dlw_load <- function(inv) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Load survey files

  dt <- pipload::load_dlw_data(pin_name = inv$pins_folder, verbose = FALSE)

  # Add data from inventory to attributes of data table and add pip class

  dt <- data_to_dt(dt,inv$survey_id)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}

data_to_dt <- function(dt, survey_id) {


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

  return(dt)
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
  attr(dt, "survey_id") <- survey_id

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
