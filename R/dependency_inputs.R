pd_canonical_projection <- function(x, keys, columns = names(x)) {
  dt <- data.table::as.data.table(data.table::copy(x))
  missing <- setdiff(c(keys, columns), names(dt))
  if (length(missing)) {
    rlang::abort(paste("Projection columns missing:", paste(missing, collapse = ", ")),
                 class = "pipdata_dependency_input_missing")
  }
  if (anyDuplicated(dt[, ..keys])) {
    rlang::abort("Duplicate semantic input keys.",
                 class = "pipdata_dependency_input_duplicate")
  }
  data.table::setorderv(dt, keys, na.last = TRUE)
  dt <- dt[, sort(columns), with = FALSE]
  list(data = dt, hash = pd_hash_object(dt))
}

pd_select_aux <- function(aux, measure, country, year = NULL,
                          survey_acronym = NULL, reporting_level = NULL) {
  dt <- data.table::as.data.table(data.table::copy(aux))
  country_col <- intersect(c("country_code", "country"), names(dt))[1L]
  if (!is.na(country_col)) dt <- dt[get(country_col) == country]
  if (!is.null(year) && "year" %in% names(dt)) {
    selected_year <- year
    dt <- dt[get("year") == selected_year]
  }
  if (!is.null(survey_acronym) && "survey_acronym" %in% names(dt)) {
    selected_survey_acronym <- survey_acronym
    dt <- dt[get("survey_acronym") == selected_survey_acronym]
  }
  if (!is.null(reporting_level) && "reporting_level" %in% names(dt)) {
    selected_reporting_level <- reporting_level
    dt <- dt[get("reporting_level") %in% selected_reporting_level]
  }
  keys <- intersect(c(country_col, "year", "survey_acronym", "reporting_level"), names(dt))
  if (!nrow(dt) && measure %in% c("cpi", "ppp", "pop")) {
    rlang::abort(paste("Missing required", measure, "projection."),
                 class = "pipdata_dependency_input_missing")
  }
  pd_canonical_projection(dt, keys = keys)
}

pd_normalize_metadata_keys <- function(metadata) {
  if (!is.list(metadata) && !is.null(names(metadata))) metadata <- as.list(metadata)
  if (!is.list(metadata) || is.null(names(metadata))) {
    rlang::abort("Metadata must be a named list.",
                 class = "pipdata_metadata_base_invalid")
  }
  names(metadata) <- tolower(trimws(names(metadata)))
  if (any(!nzchar(names(metadata))) || anyDuplicated(names(metadata))) {
    rlang::abort("Metadata keys must be unique non-empty canonical names.",
                 class = "pipdata_metadata_base_invalid")
  }
  metadata[order(names(metadata))]
}

expected_pip_ids <- function(inv_row, pfw_projection) {
  pfw <- data.table::as.data.table(pfw_projection)
  if (!nrow(pfw)) {
    rlang::abort("No usable PFW output mapping.", class = "pipdata_pfw_mapping_error")
  }
  if ("pip_id" %in% names(pfw)) return(sort(unique(toupper(pfw$pip_id))))
  required <- c("country_code", "year", "survey_acronym", "welfare_type")
  if (!all(required %in% names(pfw))) {
    rlang::abort("PFW cannot produce stable pip IDs.", class = "pipdata_pfw_mapping_error")
  }
  sort(unique(toupper(paste(pfw$country_code, pfw$year,
                            pfw$survey_acronym, pfw$welfare_type, sep = "_"))))
}
