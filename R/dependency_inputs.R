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
  pd_expected_clean_pip_ids(inv_row, pfw_projection)
}

pd_dependency_key_adapter <- function(row, require_welfare = FALSE) {
  row <- data.table::as.data.table(data.table::copy(row))
  required <- c(
    "country_code", "surveyid_year", "survey_acronym", "module"
  )
  if (isTRUE(require_welfare)) {
    required <- c(required, "welfare_type", "pip_id")
  }
  if (nrow(row) != 1L || !all(required %in% names(row))) {
    rlang::abort(
      "A single row with exact dependency keys is required.",
      class = "pipdata_dependency_input_missing"
    )
  }
  values <- as.list(row[1L, ..required])
  invalid <- vapply(values, function(value) {
    length(value) != 1L || is.na(value) ||
      (is.character(value) && !nzchar(trimws(value)))
  }, logical(1))
  if (any(invalid)) {
    rlang::abort(
      "Dependency keys must be nonmissing scalar values.",
      class = "pipdata_dependency_input_missing"
    )
  }
  values$country_code <- toupper(as.character(values$country_code))
  values$surveyid_year <- as.integer(values$surveyid_year)
  values$survey_acronym <- toupper(as.character(values$survey_acronym))
  values$module <- toupper(as.character(values$module))
  if (isTRUE(require_welfare)) {
    values$welfare_type <- tolower(as.character(values$welfare_type))
    values$pip_id <- toupper(as.character(values$pip_id))
  }
  return(values)
}

pd_exact_pfw_projection <- function(row, pfw, pip_id = NULL) {
  keys <- pd_dependency_key_adapter(row, require_welfare = !is.null(pip_id))
  pfw <- data.table::as.data.table(data.table::copy(pfw))
  required <- c(
    "country_code", "surveyid_year", "survey_acronym", "welfare_type",
    "inpovcal", .DOMAIN_COLS
  )
  if (!all(required %in% names(pfw))) {
    rlang::abort(
      "PFW is missing exact output-mapping fields.",
      class = "pipdata_dependency_input_missing"
    )
  }
  selected <- pfw[
    toupper(country_code) == keys$country_code &
      surveyid_year == keys$surveyid_year &
      toupper(survey_acronym) == keys$survey_acronym & inpovcal == 1
  ]
  if (!nrow(selected)) {
    rlang::abort(
      "No exact PFW output mapping exists.",
      class = "pipdata_dependency_input_missing"
    )
  }
  selected[, welfare_type := tolower(as.character(welfare_type))]
  if (anyNA(selected$welfare_type) || any(!nzchar(selected$welfare_type)) ||
      anyDuplicated(selected$welfare_type)) {
    rlang::abort(
      "PFW output mappings are ambiguous by welfare type.",
      class = "pipdata_dependency_input_ambiguous"
    )
  }
  selected[, reporting_level := as.character(do.call(pmax, .SD))
           , .SDcols = .DOMAIN_COLS]
  mapped <- cache_id(cpfw = selected, att = keys)
  expected <- sort(toupper(names(mapped)))
  projection <- data.table::rbindlist(mapped, use.names = TRUE, fill = TRUE)
  projection <- pd_canonical_projection(
    projection,
    keys = c(
      "country_code", "surveyid_year", "survey_acronym", "welfare_type"
    )
  )$data
  if (!is.null(pip_id)) {
    wanted <- toupper(pip_id)
    projection <- projection[toupper(cache_id) == wanted]
    if (nrow(projection) != 1L || !identical(expected[expected == wanted], wanted)) {
      rlang::abort(
        "The PIP ID does not resolve to one exact PFW row.",
        class = "pipdata_dependency_input_ambiguous"
      )
    }
  }
  return(list(data = projection, expected_pip_ids = expected, keys = keys))
}

pd_expected_clean_pip_ids <- function(inv_row, pfw) {
  if (!nrow(data.table::as.data.table(pfw))) {
    rlang::abort(
      "No usable PFW output mapping.",
      class = "pipdata_pfw_mapping_error"
    )
  }
  projection <- pd_exact_pfw_projection(inv_row, pfw)
  expected <- projection$expected_pip_ids
  if (!length(expected)) {
    rlang::abort(
      "No usable PFW output mapping.",
      class = "pipdata_pfw_mapping_error"
    )
  }
  return(expected)
}

pd_resolve_data_levels <- function(pfw_row) {
  pfw_row <- data.table::as.data.table(data.table::copy(pfw_row))
  measures <- c("ppp", "cpi", "gdp", "pce", "pop")
  domains <- paste0(measures, "_domain")
  if (nrow(pfw_row) != 1L || !all(domains %in% names(pfw_row)) ||
      anyNA(pfw_row[, ..domains])) {
    rlang::abort(
      "PFW lacks exact auxiliary domain values.",
      class = c("pipdata_dependency_input_missing", "piperr", "dom_var")
    )
  }
  values <- vapply(domains, function(domain) {
    as.integer(pfw_row[[domain]][[1L]])
  }, integer(1))
  names(values) <- measures
  if (any(!values %in% c(1L, 2L))) {
    rlang::abort(
      "PFW has unsupported auxiliary domain values.",
      class = "pipdata_dependency_input_ambiguous"
    )
  }
  domain_vars <- c("cpi_domain_var", "ppp_domain_var")
  has_domain_vars <- all(domain_vars %in% names(pfw_row)) &&
    !anyNA(pfw_row[, ..domain_vars])
  vars <- if (has_domain_vars) {
    tolower(vapply(domain_vars, function(name) {
      as.character(pfw_row[[name]][[1L]])
    }, character(1)))
  } else {
    character()
  }
  reporting_level <- if (
    "reporting_level" %in% names(pfw_row) &&
      length(pfw_row$reporting_level) == 1L &&
      !is.na(pfw_row$reporting_level)
  ) {
    as.integer(pfw_row$reporting_level[[1L]])
  } else {
    max(values)
  }
  same_reporting_level <- all(values == reporting_level)
  mismatched_vars <- same_reporting_level && length(vars) == 2L &&
    !identical(vars[[1L]], vars[[2L]])
  invalid_subnational_vars <- any(values[c("cpi", "ppp")] == 2L) &&
    (length(vars) != 2L || !all(vars == "urban"))
  if (mismatched_vars || invalid_subnational_vars) {
    rlang::abort(
      "CPI and PPP domain variables do not agree on the supported domain.",
      class = c(
        "pipdata_dependency_domain_mismatch",
        "pipdata_dependency_input_ambiguous",
        "piperr",
        "cpi_ppp_var"
      )
    )
  }
  data_levels <- ifelse(values == 1L, "national", "area")
  return(list(
    data_levels = data_levels,
    aux_data_levels = if (same_reporting_level) "same" else "different"
  ))
}

pd_measure_data_level <- function(measure, pfw_row) {
  resolved <- pd_resolve_data_levels(pfw_row)$data_levels
  if (!measure %in% names(resolved)) {
    rlang::abort(
      paste("Unsupported auxiliary domain measure:", measure),
      class = "pipdata_dependency_input_ambiguous"
    )
  }
  return(unname(resolved[[measure]]))
}

pd_aux_component_projection <- function(measure, aux, keys, pfw_row) {
  required <- switch(
    measure,
    cpi = c(
      "country_code", "year", "survey_acronym", "cpi_year",
      "reporting_level", "cpi_value"
    ),
    ppp = c(
      "country_code", "ppp_year", "release_version",
      "adaptation_version", "reporting_level", "ppp"
    ),
    pop = c("country_code", "year", "reporting_level", "pop"),
    gdp = c("country_code", "year", "reporting_level", "gdp"),
    pce = c("country_code", "year", "reporting_level", "pce"),
    character()
  )
  aux <- data.table::as.data.table(data.table::copy(aux))
  if (!length(required) || !all(required %in% names(aux))) {
    rlang::abort(
      paste("Auxiliary", measure, "data lack exact projection fields."),
      class = "pipdata_dependency_input_missing"
    )
  }
  filtered <- filter_aux_data(measure, aux, keys)
  if (!nrow(filtered)) {
    rlang::abort(
      paste("Missing exact", measure, "projection."),
      class = "pipdata_dependency_input_missing"
    )
  }
  value <- create_attr(measure, filtered)
  if (is.null(names(value)) || anyNA(names(value)) ||
      any(!nzchar(names(value))) || anyDuplicated(names(value))) {
    rlang::abort(
      paste("Ambiguous exact", measure, "projection."),
      class = "pipdata_dependency_input_ambiguous"
    )
  }
  value <- value[order(names(value))]
  data_level <- pd_measure_data_level(measure, pfw_row)
  projection <- list(data_level = data_level, value = value)
  projection$hash <- pd_hash_object(projection)
  return(projection)
}

pd_build_input_rows <- function(stage, entity_id, components) {
  components <- data.table::as.data.table(data.table::copy(components))
  required <- c("name", "version_id", "content_hash")
  allowed <- list(
    clean = c("dlw", "pfw"),
    metadata = c(
      "clean_data", "aux_cpi", "aux_ppp", "aux_pop", "aux_gdp",
      "aux_pce"
    ),
    deflate = c(
      "clean_data", "metadata", "aux_cpi", "aux_ppp", "aux_pop"
    )
  )
  nonblank <- function(values) {
    is.character(values) && !anyNA(values) && all(nzchar(trimws(values)))
  }
  if (!stage %in% names(allowed) || !is.character(entity_id) ||
      length(entity_id) != 1L || is.na(entity_id) || !nzchar(entity_id) ||
      !all(required %in% names(components)) || !nrow(components) ||
      any(components$name == "canonical") ||
      any(!components$name %in% allowed[[stage]]) ||
      anyDuplicated(components$name) || !nonblank(components$name) ||
      !nonblank(components$version_id) || !nonblank(components$content_hash)) {
    rlang::abort(
      "Named dependency input components are invalid.",
      class = "pipdata_dependency_input_invalid"
    )
  }
  components <- components[, ..required]
  data.table::setorder(components, name)
  canonical <- data.table::data.table(
    name = "canonical",
    version_id = pd_hash_object(components[, .(name, version_id)]),
    content_hash = pd_hash_object(components[, .(name, content_hash)])
  )
  rows <- data.table::rbindlist(list(canonical, components), use.names = TRUE)
  rows[, `:=`(stage = ..stage, entity_id = ..entity_id)]
  data.table::setcolorder(
    rows, c("stage", "entity_id", "name", "version_id", "content_hash")
  )
  data.table::setorder(rows, name)
  return(rows)
}

pd_aux_catalog_version <- function(snapshot, measure) {
  catalog <- data.table::as.data.table(snapshot$aux$catalog)
  selected_measure <- tolower(measure)
  row <- catalog[which(tolower(catalog$measure) == selected_measure)]
  if (nrow(row) != 1L ||
      !all(c("version_id", "content_hash") %in% names(row)) ||
      anyNA(row[, .(version_id, content_hash)]) ||
      any(!nzchar(c(row$version_id, row$content_hash)))) {
    rlang::abort(
      paste("Auxiliary artifact is not exact for", measure),
      class = "pipdata_dependency_input_ambiguous"
    )
  }
  return(as.list(row[1L, .(version_id, content_hash)]))
}

pd_legacy_entity_aux_hash <- function(snapshot, row, measures) {
  row <- as.list(data.table::as.data.table(row)[1L])
  projections <- lapply(measures, function(measure) {
    aux <- snapshot$aux$objects[[measure]]
    country <- row$country_code %||% row$country %||%
      substr(row$pip_id %||% "", 1L, 3L)
    year <- row$year %||% suppressWarnings(
      as.integer(strsplit(row$pip_id %||% "", "_")[[1L]][2L])
    )
    tryCatch(
      pd_select_aux(
        aux, measure, country, year, row$survey_acronym %||% NULL,
        row$reporting_level %||% NULL
      )$hash,
      error = function(e) pd_hash_object(aux)
    )
  })
  return(stats::setNames(unlist(projections), measures))
}

pd_legacy_input_hash <- function(snapshot, row, stage, measures) {
  row_list <- as.list(data.table::as.data.table(row)[1L])
  aux_hash <- pd_legacy_entity_aux_hash(snapshot, row, measures)
  input <- switch(
    stage,
    clean = list(row_list, aux_hash),
    metadata = list(
      row_list$version_id_data, row_list$content_hash_data, aux_hash
    ),
    deflate = list(
      row_list$version_id_data, row_list$content_hash_data,
      row_list$version_id_metadata, row_list$content_hash_metadata, aux_hash
    )
  )
  return(pd_hash_object(input))
}

pd_legacy_input_version <- function(row, stage, input_hash) {
  row <- as.list(data.table::as.data.table(row)[1L])
  version <- switch(
    stage,
    clean = input_hash,
    metadata = row$version_id_data,
    deflate = pd_hash_object(list(
      row$version_id_data, row$version_id_metadata
    )),
    NULL
  )
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
      !nzchar(version)) {
    rlang::abort(
      "Legacy canonical input version is incomplete.",
      class = "pipdata_dependency_input_missing"
    )
  }
  return(version)
}

pd_entity_input_state <- function(snapshot, row, stage, measures) {
  row_dt <- data.table::as.data.table(data.table::copy(row))
  row_list <- as.list(row_dt[1L])
  require_welfare <- !identical(stage, "clean")
  keys <- pd_dependency_key_adapter(row_dt, require_welfare)
  pfw <- pd_exact_pfw_projection(
    row_dt, snapshot$aux$objects$pfw,
    if (require_welfare) keys$pip_id else NULL
  )
  entity_id <- if (identical(stage, "clean")) {
    as.character(row_list$survey_id)
  } else {
    keys$pip_id
  }
  components <- data.table::data.table(
    name = character(), version_id = character(), content_hash = character()
  )
  aux_projection <- list()
  if (identical(stage, "clean")) {
    dlw_version <- row_list$latest_version_id %||% NA_character_
    dlw_hash <- row_list$content_hash %||% NA_character_
    pfw_receipt <- pd_aux_catalog_version(snapshot, "pfw")
    components <- data.table::data.table(
      name = c("dlw", "pfw"),
      version_id = c(dlw_version, pfw_receipt$version_id),
      content_hash = c(dlw_hash, pd_hash_object(pfw$data))
    )
  } else {
    upstream <- if (identical(stage, "metadata")) {
      data.table::data.table(
        name = "clean_data", version_id = row_list$version_id_data,
        content_hash = row_list$content_hash_data
      )
    } else {
      data.table::data.table(
        name = c("clean_data", "metadata"),
        version_id = c(
          row_list$version_id_data, row_list$version_id_metadata
        ),
        content_hash = c(
          row_list$content_hash_data, row_list$content_hash_metadata
        )
      )
    }
    auxiliary <- lapply(measures, function(measure) {
      projection <- pd_aux_component_projection(
        measure, snapshot$aux$objects[[measure]], keys, pfw$data
      )
      aux_projection[[measure]] <<- projection$value
      receipt <- pd_aux_catalog_version(snapshot, measure)
      data.table::data.table(
        name = paste0("aux_", measure),
        version_id = receipt$version_id,
        content_hash = projection$hash
      )
    })
    components <- data.table::rbindlist(c(list(upstream), auxiliary))
  }
  input_rows <- pd_build_input_rows(stage, entity_id, components)
  legacy_input_hash <- pd_legacy_input_hash(snapshot, row_dt, stage, measures)
  return(list(
    input_rows = input_rows,
    input_hash = input_rows[name == "canonical", content_hash],
    legacy_input_hash = legacy_input_hash,
    legacy_input_version = pd_legacy_input_version(
      row_dt, stage, legacy_input_hash
    ),
    expected_pip_ids = pfw$expected_pip_ids,
    aux_projection = aux_projection
  ))
}

pd_input_change_reason <- function(stage, name) {
  if (identical(name, "clean_data") || identical(name, "metadata")) {
    return("upstream_output_changed")
  }
  reasons <- c(
    dlw = "dlw_changed", pfw = "pfw_changed",
    aux_cpi = "aux_cpi_changed", aux_ppp = "aux_ppp_changed",
    aux_pop = "aux_pop_changed", aux_gdp = "aux_gdp_changed",
    aux_pce = "aux_pce_changed"
  )
  reason <- unname(reasons[[name]])
  if (is.null(reason)) {
    rlang::abort(
      paste("No input reason is defined for", stage, name),
      class = "pipdata_dependency_input_invalid"
    )
  }
  return(reason)
}

pd_validate_expected_pip_ids <- function(expected_pip_ids) {
  expected <- as.character(expected_pip_ids)
  if (!length(expected) || anyNA(expected) || any(!nzchar(expected)) ||
      anyDuplicated(expected)) {
    rlang::abort(
      "Accepted expected PIP IDs are invalid.",
      class = "pipdata_clean_output_incomplete"
    )
  }
  return(sort(expected))
}

pd_assert_clean_output_set <- function(expected_pip_ids, clean, metadata) {
  expected <- pd_validate_expected_pip_ids(expected_pip_ids)
  clean_ids <- sort(names(clean) %||% character())
  metadata_ids <- sort(names(metadata) %||% character())
  if (anyDuplicated(clean_ids) || anyDuplicated(metadata_ids) ||
      !identical(clean_ids, expected) || !identical(metadata_ids, expected)) {
    rlang::abort(
      "Worker output differs from the accepted expected PIP IDs.",
      class = "pipdata_clean_output_incomplete"
    )
  }
  return(expected)
}

pd_clean_receipt_set <- function(receipts, expected_pip_ids = NULL) {
  if (is.data.frame(receipts)) {
    rows <- data.table::as.data.table(data.table::copy(receipts))
  } else if (is.list(receipts)) {
    rows <- data.table::rbindlist(receipts, fill = TRUE)
  } else {
    rows <- data.table::data.table()
  }
  required <- c(
    "pip_id", "alias", "artifact", "path", "version_id", "content_hash"
  )
  if (!all(required %in% names(rows)) || !nrow(rows) ||
      anyNA(rows[, ..required]) ||
      any(vapply(rows[, ..required], function(x) {
        !is.character(x) || any(!nzchar(trimws(x)))
      }, logical(1))) || anyDuplicated(rows$pip_id) ||
      any(rows$pip_id != rows$artifact) ||
      ("success" %in% names(rows) && any(!rows$success))) {
    rlang::abort(
      "The clean receipt set is incomplete.",
      class = "pipdata_clean_output_incomplete"
    )
  }
  if (!is.null(expected_pip_ids) &&
      !identical(sort(rows$pip_id),
                 pd_validate_expected_pip_ids(expected_pip_ids))) {
    rlang::abort(
      "The clean receipt set differs from accepted expected PIP IDs.",
      class = "pipdata_clean_output_incomplete"
    )
  }
  tuples <- rows[, ..required]
  data.table::setorder(tuples, pip_id)
  return(list(
    receipts = tuples,
    output_version_id = pd_hash_object(
      tuples[, .(pip_id, alias, artifact, path, version_id)]
    ),
    output_hash = pd_hash_object(tuples)
  ))
}
