.dlw_acquisition_modules <- c(
  "GPWG", "GROUP", "BIN", "HIST", "ALL", "ASPIRE", "L"
)

.dlw_acquisition_active_modules <- c(
  "GPWG", "GROUP", "BIN", "HIST", "ALL"
)

.dlw_acquisition_catalog_columns <- c(
  "Country", "Year", "Survey_acronym", "Vermast", "Veralt", "Module",
  "Collection", "FileName", "Checksum", "Ext"
)

.abort_dlw_acquisition_schema <- function(message) {
  rlang::abort(
    message,
    class = c("pipdata_dlw_catalog_schema_error", "piperr")
  )
}

.dlw_normalized_filename <- function(x) {
  toupper(fs::path_file(x))
}

.dlw_survey_id <- function(x) {
  fs::path_ext_remove(fs::path_file(x))
}

.normalize_dlw_acquisition_catalog <- function(
    x,
    source = c("server", "local")
) {
  source <- match.arg(source)
  required <- .dlw_acquisition_catalog_columns
  if (identical(source, "local")) {
    required <- c(required, "data_available")
  }
  if (!is.data.frame(x)) {
    .abort_dlw_acquisition_schema("The acquisition catalog must be tabular.")
  }

  catalog <- data.table::as.data.table(data.table::copy(x))
  missing_columns <- setdiff(required, names(catalog))
  if (length(missing_columns) > 0L) {
    .abort_dlw_acquisition_schema(paste0(
      "Acquisition catalog is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    ))
  }

  normalize_character <- function(column, name) {
    if (is.factor(column)) {
      column <- as.character(column)
    }
    if (!is.character(column)) {
      .abort_dlw_acquisition_schema(paste0(
        "Acquisition catalog column `", name, "` must be character."
      ))
    }
    trimws(column)
  }

  catalog[, Module := normalize_character(Module, "Module")]
  catalog[, Ext := tolower(normalize_character(Ext, "Ext"))]
  if (anyNA(catalog$Module) || any(!nzchar(catalog$Module)) ||
      anyNA(catalog$Ext) || any(!nzchar(catalog$Ext))) {
    .abort_dlw_acquisition_schema(
      "Acquisition catalog module and extension values cannot be empty."
    )
  }
  if (identical(source, "server")) {
    catalog <- catalog[
      Module %in% .dlw_acquisition_modules & Ext == "dta"
    ]
  } else if (any(!catalog$Module %in% .dlw_acquisition_modules) ||
      any(catalog$Ext != "dta")) {
    .abort_dlw_acquisition_schema(
      "Local acquisition rows require a supported module and `dta` extension."
    )
  }

  character_columns <- c(
    "Country", "Survey_acronym", "Vermast", "Veralt", "Module",
    "Collection", "FileName", "Checksum", "Ext"
  )
  if (identical(source, "local")) {
    character_columns <- c(character_columns, "data_available")
  }
  for (column in character_columns) {
    data.table::set(
      catalog,
      j = column,
      value = normalize_character(catalog[[column]], column)
    )
  }

  year <- catalog[["Year"]]
  if (is.factor(year)) {
    year <- as.character(year)
  }
  if (!is.integer(year) && !is.numeric(year) && !is.character(year)) {
    .abort_dlw_acquisition_schema(
      "Acquisition catalog column `Year` cannot be safely coerced."
    )
  }
  numeric_year <- suppressWarnings(as.numeric(year))
  year_valid <- !is.na(numeric_year) & is.finite(numeric_year) &
    numeric_year == floor(numeric_year) & numeric_year >= 0 &
    numeric_year <= .Machine$integer.max
  if (is.character(year)) {
    year_valid <- year_valid & !is.na(year) & nzchar(trimws(year))
  }
  if (!all(year_valid)) {
    .abort_dlw_acquisition_schema(
      "Acquisition catalog column `Year` must contain whole numbers."
    )
  }
  catalog[, Year := as.integer(numeric_year)]

  nonempty_columns <- setdiff(character_columns, "Ext")
  malformed <- vapply(nonempty_columns, function(column) {
    anyNA(catalog[[column]]) || any(!nzchar(catalog[[column]]))
  }, logical(1))
  if (any(malformed)) {
    .abort_dlw_acquisition_schema(paste0(
      "Acquisition catalog has missing or empty values in: ",
      paste(nonempty_columns[malformed], collapse = ", "),
      "."
    ))
  }
  if (anyNA(catalog$Ext) || any(catalog$Ext != "dta")) {
    .abort_dlw_acquisition_schema(
      "Acquisition catalog column `Ext` must equal `dta`."
    )
  }
  if (any(tolower(fs::path_ext(catalog$FileName)) != "dta")) {
    .abort_dlw_acquisition_schema(
      "Acquisition catalog `FileName` values must be `.dta` files."
    )
  }
  if (identical(source, "local") &&
      any(!catalog$data_available %in% c("Yes", "No"))) {
    .abort_dlw_acquisition_schema(
      "Local acquisition availability must be `Yes` or `No`."
    )
  }

  catalog <- unique(catalog)
  normalized_filename <- .dlw_normalized_filename(catalog$FileName)
  checksum_count <- vapply(
    split(catalog$Checksum, normalized_filename),
    function(value) length(unique(value)),
    integer(1)
  )
  if (any(checksum_count != 1L)) {
    .abort_dlw_acquisition_schema(
      "Each normalized acquisition filename must have one checksum."
    )
  }
  if (anyDuplicated(normalized_filename)) {
    .abort_dlw_acquisition_schema(
      "Each normalized acquisition filename must have one catalog row."
    )
  }

  remaining <- sort(setdiff(
    names(catalog),
    c(.dlw_acquisition_catalog_columns, "data_available")
  ))
  last <- if (identical(source, "local")) "data_available" else character()
  data.table::setcolorder(
    catalog,
    c(.dlw_acquisition_catalog_columns, remaining, last)
  )
  if (nrow(catalog) > 0L) {
    catalog[, `..dlw_order` := .dlw_normalized_filename(FileName)]
    data.table::setorderv(catalog, c("..dlw_order", "Checksum"))
    catalog[, `..dlw_order` := NULL]
  }
  return(catalog[])
}

.load_dlw_acquisition_inventory <- function(id, verbose = FALSE) {
  inventory <- pipload::pip_read(
    id = id,
    alias = "dlw_inv",
    verbose = verbose
  )
  .normalize_dlw_acquisition_catalog(inventory, source = "local")
}

.load_dlw_acquisition_server_catalog <- function() {
  catalog <- dlw::dlw_server_catalog()
  if (is.null(catalog)) {
    rlang::abort(
      "Failed to download the current GMD catalog.",
      class = c("pipdata_dlw_catalog_load_error", "piperr")
    )
  }
  catalog <- .normalize_dlw_acquisition_catalog(catalog, source = "server")
  if (nrow(catalog) == 0L) {
    rlang::abort(
      "The current GMD server catalog contains no supported datasets.",
      class = c("pipdata_dlw_catalog_load_error", "piperr")
    )
  }
  catalog
}

.select_dlw_acquisition_candidates <- function(
    server,
    prior,
    check_missing,
    modules = .dlw_acquisition_active_modules
) {
  server <- .normalize_dlw_acquisition_catalog(server, source = "server")
  if (is.null(prior)) {
    prior <- server[0L]
    prior[, data_available := character()]
    prior <- .normalize_dlw_acquisition_catalog(prior, source = "local")
  } else {
    prior <- .normalize_dlw_acquisition_catalog(prior, source = "local")
  }
  if (!is.logical(check_missing) || length(check_missing) != 1L ||
      is.na(check_missing)) {
    .abort_dlw_acquisition_schema(
      "Candidate selection requires one non-missing logical value."
    )
  }

  server_key <- paste(
    .dlw_normalized_filename(server$FileName),
    server$Checksum,
    sep = "\r"
  )
  prior_key <- paste(
    .dlw_normalized_filename(prior$FileName),
    prior$Checksum,
    sep = "\r"
  )
  prior_index <- match(server_key, prior_key)
  prior_status <- prior$data_available[prior_index]
  selected <- server$Module %in% modules & (
    is.na(prior_index) |
      (identical(check_missing, TRUE) & prior_status == "No")
  )
  candidates <- data.table::copy(server[selected])
  candidates[, data_available := "No"]
  .normalize_dlw_acquisition_catalog(candidates, source = "local")
}

.merge_dlw_acquisition_inventory <- function(
    server,
    prior,
    worker_results
) {
  server <- .normalize_dlw_acquisition_catalog(server, source = "server")
  if (is.null(prior)) {
    prior <- data.table::copy(server[0L])
    prior[, data_available := character()]
  }
  prior <- .normalize_dlw_acquisition_catalog(prior, source = "local")
  worker_results <- data.table::as.data.table(data.table::copy(worker_results))
  required_worker_columns <- c("FileName", "data_available")
  if (!all(required_worker_columns %in% names(worker_results)) ||
      !is.character(worker_results$FileName) ||
      !is.character(worker_results$data_available) ||
      anyNA(worker_results[, required_worker_columns, with = FALSE]) ||
      any(!nzchar(worker_results$FileName)) ||
      any(!worker_results$data_available %in% c("Yes", "No"))) {
    .abort_dlw_acquisition_schema(
      "Acquisition worker results have an invalid merge schema."
    )
  }
  worker_filename <- .dlw_normalized_filename(worker_results$FileName)
  if (anyDuplicated(worker_filename)) {
    .abort_dlw_acquisition_schema(
      "Acquisition worker results contain duplicate filenames."
    )
  }

  server_key <- paste(
    .dlw_normalized_filename(server$FileName),
    server$Checksum,
    sep = "\r"
  )
  prior_key <- paste(
    .dlw_normalized_filename(prior$FileName),
    prior$Checksum,
    sep = "\r"
  )
  prior_index <- match(server_key, prior_key)
  prior_status <- prior$data_available[prior_index]
  result_status <- rep("No", nrow(server))
  result_status[!is.na(prior_index) & prior_status == "Yes"] <- "Yes"

  worker_index <- match(
    .dlw_normalized_filename(server$FileName),
    worker_filename
  )
  attempted <- !is.na(worker_index)
  result_status[attempted] <- worker_results$data_available[worker_index[attempted]]

  active <- server$Module %in% .dlw_acquisition_active_modules
  compatible <- server$Module %in% c("ASPIRE", "L") &
    !is.na(prior_index) & prior_status == "Yes"
  intended <- data.table::copy(server[active | compatible])
  intended[, data_available := result_status[active | compatible]]
  .normalize_dlw_acquisition_catalog(intended, source = "local")
}

.dlw_validation_core_columns <- c(
  "survey_id", "pipeline_version", "latest_version_id", "content_hash",
  "file_path", "status", "data_available", "date_validated", "Checksum"
)

.dlw_validation_parsed_columns <- c(
  "country_code", "surveyid_year", "survey_acronym", "vermast", "veralt",
  "collection", "module", "tool"
)

.dlw_validation_inventory_columns <- c(
  .dlw_validation_core_columns,
  .dlw_validation_parsed_columns
)

.abort_dlw_validation_inventory_schema <- function(message) {
  rlang::abort(
    message,
    class = c("pipdata_dlw_inventory_schema_error", "piperr")
  )
}

.empty_dlw_validation_inventory <- function() {
  data.table::data.table(
    survey_id = character(),
    pipeline_version = integer(),
    latest_version_id = character(),
    content_hash = character(),
    file_path = character(),
    status = character(),
    data_available = character(),
    date_validated = as.POSIXct(character(), tz = "UTC"),
    Checksum = character(),
    country_code = character(),
    surveyid_year = integer(),
    survey_acronym = character(),
    vermast = character(),
    veralt = character(),
    collection = character(),
    module = character(),
    tool = character()
  )
}

.normalize_dlw_validation_inventory <- function(
    x,
    allow_schema_light_empty = FALSE
) {
  if (!is.data.frame(x)) {
    .abort_dlw_validation_inventory_schema(
      "The validation inventory must be tabular."
    )
  }
  inventory <- data.table::as.data.table(data.table::copy(x))
  if (nrow(inventory) == 0L) {
    if (isTRUE(allow_schema_light_empty)) {
      return(.empty_dlw_validation_inventory())
    }
    expected <- .empty_dlw_validation_inventory()
    missing_columns <- setdiff(names(expected), names(inventory))
    compatible <- length(missing_columns) == 0L && all(vapply(
      names(expected),
      function(column) {
        if (identical(column, "date_validated")) {
          return(inherits(inventory[[column]], "POSIXct"))
        }
        identical(typeof(inventory[[column]]), typeof(expected[[column]]))
      },
      logical(1)
    ))
    if (!compatible) {
      .abort_dlw_validation_inventory_schema(
        "An empty durable validation inventory must retain the full typed schema."
      )
    }
    return(.empty_dlw_validation_inventory())
  }

  minimum <- c("survey_id", "status", "data_available")
  missing_minimum <- setdiff(minimum, names(inventory))
  if (length(missing_minimum) > 0L) {
    .abort_dlw_validation_inventory_schema(paste0(
      "Validation inventory is missing required state columns: ",
      paste(missing_minimum, collapse = ", "),
      "."
    ))
  }
  normalize_character <- function(value, name, allow_empty = FALSE) {
    if (is.factor(value)) {
      value <- as.character(value)
    }
    if (!is.character(value)) {
      .abort_dlw_validation_inventory_schema(paste0(
        "Validation inventory column `", name, "` must be character."
      ))
    }
    value <- trimws(value)
    if (!allow_empty && (anyNA(value) || any(!nzchar(value)))) {
      .abort_dlw_validation_inventory_schema(paste0(
        "Validation inventory column `", name, "` cannot be empty."
      ))
    }
    value
  }
  inventory[, survey_id := normalize_character(survey_id, "survey_id")]
  inventory[, status := normalize_character(status, "status", TRUE)]
  inventory[, data_available := normalize_character(
    data_available,
    "data_available",
    TRUE
  )]
  if (anyNA(inventory$status) || anyNA(inventory$data_available)) {
    .abort_dlw_validation_inventory_schema(
      "Validation inventory status and availability cannot be missing."
    )
  }

  retry <- inventory$status == "" & inventory$data_available == "No"
  artifact_columns <- c("latest_version_id", "content_hash", "file_path")
  retry_metadata_empty <- rep(TRUE, nrow(inventory))
  for (column in intersect(artifact_columns, names(inventory))) {
    value <- inventory[[column]]
    if (is.factor(value)) {
      value <- as.character(value)
    }
    if (!is.character(value)) {
      .abort_dlw_validation_inventory_schema(paste0(
        "Validation inventory column `", column, "` must be character."
      ))
    }
    retry_metadata_empty <- retry_metadata_empty &
      (is.na(value) | !nzchar(trimws(value)))
  }
  if (any(retry & !retry_metadata_empty)) {
    .abort_dlw_validation_inventory_schema(
      "Legacy validation retry rows cannot contain artifact metadata."
    )
  }

  completed <- inventory$status %in% c("valid", "invalid") &
    inventory$data_available == "Yes"
  if (any(!retry & !completed)) {
    .abort_dlw_validation_inventory_schema(
      "Validation inventory contains an unknown completed or retry state."
    )
  }
  inventory <- inventory[completed]
  if (nrow(inventory) == 0L) {
    return(.empty_dlw_validation_inventory())
  }

  missing_columns <- setdiff(.dlw_validation_inventory_columns, names(inventory))
  if (length(missing_columns) > 0L) {
    .abort_dlw_validation_inventory_schema(paste0(
      "Completed validation rows are missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    ))
  }
  character_columns <- setdiff(
    .dlw_validation_inventory_columns,
    c("pipeline_version", "date_validated", "surveyid_year")
  )
  for (column in character_columns) {
    data.table::set(
      inventory,
      j = column,
      value = normalize_character(inventory[[column]], column)
    )
  }
  normalize_integer <- function(value, name, positive = FALSE) {
    if (!is.integer(value) && !is.numeric(value)) {
      .abort_dlw_validation_inventory_schema(paste0(
        "Validation inventory column `", name,
        "` must contain whole numbers."
      ))
    }
    minimum_value <- if (positive) 1 else 0
    valid <- !is.na(value) & is.finite(value) & value == floor(value) &
      value >= minimum_value & value <= .Machine$integer.max
    if (!all(valid)) {
      .abort_dlw_validation_inventory_schema(paste0(
        "Validation inventory column `", name,
        "` must contain valid whole numbers."
      ))
    }
    as.integer(value)
  }
  inventory[, pipeline_version := normalize_integer(
    pipeline_version,
    "pipeline_version",
    TRUE
  )]
  inventory[, surveyid_year := normalize_integer(
    surveyid_year,
    "surveyid_year"
  )]

  date_value <- inventory$date_validated
  if (inherits(date_value, "Date")) {
    date_value <- as.POSIXct(date_value, tz = "UTC")
  } else if (inherits(date_value, "POSIXlt")) {
    date_value <- as.POSIXct(date_value, tz = "UTC")
  } else if (inherits(date_value, "POSIXct")) {
    date_value <- as.POSIXct(
      as.numeric(date_value),
      origin = "1970-01-01",
      tz = "UTC"
    )
  } else {
    .abort_dlw_validation_inventory_schema(
      "Validation inventory `date_validated` must be Date or POSIX time."
    )
  }
  if (anyNA(date_value)) {
    .abort_dlw_validation_inventory_schema(
      "Validation inventory `date_validated` cannot be missing."
    )
  }
  inventory[, date_validated := date_value]

  parsed <- tryCatch(
    pipload::survey_id_to_vars(data.table::data.table(
      survey_id = unique(inventory$survey_id)
    )),
    error = function(e) e
  )
  parsed_required <- c("survey_id", .dlw_validation_parsed_columns)
  if (inherits(parsed, "condition") || !is.data.frame(parsed) ||
      !all(parsed_required %in% names(parsed))) {
    .abort_dlw_validation_inventory_schema(
      "Completed validation survey IDs could not be parsed."
    )
  }
  parsed <- data.table::as.data.table(data.table::copy(parsed))
  parsed[, surveyid_year := normalize_integer(surveyid_year, "surveyid_year")]
  for (column in setdiff(.dlw_validation_parsed_columns, "surveyid_year")) {
    data.table::set(
      parsed,
      j = column,
      value = normalize_character(parsed[[column]], column)
    )
  }
  parsed <- parsed[match(inventory$survey_id, parsed$survey_id)]
  parsed_valid <- !anyNA(parsed$survey_id)
  if (parsed_valid) {
    parsed_valid <- all(vapply(.dlw_validation_parsed_columns, function(column) {
      identical(inventory[[column]], parsed[[column]])
    }, logical(1)))
  }
  if (!parsed_valid) {
    .abort_dlw_validation_inventory_schema(
      "Completed validation parsed fields do not match `survey_id`."
    )
  }

  inventory <- inventory[, .dlw_validation_inventory_columns, with = FALSE]
  data.table::setorderv(inventory, c("survey_id", "pipeline_version"))
  inventory[]
}

.dlw_validation_file_id <- function(id) {
  extension <- fs::path_ext(id)
  if (is.na(extension) || !nzchar(extension)) {
    return(fs::path_ext_set(id, "qs2"))
  }
  id
}

.is_valid_dlw_version_catalog <- function(x) {
  is.data.frame(x) && "version_id" %in% names(x) &&
    is.character(x$version_id) && !anyNA(x$version_id) &&
    all(nzchar(x$version_id)) && identical(x$version_id, trimws(x$version_id))
}

.strict_dlw_versions <- function(path, alias) {
  catalog_warning <- NULL
  versions <- tryCatch(
    withCallingHandlers(
      stamp::st_versions(path, alias = alias),
      warning = function(w) {
        catalog_warning <<- w
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (!is.null(catalog_warning)) {
    rlang::abort(
      "The DLW version catalog emitted a corruption warning.",
      class = c("pipdata_dlw_version_catalog_error", "piperr"),
      parent = catalog_warning
    )
  }
  if (inherits(versions, "condition")) {
    rlang::cnd_signal(versions)
  }
  if (!.is_valid_dlw_version_catalog(versions) ||
      anyDuplicated(versions$version_id)) {
    rlang::abort(
      "The DLW version catalog is malformed or contains duplicate IDs.",
      class = c("pipdata_dlw_version_catalog_error", "piperr")
    )
  }
  versions
}

.abort_dlw_validation_history_load <- function(message, parent = NULL) {
  rlang::abort(
    message,
    class = c("pipdata_dlw_history_load_error", "piperr"),
    parent = parent
  )
}

.scan_dlw_validation_history <- function(verbose = FALSE) {
  versions <- tryCatch(
    .strict_dlw_versions(
      .dlw_validation_file_id("gmd_valid_inv"),
      "dlw_meta"
    ),
    error = function(e) e
  )
  if (inherits(versions, "condition")) {
    .abort_dlw_validation_history_load(
      "Validation inventory history could not be listed.",
      parent = versions
    )
  }
  version_ids <- unique(versions$version_id)
  if (length(version_ids) == 0L) {
    return(data.table::data.table(
      survey_id = character(), pipeline_version = integer()
    ))
  }
  maxima <- data.table::data.table(
    survey_id = character(), pipeline_version = integer()
  )
  for (version_id in version_ids) {
    value <- tryCatch(
      pipload::pip_read(
        id = "gmd_valid_inv",
        version = version_id,
        alias = "dlw_meta",
        verbose = verbose
      ),
      error = function(e) e
    )
    if (inherits(value, "condition")) {
      .abort_dlw_validation_history_load(
        paste0(
          "Validation inventory history version `", version_id,
          "` could not be read."
        ),
        parent = value
      )
    }
    snapshot <- tryCatch(
      .normalize_dlw_validation_inventory(value),
      error = function(e) e
    )
    if (inherits(snapshot, "condition")) {
      rlang::abort(
        paste0(
          "Validation inventory history version `", version_id,
          "` is malformed."
        ),
        class = c("pipdata_dlw_inventory_schema_error", "piperr"),
        parent = snapshot
      )
    }
    snapshot <- snapshot[, .(
      pipeline_version = max(pipeline_version)
    ), by = survey_id]
    if (nrow(snapshot) == 0L) {
      next
    }
    existing <- match(snapshot$survey_id, maxima$survey_id)
    matched <- !is.na(existing)
    if (any(matched)) {
      maxima$pipeline_version[existing[matched]] <- pmax(
        maxima$pipeline_version[existing[matched]],
        snapshot$pipeline_version[matched]
      )
    }
    if (any(!matched)) {
      maxima <- data.table::rbindlist(
        list(maxima, snapshot[!matched]),
        use.names = TRUE
      )
    }
  }
  data.table::setorder(maxima, survey_id)
  maxima[]
}

.active_dlw_validation_keys <- function(acquisition) {
  acquisition <- .normalize_dlw_acquisition_catalog(
    acquisition,
    source = "local"
  )
  active <- data.table::copy(acquisition[data_available == "Yes"])
  active[, survey_id := .dlw_survey_id(FileName)]
  if (anyDuplicated(active$survey_id) ||
      anyDuplicated(paste(active$survey_id, active$Checksum, sep = "\r"))) {
    .abort_dlw_acquisition_schema(
      "Current available acquisition rows must have unique survey keys."
    )
  }
  data.table::setorder(active, survey_id, Checksum)
  active[]
}

.reconcile_dlw_validation_inventory <- function(
    acquisition,
    prior,
    historical_max
) {
  active <- .active_dlw_validation_keys(acquisition)
  prior <- if (is.null(prior)) {
    .empty_dlw_validation_inventory()
  } else {
    .normalize_dlw_validation_inventory(prior)
  }
  historical_max <- data.table::as.data.table(data.table::copy(historical_max))
  required_history <- c("survey_id", "pipeline_version")
  if (!all(required_history %in% names(historical_max)) ||
      !is.character(historical_max$survey_id) ||
      !is.integer(historical_max$pipeline_version) ||
      anyNA(historical_max) || any(historical_max$pipeline_version < 1L) ||
      anyDuplicated(historical_max$survey_id)) {
    .abort_dlw_validation_inventory_schema(
      "Historical validation pipeline maxima are malformed."
    )
  }

  active_key <- paste(active$survey_id, active$Checksum, sep = "\r")
  prior_key <- paste(prior$survey_id, prior$Checksum, sep = "\r")
  retained <- prior[prior_key %in% active_key]
  if (nrow(retained) > 0L) {
    retained <- data.table::rbindlist(lapply(
      split(retained, retained$survey_id),
      function(rows) {
        max_version <- max(rows$pipeline_version)
        tied <- unique(rows[pipeline_version == max_version])
        if (nrow(tied) != 1L) {
          .abort_dlw_validation_inventory_schema(
            "Active validation duplicate ties contain conflicting rows."
          )
        }
        tied
      }
    ), use.names = TRUE)
    retained <- .normalize_dlw_validation_inventory(retained)
  }

  candidates <- data.table::copy(
    active[!survey_id %in% retained$survey_id]
  )
  history_index <- match(candidates$survey_id, historical_max$survey_id)
  previous <- historical_max$pipeline_version[history_index]
  next_version <- ifelse(is.na(previous), 1, previous + 1)
  if (any(next_version > .Machine$integer.max)) {
    .abort_dlw_validation_inventory_schema(
      "Validation pipeline version exceeds the supported integer range."
    )
  }
  candidates[, next_pipeline_version := as.integer(next_version)]
  list(
    inventory = data.table::copy(retained),
    candidates = candidates[],
    active = active[]
  )
}

.load_current_dlw_validation_inventory <- function(verbose = FALSE) {
  value <- tryCatch(
    pipload::pip_read(
      id = "gmd_valid_inv",
      alias = "dlw_meta",
      verbose = verbose
    ),
    error = function(e) e
  )
  if (!inherits(value, "condition")) {
    return(.normalize_dlw_validation_inventory(value))
  }
  versions <- tryCatch(
    .strict_dlw_versions(
      .dlw_validation_file_id("gmd_valid_inv"),
      "dlw_meta"
    ),
    error = function(e) NULL
  )
  if (.is_valid_dlw_version_catalog(versions) && nrow(versions) == 0L) {
    return(.empty_dlw_validation_inventory())
  }
  rlang::cnd_signal(value)
}

.dlw_acquisition_file_id <- function(id) {
  extension <- fs::path_ext(id)
  if (is.na(extension) || !nzchar(extension)) {
    return(fs::path_ext_set(id, "qs2"))
  }
  id
}

.dlw_acquisition_latest_version <- function(id) {
  version_id <- stamp::st_latest(
    .dlw_acquisition_file_id(id),
    alias = "dlw_inv"
  )
  if (is.null(version_id) || length(version_id) == 0L || is.na(version_id)) {
    return(NA_character_)
  }
  as.character(version_id[[1L]])
}

.reload_dlw_acquisition_inventory_state <- function(id, verbose = FALSE) {
  read_error <- NULL
  value <- tryCatch(
    .load_dlw_acquisition_inventory(id, verbose = verbose),
    error = function(e) {
      read_error <<- e
      NULL
    }
  )
  if (!is.null(read_error)) {
    versions <- tryCatch(
      .strict_dlw_versions(
        .dlw_acquisition_file_id(id),
        "dlw_inv"
      ),
      error = function(e) NULL
    )
    if (.is_valid_dlw_version_catalog(versions) && nrow(versions) == 0L) {
      return(list(
        state = "absent",
        value = NULL,
        version_id = NA_character_
      ))
    }
    rlang::cnd_signal(read_error)
  }
  list(
    state = "present",
    value = data.table::copy(value),
    version_id = tryCatch(
      .dlw_acquisition_latest_version(id),
      error = function(e) NA_character_
    )
  )
}

.persist_dlw_acquisition_inventory <- function(
    intended,
    prior,
    id,
    verbose,
    prior_version_id = NA_character_
) {
  intended <- .normalize_dlw_acquisition_catalog(intended, source = "local")
  if (!is.null(prior)) {
    prior <- .normalize_dlw_acquisition_catalog(prior, source = "local")
  }
  write_result <- tryCatch(
    pipload::pip_write(
      x = intended,
      id = id,
      pk = c("Checksum", "FileName"),
      alias = "dlw_inv",
      verbose = verbose
    ),
    error = function(e) e
  )
  reconciled <- .reconcile_dlw_persistence(
    id = id,
    alias = "dlw_inv",
    write_result = write_result,
    intended = intended,
    prior = prior,
    reload = function() {
      .reload_dlw_acquisition_inventory_state(id, verbose = verbose)
    },
    canonicalize = .canonicalize_dlw_acquisition_inventory,
    prior_version_id = prior_version_id
  )
  reconciled$fact <- do.call(
    .new_dlw_acquisition_artifact_fact,
    reconciled$fact
  )
  return(reconciled)
}

.require_verified_dlw_acquisition_write <- function(x) {
  if (!is.list(x) || !is.list(x$fact) ||
      !isTRUE(x$fact$success) || !isTRUE(x$fact$trustworthy) ||
      is.null(x$value)) {
    rlang::abort(
      "The intended acquisition inventory could not be verified durably.",
      class = c("pipdata_dlw_inventory_save_error", "piperr")
    )
  }
  invisible(x)
}

#' Match available local GMD rows to the current server catalog
#'
#' Reconciles the default local acquisition inventory in memory against the
#' authoritative current seven-module server catalog, then returns rows whose
#' durable status is `data_available = "Yes"`. Catalog-deleted and superseded
#' rows are excluded; current successful `"ASPIRE"` and `"L"` rows are retained
#' for compatibility even though active acquisition downloads only five modules.
#' This function does not write the reconciled inventory.
#'
#' @return A `data.table` of current matched available datasets, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' pipfun::setup_working_release("20260206", "TEST")
#' df <- dlw_gmd_match()
#' head(df)
#' }
dlw_gmd_match <- \() {
  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()
  check_directory(pip_folders$dlw_inventory)
  prior <- .load_dlw_acquisition_inventory("dlw_gmd_inv")
  server <- .load_dlw_acquisition_server_catalog()
  matched <- .merge_dlw_acquisition_inventory(
    server,
    prior,
    data.table::data.table(
      FileName = character(),
      data_available = character()
    )
  )[data_available == "Yes"]
  return(invisible(matched))
}


#' Compare the local GMD inventory with the current server catalog
#'
#' Candidate comparison recognizes all seven catalog/validation modules. New
#' current rows are returned, and current rows recorded as
#' `data_available = "No"` are also returned when `check_missing = TRUE`.
#' `pipdata_get_gmd()` applies the narrower five-module download policy.
#'
#' When `update_inventory = TRUE`, the default inventory is reconciled to the
#' authoritative catalog: active five-module rows are retained, obsolete rows
#' are removed, and `"ASPIRE"`/`"L"` rows remain only when already available.
#' The direct utility write is reloaded after any uncertain return and aborts
#' unless intended durable content is verified.
#'
#' @param check_missing Logical scalar. Include current unresolved inventory
#'   rows. Default `TRUE`.
#' @param update_inventory Logical scalar. Reconcile and write the default local
#'   inventory. Default `FALSE`.
#'
#' @note This function expects a working release to be configured via
#'   [pipfun::setup_working_release()]. When called from
#'   [pipdata_get_gmd()], the release is already set. When called
#'   standalone, ensure `setup_working_release()` has been invoked first.
#'
#' @return A `data.table` with new or unresolved current GMD datasets.
#' @export
#'
#' @examples
#' \dontrun{
#' pipfun::setup_working_release("20260206", "TEST")
#' df <- dlw_gmd_new()
#' head(df)
#' }
dlw_gmd_new <- function(check_missing = TRUE, update_inventory = FALSE) {
  pip_folders <- pipfun::get_pip_folders()
  check_directory(pip_folders$dlw_inventory)
  prior <- .load_dlw_acquisition_inventory("dlw_gmd_inv")
  server <- .load_dlw_acquisition_server_catalog()
  gmd_compare <- .select_dlw_acquisition_candidates(
    server,
    prior,
    check_missing = check_missing,
    modules = .dlw_acquisition_modules
  )

  if (update_inventory) {
    intended <- .merge_dlw_acquisition_inventory(
      server,
      prior,
      data.table::data.table(
        FileName = character(),
        data_available = character()
      )
    )
    persisted <- .persist_dlw_acquisition_inventory(
      intended = intended,
      prior = prior,
      id = "dlw_gmd_inv",
      verbose = getOption("pipdata.verbose", default = TRUE),
      prior_version_id = tryCatch(
        .dlw_acquisition_latest_version("dlw_gmd_inv"),
        error = function(e) NA_character_
      )
    )
    .require_verified_dlw_acquisition_write(persisted)
  }
  return(gmd_compare)
}

#' Get current available GMD datasets without completed validation
#'
#' Reconciles the default completed validation inventory to current available
#' acquisition `survey_id`/`Checksum` keys and returns keys absent from completed
#' state. Completed state means `data_available = "Yes"` with status `"valid"`
#' or `"invalid"`; recognized legacy blank/`"No"` retry rows are not completed.
#' Consequently execution failures are returned again because they have no
#' completed inventory row. All seven validation module mappings may be present.
#'
#' @param check_missing Logical scalar retained for API compatibility. It is
#'   validated, while retry selection is now determined by absence from the
#'   completed validation inventory.
#'
#' @return Invisibly, a `data.table` of current available acquisition rows that
#'   lack a completed validation row.
#' @export
#'
#' @examples
#' \dontrun{
#' pipfun::setup_working_release("20260206", "TEST")
#' df <- dlw_gmd_unvalidated()
#' head(df)
#' }
dlw_gmd_unvalidated <- function(check_missing = TRUE) {
  if (!is.logical(check_missing) || length(check_missing) != 1L ||
      is.na(check_missing)) {
    rlang::abort(
      "`check_missing` must be one non-missing logical value.",
      class = c("pipdata_dlw_argument_error", "piperr")
    )
  }
  pipfun::get_wrk_release()
  pip_folders <- pipfun::get_pip_folders()
  check_directory(pip_folders$dlw_inventory)
  check_directory(pip_folders$dlw_metadata)
  acquisition <- .load_dlw_acquisition_inventory(
    "dlw_gmd_inv",
    verbose = getOption("pipdata.verbose", default = TRUE)
  )
  completed <- .load_current_dlw_validation_inventory(
    verbose = getOption("pipdata.verbose", default = TRUE)
  )
  historical_max <- if (nrow(completed) == 0L) {
    data.table::data.table(
      survey_id = character(), pipeline_version = integer()
    )
  } else {
    completed[, .(
      pipeline_version = max(pipeline_version)
    ), by = survey_id]
  }
  state <- .reconcile_dlw_validation_inventory(
    acquisition = acquisition,
    prior = completed,
    historical_max = historical_max
  )
  result_columns <- intersect(names(acquisition), names(state$candidates))
  return(invisible(state$candidates[, result_columns, with = FALSE]))
}

#' Retrieve the GMD server catalog and initialize an acquisition inventory
#'
#' Loads the authoritative server catalog for the seven recognized modules and
#' `.dta` files, then builds the local acquisition inventory. On a first run the
#' inventory contains the five active download modules as unavailable; current
#' `"ASPIRE"` and `"L"` rows are retained only when a prior inventory already
#' records them as available. Obsolete rows and superseded checksums are removed.
#'
#' The write uses `inv_gmd_list` consistently. Any thrown, null-version, or
#' malformed write result is treated as uncertain and reconciled by reloading
#' durable state; the function aborts unless the intended inventory is verified.
#'
#' @inheritParams pipdata_get_gmd
#'
#' @note This function expects a working release to be configured via
#'   [pipfun::setup_working_release()]. When called from
#'   [pipdata_get_gmd()], the release is already set. When called
#'   standalone, ensure `setup_working_release()` has been invoked first.
#'
#' @return Invisibly, a copy of the verified durable acquisition `data.table`.
#' @export
#'
#' @examples
#' \dontrun{
#' pipfun::setup_working_release("20260206", "TEST")
#' gmd_list <- dlw_gmd_list()
#' head(gmd_list)
#' }
dlw_gmd_list <- function(inv_gmd_list = "dlw_gmd_inv") {
  if (!is.character(inv_gmd_list) || length(inv_gmd_list) != 1L ||
      is.na(inv_gmd_list) || !nzchar(trimws(inv_gmd_list))) {
    rlang::abort(
      "`inv_gmd_list` must be one nonempty character value.",
      class = c("pipdata_dlw_argument_error", "piperr")
    )
  }
  pip_folders <- pipfun::get_pip_folders()
  check_directory(pip_folders$dlw_inventory)
  server <- .load_dlw_acquisition_server_catalog()
  prior_state <- .reload_dlw_acquisition_inventory_state(inv_gmd_list)
  prior <- if (identical(prior_state$state, "present")) {
    prior_state$value
  } else {
    NULL
  }
  intended <- .merge_dlw_acquisition_inventory(
    server,
    prior,
    data.table::data.table(
      FileName = character(),
      data_available = character()
    )
  )
  persisted <- .persist_dlw_acquisition_inventory(
    intended = intended,
    prior = prior,
    id = inv_gmd_list,
    verbose = getOption("pipdata.verbose", default = TRUE),
    prior_version_id = prior_state$version_id
  )
  .require_verified_dlw_acquisition_write(persisted)
  return(invisible(data.table::copy(persisted$value)))
}
