c4_pipeline_entities <- function() {
  data.table::data.table(
    survey_id = c(
      "COL_2018_GEIH_V01_M_V01_A_GMD_ALL",
      "COL_2019_GEIH_V01_M_V01_A_GMD_ALL",
      "PER_2018_ENAHO_V01_M_V01_A_GMD_ALL"
    ),
    country_code = c("COL", "COL", "PER"),
    surveyid_year = c(2018L, 2019L, 2018L),
    survey_acronym = c("GEIH", "GEIH", "ENAHO"),
    module = "ALL"
  )
}

c4_pipeline_pfw <- function() {
  data.table::data.table(
    country_code = c("COL", "COL", "COL", "PER"),
    surveyid_year = c(2018L, 2018L, 2019L, 2018L),
    survey_acronym = c("GEIH", "GEIH", "GEIH", "ENAHO"),
    welfare_type = c("income", "consumption", "income", "income"),
    inpovcal = 1L,
    cpi_domain = c(2L, 2L, 1L, 1L),
    ppp_domain = 1L,
    gdp_domain = 1L,
    pce_domain = 1L,
    pop_domain = 1L,
    cpi_domain_var = "urban",
    ppp_domain_var = "urban"
  )
}

c4_pipeline_aux <- function() {
  list(
    pfw = c4_pipeline_pfw(),
    cpi = data.table::data.table(
      country_code = c("COL", "COL", "PER", "COL"),
      year = c(2018L, 2019L, 2018L, 2018L),
      survey_acronym = c("GEIH", "GEIH", "ENAHO", "OTHER"),
      cpi_year = 2017L,
      reporting_level = c("urban", "national", "national", "national"),
      cpi_value = c(1.1, 1.2, 1.3, 9.9)
    ),
    ppp = data.table::data.table(
      country_code = c("COL", "PER"),
      ppp_year = 2017L,
      release_version = "v01",
      adaptation_version = "v01",
      reporting_level = "national",
      ppp = c(2.1, 2.2)
    ),
    pop = data.table::data.table(
      country_code = c("COL", "COL", "PER"),
      year = c(2018L, 2019L, 2018L),
      reporting_level = "national",
      pop = c(3.1, 3.2, 3.3)
    ),
    gdp = data.table::data.table(
      country_code = c("COL", "COL", "PER"),
      year = c(2018L, 2019L, 2018L),
      reporting_level = "national",
      gdp = c(4.1, 4.2, 4.3)
    ),
    pce = data.table::data.table(
      country_code = c("COL", "COL", "PER"),
      year = c(2018L, 2019L, 2018L),
      reporting_level = "national",
      pce = c(5.1, 5.2, 5.3)
    )
  )
}

c4_pipeline_fingerprints <- function() {
  list(
    summary = data.table::data.table(
      stage = c("clean", "metadata", "deflate"),
      hash = c("clean-code-v1", "metadata-code-v1", "deflate-code-v1")
    ),
    components = data.table::data.table(
      stage = c("clean", "clean", "metadata", "deflate"),
      component = c(
        "pd_execute_clean", "recode_spec.yml", "pd_execute_metadata",
        "pd_execute_deflate"
      ),
      hash = c("clean-component-v1", "recode-v1", "metadata-v1", "deflate-v1")
    ),
    audit = list(pipdata = "test", stamp = "test")
  )
}

c4_pipeline_context <- function(roots) {
  context <- list(
    schema_version = 1L,
    release = "20260831",
    identity = "TEST",
    roots = as.list(roots),
    namespace = "c4-test"
  )
  context$scope_id <- pd_context_hash(context)
  context
}

c4_pipeline_fixture <- function(alias_suffix = NULL) {
  fixture <- new.env(parent = emptyenv())
  fixture$root <- withr::local_tempdir(.local_envir = parent.frame())
  suffix <- gsub("[^A-Za-z0-9]", "", fs::path_file(fixture$root))
  if (!is.null(alias_suffix)) {
    suffix <- alias_suffix
  }
  standard <- c(
    "aux", "pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv"
  )
  fixture$aliases <- stats::setNames(
    paste0("c4_", standard, "_", suffix), standard
  )
  roots <- stats::setNames(
    file.path(fixture$root, paste0("repository-", standard)), standard
  )
  for (alias in standard) {
    fs::dir_create(roots[[alias]], recurse = TRUE)
    stamp::st_init(root = roots[[alias]], alias = fixture$aliases[[alias]])
  }
  withr::local_options(
    list(pipdata.dependency_manifest_path = fixture$root),
    .local_envir = parent.frame()
  )
  fixture$context <- c4_pipeline_context(roots[setdiff(standard, "aux")])
  fixture$fingerprints <- c4_pipeline_fingerprints()
  fixture$hidden <- stats::setNames(rep(list(character()), 3L), c(
    "pip", "pip_meta", "pip_deflated"
  ))
  fixture$catalog_permuted <- FALSE

  write_receipt <- function(x, id, alias) {
    receipt <- pd_save_receipt(x, id, fixture$aliases[[alias]])
    stopifnot(isTRUE(receipt$success))
    receipt$alias <- alias
    receipt
  }

  fixture$aux <- c4_pipeline_aux()
  fixture$aux_receipts <- lapply(names(fixture$aux), function(measure) {
    write_receipt(fixture$aux[[measure]], measure, "aux")
  })
  names(fixture$aux_receipts) <- names(fixture$aux)

  entities <- c4_pipeline_entities()
  fixture$inv <- entities[, .(
    survey_id,
    pipeline_version = 1L,
    latest_version_id = paste0("dlw-", surveyid_year, "-v1"),
    content_hash = paste0("dlw-", country_code, "-", surveyid_year, "-h1"),
    file_path = paste0(survey_id, ".qs2"),
    status = "valid",
    data_available = "Yes",
    date_validated = as.POSIXct("2026-08-31 10:00:00", tz = "UTC"),
    Checksum = paste0("checksum-", .I),
    country_code,
    surveyid_year,
    survey_acronym,
    vermast = "v01",
    veralt = "v01",
    collection = "GMD",
    module,
    tool = "TB"
  )]

  pfw <- fixture$aux$pfw
  master_rows <- lapply(seq_len(nrow(pfw)), function(i) {
    row <- pfw[i]
    entity <- entities[
      country_code == row$country_code &
        surveyid_year == row$surveyid_year &
        survey_acronym == row$survey_acronym
    ]
    pip_id <- pd_expected_clean_pip_ids(entity, row)
    clean <- data.table::data.table(
      pip_id = pip_id,
      value = i,
      source = "baseline-clean"
    )
    clean_receipt <- write_receipt(clean, pip_id, "pip")
    metadata <- list(
      pip_id = pip_id,
      source = "baseline-metadata",
      value = i
    )
    metadata_receipt <- write_receipt(metadata, pip_id, "pip_meta")
    deflated <- data.table::data.table(
      pip_id = pip_id,
      value = i,
      source = "baseline-deflate"
    )
    deflate_receipt <- write_receipt(deflated, pip_id, "pip_deflated")
    data.table::data.table(
      survey_id = entity$survey_id,
      pip_id = pip_id,
      country_code = entity$country_code,
      surveyid_year = entity$surveyid_year,
      survey_acronym = entity$survey_acronym,
      module = entity$module,
      welfare_type = row$welfare_type,
      version_id_data = clean_receipt$version_id,
      content_hash_data = clean_receipt$content_hash,
      version_id_metadata = metadata_receipt$version_id,
      content_hash_metadata = metadata_receipt$content_hash,
      version_id_deflated = deflate_receipt$version_id,
      content_hash_deflated = deflate_receipt$content_hash,
      deflated = TRUE
    )
  })
  fixture$master <- data.table::rbindlist(master_rows, fill = TRUE)
  data.table::setorder(fixture$master, survey_id, pip_id)

  aux_catalog <- data.table::as.data.table(stamp::st_catalog_query(
    alias = fixture$aliases[["aux"]]
  ))
  aux_catalog[, measure := tolower(fs::path_ext_remove(fs::path_file(path)))]
  catalogs <- lapply(c("pip", "pip_meta", "pip_deflated", "pip_inv"), function(alias) {
    data.table::as.data.table(stamp::st_catalog_query(
      alias = fixture$aliases[[alias]]
    ))
  })
  names(catalogs) <- c("pip", "pip_meta", "pip_deflated", "pip_inv")
  snapshot <- pd_build_dependency_snapshot(
    fixture$inv,
    fixture$master,
    fixture$context,
    aux = list(catalog = aux_catalog, objects = fixture$aux),
    catalogs = catalogs,
    fingerprints = fixture$fingerprints
  )

  manifest <- pd_empty_manifest(fixture$context)
  record_rows <- list()
  input_rows <- list()
  for (i in seq_len(nrow(snapshot$current))) {
    current <- snapshot$current[i]
    stage <- current$stage[[1L]]
    entity_id <- current$entity_id[[1L]]
    receipts <- if (identical(stage, "clean")) {
      lapply(seq_len(nrow(current$output_receipts[[1L]])), function(j) {
        as.list(current$output_receipts[[1L]][j, .(
          alias, artifact, path, version_id, content_hash
        )])
      })
    } else {
      alias <- if (identical(stage, "metadata")) "pip_meta" else "pip_deflated"
      receipt <- pd_catalog_receipt(catalogs[[alias]], entity_id)
      list(c(list(alias = alias, artifact = entity_id), receipt))
    }
    record_rows[[length(record_rows) + 1L]] <- data.table::data.table(
      stage = stage,
      entity_id = entity_id,
      output_version_id = current$output_version_id[[1L]],
      output_hash = current$output_hash[[1L]],
      input_hash = current$input_hash[[1L]],
      code_hash = current$code_hash[[1L]],
      output_receipts = list(receipts)
    )
    input_rows[[length(input_rows) + 1L]] <- current$input_rows[[1L]]
  }
  manifest$records <- data.table::rbindlist(record_rows, fill = TRUE)
  manifest$inputs <- data.table::rbindlist(input_rows, fill = TRUE)
  manifest$fingerprints <- data.table::copy(fixture$fingerprints$components)
  data.table::setorder(manifest$records, stage, entity_id)
  data.table::setorder(manifest$inputs, stage, entity_id, name)
  data.table::setorder(manifest$fingerprints, stage, component)
  lease <- pd_lease_acquire(
    fixture$context, fixture$root, run_id = "c4-baseline"
  )
  fixture$manifest <- pd_manifest_publish(
    manifest, fixture$context, lease, fixture$root, parent = NULL
  )
  pd_lease_release(lease)
  fixture
}

c4_pipeline_map_alias <- function(fixture, alias) {
  mapped <- unname(fixture$aliases[alias])
  if (!length(mapped) || is.na(mapped)) alias else mapped
}

c4_pipeline_change_aux <- function(fixture, measure, change) {
  current <- pipload::pip_read(
    measure, alias = fixture$aliases[["aux"]], verbose = FALSE
  )
  changed <- change(data.table::copy(current))
  receipt <- pd_save_receipt(
    changed, measure, fixture$aliases[["aux"]]
  )
  stopifnot(isTRUE(receipt$success))
  fixture$aux[[measure]] <- changed
  invisible(receipt)
}

c4_pipeline_change_code <- function(fixture, stage, component, hash) {
  component_rows <- which(
    fixture$fingerprints$components$stage == stage &
      fixture$fingerprints$components$component == component
  )
  summary_rows <- which(fixture$fingerprints$summary$stage == stage)
  data.table::set(
    fixture$fingerprints$components,
    i = component_rows,
    j = "hash",
    value = paste0(hash, "-component")
  )
  data.table::set(
    fixture$fingerprints$summary,
    i = summary_rows,
    j = "hash",
    value = hash
  )
  invisible(fixture)
}

c4_pipeline_external_output <- function(fixture, alias, pip_id, value) {
  receipt <- pd_save_receipt(
    value, pip_id, fixture$aliases[[alias]]
  )
  stopifnot(isTRUE(receipt$success))
  invisible(receipt)
}

c4_pipeline_run <- function(
  fixture,
  inv = fixture$inv,
  force = FALSE,
  force_surveys = NULL,
  checkpoint_size = 25L,
  checkpoint_seconds = Inf,
  fault_point = NULL
) {
  real_catalog_query <- stamp::st_catalog_query
  real_versions <- stamp::st_versions
  real_pip_write <- pipload::pip_write
  real_pip_read <- pipload::pip_read
  real_lease_acquire <- pd_lease_acquire
  real_manifest_publish <- pd_manifest_publish
  real_refresh_execution_facts <- pd_refresh_execution_facts
  counters <- new.env(parent = emptyenv())
  counters$workers <- stats::setNames(rep(list(character()), 3L), .PD_STAGES)
  counters$writes <- stats::setNames(integer(6L), c(
    "aux", "pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv"
  ))
  counters$household_reads <- 0L
  counters$receipts <- list()
  counters$faulted <- FALSE
  counters$lease_path <- NULL
  inject_fault <- function(point) {
    if (!counters$faulted && identical(fault_point, point)) {
      counters$faulted <- TRUE
      rlang::abort(
        paste("Injected public pipeline fault:", point),
        class = "pipeline_public_fault", fault_point = point
      )
    }
    invisible(NULL)
  }

  catalog_query <- function(alias, ...) {
    standard_alias <- alias
    rows <- data.table::as.data.table(real_catalog_query(
      alias = c4_pipeline_map_alias(fixture, alias), ...
    ))
    hidden <- fixture$hidden[[standard_alias]]
    if (length(hidden) && nrow(rows)) {
      artifact <- toupper(fs::path_ext_remove(fs::path_file(rows$path)))
      rows <- rows[!artifact %in% toupper(hidden)]
    }
    if (isTRUE(fixture$catalog_permuted) && nrow(rows)) {
      rows <- rows[rev(seq_len(nrow(rows)))]
    }
    rows
  }
  versions <- function(path, alias, ...) {
    real_versions(
      path, alias = c4_pipeline_map_alias(fixture, alias), ...
    )
  }
  pip_write <- function(x, id, alias, verbose = FALSE, ...) {
    if (alias %in% names(counters$writes)) {
      counters$writes[[alias]] <- counters$writes[[alias]] + 1L
    }
    result <- real_pip_write(
      x = x,
      id = id,
      alias = c4_pipeline_map_alias(fixture, alias),
      verbose = verbose,
      ...
    )
    if (identical(alias, "pip_master") &&
        identical(id, "pip_master_inventory")) {
      fixture$master <- data.table::copy(data.table::as.data.table(x))
    }
    if (!is.null(fixture$hidden[[alias]])) {
      fixture$hidden[[alias]] <- setdiff(fixture$hidden[[alias]], id)
    }
    if (identical(alias, "pip_meta")) {
      inject_fault("after_artifact_write")
    }
    if (identical(alias, "pip_inv")) {
      inject_fault("after_release_inventory")
      if (!counters$faulted && identical(fault_point, "lease_loss")) {
        counters$faulted <- TRUE
        fs::dir_delete(counters$lease_path)
      }
    }
    if (identical(alias, "pip_master")) {
      inject_fault("after_master_inventory")
    }
    result
  }
  pip_read <- function(id, alias, version = NULL, verbose = FALSE, ...) {
    if (identical(alias, "pip")) {
      counters$household_reads <- counters$household_reads + 1L
    }
    real_pip_read(
      id,
      alias = c4_pipeline_map_alias(fixture, alias),
      version = version,
      verbose = verbose,
      ...
    )
  }
  load_aux_data <- function(measure, version = NULL, verbose = FALSE, ...) {
    real_pip_read(
      measure,
      alias = fixture$aliases[["aux"]],
      version = version,
      verbose = verbose
    )
  }
  load_pip_data <- function(
    pip_id, version = NULL, alias = "pip", verbose = FALSE, ...
  ) {
    pip_read(pip_id, alias, version, verbose)
  }
  execute_clean <- function(action, inv_row, execution, recode_spec,
                            verbose = FALSE) {
    survey_id <- action$survey_id[[1L]]
    counters$workers$clean <- c(counters$workers$clean, survey_id)
    counters$household_reads <- counters$household_reads + 1L
    reasons <- pd_action_reason_codes(execution, action)
    receipts <- data.table::rbindlist(lapply(
      action$expected_pip_ids[[1L]],
      function(pip_id) {
        object <- data.table::data.table(
          pip_id = pip_id,
          source = paste(c(reasons, inv_row$content_hash[[1L]]), collapse = ":")
        )
        receipt <- pd_save_receipt(
          object, pip_id, "pip", verbose, execution$lease
        )
        counters$receipts[[paste("clean", pip_id, sep = ":")]] <- receipt
        receipt$error <- NULL
        data.table::as.data.table(c(
          list(
            stage = "clean", pip_id = pip_id,
            input_hash = action$input_hash[[1L]],
            code_hash = action$code_hash[[1L]]
          ),
          receipt
        ))
      }
    ), fill = TRUE)
    list(
      success = TRUE,
      receipts = receipts,
      expected_pip_ids = action$expected_pip_ids[[1L]],
      metadata = list()
    )
  }
  execute_metadata <- function(action, snapshot, execution,
                               clean_result = NULL, verbose = FALSE) {
    pip_id <- action$pip_id[[1L]]
    inject_fault("before_worker")
    counters$workers$metadata <- c(counters$workers$metadata, pip_id)
    reasons <- pd_action_reason_codes(execution, action)
    object <- list(
      pip_id = pip_id,
      source = paste(reasons, collapse = ":"),
      projection = action$aux_projection[[1L]]
    )
    receipt <- pd_save_receipt(
      object, pip_id, "pip_meta", verbose, execution$lease
    )
    inject_fault("after_verified_receipt")
    counters$receipts[[paste("metadata", pip_id, sep = ":")]] <- receipt
    receipt$error <- NULL
    c(
      list(
        stage = "metadata", pip_id = pip_id,
        input_hash = action$input_hash[[1L]],
        code_hash = action$code_hash[[1L]],
        data_version_id = action$data_version_id[[1L]],
        data_hash = action$data_hash[[1L]]
      ),
      receipt
    )
  }
  execute_deflate <- function(action, verbose = FALSE) {
    pip_id <- action$pip_id[[1L]]
    execution <- attr(action, "execution")
    counters$workers$deflate <- c(counters$workers$deflate, pip_id)
    reasons <- pd_action_reason_codes(execution, action)
    object <- data.table::data.table(
      pip_id = pip_id,
      source = paste(reasons, collapse = ":")
    )
    receipt <- pd_save_receipt(
      object, pip_id, "pip_deflated", verbose, execution$lease
    )
    counters$receipts[[paste("deflate", pip_id, sep = ":")]] <- receipt
    receipt$error <- NULL
    c(
      list(
        stage = "deflate", pip_id = pip_id,
        input_hash = action$input_hash[[1L]],
        code_hash = action$code_hash[[1L]],
        data_version_id = action$data_version_id[[1L]],
        data_hash = action$data_hash[[1L]],
        metadata_version_id = action$metadata_version_id[[1L]],
        metadata_hash = action$metadata_hash[[1L]]
      ),
      receipt
    )
  }
  lease_acquire <- function(...) {
    lease <- real_lease_acquire(...)
    counters$lease_path <- lease$path
    lease
  }
  manifest_publish <- function(...) {
    inject_fault("before_manifest_publication")
    real_manifest_publish(...)
  }
  refresh_execution_facts <- function(execution, ...) {
    refreshed <- real_refresh_execution_facts(execution, ...)
    if (!is.null(execution$manifest_identity) &&
        execution$manifest_identity$generation >
          attr(fixture$manifest, "manifest_identity")$generation) {
      inject_fault("after_manifest_publication")
    }
    refreshed
  }

  result <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      testthat::with_mocked_bindings(
        testthat::with_mocked_bindings(
          pd_run_pipeline(
            inv = data.table::copy(inv),
            force = force,
            force_surveys = force_surveys,
            checkpoint_size = checkpoint_size,
            checkpoint_seconds = checkpoint_seconds,
            verbose = FALSE
          ),
          pd_dependency_context = function(...) fixture$context,
          pd_code_fingerprints = function(...) fixture$fingerprints,
          pd_lease_acquire = lease_acquire,
          pd_manifest_publish = manifest_publish,
          pd_refresh_execution_facts = refresh_execution_facts,
          pd_execute_clean = execute_clean,
          pd_execute_metadata = execute_metadata,
          pd_execute_deflate = execute_deflate,
          sync_recode_spec = function(...) list(version = "test"),
          .package = "pipdata"
        ),
        load_pip_master_inventory = function(...) {
          data.table::copy(fixture$master)
        },
        load_aux_data = load_aux_data,
        load_pip_data = load_pip_data,
        pip_read = pip_read,
        pip_write = pip_write,
        .package = "pipload"
      ),
      st_catalog_query = catalog_query,
      st_versions = versions,
      .package = "stamp"
    ),
    log_add = function(...) invisible(NULL),
    .package = "pipfun"
  )
  list(result = result, counters = counters)
}

c4_pipeline_units <- function(result) {
  data.table::rbindlist(lapply(result$stage_results, function(stage) {
    if (is.null(stage)) data.table::data.table() else stage$units
  }), fill = TRUE)
}
