# Focused tests for M2 Phase 2 Steps 5 and 6:
# 1. Valid one-survey module-bearing fixture acquired, formatted to LCU/day (welfare / 365),
#    passes post-format validation, and saves through stamp with verified receipt & metadata.
# 2. Survey with status == "invalid" in gmd_valid_inv is excluded before clean execution
#    (0 clean-save calls, 0 clean versions created).
# 3. Survey failing post-format validation (non-finite/missing welfare/weight or bad domain)
#    aborts/records failure before save_pip() (0 clean versions created in stamp catalog)
#    and blocks downstream stages for that survey.

m2_step56_fixture <- function(env = parent.frame()) {
  base_tmp <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  stopifnot(grepl("^E:/", base_tmp, ignore.case = TRUE))
  root <- tempfile("m2-step56-", tmpdir = base_tmp)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE), envir = env)

  dlw_data_dir <- file.path(root, "dlw_data")
  dlw_inv_dir <- file.path(root, "dlw_inventory")
  dlw_meta_dir <- file.path(root, "dlw_metadata")
  manifest_dir <- file.path(root, "manifest")
  for (d in c(dlw_data_dir, dlw_inv_dir, dlw_meta_dir, manifest_dir)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  aliases <- c(
    "dlw", "dlw_inv", "dlw_meta", "aux", "pip", "pip_meta",
    "pip_deflated", "pip_master", "pip_inv"
  )
  suffix <- gsub("[^A-Za-z0-9]", "", basename(root))
  mapped_aliases <- stats::setNames(
    paste0("m2s56_", aliases, "_", suffix),
    aliases
  )
  roots <- stats::setNames(
    file.path(root, paste0("store-", aliases)),
    aliases
  )
  for (alias in aliases) {
    dir.create(roots[[alias]], recursive = TRUE, showWarnings = FALSE)
    stamp::st_init(
      root = roots[[alias]],
      alias = mapped_aliases[[alias]],
      verbose = FALSE
    )
  }
  old_stamp_opts <- list(
    versioning = stamp::st_opts("versioning", .get = TRUE),
    retain_versions = stamp::st_opts("retain_versions", .get = TRUE),
    meta_format = stamp::st_opts("meta_format", .get = TRUE),
    warn_missing_pk_on_load = stamp::st_opts("warn_missing_pk_on_load", .get = TRUE),
    require_pk_on_load = stamp::st_opts("require_pk_on_load", .get = TRUE)
  )
  withr::defer(
    do.call(stamp::st_opts, old_stamp_opts),
    envir = env
  )
  stamp::st_opts(
    versioning = "content",
    retain_versions = Inf,
    meta_format = "both",
    warn_missing_pk_on_load = FALSE,
    require_pk_on_load = FALSE
  )
  map_alias <- function(a) {
    if (!is.null(a) && length(a) == 1L && a %in% names(mapped_aliases)) {
      mapped_aliases[[a]]
    } else {
      a
    }
  }
  real_st_save <- stamp::st_save
  real_st_load <- stamp::st_load
  real_st_load_version <- stamp::st_load_version
  real_st_info <- stamp::st_info
  real_st_versions <- stamp::st_versions
  real_st_latest <- stamp::st_latest
  real_st_catalog_query <- stamp::st_catalog_query
  real_pip_write <- pipload::pip_write
  real_pip_read <- pipload::pip_read
  testthat::local_mocked_bindings(
    st_save = function(x, file, alias = NULL, ...) {
      real_st_save(x, file = file, alias = map_alias(alias), ...)
    },
    st_load = function(file, alias = NULL, ...) {
      real_st_load(file = file, alias = map_alias(alias), ...)
    },
    st_load_version = function(path, version_id, alias = NULL, ...) {
      real_st_load_version(path = path, version_id = version_id, alias = map_alias(alias), ...)
    },
    st_info = function(path, alias = NULL, ...) {
      real_st_info(path = path, alias = map_alias(alias), ...)
    },
    st_versions = function(path, alias = NULL, ...) {
      candidates_path <- unique(c(
        path,
        basename(path),
        if (!nzchar(fs::path_ext(path))) paste0(path, ".qs2"),
        if (!nzchar(fs::path_ext(basename(path)))) paste0(basename(path), ".qs2")
      ))
      res <- NULL
      for (p in candidates_path) {
        res <- tryCatch(
          real_st_versions(path = p, alias = map_alias(alias), ...),
          error = function(e) NULL
        )
        if (!is.null(res) && is.data.frame(res) && nrow(res) > 0L) break
      }
      if (is.null(res) || !is.data.frame(res) || nrow(res) == 0L) {
        return(data.table::data.table(
          version_id = character(),
          content_hash = character(),
          created_at = character()
        ))
      }
      res <- data.table::as.data.table(data.table::copy(res))
      for (col in names(res)) {
        data.table::set(res, j = col, value = as.character(res[[col]]))
      }
      res[]
    },
    st_latest = function(path, alias = NULL, ...) {
      real_st_latest(path = path, alias = map_alias(alias), ...)
    },
    st_catalog_query = function(alias = NULL, ...) {
      real_st_catalog_query(alias = map_alias(alias), ...)
    },
    .package = "stamp",
    .env = env
  )
  testthat::local_mocked_bindings(
    pip_write = function(x, id, alias, verbose = FALSE, ...) {
      real_pip_write(x = x, id = id, alias = map_alias(alias), verbose = verbose, ...)
    },
    pip_read = function(id, alias, version = NULL, verbose = FALSE, ...) {
      real_pip_read(id = id, alias = map_alias(alias), version = version, verbose = verbose, ...)
    },
    .package = "pipload",
    .env = env
  )

  withr::local_options(
    list(
      pipfun.log.auto = FALSE,
      pipfun.main_dir = root,
      pipdata.dependency_manifest_path = manifest_dir,
      pipdata.verbose = FALSE
    ),
    .local_envir = env
  )

  list(
    root = root,
    roots = roots,
    folders = list(
      dlw_data = dlw_data_dir,
      dlw_inventory = dlw_inv_dir,
      dlw_metadata = dlw_meta_dir
    ),
    context = list(
      schema_version = 1L,
      release = "20261030",
      identity = "TEST",
      roots = as.list(roots[c("pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv")]),
      namespace = paste0("m2-step56-", basename(root)),
      scope_id = paste0("scope-", basename(root))
    )
  )
}

test_that("unchanged saves retain verified exact receipts without new versions", {
  f <- m2_step56_fixture()
  object <- list(value = "unchanged")
  first <- pd_save_receipt(object, "unchanged-receipt", "pip")
  second <- pd_save_receipt(object, "unchanged-receipt", "pip")

  expect_true(first$success)
  expect_true(second$success)
  expect_true(second$skipped)
  expect_identical(second$version_id, first$version_id)
  expect_identical(second$content_hash, first$content_hash)
  expect_no_error(pd_revalidate_receipt(second))
  expect_identical(nrow(stamp::st_versions(second$path, alias = "pip")), 1L)

  write <- pipload::pip_write
  testthat::local_mocked_bindings(
    pip_write = function(...) {
      receipt <- write(...)
      receipt$version_id <- "unrelated-version"
      receipt
    },
    .package = "pipload"
  )
  mismatched <- pd_save_receipt(object, "unchanged-receipt", "pip")
  expect_false(mismatched$success)
  expect_error(pd_revalidate_receipt(mismatched),
               class = "pipdata_receipt_invalid")
})

m2_step56_server_catalog <- function() {
  data.table::data.table(
    Country = c("BOL", "PRY"),
    Year = c(2020L, 2019L),
    Survey_acronym = c("EH", "EPH"),
    Vermast = c("01", "01"),
    Veralt = c("01", "01"),
    Module = c("GPWG", "GPWG"),
    Collection = c("GMD", "GMD"),
    FileName = c(
      "BOL_2020_EH_V01_M_V01_A_GMD_GPWG.dta",
      "PRY_2019_EPH_V01_M_V01_A_GMD_GPWG.dta"
    ),
    Checksum = c("chk-bol-2020-gpwg", "chk-pry-2019-gpwg"),
    Ext = c("dta", "dta")
  )
}

m2_step56_selection <- function(checksum = "chk-bol-2020-gpwg") {
  list(
    country_code = "BOL",
    surveyid_year = 2020L,
    survey_acronym = "EH",
    welfare_type = "income",
    module = "GPWG",
    FileName = "BOL_2020_EH_V01_M_V01_A_GMD_GPWG.dta",
    Checksum = checksum,
    Vermast = "01",
    Veralt = "01"
  )
}

m2_step56_microdata <- function(welfare = c(365, 730, 1095, 1460),
                                weight = c(10, 20, 30, 40),
                                urban = c(1, 0, 1, 0)) {
  data.table::data.table(
    country_code = "BOL",
    year = 2020L,
    hhid = c("1", "2", "3", "4"),
    pid = c("1", "1", "1", "1"),
    welfare = welfare,
    weight = weight,
    urban = urban,
    age = c(25, 35, 45, 55),
    male = c(1, 0, 1, 0)
  )
}

m2_step56_seed_aux <- function(cpi_domain = 1L, ppp_domain = 1L) {
  pfw <- data.table::data.table(
    country_code = "BOL",
    surveyid_year = 2020L,
    survey_year = 2020,
    survey_acronym = "EH",
    welfare_type = "income",
    inpovcal = 1L,
    use_imputed = 0L,
    is_alt_welf = FALSE,
    oth_welfare1_type = "",
    cpi_domain = as.integer(cpi_domain),
    ppp_domain = as.integer(ppp_domain),
    gdp_domain = 1L,
    pce_domain = 1L,
    pop_domain = 1L,
    cpi_domain_var = "urban",
    ppp_domain_var = "urban"
  )
  cpi <- data.table::data.table(
    country_code = "BOL",
    year = 2020L,
    survey_year = 2020,
    survey_acronym = "EH",
    cpi_year = 2017L,
    reporting_level = if (cpi_domain == 2L) c("urban", "rural") else "national",
    cpi_data_level = if (cpi_domain == 2L) c("urban", "rural") else "national",
    cpi_value = if (cpi_domain == 2L) c(1.1, 1.0) else 1.05
  )
  attr(cpi, "stamp_pk") <- list(keys = c("country_code", "year", "survey_acronym", "cpi_year", "reporting_level"))
  ppp <- data.table::data.table(
    country_code = "BOL",
    ppp_year = 2017L,
    release_version = "v01",
    adaptation_version = "v01",
    reporting_level = if (ppp_domain == 2L) c("urban", "rural") else "national",
    ppp_data_level = if (ppp_domain == 2L) c("urban", "rural") else "national",
    ppp = if (ppp_domain == 2L) c(3.0, 2.5) else 2.8
  )
  attr(ppp, "stamp_pk") <- list(keys = c("country_code", "ppp_year", "release_version", "adaptation_version", "reporting_level"))
  pop <- data.table::data.table(
    country_code = "BOL",
    year = 2020L,
    reporting_level = "national",
    pop_data_level = "national",
    pop = 11000000
  )
  attr(pop, "stamp_pk") <- list(keys = c("country_code", "year", "reporting_level"))
  gdp <- data.table::data.table(
    country_code = "BOL",
    year = 2020L,
    reporting_level = "national",
    gdp_data_level = "national",
    gdp = 3500
  )
  attr(gdp, "stamp_pk") <- list(keys = c("country_code", "year", "reporting_level"))
  pce <- data.table::data.table(
    country_code = "BOL",
    year = 2020L,
    reporting_level = "national",
    pce_data_level = "national",
    pce = 2400
  )
  attr(pce, "stamp_pk") <- list(keys = c("country_code", "year", "reporting_level"))

  aux_list <- list(pfw = pfw, cpi = cpi, ppp = ppp, pop = pop, gdp = gdp, pce = pce)
  catalog_rows <- lapply(names(aux_list), function(m) {
    stamp::st_save(
      aux_list[[m]],
      file = paste0(m, ".qs2"),
      alias = "aux",
      pk = if (m == "pfw") c("country_code", "surveyid_year", "welfare_type") else attr(aux_list[[m]], "stamp_pk")$keys,
      verbose = FALSE
    )
    info <- stamp::st_info(paste0(m, ".qs2"), alias = "aux")
    data.table::data.table(
      measure = m,
      path = info$sidecar$path,
      version_id = info$catalog$latest_version_id,
      content_hash = info$sidecar$content_hash
    )
  })
  list(
    objects = aux_list,
    catalog = data.table::rbindlist(catalog_rows, fill = TRUE)
  )
}

test_that("Step 5: single-survey selection resolves module-bearing identity and gates failed/mismatched downloads", {
  f <- m2_step56_fixture()
  server <- m2_step56_server_catalog()
  sel <- m2_step56_selection()

  resolved <- .resolve_m2_one_survey_selection(sel, server)
  expect_identical(resolved$survey_id, "BOL_2020_EH_V01_M_V01_A_GMD_GPWG")
  expect_identical(resolved$pip_id, "BOL_2020_EH_INC_GPWG")
  expect_identical(nrow(resolved$catalog_row), 1L)

  # Mismatched checksum is rejected before download
  expect_error(
    .resolve_m2_one_survey_selection(m2_step56_selection(checksum = "wrong-checksum"), server),
    class = "pipdata_dlw_contract_error"
  )

  # Ensure server catalog is read once and pinned catalog_row is passed directly to core
  catalog_reads <- 0L
  download_calls <- character()
  validate_calls <- 0L
  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) f$folders,
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    dlw_server_catalog = function() {
      catalog_reads <<- catalog_reads + 1L
      if (catalog_reads > 1L) {
        mutated <- data.table::copy(server)
        mutated[Country == "BOL", Checksum := "unexpected-second-read-checksum"]
        return(mutated)
      }
      data.table::copy(server)
    },
    dlw_get_gmd = function(filename, ...) {
      download_calls <<- c(download_calls, filename)
      rlang::abort("simulated DLW network failure", class = "dlw_network_error")
    },
    .package = "dlw"
  )
  testthat::local_mocked_bindings(
    .pipdata_validate_gmd_core = function(...) {
      validate_calls <<- validate_calls + 1L
      stop("validation must not run on failed download")
    },
    .package = "pipdata"
  )

  failed_acq <- .acquire_and_validate_m2_one_survey(sel, verbose = FALSE)
  expect_identical(catalog_reads, 1L)
  expect_identical(download_calls, "BOL_2020_EH_V01_M_V01_A_GMD_GPWG.dta")
  expect_identical(validate_calls, 0L)
  expect_false(failed_acq$eligible_for_clean)
  expect_identical(nrow(failed_acq$acquisition_inventory), 1L)
  expect_identical(failed_acq$acquisition_inventory$Checksum, "chk-bol-2020-gpwg")
  expect_identical(failed_acq$acquisition_inventory$data_available, "No")
})

test_that("Step 5 & 6 (Check 1): valid one-survey fixture is acquired, formatted to LCU/day, validated, and saved to stamp with verified receipt and metadata", {
  f <- m2_step56_fixture()
  server <- m2_step56_server_catalog()
  sel <- m2_step56_selection()
  raw_dt <- m2_step56_microdata(welfare = c(365, 730, 1095, 1460))
  aux <- m2_step56_seed_aux(cpi_domain = 2L, ppp_domain = 1L)

  dlw_calls <- character()
  validated_surveys <- character()
  # Seed an unrelated prior available-but-unvalidated survey in dlw_gmd_inv
  prior_unrelated <- server[Country == "PRY"]
  prior_unrelated[, data_available := "Yes"]
  pipload::pip_write(
    prior_unrelated,
    id = "dlw_gmd_inv",
    alias = "dlw_inv",
    pk = "FileName",
    verbose = FALSE
  )

  testthat::local_mocked_bindings(
    get_wrk_release = function(...) invisible(TRUE),
    get_pip_folders = function(...) f$folders,
    log_info = function(...) invisible(TRUE),
    log_error = function(...) invisible(TRUE),
    log_add = function(...) invisible(TRUE),
    .package = "pipfun"
  )
  testthat::local_mocked_bindings(
    dlw_server_catalog = function() data.table::copy(server),
    dlw_get_gmd = function(filename, ...) {
      dlw_calls <<- c(dlw_calls, filename)
      file_id <- paste0(tolower(fs::path_ext_remove(filename)), ".qs2")
      stamp::st_save(raw_dt, file = file_id, alias = "dlw", verbose = FALSE)
      invisible(TRUE)
    },
    .package = "dlw"
  )
  testthat::local_mocked_bindings(
    load_dlw_data = function(id_name, verbose = FALSE) {
      file_id <- paste0(tolower(id_name), ".qs2")
      stamp::st_load(file = file_id, alias = "dlw", verbose = FALSE)
    },
    .package = "pipload"
  )

  testthat::local_mocked_bindings(
    pd_dependency_context = function() f$context,
    dlw_validation_engine = function(out, survey_id, module) {
      validated_surveys <<- c(validated_surveys, survey_id)
      rec <- data.table::data.table(
        table_name = survey_id,
        assertion.id = "1",
        description = "GPWG check",
        num.violations = 0L,
        call = "check",
        message = "checks passed",
        type = "success",
        error_df = list(NULL)
      )
      pd_env_append("validation_report", rec)
      rec[, .(table_name, message, type)]
    },
    .package = "pipdata"
  )

  acq_val <- .acquire_and_validate_m2_one_survey(sel, verbose = FALSE)
  expect_identical(dlw_calls, "BOL_2020_EH_V01_M_V01_A_GMD_GPWG.dta")
  expect_identical(validated_surveys, "BOL_2020_EH_V01_M_V01_A_GMD_GPWG")
  expect_true(acq_val$eligible_for_clean)
  expect_identical(acq_val$validation$outcome, "success")
  expect_identical(nrow(acq_val$validation$inventory), 1L)
  expect_identical(acq_val$validation$inventory$survey_id, "BOL_2020_EH_V01_M_V01_A_GMD_GPWG")
  expect_identical(acq_val$validation$inventory$status, "valid")

  inv_row <- .filter_completed_dlw_validation_inventory(acq_val$validation$inventory)
  expect_identical(nrow(inv_row), 1L)

  action <- data.table::data.table(
    stage = "clean",
    entity_id = inv_row$survey_id[[1L]],
    survey_id = inv_row$survey_id[[1L]],
    pip_id = NA_character_,
    action = "create",
    input_hash = "in-hash-1",
    code_hash = "code-hash-1",
    expected_pip_ids = list("BOL_2020_EH_INC_GPWG")
  )
  lease <- pd_lease_acquire(f$context, f$folders$dlw_metadata)
  withr::defer(try(pd_lease_release(lease), silent = TRUE))
  execution <- list(
    context = f$context,
    lease = lease,
    manifest = pd_empty_manifest(f$context),
    manifest_identity = NULL,
    snapshot = list(
      context = f$context,
      fingerprints = pd_code_fingerprints(),
      aux = aux,
      catalogs = list(pip = data.table::data.table(), pip_meta = data.table::data.table())
    )
  )
  recode_spec <- list(
    spec = yaml::read_yaml(system.file("extdata", "recode_spec.yml", package = "pipdata")),
    version_id = "recode-spec-v1"
  )

  clean_res <- pd_execute_clean(action, inv_row, execution, recode_spec = recode_spec, verbose = FALSE)
  expect_true(clean_res$success)
  expect_identical(clean_res$expected_pip_ids, "BOL_2020_EH_INC_GPWG")
  expect_identical(nrow(clean_res$receipts), 1L)
  expect_true(clean_res$receipts$success[[1L]])
  expect_no_error(pd_revalidate_receipt(as.list(clean_res$receipts[1L])))

  # Verify saved clean table in stamp has LCU/day welfare (welfare / 365) and valid attributes
  saved_dt <- pipload::pip_read(
    id = "BOL_2020_EH_INC_GPWG",
    version = clean_res$receipts$version_id[[1L]],
    alias = "pip",
    verbose = FALSE
  )
  expect_s3_class(saved_dt, "data.table")
  expect_identical(attr(saved_dt, "distribution_type"), "micro")
  expect_equal(sort(saved_dt$welfare), c(1, 2, 3, 4), tolerance = 1e-12)
  expect_identical(attr(saved_dt, "cpi_data_level"), "area")
  expect_identical(attr(saved_dt, "ppp_data_level"), "national")
  expect_true(all(saved_dt$area %in% c("urban", "rural")))

  # Verify retrievable metadata
  expect_true("BOL_2020_EH_INC_GPWG" %in% names(clean_res$metadata))
  meta_Res <- pd_save_receipt(
    clean_res$metadata[["BOL_2020_EH_INC_GPWG"]],
    "BOL_2020_EH_INC_GPWG",
    "pip_meta",
    verbose = FALSE,
    lease = lease
  )
  expect_true(meta_Res$success)
  expect_no_error(pd_revalidate_receipt(meta_Res))
  saved_meta <- pipload::pip_read(
    id = "BOL_2020_EH_INC_GPWG",
    version = meta_Res$version_id,
    alias = "pip_meta",
    verbose = FALSE
  )
  expect_identical(saved_meta$survey_id, "BOL_2020_EH_V01_M_V01_A_GMD_GPWG")
  expect_true(all(c("cpi", "ppp", "pop") %in% names(saved_meta)))
})

test_that("Step 6 (Check 2): survey with status == 'invalid' in gmd_valid_inv is excluded before clean execution (0 clean saves, 0 clean versions)", {
  f <- m2_step56_fixture()
  inv <- make_dependency_validation_inventory()
  invalid_only <- inv[status == "invalid"]
  expect_identical(nrow(invalid_only), 1L)

  clean_saves <- 0L
  clean_worker_calls <- 0L
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) data.table::data.table(survey_id = character(), pip_id = character()),
    pip_write = function(...) {
      clean_saves <<- clean_saves + 1L
      stop("clean write must not be called for invalid survey")
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function(...) f$context,
    pd_freeze_aux_snapshot = function(...) list(catalog = data.table::data.table(), objects = list()),
    pd_execute_clean = function(...) {
      clean_worker_calls <<- clean_worker_calls + 1L
      stop("clean worker must not be called for invalid survey")
    },
    .package = "pipdata"
  )

  filtered <- .filter_completed_dlw_validation_inventory(invalid_only)
  expect_identical(nrow(filtered), 0L)

  out <- pd_process_data(inv = invalid_only, verbose = FALSE)
  expect_identical(nrow(out), 0L)
  expect_identical(clean_worker_calls, 0L)
  expect_identical(clean_saves, 0L)
  pip_cat <- data.table::as.data.table(stamp::st_catalog_query(alias = "pip"))
  expect_identical(nrow(pip_cat), 0L)
})

test_that("Step 6 (Check 3): survey failing post-format validation aborts/records failure before save_pip (0 stamp versions) and blocks downstream stages", {
  f <- m2_step56_fixture()
  aux <- m2_step56_seed_aux(cpi_domain = 2L, ppp_domain = 1L)
  inv <- make_dependency_validation_inventory()[status == "valid"]
  inv[, `:=`(
    survey_id = "BOL_2020_EH_V01_M_V01_A_GMD_GPWG",
    module = "GPWG",
    tool = "PC"
  )]
  recode_spec <- list(
    spec = yaml::read_yaml(system.file("extdata", "recode_spec.yml", package = "pipdata")),
    version_id = "recode-spec-v1"
  )

  # Case A: Non-finite / missing welfare in raw DLW data
  bad_dt <- m2_step56_microdata(welfare = c(365, NA_real_, Inf, 1460))
  save_calls <- 0L
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) data.table::copy(bad_dt),
    pip_write = function(...) {
      save_calls <<- save_calls + 1L
      stop("pip_write must not be called when post-format validation fails")
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    log_add = function(...) invisible(TRUE),
    .package = "pipfun"
  )

  action <- data.table::data.table(
    stage = "clean",
    entity_id = inv$survey_id[[1L]],
    survey_id = inv$survey_id[[1L]],
    pip_id = NA_character_,
    action = "create",
    input_hash = "in-hash-1",
    code_hash = "code-hash-1",
    expected_pip_ids = list("BOL_2020_EH_INC_GPWG")
  )
  lease <- pd_lease_acquire(f$context, f$folders$dlw_metadata)
  withr::defer(try(pd_lease_release(lease), silent = TRUE))
  execution <- list(
    context = f$context,
    lease = lease,
    manifest = pd_empty_manifest(f$context),
    manifest_identity = NULL,
    plan = list(
      actions = data.table::rbindlist(list(
        action,
        data.table::data.table(
          stage = "metadata",
          entity_id = "BOL_2020_EH_INC_GPWG",
          survey_id = inv$survey_id[[1L]],
          pip_id = "BOL_2020_EH_INC_GPWG",
          action = "refresh",
          input_hash = "m-in",
          code_hash = "m-code",
          expected_pip_ids = list("BOL_2020_EH_INC_GPWG")
        )
      ), fill = TRUE),
      reasons = data.table::data.table(
        stage = c("clean", "metadata"),
        entity_id = c(inv$survey_id[[1L]], "BOL_2020_EH_INC_GPWG"),
        reason = c("new_entity", "new_entity"),
        input = c("manifest", "manifest"),
        old = c(NA_character_, NA_character_),
        new = c("in-hash-1", "m-in")
      )
    ),
    snapshot = list(
      context = NULL,
      aux = aux,
      catalogs = list(pip = data.table::data.table(), pip_meta = data.table::data.table())
    )
  )

  res_clean <- pd_execute_clean(action, inv, execution, recode_spec = recode_spec, verbose = FALSE)
  expect_false(res_clean$success)
  expect_identical(res_clean$condition$code, "pipdata_clean_validation_error")
  expect_true(res_clean$condition$recoverable)
  expect_identical(save_calls, 0L)
  expect_identical(nrow(stamp::st_catalog_query(alias = "pip")), 0L)

  # Case B: Subnational ("area") domain with missing/blank area values after clean
  bad_area_dt <- m2_step56_microdata(urban = c(1, NA_real_, 1, 0))
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) data.table::copy(bad_area_dt),
    .package = "pipload"
  )
  res_area <- pd_execute_clean(action, inv, execution, recode_spec = recode_spec, verbose = FALSE)
  expect_false(res_area$success)
  expect_identical(res_area$condition$code, "pipdata_clean_validation_error")
  expect_identical(save_calls, 0L)
  expect_identical(nrow(stamp::st_catalog_query(alias = "pip")), 0L)

  # Case B2: Negative or non-finite weight in raw DLW data is rejected before save
  for (bad_w in list(c(10, -1, 30, 40), c(10, NA_real_, 30, 40), c(0, 0, 0, 0))) {
    bad_w_dt <- m2_step56_microdata(weight = bad_w)
    testthat::local_mocked_bindings(
      load_dlw_data = function(...) data.table::copy(bad_w_dt),
      .package = "pipload"
    )
    res_w <- pd_execute_clean(action, inv, execution, recode_spec = recode_spec, verbose = FALSE)
    expect_false(res_w$success)
    expect_identical(res_w$condition$code, "pipdata_clean_validation_error")
    expect_identical(save_calls, 0L)
  }

  # Case B3: Missing status or data_available columns or non-tabular input rejected before inv_dlw_load
  load_calls <- 0L
  with_mocked_bindings(
    {
      for (bad_inv in list(
        data.table::data.table(survey_id = inv$survey_id[[1L]]),
        data.table::data.table(survey_id = inv$survey_id[[1L]], status = "valid"),
        data.table::data.table(survey_id = inv$survey_id[[1L]], data_available = "Yes"),
        data.table::data.table(survey_id = inv$survey_id[[1L]], status = "invalid", data_available = "Yes"),
        list(survey_id = inv$survey_id[[1L]], status = "invalid", data_available = "Yes")
      )) {
        res_bad_inv <- pd_execute_clean(action, bad_inv, execution, recode_spec = recode_spec, verbose = FALSE)
        expect_false(res_bad_inv$success)
        expect_identical(res_bad_inv$condition$code, "pipdata_clean_validation_error")
        expect_null(process_data(bad_inv, aux$objects, recode_spec = recode_spec, verbose = FALSE))
      }
    },
    inv_dlw_load = function(...) {
      load_calls <<- load_calls + 1L
      stop("inv_dlw_load must not be called when eligibility fields are missing")
    },
    .package = "pipdata"
  )
  expect_identical(load_calls, 0L)
  expect_identical(save_calls, 0L)

  # Case C: Malformed domain attribute on cleaned table is rejected by pd_validate_clean_output()
  valid_clean_dt <- data.table::copy(bad_dt)[1:2][, `:=`(welfare = c(1.0, 2.0), weight = c(10.0, 20.0), area = c("urban", "rural"))]
  data.table::setattr(valid_clean_dt, "class", c("pipmd", class(valid_clean_dt)))
  for (nm in c("survey_id", "country_code", "survey_acronym")) {
    data.table::setattr(valid_clean_dt, nm, "BOL")
  }
  data.table::setattr(valid_clean_dt, "surveyid_year", 2020L)
  data.table::setattr(valid_clean_dt, "welfare_type", "income")
  data.table::setattr(valid_clean_dt, "reporting_level", "1")
  data.table::setattr(valid_clean_dt, "distribution_type", "micro")
  for (nm in c("ppp_data_level", "cpi_data_level", "pop_data_level", "gdp_data_level", "pce_data_level")) {
    data.table::setattr(valid_clean_dt, nm, "national")
  }
  data.table::setattr(valid_clean_dt, "aux_data_levels", "same")
  expect_no_error(pd_validate_clean_output(list(BOL_2020_EH_INC_GPWG = valid_clean_dt)))

  bad_domain_dt <- data.table::copy(valid_clean_dt)
  data.table::setattr(bad_domain_dt, "cpi_data_level", "unsupported_domain")
  expect_error(
    pd_validate_clean_output(list(BOL_2020_EH_INC_GPWG = bad_domain_dt)),
    class = "pipdata_clean_validation_error"
  )

  # Verify production pd_process_data() orchestration automatically blocks downstream metadata stage
  # when clean fails post-format validation (without manually setting scheduling_state in the test)
  pd_lease_release(lease)
  meta_worker_calls <- 0L
  observed_meta_actions <- NULL
  orig_run_meta <- pd_run_metadata_stage_prepared
  testthat::local_mocked_bindings(
    load_dlw_data = function(...) data.table::copy(bad_dt),
    load_pip_master_inventory = function(...) {
      data.table::data.table(
        survey_id = inv$survey_id[[1L]],
        pip_id = "BOL_2020_EH_INC_GPWG",
        country_code = "BOL",
        surveyid_year = 2020L,
        survey_acronym = "EH",
        welfare_type = "income",
        module = "GPWG",
        version_id_data = "old-d-v1",
        content_hash_data = "old-d-h1",
        version_id_metadata = "old-m-v1",
        content_hash_metadata = "old-m-h1",
        version_id_deflated = "old-def-v1",
        content_hash_deflated = "old-def-h1",
        deflated = TRUE
      )
    },
    .package = "pipload"
  )
  testthat::local_mocked_bindings(
    pd_dependency_context = function(...) f$context,
    pd_prepare_execution = function(...) execution,
    pd_refresh_execution_facts = function(execution, ...) execution,
    sync_recode_spec = function(...) recode_spec,
    pd_persist_failed_invalidation = function(execution, master, ...) {
      list(candidate = master, execution = execution)
    },
    pd_log_stage_condition = function(...) invisible(NULL),
    pd_execute_metadata = function(...) {
      meta_worker_calls <<- meta_worker_calls + 1L
      stop("metadata worker must not run when upstream clean fails")
    },
    pd_run_metadata_stage_prepared = function(execution, actions, ...) {
      observed_meta_actions <<- data.table::copy(actions)
      orig_run_meta(execution, actions, ...)
    },
    .package = "pipdata"
  )

  pd_process_data(inv = inv, verbose = FALSE)
  expect_false(is.null(observed_meta_actions))
  expect_identical(observed_meta_actions$scheduling_state, "blocked")
  expect_identical(meta_worker_calls, 0L)
  expect_identical(save_calls, 0L)
  expect_identical(nrow(stamp::st_catalog_query(alias = "pip")), 0L)
  expect_identical(nrow(stamp::st_catalog_query(alias = "pip_meta")), 0L)
})
