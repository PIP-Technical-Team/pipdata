# Declarations only. No package loading, fixture writes, or unrelated test bodies.
# Raw aux shapes adapted from pipaux test-m2-stamp-cpi-ppp-pfw.R:84-217.
upstream_acceptance_preflight <- function() {
  testthat::skip_if(Sys.getenv("PIPDATA_UPSTREAM_ACCEPTANCE_CHILD") != "1",
                    "Requires the inspected disposable upstream-acceptance child")
  paths <- c(tempdir(), Sys.getenv(c("TEMP", "TMP", "TMPDIR")),
             getOption("pipfun.main_dir"),
             getOption("pipdata.dependency_manifest_path"))
  aliases <- data.table::as.data.table(stamp::st_alias_list())
  wanted <- c("dlw", "dlw_inv", "dlw_meta", "aux", "pip", "pip_meta",
              "pip_deflated", "pip_master", "pip_inv")
  stopifnot(all(wanted %in% aliases$alias))
  paths <- c(paths, aliases$root)
  resolved <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  stopifnot(length(resolved) > 0L, all(grepl("^E:/", resolved, ignore.case = TRUE)))
  root <- normalizePath(getOption("pipdata.upstream_runtime_root"),
                        winslash = "/", mustWork = TRUE)
  stopifnot(length(root) == 1L, grepl("^E:/", root, ignore.case = TRUE),
            all(startsWith(tolower(resolved), paste0(tolower(root), "/")) |
                  tolower(resolved) == tolower(root)))
  evidence <- Sys.getenv("PIPDATA_UPSTREAM_EVIDENCE_DIR")
  if (nzchar(evidence)) {
    stopifnot(dir.exists(evidence),
              grepl("^E:/", normalizePath(evidence, winslash = "/", mustWork = TRUE),
                    ignore.case = TRUE))
  }
  packages <- c("pipdata", "pipaux", "pipload", "pipfun", "stamp", "wbpip")
  loaded <- stats::setNames(lapply(packages, function(package) {
    list(path = getNamespaceInfo(asNamespace(package), "path"),
         version = as.character(utils::packageVersion(package)))
  }), packages)
  functions <- c("inv_dlw_load", "dlw_validation_engine", "pd_execute_clean",
                  "pd_dlw_clean", "pd_aux_attr")
  hashes <- stats::setNames(lapply(functions, function(name) {
    stamp:::st_hash_code(get(name, asNamespace("pipdata")))
  }), functions)
  list(paths = resolved, aliases = aliases, loaded = loaded, code_hashes = hashes,
       folders = pipfun::get_pip_folders(verbose = FALSE))
}

upstream_acceptance_fixture <- function(env = parent.frame()) {
  upstream_acceptance_preflight()
  root <- normalizePath(getOption("pipdata.upstream_runtime_root"),
                        winslash = "/", mustWork = TRUE)
  block <- tempfile("acceptance-block-", tmpdir = root)
  dir.create(block)
  stores <- file.path(block, "stores")
  manifest <- file.path(block, "manifest")
  dir.create(stores)
  dir.create(manifest)
  # All mappings are child-local. Restore them without removing any artifacts.
  state <- list(
    aliases = getFromNamespace(".stamp_aliases", "stamp"),
    stamp = getFromNamespace(".stamp_state", "stamp"),
    pipfun = getFromNamespace(".pipenv", "pipfun"),
    pipaux = getFromNamespace(".pipaux", "pipaux")
  )
  previous <- lapply(state, as.list)
  old_main <- gsub("\\\\", "/", getOption("pipfun.main_dir"))
  withr::defer({
    for (key in names(state)) {
      rm(list = ls(state[[key]], all.names = TRUE), envir = state[[key]])
      list2env(previous[[key]], envir = state[[key]])
    }
  }, envir = env)
  withr::local_options(list(
    pipfun.main_dir = stores, pipaux.working_dir = stores,
    pipload.working_dir = stores, pipfun.working_release = "20261103",
    pipfun.identity = "TEST", pipdata.dependency_manifest_path = manifest,
    pipdata.dependency_scope = basename(block)
  ), .local_envir = env)
  rm(list = ls(state$aliases, all.names = TRUE), envir = state$aliases)
  folders <- pipfun::set_pip_folders(
    main_dir = stores, release = "20261103", identity = "TEST", verbose = FALSE
  )
  aliases <- pipfun:::init_pip_aliases(folders, include_release = FALSE, verbose = FALSE)
  for (name in c("pip_deflated", "piplog")) {
    folders[[name]] <- file.path(stores, name)
    dir.create(folders[[name]])
    stamp::st_init(folders[[name]], alias = name, verbose = FALSE)
    aliases[[name]] <- name
  }
  wr <- list(release = "20261103", identity = "TEST", ppp = 2017L)
  gls <- lapply(previous$pipfun$gls, function(value) {
    if (!is.character(value)) return(value)
    paths <- gsub("\\\\", "/", value)
    inside <- !is.na(paths) & (tolower(paths) == tolower(old_main) |
      startsWith(tolower(paths), paste0(tolower(old_main), "/")))
    value[inside] <- paste0(stores, substring(paths[inside], nchar(old_main) + 1L))
    value
  })
  for (key in c("stamp_root", "folder_paths", "pip_aliases", "wrk_release", "gls")) {
    value <- switch(key, stamp_root = folders$stamp_root, folder_paths = folders,
                     pip_aliases = aliases, wrk_release = wr, gls = gls)
    assign(key, value, envir = state$pipfun)
  }
  for (key in c("pip_folders", "aux_alias", "aux_meta_alias", "aux_data_path",
                "aux_metadata_path", "wrk_release")) {
    value <- switch(key, pip_folders = folders, aux_alias = "aux",
                     aux_meta_alias = "aux_meta", aux_data_path = folders$aux_data,
                     aux_metadata_path = folders$aux_metadata, wrk_release = wr)
    assign(key, value, envir = state$pipaux)
  }
  pd_env_rm("validation_report")
  upstream_acceptance_preflight()
}

upstream_acceptance_evidence <- function(id, evidence) {
  destination <- Sys.getenv("PIPDATA_UPSTREAM_EVIDENCE_DIR")
  if (!nzchar(destination)) return(invisible(NULL))
  destination <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  stopifnot(grepl("^E:/", destination, ignore.case = TRUE))
  saveRDS(as.list(evidence), file.path(destination, paste0(id, ".rds")))
}

upstream_acceptance_identity <- function() {
  list(survey_id = "AAA_2010_SYNA_V01_M_V01_A_GMD_GPWG",
       pip_id = "AAA_2010_SYNA_INC_GPWG",
       file = "AAA_2010_SYNA_V01_M_V01_A_GMD_GPWG.dta")
}

upstream_acceptance_raw <- function() {
  # Annual LCU per capita; weights are already person weights, not HH weights.
  data.table::data.table(
    countrycode = "AAA", year = 2010L, survname = "SYNA",
    hhid = c("h1", "h2", "h3", "h4"), pid = c("p1", "p2", "p3", "p4"),
    hsize = c(1, 2, 3, 4), welfare = c(365, 730, 1095, 1460),
    welfshprosperity = c(365, 730, 1095, 1460),
    weight = c(10, 20, 30, 40), urban = c(1, 0, 1, 0),
    age = c(25, 35, 45, 55), male = c(1, 0, 1, 0)
  )
}

upstream_acceptance_aux_raw <- function() {
  cpi <- data.table::data.table(
    region = "SSA", code = "AAA", countryname = "Synthetic A", year = 2010,
    survname = "SYNA", ref_year = 2010, cpi_domain = "National",
    cpi_domain_value = 0, cpi_replication = 1, cpi_data_level = 2,
    cpi2005 = TRUE, cpi2011 = 1, cpi2017 = 1.2, cpi2021 = 1.4,
    cpi2011_unadj = 1, cpi2017_unadj = 1.2, cpi2021_unadj = 1.4,
    version = "v1", comparability = 1, cur_adj = 1,
    survey_coverage = "national", comparable = 1,
    change_cpi2017 = 0, change_icp2017 = 0, change_cpi2011 = 0,
    change_icp2011 = 0, cpi_id = "synthetic-cpi", cpi_domain_var = "",
    cpi2011_SM21 = 1, cpi2017_SM21 = 1.2
  )
  ppp <- data.table::data.table(
    CountryName = "Synthetic A", code = "AAA", CoverageType = "National",
    ppp_2005_v1_v1 = 1, ppp_2011_v1_v1 = 1.1, ppp_2011_v1_v2 = 1.2,
    ppp_2011_v2_v1 = 1.3, ppp_2011_v2_v2 = 1.35,
    ppp_2017_v1_v1 = 1.4, ppp_2017_v1_v2 = 1.5,
    ppp_2021_v1_v1 = 1.6, ppp_2021_v1_v2 = 1.7,
    source_ppp_2011 = 1, source_ppp_2005 = 1, Time = "2011", datalevel = 2,
    ppp_domain = 1, ppp_domain_value = 1, ppp_domain_var = "",
    oldicp2005 = 1, oldicp2011 = 1.1, Seriesname = "Synthetic", note_may192020 = ""
  )
  pfw <- data.table::data.table(
    region = "SSA", code = "AAA", reg_pcn = "SSA", ctryname = "Synthetic A",
    year = 2010, surveyid_year = 2010L, timewp = 1, fieldwork = 2010,
    survname = "SYNA", link = "", altname = "", survey_time = "2010",
    wbint_link = 1, wbext_link = 1, alt_link = 0, pip_meta = 1,
    surv_title = "Synthetic only", surv_producer = "Test fixture",
    survey_coverage = "N", datatype = "I", use_imputed = 0,
    use_microdata = 1, use_bin = 0, use_groupdata = 0, rep_year = 2010,
    comparability = 1, comp_note = "", preferable = "", display_cp = 1,
    fieldwork_range = "", ref_year = 2010, newref = "", ref_year_des = 1,
    wf_baseprice = "", wf_baseprice_note = "", wf_baseprice_des = -9,
    wf_spatial_des = 0, wf_spatial_var = "", cpi_replication = -9,
    cpi_domain = 1, cpi_domain_var = "", wf_currency_des = 0,
    ppp_replication = -9, ppp_domain = 1, ppp_domain_var = "",
    wf_add_temp_des = -9, wf_add_temp_var = 0,
    wf_add_spatial_des = -9, wf_add_spatial_var = 0,
    tosplit = NA_real_, tosplit_var = "", inpovcal = 1,
    oth_welfare1_type = "", oth_welfare1_var = "",
    gdp_domain = 1, pce_domain = 1, pop_domain = 1, pfw_id = "synthetic-main"
  )
  raw <- list(cpi = cpi, ppp = ppp, pfw = pfw)
  for (measure in names(raw)) {
    data.table::setattr(raw[[measure]], "gh", list(
      gh_raw_sha = paste0("synthetic-", measure, "-v1"), branch = "fixture-only"
    ))
  }
  raw
}

upstream_acceptance_produce <- function(env = parent.frame()) {
  raw <- upstream_acceptance_aux_raw()
  seen <- character()
  testthat::local_mocked_bindings(
    load_from_gh = function(measure, branch = NULL, ...) {
      stopifnot(measure %in% names(raw), identical(branch, "fixture-only"))
      seen <<- c(seen, measure)
      data.table::copy(raw[[measure]])
    }, .package = "pipfun", .env = env
  )
  # Prerequisites are saved fixtures, NOT certified producer outputs.
  country <- data.table::data.table(country_code = "AAA", region_code = "SSA")
  fixtures <- list(country_list = pipaux:::pip_aux_save(
    country, id = "country_list", pk = "country_code", verbose = FALSE
  ))
  for (measure in c("pop", "gdp", "pce")) {
    dt <- data.table::data.table(country_code = "AAA", year = 2010L,
                                  reporting_level = "national")
    data.table::set(dt, j = paste0(measure, "_data_level"), value = "national")
    data.table::set(dt, j = measure,
                    value = switch(measure, pop = 1000, gdp = 3500, pce = 2400))
    fixtures[[measure]] <- pipaux:::pip_aux_save(
      dt, id = measure, pk = c("country_code", "year", "reporting_level"),
      verbose = FALSE
    )
  }
  pins <- list(
    cpi = pipaux:::aux_cpi_update(branch = "fixture-only", detail = FALSE, verbose = FALSE),
    ppp = pipaux:::aux_ppp_update(branch = "fixture-only", detail = FALSE, verbose = FALSE),
    pfw = pipaux:::aux_pfw_update(branch = "fixture-only", detail = FALSE, verbose = FALSE)
  )
  list(raw = raw, transport_calls = seen, producer_pins = pins,
       fixture_pins = fixtures,
       snapshot = pd_freeze_aux_snapshot(c("pfw", "cpi", "ppp", "pop", "gdp", "pce")))
}

upstream_acceptance_acquire <- function(env = parent.frame()) {
  id <- upstream_acceptance_identity()
  raw <- upstream_acceptance_raw()
  catalog <- data.table::data.table(
    Country = "AAA", Year = 2010L, Survey_acronym = "SYNA", Vermast = "01",
    Veralt = "01", Module = "GPWG", Collection = "GMD", FileName = id$file,
    Checksum = "synthetic-aaa-2010-syna-v1", Ext = "dta"
  )
  selection <- list(
    country_code = "AAA", surveyid_year = 2010L, survey_acronym = "SYNA",
    welfare_type = "income", module = "GPWG", FileName = id$file,
    Checksum = "synthetic-aaa-2010-syna-v1", Vermast = "01", Veralt = "01"
  )
  # Materialize through the same real storage read by the actual DLW loader.
  pin <- pipload::pip_write(raw, id = tolower(id$survey_id), alias = "dlw",
                             verbose = FALSE)
  downloads <- character()
  testthat::local_mocked_bindings(
    dlw_server_catalog = function() data.table::copy(catalog),
    dlw_get_gmd = function(filename, ...) {
      stopifnot(identical(filename, id$file))
      downloads <<- c(downloads, filename)
      pipload::pip_write(data.table::copy(raw), id = tolower(id$survey_id),
                         alias = "dlw", verbose = FALSE)
      invisible(TRUE)
    }, .package = "dlw", .env = env
  )
  result <- .acquire_and_validate_m2_one_survey(selection, verbose = FALSE)
  list(result = result, raw_pin = pin, raw = raw, catalog = catalog,
       downloads = downloads)
}

upstream_acceptance_execution <- function(aux, env = parent.frame()) {
  context <- pd_dependency_context()
  folders <- pipfun::get_pip_folders(verbose = FALSE)
  recode <- sync_recode_spec(verbose = FALSE)
  lease <- pd_lease_acquire(context, folders$dlw_metadata)
  withr::defer(pd_lease_release(lease), envir = env)
  id <- upstream_acceptance_identity()
  action <- data.table::data.table(
    stage = "clean", entity_id = id$survey_id, survey_id = id$survey_id,
    pip_id = NA_character_, action = "create",
    input_hash = pd_hash_object(aux$catalog),
    code_hash = pd_hash_object(pd_code_fingerprints()),
    expected_pip_ids = list(id$pip_id)
  )
  list(action = action, recode = recode, execution = list(
    context = context, lease = lease, manifest = pd_empty_manifest(context),
    manifest_identity = NULL, snapshot = list(
      context = context, fingerprints = pd_code_fingerprints(), aux = aux,
      catalogs = list(pip = stamp::st_catalog_query(alias = "pip"),
                      pip_meta = stamp::st_catalog_query(alias = "pip_meta"))
    )
  ))
}

upstream_acceptance_trace <- function(names, env = parent.frame()) {
  counts <- new.env(parent = emptyenv())
  for (name in names) {
    counts[[name]] <- 0L
    observer <- local({
      key <- name
      function() counts[[key]] <- counts[[key]] + 1L
    })
    option <- paste0("pipdata.upstream_call_observer.", name)
    withr::local_options(stats::setNames(list(observer), option),
                          .local_envir = env)
    tracer <- bquote(getOption(.(option))())
    trace(name, tracer = tracer, where = asNamespace("pipdata"), print = FALSE)
  }
  withr::defer({
    for (name in names) untrace(name, where = asNamespace("pipdata"))
  }, envir = env)
  counts
}

upstream_acceptance_trace_raw_reads <- function(env = parent.frame()) {
  reads <- new.env(parent = emptyenv())
  reads$calls <- list()
  # This expression executes inside the real loader's call frame. It observes
  # the requested immutable version without replacing the loader or storage.
  observer <- function(frame) {
    reads$calls[[length(reads$calls) + 1L]] <- list(
      id_name = get("id_name", envir = frame, inherits = FALSE),
      version = get("version", envir = frame, inherits = FALSE)
    )
  }
  withr::local_options(list(pipdata.upstream_raw_read_observer = observer),
                        .local_envir = env)
  tracer <- quote(getOption("pipdata.upstream_raw_read_observer")(environment()))
  trace("load_dlw_data", tracer = tracer, where = asNamespace("pipload"), print = FALSE)
  withr::defer(untrace("load_dlw_data", where = asNamespace("pipload")), envir = env)
  reads
}
