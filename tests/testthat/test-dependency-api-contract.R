test_that("required public storage APIs retain exact-version contracts", {
  expect_named(formals(stamp::st_versions), c("path", "alias"))
  expect_named(formals(stamp::st_hash_obj), "x")
  expect_true("version" %in% names(formals(pipload::load_aux_data)))
  expect_true("version" %in% names(formals(pipload::pip_read)))
  expect_identical(names(formals(pd_process_data))[1:5],
                   c("inv", "aux_measures", "force", "verbose", "force_surveys"))
  expect_identical(
    names(formals(pd_run_pipeline)),
    c(
      "inv", "force", "verbose", "force_surveys", "bootstrap",
      "bootstrap_entities", "checkpoint_size", "checkpoint_seconds"
    )
  )
})

test_that("V14 touched APIs retain exact formals defaults and positional order", {
  expect_identical(
    names(formals(pd_process_data)),
    c(
      "inv", "aux_measures", "force", "verbose", "force_surveys",
      "bootstrap", "bootstrap_entities", "dependency_plan"
    )
  )
  expect_identical(
    names(formals(pd_deflate_pipeline)),
    c(
      "inv", "force", "verbose", "bootstrap", "bootstrap_entities",
      "dependency_plan"
    )
  )
  expect_identical(
    names(formals(pd_run_deflate_stage)),
    c(
      "inv", "force", "verbose", "bootstrap", "bootstrap_entities",
      "dependency_plan", "force_surveys", "entity_error_policy",
      "fatal_error_policy"
    )
  )
  expect_identical(
    names(formals(pd_change_report)),
    c("inv", "master", "manifest", "context")
  )
  expect_identical(
    names(formals(log_report)),
    c("log", "path", "title", "overwrite")
  )
  expect_identical(
    names(formals(pd_run_pipeline)),
    c(
      "inv", "force", "verbose", "force_surveys", "bootstrap",
      "bootstrap_entities", "checkpoint_size", "checkpoint_seconds"
    )
  )

  expect_identical(formals(pd_process_data)$inv, NULL)
  expect_identical(
    formals(pd_process_data)$aux_measures,
    quote(c("pfw", "cpi", "ppp", "pop", "gdp", "pce"))
  )
  expect_identical(formals(pd_process_data)$force, FALSE)
  expect_identical(formals(pd_process_data)$force_surveys, NULL)
  expect_identical(formals(pd_process_data)$bootstrap, FALSE)
  expect_identical(formals(pd_process_data)$bootstrap_entities, NULL)
  expect_identical(formals(pd_process_data)$dependency_plan, NULL)
  expect_identical(formals(pd_deflate_pipeline)$inv, NULL)
  expect_identical(formals(pd_deflate_pipeline)$force, FALSE)
  expect_identical(formals(pd_deflate_pipeline)$bootstrap, FALSE)
  expect_identical(formals(pd_deflate_pipeline)$bootstrap_entities, NULL)
  expect_identical(formals(pd_deflate_pipeline)$dependency_plan, NULL)
  expect_identical(formals(pd_run_pipeline)$checkpoint_size, 25L)
  expect_identical(formals(pd_run_pipeline)$checkpoint_seconds, Inf)
})

test_that("V14 export and S3 registration delta is exact", {
  namespace_path <- system.file("NAMESPACE", package = "pipdata")
  if (!nzchar(namespace_path)) {
    namespace_path <- testthat::test_path("..", "..", "NAMESPACE")
  }
  namespace <- readLines(namespace_path)
  exports <- sort(sub("^export\\((.*)\\)$", "\\1", grep(
    "^export\\(", namespace, value = TRUE
  )))
  baseline_exports <- sort(c(
    "add_area", "add_dist_type", "apply_recode_spec", "build_pip_inventory",
    "check_directory", "cln_changes", "cpfw_merge", "deflation",
    "diff_recode_spec", "dlw_clean", "dlw_gmd_list", "dlw_gmd_match",
    "dlw_gmd_new", "dlw_gmd_unvalidated", "dlw_validation",
    "dlw_validation_all", "dlw_validation_aspire", "dlw_validation_bin",
    "dlw_validation_engine", "dlw_validation_gpwg", "dlw_validation_group",
    "dlw_validation_hist", "dlw_validation_l", "dlw_validation_skip",
    "dlw_var_check", "export_recode_spec_yaml", "get_country_pfw",
    "get_data_status", "get_validation_ctry", "get_validation_list",
    "get_validation_report", "inv_dlw_load", "list_recode_spec_versions",
    "log_report", "num_vars_to_attr", "pd_aux_attr", "pd_change_report",
    "pd_cpfw_merge", "pd_deflate_pipeline", "pd_deflation", "pd_dlw_clean",
    "pd_process_data", "pd_split_alt_welfare", "pd_wbpip_clean",
    "pipdata_dlw_process", "pipdata_get_gmd", "pipdata_int",
    "pipdata_validate_gmd", "ppp_to_wide", "process_data", "save_pip_data",
    "uniq_vars_to_attr", "unq_obs_dt", "valid_aux_load", "valid_dlw_load",
    "vars_to_attr", "wbpip_clean"
  ))
  expect_identical(exports, sort(c(baseline_exports, "pd_run_pipeline")))
  expect_identical(setdiff(exports, baseline_exports), "pd_run_pipeline")
  expect_true("S3method(print,pipdata_pipeline_result)" %in% namespace)
  expect_identical(
    getS3method("print", "pipdata_pipeline_result", optional = TRUE),
    print.pipdata_pipeline_result
  )
})

test_that("V14 return aliases and sentinels remain compatible", {
  master <- data.table::data.table(pip_id = "P1", deflated = FALSE)
  testthat::local_mocked_bindings(
    pd_deflate_pipeline_core = function(...) list(master = master),
    .package = "pipdata"
  )
  legacy <- pd_deflate_pipeline(master, FALSE, FALSE)
  expect_s3_class(legacy, "data.table")
  expect_identical(legacy, master)
  expect_identical(data_level_column("area"), "area")
  expect_true(is.na(data_level_column("national")))
  expect_identical(.PD_PIPELINE_MEASURES, c("pfw", "cpi", "ppp", "pop", "gdp", "pce"))
  aliases <- c("pip", "pip_meta", "pip_deflated", "pip_master", "pip_inv")
  context <- list(
    roots = as.list(stats::setNames(file.path("root", aliases), aliases))
  )
  expect_identical(
    pd_pipeline_storage(context)$aliases,
    c(
      pip = "pip", pip_meta = "pip_meta", pip_deflated = "pip_deflated",
      pip_master = "pip_master", pip_inv = "pip_inv"
    )
  )
})
