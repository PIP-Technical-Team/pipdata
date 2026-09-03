.PD_FINGERPRINT_COMPONENTS <- list(
  clean = c("inv_dlw_load", "pd_cpfw_merge", "pd_dlw_clean", "apply_recode_spec",
            "pd_wbpip_clean", "pd_split_alt_welfare", "expected_pip_ids",
            "pd_expected_clean_pip_ids", "pd_exact_pfw_projection",
            "pd_dependency_key_adapter", "pd_canonical_projection",
            "pd_hash_object",
            "data_to_dt", "get_country_pfw", "load_stamp_recode_spec",
            "recode_binary", "recode_binned", "recode_haven",
            "recode_indicator", "recode_quantile", "recode_range",
            "survey_id_to_attr", "cache_id", "report_lvl", "pd_env_get",
            "dlw_clean", "dlw_clean.pipmd", "dlw_clean.pipgd",
            "wbpip_clean", "wbpip_clean.pipmd", "wbpip_clean.pipgd",
            "add_dist_type", "add_dist_type.pipmd", "add_dist_type.pipgd",
            "add_area", "add_area.pipmd", "add_area.pipgd",
            "get_gd_type", "area_gd_clean", "format_wgt", "format_wlf",
            "shift_subnatid", "pip_vars"),
  metadata = c(
    "pd_aux_attr", "pd_metadata_refresh", "pd_normalize_metadata_keys",
    "pd_validate_metadata_base"
  ),
  deflate = c("pd_deflation_exact", "pd_deflation_exact_strict", "pd_deflation", "adjust_population",
              ".load_deflation_aux", "deflation", "deflation.pipmd",
              "deflation.pipgd", "safe_deflation", ".validate_deflation_input",
              ".deflation_pipmd_core", ".deflation_pipgd_core",
              "finalize_deflation_output", "add_aux", "welfare_lcu",
               "deflate_wlf", "char_to_fct", "ppp_to_wide", "add_cpi",
               "add_ppp", "cpi_ppp_years", "data_level_column", "log_failure", "pd_env_set",
              "pd_env_rm", "piperr", "add_log", "find_condition")
)

.PD_FINGERPRINT_CONSTANTS <- list(clean = ".DOMAIN_COLS")

.PD_EXTERNAL_FINGERPRINT_COMPONENTS <- list(
  clean = c("md_clean_data", "gd_clean_data", "md_compute_quantiles"),
  deflate = "deflate_welfare_mean"
)

pd_canonical_function <- function(fn) {
  fn <- utils::removeSource(fn)
  list(formals = formals(fn), body = body(fn))
}

pd_code_fingerprints <- function(components = .PD_FINGERPRINT_COMPONENTS,
                                  namespace = asNamespace("pipdata"),
                                  constants = .PD_FINGERPRINT_CONSTANTS,
                                  external_components =
                                    .PD_EXTERNAL_FINGERPRINT_COMPONENTS) {
  rows <- lapply(names(components), function(stage) {
    names <- sort(unique(components[[stage]]))
    hashes <- vapply(names, function(name) {
      if (!exists(name, envir = namespace, inherits = FALSE)) {
        rlang::abort(paste("Missing fingerprint component", name),
                     class = "pipdata_fingerprint_component_missing")
      }
      pd_hash_object(pd_canonical_function(get(name, envir = namespace)))
    }, character(1))
    data.table::data.table(stage, component = names, hash = hashes)
  })
  detail <- data.table::rbindlist(rows)
  if (identical(namespace, asNamespace("pipdata"))) {
    constant_rows <- lapply(names(constants), function(stage) {
      data.table::rbindlist(lapply(constants[[stage]], function(name) {
        if (!exists(name, envir = namespace, inherits = FALSE)) {
          rlang::abort(paste("Missing fingerprint constant", name),
                       class = "pipdata_fingerprint_component_missing")
        }
        data.table::data.table(
          stage, component = name,
          hash = pd_hash_object(get(name, envir = namespace, inherits = FALSE))
        )
      }))
    })
    recode_path <- system.file("extdata", "recode_spec.yml", package = "pipdata")
    recode_hash <- if (nzchar(recode_path)) digest::digest(file = recode_path, algo = "sha256") else NA_character_
    external <- lapply(names(external_components), function(stage) {
      data.table::rbindlist(lapply(external_components[[stage]], function(name) {
        fn <- get(name, envir = asNamespace("wbpip"), inherits = FALSE)
        data.table::data.table(
          stage, component = paste0("wbpip::", name),
          hash = pd_hash_object(pd_canonical_function(fn))
        )
      }))
    })
    detail <- data.table::rbindlist(c(list(detail), constant_rows, external,
      list(data.table::data.table(stage = "clean", component = "recode_spec.yml",
                                   hash = recode_hash))))
  }
  summary <- detail[, .(hash = pd_hash_object(stats::setNames(hash, component))), by = stage]
  list(summary = summary, components = detail,
       audit = list(pipdata = as.character(utils::packageVersion("pipdata")),
                    stamp = as.character(utils::packageVersion("stamp")),
                    pipload = as.character(utils::packageVersion("pipload"))))
}

pd_fingerprint_audit <- function(components = .PD_FINGERPRINT_COMPONENTS,
                                 namespace = asNamespace("pipdata")) {
  declared <- unique(unlist(components))
  calls <- unique(unlist(lapply(declared, function(name) {
    if (!exists(name, namespace, inherits = FALSE)) return(character())
    fn <- get(name, namespace)
    if (!is.function(fn)) return(character())
    globals <- codetools::findGlobals(fn, merge = FALSE)$functions
    intersect(globals, ls(namespace, all.names = TRUE))
  })))
  setdiff(calls, declared)
}
