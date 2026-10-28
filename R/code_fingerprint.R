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

pd_engine_digest <- function(x) {
  digest::digest(serialize(x, NULL, version = 3L), algo = "sha256",
                 serialize = FALSE)
}

pd_engine_names <- function(x) {
  is.list(x) && !is.object(x) && (!length(x) ||
    (!is.null(names(x)) && !anyNA(names(x)) && all(nzchar(names(x))) &&
       !anyDuplicated(names(x))))
}

pd_engine_value <- function(x, allow_na = FALSE) {
  # Only reviewed immutable base value representations, never reference objects.
  if (is.null(x)) return(TRUE)
  if (!typeof(x) %in% c("logical", "integer", "double", "complex", "character",
                        "raw", "list")) return(FALSE)
  if (is.object(x) && !all(class(x) %in%
      c("data.frame", "factor", "ordered", "Date", "POSIXct", "POSIXt"))) {
    return(FALSE)
  }
  if (is.list(x)) {
    if (!all(vapply(x, pd_engine_value, logical(1L), allow_na = allow_na))) return(FALSE)
  } else if (!allow_na && anyNA(x)) return(FALSE)
  attrs <- attributes(x)
  is.null(attrs) || all(vapply(attrs, pd_engine_value, logical(1L), allow_na = allow_na))
}

pd_engine_proof <- function(calculation, helpers, constants) {
  fail <- function(message) rlang::abort(message,
    class = "pipdata_engine_recipe_error")
  if (!pd_engine_names(helpers) || !pd_engine_names(constants)) {
    fail("Helpers and constants require unique nonempty names.")
  }
  bindings <- c(names(helpers), names(constants))
  if (anyDuplicated(bindings) || any(bindings %in% c("path", "parents"))) {
    fail("Proof bindings must be disjoint from each other and callback formals.")
  }
  functions <- c(list(calculation), unname(helpers))
  if (!all(vapply(functions, function(fn) is.function(fn) && !is.primitive(fn),
                  logical(1L)))) fail("Components must be ordinary R closures, not primitives.")
  if (!identical(names(formals(calculation)), c("path", "parents"))) {
    fail("Calculation requires exactly (path, parents) formals.")
  }
  if (!all(vapply(constants, pd_engine_value, logical(1L)))) {
    fail("Constants require complete immutable values without missing values.")
  }
  forbidden <- c("<<-", "->>", "assign", "delayedAssign", "makeActiveBinding",
    "get", "get0", "mget", "exists", "eval", "evalq", "eval.parent", "parse",
    "str2lang", "str2expression", "do.call", "match.fun", "environment",
    "environment<-", "parent.env", "parent.frame", "sys.frame", "sys.function",
    "sys.call", "sys.calls", "sys.frames", "sys.parents", "sys.nframe",
    "globalenv", "as.environment", "asNamespace", "getNamespace", "getFromNamespace",
    "getExportedValue", "getS3method", "UseMethod", "NextMethod", ":::",
    ".Call", ".C", ".Fortran", ".External", ".Internal", ".Primitive",
    "options", "getOption", "Sys.getenv", "Sys.setenv", "Sys.unsetenv",
    "Sys.time", "Sys.Date", "Sys.info", "Sys.getpid", "getwd", "setwd",
    "system", "system2", "shell", "source", "sys.source", "load", "save",
    "saveRDS", "readRDS", "serialize", "unserialize", "file", "url",
    "gzfile", "bzfile", "xzfile", "pipe", "socketConnection", "unlink",
    "dir.create", "list.files", "list.dirs", "dir", "file.copy", "file.rename",
    "file.remove", "write", "writeLines", "writeBin", "readLines", "readBin",
    "cat", "sink", "library", "require", "loadNamespace", "attach", "detach",
    "unlockBinding", "lockBinding", "lockEnvironment", "bindingIsActive",
    "set.seed", "RNGkind", "sample", "sample.int", "runif", "rnorm",
    "on.exit", "reg.finalizer", "trace", "untrace", "debug", "browser")
  reads <- list(pipload = c("pip_read"), stamp = c("st_execution_info",
    "st_read_sidecar", "st_load_version"),
    rlang = "abort")
  pure_base <- c("{", "(", "if", "else", "for", "while", "repeat", "break",
    "next", "return", "function", "<-", "=", "->", "+", "-", "*", "/", "^",
    "%%", "%/%", ":", "[", "[[", "$", "[<-", "[[<-", "$<-", "!", "&", "&&",
    "|", "||", "==", "!=", "<", ">", "<=", ">=", "%in%", "~", "list", "c",
    "length", "lengths", "names", "names<-", "unname", "attr", "attr<-",
    "attributes", "attributes<-", "dim", "dim<-", "dimnames", "dimnames<-",
    "nrow", "ncol", "seq_len", "seq_along", "seq.int", "rep", "rep.int",
    "sum", "prod", "min", "max", "range", "abs", "sqrt", "log", "log10",
    "log2", "exp", "round", "floor", "ceiling", "trunc", "sign", "mean",
    "any", "all", "anyNA", "which", "which.min", "which.max", "identical",
    "is.null", "is.na", "is.nan", "is.finite", "is.infinite", "is.list",
    "is.logical", "is.integer", "is.double", "is.numeric", "is.character",
    "is.atomic", "is.vector", "is.matrix", "is.data.frame", "isTRUE", "isFALSE",
    "as.logical", "as.integer", "as.double", "as.numeric", "as.character",
    "as.list", "as.vector", "as.matrix", "as.data.frame", "data.frame", "matrix",
    "array", "structure", "paste", "paste0", "sprintf", "nchar", "nzchar",
    "tolower", "toupper", "trimws", "substr", "substring", "grepl", "grep",
    "sub", "gsub", "strsplit", "startsWith", "endsWith", "match", "unique",
    "duplicated", "anyDuplicated", "sort", "order", "rev", "head", "tail",
    "intersect", "union", "setdiff", "setequal", "stop", "stopifnot", "invisible")
  # These generics/conversions have implicit methods or ambient locale/timezone
  # inputs. The narrow recipe protocol does not claim to fingerprint that state.
  pure_base <- setdiff(pure_base, c("mean", "as.character", "as.logical",
    "as.integer", "as.double", "as.numeric", "as.list", "as.vector", "as.matrix",
    "as.data.frame", "data.frame", "structure", "attr<-", "attributes<-",
    "sort", "order", "tolower", "toupper", "sprintf", "grep", "grepl", "sub",
    "gsub", "strsplit", "unique", "duplicated", "anyDuplicated"))
  pure_base <- c(pure_base, "unclass")
  visit_arguments <- function(expr) {
    entries <- as.list(expr)[-1L]
    for (i in seq_along(entries)) {
      if (!identical(entries[[i]], quote(expr = ))) inspect_call(entries[[i]])
    }
  }
  inspect <- function(expr) {
    if (is.pairlist(expr) || is.expression(expr)) {
      lapply(as.list(expr), inspect)
    } else if (is.call(expr)) {
      head <- expr[[1L]]
      if (!is.symbol(head)) fail("Unsupported dynamic function dispatch.")
      name <- as.character(head)
      if (name %in% forbidden) fail(paste("Unsupported dynamic or side-effect operation:", name))
      if (name == "::") {
        package <- as.character(expr[[2L]])
        fun <- as.character(expr[[3L]])
        if (length(package) != 1L || length(fun) != 1L ||
            !fun %in% reads[[package]]) fail("Unsupported qualified dependency.")
      } else {
        # A qualified call has a :: expression as its head; handled below.
        for (item in as.list(expr)[-1L]) inspect(item)
      }
    }
    invisible(NULL)
  }
  inspect_call <- function(expr) {
    if (is.call(expr) && is.call(expr[[1L]]) &&
        identical(expr[[1L]][[1L]], as.name("::"))) {
      inspect(expr[[1L]])
      visit_arguments(expr)
    } else if (is.call(expr)) {
      if (!is.symbol(expr[[1L]])) fail("Unsupported dynamic function dispatch.")
      if (as.character(expr[[1L]]) %in% forbidden) {
        fail(paste("Unsupported dynamic or side-effect operation:", expr[[1L]]))
      }
      if (!as.character(expr[[1L]]) %in% c(pure_base, names(helpers))) {
        fail("Unsupported dynamic or undeclared function dispatch.")
      }
      if (as.character(expr[[1L]]) %in% c("<-", "=", "->") &&
          is.call(expr[[2L]])) fail("Unsupported implicit replacement dispatch.")
      visit_arguments(expr)
    } else if (is.pairlist(expr) || is.expression(expr)) {
      for (item in as.list(expr)) inspect_call(item)
    }
    invisible(NULL)
  }
  private <- new.env(parent = baseenv())
  access <- new.env(parent = emptyenv())
  access$parents <- list()
  # Internal audited boundaries are not caller-supplied helpers. Their code is
  # separately included in component proof; effective parents belong to stamp.
  assign("::", pd_engine_read_boundary(access), private)
  operators <- setdiff(pure_base, c("{", "(", "if", "else", "for", "while",
    "repeat", "break", "next", "return", "function", "<-", "=", "->",
    "&&", "||", "list", "unclass", "invisible", "stop", "stopifnot"))
  operators <- operators[vapply(operators, function(op)
    exists(op, baseenv(), inherits = FALSE), logical(1L))]
  for (op in operators) assign(op, pd_engine_base_boundary(op), private)
  for (name in names(constants)) {
    assign(name, unserialize(serialize(constants[[name]], NULL, version = 3L)),
           envir = private)
  }
  copies <- lapply(functions, function(fn) {
    inspect_call(body(fn))
    # Defaults are executable code too.
    defaults <- formals(fn)
    for (i in seq_along(defaults)) {
      if (!identical(defaults[[i]], quote(expr = ))) inspect_call(defaults[[i]])
    }
    globals <- codetools::findGlobals(fn, merge = FALSE)
    free <- unique(c(globals$functions, globals$variables))
    free <- setdiff(free, c(bindings, "::"))
    unknown <- free[!vapply(free, function(name) exists(name, baseenv(),
                                                      inherits = FALSE), logical(1L))]
    if (length(unknown)) fail(paste("Undeclared free dependency:", paste(unknown, collapse = ", ")))
    mutable <- setdiff(globals$variables,
      c(bindings, "pi", "letters", "LETTERS", "month.abb", "month.name"))
    if (length(mutable)) fail("Unsupported undeclared or run-state value dependency.")
    unsupported <- setdiff(globals$functions, c(bindings, pure_base, "::"))
    if (length(unsupported)) fail(paste("Unsupported base/dynamic operation:",
                                       paste(unsupported, collapse = ", ")))
    fn <- utils::removeSource(fn)
    attributes(fn) <- NULL
    environment(fn) <- private
    fn
  })
  for (i in seq_along(helpers)) assign(names(helpers)[[i]], copies[[i + 1L]], envir = private)
  lockEnvironment(private, bindings = TRUE)
  out <- list(calculation = copies[[1L]], helpers = names(helpers),
              constants = names(constants), environment = private, access = access)
  out$components <- pd_engine_components(out)
  return(out)
}

pd_engine_components <- function(proof) {
  descriptor <- function(value, schema) list(hash = pd_engine_digest(value),
    algorithm = "sha256", schema = schema)
  out <- list(calculation = descriptor(pd_canonical_function(proof$calculation),
                                       "pipdata-r-function-v1"))
  out[["runtime:read-policy"]] <- descriptor(
    pd_canonical_function(pd_engine_read_boundary), "pipdata-read-policy-v1")
  out[["runtime:base-policy"]] <- descriptor(
    pd_canonical_function(pd_engine_base_boundary), "pipdata-base-policy-v1")
  out[["runtime:closure-policy"]] <- descriptor(
    pd_canonical_function(pd_engine_proof), "pipdata-closure-policy-v1")
  for (name in sort(proof$helpers)) {
    out[[paste0("helper:", name)]] <- descriptor(pd_canonical_function(
      get(name, proof$environment, inherits = FALSE)), "pipdata-r-function-v1")
  }
  for (name in sort(proof$constants)) {
    out[[paste0("constant:", name)]] <- descriptor(
      get(name, proof$environment, inherits = FALSE), "pipdata-r-value-v1")
  }
  return(out[sort(names(out))])
}

pd_engine_base_boundary <- function(op) {
  primitive <- get(op, baseenv(), inherits = FALSE)
  if (op == "$") return(function(x, name) {
    if (is.object(x)) rlang::abort("Implicit S3 dispatch is unsupported; use unclass explicitly.")
    base::`[[`(x, as.character(substitute(name)))
  })
  if (op %in% c("[", "[[")) return(function(x, ...) {
    if (is.object(x)) rlang::abort("Implicit S3 dispatch is unsupported; use unclass explicitly.")
    primitive(x, ...)
  })
  function(...) {
    args <- list(...)
    has_character <- function(x) {
      is.character(x) || (is.list(x) && any(vapply(x, has_character, logical(1L))))
    }
    if (op %in% c("<", ">", "<=", ">=", "min", "max", "range") &&
        any(vapply(args, has_character, logical(1L)))) {
      rlang::abort("Ambient character collation is unsupported.")
    }
    if (any(vapply(args, is.object, logical(1L)))) {
      rlang::abort("Implicit S3 dispatch is unsupported; use unclass explicitly.")
    }
    do.call(primitive, args)
  }
}

pd_engine_read_boundary <- function(access) {
  function(pkg, name) {
    package <- as.character(substitute(pkg))
    fun <- as.character(substitute(name))
    api <- getExportedValue(package, fun)
    if (package == "rlang" && fun == "abort") return(api)
    function(...) {
      args <- list(...)
      matched <- as.list(match.call(api,
        as.call(c(list(as.name("read")), args)), expand.dots = FALSE))[-1L]
      path_field <- if (fun == "pip_read") "id" else if (fun == "st_read_sidecar") "rel_path" else "path"
      pin_field <- if (fun == "pip_read") "version" else if (fun == "st_execution_info") "receipt_id" else "version_id"
      path <- matched[[path_field]]
      pin <- matched[[pin_field]]
      if (fun == "pip_read" && (!pd_engine_string(matched$format) ||
          !matched$format %in% c("rds", "qs2", "fst"))) {
        rlang::abort("Parent reads require an explicit supported format.")
      }
      if (!pd_engine_string(path) || !pd_engine_string(pin) ||
          tolower(trimws(pin)) %in% c("latest", "current", "available", "oldest", "select", "pick", "choose", "0") ||
          grepl("^[+-][0-9]+$", pin)) rlang::abort("Reads require an explicit exact declared parent pin.")
      matching <- Filter(function(parent) {
        identical(tolower(as.character(fs::path_abs(path))), tolower(parent$path)) &&
          (fun == "st_execution_info" || identical(pin, parent$version_id))
      }, access$parents)
      if (length(matching) != 1L) rlang::abort("Read is not an effective declared parent path/version.")
      value <- do.call(api, args)
      if (fun == "st_execution_info" &&
          (!identical(value$state, "success") ||
           !identical(value$version_id, matching[[1L]]$version_id))) {
        rlang::abort("Exact parent receipt does not prove the declared version.")
      }
      value
    }
  }
}
