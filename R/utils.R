#' Identify unique variables in data frame
#' @param x data frame.
#'
#' @return character vector of unique variable names
#' @noRd
uniq_vars <- function(x) {
  x <- check_data_table(x)
  N_vars <- x[, lapply(.SD, uniqueN)]
  uni_vars <- names(N_vars)[N_vars == 1]

  return(uni_vars)
}

#' Turn data to data.table if it is not already
#' @noRd
check_data_table <- function(x) {
  if (!is.data.table(x)) {
    x <- collapse::qDT(x)
  }
  x
}

#' convert variables with unique values along the data set to attributes and then
#' remove those unique variables
#'
#' @param x data frame.
#'
#' @return list of single-value variables from dataframe `x`
#' @noRd
uniq_vars_to_list <- function(x) {
  uni_vars <- uniq_vars(x)

  y <- x[, lapply(.SD, unique), .SDcols = uni_vars]

  as.list(y)
}

#' Return a named list with unique values of variables
#'
#' @param x A data.table
#' @param vars variable to be turn to attributes.
#' @param nm variables for naming attributes
#'
#' @return a named list with unique values
#'
vars_to_list <- function(x, vars, nm = NULL) {
  var1 <- lapply(x[, ..vars], unique)
  if (!is.null(nm)) {
    var2 <- lapply(x[, ..nm], unique)
    if (!all(mapply(\(x, y) length(x) == length(y), var1, var2))) {
      cli::cli_abort(
        "The unique values in {.arg num_var} and {.arg name_var} column are not equal"
      )
    }
    var1 <- Map(stats::setNames, var1, var2)
  }
  var1
}

#' convert variables with unique values along the data set to attributes and then
#' remove those unique variables
#'
#' @param x a data.frame
#' @param exclude_vars variables to be excluded from turning to attributes (default NULL)
#'
#' @return data.frame with multiple-value variables only and single-value
#'   variables as attributes
#' @export
#' @examples
#' dt <- data.table::data.table(a = 1, b = 1:10, c = 5)
#' out <- uniq_vars_to_attr(dt)
#' out[]
#' attr(out, "a")
#' attr(out, "c")
#'
#' # Exclude `a` from being added as attribute
#' out <- uniq_vars_to_attr(dt, "a")
#' out[]
#'
#' # var `a` is not included as part of the attributes
#' attr(out, "a")
#'
#' # Var `c` is
#' attr(out, "c")
uniq_vars_to_attr <- function(x, exclude_vars = NULL) {
  nm <- names(x) |>
    copy() # make sure names are not modified by reference
  # Doing everything on copy of x since we want to preserve x in it's original form
  x1 <- copy(x)

  # Drop exclude_vars columns
  if (!is.null(exclude_vars)) {
    # Make sure that the column names in exclude_vars is a part of data
    if (!all(exclude_vars %in% nm)) {
      ev <- exclude_vars[!exclude_vars %in% nm]
      cli::cli_abort(
        "{.var {ev}} {?is/are} not {?a/} column name{?s} in data.
                     Choose one of {.var {nm}}"
      )
    }

    #Dropping columns from x1
    x1[, (exclude_vars) := NULL]
  }
  uvl <- uniq_vars_to_list(x1)

  uni_vars <- names(uvl)
  mul_vars <- setdiff(nm, uni_vars)
  x <- change_vars_to_attr(x, uvl)
  x <- x[, ..mul_vars]

  return(x)
}

change_vars_to_attr <- function(df, uvl) {
  for (i in seq_along(uvl)) {
    var <- names(uvl)[i]
    value <- uvl[[i]]

    # make sure that attributes are set correctly for data.table.
    if (inherits(df, "data.table")) {
      setattr(df, var, value)
    } else {
      attr(df, var) <- value
    }
  }
  df
}


#' Get path to pipdata original files
#'
#' pipdata comes bundled with a number of internal datasets originally created
#' in CSV format and then converted to proper R format. They are placed in  its
#' `inst/extdata` directory. This function make them easy to access. This
#' function is based (mainly copied) from `readr_example` in the `readr` package
#'
#' @param file Name of file. If `NULL`, the internal files will be listed.
#' @export
#' @examples
#' pipdata_int()
#' pipdata_int("pip_pc_var_type.csv")
pipdata_int <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "pipdata"))
  } else {
    system.file("extdata", file, package = "pipdata", mustWork = TRUE)
  }
}


#' Make vars as attributes
#'
#' @param df A data.frame
#' @param vars variables to changed to attributes
#'
#' @return A data.frame with vars variables as attributes
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- data.table(a = c(1, 2), b = 1:10, c = 5)
#' out <- vars_to_attr(dt, "a")
#' }
vars_to_attr <- function(df, vars) {
  df <- check_data_table(df)
  uvl <- vars_to_list(df, vars)
  df <- change_vars_to_attr(df, uvl)
  df[, !..vars]
}


#' Create a named vector of attributes
#'
#' @param df A data.frame
#' @param num_var Column name with numerical values
#' @param name_var Column name with name values
#'
#' @return Data.table with named attributes
#' @export
#'
#' @examples
#' \dontrun{
#'  dt <- data.table(a = c(1, 2), b = 1:10, c = c("a", "b"))
#'  out <- num_vars_to_attr(dt, "a", "c")
#' }
num_vars_to_attr <- function(df, num_var, name_var) {
  dt <- check_data_table(df)

  if (length(num_var) != length(name_var)) {
    cli::cli_abort(
      "{.arg num_var} and {.arg name_var} should be of same length.
                   You have passed {length(num_var)} variable{?s} in {.arg num_var}
                   whereas {.arg name_var} consists of {length(name_var)} variable{?s}."
    )
  }
  uvl <- vars_to_list(dt, num_var, name_var)
  dt <- change_vars_to_attr(dt, uvl)
  c_col <- c(num_var, name_var)
  dt[, !..c_col]
}

#' Customized PIP error
#'
#' @param message message
#' @param name name assigned to the error. When "skip"
#'
#' @return error
#' @keywords internal
piperr <- function(message, name = "skip") {
  svy <- pd_env_get("log_survey_id")

  rlang::abort(
    message = message,
    class = c(name, "piperr"),
    id = svy,
    call = sys.call(sys.parent()),
    use_cli_format = TRUE
  )
}

#' Add errors to the package environment
#'
#' @param line line to be added to the log
#' @param class PIP error or warning class. Values are stored in `.pipdataenv`
#'   under the key `paste0("log_", class)`. Currently used values:
#'   `"piperr"` (stored as `"log_piperr"`) and `"unk_err"` (stored as
#'   `"log_unk_err"`). Retrieve with `pd_env_get(paste0("log_", class))`.
#' @param error name of error or warning list
#'
#' @return Updated error list stored in `.pipdataenv` under `paste0("log_", class)`.
#' @keywords internal
add_log <- function(line, error = NULL, class = "piperr") {
  # Key convention: paste0("log_", class) — e.g. class="piperr" => "log_piperr"
  log_key <- paste0("log_", class)
  if (is.null(pd_env_get(log_key))) {
    pd_env_set(log_key, list())
  }

  # load list
  log_list <- pd_env_get(log_key)

  key <- if (is.null(error)) "unknown errors" else error

  # Check if the error name already exists
  if (key %in% names(log_list)) {
    log_list[[key]][[1]] <- append(log_list[[key]][[1]], line)
  } else {
    log_list[[key]] <- list(line)
  }

  pd_env_set(paste0("log_", class), log_list)

  invisible()
}


#' Add new attributes to data.table
#'
#' @param dt data.table which is missing the new attributes
#' @param new_attrs list with new attributes
#'
#' @return data.table
#' @noRd
add_attributes <- function(dt, new_attrs) {
  for (name in names(new_attrs)) {
    attr(dt, name) <- new_attrs[[name]]
  }

  return(dt)
}

char_to_fct <- function(dt) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chr_vars <- names(collapse::char_vars(dt))

  dt[,
    (chr_vars) := lapply(.SD, kit::charToFact),
    .SDcols = chr_vars
  ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)
}


#' Log the error
#'
#' @param e condition from the error
#'
#' @return NULL
#' @keywords internal
log_failure <- function(e) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  root <- find_condition(e, "piperr")

  if (!is.null(root)) {
    line <- sprintf(
      "[%s] %s for %s",
      ts,
      cli::ansi_strip(conditionMessage(root)),
      root$id
    )
    add_log(line, error = deparse(root$call[[1]]), class = "piperr")
  } else {
    line <- sprintf(
      "[%s] %s for %s",
      ts,
      cli::ansi_strip(conditionMessage(e)),
      deparse(conditionCall(e))
    )
    add_log(line, error = deparse(e$call[[1]]), class = "unk_err")
  }

  return(NULL)
}


find_condition <- function(cnd, class) {
  while (!is.null(cnd)) {
    if (inherits(cnd, class)) {
      return(cnd)
    }
    cnd <- cnd$parent
  }
  NULL
}


last_ver_inv <- function(dt) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # dt <- copy(dt)

  dt <- dt[,
    # Get max master version and filter
    maxmast := vermast == max(vermast),
    by = .(country_code, surveyid_year, survey_acronym, module, tool)
  ][
    maxmast == TRUE
  ][,
    # Get max veralt version and filter
    maxalt := veralt == max(veralt),
    by = .(country_code, surveyid_year, survey_acronym, module, tool)
  ][
    maxalt == TRUE
  ][,
    # Get max pip version and filter
    maxpip := pipeline_version == max(pipeline_version),
    by = .(country_code, surveyid_year, survey_acronym, module, tool)
  ][
    maxpip == TRUE
  ][,
    c("maxmast", "maxalt", "maxpip") := NULL
  ][
    status == "valid"
  ][
    module %in% c("GPWG", "GROUP", "BIN", "ALL", "HIST")
  ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)
}


#' Find unique values in PFW according to some key variables
#'
#' @param dt data.table or data.frame
#' @param keyVar character vector with variables to determine unique observations
#'
#' @return data.table or data.frame
#' @export
#'
#' @examples
#' release <- "20260401"
#' pipfun::setup_working_release(release)
#'
#' pfw <- pipload::load_aux_data("pfw")
#' keyVar <- c("country_code", "survey_year", "survey_acronym", "welfare_type")
#' unq_obs_dt(pfw, keyVar)
unq_obs_dt <- function(dt, keyVar) {
  if (uniqueN(dt, by = keyVar) != nrow(dt)) {
    dt_d <- dt[duplicated(dt, by = keyVar)]
    n_rep <- nrow(dt_d)

    cli::cli_abort(
      "There {?is/are} {n_rep} duplicates in PFW",
      class = c("piperr", "dup_pfw")
    )
  }

  return(dt)
}

#' Resolve current content hashes for auxiliary measures from the aux catalog
#'
#' Queries the `"aux"` stamp catalog once and returns the current
#' `content_hash` for each requested auxiliary measure. Each measure is
#' matched to exactly one catalog artifact whose path basename is
#' `<measure>.qs2` (e.g. `cpi.qs2`, `ppp.qs2`, `pfw.qs2`).
#'
#' @param aux_measures Character vector of auxiliary measures to resolve.
#' @param verbose Logical. Print progress messages. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#'
#' @return A named character vector of `content_hash` values, one per
#'   requested measure. Names are the measure names.
#'
#' @details
#' This is the single source of the current aux hashes used to gate
#' aux-change detection in [valid_dlw_load()]. It must be called once per
#' pipeline run, before aux data is loaded, and the result passed through
#' the run so that the hashes recorded in the master inventory match the
#' aux data actually used.
#'
#' The function aborts loudly when the `"aux"` alias is unavailable, a
#' requested artifact is missing, or multiple catalog rows match a measure.
#' It never falls back to `stamp::st_latest()` or to hashing loaded aux
#' tables.
#'
#' @family pd_process_data pipeline
#' @keywords internal
get_aux_hashes <- function(
  aux_measures,
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat_aux <- tryCatch(
    stamp::st_catalog_query(alias = "aux"),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not query the aux stamp catalog.",
          "i" = "Ensure the working release is set up with an aux_data folder.",
          "x" = "Error: {conditionMessage(e)}"
        ),
        class = c("get_aux_hashes_catalog_failure", "piperr")
      )
    }
  )

  if (is.null(cat_aux) || nrow(cat_aux) == 0L) {
    cli::cli_abort(
      "The aux stamp catalog is empty. Cannot resolve auxiliary hashes.",
      class = c("get_aux_hashes_empty_catalog", "piperr")
    )
  }

  # Derive the artifact basename (e.g. "cpi.qs2") from each catalog path.
  cat_aux[, artifact := fs::path_file(path)]

  hashes <- vapply(aux_measures, function(m) {
    target <- paste0(m, ".qs2")
    matches <- cat_aux[artifact == target]

    if (nrow(matches) == 0L) {
      cli::cli_abort(
        "No aux catalog artifact found for measure {.val {m}} (expected {.val {target}}).",
        class = c("get_aux_hashes_missing_artifact", "piperr")
      )
    }
    if (nrow(matches) > 1L) {
      cli::cli_abort(
        "Multiple aux catalog artifacts match measure {.val {m}} ({.val {target}}).",
        class = c("get_aux_hashes_ambiguous_artifact", "piperr")
      )
    }

    hash <- matches$content_hash[[1L]]
    if (is.na(hash) || !nzchar(hash)) {
      cli::cli_abort(
        "Aux catalog artifact for measure {.val {m}} has no content_hash.",
        class = c("get_aux_hashes_missing_hash", "piperr")
      )
    }

    hash
  }, character(1))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(hashes)
}
