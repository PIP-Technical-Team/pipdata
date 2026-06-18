#' Save cleaned PIP data or metadata to versioned storage
#'
#' Iterates over a named list of cleaned `data.table` objects and writes
#' each one to the PIP storage backend via [pipload::pip_write()].
#' Errors during individual saves are caught, logged, and returned as
#' `NULL` so that remaining surveys can continue.
#'
#' @param data A named list of `data.table` objects to save. Names
#'   are used as the `id` argument to [pipload::pip_write()].
#' @param alias Character scalar. The storage alias passed to
#'   [pipload::pip_write()] (e.g., `"pip"` for survey data,
#'   `"pip_meta"` for metadata).
#' @param verbose Logical. Controls verbosity of downstream
#'   [pipload::pip_write()] calls. Default:
#'   `getOption("pipdata.verbose", default = TRUE)`.
#'
#' @return A named list with one entry per artifact: `list(pip_id, success = TRUE)`
#'   on success or `NULL` on failure. Version metadata is persisted to the
#'   stamp catalog and read back by [build_pip_inventory()] — it is not
#'   returned here.
#'
#' @family pd_process_data pipeline
#' @export
save_pip_data <- function(
  data,
  alias,
  verbose = getOption("pipdata.verbose", default = TRUE)
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  versions <- lapply(names(data), \(y) {
    # on.exit ------------
    on.exit({
      pd_env_rm("save_id_name")
    })

    pd_env_set("save_id_name", y)

    tryCatch(
      expr = {
        # Save data (version metadata is persisted to stamp catalog)
        pipload::pip_write(x = data[[y]], id = y, alias = alias, verbose = verbose)

        list(pip_id = y, success = TRUE)
      },
      error = function(cnd) {
        id_name <- c(pd_env_get("save_id_name"))

        pipfun::log_add(
          event = "error",
          message = cnd$message,
          name = "pipdata_log",
          logmeta = list(
            error = "save_error",
            id_name = id_name,
            status = "The cleaned survey was not saved"
          )
        )

        NULL
      }
    )
  })

  names(versions) <- names(data)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(versions)
}

