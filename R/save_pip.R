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
#'
#' @return A named list of version metadata returned by
#'   [pipload::pip_write()], with `NULL` entries for failed saves.
#'
#' @family pd_process_data pipeline
#' @export
save_pip_data <- function(data, alias) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  versions <- purrr::map2(.x = data, .y = names(data), .f = \(x, y) {
    # on.exit ------------
    on.exit({
      pd_env_rm("save_id_name")
    })

    id_name <- y

    pd_env_set("save_id_name", id_name)

    tryCatch(
      expr = {
        # Sys.sleep(.9)

        # Save data
        res <- pipload::pip_write(x = x, id = y, alias = alias)

        # if ("skipped" %in% names(res) && res$skipped == TRUE) {
        #   pipfun::log_add(
        #     event = "warning",
        #     message = "The cleaned survey or metadata was not saved because it is identical to the previous version",
        #     name = "pipdata_log",
        #     logmeta = list(
        #       warning = "identical_version",
        #       id_name = id_name,
        #       status = "This survey or metadata was cleaned even though it has no changes compared to the previous version. Check why it was not filtered out before cleaning."
        #     )
        #   )
        #   return(NULL)
        # }
        return(res)
      },
      error = function(cnd) {
        id_name <- c(pd_env_get("save_id_name"))

        pipfun::log_add(
          event = "error",
          message = cnd$message,
          name = "pipdata_log",
          # .trace = cnd$call,
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(versions)
}
