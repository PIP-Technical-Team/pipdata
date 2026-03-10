save_pip_data <- function(data, alias) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  versions <- purrr::map2(.x = data, .y = names(data), .f = \(x, y) {
    # on.exit ------------
    on.exit({
      rm(id_name, envir = .pipdataenv)
    })

    id_name <- y

    assign("id_name", id_name, envir = .pipdataenv)

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
        id_name <- c(.pipdataenv$id_name)

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
