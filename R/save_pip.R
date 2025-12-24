save_pip_data <- function(data, dir, test = FALSE) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (dir == "pip_data") {
    dir <- pipfun::get_pip_folders()$pip_data
  } else if (dir == "pip_metadata") {
    dir <- pipfun::get_pip_folders()$pip_metadata
  } else {
    cli::cli_abort("Need to specified the directory")
  }

  # }

  versions <- purrr::map2(.x = data, .y = names(data), .f = \(x, y) {
    # on.exit ------------
    on.exit({
      rm(id_name, envir = .pipdataenv)
    })

    id_name <- y

    assign("id_name", id_name, envir = .pipdataenv)

    tryCatch(
      expr = {
        Sys.sleep(.9)

        # Save data

        pipload::pip_write(dir = dir, x = x, id = y)

        # Get last version

        #  vers <- pins::pin_versions(board = board,
        #                     name  = y)

        #  vers[rev(order(vers$created)),][1,]
      },
      error = function(cnd) {
        id_name <- c(.pipdataenv$id_name)

        pipfun::log_add(
          event = "error",
          message = cnd$message,
          name = "pipdata_log",
          .trace = cnd$call,
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
