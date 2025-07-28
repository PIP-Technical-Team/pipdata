pip_load <- function(pip_board,
                     file_name,
                     release = NULL,
                     version = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # check if pin board folder exists -------------------
  # if (!dir.exists(pip_board)) {
  #   folder <- fs::path_file(pip_board)
  #
  #   cli::cli_abort(
  #     "Pin folder ({folder}) is not avaiable"
  #   )
  #
  # }

  # Check release
  if(is.null(release)){

    release <- pipfun::get_wrk_release(verbose = FALSE)

  }



  # Load from board
  dt <- pins::pin_read(board = pip_board, name = file_name, version = version)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(dt)

}
