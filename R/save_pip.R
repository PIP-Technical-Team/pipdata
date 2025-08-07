 save_pip_data <- function(data,
                           board) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if(board == "pip_data"){

     # Flatten list:
     data <- purrr::flatten(data)

     board <- pipfun::get_pins_boards()$pip_data

   }else if(board == "pip_metadata"){

     board <- pipfun::get_pins_boards()$pip_metadata

   }else{

     cli::cli_abort("Need to specified the board")

   }

   versions <- purrr::map2(.x = data,
                           .y = names(data),
                           .f = \(x, y){

                           pins::pin_write(board                 = board,
                                           x                     = x,
                                           name                  = y,
                                           force_identical_write = FALSE,
                                           type                  = "qs",
                                           versioned             = TRUE)

                           pins::pin_versions(board = board, name  = y)
                           })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(versions)

 }

