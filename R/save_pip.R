 save_pip_data <- function(data,
                           board,
                           test = FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # if(test){
   #
   #   if(board == "pip_data"){
   #
   #     board <- pins::board_folder("//tsclient/P/03.pip/pip_data/surveys")
   #
   #   }else if(board == "pip_metadata"){
   #
   #     board <- pins::board_folder("//tsclient/P/03.pip/pip_data/surveys_metadata")
   #
   #   }else{
   #
   #     cli::cli_abort("Need to specified the board")
   #
   #   }
   #
   # }else{

     if(board == "pip_data"){

       board <- pipfun::get_pins_boards()$pip_data

     }else if(board == "pip_metadata"){

       board <- pipfun::get_pins_boards()$pip_metadata

     }else{

       cli::cli_abort("Need to specified the board")

     }

   # }

   versions <- purrr::map2(.x = data,
                           .y = names(data),
                           .f = \(x, y){

                             # on.exit ------------
                             on.exit({
                               rm(pin_name,
                                  envir = .pipdataenv)
                             })

                             pin <- y

                             assign("pin_name",
                                    pin,
                                    envir = .pipdataenv)

                             tryCatch(
                               expr = {

                                 Sys.sleep(.9)

                                 # Save data

                                 pipload::pip_write(board                 = board,
                                                    x                     = x,
                                                    pin_name              = y)

                                 # Get last version

                                 vers <- pins::pin_versions(board = board,
                                                    name  = y)

                                 vers[rev(order(vers$created)),][1,]

                               },
                               error = function(cnd){

                                 pin_name <- c(.pipdataenv$pin_name)

                                 pipfun::log_add(event = "error",
                                                 message = cnd$message,
                                                 name = "pipdata_log",
                                                 .trace = cnd$call,
                                                 logmeta = list(error = "save_error",
                                                                pin_name = pin_name,
                                                                status = "The cleaned survey was not saved"))

                                 NULL

                               }
                             )
                           })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(versions)

 }

