### Test of pipdata functions

# Check packages updates
metapip::update_pip_packages()

#------- Load libraries and set release-----

library(devtools)
load_all()

release <- "20250203"
identity <- "TEST"
pipfun::setup_working_release(release, identity, verbose = FALSE)

#----- Load inventory to clean -----

inv <- suppressMessages(pipload::load_gmd_valid_inv())

# ------- Filter valid inventory until specific date ---------
# dt_v <- date_valid(inv, 3)

dt_v <- as.POSIXct("2025-08-10", tz = "UTC") # The previous to last validation

inv_to_clean  <- valid_dlw_load(inv = inv ,
                                date_valid = dt_v)

#--------- Clean surveys and create metadata -----

process_data <- pd_process_data(inv_to_clean = inv_to_clean)

#--------- Create or Update inventory---------

old_pip_inv <- pipload::load_pip_master_inventory()

new_pip_inv <- update_pip_inventory(inv_to_clean = inv_to_clean,
                                    process_data = process_data)



# board_master <- pipfun::get_pins_boards(board = "pip_master_inventory")
# tst <- pins::pin_read(board = board_master, name = "pip_master_inventory")
# vs <- pins::pin_versions(board = board_master, name = "pip_master_inventory")
# tst2 <- pins::pin_read(board = board_master, name = "pip_master_inventory", version = "20250819T173702Z-6d58c")

# Check log
# pipfun::log_filter(name = "pipdata_log")
# pipfun::log_save(name = "pipdata_log", path = "log.qs")
# log <- qs::qread("log.qs")


#------ Load data tests -----
#
# NIC <- pipload::find_pip_data(board = pipfun::get_pins_boards(board = "pip_data"),
#                              country_code = "NIC", where = "master", surveyid_year = 2001)
#
# NIC_2 <- pipload::load_pip_data(country_code = "NIC", surveyid_year = 2001)
