# Test of pipdata functions
# Check packages updates
metapip::update_pip_packages()
# metapip::init_metapip()

#----- Load libraries and set release---

library(devtools)
load_all()

release <- "20250203"
identity <- "TEST"
pipfun::setup_working_release(release, verbose = FALSE)

#----- Load inventory to clean -----

inv <- suppressMessages(pipload::load_gmd_valid_inv())

inv_to_clean  <- valid_dlw_load(inv, aux_measures = c("pfw"))

#--------- Clean surveys and create metadata -----

clean_data <- pd_process_data(inv_to_clean)

#--------- Save clean_data and metadata------

versions_data <- save_pip_data(clean_data,
                               board = "pip_data")


versions_metadata <- save_pip_data(metadata,
                                   board = "pip_metadata")

# Create or Update inventory

new_pip_inv <- update_pip_inventory(inv_to_clean          = inv_to_clean,
                                    clean_data            = clean_data,
                                    pins_versions_data     = versions_data,
                                    pins_versions_metadata = versions_metadata)


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
