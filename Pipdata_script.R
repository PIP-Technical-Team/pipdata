# Test of pipdata functions

#----- Load libraries and set release---

library(devtools)
load_all()

# Check packages updates
metapip::update_pip_packages()
metapip::init_metapip()

release <- "20250203"
pipfun::setup_working_release(release, verbose = FALSE)

# Load inventory from validated DLW:
inv <- pins::pin_read(board = pipfun::get_pins_boards(board = "dlw_metadata"),
                    name  = "gmd_valid_inv")

#----- Load inventory to clean -----

inv_to_clean  <- valid_dlw_load(inv, aux_measures = c("pfw"))

#--------- Clean surveys -----

clean_data <- pd_process_data(inv_to_clean)

#--------- Validate -----------

#valid_inv    <- pip_validation(clean_data)
#valid_data   <- valid_clean_data(valid_inv)

#--------- Create metadata-----

metadata <- pd_aux_attr(clean_data   = clean_data,
                        aux_measures = c("cpi","ppp"))

#--------- Save clean_data and metadata------

versions_data <- save_pip_data(clean_data,
                               board = "pip_data")


versions_metadata <- save_pip_data(metadata,
                                   board = "pip_metadata")

# Create or Update inventory

# inv_to_clean <- fix_inv(inv = pidpata_dlw_gmd_inv(),
#                         inv_to_clean = inv_to_clean)

new_pip_inv <- update_pip_inventory(inv_to_clean          = inv_to_clean,
                                    clean_data            = clean_data,
                                    pins_versions_data     = versions_data,
                                    pins_versions_metadata = versions_metadata)


# Check log
# pipfun::log_filter(name = "pipdata_log")
# pipfun::log_save(name = "pipdata_log", path = "log.qs")
# log <- qs::qread("log.qs")

