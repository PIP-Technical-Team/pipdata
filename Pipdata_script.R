# Test of pipdata functions

#----- Load libraries and set release---

library(devtools)
load_all()

release <- "20250203"
pipfun::setup_working_release(release)


# Load inventory from validated DLW:
inv <- pidpata_dlw_gmd_inv()

#----- Load inventory to clean -----

inv_to_clean  <- valid_dlw_load(inv, aux_measures = c("pfw"))

#--------- Clean surveys -----

clean_data <- pd_process_data(inv_to_clean)

#--------- Validate -----------


#--------- Create metadata-----

metadata <- pd_aux_attr(clean_data = clean_data,
                        aux_measures = c("cpi","ppp"))

#--------- Save clean_data and metadata------

versions_data <- save_pip_data(clean_data,
                               board = "pip_data")


versions_metadata <- save_pip_data(metadata,
                               board = "pip_metadata")

# Create or Update inventory



# Check log
pipfun::log_filter(name = "pipdata_log")
pipfun::log_save(name = "pipdata_log", path = "log.qs")
log <- qs::qread("log.qs")
