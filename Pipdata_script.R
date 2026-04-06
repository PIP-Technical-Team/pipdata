### Test of pipdata functions

# Check packages updates
metapip::update_pip_packages()

# ----- Load libraries and set release-----

library(devtools)
load_all()

stamp::st_opts("warn_missing_pk_on_load", .get = TRUE)
stamp::st_opts(warn_missing_pk_on_load = FALSE)

release <- "20260206"
identity <- "TEST"

pipfun::setup_working_release(
  release,
  identity,
  verbose = FALSE
)

# ----- Load inventory to clean -----
inv <- pipload::load_gmd_valid_inv()

#--------- Clean surveys and create metadata -----
old_pip_inv <- pipload::load_pip_master_inventory(verbose = FALSE)

new_pip_inv <- pd_process_data(inv = inv)

waldo::compare(old_pip_inv, new_pip_inv)

# Check log
# pipfun::log_filter(name = "pipdata_log")
# pipfun::log_save(name = "pipdata_log", path = "log.qs")
# log <- qs::qread("log.qs")

#------ Load data tests -----
# # Load cleaned data for a survey
BOL <- pipload::load_pip_data(country_code = "BOL", surveyid_year = 2021)

