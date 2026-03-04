### Test of pipdata functions

# Check packages updates
metapip::update_pip_packages()

# ----- Load libraries and set release-----

library(devtools)
load_all()

release <- "20260206"
identity <- "TEST"

# Create a New PIP Release (NPR)
# pipfun::new_pip_release(release = release,
#                        identity = identity,
#                        root_dir = Sys.getenv("PIP_ROOT_DIR"))
# wrkrl <- pipfun::get_latest_pip_release()
# pipfun::setup_working_release(
#   wrkrl$release,
#   wrkrl$identity,
#   main_dir = fs::path(
#     Sys.getenv("PIP_ROOT_DIR"),
#     "PIP_ingestion_pipeline_v2/testing_folder"
#   ),
#   verbose = FALSE
# )

pipfun::setup_working_release(
  release,
  identity,
  verbose = FALSE
)

# ----- Load inventory to clean -----
# stamp::st_load("indicators.qs2", alias = "aux")
# valid_inv <- stamp::st_load("gmd_valid_inv.qs2", alias = "dlw_meta")

inv <- pipload::load_gmd_valid_inv()

# ------- Filter valid inventory until specific date ---------

# # Can only be run when Rossi fix the compare functions
# inv_to_clean <- valid_dlw_load(inv = inv, filter = "all") # It can be all, compare, random. Compare means compare to previous inventory

# inv <- as.data.table(inv)
inv_to_clean <- inv[status == "valid"]

# COL_inv <- inv_to_clean[country_code == "COL", ]

#--------- Clean surveys and create metadata -----

process_data <- pd_process_data(inv_to_clean = inv_to_clean)

#--------- Create or Update inventory---------

# old_pip_inv <- pipload::load_pip_master_inventory()

new_pip_inv <- update_pip_inventory(
  inv_to_clean = COL_inv,
  process_data = process_data
)

waldo::compare(old_pip_inv, new_pip_inv)

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
