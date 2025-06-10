# Test of pipdata functions

#----- Load libraries and set release---

library(devtools)
load_all()

release <- "20250203"
pipfun::setup_working_release(release)

#----- Temporary load data -----

# Load inventory from validated DLW:
# inv  <- pipload::pip_load_dlw_inventory()

folder <- "DLW-OUTPUT"
# inv_files <- fs::dir_ls(fs::path(Sys.getenv("PIP_ROOT_DIR"),folder,"/_Inventory/_release"))
name_inv <- "pip_raw_inventory_20250203_TEST.qs"

inv   <- qs::qread(fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                   folder,
                                   "/_Inventory/_release",
                                   name_inv))

# Check validation report:
# val_rep <-  qs::qread(file.path(path, "_Inventory/_release/validation_report.qs"))

# Create mock changes for the inventory
inv_to_be_cleaned <- m_inv_load(inv)

# Load data
ls  <- valid_dlw_load(inv_to_be_cleaned)

# Load PFW
pfw_aux  <- pipaux::load_aux("pfw", maindir = fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                                       "PIP_ingestion_pipeline_V2"))

# aux_pfw_key -> creates reporting level variable
# pfw  <- pipload::pip_load_aux("pfw")
# ppp  <- pipload::pip_load_aux("ppp")
# cpi  <- pipload::pip_load_aux("cpi")
# pop  <- pipload::pip_load_aux("pop")
# gdo  <- pipload::pip_load_aux("gdp")

#--------- Run pipdata functions -----

# Process data
results <- lapply(ls,
                  process_data,
                  pfw = pfw_aux)


# # Clean NA results
# clean_res <- Filter(Negate(is.na), results)
#
# # Deflation
# delfated <- lapply(clean_res, pd_deflation,
#                          cpi = cpi,
#                          ppp = ppp,
#                          pop = pop)

