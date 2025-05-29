# Test of pipdata functions

#----- Load libraries and set release---

library(devtools)
load_all()

pipfun::setup_working_release("20250203")


#----- Temporary load data -----

# Load inventory from validated DLW

# inv  <- pipload::pip_load_dlw_inventory()
inv          <- qs::qread(file.path(Sys.getenv("PIP_ROOT_DIR"),"DLW-OUTPUT/_Inventory/_release/pip_raw_inventory_20250203_TEST.qs"))
inv$fullname <- file.path(path, basename(inv$pip_file_path)) # Small fix for now
# val_rep <-  qs::qread(file.path(path, "_Inventory/_release/validation_report.qs"))

# Sample files for now
set.seed(1089)
n    <- 20 # Number of random surveys loaded
selected   <- sample(1:nrow(inv), n)
inv_smp    <- inv[selected,]

# Load data (just sample)
ls  <- valid_dlw_load(inv_smp) # Change folder name if it changes

aux <- valid_aux_load() # It gives a list of the surveys to be updated

#--------- Run pipdata functions -----

# Process data
results <- lapply(ls,
                  process_data,
                  pfw = pfw_aux)


# Create aux attributes


# aux_pfw_key -> creates reporting level variable
# pfw  <- pipload::pip_load_aux("pfw")
# pfw_aux  <- pipaux::load_aux("pfw", maindir = fs::path(Sys.getenv("PIP_ROOT_DIR"),"PIP_ingestion_pipeline_V2"))
# ppp  <- pipload::pip_load_aux("ppp")
# cpi  <- pipload::pip_load_aux("cpi")
# pop  <- pipload::pip_load_aux("pop")
# gdo  <- pipload::pip_load_aux("gdp")

# Merge aux file
# pfw_all  <- qs::qread("cpi_pop_gdp_ppp_pfw.qs")
# pfw_all <- pfw_all[order(country_code,year),]
# pfw_ur <- pfw_all[pfw_all$reporting_level!="national",]

# # Clean NA results
# clean_res <- Filter(Negate(is.na), results)
#
# # Deflation
# delfated <- lapply(clean_res, pd_deflation,
#                          cpi = cpi,
#                          ppp = ppp,
#                          pop = pop)

# Print errors
.logenv$piperr
.logenv$unk_err
