# Test of pipdata functions

#----- Load libraries and set release---

library(devtools)
load_all()

release <- "20250203"
pipfun::setup_working_release(release)

#----- Temporary load data -----

# Load inventory from validated DLW:
inv <- pidpata_dlw_gmd_inv()

inv_to_clean  <- valid_dlw_load(inv,
                                aux_measures = c("pfw"))



# Check for unique obs per pfw --------
# keyVar <- c("country_code", "survey_year", "survey_acronym", "welfare_type")
# pfw <- unq_obs_dt(pfw, keyVar)

#--------- Run pipdata functions -----

# Load PFW
pfw  <- pipload::pip_load_aux("pfw")

# Process data
inv_ls <- split(inv_to_clean,
                seq_len(nrow(inv_to_clean)))

results <- purrr::map(inv_ls,
                  process_data,
                  pfw = pfw)

names(results) <- inv_to_clean$survey_id

# Create metadata

metadata <- pd_aux_attr()

# Save results and metadata

# Create or Update inventory

# # Deflation
# delfated <- lapply(results, pd_deflation,
#                          cpi = cpi,
#                          ppp = ppp,
#                          pop = pop)

pipfun::log_filter(name = "pipdata_log")
pipfun::log_save(name = "pipdata_log", path = "log.qs")
log <- qs::qread("log.qs")
