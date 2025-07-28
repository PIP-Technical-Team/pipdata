# Test of pipdata functions

#----- Load libraries and set release---

library(devtools)
load_all()

release <- "20250203"
pipfun::setup_working_release(release)

#----- Temporary load data -----

# Load inventory from validated DLW:
inv <- pipdata_load_report(report_type = "inventory")

# Load data
ls  <- valid_dlw_load(inv,
                      aux_measures = c("pfw", "ppp", "cpi", "pop"))

# Load PFW
pfw  <- pipload::pip_load_aux("pfw")

# Check for unique obs per pfw --------
keyVar <- c("country_code", "survey_year", "survey_acronym", "welfare_type")
pfw <- unq_obs_dt(pfw, keyVar)

#--------- Run pipdata functions -----

# Process data
results <- lapply(ls,
                  process_data,
                  pfw = pfw)

# Check if list has attributes with specific names
# dt <- find_dt_with_attribute(ls, attr_name = "country_code", attr_value = "PHL")[[3]]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load aux data --------

ppp  <- pipload::pip_load_aux("ppp")
cpi  <- pipload::pip_load_aux("cpi")
pop  <- pipload::pip_load_aux("pop")
gdp  <- pipload::pip_load_aux("gdp")


# # Clean NA results
# clean_res <- Filter(Negate(is.na), results)
#
# # Deflation
# delfated <- lapply(clean_res, pd_deflation,
#                          cpi = cpi,
#                          ppp = ppp,
#                          pop = pop)

pipfun::log_filter(name = "pipdata_log")
pipfun::log_save(name = "pipdata_log", path = "log.qs")
log <- qs::qread("log.qs")
