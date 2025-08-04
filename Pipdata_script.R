# Test of pipdata functions

#----- Load libraries and set release---

library(devtools)
load_all()

release <- "20250203"
pipfun::setup_working_release(release)

#----- Temporary load data -----

# Load inventory from validated DLW:
inv <- pipdata_load_report(report_type = "inventory")

inv_to_clean  <- valid_dlw_load(inv,
                                aux_measures = c("pfw"))
# Load data
# svys <- inv_dlw_load(inv_to_clean)

# Load PFW
pfw  <- pipload::pip_load_aux("pfw")

# Check for unique obs per pfw --------
# keyVar <- c("country_code", "survey_year", "survey_acronym", "welfare_type")
# pfw <- unq_obs_dt(pfw, keyVar)

#--------- Run pipdata functions -----

# Process data
inv_ls <- split(inv_to_clean,
                seq_len(nrow(inv_to_clean)))

results <- purrr::map(inv_ls,
                  process_data,
                  pfw = pfw)

names(results) <- inv_to_clean$survey_id

# Name survey_id




# Check if list has attributes with specific names
# dt <- find_dt_with_attribute(ls, attr_name = "country_code", attr_value = "PHL")[[3]]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load aux data --------

ppp  <- pipload::pip_load_aux("ppp")
cpi  <- pipload::pip_load_aux("cpi")
pop  <- pipload::pip_load_aux("pop")
gdp  <- pipload::pip_load_aux("gdp")

valid_dlw_load(inv,
               aux_measures = c("pfw"))

# # Deflation
delfated <- lapply(results, pd_deflation,
                         cpi = cpi,
                         ppp = ppp,
                         pop = pop)

pipfun::log_filter(name = "pipdata_log")
pipfun::log_save(name = "pipdata_log", path = "log.qs")
log <- qs::qread("log.qs")
