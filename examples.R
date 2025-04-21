# Test of
library(devtools)
load_all()

#----- Temporary load of svy data -----

# Set parameters
path <- "//tsclient/Y/DLW-QS" # Path to DLW-QS
n <- 20 # Number of random surveys loaded

# Load inventory
inv  <- pipload::pip_load_dlw_inventory()

# Sample files
set.seed(51089)

selected   <- sample(1:nrow(inv), n)
inv_smp <- inv[selected,]

# Load survey data
ls <- valid_dlw_load(inv_smp, path)

# Load Aux data
pfw  <- pipload::pip_load_aux("pfw")

# Process data

#--------- Run pipdata functions -----


results <- lapply(ls,
                  process_data,
                  pfw = pfw)

# Clean NA
clean_res <- Filter(Negate(is.na), results)

# Deflation

ppp  <- pipload::pip_load_aux("ppp")
cpi  <- pipload::pip_load_aux("cpi")
pop  <- pipload::pip_load_aux("pop")

delfated <- lapply(clean_res, pd_deflation,
                         cpi = cpi,
                         ppp = ppp,
                         pop = pop)

# ----------------------Mini example-----------------------------

md   <- pipload::pip_load_dlw(country = "PHL", 2012)
cpfw <- get_country_pfw(md, pfw)
ls   <- pd_cpfw_merge(md, cpfw)
ls_c    <- pd_dlw_clean(ls)
rm(ls)
ls_f    <- pd_wbpip_clean(ls_c)
rm(ls_c)

gd   <- pipload::pip_load_dlw("CHN", 2015)
gd <- ls[[10]]
cpfw <- get_country_pfw(gd, pfw)
ls   <- pd_cpfw_merge(gd, cpfw)
ls_c    <- pd_dlw_clean(ls)
ls_f    <- pd_wbpip_clean(ls_c)

ppp  <- pipload::pip_load_aux("ppp")
cpi  <- pipload::pip_load_aux("cpi")
pop  <- pipload::pip_load_aux("pop")

delfated <- pd_deflation(lf = ls_f,
                         cpi = cpi,
                         ppp = ppp,
                         pop = pop)

#-----------------------Extra -----------------------



# PPP manipulation
ppp <- ppp_to_wide(ppp = ppp)

# CPI manipulation
if ("cpi2005_SM21" %in% names(cpi)) {
  setnames(cpi, "cpi2005_SM21", "cpi2005") # temporal solution
}

