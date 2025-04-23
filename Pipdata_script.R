# Test of
library(devtools)
load_all()

#----- Temporary load of svy data -----

# Set parameters
path <- "//tsclient/Y/DLW-OUTPUT" # Path to DLW-QS
n    <- 20 # Number of random surveys loaded

# Load inventory
# inv  <- pipload::pip_load_dlw_inventory()
inv          <- qs::qread(file.path(path, "_Inventory/_release/pip_raw_inventory_20250417_INT.qs"))
inv$fullname <- file.path(path, basename(inv$pip_file_path))

# Sample files
set.seed(1089)
selected   <- sample(1:nrow(inv), n)
inv_smp    <- inv[selected,]

# Load data
ls <- valid_dlw_load(inv_smp, path)
pfw  <- pipload::pip_load_aux("pfw")

# pfw_all  <- qs::qread("cpi_pop_gdp_ppp_pfw.qs")
# pfw_all <- pfw_all[order(country_code,year),]
# pfw_ur <- pfw_all[pfw_all$reporting_level!="national",]

#--------- Run pipdata functions -----

# Process data
results <- lapply(ls,
                  process_data,
                  pfw = pfw)

# Clean NA results
clean_res <- Filter(Negate(is.na), results)

# Deflation
ppp  <- pipload::pip_load_aux("ppp")
cpi  <- pipload::pip_load_aux("cpi")
pop  <- pipload::pip_load_aux("pop")

delfated <- lapply(clean_res, pd_deflation,
                         cpi = cpi,
                         ppp = ppp,
                         pop = pop)

# Print errors
.logenv$piperr
.logenv$unk_err
