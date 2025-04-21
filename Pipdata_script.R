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
