# Example
library(devtools)
load_all()

# Load svy data
set.seed(51089)
n <- 10
path <- "//tsclient/Y/DLW-QS"
inv  <- pipload::pip_load_dlw_inventory()
selected   <- sample(1:nrow(inv), n)
inv_smp <- inv[selected,]

file_dta <- basename(inv_smp$fullname)
file_qs <- sub("\\.dta$", ".qs", file_dta)
file_qs <- sort(file_qs)

ls_svy     <- lapply(1:n, \(x) qs::qread(file.path(path, file_qs[x])))

## Order inv_smp

inv_smp <- inv_smp |>
  dplyr::mutate(file_dta = basename(fullname))

inv_smp <- inv_smp[order(file_dta),]

# Load Aux data

pfw  <- pipload::pip_load_aux("pfw")

# Process data

#--------- Mock-up pipload functions ---------

data_to_dt <- function(x, y) {

  # df <- haven::read_dta(x)
  df <- x
  df$survey_id <- y

  #--------- leaving just the 'label' attribute ---------
  nn  <- names(df)
  for (j in seq_along(nn)) {

    ats       <- attributes(df[[j]])
    atsn      <- names(ats)
    to_remove <- atsn[!grepl("label", atsn)]

    for (i in seq_along(to_remove)) {
      attr(df[[j]], to_remove[i]) <- NULL
    }

  }

  #--------- Survey ID and its components ---------

  df <- pipload::survey_id_to_vars(df)

  ### Add class ---------
  df <- pipload::as_pip(df)

  return(df)
}

poss_data_to_df <- purrr::possibly(.f = data_to_dt,
                                   otherwise = NULL)

#--------- Run pipdata functions -----

ls <- purrr::map2(.x = ls_svy,
                  .y = as.list(inv_smp$survey_id),
                  .f = poss_data_to_df)

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

