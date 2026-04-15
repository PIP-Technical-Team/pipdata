### Test of pipdata functions

# Check packages updates
metapip::update_pip_packages()

# ----- Load libraries and set release-----

library(devtools)
load_all()

# Options
options(pipload.verbose = FALSE)
# stamp::st_opts("warn_missing_pk_on_load", .get = TRUE)
stamp::st_opts(warn_missing_pk_on_load = FALSE)

release <- "20260401"
identity <- "TEST"

pipfun::setup_working_release(
  release,
  identity,
  verbose = FALSE
)

# ----- Load inventory to clean -----
inv <- pipload::load_gmd_valid_inv()

#--------- Clean surveys and create metadata -----
old_pip_inv <- pipload::load_pip_master_inventory(verbose = FALSE)
# new_pip_inv <- pipload::load_pip_master_inventory()
new_pip_inv <- pd_process_data(inv = inv, force = TRUE, verbose = FALSE)

# Compare inventories
waldo::compare(old_pip_inv, new_pip_inv)

# Check report
log <- pipfun::log_filter(name = "pipdata_log")
report <- log_report(log, path = "log_reports\\log_report.md", overwrite = TRUE)

# Save log
stamp::st_init(
  root = fs::path(getOption("pipfun.main_dir"), "pip_repository", "pip_logs"),
  alias = "piplog"
)

stamp::st_save(log, "cleaning_log", alias = "piplog", verbose = FALSE)

#------ Load data tests -----
# # Load cleaned data for a survey
# BOL <- pipload::load_pip_data(
#   country_code = "BOL",
#   surveyid_year = 2022,
#   module = "ALL",
#   verbose = FALSE
# )

# BOL2 <- pipload::load_pip_data(id_name = "BOL_2022_EH_INC_ALL")

# NGA <- dlw::dlw_get_gmd(country_code = "NGA", year = 2022, module = "ALL")


# # load validation inventory
# validation_inv_list <- pipload::load_gmd_valid_inv()

# # view invalid datasets in the validation inventory
# validation_inv_list[status == "invalid", ] |> View()

# # load validation report
# validation_report <- pipload::load_gmd_valid_report()

# # view error with description that failed validation
# validation_report[type == "error", .(table_name, description)] |> View()

# # Load error
# tst <- pipload::load_dlw_data(
#   id_name = validation_report[type == "error", table_name][1]
# )

# tst <- pip_inv |>
#     unique() |>
#     joyn::inner_join(
#       vrs,
#       by = c("survey_id", "pip_id")
#     ) 

# "BOL_1990_EPF_v01_M_v01_A_GMD_GROUP"


