### Test of pipdata functions

# Check packages updates
metapip::update_pip_packages()

# ----- Load libraries and set release-----

library(devtools)
load_all()

# Options
# Suppress downstream I/O messages from pipload/stamp for batch runs.
# Set pipdata.verbose = TRUE (or remove this line) for interactive exploration.
# options(pipdata.verbose = FALSE)
# options(pipload.verbose = FALSE)
# stamp::st_opts("warn_missing_pk_on_load", .get = TRUE)
# stamp::st_opts(warn_missing_pk_on_load = FALSE)

release <- "20260401"
identity <- "TEST"

pipfun::setup_working_release(
  release,
  identity,
  verbose = FALSE
)

# First run pipaux::update_aux_data and pipdata::pipdata_dlw_process,
# then continue with the rest of the script.

# ----- Load inventory to clean -----
inv <- pipload::load_gmd_valid_inv(verbose = FALSE)
inv_ARG <- inv[country_code == "ARG", ]

#--------- Clean surveys and create metadata -----
old_pip_inv <- pipload::load_pip_master_inventory(verbose = FALSE)

new_pip_inv <- pd_process_data(inv = inv, verbose = FALSE)
new_pip_inv <- pd_process_data(inv = inv_ARG, verbose = FALSE)

# Compare inventories
waldo::compare(old_pip_inv, new_pip_inv)

# Check report
log <- pipfun::log_filter(name = "pipdata_log")
report <- log_report(
  log,
  path = file.path("log_reports", "log_report.md"),
  overwrite = TRUE
)

# Save log
stamp::st_init(
  root = fs::path(getOption("pipfun.main_dir"), "pip_repository", "pip_logs"),
  alias = "piplog"
)

stamp::st_save(log, "cleaning_log", alias = "piplog", verbose = FALSE)

# Do not run from now on, as the rest of the script is for testing purposes only.
# The next steps are to load the cleaned data and check that it is correct.
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

# ----- Test pd_deflation -----

dt_meta <- pipload::pip_read(
  id = "CHN_2011_CRHS-CUHS_CON_GROUP",
  alias = "pip_meta"
)


dt <- pipload::pip_read(id = "CHN_2011_CRHS-CUHS_CON_GROUP", alias = "pip")

# Mode B: load survey and metadata from stamp by pip_id
bol_deflated <- pd_deflation(pip_id = "BOL_2022_EH_INC_ALL")

# Inspect result
class(bol_deflated)
names(bol_deflated)

# Check welfare_lcu and welfare_ppp columns were created
grep("^welfare", names(bol_deflated), value = TRUE)

# Quick sanity check: no all-NA welfare_ppp column
welfare_ppp_cols <- grep("^welfare_ppp", names(bol_deflated), value = TRUE)
sapply(bol_deflated[, welfare_ppp_cols, with = FALSE], \(x) mean(is.na(x)))

# ----- Test st_catalog_query --------------------------------------------------

# Query pip alias (survey data artifacts)
cat_pip <- stamp::st_catalog_query(alias = "pip")
cat_pip

# Query pip_meta alias (metadata artifacts)
cat_meta <- stamp::st_catalog_query(alias = "pip_meta")
cat_meta

# Quick checks
nrow(cat_pip) # one row per cleaned survey
nrow(cat_meta) # should match or be close

# Artifacts in pip but not in pip_meta (missing metadata)
cat_pip[!cat_pip$version_id %chin% cat_meta$version_id]

# Derive pip_id from path (same logic as build_pip_inventory will use)
cat_pip[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]
cat_meta[, pip_id := toupper(fs::path_ext_remove(fs::path_file(path)))]

# Surveys in data but missing from metadata
cat_pip[!pip_id %chin% cat_meta$pip_id, .(pip_id, path, created_at)]

# Inspect the schema
str(cat_pip)

# Check content_hash and code_hash coverage
cat_pip[, .(
  n = .N,
  n_content_hash = sum(!is.na(content_hash)),
  n_code_hash = sum(!is.na(code_hash))
)]

# Issue with ARG 2003

arg_deflated <- pd_deflation(pip_id = "ARG_2003_EPHC-S2_INC_ALL")
