## code to prepare `DATASET` dataset goes here



pip_var_type <- pipdata_int(file = "pip_pc_var_type.csv")
dlw_var_type <- pipdata_int(file = "dlw_pip_var_type.csv")

pip_var_type <- readr::read_csv(pip_var_type)
dlw_var_type <- readr::read_csv(dlw_var_type)



usethis::use_data(pip_var_type, overwrite = TRUE, internal = TRUE)
usethis::use_data(dlw_var_type, overwrite = TRUE, internal = TRUE)



