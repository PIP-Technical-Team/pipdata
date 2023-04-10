## code to prepare `DATASET` dataset goes here
library(dplyr)


pip_var_type <- pipdata_int(file = "pip_pc_var_type.csv")
dlw_var_type <- pipdata_int(file = "dlw_pip_var_type.csv")

pip_var_type <-
  readr::read_csv(pip_var_type) |>
  mutate(pip_vars_pc_class = if_else(pip_vars_pc_class == "string", "character", pip_vars_pc_class))


dlw_var_type <-
  readr::read_csv(dlw_var_type)  |>
  mutate(pip_vars_class = if_else(pip_vars_class == "string", "character", pip_vars_class))


usethis::use_data(dlw_var_type, pip_var_type,  overwrite = TRUE, internal = TRUE)



