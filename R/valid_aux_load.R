valid_aux_load <- function(measure = c("cpi", "ppp","gdp","pfw","pop"),
                           maindir = fs::path(Sys.getenv("PIP_ROOT_DIR"),"PIP_ingestion_pipeline_V2")) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  measure = c("cpi", "ppp","gdp","pfw","pop")
  maindir = fs::path(Sys.getenv("PIP_ROOT_DIR"),"PIP_ingestion_pipeline_V2")

  changes <- pipaux::inventory_aux_changes(measures = measure,
                                           maindir = maindir)
#
#   path <- "//tsclient/Y/PIP_ingestion_pipeline_v2" # Path to DLW-QS
  inv_aux   <- qs::qread(file.path(maindir, "aux_data/20250203_TEST/aux_inv_list.qs"))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_aux)

}
