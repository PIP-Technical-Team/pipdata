m_inv_load <- function(folder = "DLW-OUTPUT",
                       name_inv = "pip_raw_inventory_20250203_TEST.qs") {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # inv_files <- fs::dir_ls(fs::path(Sys.getenv("PIP_ROOT_DIR"),folder,"/_Inventory/_release"))

  inv   <- qs::qread(fs::path(Sys.getenv("PIP_ROOT_DIR"),
                              folder,
                              "/_Inventory/_release",
                              name_inv))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv)

}

m_inv_filter <- function(inv,
                         options = c("new", "changed"),
                         seed = 1089,
                         n = 20) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  set.seed(seed)

  selected   <- sample(1:nrow(inv), n) # Needs to be changed to filter by status
  inv_smp    <- inv[selected,]

  # Randomly assign names to a new variable
  inv_smp[, status := sample(options, .N, replace = TRUE)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(inv_smp)

}
