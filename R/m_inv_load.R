m_inv_load <- function(inv,
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
