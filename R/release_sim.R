# Release simulation functions -----
## Note: they will be superseeded by pipfun functions.
## We keep them because it helps having them stable and local for the simulation.
create_release_label <- function(
    date      = Sys.Date(),
    ppp_round = "2017",
    rv        = "01",           # release version
    av        = "01",           # adaptation version
    identity    = "PROD",
    full      = TRUE            # must be one of PROD, TEST, or INT
) {
  # 1. Format date as YYYYMMDD
  date_str <- format(as.Date(date), "%Y%m%d")

  # 2. Validate suffix
  if (!identity %in% c("PROD", "TEST", "INT")) {
    stop("Suffix must be one of 'PROD', 'TEST', or 'INT'. Got: ", suffix)
  }

  # 3. Build the label
  release_label <- paste(date_str, identity, sep = "_")
  release_label_full <- paste(date_str, ppp_round, rv, av, identity, sep = "_")

  if (isTRUE(full)){
    return(release_label_full)
  } else {
    return(release_label)
  }

}

setup_working_release <- function(release_label) {

  options(dlw_working_release = release_label)
  message("DLW working release set to: ", release_label)
}

get_wrk_release <- function() {
  rel <- getOption("dlw_working_release", default = NA_character_)
  return(rel)
}
