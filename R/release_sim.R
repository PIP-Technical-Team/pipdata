create_release_label <- function(
    date      = Sys.Date(),
    ppp_round = "2017",
    rv        = "01",           # release version
    av        = "01",           # adaptation version
    suffix    = "PROD",
    full      = TRUE            # must be one of PROD, TEST, or INT
) {
  # 1. Format date as YYYYMMDD
  date_str <- format(as.Date(date), "%Y%m%d")

  # 2. Validate suffix
  if (!suffix %in% c("PROD", "TEST", "INT")) {
    stop("Suffix must be one of 'PROD', 'TEST', or 'INT'. Got: ", suffix)
  }

  # 3. Build the label
  release_label <- paste(date_str, suffix, sep = "_")
  release_label_full <- paste(date_str, ppp_round, rv, av, suffix, sep = "_")

  if (isTRUE(full)){
    return(release_label_full)
  } else {
    return(release_label)
  }

}

set_current_release <- function(release_label) {

  options(pip_current_release = release_label)
  message("Current release set to: ", release_label)
}

get_current_release <- function() {
  rel <- getOption("pip_current_release", default = NA_character_)
  return(rel)
}

