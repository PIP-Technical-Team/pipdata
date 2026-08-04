
pipdata_default_options <- list(
  pipdata.verbose  = TRUE
)

.onLoad <- function(libname, pkgname) {
  # Reset unified package environment to a clean state on load.
  # .pipdataenv is defined in aaa.R and is the single source of mutable
  # package state. No re-creation needed — just wipe any stale keys.
  pd_env_reset()

  # initiate logging
  pipfun::log_init("pipdata_log", overwrite = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op <- options()
  toset <- !(names(pipdata_default_options) %in% names(op))
  if (any(toset)) {
    options(pipdata_default_options[toset])
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------

  invisible()
}
