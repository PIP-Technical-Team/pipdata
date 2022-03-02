

pipdata_default_options <- list(
  pipdata.verbose  = TRUE
)

.onLoad <- function(libname, pkgname) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipdata_default_options) %in% names(op))
  if (any(toset)) options(pipdata_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------

  invisible()
}
