

pipdata_default_options <- list(
  pipdata.verbose  = TRUE
)

.onLoad <- function(libname, pkgname) {

  # make sure .logenv is exported properly-----

  if (!exists(".logenv", envir = asNamespace(pkgname))) {
    assign(".logenv", new.env(parent = emptyenv()), envir = asNamespace(pkgname))
  }

  # initiate logging
  pipfun::log_init("pipdata_log", overwrite = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipdata_default_options) %in% names(op))
  if (any(toset)) options(pipdata_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------

  invisible()
}
