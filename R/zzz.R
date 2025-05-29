

pipdata_default_options <- list(
  pipdata.verbose  = TRUE
)

.onLoad <- function(libname, pkgname) {

  pipfun::log_init("pipdata_log") # Maybe do it per release?

  # make sure .logenv is exported properly-----

  if (!exists(".logenv", envir = asNamespace(pkgname))) {
    assign(".logenv", new.env(parent = emptyenv()), envir = asNamespace(pkgname))
  }

  # make sure .pipdataenv is exported properly-----

  if (!exists(".pipdataenv", envir = asNamespace(pkgname))) {
    assign(".pipdataenv", new.env(parent = emptyenv()), envir = asNamespace(pkgname))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipdata_default_options) %in% names(op))
  if (any(toset)) options(pipdata_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------

  invisible()
}
