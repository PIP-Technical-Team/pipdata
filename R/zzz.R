
pipdata_default_options <- list(
  pipdata.verbose  = TRUE,
  pipdata.arrow_repo = "Y:/PIP_ingestion_pipeline_v2/pip_repository/tm_data/arrow",
  pipdata.manifest_root = "Y:/PIP_ingestion_pipeline_v2/pip_repository/tm_data/manifests"
)

.onLoad <- function(libname, pkgname) {

  # make sure .logenv is exported properly-----

  if (!exists(".logenv", envir = asNamespace(pkgname))) {
    assign(".logenv", new.env(parent = emptyenv()), envir = asNamespace(pkgname))
  }

  # initiate logging
  pipfun::log_init("pipdata_log", overwrite = TRUE)

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
