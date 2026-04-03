
.pipdata <-  new.env(parent = emptyenv())

.logenv <-  new.env(parent = emptyenv())

.pipdataenv <-  new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  tryCatch({
    .SCHEMA_GEN        <<- piptm::pip_arrow_schema()
    .REQUIRED_COLS_GEN <<- piptm::pip_required_cols()
    .ALLOWED_COLS_GEN  <<- piptm::pip_allowed_cols()
    .GENDER_LEVELS_GEN <<- .SCHEMA_GEN$levels$gender
    .AREA_LEVELS_GEN   <<- .SCHEMA_GEN$levels$area
  }, error = function(e) {
    # piptm may not be available in a load_all() dev session.
    # Globals remain NULL; .validate_for_write() will call piptm:: lazily.
    packageStartupMessage(
      "[pipdata] Could not initialise Arrow schema globals from {piptm}: ",
      conditionMessage(e),
      "\n  Arrow validation will call piptm:: lazily at runtime."
    )
  })
}

