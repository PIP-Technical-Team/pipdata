# Regenerate the committed golden fixtures for the DLW validation engine.
#
# The data generators and RNG protocol are shared with the engine tests via
# tests/testthat/helper-dlw-data.R (dlw_fixture_data uses a per-module seed),
# so re-running this script reproduces the committed tests/testthat/fixtures/*.
#
# Usage: run from the package root with devtools::load_all(), e.g.
#   Rscript -e "devtools::load_all(quiet=TRUE); source('tests/testthat/generate-fixtures.R')"

source("tests/testthat/helper-dlw-data.R")

dir.create("tests/testthat/fixtures", showWarnings = FALSE, recursive = TRUE)

modules <- c("gpwg", "group", "bin", "hist", "all", "aspire", "l", "skip")

.for_mod <- function(dlw, nm, module) {
  pd_env_rm("validation_report")
  dlw_validation_engine(dlw, nm, module)
  pd_env_get("validation_report")
}

for (mod in modules) {
  dlw <- if (mod == "skip") make_skip_data() else dlw_fixture_data(mod)
  rec <- .for_mod(dlw, paste0("fixture_", mod), mod)
  saveRDS(rec, file.path("tests/testthat/fixtures", paste0("validation_", mod, ".rds")))
  cat(mod, "rows:", nrow(rec), "\n")
}

for (mod in modules[modules != "skip"]) {
  dlw <- dlw_fixture_data(mod)
  dlw[[dlw_error_column(mod)]] <- NULL
  rec <- .for_mod(dlw, paste0("fixture_error_", mod), mod)
  stopifnot(any(rec$type == "error"))
  saveRDS(rec, file.path("tests/testthat/fixtures", paste0("validation_error_", mod, ".rds")))
  cat("error", mod, "rows:", nrow(rec), "\n")
}

# blank skip fixture -> type == "error" (uses assertr error_stop via critical severity)
skip_blank <- local({
  pd_env_rm("validation_report")
  dlw_validation_engine(make_empty_data(), "fixture_skip_blank", "skip")
  pd_env_get("validation_report")
})
stopifnot(any(skip_blank$type == "error"))
saveRDS(skip_blank, "tests/testthat/fixtures/validation_skip_blank.rds")
cat("skip_blank rows:", nrow(skip_blank), "\n")

cat("FIXTURES GENERATED\n")