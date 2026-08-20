# Shared synthetic DLW data generators for validation engine tests and
# golden-fixture regeneration. Both `test-dlw_validation_engine.R` and
# `generate-fixtures.R` consume these so the RNG protocol stays in one place.

make_gpwg_data <- function() {
  data.table::data.table(
    countrycode = "USA",
    year = 2020L,
    hhid = 1:100,
    pid = 1:100,
    welfare = runif(100, 100, 10000),
    welfshprosperity = runif(100, 100, 10000),
    weight = runif(100, 1, 10),
    hsize = sample(1:8, 100, replace = TRUE),
    urban = sample(0:1, 100, replace = TRUE)
  )
}

make_group_data <- function() {
  data.table::data.table(
    weight = runif(50, 1, 10),
    welfare = runif(50, 100, 10000),
    urban = sample(0:1, 50, replace = TRUE),
    code = sample(letters, 50, replace = TRUE),
    type = sample(c("income", "consumption"), 50, replace = TRUE),
    welfare_type = sample(c("C", "I"), 50, replace = TRUE)
  )
}

make_bin_data <- function() {
  data.table::data.table(
    code = sample(letters, 60, replace = TRUE),
    year = 2020L,
    share = runif(60, 0, 1),
    weight = runif(60, 1, 10),
    welfare = runif(60, 100, 10000),
    verm = sample(c("v1", "v2"), 60, replace = TRUE),
    vera = sample(c("a1", "a2"), 60, replace = TRUE),
    region = sample(c("East", "West"), 60, replace = TRUE),
    countryname = "TestCountry"
  )
}

make_hist_data <- function() {
  data.table::data.table(
    urban = sample(0:1, 40, replace = TRUE),
    year = 2020L,
    hsize = sample(1:8, 40, replace = TRUE),
    datayear = 2019L,
    type = 1L,
    weight = runif(40, 1, 10),
    welfare = runif(40, 100, 10000),
    code = sample(letters, 40, replace = TRUE),
    survname = "TestSurvey"
  )
}

make_all_data <- function() {
  data.table::data.table(
    weight = runif(80, 1, 10),
    welfare = runif(80, 100, 10000),
    urban = sample(0:1, 80, replace = TRUE),
    age = sample(0:110, 80, replace = TRUE),
    male = sample(0:2, 80, replace = TRUE),
    educat7 = sample(1:7, 80, replace = TRUE),
    school = sample(0:1, 80, replace = TRUE)
  )
}

make_aspire_data <- function() {
  data.table::data.table(
    hhweight = runif(50, 1, 10),
    year = 2020L,
    hsize = sample(1:8, 50, replace = TRUE),
    urban = sample(0:1, 50, replace = TRUE)
  )
}

make_l_data <- function() {
  data.table::data.table(
    lstatus = sample(1:3, 70, replace = TRUE),
    empstat = sample(1:5, 70, replace = TRUE),
    countrycode = "USA",
    hhid = 1:70,
    pid = 1:70,
    year = 2020L,
    whours = sample(0:60, 70, replace = TRUE)
  )
}

make_skip_data <- function() {
  data.table::data.table(x = 1:10)
}

make_empty_data <- function() {
  data.table::data.table(x = integer(0))
}

#' Deterministic per-module data builder
#'
#' Seeds the RNG with a module-derived seed so that the golden-fixture
#' regeneration script and the engine tests produce identical data regardless
#' of iteration order or prior RNG state.
#'
#' @param module Character module id.
#' @param seed Integer base seed.
dlw_fixture_data <- function(module, seed = 42) {
  if (module == "skip") {
    return(make_skip_data())
  }
  module_order <- c("gpwg", "group", "bin", "hist", "all", "aspire", "l")
  pos <- match(module, module_order)
  set.seed(seed + pos)
  switch(module,
    gpwg = make_gpwg_data(),
    group = make_group_data(),
    bin = make_bin_data(),
    hist = make_hist_data(),
    all = make_all_data(),
    aspire = make_aspire_data(),
    l = make_l_data(),
    skip = make_skip_data()
  )
}

#' Column removed to trigger a `type == "error"` fixture per module.
dlw_error_column <- function(module) {
  switch(module,
    gpwg = "weight",
    group = "weight",
    bin = "bins",
    hist = "weight",
    all = "age",
    aspire = "hhweight",
    l = "lstatus"
  )
}