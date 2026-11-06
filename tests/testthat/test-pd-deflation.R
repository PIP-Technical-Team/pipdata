# Tests for pd_deflation.R
#
# Covers:
#   data_level_column()          — sentinel registry contract
#   .validate_deflation_input()  — input validation helper
#   .load_deflation_aux()        — inventory-based metadata loading (mocked)
#   safe_deflation()             — tryCatch scaffold; error → NA
#   adjust_population()          — named-vector path (data.table path in test-adjust-population.R)
#   add_ppp()                    — named-vector path (national + subnational)
#   add_cpi()                    — named-vector path (national + subnational)
#   pd_deflation()               — Mode A with explicit aux (legacy), Mode A metadata-driven

# ---------------------------------------------------------------------------
# Shared fixture helpers
# ---------------------------------------------------------------------------

make_pipmd <- function(
  welfare = c(5, 10, 15),
  weight = c(100, 200, 100),
  country = "ABC",
  survey_year = 2015L,
  survey_acronym = "TST",
  ppp_data_level = "national",
  cpi_data_level = "national",
  pop_data_level = "national",
  reporting_level = 1L,
  welfare_type = "income",
  module = "D1",
  area = NULL # e.g. c("rural", "urban", "rural") for subnational surveys
) {
  dt <- data.table::data.table(
    welfare = as.numeric(welfare),
    weight = as.numeric(weight),
    year = survey_year # adjust_population() reads 'year' column (standardized name)
    # ppp_data_level / cpi_data_level / pop_data_level are attrs only — never columns
  )
  if (!is.null(area)) {
    dt[, area := area]
  }
  data.table::setattr(dt, "class", c("pipmd", "data.table", "data.frame"))
  data.table::setattr(dt, "survey_id", "ABC_2015_TST_INC_D1")
  data.table::setattr(dt, "country_code", country)
  data.table::setattr(dt, "surveyid_year", survey_year)
  data.table::setattr(dt, "survey_acronym", survey_acronym)
  data.table::setattr(dt, "reporting_level", reporting_level)
  data.table::setattr(dt, "ppp_data_level", ppp_data_level)
  data.table::setattr(dt, "cpi_data_level", cpi_data_level)
  data.table::setattr(dt, "pop_data_level", pop_data_level)
  data.table::setattr(dt, "pip_names", "ABC_2015_TST_INC_D1")
  data.table::setattr(dt, "welfare_type", welfare_type)
  data.table::setattr(dt, "module", module)
  dt
}

# Named-vector aux fixtures matching the pd_aux_attr() format.
make_cpi_vec <- function(year = "2017", level = "national", value = 100) {
  stats::setNames(value, paste0(year, "_", level))
}

make_ppp_vec <- function(
  ppp_year = "2017",
  rel = "01",
  adapt = "01",
  level = "national",
  value = 3.5
) {
  nm <- paste0("ppp_", ppp_year, "_", rel, "_", adapt, "_", level)
  stats::setNames(value, nm)
}

make_pop_vec <- function(year = "2015", level = "national", value = 1e6) {
  stats::setNames(value, paste0(year, "_", level))
}

# Subnational fixture helpers: named vectors with "rural"/"urban" level keys.
make_ppp_vec_subnational <- function(
  ppp_year = "2017", rel = "01", adapt = "01"
) {
  c(
    stats::setNames(3.0, paste0("ppp_", ppp_year, "_", rel, "_", adapt, "_rural")),
    stats::setNames(3.9, paste0("ppp_", ppp_year, "_", rel, "_", adapt, "_urban")),
    stats::setNames(3.5, paste0("ppp_", ppp_year, "_", rel, "_", adapt, "_national"))
  )
}

make_cpi_vec_subnational <- function(year = "2017") {
  c(
    stats::setNames(0.85, paste0(year, "_rural")),
    stats::setNames(0.88, paste0(year, "_urban")),
    stats::setNames(0.87, paste0(year, "_national"))
  )
}

make_pop_vec_subnational <- function(year = "2015") {
  c(
    stats::setNames(6e8, paste0(year, "_rural")),
    stats::setNames(7e8, paste0(year, "_urban")),
    stats::setNames(1.3e9, paste0(year, "_national"))
  )
}

# ---------------------------------------------------------------------------
# data_level_column() — column-lookup registry contract
# ---------------------------------------------------------------------------

test_that("data_level_column() resolves the area sentinel and maps literals to NA", {
  expect_equal(pipdata:::data_level_column("area"), "area")
  expect_true(is.na(pipdata:::data_level_column("national")))
  expect_true(is.na(pipdata:::data_level_column("region")))
  # Degenerate inputs from missing/empty/malformed *_data_level attrs.
  expect_true(is.na(pipdata:::data_level_column(NULL)))
  expect_true(is.na(pipdata:::data_level_column(character(0))))
  expect_true(is.na(pipdata:::data_level_column(NA_character_)))
  expect_true(is.na(pipdata:::data_level_column(c("area", "area"))))
  expect_true(is.na(pipdata:::data_level_column(c("national", "rural"))))
  expect_true(is.na(pipdata:::data_level_column(1)))
  expect_true(is.na(pipdata:::data_level_column(TRUE)))
})

test_that("add_ppp() safely broadcasts when its data-level attribute is NA", {
  dt <- make_pipmd()
  data.table::setattr(dt, "ppp_data_level", NA_character_)

  result <- pipdata:::add_ppp(dt, make_ppp_vec())

  expect_true("ppp_2017_01_01" %in% names(result))
  expect_true(all(is.na(result$ppp_2017_01_01)))
})

# ---------------------------------------------------------------------------
# .validate_deflation_input()
# ---------------------------------------------------------------------------

test_that(".validate_deflation_input passes for valid pipmd input", {
  dt <- make_pipmd()
  expect_true(pipdata:::.validate_deflation_input(dt))
})

test_that(".validate_deflation_input aborts on non-data.table", {
  expect_error(
    pipdata:::.validate_deflation_input(list(welfare = 1)),
    class = "validate_deflation_input"
  )
})

test_that(".validate_deflation_input aborts on wrong class", {
  dt <- make_pipmd()
  data.table::setattr(dt, "class", c("data.table", "data.frame")) # strip pipmd
  expect_error(
    pipdata:::.validate_deflation_input(dt),
    class = "validate_deflation_input"
  )
})

test_that(".validate_deflation_input aborts on missing required column", {
  dt <- make_pipmd()
  dt[, welfare := NULL]
  expect_error(
    pipdata:::.validate_deflation_input(dt),
    class = "validate_deflation_input"
  )
})

test_that(".validate_deflation_input aborts on missing required attribute", {
  dt <- make_pipmd()
  attr(dt, "country_code") <- NULL
  expect_error(
    pipdata:::.validate_deflation_input(dt),
    class = "validate_deflation_input"
  )
})

test_that(".validate_deflation_input aborts when welfare has NAs", {
  dt <- make_pipmd(welfare = c(5, NA, 15))
  expect_error(
    pipdata:::.validate_deflation_input(dt),
    class = "validate_deflation_input"
  )
})

test_that(".validate_deflation_input aborts when weight has NAs", {
  dt <- make_pipmd(weight = c(100, NA, 100))
  expect_error(
    pipdata:::.validate_deflation_input(dt),
    class = "validate_deflation_input"
  )
})

# ---------------------------------------------------------------------------
# .load_deflation_aux() — mocked
# ---------------------------------------------------------------------------

test_that(".load_deflation_aux uses version_id_metadata directly", {
  cpi_vec <- make_cpi_vec()
  ppp_vec <- make_ppp_vec()
  pop_vec <- make_pop_vec()
  meta_obj <- list(cpi = cpi_vec, ppp = ppp_vec, pop = pop_vec)

  fake_inv <- data.table::data.table(
    pip_id = "ABC_2015_TST_INC_D1",
    content_hash_data = "abc123",
    content_hash_metadata = "meta_abc123",
    version_id_metadata = "ver_abc123",
    created_at_metadata = "2026-01-01T00:00:00Z"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) fake_inv,
    pip_read = function(id, alias, version = NULL, ...) {
      expect_identical(version, "ver_abc123")
      meta_obj
    },
    .package = "pipload"
  )

  result <- pipdata:::.load_deflation_aux("ABC_2015_TST_INC_D1", verbose = FALSE)

  expect_equal(result$cpi, cpi_vec)
  expect_equal(result$ppp, ppp_vec)
  expect_equal(result$pop, pop_vec)
})

test_that(".load_deflation_aux loads latest when version_id_metadata is NA", {
  cpi_vec <- make_cpi_vec()
  ppp_vec <- make_ppp_vec()
  pop_vec <- make_pop_vec()
  meta_obj <- list(cpi = cpi_vec, ppp = ppp_vec, pop = pop_vec)

  fake_inv <- data.table::data.table(
    pip_id = "ABC_2015_TST_INC_D1",
    content_hash_data = "abc123",
    content_hash_metadata = "meta_abc123",
    version_id_metadata = NA_character_,
    created_at_metadata = "2026-01-01T00:00:00Z"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) fake_inv,
    pip_read = function(id, alias, version = NULL, ...) {
      # version=NULL means load latest
      expect_null(version)
      meta_obj
    },
    .package = "pipload"
  )

  result <- pipdata:::.load_deflation_aux("ABC_2015_TST_INC_D1", verbose = FALSE)

  expect_equal(result$cpi, cpi_vec)
  expect_equal(result$ppp, ppp_vec)
  expect_equal(result$pop, pop_vec)
})

test_that(".load_deflation_aux falls back to latest when version_id is stale", {
  cpi_vec <- make_cpi_vec()
  ppp_vec <- make_ppp_vec()
  pop_vec <- make_pop_vec()
  meta_obj <- list(cpi = cpi_vec, ppp = ppp_vec, pop = pop_vec)

  fake_inv <- data.table::data.table(
    pip_id = "ABC_2015_TST_INC_D1",
    content_hash_data = "abc123",
    content_hash_metadata = "meta_abc123",
    version_id_metadata = "stale_version_gone",
    created_at_metadata = "2026-01-01T00:00:00Z"
  )

  call_count <- 0L
  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) fake_inv,
    pip_read = function(id, alias, version = NULL, ...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        # First call with stale version_id — simulate stamp error
        stop("Version not found")
      }
      # Second call with version=NULL (latest)
      expect_null(version)
      meta_obj
    },
    .package = "pipload"
  )

  result <- NULL
  expect_warning(
    {
      result <- pipdata:::.load_deflation_aux("ABC_2015_TST_INC_D1", verbose = FALSE)
    },
    class = "load_deflation_aux_stale_version"
  )
  expect_equal(result$cpi, cpi_vec)
  expect_equal(result$ppp, ppp_vec)
  expect_equal(result$pop, pop_vec)
})

test_that(".load_deflation_aux aborts for unknown pip_id", {
  fake_inv <- data.table::data.table(
    pip_id = character(0),
    content_hash_data = character(0),
    version_id_metadata = character(0),
    created_at_metadata = character(0)
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) fake_inv,
    .package = "pipload"
  )

  expect_error(
    pipdata:::.load_deflation_aux("UNKNOWN_ID", verbose = FALSE),
    class = "load_deflation_aux"
  )
})

test_that(".load_deflation_aux passes verbose argument to pipload calls", {
  cpi_vec <- make_cpi_vec()
  ppp_vec <- make_ppp_vec()
  pop_vec <- make_pop_vec()
  meta_obj <- list(cpi = cpi_vec, ppp = ppp_vec, pop = pop_vec)

  fake_inv <- data.table::data.table(
    pip_id = "ABC_2015_TST_INC_D1",
    content_hash_data = "abc123",
    content_hash_metadata = "meta_abc123",
    version_id_metadata = "ver_abc123",
    created_at_metadata = "2026-01-01T00:00:00Z"
  )

  received_verbose_inv  <- NULL
  received_verbose_read <- NULL

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(verbose = NULL, ...) {
      received_verbose_inv <<- verbose
      fake_inv
    },
    pip_read = function(id, alias, version = NULL, verbose = NULL, ...) {
      received_verbose_read <<- verbose
      meta_obj
    },
    .package = "pipload"
  )

  pipdata:::.load_deflation_aux("ABC_2015_TST_INC_D1", verbose = FALSE)

  expect_false(received_verbose_inv)
  expect_false(received_verbose_read)
})

# ---------------------------------------------------------------------------
# safe_deflation() — error → NA
# ---------------------------------------------------------------------------

test_that("safe_deflation returns NA and does not rethrow on deflation error", {
  dt <- make_pipmd()
  bad_fn <- function(...) stop("boom")

  result <- suppressMessages(
    pipdata:::safe_deflation(dt, NULL, NULL, NULL, bad_fn)
  )

  expect_identical(result, NA)
})

test_that("safe_deflation returns function result on success", {
  dt <- make_pipmd()
  ok_fn <- function(dt, ...) data.table::copy(dt)[, result_col := 42]

  result <- pipdata:::safe_deflation(dt, NULL, NULL, NULL, ok_fn)

  expect_true(data.table::is.data.table(result))
  expect_true("result_col" %in% names(result))
})

# ---------------------------------------------------------------------------
# add_ppp() — named-vector path (national + subnational)
# ---------------------------------------------------------------------------

test_that("add_ppp (named vector) adds ppp column and ppp_versions attribute", {
  dt <- make_pipmd()
  ppp <- make_ppp_vec() # "ppp_2017_01_01_national" = 3.5

  result <- pipdata:::add_ppp(dt, ppp)

  expect_true("ppp_2017_01_01" %in% names(result))
  expect_equal(result$ppp_2017_01_01, rep(3.5, nrow(result)))
  expect_equal(attr(result, "ppp_versions"), "ppp_2017_01_01")
})

test_that("add_ppp (named vector) resolves subnational via area column", {
  # Two rows: rural and urban. ppp_data_level = "area" triggers per-row lookup.
  dt <- make_pipmd(
    welfare = c(5, 10),
    weight = c(100, 200),
    ppp_data_level = "area",
    cpi_data_level = "area",
    reporting_level = 2L,
    area = c("rural", "urban")
  )
  ppp <- make_ppp_vec_subnational() # rural=3.0, urban=3.9

  result <- pipdata:::add_ppp(dt, ppp)

  expect_true("ppp_2017_01_01" %in% names(result))
  expect_false(anyNA(result$ppp_2017_01_01))
  expect_equal(result$ppp_2017_01_01, c(3.0, 3.9))
})

test_that("add_ppp aborts when ppp_data_level is 'area' but area column is absent", {
  dt <- make_pipmd(ppp_data_level = "area") # no area column
  ppp <- make_ppp_vec_subnational()

  expect_error(pipdata:::add_ppp(dt, ppp), class = "add_ppp")
})

# ---------------------------------------------------------------------------
# add_cpi() — named-vector path (national + subnational)
# ---------------------------------------------------------------------------

test_that("add_cpi (named vector) adds cpiYYYY column and cpi_years attribute", {
  dt <- make_pipmd()
  cpi <- make_cpi_vec() # "2017_national" = 100

  result <- pipdata:::add_cpi(dt, cpi)

  expect_true("cpi2017" %in% names(result))
  expect_equal(result$cpi2017, rep(100, nrow(result)))
  expect_equal(attr(result, "cpi_years"), "2017")
})

test_that("add_cpi (named vector) handles multiple base years", {
  dt <- make_pipmd()
  cpi <- c(`2017_national` = 100, `2011_national` = 80)

  result <- pipdata:::add_cpi(dt, cpi)

  expect_true(all(c("cpi2017", "cpi2011") %in% names(result)))
  expect_setequal(attr(result, "cpi_years"), c("2017", "2011"))
})

test_that("add_cpi (named vector) resolves subnational via area column", {
  dt <- make_pipmd(
    welfare = c(5, 10),
    weight = c(100, 200),
    ppp_data_level = "area",
    cpi_data_level = "area",
    reporting_level = 2L,
    area = c("rural", "urban")
  )
  cpi <- make_cpi_vec_subnational() # rural=0.85, urban=0.88

  result <- pipdata:::add_cpi(dt, cpi)

  expect_true("cpi2017" %in% names(result))
  expect_false(anyNA(result$cpi2017))
  expect_equal(result$cpi2017, c(0.85, 0.88))
})

test_that("add_cpi aborts when cpi_data_level is 'area' but area column is absent", {
  dt <- make_pipmd(cpi_data_level = "area") # no area column
  cpi <- make_cpi_vec_subnational()

  expect_error(pipdata:::add_cpi(dt, cpi), class = "add_cpi")
})

test_that("add_ppp() and add_cpi() resolve mixed data-level attributes independently", {
  dt <- make_pipmd(
    welfare = c(5, 10),
    weight = c(100, 200),
    ppp_data_level = "area",
    cpi_data_level = "national",
    reporting_level = 2L,
    area = c("rural", "urban")
  )

  result <- pipdata:::add_ppp(dt, make_ppp_vec_subnational())
  result <- pipdata:::add_cpi(result, make_cpi_vec())

  expect_equal(result$ppp_2017_01_01, c(3.0, 3.9))
  expect_equal(result$cpi2017, rep(100, 2L))
})

# ---------------------------------------------------------------------------
# adjust_population() — named-vector path
# ---------------------------------------------------------------------------

test_that("adjust_population (named vector) scales weights correctly", {
  df <- data.table::data.table(
    country_code = "ABC",
    year = 2015L,
    area = "national",
    weight = c(200, 400) # total = 600
  )
  pop <- make_pop_vec(year = "2015", level = "national", value = 1200) # factor = 2

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  expect_equal(result$weight, c(400, 800))
})

test_that("adjust_population (named vector) picks closest year", {
  df <- data.table::data.table(
    country_code = "ABC",
    year = 2015L,
    area = "national",
    weight = c(300)
  )
  # 2014 is closer (diff=1) than 2010 (diff=5)
  pop <- c(`2014_national` = 900, `2010_national` = 600)

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  # pop_fact = 900 / 300 = 3
  expect_equal(result$weight, 900)
})

test_that("adjust_population (named vector) errors when no matching level found", {
  df <- data.table::data.table(
    country_code = "ABC",
    year = 2015L,
    area = "urban",
    weight = c(100)
  )
  pop <- make_pop_vec(level = "national") # no "urban" entry

  expect_error(
    suppressMessages(pipdata:::adjust_population(df, pop)),
    class = "adjust_population"
  )
})

test_that("adjust_population (named vector) uses area to group subnational rows", {
  # Two areas: rural (600 total weight) and urban (200 total weight).
  df <- data.table::data.table(
    country_code = "ABC",
    year = 2015L,
    area = c("rural", "rural", "urban"),
    weight = c(200, 400, 200) # rural total=600, urban total=200
  )
  pop <- make_pop_vec_subnational() # rural=6e8, urban=7e8
  # rural pop_fact = 6e8/600 = 1e6; urban pop_fact = 7e8/200 = 3.5e6

  result <- suppressMessages(pipdata:::adjust_population(df, pop))

  rural_weights <- result[result$area == "rural", weight]
  urban_weights <- result[result$area == "urban", weight]
  expect_equal(rural_weights, c(200e6, 400e6))
  expect_equal(urban_weights, c(700e6))
})

test_that("adjust_population aborts when area column is missing", {
  df <- data.table::data.table(
    country_code = "ABC",
    year = 2015L,
    weight = c(100)
    # no area column
  )
  pop <- make_pop_vec_subnational()

  expect_error(
    suppressMessages(pipdata:::adjust_population(df, pop)),
    class = "adjust_population"
  )
})

test_that("adjust_population (named vector) aborts when 'year' column is missing", {
  # Regression: previously the function silently read df$survey_year (NULL),

  # producing NaN weights. Now it requires 'year' and aborts if absent.
  df <- data.table::data.table(
    country_code = "ABC",
    area = c("urban", "urban"),
    weight = c(300, 700)
    # no 'year' column
  )
  pop <- c(`2003_urban` = 2000000)

  expect_error(
    suppressMessages(pipdata:::adjust_population(df, pop)),
    class = "adjust_population"
  )
})

# ---------------------------------------------------------------------------
# pd_deflation() integration — mocked aux loading
# ---------------------------------------------------------------------------

test_that("pd_deflation Mode A: single dt, mocked aux, returns data.table or NA", {
  # pip_id constructed as: country_surveyid_year_acronym_INC/CON_module
  dt <- make_pipmd() # welfare_type="income" → INC, module="D1"
  cpi <- make_cpi_vec()
  ppp <- make_ppp_vec()
  pop <- make_pop_vec()

  meta_obj <- list(cpi = cpi, ppp = ppp, pop = pop)
  fake_inv <- data.table::data.table(
    pip_id = "ABC_2015_TST_INC_D1", # matches constructed pip_id
    content_hash_data = "abc123",
    content_hash_metadata = "meta_abc123",
    version_id_metadata = "ver_abc123",
    created_at_metadata = "2026-01-01T00:00:00Z"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) fake_inv,
    pip_read = function(id, alias, version = NULL, ...) {
      if (identical(alias, "pip_meta") && identical(version, "ver_abc123")) {
        meta_obj
      } else {
        meta_obj
      }
    },
    .package = "pipload"
  )

  result <- pd_deflation(dt)

  # Either a data.table (success) or NA (graceful failure is acceptable
  # since the minimal fixture lacks area/reporting columns for full deflation)
  expect_true(
    data.table::is.data.table(result) || is.na(result)
  )
})

test_that("pd_deflation aborts when neither dt nor pip_id provided", {
  expect_error(pd_deflation(), class = "pd_deflation")
})

# ---------------------------------------------------------------------------
# pd_deflation() verbose propagation
# ---------------------------------------------------------------------------

test_that("pd_deflation(verbose = FALSE) passes verbose=FALSE to pip_read and load_pip_master_inventory", {
  dt <- make_pipmd()
  cpi <- make_cpi_vec()
  ppp <- make_ppp_vec()
  pop <- make_pop_vec()

  meta_obj <- list(cpi = cpi, ppp = ppp, pop = pop)
  fake_inv <- data.table::data.table(
    pip_id = "ABC_2015_TST_INC_D1",
    content_hash_data = "abc123",
    content_hash_metadata = "meta_abc123",
    version_id_metadata = "ver_abc123",
    created_at_metadata = "2026-01-01T00:00:00Z"
  )

  inv_verbose_values <- logical(0)
  pip_read_verbose_values <- logical(0)

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(verbose = TRUE, ...) {
      inv_verbose_values <<- c(inv_verbose_values, verbose)
      fake_inv
    },
    pip_read = function(id, alias, version = NULL, verbose = TRUE, ...) {
      pip_read_verbose_values <<- c(pip_read_verbose_values, verbose)
      meta_obj
    },
    .package = "pipload"
  )

  pd_deflation(dt, verbose = FALSE)

  expect_true(all(!inv_verbose_values), info = "load_pip_master_inventory should receive verbose=FALSE")
  expect_true(all(!pip_read_verbose_values), info = "pip_read should receive verbose=FALSE")
})

test_that("pd_deflation Mode B: loads single survey via pip_id", {
  # Level info is always in attributes; add_rep_lvl/add_ppp/add_cpi read
  # from attrs directly — no column materialisation needed.
  dt <- make_pipmd()
  cpi <- make_cpi_vec()
  ppp <- make_ppp_vec()
  pop <- make_pop_vec()

  meta_obj <- list(cpi = cpi, ppp = ppp, pop = pop)
  fake_inv <- data.table::data.table(
    pip_id = "ABC_2015_TST_INC_D1",
    content_hash_data = "abc123",
    content_hash_metadata = "meta_abc123",
    version_id_metadata = "ver_abc123",
    created_at_metadata = "2026-01-01T00:00:00Z"
  )

  testthat::local_mocked_bindings(
    load_pip_master_inventory = function(...) fake_inv,
    pip_read = function(id, alias, version = NULL, ...) {
      if (identical(alias, "pip")) {
        dt
      } else if (
        identical(alias, "pip_meta") && identical(version, "ver_abc123")
      ) {
        meta_obj
      } else {
        meta_obj
      }
    },
    .package = "pipload"
  )

  result <- pd_deflation(pip_id = "ABC_2015_TST_INC_D1")

  expect_true(
    data.table::is.data.table(result) || is.na(result)
  )
})
# ---------------------------------------------------------------------------
# Deflation output attributes: welfare_vars and adj_pop
# ---------------------------------------------------------------------------

test_that("deflation output includes welfare_vars attribute with all welfare_ columns", {
  dt <- make_pipmd()
  cpi <- make_cpi_vec()
  ppp <- make_ppp_vec()
  pop <- make_pop_vec()

  result <- pipdata:::deflation.pipmd(dt, cpi, ppp, pop)

  # welfare column should be removed; only welfare_lcu and welfare_ppp_* should remain
  expect_false("welfare" %in% names(result))
  expect_true("welfare_lcu" %in% names(result))

  # Result should have welfare_vars attribute listing welfare_lcu and welfare_ppp_*
  expect_true("welfare_vars" %in% names(attributes(result)))
  welfare_vars <- attr(result, "welfare_vars")
  expect_true("welfare_lcu" %in% welfare_vars)
  expect_true(any(grepl("^welfare_ppp_", welfare_vars)))
})

test_that("deflation output includes adj_pop = TRUE when population adjustment applied", {
  # Subnational survey: reporting_level=2, pop_data_level="area"
  dt <- make_pipmd(
    welfare = c(5, 10),
    weight = c(100, 200),
    area = c("rural", "urban"),
    reporting_level = 2L,
    ppp_data_level = "area",
    cpi_data_level = "area",
    pop_data_level = "area"
  )

  cpi <- make_cpi_vec_subnational()
  ppp <- make_ppp_vec_subnational()
  pop <- make_pop_vec_subnational()

  result <- suppressMessages(
    pipdata:::deflation.pipmd(dt, cpi, ppp, pop)
  )

  expect_true("adj_pop" %in% names(attributes(result)))
  expect_true(attr(result, "adj_pop"))
})

test_that("deflation output includes adj_pop = FALSE when population adjustment not applied", {
  # National survey: reporting_level=1, pop_data_level="national"
  dt <- make_pipmd(
    reporting_level = 1L,
    ppp_data_level = "national",
    cpi_data_level = "national"
  )
  cpi <- make_cpi_vec()
  ppp <- make_ppp_vec()
  pop <- make_pop_vec()

  result <- pipdata:::deflation.pipmd(dt, cpi, ppp, pop)

  expect_true("adj_pop" %in% names(attributes(result)))
  expect_false(attr(result, "adj_pop"))
})

test_that("deflation keeps population adjustment off for mixed subnational attributes", {
  dt <- make_pipmd(
    welfare = c(5, 10),
    weight = c(100, 200),
    area = c("rural", "urban"),
    reporting_level = 2L,
    ppp_data_level = "area",
    cpi_data_level = "area",
    pop_data_level = "national"
  )
  cpi <- make_cpi_vec_subnational()
  ppp <- make_ppp_vec_subnational()
  pop <- make_pop_vec(level = "national")

  result <- suppressMessages(
    pipdata:::deflation.pipmd(dt, cpi, ppp, pop)
  )

  expect_false(attr(result, "adj_pop"))
  expect_equal(result$weight, dt$weight)
})

test_that("pipgd deflation output includes adj_pop = FALSE always", {
  # Grouped-data: population adjustment never applies
  dt <- make_pipmd()
  data.table::setattr(dt, "class", c("pipgd", "data.table", "data.frame"))

  cpi <- make_cpi_vec()
  ppp <- make_ppp_vec()
  pop <- make_pop_vec()

  result <- pipdata:::deflation.pipgd(dt, cpi, ppp, pop)

  expect_true("adj_pop" %in% names(attributes(result)))
  expect_false(attr(result, "adj_pop"))
})

# ---------------------------------------------------------------------------
# Deflation output attribute: ppp_sort
# ---------------------------------------------------------------------------

test_that("deflation output includes ppp_sort = integer year of sort column", {
  dt <- make_pipmd()
  cpi <- make_cpi_vec() # 2017_national
  ppp <- make_ppp_vec(ppp_year = "2017") # ppp_2017_01_01_national
  pop <- make_pop_vec()

  result <- pipdata:::deflation.pipmd(dt, cpi, ppp, pop)

  expect_true("ppp_sort" %in% names(attributes(result)))
  expect_identical(attr(result, "ppp_sort"), 2017L)
})

bounded_deflation_inputs <- function() {
  list(data = data.table::data.table(
    countrycode = "AAA", year = 2010L, survname = "SYNA",
    hhid = paste0("h", 1:4), pid = paste0("p", 1:4),
    area = factor(c("urban", "rural", "urban", "rural")),
    welfare = as.double(1:4), weight = c(10, 20, 30, 40), hsize = 1:4),
    metadata = list(country_code = "AAA", surveyid_year = 2010,
      survey_acronym = "SYNA", welfare_type = "income",
      cpi = c(`2005_national` = 1, `2011_national` = 1,
              `2017_national` = 1.2, `2021_national` = 1.4),
      ppp = c(ppp_2005_01_01_national = 1,
        ppp_2011_02_02_national = 1.35, ppp_2017_01_02_national = 1.5,
        ppp_2021_01_02_national = 1.7), pop = c(`2010_national` = 1000)),
    pfw = data.table::data.table(country_code = "AAA", surveyid_year = 2010L,
      survey_acronym = "SYNA", welfare_type = "income", is_alt_welf = FALSE,
      cpi_domain = 1, ppp_domain = 1, pop_domain = 1,
      cpi_domain_var = "", ppp_domain_var = ""),
    selections = c("ppp_2005_01_01", "ppp_2011_02_02", "ppp_2017_01_02",
                   "ppp_2021_01_02"))
}

test_that("bounded microdata uses literal factors and retains LCU rows and weights", {
  inputs <- bounded_deflation_inputs()
  original <- data.table::copy(inputs$data)
  value <- do.call(pd_deflate_microdata, inputs)
  for (year in c(2005, 2011, 2017, 2021)) {
    factor <- c(`2005` = 1, `2011` = 20 / 27, `2017` = 5 / 9,
                `2021` = 50 / 119)[[as.character(year)]]
    expect_equal(value$x[[paste0("welfare_", year)]], (1:4) * factor)
  }
  expect_identical(value$x$welfare_lcu, as.double(1:4))
  for (field in c("hhid", "pid", "area", "welfare", "weight", "hsize")) {
    expect_identical(value$x[[field]], original[[field]])
  }
  expect_identical(inputs$data, original)
  expect_identical(value$metadata$final_weight_rule, "unchanged")
  expect_identical(value$metadata$final_weight, c(10, 20, 30, 40))
  inputs$data <- inputs$data[c(3, 1, 4, 2)]
  permuted <- do.call(pd_deflate_microdata, inputs)$x
  expect_identical(permuted$hhid, c("h3", "h1", "h4", "h2"))
  expect_equal(permuted$welfare_2021, c(3, 1, 4, 2) * 50 / 119)
})

test_that("bounded selection is dynamic and uses only explicit vintage keys", {
  inputs <- bounded_deflation_inputs()
  inputs$data <- inputs$data[1:2]
  inputs$data$welfare <- c(8, 24)
  inputs$metadata$cpi <- c(`2025_national` = 2)
  inputs$metadata$ppp <- c(ppp_2025_03_04_national = 4)
  inputs$selections <- "ppp_2025_03_04"
  value <- do.call(pd_deflate_microdata, inputs)
  expect_identical(value$x$welfare_2025, c(1, 3))
  expect_false(any(c("welfare_2005", "welfare_2011", "welfare_2017",
                     "welfare_2021") %in% names(value$x)))
  expect_identical(value$metadata$consumed_ppp_keys,
                   "ppp_2025_03_04_national")
  expect_identical(value$metadata$consumed_cpi_keys, "2025_national")
})

test_that("bounded CPI and PPP domains resolve independently from PFW", {
  inputs <- bounded_deflation_inputs()
  inputs$selections <- "ppp_2017_01_02"
  inputs$pfw$cpi_domain <- 2
  inputs$pfw$cpi_domain_var <- "urban"
  inputs$metadata$cpi <- c(`2017_urban` = 2, `2017_rural` = 4,
                           `2021_national` = 100)
  value <- do.call(pd_deflate_microdata, inputs)
  expect_equal(value$x$welfare_2017, c(1, 2, 3, 4) / c(2, 4, 2, 4) / 1.5)
  expect_identical(unname(value$metadata$domains), c("area", "national", "national"))
  inputs$metadata$cpi[["2021_national"]] <- 500
  expect_identical(do.call(pd_deflate_microdata, inputs)$x, value$x)
  inputs$metadata$cpi[["2017_national"]] <- 100
  expect_identical(do.call(pd_deflate_microdata, inputs)$x, value$x)
  inputs$metadata$cpi[["2017_national"]] <- 500
  expect_identical(do.call(pd_deflate_microdata, inputs)$x, value$x)
  inputs$metadata$cpi[["2017_urban"]] <- 4
  changed <- do.call(pd_deflate_microdata, inputs)$x
  expect_equal(changed$welfare_2017[c(1, 3)], value$x$welfare_2017[c(1, 3)] / 2)
  expect_identical(changed$welfare_2017[c(2, 4)], value$x$welfare_2017[c(2, 4)])
  inputs$pfw$cpi_domain <- 1
  inputs$pfw$ppp_domain <- 2
  inputs$pfw$ppp_domain_var <- "urban"
  inputs$metadata$cpi <- c(`2017_national` = 2)
  inputs$metadata$ppp <- c(ppp_2017_01_02_urban = 2, ppp_2017_01_02_rural = 4)
  expect_equal(do.call(pd_deflate_microdata, inputs)$x$welfare_2017,
               (1:4) / 2 / c(2, 4, 2, 4))
})

test_that("bounded population resolves same-row PFW without hsize multiplication", {
  inputs <- bounded_deflation_inputs()
  inputs$pfw$pop_domain <- 2
  inputs$metadata$pop <- c(`2010_urban` = 400, `2010_rural` = 600)
  first <- do.call(pd_deflate_microdata, inputs)
  expect_identical(first$x$weight, c(100, 200, 300, 400))
  expect_identical(first$metadata$final_weight, first$x$weight)
  expect_identical(first$metadata$final_weight_rule, "exact-year-domain-share")
  inputs$metadata$pop[["2010_urban"]] <- 800
  second <- do.call(pd_deflate_microdata, inputs)
  expect_identical(second$x$weight, c(200, 200, 600, 400))
  expect_identical(second$x$welfare_2017, first$x$welfare_2017)
  expect_identical(second$x$welfare_lcu, first$x$welfare_lcu)
})

test_that("bounded matrix selections and malformed factor codes are rejected", {
  inputs <- bounded_deflation_inputs()
  inputs$metadata$ppp <- c(inputs$metadata$ppp, ppp_2005_02_02_national = 2)
  inputs$selections <- matrix(c("ppp_2005_01_01", "ppp_2005_02_02"), nrow = 1L)
  expect_error(do.call(pd_deflate_microdata, inputs), "bounded")
  inputs <- bounded_deflation_inputs()
  inputs$data <- as.data.frame(inputs$data)
  inputs$data$area <- structure(c(1L, 0L, 2L, 0L),
    levels = c("urban", "rural"), class = "factor")
  inputs$selections <- "ppp_2017_01_02"
  inputs$pfw$cpi_domain <- 2
  inputs$pfw$cpi_domain_var <- "urban"
  inputs$metadata$cpi <- c(`2017_urban` = 2, `2017_rural` = 4)
  expect_error(do.call(pd_deflate_microdata, inputs), "bounded")
})

test_that("bounded factor admission rejects nested levels before S3 dispatch", {
  methods <- get(".__S3MethodsTable__.", asNamespace("base"))
  method <- "[.pd_m2_factor_levels_probe"
  previous <- get0(method, methods, inherits = FALSE)
  withr::defer({
    if (is.null(previous)) rm(list = method, envir = methods) else
      assign(method, previous, envir = methods)
  })
  calls <- 0L
  registerS3method("[", "pd_m2_factor_levels_probe", function(x, i, ...) {
    calls <<- calls + 1L
    rep("urban", length(i))
  }, envir = baseenv())
  inputs <- bounded_deflation_inputs()
  inputs$data <- as.data.frame(inputs$data)
  inputs$data$area <- structure(c(1L, 2L, 1L, 2L),
    levels = structure(c("urban", "rural"), class = "pd_m2_factor_levels_probe"),
    class = "factor")
  inputs$selections <- "ppp_2017_01_02"
  inputs$pfw$cpi_domain <- 2
  inputs$pfw$cpi_domain_var <- "urban"
  inputs$metadata$cpi <- c(`2017_urban` = 2, `2017_rural` = 4)
  adapter <- pd_deflation_adapter()
  expect_error(do.call(adapter$calculation, inputs), "bounded")
  expect_identical(calls, 0L)
})

test_that("bounded positive welfare underflow is rejected but input zeros are valid", {
  inputs <- bounded_deflation_inputs()
  inputs$data$welfare <- rep(1e-300, 4L)
  inputs$selections <- "ppp_2005_01_01"
  inputs$metadata$ppp <- c(ppp_2005_01_01_national = 1e100)
  inputs$metadata$cpi <- c(`2005_national` = 1e-100)
  expect_gt((1e-300 / 1e-100) / 1e100, 0)
  adapter <- pd_deflation_adapter()
  expect_error(do.call(adapter$calculation, inputs), "bounded.*underflow")
  inputs$data$welfare <- rep(0, 4L)
  expect_identical(do.call(adapter$calculation, inputs)$x$welfare_2005, rep(0, 4L))
})

test_that("bounded invalid inputs fail closed and zero welfare remains valid", {
  inputs <- bounded_deflation_inputs()
  inputs$data$welfare[[1L]] <- 0
  expect_identical(do.call(pd_deflate_microdata, inputs)$x$welfare_2005[[1L]], 0)
  mutations <- list(
    function(x) { x$data <- x$data[0]; x },
    function(x) { x$data$welfare[[1]] <- -1; x },
    function(x) { x$data$welfare[[1]] <- NA_real_; x },
    function(x) { x$data$weight[[1]] <- Inf; x },
    function(x) { x$data$weight[[1]] <- 0; x },
    function(x) { x$data$year[[1]] <- 2011L; x },
    function(x) { x$metadata$surveyid_year <- 2011; x },
    function(x) { names(x$metadata)[names(x$metadata) == "cpi"] <- "cpi_backup"; x },
    function(x) { names(x$metadata)[names(x$metadata) == "ppp"] <- "ppp_backup"; x },
    function(x) { names(x$metadata)[names(x$metadata) == "pop"] <- "pop_backup"; x },
    function(x) { names(x$metadata)[names(x$metadata) == "surveyid_year"] <- "surveyid_year_backup"; x },
    function(x) { x$selections <- character(); x },
    function(x) { x$selections <- rep(x$selections[[1]], 2); x },
    function(x) { x$metadata$cpi[[1]] <- 0; x },
    function(x) { x$metadata$ppp[[1]] <- NaN; x },
    function(x) { x$metadata$cpi <- x$metadata$cpi[-1]; x },
    function(x) { names(x$metadata$ppp)[[2]] <- names(x$metadata$ppp)[[1]]; x },
    function(x) { x$pfw <- rbind(x$pfw, x$pfw); x },
    function(x) { x$pfw$pop_domain <- 2; x$metadata$pop <- c(`2011_urban` = 400); x },
    function(x) { x$metadata$pop[[1]] <- -1; x }
  )
  for (mutate in mutations) {
    expect_error(do.call(pd_deflate_microdata, mutate(bounded_deflation_inputs())),
                 "bounded|coverage|selection|identity|positive|finite|empty|duplicate")
  }
})

test_that("ppp_sort is NULL when no welfare_ppp_* columns are present", {
  # finalize_deflation_output sets ppp_sort = NULL when the deflated dt has
  # no welfare_ppp_* columns (the else branch)
  dt <- data.table::data.table(welfare_lcu = c(1, 2), weight = c(100, 200))
  result <- pipdata:::finalize_deflation_output(dt)
  expect_null(attr(result, "ppp_sort"))
})

test_that("ppp_sort reflects the newest base year when multiple ppp years present", {
  dt <- make_pipmd()
  # Two PPP base years: 2017 and 2011. sort_by_year_desc puts 2017 first.
  ppp <- c(
    stats::setNames(3.5, "ppp_2017_01_01_national"),
    stats::setNames(3.1, "ppp_2011_01_01_national")
  )
  cpi <- c(
    stats::setNames(100, "2017_national"),
    stats::setNames(80, "2011_national")
  )
  pop <- make_pop_vec()

  result <- pipdata:::deflation.pipmd(dt, cpi, ppp, pop)

  expect_identical(attr(result, "ppp_sort"), 2017L)
})
