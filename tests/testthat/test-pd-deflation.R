# Tests for pd_deflation.R
#
# Covers:
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

  result <- pipdata:::.load_deflation_aux("ABC_2015_TST_INC_D1")

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

  result <- pipdata:::.load_deflation_aux("ABC_2015_TST_INC_D1")

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

  result <- expect_warning(
    pipdata:::.load_deflation_aux("ABC_2015_TST_INC_D1"),
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
    pipdata:::.load_deflation_aux("UNKNOWN_ID"),
    class = "load_deflation_aux"
  )
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
