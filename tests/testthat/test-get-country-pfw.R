# Tests for get_country_pfw.R
#
# Covers:
#   report_lvl()  — reporting_level computation, abort branches, dcols check
#   cache_id()    — cache_id construction, welfare_type abort

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_cpfw <- function(
  country_code = "ABC",
  surveyid_year = 2015L,
  survey_acronym = "TST",
  welfare_type = "income",
  inpovcal = 1L,
  cpi_domain = 1L,
  ppp_domain = 1L,
  gdp_domain = 1L,
  pce_domain = 1L,
  pop_domain = 1L
) {
  data.table::data.table(
    country_code = country_code,
    surveyid_year = surveyid_year,
    survey_acronym = survey_acronym,
    welfare_type = welfare_type,
    inpovcal = inpovcal,
    cpi_domain = cpi_domain,
    ppp_domain = ppp_domain,
    gdp_domain = gdp_domain,
    pce_domain = pce_domain,
    pop_domain = pop_domain
  )
}

# ---------------------------------------------------------------------------
# report_lvl() — happy path
# ---------------------------------------------------------------------------

test_that("report_lvl sets reporting_level '1' when all domains are 1", {
  cpfw <- make_cpfw()

  result <- pipdata:::report_lvl(cpfw)

  expect_equal(result$reporting_level, "1")
})

test_that("report_lvl sets reporting_level '2' when any domain is 2", {
  # cpi_domain = 2 → subnational CPI → reporting_level should be "2"
  cpfw <- make_cpfw(cpi_domain = 2L)

  result <- pipdata:::report_lvl(cpfw)

  expect_equal(result$reporting_level, "2")
})

test_that("report_lvl filters rows where inpovcal != 1", {
  cpfw <- data.table::rbindlist(list(
    make_cpfw(inpovcal = 1L, cpi_domain = 1L),
    make_cpfw(inpovcal = 0L, cpi_domain = 2L) # excluded by filter
  ))

  result <- pipdata:::report_lvl(cpfw)

  expect_equal(nrow(result), 1L)
  expect_equal(result$reporting_level, "1")
})

# ---------------------------------------------------------------------------
# report_lvl() — abort branches
# ---------------------------------------------------------------------------

test_that("report_lvl aborts when PFW has no inpovcal == 1 rows", {
  cpfw <- make_cpfw(inpovcal = 0L) # all excluded → nrow == 0 after filter

  expect_error(
    pipdata:::report_lvl(cpfw),
    class = "info_pfw"
  )
})

test_that("report_lvl aborts when multiple rows share the same welfare_type", {
  cpfw <- data.table::rbindlist(list(
    make_cpfw(welfare_type = "income"),
    make_cpfw(welfare_type = "income") # duplicate same type → non-unique
  ))

  expect_error(
    pipdata:::report_lvl(cpfw),
    class = "no_unq_pfw"
  )
})

test_that("report_lvl aborts when domain columns are missing from cpfw", {
  cpfw <- make_cpfw()
  cpfw[, cpi_domain := NULL] # drop one required domain column

  expect_error(
    pipdata:::report_lvl(cpfw),
    class = "report_lvl"
  )
})

# ---------------------------------------------------------------------------
# cache_id() — happy path
# ---------------------------------------------------------------------------

test_that("cache_id builds correct INC cache_id for income welfare_type", {
  cpfw <- make_cpfw()
  cpfw[, reporting_level := "1"]
  att <- list(
    country_code = "ABC",
    surveyid_year = 2015L,
    survey_acronym = "TST",
    module = "D1"
  )

  result <- pipdata:::cache_id(att = att, cpfw = cpfw)

  expect_type(result, "list")
  expect_true("ABC_2015_TST_INC_D1" %in% names(result))
})

test_that("cache_id builds correct CON cache_id for consumption welfare_type", {
  cpfw <- make_cpfw(welfare_type = "consumption")
  cpfw[, reporting_level := "1"]
  att <- list(
    country_code = "XYZ",
    surveyid_year = 2018L,
    survey_acronym = "HIES",
    module = "ALL"
  )

  result <- pipdata:::cache_id(att = att, cpfw = cpfw)

  expect_true("XYZ_2018_HIES_CON_ALL" %in% names(result))
})

test_that("cache_id aborts when welfare_type is not income or consumption", {
  cpfw <- make_cpfw(welfare_type = "expenditure") # not a recognised type
  cpfw[, reporting_level := "1"]
  att <- list(
    country_code = "ABC",
    surveyid_year = 2015L,
    survey_acronym = "TST",
    module = "D1"
  )

  expect_error(
    pipdata:::cache_id(att = att, cpfw = cpfw),
    class = "no_wlf_tp"
  )
})
