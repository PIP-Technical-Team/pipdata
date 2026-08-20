test_that("valid spec passes validation", {
  spec <- load_package_validation_spec()
  expect_true(validate_validation_spec(spec))
})

test_that("missing schema_version aborts", {
  spec <- list(modules = list(skip = list(validations = list())))
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("unknown type aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(bad = list(type = "bogus")))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("module without validations aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(test = list())
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("variable_availability with pattern aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "variable_availability", pattern = "x")
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("variable_availability without prefix aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "variable_availability")
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("severity on helper-fixed check aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(
          type = "numeric_validation",
          pattern = "x",
          checks = list(list(name = "is_numeric", severity = "warning"))
        )
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("bad severity value on not_missing aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(
          type = "not_missing",
          variable = "x",
          severity = "bogus"
        )
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("numeric_validation without pattern aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "numeric_validation")
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("value_constraint without valid_values aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "value_constraint", variable = "x")
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("uniqueness without key_variables aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "uniqueness")
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("data_presence without severity aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "data_presence")
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("validation_group with empty checks aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "validation_group", pattern = "x", checks = list())
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("entry-level bad severity on numeric_validation aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(
          type = "numeric_validation",
          pattern = "x",
          severity = "bogus"
        )
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("unknown check name in checks aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(
          type = "numeric_validation",
          pattern = "x",
          checks = list(list(name = "is_positivee"))
        )
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("single_variable with unknown check aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "single_variable", variable = "x", check = "bogus_check")
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("not_missing with unknown condition aborts", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        bad = list(type = "not_missing", variable = "x", severity = "warning", condition = "bogus")
      ))
    )
  )
  expect_error(
    validate_validation_spec(spec),
    class = "validation_spec_invalid"
  )
})

test_that("bare-string helper-fixed check passes (P2.5 guard)", {
  spec <- list(
    schema_version = "1.0",
    modules = list(
      test = list(validations = list(
        ok = list(
          type = "numeric_validation",
          pattern = "x",
          checks = list("is_numeric")
        )
      ))
    )
  )
  expect_true(validate_validation_spec(spec))
})
