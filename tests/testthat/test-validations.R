context("validations")

source("../../modules/validator.R")
source("../../code/data-assumptions.R")

test_that("check_validation_conditions returns ok if all conditions are ok", {
  res <- check_validation_conditions(
    condition(
      FALSE,
      validation_error(
        "Test."
      )
    )
  )
  expect_equal(res, validation_ok())
})

test_that("check_validation_conditions returns error if a condition fails ", {
  res <- check_validation_conditions(
    condition(
      TRUE,
      validation_error(
        "Test."
      )
    )
  )
  expect_equal(res$type, "error")
})

test_that("valid_sex", {
  expect_false(valid_sex(3i))
  expect_false(valid_sex(list(1)))
  expect_false(valid_sex(3))
  expect_false(valid_sex(FALSE))
  expect_false(valid_sex("3"))
  expect_false(valid_sex(integer()))
  expect_false(valid_sex(c(NA_real_, NA_real_)))
  expect_true(valid_sex(c("1", "2", "m", "M", "f", "F")))
})

test_that("valid_oedema", {
  expect_false(valid_oedema(3i))
  expect_false(valid_oedema(list(1)))
  expect_false(valid_oedema(3))
  expect_false(valid_oedema(FALSE))
  expect_false(valid_oedema("3"))
  expect_false(valid_oedema(integer()))
  expect_false(valid_oedema(c(NA_character_, NA_character_)))
  expect_true(valid_oedema(c("Y", "y", "n", "N", "1", "2")))
})

test_that("valid_lh", {
  expect_false(valid_lh(3i))
  expect_false(valid_lh(list(1)))
  expect_false(valid_lh(3))
  expect_false(valid_lh(FALSE))
  expect_false(valid_lh("3"))
  expect_false(valid_lh(integer()))
  expect_false(valid_lh(c(NA_real_, NA_real_)))
  expect_true(valid_lh(c("L", "l", "H", "h")))
})

test_that("valid_wiq", {
  expect_false(valid_wiq(3i))
  expect_false(valid_wiq(list(1)))
  expect_false(valid_wiq(10))
  expect_false(valid_wiq(FALSE))
  expect_false(valid_wiq("10"))
  expect_false(valid_wiq(integer()))
  expect_false(valid_wiq(c(NA_character_, NA_character_)))
  expect_true(valid_wiq(c(as.character(1:5), paste0("Q", 1:5))))
  expect_true(valid_wiq(1:5))
})

test_that("valid_numeric", {
  expect_true(valid_numeric(3))
  expect_true(valid_numeric(c(3, NA_real_)))
  expect_false(valid_numeric("3"))
  expect_false(valid_numeric(NA_real_))
  expect_false(valid_numeric(numeric()))
})

test_that("valid_date", {
  expect_false(valid_date(10))
  expect_false(valid_date("2025-01-01"))
  expect_false(valid_date("123/01/2025"))
  expect_false(valid_date("1/01/25"))
  expect_true(valid_date(lubridate::today()))
  expect_true(valid_date("01/01/2025"))
  expect_true(valid_date("1/1/2025"))
})

test_that("valid_integerlike", {
  expect_false(valid_integerlike(10.5))
  expect_false(valid_integerlike("10.5"))
  expect_false(valid_integerlike(NA_character_))
  expect_false(valid_integerlike(NA_real_))
  expect_false(valid_integerlike(NA_integer_))
  expect_false(valid_integerlike(character()))
  expect_false(valid_integerlike(numeric()))
  expect_false(valid_integerlike(integer()))
  expect_false(valid_integerlike("asd"))
  expect_true(valid_integerlike(10L))
  expect_true(valid_integerlike(10.0))
  expect_true(valid_integerlike("10"))
})
