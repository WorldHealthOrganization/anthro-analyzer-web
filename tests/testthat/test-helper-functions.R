context("helpers")

source("../../code/functions-helper.R")
source("../../code/data-assumptions.R")

test_that("age groups are computed correctly", {
  range <- 0:65
  groups <- anthro_age_groups(range)

  expect_true(is.factor(groups))
  expected_groups <- c(
    "00-05 mo",
    "06-11 mo",
    "12-23 mo",
    "24-35 mo",
    "36-47 mo",
    "48-59 mo"
  )
  expect_equal(levels(groups), expected_groups)
  expect_equal(as.character(anthro_age_groups(59.9)), "48-59 mo")

  # age 60.5 is not in group 48-59 mo
  expect_true(is.na(anthro_age_groups(60.5)))
  expect_true(is.na(anthro_age_groups(60)))
  # age 61 in NA
  expect_true(is.na(anthro_age_groups(61)))
})

test_that("dates accepts various formats", {
  expect_true(valid_date("21/10/2018"))
  expect_true(valid_date("10/30/2018"))
  expect_true(valid_date("7/3/2018"))
  expect_true(valid_date("31/3/2018"))
  expect_true(valid_date("99/3/2018")) # just check syntax atm
  expect_true(valid_date(Sys.Date()))
})

test_that("age in months is mapped correctly to years", {
  expect_equal(age_in_months_to_years(0), 0)
  expect_equal(age_in_months_to_years(11), 0)
  expect_equal(age_in_months_to_years(12), 1)
  expect_equal(age_in_months_to_years(23), 1)
  expect_equal(age_in_months_to_years(24), 2)
  expect_equal(age_in_months_to_years(35), 2)
  expect_equal(age_in_months_to_years(36), 3)
  expect_equal(age_in_months_to_years(47), 3)
  expect_equal(age_in_months_to_years(48), 4)
  expect_equal(age_in_months_to_years(59), 4)
  expect_equal(age_in_months_to_years(60), NA_integer_)
  expect_equal(age_in_months_to_years(integer()), integer(0))
  expect_error(age_in_months_to_years("some string"))
})
