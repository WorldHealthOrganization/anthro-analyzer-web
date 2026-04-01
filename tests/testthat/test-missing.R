context("missing data")

source("../../code/missing-data-lib.R")

test_that("it correctly computes the missing values #1", {
  data <- mtcars
  data$drat[data$drat < 3.9] <- NA_real_
  res <- count_missing_values(data, c("disp", "drat"), character(0))
  expect_equal(res$n, c(32L, 32L))
  expect_equal(res$na, c(0L, 20L))
  expect_equal(res$var, c("disp", "drat"))
  res <- count_missing_values(data, c("disp", "drat"), "cyl")
  expect_equal(res$na, c(0, 0, 0, 4, 3, 13))
})

test_that("it correctly computes the missing values #2", {
  data <- mtcars
  data$drat[data$drat < 3.9] <- NA_real_
  res <- count_missing_values(data, "drat", character(0))
  expect_equal(res$n, 32L)
  expect_equal(res$na, 20L)
  res <- count_missing_values(data, "drat", "cyl")
  expect_equal(res$na, c(4, 3, 13))
})

test_that("it correctly computes the missing values #3", {
  data <- mtcars
  data$drat[data$drat < 3.9] <- NA_real_
  data[["drat_drat_drat"]] <- data$drat
  res <- count_missing_values(data, c("disp", "drat_drat_drat"), character(0))
  expect_equal(res$n, c(32L, 32L))
  expect_equal(res$na, c(0L, 20L))
  expect_equal(res$var, c("disp", "drat_drat_drat"))
})
