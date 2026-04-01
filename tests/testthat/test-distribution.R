context("Module destribution")

source("../../code/functions-helper.R")
source("../../code/distributions-lib.R")

test_that("compute_mismatch_table is correct", {
  test_dataset <- data.frame(
    age_in_months = c(20, 26, 20, 26, 24, 20),
    lh = c("l", "l", "h", "h", "h", "l"),
    stringsAsFactors = FALSE
  )
  res <- dist_compute_mismatch_table(
    test_dataset,
    as.symbol("lh"),
    as.symbol("age_in_months")
  )
  expect_equal(res$Total, c(3, 3, 6))
  expect_equal(res$`Observed mismatch*`, c(1, 1, 2))
  expect_equal(
    res$`% mismatch*`,
    scales::percent(c(1 / 3, 1 / 3, 2 / 6), accuracy = 0.1)
  )
})
