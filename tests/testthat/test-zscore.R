context("Module zscores")

source("../../code/macro-z.R")
source("../../code/zscore-dq-lib.R")

test_that("flagged zscores are converted to NA", {
  test_df <- data.frame(
    zlen = 1:3,
    zwei = 1:3,
    zbmi = 1:3,
    zwfl = 1:3,
    zlen_flag = c(0, 1, 0),
    zwei_flag = c(0, 1, 0),
    zbmi_flag = c(0, 1, NA_real_),
    zwfl_flag = c(0, 1, 0)
  )
  res <- set_flagged_zscores_to_na(test_df)
  expect_equal(res$zlen, c(1, NA_real_, 3))
  expect_equal(res$zwei, c(1, NA_real_, 3))
  expect_equal(res$zbmi, c(1, NA_real_, 3))
  expect_equal(res$zwfl, c(1, NA_real_, 3))
})

test_that("summarise zscores", {
  data <- data.frame(cyl = mtcars$cyl, vs = mtcars$vs)
  data$zlen <- c(rnorm(nrow(data) - 1), 2)
  data$zwei <- c(rnorm(nrow(data) - 1), 2)
  data$zbmi <- c(rnorm(nrow(data) - 1), 2)
  data$zwfl <- c(rnorm(nrow(data) - 1), 2)
  data$age_group <- c(rep.int("00-05 mo", nrow(data) - 1), NA_character_)
  data$csex <- "1"

  groups <- list(
    list(prefix = "Team", column = "cyl"),
    list(prefix = "Area", column = "vs")
  )
  res <- summarise_zscore_data(data, groups = groups)
  expect_equal(
    res$group,
    c(
      "All",
      "Age group: 00-05 mo",
      "Sex: Male",
      paste0("Team: ", sort(unique(data$cyl))),
      paste0("Area: ", unique(data$vs))
    )
  )
  first_row <- res[1L, ]
  zscore_cols <- c("zlen", "zwei", "zbmi", "zwfl")
  expect_equal(first_row[["n"]], nrow(data))
  rd <- function(x) sprintf("%.2f", round(x, 2))
  for (col in zscore_cols) {
    zscores <- data[[col]]
    expect_equal(first_row[[paste0(col, "_mean")]], rd(mean(zscores)))
    expect_equal(first_row[[paste0(col, "_sd")]], rd(sd(zscores)))
    expect_equal(
      first_row[[paste0(col, "_skewness")]],
      rd(moments::skewness(zscores))
    )
    expect_equal(
      first_row[[paste0(col, "_kurtosis")]],
      rd(moments::kurtosis(zscores))
    )
  }
})

test_that("rounding up works", {
  expect_equal(round_up(c(NA_real_, 730.49, 730.5)), c(NA_real_, 730, 731))
  expect_equal(round_up(NA_real_), NA_real_)
  expect_equal(round_up(73.5), 74)
  expect_equal(round_up(numeric()), numeric())
  expect_error(round_up("730"))
  expect_error(round_up(-1))
})

test_that("z-score adjusts weight/lenhei with 2019 standards", {
  data <- data.frame(
    sex = 1,
    age_in_days = 100,
    age_in_months = 3,
    age_group = "00-30",
    weight = c(0.4, 0.5, 10, 40, 41),
    lenhei = c(34, 65, 110, 140, 141)
  )
  result <- CalculateZScores(
    data,
    sex = "sex",
    weight = "weight",
    lenhei = "lenhei"
  )
  expect_equal(is.na(result$cbmi), c(TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(is.na(result$zwfl), c(TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(is.na(result$zbmi), c(TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(is.na(result$zlen), c(TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(is.na(result$zwei), c(TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("z-score computation can handle negative ages", {
  data <- data.frame(
    sex = 1,
    age_in_days = -100,
    age_in_months = -3,
    age_group = "00-30",
    weight = c(0.4, 0.5, 10, 40, 41),
    lenhei = c(34, 65, 110, 140, 141)
  )
  result <- CalculateZScores(
    data,
    sex = "sex",
    weight = "weight",
    lenhei = "lenhei"
  )
  expect_equal(is.na(result$zwfl), c(TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(is.na(result$zbmi), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(is.na(result$zlen), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(is.na(result$zwei), c(TRUE, TRUE, TRUE, TRUE, TRUE))
})

test_that("z-score output has an odema column", {
  data <- data.frame(
    sex = 1,
    age_in_days = 100,
    age_in_months = 100,
    age_group = "00-30",
    weight = c(0.4, 0.5, 10, 40, 41),
    lenhei = c(34, 65, 110, 140, 141),
    oedema = c("y", "n", "y", "n", "y")
  )
  result <- CalculateZScores(
    data,
    sex = "sex",
    weight = "weight",
    lenhei = "lenhei",
    oedema = "oedema"
  )
  expect_equal(result[["coedema"]], data$oedema)
  result <- CalculateZScores(
    data,
    sex = "sex",
    weight = "weight",
    lenhei = "lenhei"
  )
  expect_equal(result[["coedema"]], rep.int("n", nrow(data)))
})

test_that("conflicting columns are handled", {
  data <- data.frame(
    sex = 1,
    age_in_days = 100,
    age_in_months = 100,
    age_group = "00-30",
    weight = c(0.4, 0.5, 10, 40, 41),
    lenhei = c(34, 65, 110, 140, 141),
    oedema = c("y", "n", "y", "n", "y"),
    zwfl_flag = "test",
    other_col = "test2"
  )
  expect_silent(
    result <- CalculateZScores(
      data,
      sex = "sex",
      weight = "weight",
      lenhei = "lenhei"
    )
  )
  expect_equal(which(colnames(result) == "coedema"), 9)
  expect_contains(colnames(result), "zwfl_flag")
  expect_true(is.numeric(result$zwfl_flag))
  expect_contains(colnames(result), "uid")
})
