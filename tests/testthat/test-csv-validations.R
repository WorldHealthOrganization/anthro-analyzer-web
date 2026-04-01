context("csv-validations")

source("../../code/data-assumptions.R")
source("../../code/input-variable-mapping.R")
source("../../modules/upload.R")

expect_upload_error <- function(object, msg) {
  expect_s3_class(object, "upload_error")
  expect_match(object$error, msg, fixed = TRUE)
}

describe("read_user_csv", {
  config <- list(
    max_upload_size = 50 * 1024**2
  )
  it("removes csv on exit", {
    t <- tempfile()
    saveRDS(mtcars, file = t)
    expect_true(file.exists(t))
    read_user_csv(basename(t), t, anthro_input_variable_mapping(), config)
    expect_true(!file.exists(t))
  })
  it("only accepts extension .csv", {
    f <- file.path(tempdir(), "my_file.txt")
    write.csv(mtcars, file = f)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_upload_error(res, "file needs to be a csv")
  })
  it("rejects file that are too large", {
    config$max_upload_size <- 1
    f <- file.path(tempdir(), "mtcars.csv")
    write.csv(mtcars, file = f)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_upload_error(res, "too large")
  })
  it("validates the utf-8 encoding", {
    f <- file.path(tempdir(), "mtcars.csv")
    saveRDS(mtcars, f)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_upload_error(res, "UTF-8")
  })
  it("validates the commas per line", {
    f <- file.path(tempdir(), "mtcars.csv")
    df <- as.data.frame(t(as.character(1:300)))
    write.csv(df, f)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_upload_error(res, "many columns")
  })
  it("errors if file has no data", {
    f <- file.path(tempdir(), "mtcars.csv")
    writeLines("", f)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_upload_error(res, "any rows")
  })
  it("validates if file has just one column", {
    f <- file.path(tempdir(), "mtcars.csv")
    write.csv(mtcars[, 1, drop = FALSE], f, row.names = FALSE)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_upload_error(res, "one column")
  })
  it("validates if there are too many rows", {
    f <- file.path(tempdir(), "mtcars.csv")
    config$max_rows <- 5
    writeLines(paste0(as.character(1:10), ",a"), f)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_upload_error(res, "too many rows")
  })
  it("validates maximum columns", {
    f <- file.path(tempdir(), "mtcars.csv")
    df <- as.data.frame(t(as.character(1:100)))
    write.csv(df, f)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_upload_error(res, "100 are supporte")
  })
  it("drops invalid column names", {
    f <- file.path(tempdir(), "mtcars.csv")
    df <- mtcars
    df[["mpg@("]] <- df$mpg
    write.csv(df, f)
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_true(is.list(res))
    expect_equal(colnames(res$result), colnames(mtcars))
  })
  it("sanitizes character columns", {
    f <- file.path(tempdir(), "mtcars.csv")
    writeLines(
      c(
        "csv-injection,white-space,empty",
        "=1+1, a ,",
        "-1+1, a ,",
        "@1+1, a ,",
        "+1+1, a ,",
        "\n1+1, a ,",
        "\t1+1, a ,",
        "\r1+1, a ,"
      ),
      f
    )
    res <- read_user_csv(
      basename(f),
      f,
      anthro_input_variable_mapping(),
      config
    )
    expect_match(res$result[["csv-injection"]], "^'")
    expect_equal(res$result[["white-space"]], rep_len("a", nrow(res$result)))
    expect_true(
      all(is.na(res$result[["empty"]]))
    )
  })
})
