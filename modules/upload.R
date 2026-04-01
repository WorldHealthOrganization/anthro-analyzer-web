UploadInput <- function(id) {
  ns <- NS(id)
  fileInput(
    ns("data_upload"),
    HTML("Upload data<br>(.csv format)"),
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  )
}

Upload <- function(input, output, session) {
  reactive({
    input$data_upload
  })
}

read_user_csv <- function(file_name, file_path, df_map_vars, config) {
  on.exit({
    suppressWarnings({
      try(file.remove(file_path), silent = TRUE)
    })
  })

  sanitize_file_name <- function(x) {
    basename(trimws(x))
  }

  validate_file_name <- function(x) {
    stringi::stri_enc_isascii(x) &&
      grepl(
        x = x,
        pattern = "^[a-zA-Z0-9][a-zA-Z0-9_\\-]{0,219}\\.csv$"
      )
  }

  validate_mime_type <- function(x) {
    mime <- mime::guess_type(x, empty = FALSE)
    mime %in% c("text/csv", "text/plain", "text/comma-separated-values")
  }

  validate_encoding <- function(content) {
    stringi::stri_enc_isutf8(content)
  }

  valid_column_names <- function(col_names) {
    grepl("^[a-zA-Z0-9_\\-]{1,30}$", col_names)
  }

  upload_error <- function(msg) {
    structure(
      list(error = msg),
      class = "upload_error"
    )
  }

  # this check is executed in a separate process in a hosted environment
  local_config <- list(
    max_comma_count_per_line = 150,
    max_cols = 100,
    max_rows = 2 * 1000 * 1000
  )
  if (!is.null(config$max_rows)) {
    local_config$max_rows <- config$max_rows
  }
  try({
    suppressMessages({
      # input validation
      file_name <- sanitize_file_name(file_name)
      if (!validate_file_name(file_name)) {
        return(upload_error(
          'The uploaded file needs to be a csv and the file name can only contain characters, numbers, "_" and "-".'
        ))
      }
      if (tolower(tools::file_ext(file_path)) != "csv") {
        # The file path is chosen by shiny, but we check the extension just in case
        # Important since read_csv can make network calls or uncompress files
        # depending on extension.
        return(upload_error(
          'The uploaded file needs with .csv'
        ))
      }
      if (!file.exists(file_path)) {
        # this is unexpected
        return(upload_error(
          'Something went wrong processing the request.'
        ))
      }
      if (file.size(file_path) > config$max_upload_size) {
        return(upload_error(
          "The upload file is too large"
        ))
      }
      if (!validate_mime_type(file_path)) {
        return(upload_error(
          "The uploaded file needs to have mime type 'text/csv' or 'text/plain'"
        ))
      }
      raw_content <- readr::read_file_raw(
        file_path
      )
      if (!validate_encoding(raw_content)) {
        return(upload_error(
          "The uploaded file needs to be UTF-8 encoded"
        ))
      }
      lines <- readr::read_lines(
        I(raw_content),
        locale = readr::locale(encoding = "UTF-8")
      )
      if (
        any(
          stringr::str_count(lines, stringr::fixed(",")) >
            local_config$max_comma_count_per_line
        )
      ) {
        return(upload_error(
          "The uploaded file appears to have too many columns"
        ))
      }
      # parse
      data <- readr::read_csv(
        I(raw_content),
        locale = readr::locale(encoding = "UTF-8")
      )
      # validate further
      if (!is.data.frame(data)) {
        return(upload_error(
          'You uploaded file is not a valid csv file with "," as delimiter.'
        ))
      }
      rownames(data) <- NULL
      data <- tibble::as_tibble(data)

      # remove all NA rows
      old_rows <- nrow(data)
      data <- data[rowSums(is.na(data)) < ncol(data), ]
      num_removed_na_rows <- old_rows - nrow(data)
      if (ncol(data) == 0L) {
        return(upload_error(
          "Your dataset does not have any rows. Maybe the csv file is not formatted correctly?"
        ))
      }
      if (ncol(data) <= 1L) {
        return(upload_error(
          "Your dataset appears to only have one column. Maybe the csv file is not formatted correctly?"
        ))
      }
      if (nrow(data) == 0) {
        return(upload_error(
          "Your dataset does not have any rows. Maybe the csv file is not formatted correctly?"
        ))
      }
      if (ncol(data) > local_config$max_cols) {
        return(upload_error(
          "Your dataset has too many columns. Only 100 are supported."
        ))
      }
      if (nrow(data) > local_config$max_rows) {
        return(upload_error(
          "Your dataset has too many rows. Only 2 million are currently supported."
        ))
      }
      # we drop invalid column names
      is_valid_colname <- valid_column_names(colnames(data))
      if (any(!is_valid_colname)) {
        data <- data[, is_valid_colname, drop = FALSE]
      }
      if (ncol(data) <= 1) {
        return(upload_error(
          "After removing invalid column names your dataset only has a single column. Valid column names have only letters, underscores, hyphens or numbers and cannot be longer than 30 characters."
        ))
      }

      # sanitize
      data <- dplyr::mutate_if(data, is.character, trimws)
      data <- dplyr::mutate_if(data, is.character, function(x) {
        x[x == ""] <- NA_character_
        x
      })
      # we prepend a single quote to prevent CSV injections downstream
      # https://owasp.org/www-community/attacks/CSV_Injection
      data <- dplyr::mutate(
        data,
        across(
          where(\(x) {
            is.character(x) &&
              any(grepl("^[=+\\-@\t\r\n]", x, perl = TRUE), na.rm = TRUE)
          }),
          function(x) {
            paste0("'", x)
          }
        )
      )

      # the last compute intensive part is to compute which concepts (e.g. sex or age) map to
      # what columns of the dataset
      concept_mapping <- lapply(seq_len(nrow(df_map_vars)), function(i) {
        validator_explanation <- df_map_vars$validator_explanations[[i]]
        validator <- df_map_vars$validators[[i]]
        valid_df_names <- purrr::keep(colnames(data), function(col_name) {
          validator(data[[col_name]])
        })
        choices <- if (length(valid_df_names) == 0L) {
          c("Unavailable" = "None")
        } else {
          c("None", valid_df_names)
        }
        list(
          variable = df_map_vars$vars[i],
          explanation = validator_explanation,
          choices = choices
        )
      })
      list(
        result = data,
        file_name = file_name,
        file_name_sans_ext = tools::file_path_sans_ext(file_name),
        concept_mapping = concept_mapping,
        num_removed_na_rows = num_removed_na_rows
      )
    })
  })
}
