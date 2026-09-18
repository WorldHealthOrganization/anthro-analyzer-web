## Function for calculating the z-scores for all indicators
CalculateZScores <- function(
  data,
  sex = NULL,
  weight = NULL,
  lenhei = NULL,
  lenhei_unit = NULL,
  oedema = NULL
) {
  stopifnot(c("age_in_days", "age_group", "age_in_months") %in% colnames(data))
  stopifnot(!is.null(sex))

  # remove column for data that will be added later to prevent bugs
  col_names <- colnames(data)
  zscore_cols <- c("wfl", "bmi", "len", "wei")
  zf_cols <- c(
    "clenhei",
    "cmeasure",
    "c9mo_flag",
    "cbmi",
    "csex",
    unlist(
      purrr::transpose(
        list(paste0("z", zscore_cols), paste0("f", zscore_cols))
      )
    )
  )
  col_names <- setdiff(col_names, c("uid", zf_cols))
  original_data <- data[, col_names, drop = FALSE]
  cleaned_data <- data

  # adjustments of weight/lenhei according to 2019 WHO/UNICEF
  if (!is.null(weight) && weight != "None") {
    cleaned_data[[weight]] <- dplyr::if_else(
      dplyr::between(cleaned_data[[weight]], 0.5, 40),
      cleaned_data[[weight]],
      NA_real_
    )
  }
  if (!is.null(lenhei) && lenhei != "None") {
    cleaned_data[[lenhei]] <- dplyr::if_else(
      dplyr::between(cleaned_data[[lenhei]], 35, 140),
      cleaned_data[[lenhei]],
      NA_real_
    )
  }
  col_value <- function(col_name, default_val) {
    if (is.null(col_name) || col_name == "None") {
      default_val
    } else {
      cleaned_data[[col_name]]
    }
  }

  # negative ages are set to NA
  cleaned_data[["age_in_days"]] <- dplyr::if_else(
    cleaned_data[["age_in_days"]] < 0,
    NA_real_,
    cleaned_data[["age_in_days"]]
  )
  coedema <- col_value(oedema, "n")

  zscores <- anthro::anthro_zscores(
    sex = cleaned_data[[sex]],
    age = cleaned_data[["age_in_days"]],
    is_age_in_month = FALSE,
    weight = col_value(weight, NA_real_),
    lenhei = col_value(lenhei, NA_real_),
    measure = col_value(lenhei_unit, NA_character_),
    oedema = coedema
  )

  # now we need to rename and remove certain columns

  zscores <- zscores[, zf_cols, drop = FALSE]

  zscores[["uid"]] <- seq_len(nrow(zscores))
  new_flag_vars <- paste0("z", zscore_cols, "_flag")
  old_flag_vars <- paste0("f", zscore_cols)
  names(old_flag_vars) <- new_flag_vars
  zscores <- dplyr::rename(zscores, !!old_flag_vars)
  cols_to_remove <- c("hc", "ac", "ts", "ss")
  cols_to_remove <- c(
    paste0("f", cols_to_remove),
    paste0("z", cols_to_remove),
    paste0("f", zscore_cols)
  )
  zscores <- zscores[, setdiff(colnames(zscores), cols_to_remove), drop = FALSE]

  # we want to make sure the input to oedema is always in the output dataset
  # we also want coedema to be placed at the beginning
  zscores[["coedema"]] <- coedema
  zscores <- zscores[, c("coedema", setdiff(colnames(zscores), "coedema"))]

  # now we need to check for naming conflicts before merging with original data
  # in case of a conflict, we select the data from `zscores`
  conflicts <- intersect(colnames(zscores), colnames(original_data))
  original_data <- original_data[,
    setdiff(colnames(original_data), conflicts),
    drop = FALSE
  ]
  dplyr::bind_cols(original_data, zscores)
}
