## Function for calculating the prevalences for all indicators
CalculatePrev <- function(
  data,
  sex = NULL,
  age = NULL,
  age.month = FALSE,
  date_obs = NULL,
  date_birth = NULL,
  weight = NULL,
  lenhei = NULL,
  lenhei_unit = NULL,
  headc = NULL,
  armc = NULL,
  triskin = NULL,
  subskin = NULL,
  typeres = NULL,
  wealthq = NULL,
  gregion = NULL,
  mothered = NULL,
  othergr = NULL,
  sw = NULL,
  cluster = NULL,
  strata = NULL,
  oedema = NULL,
  on_progress = function() invisible(TRUE)
) {
  # check input
  # the code now takes the age group and age_in_days from the input data and does not
  # compute it again
  stopifnot(all(c("age_in_days", "age_group") %in% colnames(data)))

  # Remove empty rows
  data %<>% filter(!rowSums(is.na(.)) %in% ncol(.))

  col_value <- function(col_name, default_val) {
    if (is.null(col_name) || col_name == "None") {
      default_val
    } else {
      data[[col_name]]
    }
  }

  anthro::anthro_prevalence(
    sex = data[[sex]],
    age = data[["age_in_days"]],
    is_age_in_month = FALSE,
    weight = col_value(weight, NA_real_),
    lenhei = col_value(lenhei, NA_real_),
    measure = col_value(lenhei_unit, NA_character_),
    oedema = col_value(oedema, "n"),
    sw = col_value(sw, NULL),
    cluster = col_value(cluster, NULL),
    strata = col_value(strata, NULL),
    wealthq = col_value(wealthq, NA_character_),
    othergr = col_value(othergr, NA_character_),
    gregion = col_value(gregion, NA_character_),
    mothered = col_value(mothered, NA_character_),
    typeres = col_value(typeres, NA_character_)
  )
}

# this function takes a dataset and counts the oedema = yes cases
# by group
# this is needed to report the number of oedema cases per stratum
count_oedema_yes <- function(data, stratifications, oedema_col) {
  total_oedema <- sum(is_oedema_yes(data[[oedema_col]]))
  res <- lapply(stratifications, function(group) {
    df <- dplyr::filter(data, !is.na(!!as.symbol(group)))
    df <- dplyr::group_by(df, !!as.symbol(group))
    df <- dplyr::summarise(
      df,
      oedema_yes = sum(is_oedema_yes(!!(as.symbol(oedema_col))))
    )
    df[["oedema_yes"]]
  })
  c(total_oedema, unlist(res))
}
