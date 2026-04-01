# These functions define valid values of data field
# They are all implemented as for loops to reduce memory allocations for large
# inputs.

valid_sex <- function(x) {
  if (!(is.numeric(x) || is.character(x))) {
    return(FALSE)
  }
  all_na <- TRUE
  if (is.numeric(x)) {
    for (el in x) {
      is_na <- is.na(el)
      if (is_na) {
        next
      }
      if (!(el == 1 || el == 2)) {
        return(FALSE)
      }
      if (all_na) {
        all_na <- FALSE
      }
    }
  } else {
    for (el in x) {
      is_na <- is.na(el)
      if (is_na) {
        next
      }
      if (
        !(el == "1" ||
          el == "M" ||
          el == "m" ||
          el == "2" ||
          el == "F" ||
          el == "f")
      ) {
        return(FALSE)
      }
      if (all_na) {
        all_na <- FALSE
      }
    }
  }
  return(!all_na)
}

valid_oedema <- function(x) {
  if (!(is.numeric(x) || is.character(x))) {
    return(FALSE)
  }
  all_na <- TRUE
  if (is.numeric(x)) {
    for (el in x) {
      is_na <- is.na(el)
      if (is_na) {
        next
      }
      if (
        !(el == 1 ||
          el == 2)
      ) {
        return(FALSE)
      }
      if (all_na) {
        all_na <- FALSE
      }
    }
  } else {
    for (el in x) {
      is_na <- is.na(el)
      if (is_na) {
        next
      }
      if (
        !(el == "Y" ||
          el == "y" ||
          el == "N" ||
          el == "n" ||
          el == "1" ||
          el == "2")
      ) {
        return(FALSE)
      }
      if (all_na) {
        all_na <- FALSE
      }
    }
  }
  return(!all_na)
}

valid_lh <- function(x) {
  if (!is.character(x)) {
    return(FALSE)
  }
  all_na <- TRUE
  for (el in x) {
    is_na <- is.na(el)
    if (is_na) {
      next
    }
    if (
      !(el == "L" ||
        el == "l" ||
        el == "H" ||
        el == "h")
    ) {
      return(FALSE)
    }
    if (all_na) {
      all_na <- FALSE
    }
  }
  return(!all_na)
}

valid_wiq <- function(x) {
  if (!(is.numeric(x) || is.character(x))) {
    return(FALSE)
  }
  all_na <- TRUE
  if (is.numeric(x)) {
    for (el in x) {
      is_na <- is.na(el)
      if (is_na) {
        next
      }
      if (
        !(el == 1 ||
          el == 2 ||
          el == 3 ||
          el == 4 ||
          el == 5)
      ) {
        return(FALSE)
      }
      if (all_na) {
        all_na <- FALSE
      }
    }
  } else {
    for (el in x) {
      is_na <- is.na(el)
      if (is_na) {
        next
      }
      if (
        !(el == "1" ||
          el == "2" ||
          el == "3" ||
          el == "4" ||
          el == "5" ||
          el == "Q1" ||
          el == "Q2" ||
          el == "Q3" ||
          el == "Q4" ||
          el == "Q5")
      ) {
        return(FALSE)
      }
      if (all_na) {
        all_na <- FALSE
      }
    }
  }
  return(!all_na)
}

valid_numeric <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }
  for (el in x) {
    if (!is.na(el)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

valid_weight <- valid_numeric
valid_height <- valid_numeric
valid_age <- valid_numeric

valid_date <- function(x) {
  if (lubridate::is.Date(x)) {
    return(TRUE)
  }
  if (!is.character(x)) {
    return(FALSE)
  }
  all_na <- TRUE
  for (el in x) {
    if (is.na(el)) {
      next
    }
    if (!grepl(pattern = "^\\d{1,2}/\\d{1,2}/\\d{4}$", x = el)) {
      return(FALSE)
    }
    if (all_na) {
      all_na <- FALSE
    }
  }
  return(!all_na)
}

# valid if at least one value is not NA
valid_always <- function(x) {
  for (el in x) {
    if (!is.na(el)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

valid_integerlike <- function(x) {
  if (!is.integer(x) && !is.numeric(x) && !is.character(x)) {
    return(FALSE)
  }
  if (is.integer(x)) {
    for (el in x) {
      if (!is.na(el)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  if (is.numeric(x)) {
    all_na <- TRUE
    for (el in x) {
      if (is.na(el)) {
        next
      }
      if (el %% 1 != 0) {
        return(FALSE)
      }
      if (all_na) {
        all_na <- FALSE
      }
    }
    return(!all_na)
  }
  if (is.character(x)) {
    all_na <- TRUE
    for (el in x) {
      if (is.na(el)) {
        next
      }
      suppressWarnings(x_integer <- as.integer(el))
      if (is.na(x_integer)) {
        # if el was not NA, but turned into NA we stop
        return(FALSE)
      }
      suppressWarnings(x_character <- as.character(x_integer))
      if (x_character != el) {
        return(FALSE)
      }
      if (all_na) {
        all_na <- FALSE
      }
    }
    return(!all_na)
  }
  return(FALSE)
}
