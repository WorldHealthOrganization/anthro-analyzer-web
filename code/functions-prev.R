#' Computes the number of missing and excluded cases from the prevalence calculation
#' Only works in a reactive conext.
#'
#' This helper function is used in the prevalence module and in the report.
#'
#' @param dataset a data frame
#' @param list_map_vars a reactive list of variable mappings
anthro_prev_excluded_cases <- function(dataset, list_map_vars) {
  is_none <- function(x) x == "None"
  missing_values_names <- c(
    "sw" = "Sample Weight",
    "cluster" = "Cluster",
    "strata" = "Strata"
  )

  missing_value_counts <- map(names(missing_values_names), function(var) {
    col_name <- list_map_vars[[var]]()
    if (is_none(col_name)) {
      0L
    } else {
      as.integer(sum(is.na(dataset[[col_name]])))
    }
  })

  missing_values_names <- missing_values_names[missing_value_counts > 0L]
  missing_value_counts <- missing_value_counts[missing_value_counts > 0L]
  list(
    missing_values_names = missing_values_names,
    missing_value_counts = missing_value_counts
  )
}
