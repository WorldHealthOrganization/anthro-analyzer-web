DFTableOutput <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Dataset",
    icon = icon("table"),
    tags$style(type = "text/css", ".row {margin-top: 15px;}"),
    tags$head(tags$style("tfoot {display: table-header-group;}")), # puts filter at top of table
    fluidRow(
      uiOutput(ns("warning")),
      column(12, DT::dataTableOutput(ns("table")))
    )
  )
}

DFTable <- function(
  input,
  output,
  session,
  dataset,
  num_removed_na_rows,
  number_age_set_to_na
) {
  stopifnot(is.reactive(num_removed_na_rows))
  output$table <- DT::renderDataTable(
    {
      validate(
        need(
          is.data.frame(dataset()),
          "The uploaded data does not seem to be a table"
        ),
        need(nrow(dataset()) > 0, "The data needs to have at least one row")
      )
      dplyr::select(dataset(), -uid)
    },
    options = list(paging = TRUE, pageLength = 25, searching = FALSE),
    rownames = FALSE,
    filter = "none",
    escape = TRUE
  )

  output$warning <- renderUI({
    n <- num_removed_na_rows()
    warnings <- list()
    if (!is.null(n) && n > 0L) {
      warnings[[length(warnings) + 1]] <- datatabe_render_warning(
        "Removed rows from dataset",
        n_age,
        function(n_rows, rows_label) {
          paste0(
            "The dataset contained ",
            n_rows,
            " with all",
            " columns having missing values. These ",
            rows_label,
            " have been removed from the dataset."
          )
        }
      )
    }
    n_age <- number_age_set_to_na()
    if (!is.null(n_age) && n_age > 0L) {
      warnings[[length(warnings) + 1]] <- datatabe_render_warning(
        "Some age values were set to 'missing'",
        n_age,
        function(n_rows, rows_label) {
          paste0(
            "The dataset contained ",
            n_rows,
            " with",
            " negative age values. These ",
            rows_label,
            " have been set to missing for all further computations."
          )
        }
      )
    }
    warnings
  })
}

datatabe_render_warning <- function(heading, n, warning_fun) {
  rows_label <- if (n == 1L) "row" else "rows"
  n_rows <- paste0(n, " ", rows_label)
  column(
    12,
    div(
      class = "alert alert-warning",
      h4(class = "alert-heading", heading),
      p(warning_fun(n_rows, rows_label))
    )
  )
}
