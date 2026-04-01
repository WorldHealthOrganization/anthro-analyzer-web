ReportOutput <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Summary Report",
    icon = icon("book"),
    fluidRow(column(12, validatorOutput(ns("validation")))),
    p(
      "This function allows to export a template summary report in Word laying out guidance on minimum required details to follow good practice in reporting. The report also includes main findings (graphics and tables) regarding prevalence estimates by different disaggregation factors for the five main indicators, namely stunting, wasting, severe wasting, overweight and underweight, as well as some data quality assessment statistics."
    ),
    fluidRow(
      column(
        12,
        shinyjs::disabled(
          input_task_button(
            ns("generate_report"),
            "Generate report"
          )
        ),

        shinyjs::disabled(
          downloadButton(
            ns("dl_report"),
            label = "Download report"
          )
        )
      )
    ),
    p(
      "* Note: due to technical reasons 'Download report' opens a new browser window which can be closed once the report is downloaded."
    )
  )
}

Report <- function(
  input,
  output,
  session,
  df_filtered,
  age_in_months,
  list_map_vars,
  df_raw,
  original_filename,
  number_removed_due_to_age
) {
  # input validation
  stopifnot(is.reactive(df_filtered))
  stopifnot(is.reactive(age_in_months))
  stopifnot(is.list(list_map_vars))
  stopifnot(is.reactive(df_raw))
  stopifnot(is.reactive(original_filename))
  stopifnot(is.reactive(number_removed_due_to_age))

  validation <- callModule(
    validatorModule,
    "validation",
    anthro_create_prevalence_validation(list_map_vars, df_filtered)
  )

  observe({
    preconditions_ok <- validation()
    if (preconditions_ok) {
      shinyjs::enable("generate_report")
    } else {
      shinyjs::disable("generate_report")
    }
    shinyjs::toggle("precondition-explanation", condition = !preconditions_ok)
  })

  observeEvent(report_results$status(), {
    req(validation(), report_results$status() == "success")
    shinyjs::enable("dl_report")
  })

  report_results <- ExtendedTask$new(function(
    variables
  ) {
    promise(function(resolve, reject) {
      resolve(suppressWarnings(generate_report(variables)))
    })
  }) |>
    bind_task_button("generate_report")

  # Run prevalence calculations
  ## Only run calculations once button explicitly pressed
  observeEvent(input$generate_report, {
    req(validation())
    df_filtered_val <- df_filtered()
    marerialized_list_map_vars <- materialize_list(list_map_vars)
    variables <- list(
      df_filtered = materialize(df_filtered),
      age_in_months = materialize(age_in_months),
      list_map_vars = marerialized_list_map_vars,
      df_raw = materialize(df_raw),
      zscore_flag_cols = zscore_flag_cols,
      number_removed_due_to_age = materialize(number_removed_due_to_age),
      dist_age_by_month = function() {
        dist_plot(
          plot_data = dist_prepare_plot_data(df_filtered_val),
          x_variable = "plot_age_in_months",
          x_variable_label = "Age in months",
          is_x_discrete = TRUE
        )
      },
      dist_age_group_by_sex = function() {
        dist_plot(
          plot_data = dist_prepare_plot_data(df_filtered_val),
          x_variable = "age_group",
          x_variable_label = "Standard age group",
          is_x_discrete = TRUE,
          stratification_variable = marerialized_list_map_vars[["sex"]](),
          stratification_variable_label = "Sex"
        )
      },
      dist_age_year_by_sex = function() {
        dist_plot(
          plot_data = dist_prepare_plot_data(df_filtered_val),
          x_variable = "plot_age_in_years",
          x_variable_label = "Age in years",
          is_x_discrete = TRUE,
          stratification_variable = marerialized_list_map_vars[["sex"]](),
          stratification_variable_label = "Sex"
        )
      }
    )
    report_results$invoke(variables)
  })

  output$dl_report <-
    downloadHandler(
      filename = function() {
        make_export_filename(original_filename(), "report.docx")
      },

      content = function(file) {
        req(validation(), report_results$status() == "success")
        data <- report_results$result()
        req(inherits(data, "rdocx"))
        print(data, target = file)
      }
    )
}

random_temp_file_path <- function() {
  file.path(
    tempdir(),
    paste0(openssl::sha256(openssl::rand_bytes(32)), collapse = "")
  )
}
