DataQualityModuleOutput <- function(id) {
  ns <- NS(id)
  tabset_call <- rlang::quo({
    tabsetPanel(
      PlotDistOutput(ns("PlotDist")),
      PlotMissOutput(ns("PlotMiss")),
      PlotDigitOutput(ns("PlotDigit")),
      !!!ZscoreDataQualityModuleOutput(ns("ZscoreDataQuality")),
      tabPanel(
        "Data Quality Report",
        icon = icon("book"),
        fluidRow(column(12, validatorOutput(ns("validation_report")))),
        p(
          "This function allows the user to export a Data Quality Assessment report in Word format, compiling main summary statistics and visualisations to aid with Data Quality Assessment of the survey data. It includes key data quality checks that can help to identify issues with the data and considerations when interpreting results. Other outputs that can be relevant to your analyses can be saved directly from the tool’s interactive dashboards and added to the report."
        ),
        p(
          "For guidance on how to interpret the results, user should refer to the document “Recommendations for improving the quality of anthropometric data and its analysis and reporting” by the Working Group on Anthropometric Data Quality, for the WHO-UNICEF Technical Expert Advisory Group on Nutrition Monitoring (TEAM). The document is available at",
          a(
            "this WHO page",
            href = "https://www.who.int/publications/i/item/9789241515559 ",
            target = "_blank"
          ),
          "under \"Technical reports and papers\"."
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
    )
  })
  rlang::eval_tidy(
    rlang::quo(
      tabPanel(
        "Data Quality Assessment",
        icon = icon("sort-numeric-asc"),
        fluidRow(
          column(
            12,
            !!tabset_call
          )
        )
      )
    )
  )
}

DataQualityModule <- function(
  input,
  output,
  session,
  df_filtered,
  df_matched_vars_strat,
  list_map_vars,
  age_in_months,
  original_filename,
  df_zscores
) {
  callModule(
    PlotDist,
    "PlotDist",
    dataset = df_filtered,
    df_matched_vars_strat,
    list_map_vars,
    original_filename = original_filename
  )
  callModule(
    ZscoreDataQualityModule,
    "ZscoreDataQuality",
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    dataset = df_filtered,
    df_matched_vars_strat,
    original_filename = original_filename,
    df_zscores = df_zscores
  )
  callModule(
    PlotMiss,
    "PlotMiss",
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    dataset = df_filtered,
    original_filename = original_filename,
    df_zscores = df_zscores
  )
  callModule(
    PlotDigit,
    "PlotDigit",
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    dataset = df_filtered,
    original_filename = original_filename,
    df_zscores = df_zscores
  )

  validation_checks <- callModule(
    validatorModule,
    "validation_report",
    # we use the same validations as the prevalence module
    anthro_create_prevalence_validation(list_map_vars, df_filtered)
  )

  observe({
    if (validation_checks()) {
      shinyjs::enable("generate_report")
    } else {
      shinyjs::disable("generate_report")
    }
  })

  observeEvent(report_results$status(), {
    req(validation_checks(), report_results$status() == "success")
    shinyjs::enable("dl_report")
  })

  report_results <- ExtendedTask$new(function(
    df_filtered,
    df_zscores,
    list_map_vars
  ) {
    promise(function(resolve, reject) {
      report_vars <- generate_dq_report_variables(
        df_filtered,
        df_zscores,
        list_map_vars
      )
      resolve(
        suppressWarnings(
          generate_dq_report(report_vars)
        )
      )
    })
  }) |>
    bind_task_button("generate_report")

  observeEvent(input$generate_report, {
    req(validation_checks())
    report_results$invoke(
      materialize(df_filtered),
      materialize(df_zscores),
      materialize_list(list_map_vars)
    )
  })

  output$dl_report <-
    downloadHandler(
      filename = function() {
        make_export_filename(original_filename(), "dq-report.docx")
      },

      content = function(file) {
        req(validation_checks(), report_results$status() == "success")
        data <- report_results$result()
        req(inherits(data, "rdocx"))
        print(data, target = file)
      }
    )
}
