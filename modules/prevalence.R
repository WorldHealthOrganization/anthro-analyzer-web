PrevalenceOutput <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Prevalence",
    icon = icon("bar-chart"),

    tags$style(
      type = "text/css",
      "#dl_prevalence {background-color:LightGrey; float:right; margin-bottom: 15px;}"
    ),
    validatorOutput(ns("validation")),
    uiOutput(ns("warnings")),
    uiOutput(ns("nested_clusters")),
    fluidRow(
      column(
        12,
        shinyjs::disabled(
          input_task_button(
            ns("button_prev"),
            "Click to calculate prevalence estimates"
          )
        ),

        shinyjs::disabled(
          downloadButton(
            ns("dl_prevalence"),
            label = "Download prevalence estimates"
          )
        )
      )
    ),
    fluidRow(column(12, DT::dataTableOutput(ns("prev"))))
  )
}

Prevalence <- function(
  input,
  output,
  session,
  list_map_vars,
  age_in_months,
  dataset,
  original_filename
) {
  ns <- session$ns

  validation_checks <- callModule(
    validatorModule,
    "validation",
    anthro_create_prevalence_validation(list_map_vars, dataset)
  )

  clusters_not_nested <- reactive({
    are_there_non_nested_clusters(dataset, list_map_vars)
  })

  cluster_nesting_ok <- reactive({
    # if clusters are not nested the user needs to force nesting
    !clusters_not_nested() || isTRUE(input$force_nesting)
  })

  validation <- reactive({
    isTRUE(validation_checks()) && cluster_nesting_ok()
  })

  output$nested_clusters <- renderUI({
    nested_clusters <- !clusters_not_nested()
    if (isFALSE(validation_checks()) || nested_clusters) {
      return(list())
    }
    list(
      br(),
      div(
        class = "alert alert-warning",
        h4("Cluster ids not nested within strata", class = "alert-heading"),
        list(
          p(paste0(
            "Note that the clusters are not currently nested within strata; that is, cluster numbers overlap across strata.",
            " If that should not be the case, you can fix the incorrect labels in your dataset and re-upload it. ",
            "Otherwise, just tick \"Continue\" and the tool will relabel the clusters to enforce nesting within strata and calculate prevalence estimates."
          )),
          shiny::checkboxInput(
            ns("force_nesting"),
            label = "Continue",
            width = "100%"
          )
        )
      )
    )
  })

  output$warnings <- renderUI({
    req(validation())

    missing_vals <- anthro_prev_excluded_cases(dataset(), list_map_vars)
    missing_values_names <- missing_vals$missing_values_names
    missing_value_counts <- missing_vals$missing_value_counts

    if (length(missing_values_names) > 0L) {
      error_div <- div(
        class = "alert alert-warning",
        h4(
          "The following columns include missing values",
          class = "alert-heading"
        ),
        tags$ul(map2(
          missing_values_names,
          missing_value_counts,
          function(field, count) {
            tags$li(
              "'",
              field,
              "' with ",
              count,
              " missing ",
              if (count == 1) "case" else "cases"
            )
          }
        )),
        hr(),
        p(
          "All rows with missing values in these columns will be excluded from the analysis!"
        )
      )
      fluidRow(column(12, error_div))
    } else {
      HTML("")
    }
  })

  validated_dataset <- reactive({
    req(validation())
    dataset()
  })

  output$nrows <- renderText(nrow(validated_dataset()))

  observe({
    if (validation()) {
      shinyjs::enable("button_prev")
    } else {
      shinyjs::disable("button_prev")
    }
  })

  prevalence_results <- ExtendedTask$new(function(
    data,
    sex = NULL,
    weight = NULL,
    lenhei = NULL,
    lenhei_unit = NULL,
    age = NULL,
    age.month = FALSE,
    date_obs = NULL,
    date_birth = NULL,
    typeres = NULL,
    wealthq = NULL,
    gregion = NULL,
    mothered = NULL,
    othergr = NULL,
    sw = NULL,
    cluster = NULL,
    strata = NULL,
    oedema = NULL
  ) {
    promise(function(resolve, reject) {
      result <- try(
        CalculatePrev(
          data = data,
          age.month = age.month,
          age = age,
          date_birth = date_birth,
          date_obs = date_obs,
          sex = sex,
          weight = weight,
          lenhei = lenhei,
          lenhei_unit = lenhei_unit,
          sw = sw,
          cluster = cluster,
          strata = strata,
          typeres = typeres,
          gregion = gregion,
          wealthq = wealthq,
          mothered = mothered,
          othergr = othergr,
          oedema = oedema
        )
      )
      if (inherits(result, "try-error")) {
        reject(result)
      } else {
        resolve(result)
      }
    })
  }) |>
    bind_task_button("button_prev")

  # Run prevalence calculations
  ## Only run calculations once button explicitly pressed
  observeEvent(input$button_prev, {
    req(validation())
    prevalence_results$invoke(
      data = validated_dataset(),
      sex = list_map_vars[["sex"]](),
      weight = list_map_vars[["weight"]](),
      lenhei = list_map_vars[["lenhei"]](),
      lenhei_unit = list_map_vars[["lenhei_unit"]](),
      oedema = list_map_vars[["oedema"]](),
      age.month = age_in_months(),
      age = list_map_vars[["age"]](),
      date_birth = list_map_vars[["date_birth"]](),
      date_obs = list_map_vars[["date_obs"]](),
      sw = list_map_vars[["sw"]](),
      cluster = list_map_vars[["cluster"]](),
      strata = list_map_vars[["strata"]](),
      typeres = list_map_vars[["typeres"]](),
      gregion = list_map_vars[["gregion"]](),
      wealthq = list_map_vars[["wealthq"]](),
      mothered = list_map_vars[["mothered"]](),
      othergr = list_map_vars[["othergr"]]()
    )
  })

  output$prev <- DT::renderDataTable(
    {
      req(validation(), prevalence_results$status() == "success")
      df_prevs <- prevalence_results$result()
      shiny::validate(
        need(
          !inherits(df_prevs, "try-error"),
          "Something unexpected went wrong during the computation. Please contact the administrator of this application."
        )
      )
      req(is.data.frame(df_prevs))
      data <- dplyr::mutate_at(
        df_prevs,
        dplyr::vars(dplyr::ends_with("_se")),
        dplyr::funs(round(., digits = 4))
      )
      for (col in colnames(data)) {
        is_unweighted_pop <- grepl(x = col, pattern = "\\_unwpop$")
        is_weighted_pop <- grepl(x = col, pattern = "\\_pop$")
        is_not_se <- !grepl(x = col, pattern = "\\_se$")
        if (is_unweighted_pop) {
          data[[col]] <- as.integer(data[[col]])
        } else if (is_weighted_pop) {
          data[[col]] <- round(as.numeric(data[[col]]), digits = 1)
        } else if (is_not_se && is.numeric(data[[col]])) {
          data[[col]] <- round(data[[col]], digits = 2)
        }
      }
      data
    },
    options = list(paging = FALSE, digits = 2),
    escape = TRUE
  )

  # Download table
  ## Only enable download button once z-score calculation button has been clicked
  observeEvent(prevalence_results$status(), {
    req(validation(), prevalence_results$status() == "success")
    shinyjs::enable("dl_prevalence")
  })

  output$dl_prevalence <- downloadHandler(
    filename = function() {
      req(validation())
      make_export_filename(original_filename(), "prevalence.xlsx")
    },
    content = function(file) {
      req(validation(), prevalence_results$status() == "success")
      data <- prevalence_results$result()
      req(is.data.frame(data))
      writexl::write_xlsx(
        list(
          `Data` = prevalence_results$result(),
          `Variable Dictionary` = prevalence_variable_dict(),
          `Variable mapping` = generate_variable_mapping(list_map_vars)
        ),
        file
      )
    }
  )
}

prevalence_variable_dict <- function() {
  prevalence_var_dict <- tibble::tribble(
    ~Index  , ~d1                                                                                       , ~`Cut-Offs` , ~d2                                  , ~Suffix   , ~d3                                   ,
    "HA"    , "Height-for-age"                                                                          , "_3"        , "Prevalence corresponding to < -3SD" , "_pop"    , "Weighted sample size"                ,
    "WA"    , "Weight-for-age"                                                                          , "_2"        , "Prevalence corresponding to < -2SD" , "_unwpop" , "Unweighted sample size"              ,
    "BMI"   , "Body-mass-index-for-age"                                                                 , "_1"        , "Prevalence corresponding to < -1SD" , "_r"      , "Mean/prevalence"                     ,
    "WH"    , "Weight-for-height"                                                                       , "1"         , "Prevalence corresponding to > +1SD" , "_ll"     , "95% confidence interval lower limit" ,
    "HA_WH" , "Combined indicator based on height-for-age and weight-for-height (stunted & overweight)" , "2"         , "Prevalence corresponding to > +2SD" , "_ul"     , "95% confidence interval upper limit" ,
    ""      , ""                                                                                        , "3"         , "Prevalence corresponding to > +3SD" , "_stdev"  , "Standard Deviation"                  ,
    ""      , ""                                                                                        , ""          , ""                                   , "_se"     , "Standard Error"
  )
  colnames(prevalence_var_dict) <- c(
    "Index",
    "",
    "Cut-Offs",
    "",
    "Suffix",
    ""
  )
  prevalence_var_dict
}

generate_variable_mapping <- function(list_map_vars) {
  variable_keys <- names(list_map_vars)
  variable_values <- lapply(variable_keys, function(x) list_map_vars[[x]]())
  rm <- vapply(variable_values, is.null, logical(1L))
  variable_values <- unlist(variable_values[!rm])
  variable_keys <- variable_keys[!rm]
  mapped_variables <- data.frame(
    vars = variable_keys,
    `Column` = variable_values
  )
  var_info <- anthro_input_variable_mapping()
  select(var_info, vars, labs) %>%
    inner_join(mapped_variables, by = "vars") %>%
    rename(`Variable` = labs) %>%
    select(-vars)
}
