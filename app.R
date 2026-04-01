suppressPackageStartupMessages({
  library(shiny, quietly = TRUE)
  library(shinyjs, quietly = TRUE)
  library(tidyr, quietly = TRUE)
  library(magrittr, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(forcats, quietly = TRUE)
  library(lazyeval, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(purrr, quietly = TRUE)
  library(ggplot2, quietly = TRUE)
  library(bslib, quietly = TRUE)
  library(promises, quietly = TRUE)
  library(officer, quietly = TRUE)
})

# config
source("config.R")

# source the auxiliary functions
source("code/functions-helper.R")
source("code/functions-prev.R")
source("code/data-assumptions.R")
source("code/input-variable-mapping.R")
source("code/plots.R")

# source the core functions
source("code/macro-z.R")
source("code/macro-prev.R")

# module support libraries
source("code/missing-data-lib.R")
source("code/distributions-lib.R")
source("code/zscore-dq-lib.R")
source("code/zscore-lib.R")
source("code/digit-lib.R")
source("code/data-quality-lib.R")
source("code/report-lib.R")

# source modules

source("modules/setup.R")
source("modules/upload.R")
source("modules/welcome.R")
source("modules/validator.R")
source("modules/mapping-selector.R")

## sidebar
source("modules/variables.R")
source("modules/variables-strat.R")

## main page
source("modules/datatable.R")
source("modules/zscore.R")
source("modules/prevalence.R")
source("modules/zscore-data-quality.R")
source("modules/data-quality.R")
source("modules/report.R")

## graphics
source("modules/plot-dist.R")
source("modules/plot-miss.R")
source("modules/plot-digit.R")

## ui --------------------------------------------------------------------------
source("modules/layout.R")

ui <- anthro_UI()

## Variable mapping
df_map_vars <- anthro_input_variable_mapping()

## server ----------------------------------------------------------------------
server <- function(input, output, session) {
  # initial setup

  data_uploaded <- callModule(Upload, "upload")
  upload_processor <- ExtendedTask$new(function(
    file_name,
    file_path,
    df_map_vars,
    config
  ) {
    promise(function(resolve, reject) {
      resolve(read_user_csv(file_name, file_path, df_map_vars, config))
    })
  })

  # data setup
  observe({
    inFile <- data_uploaded()
    req(!is.null(inFile))
    upload_processor$invoke(inFile$name, inFile$datapath, df_map_vars, config)
  })

  data_validator <- callModule(
    validatorModule,
    "data_input_validation",
    reactive({
      res <- upload_processor$result()
      req(!is.null(res))
      validation_result <- validation_ok()
      if ("upload_error" %in% class(res)) {
        validation_result <- validation_error(res$error)
      }
      if ("try-error" %in% class(res)) {
        validation_result <- validation_error(
          "Something went wrong reading the CSV. Please contact the administrator if the error persists."
        )
      }
      validation_result
    }),
    heading = "Upload failed, please see the error below for more information"
  )

  # upload data and return dataframes
  df_read <- reactive({
    res <- upload_processor$result()
    req(
      is.list(res),
      all(
        c("result", "file_name", "concept_mapping", "file_name_sans_ext") %in%
          names(res)
      )
    )
    list(
      result = res$result,
      file_name = res$file_name,
      concept_mapping = res$concept_mapping,
      file_name_sans_ext = res$file_name_sans_ext
    )
  })

  original_filename <- reactive({
    df_read()[["file_name_sans_ext"]]
  })

  # this is the original data frame
  df_raw <- reactive({
    req(is.list(df_read()))
    df <- df_read()[["result"]]
    req(is.data.frame(df))
    req(nrow(df) > 1L)
    req(ncol(df) > 1L)
    df
  })

  num_removed_na_rows <- reactive({
    req(is.list(df_read()))
    df_read()[["num_removed_na_rows"]]
  })

  observeEvent(df_read(), {
    showNotification(
      "This application may display sensitive information based on your input. Please consider this when sharing data via screenshots or downloads.",
      duration = NULL,
      closeButton = TRUE,
      type = "message"
    )
  })

  ## Add age group when age variable gets mapped
  age_in_months <- reactive({
    if (
      !is.null(input$ui_setup_compute_age) ||
        is.null(input$compute_age)
    ) {
      FALSE
    } else {
      input$id_age_unit && !isTRUE(input$compute_age)
    }
  })

  compute_age_by_dates <- reactive({
    !is.null(input$compute_age) && isTRUE(input$compute_age)
  })

  # this dataset is the raw dataset plus an age_group column
  df_age_group_base <- reactive({
    req(is.data.frame(df_raw()))
    req(nrow(df_raw()) >= 1L)
    req(length(list_map_vars) >= 1L)
    req(
      !compute_age_by_dates() ||
        length(list_map_vars[["date_birth"]]()) == 1L
    )
    req(
      !compute_age_by_dates() ||
        length(list_map_vars[["date_obs"]]()) == 1L
    )
    req(
      compute_age_by_dates() ||
        length(list_map_vars[["age"]]()) == 1L
    )

    # either age can be computed by date of birth/obs or by a column.
    # but never both
    dates_given <- list_map_vars[["date_birth"]]() != "None" &&
      list_map_vars[["date_obs"]]() != "None" &&
      compute_age_by_dates()
    age_given <-
      list_map_vars[["age"]]() != "None" && !compute_age_by_dates()
    month_scaling_constant <- 30.4375
    col_names <- colnames(df_raw())
    req(
      is.character(input$date_format) &&
        length(input$date_format) == 1L
    )
    d_parse <- function(x) parse_date(x, format = input$date_format)
    tmp <- if (dates_given) {
      req(list_map_vars[["date_obs"]]() %in% col_names)
      req(list_map_vars[["date_birth"]]() %in% col_names)
      age_in_days <- as.integer(
        d_parse(df_raw()[[list_map_vars[["date_obs"]]()]]) -
          d_parse(df_raw()[[list_map_vars[["date_birth"]]()]])
      )
      mutate(
        df_raw(),
        age_in_days = as.integer(age_in_days),
        age_in_months = age_in_days / month_scaling_constant,
        age_group = anthro_age_groups(age_in_days / month_scaling_constant)
      )
    } else if (age_given) {
      req(list_map_vars[["age"]]() %in% col_names)
      age_values <- as.numeric(df_raw()[[list_map_vars[["age"]]()]])
      if (age_in_months()) {
        mutate(
          df_raw(),
          age_in_days = as.integer(round_up(
            age_values * month_scaling_constant
          )),
          age_in_months = age_values,
          age_group = anthro_age_groups(age_in_months)
        )
      } else {
        mutate(
          df_raw(),
          age_in_days = as.integer(age_values),
          age_in_months = age_values / month_scaling_constant,
          age_group = anthro_age_groups(age_in_months)
        )
      }
    } else {
      df_raw()
    }

    for (col in c(
      list_map_vars[["date_obs"]](),
      list_map_vars[["date_birth"]]()
    )) {
      if (!col %in% "None" && col %in% col_names) {
        tmp[[col]] <- d_parse(tmp[[col]])
      }
    }

    for (col in c(
      list_map_vars[["age"]](),
      list_map_vars[["weight"]](),
      list_map_vars[["lenhei"]](),
      list_map_vars[["sw"]](),
      list_map_vars[["cluster"]](),
      list_map_vars[["strata"]]()
    )) {
      if (!col %in% "None" && col %in% col_names) {
        tmp[[col]] <- as.numeric(tmp[[col]])
      }
    }
    mutate(tmp, uid = row_number())
  })

  df_age_group <- reactive({
    req(is.data.frame(df_age_group_base()))

    df <- df_age_group_base()

    # negative ages are set to NA
    if ("age_in_days" %in% colnames(df)) {
      df[["age_in_days"]] <- dplyr::if_else(
        df[["age_in_days"]] < 0,
        NA_real_,
        df[["age_in_days"]]
      )
      df[["age_in_months"]] <- dplyr::if_else(
        df[["age_in_months"]] < 0,
        NA_real_,
        df[["age_in_months"]]
      )
    }

    # here we exclude all children with age >= 60 months
    if ("age_in_months" %in% colnames(df)) {
      dplyr::filter(
        df,
        is.na(age_in_months) | age_in_months < 60
      )
    } else {
      df
    }
  })

  number_removed_due_to_age <- reactive({
    req(is.data.frame(df_age_group_base()))
    req(is.data.frame(df_age_group()))

    nrow(df_age_group_base()) - nrow(df_age_group())
  })

  number_age_set_to_na <- reactive({
    req(is.data.frame(df_age_group_base()))
    req(is.data.frame(df_age_group()))
    req("age_in_days" %in% colnames(df_age_group()))
    req("age_in_days" %in% colnames(df_age_group_base()))
    age_na_orig <- sum(is.na(df_age_group_base()[["age_in_days"]]))
    age_na_final <- sum(is.na(df_age_group()[["age_in_days"]]))
    age_na_final - age_na_orig
  })

  # the last dataset is the dataset where filters might have been applied to
  in_memory_database <- reactiveValues()

  # set the main data frame to an intial values
  observe({
    req(is.data.frame(df_age_group()))
    in_memory_database[["df_filtered"]] <- df_age_group()
  })

  ####################################
  ##### Code for dynamic filters #####
  ####################################

  ## Call main filter module
  ## only have filters with less than 100 unique values
  filter_choices <- reactive({
    df <- df_raw()
    purrr::keep(
      colnames(df),
      ~ dplyr::between(dplyr::n_distinct(df[[.x]]), 2L, 30L)
    )
  })

  filter_variables <-
    callModule(
      Variables,
      "id_variables_filter",
      label = "Filter variables",
      choices = filter_choices,
      selected = "",
      multiple = TRUE
    )

  # this list of reactive values does the bookkeeping of input values
  in_memory_database[["selected_filter_values"]] <- list()

  observeEvent(df_read(), {
    in_memory_database[["selected_filter_values"]] <- list()
  })

  # this renders the global filters box
  output$ui_filter <- renderUI({
    # first we read the filters from the filter select box
    filters <- filter_variables()

    # in this slot we store the current selected value for each filter
    # this needs to happen as we have to redraw the UI elements whenever the filters are changed
    in_memory_database[["selected_filter_values"]] <-
      isolate(in_memory_database[["selected_filter_values"]][filters])

    # this stores the current filters; this is used in get_current_dataset() and triggers a reload
    in_memory_database[["current_filters"]] <- filters

    lapply(filters, function(name) {
      req(name %in% colnames(df_raw()))
      control_name <- paste0("filter_", name)
      row_values <- df_age_group()[[name]]
      selected_value <-
        isolate(in_memory_database[["selected_filter_values"]][[name]])
      choices <- sort(unique(as.character(df_age_group()[[name]])))
      selectInput(
        control_name,
        paste0("Values: ", name),
        choices,
        multiple = TRUE,
        selected = selected_value
      )
    })
  })

  # Whenever filter is selected, this redraws the select boxes, keeping previous entries in place
  observe({
    # one observer per filter element
    filters <- filter_variables()

    # for each of the previous filters we had an observer
    # we first need to make sure these observers are being destroyed before creating new ones
    if (!is.null(isolate(in_memory_database[["filters_update_observers"]]))) {
      isolate(in_memory_database[["filters_update_observers"]]) %>%
        lapply(function(x) {
          x$destroy()
        })
    }

    # for each filter we create a new observer that takes care of the selected value bookkeeping
    in_memory_database[["filters_update_observers"]] <-
      filters %>%
      lapply(function(f) {
        element_name <- paste0("filter_", f)
        observe({
          in_memory_database[["selected_filter_values"]][[f]] <-
            input[[element_name]]
          in_memory_database[["filters_value_changed"]] <-
            Sys.time()
        })
      })
  })

  # fires if the button was clicked or a new age group is defined
  observeEvent(
    {
      input$apply_filter
      df_age_group()
    },
    {
      res <- if (is.null(in_memory_database[["current_filters"]])) {
        df_age_group()
      } else {
        Reduce(
          f = function(dataset, filter_name) {
            req(is.character(filter_name), filter_name %in% colnames(df_raw()))
            filter_vals <- input[[paste0("filter_", filter_name)]]
            if (
              length(filter_vals) == 0L ||
                is.null(filter_vals) ||
                any(is.na(filter_vals)) ||
                all(filter_vals == "")
            ) {
              return(dataset)
            }
            dataset[dataset[[filter_name]] %in% filter_vals, ]
          },
          init = df_age_group(),
          x = in_memory_database[["current_filters"]]
        )
      }
      in_memory_database[["df_filtered"]] <- res
    }
  )

  output$panel_side <- renderUI({
    req(is.list(df_read()))
    req(is.data.frame(df_raw()))
    req(ncol(df_raw()) > 1L)
    req(nrow(df_raw()) >= 1L)
    ui_setup()[["side"]]
  })

  output$panel_main <- renderUI({
    if (is.null(data_uploaded())) {
      callModule(Welcome, "tmp")
    } else {
      ui_setup()[["main"]]
    }
  })

  # event for the front page
  observeEvent(input$gotoanthro, {
    updateTabsetPanel(session, "mainNavBar", selected = "analyser")
  })

  # this is a list of reactive values having the column mapping
  list_map_vars <- lapply(seq_len(nrow(df_map_vars)), function(i) {
    module_ns <- paste0("id_variables_", df_map_vars$vars[i])
    validator_explanation <- df_map_vars$validator_explanations[[i]]
    choices <- reactive({
      req(is.list(df_read()), !is.null(df_read()[["concept_mapping"]]))
      concept_mapping <- df_read()[["concept_mapping"]]
      req(concept_mapping[[i]]$variable == df_map_vars$vars[i]) # just in case
      concept_mapping[[i]]$choices
    })
    popover_title <- reactive({
      if (length(choices()) == 1L && choices() == "None") {
        "No variables with correct format in dataset"
      }
    })
    popover_content <- reactive({
      if (length(choices()) == 1L && choices() == "None") {
        paste0(
          "We could not identify any columns in your dataset that match the formatting criteria for this variable. ",
          validator_explanation
        )
      }
    })
    callModule(
      Variables,
      module_ns,
      choices = choices,
      label = df_map_vars$labs[i],
      selected = "None",
      popover_title = popover_title,
      popover_content = popover_content
    )
  }) |>
    set_names(df_map_vars$vars)

  # This global validator ensures that no variable is mapped to multiple
  # variables.
  global_validation <- callModule(
    validatorModule,
    "global_validation",
    reactive({
      in_memory_database[["df_filtered"]]
      vals <- Map(function(x) list_map_vars[[x]](), names(list_map_vars))
      mapped_vals <- Filter(function(x) !is.null(x) && x != "None", vals)
      mapping_map <- Reduce(
        function(acc, concept) {
          col <- mapped_vals[[concept]]
          acc[[col]] <- c(concept, acc[[col]])
          acc
        },
        names(mapped_vals),
        list()
      )
      validation_result <- validation_ok()
      for (column in names(mapping_map)) {
        variables <- mapping_map[[column]]
        if (length(variables) <= 1) {
          next
        }
        variable_labels <- df_map_vars[
          df_map_vars$vars %in% variables,
          "short_label",
          drop = TRUE
        ]
        validation_result <- validation_error(
          paste0(
            "Column '",
            column,
            "' was mapped to multiple variables (",
            paste0(variable_labels, collapse = ", "),
            "). Make sure each column only maps to one variable."
          )
        )
        break
      }
      validation_result
    }),
    heading = "Input error"
  )

  df_filtered <- reactive({
    df <- in_memory_database[["df_filtered"]]
    req(is.data.frame(df))
    req(global_validation())
    df
  })

  matched_vars <- reactive({
    plyr::ldply(
      df_map_vars$vars,
      function(i) {
        list_map_vars[[i]]()
      }
    ) %>%
      filter(V1 != "None") %>%
      extract2("V1")
  })

  ## Variable mapping for stratification
  matched_vars_strat <- reactive({
    plyr::ldply(
      c(
        "sex",
        "typeres",
        "gregion",
        "wealthq",
        "mothered",
        "team",
        "othergr"
      ),
      function(i) {
        list_map_vars[[i]]()
      }
    ) %>%
      filter(V1 != "None") %>%
      extract2("V1")
  })

  df_matched_vars_strat <- reactive({
    tibble(
      key = c(
        "Age group (months)",
        "Sex",
        "Residence type",
        "Geographical region",
        "Wealth quintile",
        "Mother education",
        "Team",
        "Other grouping variable"
      ),
      value = c(
        "age_group",
        list_map_vars[["sex"]](),
        list_map_vars[["typeres"]](),
        list_map_vars[["gregion"]](),
        list_map_vars[["wealthq"]](),
        list_map_vars[["mothered"]](),
        list_map_vars[["team"]](),
        list_map_vars[["othergr"]]()
      )
    ) %>%
      filter(!value %in% "None")
  })

  # Many components need zscores so we compute them once here
  zscore_computer <- ExtendedTask$new(function(
    data,
    sex_col,
    weight_col,
    lenhei_col,
    lenhei_unit_col,
    oedema_col
  ) {
    promise(function(resolve, reject) {
      resolve(
        CalculateZScores(
          data = data,
          sex = sex_col,
          weight = weight_col,
          lenhei = lenhei_col,
          lenhei_unit = lenhei_unit_col,
          oedema = oedema_col
        )
      )
    })
  })

  validate_zscore <- reactive_zscore_validation(
    df_filtered,
    list_map_vars,
    height_and_weight = TRUE
  )

  observe({
    req(is_valid(validate_zscore()))
    zscore_computer$invoke(
      df_filtered(),
      list_map_vars[['sex']](),
      list_map_vars[['weight']](),
      list_map_vars[['lenhei']](),
      list_map_vars[['lenhei_unit']](),
      list_map_vars[['oedema']]()
    )
  })

  df_zscores <- reactive({
    result <- zscore_computer$result()
    req(is.data.frame(result))
    result
  })

  # ui setup
  ui_setup <- callModule(
    SetUp,
    "ui_setup",
    tabsetPanel(
      DFTableOutput("DFTable"),
      zscoreOutput("zscore"),
      PrevalenceOutput("Prevalence"),
      DataQualityModuleOutput("DataQuality"),
      ReportOutput("id_report")
    ),
    original_filename
  )

  # ui Modules that depend on other reactive elements
  callModule(
    DFTable,
    "DFTable",
    dataset = df_filtered,
    num_removed_na_rows = num_removed_na_rows,
    number_age_set_to_na = number_age_set_to_na
  )
  callModule(
    zscore,
    "zscore",
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    dataset = df_filtered,
    df_matched_vars_strat,
    df_age_group = df_age_group_base,
    original_filename = original_filename,
    df_zscores = df_zscores
  )
  callModule(
    Prevalence,
    "Prevalence",
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    dataset = df_filtered,
    original_filename = original_filename
  )
  callModule(
    DataQualityModule,
    "DataQuality",
    df_matched_vars_strat = df_matched_vars_strat,
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    df_filtered = df_filtered,
    original_filename = original_filename,
    df_zscores = df_zscores
  )
  callModule(
    Report,
    "id_report",
    df_raw = df_raw,
    list_map_vars = list_map_vars,
    age_in_months = age_in_months,
    df_filtered = df_filtered,
    original_filename = original_filename,
    number_removed_due_to_age = number_removed_due_to_age
  )

  showModal(modalDialog(
    title = "Disclaimer",
    HTML(
      "<p>The Anthro Survey Analyser is a client-side only tool. All data is handled in your browser and never leaves your computer.</p>
<p>All reasonable precautions have been taken by WHO to verify the calculations performed by this application. However, the application is being distributed without warranty of any kind, either express or implied. The responsibility for the use and interpretation of the application’s output lies with the user. In no event shall the World Health Organization be liable for damages arising from its use.</p>"
    ),
    footer = tagList(
      modalButton("Accept", icon = icon("check")),
      extendShinyjs(
        text = "shinyjs.closeWindow = function() { window.close(); }",
        functions = c("closeWindow")
      ),
      actionButton(
        "close",
        "Cancel",
        icon = icon("remove"),
        style = "color: #ED4337;"
      )
    )
  ))
}

shinyApp(ui, server)
