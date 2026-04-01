generate_dq_report_variables <- function(
  df_filtered,
  df_zscores,
  list_map_vars
) {
  df_filtered <- df_filtered()
  list_map_vars <- list_mapping_to_char(list_map_vars)
  n_children <- nrow(df_filtered)
  df_zscores <- df_zscores()
  is_geo_mapped <- is_variable_mapped(list_map_vars[["gregion"]])
  is_team_mapped <- is_variable_mapped(list_map_vars[["team"]])
  is_wealthq_mapped <- is_variable_mapped(list_map_vars[["wealthq"]])
  is_measure_mapped <- is_variable_mapped(list_map_vars[["lenhei_unit"]])
  number_lenhei_set_to_na <- sum(is.na(df_zscores$clenhei)) -
    sum(is.na(df_filtered[[list_map_vars[["lenhei"]]]]))
  missing_plot_data <- function(stratify_by = NULL) {
    missing_data_prepare_plot_data(
      dataset = df_filtered,
      df_zscores = df_zscores,
      stratification_variable = stratify_by,
      is_stratification_variable_selected = !is.null(stratify_by),
      othergr_col = list_map_vars[["othergr"]],
      sex_col = list_map_vars[["sex"]],
      typeres_col = list_map_vars[["typeres"]],
      gregion_col = list_map_vars[["gregion"]],
      wealthq_col = list_map_vars[["wealthq"]],
      mothered_col = list_map_vars[["mothered"]],
      weight_col = list_map_vars[["weight"]],
      lenhei_col = list_map_vars[["lenhei"]],
      lenhei_unit_col = list_map_vars[["lenhei_unit"]],
      oedema_col = list_map_vars[["oedema"]]
    )
  }
  digit_grouped_heaping_plot <- function(strat_column) {
    # this function takes all data and produces a plot for groups of three
    # elements in the strat_column.
    # For example: a dataset has 21 regions, then it creates 7 plots
    # with 3 regions each
    plot_data <- digit_prepare_heaping_plot_data(
      df_zscores,
      weight_col = list_map_vars[["weight"]],
      lenhei_col = list_map_vars[["lenhei"]],
      strat_column = strat_column
    )
    strata <- sort(unique(plot_data[["strat_var"]]))
    n_strata <- length(strata)
    subplots <- 3L # the number of rows per facet plot
    n_plots <- as.integer(n_strata / subplots)
    if (n_strata %% subplots > 0) {
      n_plots <- n_plots + 1L
    }
    if (n_plots < 1 || n_plots > 50) {
      # for safety
      return()
    }
    max_x_axis_length <- -1
    chunked_data <- lapply(1:n_plots, function(i) {
      strata_plot <- strata[seq_len(pmin(subplots, length(strata)))]
      strata <<- setdiff(strata, strata_plot)
      data <- dplyr::filter(plot_data, strat_var %in% strata_plot)
      if (nrow(data) > 0) {
        max_x_axis_length <<- max(max_x_axis_length, max(data$prop))
        data
      }
    })
    plots <- list()
    for (data in chunked_data) {
      if (is.null(data)) {
        next
      }
      plots[[length(plots) + 1]] <- digit_create_heaping_plot(
        data,
        TRUE,
        max_x_axis_length
      )
    }
    plots
  }
  list(
    total_number_of_children = function() {
      n_children
    },
    missing_data_plot = function() {
      missing_data_make_plot(missing_plot_data())
    },
    number_lenhei_set_to_na = number_lenhei_set_to_na,
    show_missing_data_by_geo = is_geo_mapped,
    show_missing_data_by_team = is_team_mapped,
    missing_data_table_by_geo = function() {
      label <- "Geographical region"
      missing_data_prepare_missings_table(
        missing_plot_data(
          stratify_by = list(label = label, column = list_map_vars[["gregion"]])
        ),
        label
      )
    },
    missing_data_table_by_team = function() {
      label <- "Team"
      missing_data_prepare_missings_table(
        missing_plot_data(
          stratify_by = list(label = label, column = list_map_vars[["team"]])
        ),
        label
      )
    },
    dist_age_group_by_sex = function() {
      dist_plot(
        plot_data = dist_prepare_plot_data(df_filtered),
        x_variable = "age_group",
        x_variable_label = "Standard age group",
        is_x_discrete = TRUE,
        stratification_variable = list_map_vars[["sex"]],
        stratification_variable_label = "Sex"
      )
    },
    dist_age_year_by_sex = function() {
      dist_plot(
        plot_data = dist_prepare_plot_data(df_filtered),
        x_variable = "plot_age_in_years",
        x_variable_label = "Age in years",
        is_x_discrete = TRUE,
        stratification_variable = list_map_vars[["sex"]],
        stratification_variable_label = "Sex"
      )
    },
    dist_age_by_month = function() {
      dist_plot(
        plot_data = dist_prepare_plot_data(df_filtered),
        x_variable = "plot_age_in_months",
        x_variable_label = "Age in months",
        is_x_discrete = TRUE
      )
    },
    show_mismatch_table = is_measure_mapped,
    mismatch_table = function() {
      dist_compute_mismatch_table(
        data = dist_prepare_plot_data(df_zscores),
        measure_col = as.symbol(list_map_vars[["lenhei_unit"]]),
        age_in_month_col = as.symbol("age_in_months")
      )
    },
    mismatch_n_children = function() {
      dist_mismatch_n_children(
        dist_prepare_plot_data(df_zscores),
        list_map_vars[["lenhei_unit"]]
      )
    },
    zscore_plot_flagged_zscores = function() {
      zscore_plot_flagged_zscores(df_zscores)
    },
    zscore_plot_dist = function() {
      data <- zscore_prepare_plot_dist_data(
        df_zscores = df_zscores,
        stratification_var_name = "None",
        list_map_vars = list_map_vars
      )
      suppressWarnings(zscore_plot_distribution(data))
    },
    zscore_plot_dist_by_sex = function() {
      data <- zscore_prepare_plot_dist_data(
        df_zscores = df_zscores,
        stratification_var_name = "Sex",
        list_map_vars = list_map_vars
      )
      suppressWarnings(zscore_plot_distribution(data))
    },
    zscore_plot_dist_by_age_group = function() {
      data <- zscore_prepare_plot_dist_data(
        df_zscores = df_zscores,
        stratification_var_name = "Age group (months)",
        list_map_vars = list_map_vars
      )
      suppressWarnings(zscore_plot_distribution(data))
    },
    zscore_summary_table = function() {
      data <- zscore_compute_summary_data(
        set_flagged_zscores_to_na(df_zscores),
        zscore_summary_groups(list_map_vars)
      )
      # for the report we split the data frame into two tables
      stopifnot(ncol(data) == 18)
      df1 <- data[, 1:10]
      df2 <- dplyr::bind_cols(data[, 1:2], data[, 11:18])
      list(
        df1 = df1,
        df2 = df2
      )
    },
    digit_plot_heaping = function() {
      plot_data <- digit_prepare_heaping_plot_data(
        df_zscores,
        weight_col = list_map_vars[["weight"]],
        lenhei_col = list_map_vars[["lenhei"]],
        strat_column = NULL
      )
      digit_create_heaping_plot(
        plot_data,
        is_stratification_variable_selected = FALSE
      )
    },
    show_digit_plot_heaping_by_team = is_team_mapped,
    digit_plot_heaping_by_team = function() {
      digit_grouped_heaping_plot(list_map_vars[["team"]])
    },
    show_digit_plot_heaping_by_region = is_geo_mapped,
    digit_plot_heaping_by_region = function() {
      digit_grouped_heaping_plot(list_map_vars[["gregion"]])
    },
    digit_plot_integer_for_weight = function() {
      plot_data <- digit_prepare_weight_plot_data(
        df_filtered,
        list_map_vars[["weight"]]
      )
      digit_weight_plot(plot_data)
    },
    digit_plot_integer_for_lenhei = function() {
      plot_data <- digit_prepare_height_plot_data(
        df_filtered,
        list_map_vars[["lenhei"]]
      )
      digit_height_plot(plot_data)
    }
  )
}

generate_dq_report <- function(variables) {
  zscore_table <- variables$zscore_summary_table()
  doc <- read_docx(path = "reports/dq-report-template.docx")
  word_size <- docx_dim(doc)
  # it's landscape, so we flip width/height
  plot_height <- (word_size$page["width"] -
    word_size$margins["left"] -
    word_size$margins["right"]) *
    0.85
  plot_width <- (word_size$page["height"] -
    word_size$margins["top"] -
    word_size$margins["bottom"])
  unordered_list <- function(doc, items) {
    # officer does not support lists yet. This is not great, but can be improved
    # in the future.
    for (item in items) {
      doc <- body_add(
        doc,
        do.call(
          fpar,
          c(
            list("\u2022 "),
            item
          )
        )
      )
    }
    doc
  }
  doc <- doc |>
    body_add_par("Table of Contents", style = "TOC Heading") |>
    body_add_toc(level = 2) |>
    body_add_par(value = "Recommended citation:") |>
    body_add_par(
      value = "Data quality assessment report template with results from WHO Anthro Survey Analyser"
    ) |>
    body_add_par(
      value = paste0(
        "Analysis date: ",
        format(Sys.time(), format = "%Y-%m-%d %H:%M %Z")
      )
    ) |>
    body_add_par(
      value = "This report is a template that includes key data quality checks that can help to identify issues with the data and considerations when interpreting results. Other outputs that can be relevant to your analyses can be saved directly from the tool interactive dashboards and added to the report."
    ) |>
    body_add(
      fpar(
        "For guidance on how to interpret the results, user should refer to the document “Recommendations for improving the quality of anthropometric data and its analysis and reporting” by the Working Group on Anthropometric Data Quality, for the WHO-UNICEF Technical Expert Advisory Group on Nutrition Monitoring (TEAM).",
        " The document is available at ",
        hyperlink_ftext(
          text = "this WHO page",
          href = "https://www.who.int/publications/i/item/9789241515559",
          prop = fp_text(underlined = TRUE)
        ),
        "."
      )
    ) |>
    body_add_par(value = "Missing data", style = "heading 1") |>
    body_add_par(
      value = "Percentage (number of cases) of children missing information on variables used in the analysis",
      style = "heading 2"
    ) |>
    body_add_par(
      value = paste0(
        "Total number of children: ",
        variables$total_number_of_children()
      )
    ) |>
    body_add_gg(
      value = variables$missing_data_plot(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add(
      fpar(
        paste0(
          "Note: In addition to those missing values, ",
          variables$number_lenhei_set_to_na
        ),
        " lengths/heights were set to missing, because they were outside the plausibility intervals (",
        "see ",
        hyperlink_ftext(
          text = "Quick Guide",
          href = "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/software/anthro-survey-analyser-quickguide.pdf?sfvrsn=dc7ddc6f_6",
          prop = fp_text(underlined = TRUE)
        ),
        ")"
      )
    )
  if (variables$show_missing_data_by_geo) {
    tbl <- variables$missing_data_table_by_geo()
    doc <- body_add_par(
      doc,
      value = "Missing data by Geographical Region",
      style = "heading 2"
    ) |>
      flextable::body_add_flextable(
        value = flextable::flextable(tbl) |>
          flextable::align(j = 2:ncol(tbl), align = "right")
      ) |>
      body_add_par(
        value = "The percentage of missing values for age are based on dates that have either or both month and year of birth missing."
      )
  }
  if (variables$show_missing_data_by_team) {
    tbl <- variables$missing_data_table_by_team()
    doc <- body_add_par(
      doc,
      value = "Missing data by Team",
      style = "heading 2"
    ) |>
      flextable::body_add_flextable(
        value = flextable::flextable(tbl) |>
          flextable::align(j = 2:ncol(tbl), align = "right")
      ) |>
      body_add_par(
        value = "The percentage of missing values for age are based on dates that have either or both month and year of birth missing."
      )
  }

  doc <- body_add_par(doc, value = "Data distribution", style = "heading 1") |>
    body_add_par(
      value = "Distribution by standard age grouping and sex",
      style = "heading 2"
    ) |>
    body_add_gg(
      value = variables$dist_age_group_by_sex(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_par(
      value = "Distribution by age in years and sex",
      style = "heading 2"
    ) |>
    body_add_gg(
      value = variables$dist_age_year_by_sex(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    )
  if (variables$show_mismatch_table) {
    tbl <- variables$mismatch_table()
    doc <- body_add_par(
      doc,
      value = "Number of cases and proportions of mismatches between length/height measurement position and recommended position, by age group.",
      style = "heading 2"
    ) |>
      flextable::body_add_flextable(
        value = flextable::flextable(tbl) |>
          flextable::align(j = 2:ncol(tbl), align = "right")
      ) |>
      body_add_par(
        value = paste0(
          "Number of children with missing information on measurement position:",
          variables$mismatch_n_children()
        )
      ) |>
      body_add(
        fpar(
          ftext("Note: ", fp_text(bold = TRUE)),
          "Mismatch means children under 24 months were measured standing (height) or children 24 months or older were measured lying down (recumbent length), as opposed to the recommendation. Specifically, for children under 9 months where the measurement position was incorrectly recorded as ‘height’ instead of ‘length’, all measurement positions coded as ‘h’ will be treated as missing during reanalysis."
        )
      )
  }
  doc <- body_add_par(
    doc,
    value = "Distribution by age in months",
    style = "heading 2"
  ) |>
    body_add_gg(
      value = variables$dist_age_by_month(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    )
  doc <- body_add_par(
    doc,
    value = "Digit preference charts",
    style = "heading 1"
  ) |>
    body_add_par(
      value = "Decimal digit preference for weight and length/height",
      style = "heading 2"
    ) |>
    body_add_gg(
      value = variables$digit_plot_heaping(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    )
  if (variables$show_digit_plot_heaping_by_region) {
    doc <- body_add_par(
      doc,
      value = "Decimal digit preference by Geographical Region",
      style = "heading 2"
    )
    for (plot in variables$digit_plot_heaping_by_region()) {
      doc <- body_add_gg(
        doc,
        value = plot,
        style = "Figure",
        width = plot_width,
        height = plot_height
      )
    }
  }
  if (variables$show_digit_plot_heaping_by_team) {
    doc <- body_add_par(
      doc,
      value = "Decimal digit preference by Team",
      style = "heading 2"
    )

    for (plot in variables$digit_plot_heaping_by_team()) {
      doc <- body_add_gg(
        doc,
        value = plot,
        style = "Figure",
        width = plot_width,
        height = plot_height
      )
    }
  }
  doc <- body_add_par(
    doc,
    value = "Whole number digit preference for weight",
    style = "heading 2"
  ) |>
    body_add_gg(
      value = variables$digit_plot_integer_for_weight(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_par(
      value = "Whole number digit preference for length/height",
      style = "heading 2"
    ) |>
    body_add_gg(
      value = variables$digit_plot_integer_for_lenhei(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    )
  doc <- body_add_par(
    doc,
    value = "Z-score distribution of indicators",
    style = "heading 1"
  ) |>
    body_add_par(
      value = "Z-score distribution by index",
      style = "heading 2"
    ) |>
    body_add_gg(
      value = variables$zscore_plot_dist(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_par(
      value = "Z-score distribution by index and sex",
      style = "heading 2"
    ) |>
    body_add_gg(
      value = variables$zscore_plot_dist_by_sex(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_par(
      value = "Z-score distribution by index and age group",
      style = "heading 2"
    ) |>
    body_add_gg(
      value = variables$zscore_plot_dist_by_age_group(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    )
  doc <- body_add_par(
    doc,
    value = "Percentage of flagged z-scores based on WHO flagging system by index",
    style = "heading 2"
  ) |>
    body_add_gg(
      value = variables$zscore_plot_flagged_zscores(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_par(
      value = "Z-score summary table",
      style = "heading 1"
    ) |>
    body_add_par(
      value = "Z-score distribution of unweighted summary statistics by index",
      style = "heading 2"
    ) |>
    flextable::body_add_flextable(
      value = flextable::flextable(zscore_table$df1) |>
        flextable::align(j = 2:ncol(zscore_table$df1), align = "right")
    ) |>
    body_add_par(
      value = "Z-score distribution of unweighted summary statistics by index (continued)",
      style = "heading 2"
    ) |>
    flextable::body_add_flextable(
      value = flextable::flextable(zscore_table$df2) |>
        flextable::align(j = 2:ncol(zscore_table$df2), align = "right")
    ) |>
    body_end_block_section(
      block_section(prop_section(
        page_size = page_size(orient = "landscape")
      ))
    )
  doc <- body_add_par(
    doc,
    value = "Annex: Summary of recommended data quality checks",
    style = "heading 1"
  ) |>
    body_add_par(
      value = "The Working Group (WG) on Anthropometry Data Quality recommendation is that data quality be assessed and reported based on assessment on the following 7 parameters: (i) Completeness; (ii) Sex ratio; (iii) Age distribution; (iv) Digit preference of heights and weights; (v) Implausible z score values; (vi) Standard deviation of z scores; and (vii) Normality of z scores."
    ) |>
    body_add(
      fpar(
        "The WG recommends that (i) data quality checks should not be considered in isolation; (ii) formal tests or scoring should not be conducted; (iii) the checks should be used to help users identify issues with the data quality to improve interpretation of the malnutrition estimates from the survey. Although not exhaustive, a summary of details on the various checks is provided below to help their use. Full details and more comprehensive guidance, including on how to calculate, can be found at the full report on the WG’s recommendations",
        run_footnote(
          block_list(
            fpar(
              "Working Group on Anthropometric Data Quality, for the WHO-UNICEF Technical Expert Advisory Group on Nutrition Monitoring (TEAM). Recommendations for improving the quality of anthropometric data and its analysis and reporting. Available at ",
              hyperlink_ftext(
                text = "www.who.int/nutrition/team",
                href = "https://www.who.int/nutrition/team",
                prop = fp_text(underlined = TRUE)
              ),
              " (under \"Technical reports and papers\")"
            )
          ),
          prop = fp_text_lite(vertical.align = "superscript")
        ),
        "."
      )
    ) |>
    body_add(
      fpar(
        ftext(
          "(i) Completeness: although not all statistics are included in the WHO Anthro Survey Analyser, report on structural integrity of the aspects listed below should be included in the final report:",
          fp_text(bold = TRUE)
        )
      )
    ) |>
    unordered_list(
      c(
        "PSUs: % of selected PSUs that were visited.",
        "Households: % of selected households in the PSUs interviewed or recorded as not interviewed (specifying why).",
        "Household members: % of household rosters that were completed.",
        "Children: % of all eligible children are interviewed and measured, or recorded as not interviewed or measured (specifying why), with no duplicate cases.",
        "Dates of birth: % of dates of birth for all eligible children that were complete."
      )
    ) |>
    body_add(
      fpar(ftext("(ii) Sex ratio:", fp_text(bold = TRUE)))
    ) |>
    unordered_list(
      c(
        "What – ratio of girls to boys in the survey and compare to expected for country. The observed ratios should be compared to the expect patterns based on reliable sources.",
        "Why – to identify potential selection biases."
      )
    ) |>
    body_add(
      fpar(ftext("(iii) Age distribution:", fp_text(bold = TRUE)))
    ) |>
    unordered_list(
      c(
        "What – age distributions by age in completed years (6 bars weighted), months (72 bars) and calendar month of birth (12 bars), as histograms.",
        "Why – to identify potential selection biases or misreporting."
      )
    ) |>
    body_add(
      fpar(ftext(
        "(iv) Height and weight digit preference:",
        fp_text(bold = TRUE)
      ))
    ) |>
    unordered_list(
      c(
        "What – terminal digits as well as whole number integer distributions through histograms.",
        "Why – Digit preference may be a tell-tale sign of data fabrication or inadequate care and attention during data collection and recording. When possible, it should be presented by team or other relevant disaggregation categories."
      )
    ) |>
    body_add(
      fpar(ftext("(v) Implausible z score values:", fp_text(bold = TRUE)))
    ) |>
    unordered_list(
      list(
        list(
          "What – the % of cases outside of WHO flags",
          run_footnote(
            block_list(
              fpar(
                "WHO Anthro Software for personal computers - Manual (2011). Available at ",
                hyperlink_ftext(
                  text = "cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/software/anthro-pc-manual-v322.pdf",
                  href = "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/software/anthro-pc-manual-v322.pdf",
                  prop = fp_text(underlined = TRUE)
                )
              )
            ),
            prop = fp_text_lite(vertical.align = "superscript")
          ),
          " for each HAZ, WHZ and WAZ."
        ),
        "Why – a percent above 1% can be indicative of potential data quality issues in measurements or age determination. It should be presented by team or other relevant disaggregation categories."
      )
    ) |>
    body_add(
      fpar(ftext("(vi) Standard deviations:", fp_text(bold = TRUE)))
    ) |>
    unordered_list(
      c(
        "What – SD for each HAZ, WHZ and WAZ.",
        "Why – large SDs may be a sign of data quality problems and/or population heterogeneity. It is unclear what causes SD’s size and more research is needed to determine appropriate interpretation. It should be noted that SDs are typically wider for HAZ than WHZ or WAZ, and that HAZ SD is typically widest in youngest (0-5 mo) and increases as children age through to 5 years. No substantial difference should be observed between boys and girls. It should be presented by team or other relevant disaggregation categories."
      )
    ) |>
    body_add(
      fpar(ftext("(vii) Checks of normality:", fp_text(bold = TRUE)))
    ) |>
    unordered_list(
      c(
        "What – measures of asymmetry (skew) and tailedness (kurtosis) of HAZ, WHZ and WAZ, as well as density plots.",
        "Why – general assumption that 3 indices are normally distributed but unclear if applicable to populations with varying patterns of malnutrition. One can use the rule of thumb ranges of <-0.5 or >+0.5 for skewness to indicate asymmetry and <2 or >4 for kurtosis to indicate heavy or light tails. Further research needed to understand patterns in different contexts. Anyhow the comparisons amongst the distribution by disaggregation categories might help with the interpretation of results."
      )
    )
  doc
}
