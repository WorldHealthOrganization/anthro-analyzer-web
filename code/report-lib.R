#' Generates the input data for the main plot in the summary report.
generate_prevalence_plot_data <- function(df_prevs) {
  name_var <-
    paste0(
      rep(
        c(
          "Stunting",
          "Wasting",
          "Severe wasting",
          "Overweight",
          "Obesity",
          "Underweight"
        ),
        each = 3
      ),
      c("_r", "_ll", "_ul")
    )

  select_var <-
    c(
      paste0(
        rep("HA_2", each = 3),
        c("_r", "_ll", "_ul")
      ),
      paste0(
        rep("WH", each = 3),
        rep(c("_2", "_3"), each = 3),
        c("_r", "_ll", "_ul")
      ),
      paste0(
        rep("WH", each = 3),
        rep(c("2", "3"), each = 3),
        c("_r", "_ll", "_ul")
      ),
      paste0(
        rep("WA_2", each = 3),
        c("_r", "_ll", "_ul")
      )
    )

  df_prevs %>%
    filter(str_detect(Group, c("All|Age group|Sex|Area|Wealth quintile"))) %>%
    select(Group, one_of(select_var)) %>%
    set_names(c("key", name_var)) %>%
    gather(outcome, prop, -key) %>%
    mutate(
      prop = prop %>% as.numeric(),
      key = key %>% fct_inorder(),
      z = str_replace_all(
        outcome,
        c(".*_r" = "estimate", ".*_ll" = "lower", ".*_ul" = "upper")
      ),
      outcome = str_replace_all(outcome, "_.*", "") %>% fct_inorder()
    ) %>%
    spread(key = z, value = prop)
}

format_prevalence_plot_data_as_table <- function(data) {
  f_num <- function(x) round(x, 2)
  data %>%
    mutate(
      value = paste0(
        f_num(estimate),
        " (",
        f_num(lower),
        "; ",
        f_num(upper),
        ")"
      )
    ) %>%
    select(-estimate, -lower, -upper) %>%
    spread(outcome, value) %>%
    rename(Group = key)
}

generate_report <- function(variables) {
  list2env(variables, envir = environment())
  # this function replaces an old .Rmd file. It thus mixes UI with data
  # generation.
  doc <- read_docx(path = "reports/word-styles-reference.docx")
  word_size <- docx_dim(doc)
  # it's landscape, so we flip width/height
  plot_height <- (word_size$page["width"] -
    word_size$margins["left"] -
    word_size$margins["right"]) *
    0.8
  plot_width <- (word_size$page["height"] -
    word_size$margins["top"] -
    word_size$margins["bottom"])
  run_fnum <- run_autonum(
    seq_id = "fig",
    pre_label = "Figure ",
    bkm = "figure"
  )
  run_tnum <- run_autonum(
    seq_id = "tab",
    pre_label = "Table ",
    bkm = "table"
  )
  doc <- doc |>
    body_end_block_section(
      block_section(prop_section(
        page_size = page_size(orient = "landscape")
      ))
    ) |>
    body_add_par("Table of Contents", style = "heading 1") |>
    body_add_toc(level = 2) |>
    body_add_par(value = "Recommended citation:") |>
    body_add_par(
      value = "Report template with results from WHO Anthro Survey Analyser"
    ) |>
    body_add_par(
      value = paste0(
        "Analysis date: ",
        format(Sys.time(), format = "%Y-%m-%d %H:%M %Z")
      )
    ) |>
    body_add_par(
      "Overall survey results summary",
      style = "heading 1"
    ) |>
    body_add_par(
      "Outcome plots",
      style = "heading 2"
    )

  # Create stratification variables for use in automated reporting
  other_grouping_name <- anthro_make_grouping_label(list_map_vars[[
    'othergr'
  ]]())
  df_strat <-
    tibble(
      key = c(
        'Age group',
        'Sex',
        'Residence type',
        'Geographical region',
        'Wealth quintile',
        'Mother education',
        other_grouping_name
      ),
      value = c(
        'age_group',
        list_map_vars[['sex']](),
        list_map_vars[['typeres']](),
        list_map_vars[['gregion']](),
        list_map_vars[['wealthq']](),
        list_map_vars[['mothered']](),
        list_map_vars[['othergr']]()
      )
    ) %>%
    filter(!value %in% 'None')

  # check that all list_map_vars are part of the dataset
  # as list_map_vars are user supplied
  stopifnot(all(df_strat$value %in% colnames(df_filtered())))

  df_zscores <-
    CalculateZScores(
      data = df_filtered(),
      sex = list_map_vars[['sex']](),
      weight = list_map_vars[['weight']](),
      lenhei = list_map_vars[['lenhei']](),
      lenhei_unit = list_map_vars[['lenhei_unit']](),
      oedema = list_map_vars[['oedema']]()
    )

  # calculate the missing cases if cluster, strata or sw is set
  prev_missing_vals <- anthro_prev_excluded_cases(df_zscores, list_map_vars)
  prev_missing_values_names <- prev_missing_vals$missing_values_names
  prev_missing_value_counts <- prev_missing_vals$missing_value_counts

  df_prevs <-
    CalculatePrev(
      data = df_filtered(),
      age.month = age_in_months(),
      age = list_map_vars[['age']](),
      date_birth = list_map_vars[['date_birth']](),
      date_obs = list_map_vars[['date_obs']](),
      sex = list_map_vars[['sex']](),
      weight = list_map_vars[['weight']](),
      lenhei = list_map_vars[['lenhei']](),
      lenhei_unit = list_map_vars[['lenhei_unit']](),
      sw = list_map_vars[['sw']](),
      cluster = list_map_vars[['cluster']](),
      strata = list_map_vars[['strata']](),
      typeres = list_map_vars[['typeres']](),
      gregion = list_map_vars[['gregion']](),
      wealthq = list_map_vars[['wealthq']](),
      mothered = list_map_vars[['mothered']](),
      othergr = list_map_vars[['othergr']](),
      oedema = list_map_vars[['oedema']]()
    )

  ## outcome plot
  prev_plot_data <- generate_prevalence_plot_data(df_prevs)
  p <- prev_plot_data %>%
    filter(!str_detect(key, "All")) %>%
    full_join(
      prev_plot_data %>% filter(str_detect(key, "All")) %>% select(-key),
      by = "outcome"
    ) %>%
    filter(outcome != "Obesity") %>% # exclude obesity in plot
    mutate(legend = "Total /n (95% CI)") %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    ggplot() +
    aes(x = key %>% fct_rev()) +
    geom_pointrange(
      aes(y = estimate.x, ymin = lower.x, ymax = upper.x),
      fatten = 2
    ) +
    geom_hline(
      aes(yintercept = estimate.y, colour = "Total \n (95% CI)"),
      alpha = .5,
      linetype = "dashed"
    ) +
    geom_ribbon(
      aes(ymin = lower.y, ymax = upper.y, fill = "Total \n (95% CI)"),
      group = 1,
      alpha = .25
    ) +
    scale_colour_manual(
      "",
      breaks = c("Total (95% CI)"),
      values = c("black")
    ) +
    scale_fill_manual(
      "",
      breaks = c("Total (95% CI)"),
      values = c("grey75")
    ) +
    guides(colour = guide_legend(nrow = 2)) +
    coord_flip(ylim = c(0, NA_real_)) +
    expand_limits(y = 0) +
    labs(x = "", y = "Proportion (%)") +
    facet_grid(~outcome, scales = "fixed") +
    anthro_ggplot2_style() +
    theme(legend.position = "bottom")
  doc <- body_add_gg(
    doc,
    p,
    style = "Figure",
    width = plot_width,
    height = plot_height
  ) |>
    body_add_caption(
      block_caption(
        "Nutritional status by stratification variable",
        autonum = run_fnum
      )
    )

  # Table 1: Nutritional status by stratification variable
  tab1data <- format_prevalence_plot_data_as_table(prev_plot_data)
  doc <- doc |>
    body_add_break() |>
    body_add_caption(
      block_caption(
        "Nutritional status by stratification variable",
        autonum = run_tnum
      )
    ) |>
    flextable::body_add_flextable(
      value = flextable::theme_booktabs(flextable::flextable(
        tab1data
      )) |>
        flextable::align(j = 2:ncol(tab1data), align = "right")
    )

  doc <- body_add_break(doc) |>
    body_add_par("Summary on survey description", style = "heading 1")

  data_collect_start <-
    if (list_map_vars[['date_obs']]() != 'None') {
      tmp <- df_zscores[[list_map_vars[['date_obs']]()]] %>% min(na.rm = TRUE)
      str_c(day(tmp), month(tmp, label = TRUE), year(tmp), sep = ' ')
    } else {
      'unknown'
    }

  data_collect_end <-
    if (list_map_vars[['date_obs']]() != 'None') {
      tmp <- df_zscores[[list_map_vars[['date_obs']]()]] %>% max(na.rm = TRUE)
      str_c(day(tmp), month(tmp, label = TRUE), year(tmp), sep = ' ')
    } else {
      'unknown'
    }

  # Raw numbers
  df_eligible <- df_zscores
  eligible_total <- df_eligible %>% nrow()
  eligible_height <-
    df_eligible %>%
    filter_(interp(~ x > 0, x = as.name(list_map_vars[['lenhei']]()))) %>%
    nrow()
  eligible_weight <-
    df_eligible %>%
    filter_(interp(~ x > 0, x = as.name(list_map_vars[['weight']]()))) %>%
    nrow()
  eligible_height_text <- str_c(
    eligible_height,
    ' (',
    (eligible_height / eligible_total * 100) %>% round(1),
    '%)'
  )
  eligible_weight_text <- str_c(
    eligible_weight,
    ' (',
    (eligible_weight / eligible_total * 100) %>% round(1),
    '%)'
  )

  # Oedema
  oedema_n <-
    ifelse(
      list_map_vars[['oedema']]() == 'None',
      'None',
      df_zscores %>%
        group_by_(list_map_vars[['oedema']]()) %>%
        tally %>%
        filter_(
          interp(
            ~ x %in% c('Y', 'y', '1', 1),
            x = as.name(list_map_vars[['oedema']]())
          )
        ) %>%
        extract2('n')
    )

  is_oedema_mapped <- !is.na(oedema_n) &&
    length(oedema_n) == 1L &&
    oedema_n > 0 &&
    oedema_n != "None"

  # helper function for later in the document
  print_oedema_note <- function(oedema_n) {
    if (is_oedema_mapped && oedema_n > 0) {
      cat(
        "\nThere were ",
        oedema_n,
        " cases of bilateral oedema, for which weight-for-age and weight-for-height z-scores were considered as below -3 for prevalence calculation purposes.\n"
      )
    }
  }

  oedema_sentence <-
    ifelse(
      oedema_n == 'None',
      'Information on oedema was not provided.',
      str_c('There were', oedema_n, 'cases of oedema reported.', sep = ' ')
    )

  # # Flags
  df_flagged <- zscore_flagged_zscores(df_zscores) %>%
    mutate(
      pretty_key = fct_recode(
        key,
        `length- or height-for-age` = 'zlen_flag',
        `weight-for-length or height` = 'zwfl_flag',
        `weight-for-age` = 'zwei_flag',
        `body mass index-for-age` = 'zbmi_flag'
      ) %>%
        as.character(),
    )

  flag_sentence <-
    str_c(
      'There were ',
      df_flagged %>%
        select(-key) %>%
        as.list %>%
        pmap(function(pretty_key, n, value) {
          str_c(
            n,
            ' (',
            round(value, 1),
            '%) ',
            'flags for ',
            pretty_key,
            sep = ''
          )
        }) %>%
        str_c(collapse = ', '),
      '.'
    )

  # missing age
  # the missing age is computed by looking at the zscore age column
  # and relating that to the total number of records of the z_scores
  stopifnot('age_in_days' %in% colnames(df_zscores))
  ages <- as.numeric(df_filtered()[['age_in_days']])
  missing_age_n <- sum(is.na(ages))
  n_rows <- nrow(df_filtered())
  percent_format <- function(value) {
    format(round(value / n_rows * 100, 1), digits = 1)
  }
  missing_age_text <- paste0(
    if (missing_age_n == 1) 'was ' else 'were ',
    missing_age_n,
    ' (',
    percent_format(missing_age_n),
    '%) ',
    if (missing_age_n == 1) 'child' else 'children'
  )
  no_negative_age <- sum(df_filtered()[['age_in_days']] < 0, na.rm = TRUE)
  negative_age_text <- if (no_negative_age > 0) {
    paste0(
      " and ",
      no_negative_age,
      " (",
      percent_format(no_negative_age),
      "%) ",
      if (no_negative_age == 1) "child" else "children",
      " with negative values for age"
    )
  } else {
    ""
  }

  sex_col <- list_map_vars[['sex']]()
  missing_sex_n <- sum(is.na(df_filtered()[[sex_col]]))
  missing_sex_text <- paste0(
    if (missing_age_n == 1) 'was ' else 'were ',
    missing_sex_n,
    ' (',
    percent_format(missing_sex_n),
    '%) ',
    ' ',
    if (missing_age_n == 1) 'child' else 'children'
  )

  removed_due_to_age_text <- ""
  if (number_removed_due_to_age() > 0) {
    n <- number_removed_due_to_age()
    n_percent <- percent_format(n)
    was <- if (n == 1) "was " else "were "
    children <- if (n == 1) "child " else "children "
    removed_due_to_age_text <- paste0(
      "There ",
      was,
      " ",
      n,
      " (",
      n_percent,
      "%) ",
      children,
      " aged greater than sixty months who ",
      was,
      " excluded from the analysis."
    )
  }
  # we also calculate which weight/lenhei values where set to NA that were not NA before
  data <- df_filtered()
  number_lenhei_set_to_na <- sum(is.na(df_zscores$clenhei)) -
    sum(is.na(data[[list_map_vars[['lenhei']]()]]))

  doc <- body_add_par(
    doc,
    "Sample size:",
    style = "heading 2"
  ) |>
    body_add_fpar(
      fpar(
        paste0(
          "The original sample was of ",
          nrow(df_raw()),
          " children. There were ",
          eligible_total,
          " children retained after filtering for ["
        ),
        ftext(
          "INSERT DETAILS OF ANY FILTERING APPLIED",
          fp_text(bold = TRUE)
        ),
        paste0(
          "; height measurements were obtained for ",
          eligible_height_text,
          " children and weight measurements were obtained for ",
          eligible_weight_text,
          ". There ",
          missing_sex_text,
          " with missing information on sex and there ",
          missing_age_text,
          " with missing age",
          negative_age_text
        )
      )
    ) |>
    body_add_par(
      removed_due_to_age_text
    ) |>
    body_add_par(
      oedema_sentence
    )

  walk2(
    prev_missing_values_names,
    prev_missing_value_counts,
    function(col, count) {
      if (count > 0) {
        cat('\\n')
        child <- if (count == 1) "child" else "children"
        was <- if (count == 1) "was" else "were"
        txt <- paste0(
          count,
          " ",
          child,
          " had missing '",
          col,
          "' information and ",
          was,
          " excluded from the prevalence calculations."
        )
        cat('\\n')
        doc <<- body_add_par(
          doc,
          txt
        )
      }
    }
  )

  doc <- body_add_par(
    doc,
    "Sample design:",
    style = "heading 2"
  ) |>
    body_add_par(
      "Household listing (source or how was it done to update existing information)",
      style = "heading 2"
    ) |>
    body_add_par(
      "Training of field staff: How many, how many teams, how many measurements per team per day",
      style = "heading 2"
    ) |>
    body_add_par(
      "Standardization",
      style = "heading 2"
    ) |>
    body_add_par(
      "Equipment and calibration",
      style = "heading 2"
    ) |>
    body_add_par(
      "Data collection period",
      style = "heading 2"
    ) |>
    body_add_fpar(
      fpar(
        "Data collection: Start: [",
        ftext(
          "enter month and year the survey started MM/YYYY",
          fp_text(bold = TRUE)
        ),
        "]; End: [",
        ftext(
          "enter month and year the survey ended MM/YYYY",
          fp_text(bold = TRUE)
        ),
        "]"
      )
    ) |>
    body_add_par(
      "Data entry",
      style = "heading 2"
    ) |>
    body_add_par(
      "Supervision",
      style = "heading 2"
    ) |>
    body_add_par(
      "Other survey context important for the interpretation of results",
      style = "heading 1"
    ) |>
    body_add_par(
      "Seasonality (e.g. harvest and malaria)",
      style = "heading 2"
    ) |>
    body_add_par(
      "Climate conditions (e.g. monsoon, drought, natural catastrophes)",
      style = "heading 2"
    ) |>
    body_add_par(
      "Epidemics, high mortality",
      style = "heading 2"
    ) |>
    body_add_par(
      "Security issues, civil unrest",
      style = "heading 2"
    ) |>
    body_add_par(
      "Population groups not covered (e.g. slums, refugees)",
      style = "heading 2"
    ) |>
    body_add_par(
      "Summary of survey analysis",
      style = "heading 1"
    ) |>
    body_add_par(
      "Data processing: Software …..",
      style = "heading 2"
    ) |>
    body_add_par(
      "Data cleaning:",
      style = "heading 2"
    ) |>
    body_add_par(
      "Imputations:",
      style = "heading 2"
    ) |>
    body_add_break() |>
    body_add_par(
      "Data quality indicators and assessment",
      style = "heading 1"
    ) |>
    body_add_par(
      "Flags:",
      style = "heading 2"
    ) |>
    body_add_par(
      "Flags were calculated as follows:...",
    ) |>
    body_add_par(
      flag_sentence,
    ) |>
    body_add_break() |>
    body_add_par(
      "Missing data",
      style = "heading 2"
    )

  # Missing data

  quality_vars <-
    tibble(
      key = c('Age', 'Weight (kg)', 'Length or height (cm)'),
      value = c(
        'age_in_days',
        list_map_vars[['weight']](),
        list_map_vars[['lenhei']]()
      )
    ) %>%
    bind_rows(df_strat) %>%
    mutate(
      key = fct_inorder(key)
    ) %>%
    filter(!value %in% c('None', 'age_group'))
  df_missing <-
    df_zscores %>%
    select(one_of(quality_vars$value)) %>%
    mutate_all(funs(sum(is.na(.)))) %>%
    gather(name, n) %>%
    group_by(name) %>%
    slice(1) %>%
    full_join(quality_vars, by = c('name' = 'value')) %>%
    ungroup %>%
    mutate(
      name = fct_relevel(name, unique(quality_vars$value)),
      value = (n / nrow(df_zscores) * 100) %>% round(1),
      missing = str_c(n, ' (', value, '%)')
    ) %>%
    arrange(name) %>%
    select(key, everything())

  max_plot_val <- max(df_missing$value)
  p <- df_missing %>%
    ggplot(aes(x = fct_rev(key), y = value)) +
    geom_segment(aes(xend = fct_rev(key), yend = 0), size = 0.5) +
    geom_point(size = 0.5) +
    geom_text(
      aes(label = str_c(value, '%', ' (', n, ')')),
      size = 3,
      fontface = 'bold',
      vjust = 0.5,
      hjust = -0.4
    ) +
    coord_flip() +
    labs(x = 'Variable', y = 'Proportion missing (%)') +
    anthro_ggplot2_style() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

  if (max_plot_val <= 0.0001) {
    p <- p + scale_y_continuous(limits = c(0, 0.1))
  }

  doc <- body_add_gg(
    doc,
    p,
    style = "Figure",
    width = plot_width,
    height = plot_height
  ) |>
    body_add_caption(
      block_caption(
        "Missing data",
        autonum = run_fnum
      )
    ) |>
    body_add(
      fpar(
        paste0(
          "Note: In addition to those missing values, ",
          number_lenhei_set_to_na
        ),
        " lengths/heights were set to missing, because they were outside the plausibility intervals (see ",
        hyperlink_ftext(
          text = "Quick Guide",
          href = "https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/software/anthro-survey-analyser-quickguide.pdf",
          prop = fp_text(underlined = TRUE)
        ),
        ")."
      )
    ) |>
    body_add_break() |>
    body_add_par(
      "Data distributions",
      style = "heading 2"
    ) |>
    body_add_gg(
      #distribution by standard age grouping and sex
      dist_age_group_by_sex(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_caption(
      block_caption(
        "distribution by standard age grouping and sex",
        autonum = run_fnum
      )
    ) |>
    body_add_gg(
      #distribution by age in years and sex
      dist_age_year_by_sex(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_caption(
      block_caption(
        "distribution by age in years and sex",
        autonum = run_fnum
      )
    ) |>
    body_add_gg(
      #distribution by age in months
      dist_age_by_month(),
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_caption(
      block_caption(
        "distribution by age in months",
        autonum = run_fnum
      )
    ) |>
    body_add_par(
      "Digit heaping charts (with mapping variable labels)",
      style = "heading 2"
    )

  # Digit preference for weight and height
  digit_preference <-
    df_zscores %>%
    select(!!list_map_vars[['weight']](), !!list_map_vars[['lenhei']]()) %>%
    mutate_all(round, digits = 1) %>%
    mutate_all(funs(as.character)) %>%
    mutate_all(funs(ifelse(
      str_detect(., '\\.') %in% FALSE,
      paste0(., '.0'),
      .
    ))) %>%
    mutate_all(str_extract, pattern = "\\..*") %>%
    mutate_all(funs(ifelse(is.na(.), NA_character_, paste0("0", .)))) %>%
    gather() %>%
    mutate(key = fct_inorder(key)) %>%
    group_by(key, value) %>%
    tally %>%
    group_by(key) %>%
    filter(!is.na(value)) %>%
    mutate(prop = (n / sum(n) * 100) %>% round(1))

  digit_preference$pretty_key <-
    factor(
      digit_preference$key,
      labels = c('Weight (kg)', 'Length or height (cm)')
    )

  p <- digit_preference %>%
    ggplot(aes(x = value %>% fct_rev, y = prop)) +
    geom_segment(aes(xend = fct_rev(value), yend = 0)) +
    geom_point(size = 1) +
    coord_flip() +
    geom_text(
      aes(label = str_c(prop, '%')),
      fontface = 'bold',
      vjust = 0.5,
      hjust = -0.4
    ) +
    facet_wrap(~pretty_key, nrow = 2) +
    labs(x = 'Digit', y = 'Proportion (%)') +
    anthro_ggplot2_style() +
    theme(
      panel.spacing.x = unit(.04, 'cm'),
      panel.spacing.y = unit(.02, 'cm')
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

  # Figure Digit preference for weight \\& height measurements
  doc <- body_add_gg(
    doc,
    p,
    style = "Figure",
    width = plot_width,
    height = plot_height
  ) |>
    body_add_caption(
      block_caption(
        "Digit preference for weight & height measurements",
        autonum = run_fnum
      )
    ) |>
    body_add_par(
      "Z-score distribution issues",
      style = "heading 2"
    )

  # Z-score density plots
  df_zscore_dist <-
    df_zscores %>%
    select(one_of(df_strat$value), one_of(zscore_flag_cols) - 1) %>%
    mutate_at(df_strat$value, funs(as.factor(.))) %>%
    gather(key, value, -tidyselect::one_of(df_strat$value)) %>%
    filter(!is.na(value)) %>%
    mutate(key = fct_relevel(key, 'zlen', 'zwfl', 'zwei', 'zbmi')) %>%
    mutate(
      key = fct_recode(
        key,
        `Length- or height-for-age` = 'zlen',
        `Weight-for-length or height` = 'zwfl',
        `Weight-for-age` = 'zwei',
        `Body mass index-for-age` = 'zbmi'
      )
    )
  indx <- df_zscore_dist %>% names() %>% match(df_strat$value, nomatch = 0)
  df_zscore_dist %<>% rename_at(names(.)[indx != 0], ~ df_strat$key[indx])

  if (!is.null(df_zscore_dist$Sex)) {
    df_zscore_dist <-
      df_zscore_dist %>%
      mutate(
        Sex = recode_sex_factor(Sex)
      )
  }

  if (!is.null(df_zscore_dist[["Wealth quintile"]])) {
    df_zscore_dist <-
      df_zscore_dist %>%
      mutate(
        `Wealth quintile` = recode_wiq_factor(`Wealth quintile`)
      )
  }

  # we do not show NA values in the stratification variable
  # similiar to the plot in the tool
  df_zscore_dist_age <- dplyr::filter(
    df_zscore_dist,
    !is.na(`Age group`)
  )

  p <- ggplot(df_zscore_dist_age) +
    stat_density(
      aes(value, linetype = "Samples"),
      size = .5,
      geom = "line",
      position = "identity"
    ) +
    anthro_ggplot2_standard_normal(linetype_as_aes = TRUE) +
    facet_grid(`Age group` ~ key) +
    xlim(-6, 6) +
    labs(x = 'z-scores', y = 'Density') +
    anthro_ggplot2_style() +
    theme(
      panel.spacing.x = unit(.04, 'cm'),
      panel.spacing.y = unit(.2, 'cm'),
      strip.text.y = element_text(size = 8)
    ) +
    theme(legend.position = "bottom") +
    anthro_scale_linetype_manual_no_groups +
    scale_y_continuous(guide = guide_axis(n.dodge = 2, check.overlap = TRUE))

  # Z-score distributions by age group
  doc <- body_add_gg(
    doc,
    p,
    style = "Figure",
    width = plot_width,
    height = plot_height
  ) |>
    body_add_caption(
      block_caption(
        "Z-score distributions by age group",
        autonum = run_fnum
      )
    )

  # Z-score distributions by sex

  p <-
    ggplot(df_zscore_dist) +
    stat_density(
      aes(value, linetype = Sex),
      size = .25,
      geom = "line",
      position = "identity"
    ) +
    anthro_ggplot2_standard_normal() +
    facet_wrap(~key, scales = 'free') +
    xlim(-6, 6) +
    labs(
      x = 'z-scores',
      y = 'Density',
      colour = df_strat$key[df_strat$value == 'Sex']
    ) +
    anthro_ggplot2_style() +
    theme(
      legend.key = element_blank(),
      legend.key.size = unit(.2, 'cm'),
      panel.spacing.x = unit(.04, 'cm'),
      panel.spacing.y = unit(.02, 'cm')
    )
  doc <- body_add_gg(
    doc,
    p,
    style = "Figure",
    width = plot_width,
    height = plot_height
  ) |>
    body_add_caption(
      block_caption(
        "Z-score distributions by sex",
        autonum = run_fnum
      )
    )

  if (!is.null(df_zscore_dist[["Geographical region"]])) {
    p <- ggplot(df_zscore_dist) +
      stat_density(
        aes(value, linetype = "Samples"),
        size = .25,
        geom = "line",
        position = "identity"
      ) +
      anthro_ggplot2_standard_normal(linetype_as_aes = TRUE) +
      facet_grid(`Geographical region` ~ key) +
      xlim(-6, 6) +
      labs(x = 'z-scores', y = 'Density') +
      anthro_ggplot2_style() +
      theme(
        panel.spacing.x = unit(.04, 'cm'),
        panel.spacing.y = unit(.2, 'cm'),
        strip.text.y = element_text(size = 8)
      ) +
      theme(legend.position = "bottom") +
      anthro_scale_linetype_manual_no_groups +
      scale_y_continuous(guide = guide_axis(n.dodge = 2, check.overlap = TRUE))
    # Z-score distributions by geographical region
    doc <- body_add_gg(
      doc,
      p,
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
      body_add_caption(
        block_caption(
          "Z-score distributions by geographical region",
          autonum = run_fnum
        )
      )
  }

  # Z-score box/violin plots
  violin_groups <-
    df_strat %>%
    filter(
      key %in% c('Age group', 'Sex', 'Residence type', 'Wealth quintile')
    ) %>%
    pull(key)

  ps <-
    map(violin_groups, as.name) %>%
    map(function(strat_var) {
      strat_var_chr <- as.character(strat_var)
      if (strat_var_chr == 'Wealth quintile') {
        df_zscore_dist[[strat_var_chr]] <- recode_wiq_factor(df_zscore_dist[[
          strat_var_chr
        ]])
      }
      ggplot(df_zscore_dist) +
        aes(fct_rev(key), value, fill = !!strat_var) +
        geom_violin(
          size = .25,
          position = position_dodge(width = .9)
        ) +
        coord_flip() +
        scale_fill_grey(start = .6, end = .9, na.value = "darkred") +
        ylim(-6, 6) +
        labs(
          x = "Z-score measure",
          y = "Z-score",
          colour = df_strat$key[df_strat$value == .]
        ) +
        anthro_ggplot2_style() +
        theme(
          legend.key = element_blank(),
          legend.key.size = unit(.2, "cm")
        )
    })

  doc <- body_add_break(doc) |>
    # z-score distribution violin plot by age group
    body_add_gg(
      ps[[1]],
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_caption(
      block_caption(
        "Z-score distribution violin plot by age group",
        autonum = run_fnum
      )
    ) |>
    # z-score distribution violin plot by sex
    body_add_gg(
      ps[[2]],
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
    body_add_caption(
      block_caption(
        "Z-score distribution violin plot by sex",
        autonum = run_fnum
      )
    )

  is_in_group <- 'Residence type' == violin_groups
  if (any(is_in_group)) {
    # z-score distribution violin plot by residence type
    doc <- body_add_gg(
      doc,
      ps[[which(is_in_group)]],
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
      body_add_caption(
        block_caption(
          "Z-score distribution violin plot by residence type",
          autonum = run_fnum
        )
      )
  }

  is_in_group <- 'Wealth quintile' == violin_groups
  if (any(is_in_group)) {
    # z-score distribution violin plots by wealth quintile
    doc <- body_add_gg(
      doc,
      ps[[which(is_in_group)]],
      style = "Figure",
      width = plot_width,
      height = plot_height
    ) |>
      body_add_caption(
        block_caption(
          "Z-score distribution violin plots by wealth quintile",
          autonum = run_fnum
        )
      )
  }

  doc <- doc |>
    body_add_break() |>
    body_add_par(
      "Appendix: Nutritional status tables",
      style = "heading 1"
    )

  to_NA <- function(x, y) {
    y[is.na(x) | x == "NA"] <- NA_character_
    y
  }
  f_num <- function(x, digits = 1) format(as.numeric(x), nsmall = digits)

  additional_oedema_col <- if (is_oedema_mapped) {
    "Oedema_cases"
  } else {
    NULL
  }

  # just in case the Oedema_cases column was not generated
  if (is_oedema_mapped && is.null(df_prevs[[additional_oedema_col]])) {
    additional_oedema_col <- NULL
  }

  list_zscore <-
    c('HA', 'WA') %>%
    map(function(x) {
      df_prevs %>%
        select(
          Group,
          starts_with(x),
          -matches('_se|[A-Z][0-9]|1_|_W'),
          !!!additional_oedema_col
        ) %>%
        mutate_at(
          .vars = vars(matches('_pop|_r|_ll|_ul')),
          .funs = funs(as.numeric(.) %>% round(1))
        ) %>%
        filter(!str_detect(Group, 'age_sex')) %>%
        set_names(c(
          'disaggregation',
          'weighted',
          'unweighted',
          'r_3',
          'll_3',
          'ul_3',
          'r_2',
          'll_2',
          'ul_2',
          'r',
          'll',
          'ul',
          'sd',
          additional_oedema_col
        )) %>%
        mutate_at(
          .vars = vars(weighted:ul),
          .funs = funs(round(as.numeric(.), 1))
        ) %>%
        mutate(unweighted = as.integer(unweighted), sd = round(sd, 2)) %>%
        mutate(
          `-3` = to_NA(
            weighted,
            str_c(f_num(r_3), ' (', f_num(ll_3), '; ', f_num(ul_3), ')')
          ),
          `-2` = to_NA(
            weighted,
            str_c(f_num(r_2), ' (', f_num(ll_2), '; ', f_num(ul_2), ')')
          ),
          mean = to_NA(
            weighted,
            str_c(f_num(r), ' (', f_num(ll), '; ', f_num(ul), ')')
          )
        ) %>%
        select(-matches('^r|^ll|^ul')) %>%
        mutate_at(
          .vars = vars(weighted:unweighted),
          .funs = funs(as.numeric)
        ) %>%
        mutate_at(.vars = vars(disaggregation), .funs = funs(str_trim)) %>%
        set_names(
          c(
            'Group',
            'Weighted N',
            'Unweighted N',
            'z-score SD',
            additional_oedema_col,
            '-3SD (95% CI)',
            '-2SD (95% CI)',
            'z-score mean (95% CI)'
          )
        ) %>%
        select(
          1:3,
          !!(if (is_oedema_mapped) 6:7 else 5:7),
          4,
          !!(if (is_oedema_mapped) 5 else NULL)
        )
    }) %>%
    set_names(c('Stunting (Height-for-age)', 'Weight-for-age'))

  weight_for_height <-
    df_prevs %>%
    select(
      Group,
      starts_with('WH'),
      -matches('_se|1_|_W'),
      !!!additional_oedema_col
    ) %>%
    filter(!str_detect(Group, 'age_sex')) %>%
    set_names(
      c(
        'disaggregation',
        'weighted',
        'unweighted',
        'r_3',
        'll_3',
        'ul_3',
        'r_2',
        'll_2',
        'ul_2',
        'r2',
        'll2',
        'ul2',
        'r3',
        'll3',
        'ul3',
        'r',
        'll',
        'ul',
        'sd',
        additional_oedema_col
      )
    ) %>%
    mutate_at(
      .vars = vars(weighted:ul),
      .funs = funs(round(as.numeric(.), 1))
    ) %>%
    mutate(unweighted = as.integer(unweighted), sd = round(sd, 2)) %>%
    mutate(
      `-3` = to_NA(
        weighted,
        str_c(f_num(r_3), ' (', f_num(ll_3), '; ', f_num(ul_3), ')')
      ),
      `-2` = to_NA(
        weighted,
        str_c(f_num(r_2), ' (', f_num(ll_2), '; ', f_num(ul_2), ')')
      ),
      `+2` = to_NA(
        weighted,
        str_c(f_num(r2), ' (', f_num(ll2), '; ', f_num(ul2), ')')
      ),
      `+3` = to_NA(
        weighted,
        str_c(f_num(r3), ' (', f_num(ll3), '; ', f_num(ul3), ')')
      ),
      mean = to_NA(
        weighted,
        str_c(f_num(r), ' (', f_num(ll), '; ', f_num(ul), ')')
      )
    ) %>%
    mutate_at(.vars = vars(weighted:unweighted), .funs = funs(as.numeric)) %>%
    mutate_at(.vars = vars(disaggregation), .funs = funs(str_trim)) %>%
    select(-matches('^r|^ll|^ul')) %>%
    set_names(
      c(
        'Group',
        'Weighted N',
        'Unweighted N',
        'z-score SD',
        additional_oedema_col,
        '-3SD (95% CI)',
        '-2SD (95% CI)',
        '+2SD (95% CI)',
        '+3SD (95% CI)',
        'z-score mean (95% CI)'
      )
    ) %>%
    select(
      1:3,
      !!(if (is_oedema_mapped) 6:9 else 5:9),
      4,
      !!(if (is_oedema_mapped) 5 else NULL)
    ) %>%
    list %>%
    set_names('Weight-for-height')

  list_zscore %<>% c(weight_for_height)

  # remove the Oedema_cases column for height for age if oedema is mapped
  # as the oedema information is only relevant for Weight-for-age and Weight-for-height
  if (is_oedema_mapped && !is.null(list_zscore[[1]][["Oedema_cases"]])) {
    list_zscore[[1]][["Oedema_cases"]] <- NULL
  }

  doc |>
    body_add_caption(
      block_caption(
        "Height-for-age",
        autonum = run_tnum
      )
    ) |>
    flextable::body_add_flextable(
      value = flextable::theme_booktabs(flextable::flextable(
        list_zscore[[1]] # Table A1: Height-for-age'
      )) |>
        flextable::align(j = 2:ncol(list_zscore[[1]]), align = "right")
    ) |>
    body_add_caption(
      block_caption(
        "Weight-for-age",
        autonum = run_tnum
      )
    ) |>
    flextable::body_add_flextable(
      value = flextable::theme_booktabs(flextable::flextable(
        list_zscore[[2]] # Table A2: Weight-for-age
      )) |>
        flextable::align(j = 2:ncol(list_zscore[[2]]), align = "right")
    ) |>
    body_add_par(print_oedema_note(oedema_n)) |>
    body_add_caption(
      block_caption(
        "Weight-for-height",
        autonum = run_tnum
      )
    ) |>
    flextable::body_add_flextable(
      value = flextable::theme_booktabs(flextable::flextable(
        list_zscore[[3]] # Table A3: Weight-for-height
      )) |>
        flextable::align(j = 2:ncol(list_zscore[[3]]), align = "right")
    ) |>
    body_add_par(print_oedema_note(oedema_n)) |>
    body_end_block_section(
      block_section(prop_section(
        page_size = page_size(orient = "landscape")
      ))
    )
}
#' materialize turns a reactive value into a plain function with its
#' data being a static return value.
#' @noRd
materialize <- function(value) {
  stopifnot(is.reactive(value))
  value <- value()
  function() value
}

materialize_list <- function(values) {
  stopifnot(is.list(values))
  lapply(values, function(x) {
    materialize(x)
  })
}
