plot_confidence_by_group <- function(
  data,
  group_var,
  title = "Confidence in MAC's Ability to Fulfill Its Purpose"
) {
  # Filter out missing values and unwanted responses
  plot_data <- data |>
    filter(
      !confident_in_mac_ability %in% c("Don’t know enough to answer", "Missing")
    ) |>
    drop_na({{ group_var }}) |>
    filter({{ group_var }} != "Missing") |>
    # Drop unused factor levels
    mutate(
      confident_in_mac_ability = fct_drop(confident_in_mac_ability),
      {{ group_var }} := fct_drop({{ group_var }})
    )

  # Count unique levels in the grouping variable
  n_groups <- plot_data |>
    pull({{ group_var }}) |>
    n_distinct()

  # Define color palettes based on number of groups
  colors <- if (n_groups == 2) {
    c(blue, green)
  } else if (n_groups == 3) {
    c(blue, green, mustard)
  } else {
    # For more groups, create a palette
    colorRampPalette(c(blue, green, mustard, orange))(n_groups)
  }

  # Create the plot
  plot_data |>
    ggplot(aes(
      x = confident_in_mac_ability,
      y = after_stat(prop),
      fill = {{ group_var }}
    )) +
    geom_bar(
      aes(group = {{ group_var }}),
      position = "dodge",
      colour = "white"
    ) +
    geom_text(
      aes(
        label = paste0("n = ", after_stat(count)),
        y = after_stat(prop),
        group = {{ group_var }}
      ),
      stat = "count",
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      size = 4
    ) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 0.75)) +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "", y = "", fill = NULL) +
    plot_theme() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom"
    )
}


plot_positive_satisfaction_by_group <- function(
  data,
  group_var,
  title = "Positive Satisfaction by Group"
) {
  satisfaction_cols <- c(
    "deadline_comms",
    "guidelines_comms",
    "feedback",
    "manipogo",
    "response_enquiries",
    "response_results",
    "trasparent_granting",
    "complaints_process"
  )

  satisfaction_summary <- data |>
    select({{ group_var }}, all_of(satisfaction_cols)) |>
    pivot_longer(
      cols = -{{ group_var }},
      names_to = "question",
      values_to = "response"
    ) |>
    filter(
      response != "Don't know",
      response != "Missing",
      !is.na({{ group_var }}),
      {{ group_var }} != "Missing"
    ) |>
    mutate(
      question = case_when(
        question == "trasparent_granting" ~ "Transparency",
        question == "response_results" ~ "Response time",
        question == "response_enquiries" ~ "Responsiveness",
        question == "manipogo" ~ "Manipogo",
        question == "guidelines_comms" ~ "Grant clarity",
        question == "feedback" ~ "Feedback",
        question == "deadline_comms" ~ "Deadline comms",
        question == "complaints_process" ~ "Complaints"
      )
    ) |>
    mutate(positive = response %in% c("Good", "Excellent")) |>
    group_by({{ group_var }}, question) |>
    summarise(pct_positive = mean(positive), .groups = "drop")

  n_groups <- satisfaction_summary |>
    pull({{ group_var }}) |>
    n_distinct()

  colors <- if (n_groups == 2) {
    c(blue, green)
  } else if (n_groups == 3) {
    c(blue, green, mustard)
  } else {
    colorRampPalette(c(blue, green, mustard, orange))(n_groups)
  }

  ggplot(
    satisfaction_summary,
    aes(x = question, y = pct_positive, fill = {{ group_var }})
  ) +
    geom_col(position = "dodge", colour = "white") +
    coord_flip() +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(
      title = title,
      subtitle = "% responding Good or Excellent",
      x = "",
      y = "",
      fill = ""
    ) +
    plot_theme() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}


plot_connection_by_group <- function(
  data,
  group_var,
  title = "Connection to Arts & Cultural Community"
) {
  # Filter out missing values and unwanted responses
  plot_data <- data |>
    filter(
      connection_to_arts_community != "Missing"
    ) |>
    drop_na({{ group_var }}) |>
    filter({{ group_var }} != "Missing") |>
    # Drop unused factor levels
    mutate(
      connection_to_arts_community = fct_drop(connection_to_arts_community),
      {{ group_var }} := fct_drop({{ group_var }})
    )

  # Count unique levels in the grouping variable
  n_groups <- plot_data |>
    pull({{ group_var }}) |>
    n_distinct()

  # Define color palettes based on number of groups
  colors <- if (n_groups == 2) {
    c(blue, green)
  } else if (n_groups == 3) {
    c(blue, green, mustard)
  } else {
    # For more groups, create a palette
    colorRampPalette(c(blue, green, mustard, orange))(n_groups)
  }

  # Create the plot
  plot_data |>
    ggplot(aes(
      x = connection_to_arts_community,
      y = after_stat(count / sum(count)),
      fill = {{ group_var }}
    )) +
    geom_bar(
      aes(group = {{ group_var }}),
      position = "dodge",
      colour = "white"
    ) +
    geom_text(
      aes(
        label = paste0("n = ", after_stat(count)),
        y = after_stat(count / sum(count)),
        group = {{ group_var }}
      ),
      stat = "count",
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      size = 4
    ) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 0.75)) +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "", y = "", fill = NULL) +
    plot_theme() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom"
    )
}
