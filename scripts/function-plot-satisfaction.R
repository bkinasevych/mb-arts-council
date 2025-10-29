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
